module Sequence = Instruction_sequence
module Graph = Block_graph
module X87 = X87_stack
module Block_id = Sequence.Block_id
module Instruction_id = Sequence.Instruction_id
module Value_id = Sequence.Value_id

module Value_map = Map.Make (struct
  type t = Value_id.t

  let compare = Value_id.compare
end)

module Instruction_map = Map.Make (struct
  type t = Instruction_id.t

  let compare = Instruction_id.compare
end)

module Instruction_set = Set.Make (struct
  type t = Instruction_id.t

  let compare = Instruction_id.compare
end)

type validation_stage = Graph_rebuild | X87_reverification

type error = {
  stage : validation_stage;
  code : string;
  message : string;
  block_id : int option;
  instruction_id : int option;
  span : Common.Span.t option;
}

type rewrite = {
  block_id : Block_id.t;
  removed_instruction_id : Instruction_id.t;
  removed_value_id : Value_id.t;
  retained_instruction_id : Instruction_id.t;
  retained_value_id : Value_id.t;
  opcode : Opcode.t;
  input_bits : int64;
  folded_bits : int64;
}

type t = { x87 : X87_stack.t; rewrites : rewrite list }
type word_type = I64 | U64

type immediate = {
  instruction_id : Instruction_id.t;
  value_id : Value_id.t;
  word_type : word_type;
  bits : int64;
}

let reference_commit = Sequence.reference_commit

let word_type = function
  | Some type_ when Sema.Type.pointer_depth type_ = 0 -> (
      match Sema.Type.base type_ with
      | Sema.Type.Primitive (Sema.Type.Internal_storage, primitive)
        when Sema.Primitive_type.equal primitive Sema.Primitive_type.I64 ->
          Some I64
      | Sema.Type.Primitive (Sema.Type.Internal_storage, primitive)
        when Sema.Primitive_type.equal primitive Sema.Primitive_type.U64 ->
          Some U64
      | Sema.Type.Primitive _ | Sema.Type.Aggregate _ -> None)
  | Some _ | None -> None

let immediate_of_description (description : Sequence.description) =
  match
    ( description.opcode,
      description.operands,
      description.result,
      word_type description.target_type,
      description.payload,
      description.flags )
  with
  | ( Opcode.Ic_imm_i64,
      [],
      Some result,
      Some word_type,
      Some (Sequence.Integer bits),
      0L ) ->
      Some
        {
          instruction_id = description.instruction_id;
          value_id = result.value_id;
          word_type;
          bits;
        }
  | _ -> None

let unary_result_type opcode operand result =
  match opcode with
  | Opcode.Ic_com -> result = I64
  | Opcode.Ic_not -> result = operand
  | Opcode.Ic_unary_minus -> result = I64
  | _ -> false

let fold_operation opcode =
  match opcode with
  | Opcode.Ic_com -> Some Int64.lognot
  | Opcode.Ic_not -> Some (fun bits -> if bits = 0L then 1L else 0L)
  | Opcode.Ic_unary_minus -> Some Int64.neg
  | _ -> None

let add_use counts value_id =
  Value_map.update value_id
    (function
      | None -> Some 1
      | Some count -> Some (count + 1))
    counts

let use_counts graph =
  Graph.blocks graph
  |> List.fold_left
       (fun counts block ->
         Graph.instructions block |> Sequence.instructions
         |> List.fold_left
              (fun counts instruction ->
                let description = Sequence.description instruction in
                List.fold_left add_use counts description.operands)
              counts)
       Value_map.empty

let exactly_one_use counts value_id =
  match Value_map.find_opt value_id counts with
  | Some 1 -> true
  | _ -> false

let fold_block counts block =
  let current_immediates = ref Value_map.empty in
  let removed = ref Instruction_set.empty in
  let replacements = ref Instruction_map.empty in
  let rewrites_rev = ref [] in
  let descriptions =
    Graph.instructions block |> Sequence.instructions
    |> List.map Sequence.description
  in
  List.iter
    (fun (description : Sequence.description) ->
      let folded =
        match
          ( description.operands,
            description.result,
            description.payload,
            word_type description.target_type,
            description.flags,
            fold_operation description.opcode )
        with
        | [ operand ], Some result, None, Some result_type, 0L, Some evaluate
          -> (
            match Value_map.find_opt operand !current_immediates with
            | Some producer
              when exactly_one_use counts operand
                   && unary_result_type description.opcode producer.word_type
                        result_type ->
                let bits = evaluate producer.bits in
                let replacement =
                  {
                    description with
                    opcode = Opcode.Ic_imm_i64;
                    operands = [];
                    payload = Some (Sequence.Integer bits);
                  }
                in
                removed := Instruction_set.add producer.instruction_id !removed;
                replacements :=
                  Instruction_map.add description.instruction_id replacement
                    !replacements;
                current_immediates :=
                  Value_map.remove producer.value_id !current_immediates;
                current_immediates :=
                  Value_map.add result.value_id
                    {
                      instruction_id = description.instruction_id;
                      value_id = result.value_id;
                      word_type = result_type;
                      bits;
                    }
                    !current_immediates;
                rewrites_rev :=
                  {
                    block_id = Graph.block_id block;
                    removed_instruction_id = producer.instruction_id;
                    removed_value_id = producer.value_id;
                    retained_instruction_id = description.instruction_id;
                    retained_value_id = result.value_id;
                    opcode = description.opcode;
                    input_bits = producer.bits;
                    folded_bits = bits;
                  }
                  :: !rewrites_rev;
                true
            | Some _ | None -> false)
        | _ -> false
      in
      if not folded then
        match immediate_of_description description with
        | Some immediate ->
            current_immediates :=
              Value_map.add immediate.value_id immediate !current_immediates
        | None -> ())
    descriptions;
  let instructions =
    descriptions
    |> List.filter_map (fun (description : Sequence.description) ->
        if Instruction_set.mem description.instruction_id !removed then None
        else
          Some
            (Instruction_map.find_opt description.instruction_id !replacements
            |> Option.value ~default:description))
  in
  ( { Graph.block_id = Graph.block_id block; instructions },
    List.rev !rewrites_rev )

let graph_error (child : Graph.error) =
  {
    stage = Graph_rebuild;
    code = child.code;
    message = child.message;
    block_id = child.block_id;
    instruction_id = child.instruction_id;
    span = child.span;
  }

let x87_error (child : X87.error) =
  {
    stage = X87_reverification;
    code = child.code;
    message = child.message;
    block_id = child.block_id;
    instruction_id = child.instruction_id;
    span = child.span;
  }

let fold checked =
  let graph = X87.graph checked in
  let counts = use_counts graph in
  let blocks, rewrites =
    Graph.blocks graph |> List.map (fold_block counts) |> List.split
  in
  let entry = Graph.entry graph |> Graph.block_id in
  match Graph.create ~entry blocks with
  | Error errors -> Error (List.map graph_error errors)
  | Ok rebuilt -> (
      match X87.verify rebuilt with
      | Error errors -> Error (List.map x87_error errors)
      | Ok x87 -> Ok { x87; rewrites = List.concat rewrites })

let x87 folded = folded.x87
let rewrites folded = folded.rewrites

let human folded =
  let buffer = Buffer.create 256 in
  Printf.bprintf buffer
    "holyc-ir-integer-unary-folding-v1 reference=%s\nrewrites=%d\n"
    reference_commit
    (List.length folded.rewrites);
  List.iter
    (fun rewrite ->
      Printf.bprintf buffer
        "^b%d remove=!i%d:%%v%d retain=!i%d:%%v%d opcode=%s input=0x%016Lx \
         output=0x%016Lx\n"
        (Block_id.to_int rewrite.block_id)
        (Instruction_id.to_int rewrite.removed_instruction_id)
        (Value_id.to_int rewrite.removed_value_id)
        (Instruction_id.to_int rewrite.retained_instruction_id)
        (Value_id.to_int rewrite.retained_value_id)
        (Opcode.to_source_name rewrite.opcode)
        rewrite.input_bits rewrite.folded_bits)
    folded.rewrites;
  Buffer.contents buffer
