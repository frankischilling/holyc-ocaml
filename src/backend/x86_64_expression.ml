module Sequence = Ir.Instruction_sequence
module Graph = Ir.Block_graph
module Opcode = Ir.Opcode
module Type = Sema.Type
module Primitive = Sema.Primitive_type
module Computation = Sema.Integer_computation_class
module Encoder = X86_64_encoder
module Value_map = Map.Make (Sequence.Value_id)
module Instruction_set = Set.Make (Sequence.Instruction_id)

type word_type = I64 | U64
type error = { code : string; message : string; span : Common.Span.t option }

type t = {
  encoded : bytes;
  word_type : word_type;
  ir_count : int;
  machine_count : int;
  peak : int;
}

let hard_ir_limit = 100_000

let validate_limits ~max_ir_instructions ~max_code_bytes =
  let invalid message =
    Error [ { code = "HCBACK0001"; message; span = None } ]
  in
  if max_ir_instructions <= 0 || max_ir_instructions > hard_ir_limit then
    invalid
      (Printf.sprintf "max_ir_instructions must be between 1 and %d"
         hard_ir_limit)
  else if max_code_bytes <= 0 || max_code_bytes > Encoder.max_code_bytes then
    invalid
      (Printf.sprintf "max_code_bytes must be between 1 and %d"
         Encoder.max_code_bytes)
  else Ok ()

exception Rejected of error

let reject ?span code message = raise (Rejected { code; message; span })

let malformed (description : Sequence.description) message =
  reject ?span:description.span "HCBACK0003"
    (Printf.sprintf "%s: %s" (Opcode.to_source_name description.opcode) message)

let unsupported (description : Sequence.description) message =
  reject ?span:description.span "HCBACK0002"
    (Printf.sprintf "%s: %s" (Opcode.to_source_name description.opcode) message)

let checked_word description type_ =
  if Type.pointer_depth type_ <> 0 then
    unsupported description "native expressions do not support pointer values";
  match Type.base type_ with
  | Type.Primitive (Type.Internal_storage, Primitive.I64) -> I64
  | Type.Primitive (Type.Internal_storage, Primitive.U64) -> U64
  | _ ->
      unsupported description
        "native expressions require internal I64 or U64 values"

type value = {
  value_id : Sequence.Value_id.t;
  declared_type : Type.t;
  computation_type : Type.t;
  mutable last_use : int;
}

type operation =
  | Load_immediate of value * int64
  | Apply_unary of Encoder.unary * value * value
  | Apply_binary of Encoder.binary * value * value * value
  | Apply_comparison of Encoder.condition * value * value * value
  | Apply_logical_not of value * value
  | Return_value of value
  | Return

type prepared_instruction = {
  operation : operation;
  span : Common.Span.t option;
}

type kind =
  | Immediate_kind
  | Unary_kind of Encoder.unary
  | Binary_kind of Encoder.binary
  | Comparison_kind of Encoder.condition * Encoder.condition
  | Logical_not_kind
  | Return_value_kind
  | Return_kind

let opcode_kind = function
  | Opcode.Ic_imm_i64 -> Some Immediate_kind
  | Opcode.Ic_unary_minus -> Some (Unary_kind Encoder.Neg)
  | Opcode.Ic_com -> Some (Unary_kind Encoder.Not)
  | Opcode.Ic_not -> Some Logical_not_kind
  | Opcode.Ic_add -> Some (Binary_kind Encoder.Add)
  | Opcode.Ic_sub -> Some (Binary_kind Encoder.Sub)
  | Opcode.Ic_mul -> Some (Binary_kind Encoder.Imul)
  | Opcode.Ic_and -> Some (Binary_kind Encoder.And)
  | Opcode.Ic_or -> Some (Binary_kind Encoder.Or)
  | Opcode.Ic_xor -> Some (Binary_kind Encoder.Xor)
  | Opcode.Ic_equ_equ -> Some (Comparison_kind (Encoder.E, Encoder.E))
  | Opcode.Ic_not_equ -> Some (Comparison_kind (Encoder.NE, Encoder.NE))
  | Opcode.Ic_less -> Some (Comparison_kind (Encoder.L, Encoder.B))
  | Opcode.Ic_greater_equ -> Some (Comparison_kind (Encoder.GE, Encoder.AE))
  | Opcode.Ic_greater -> Some (Comparison_kind (Encoder.G, Encoder.A))
  | Opcode.Ic_less_equ -> Some (Comparison_kind (Encoder.LE, Encoder.BE))
  | Opcode.Ic_return_val -> Some Return_value_kind
  | Opcode.Ic_ret -> Some Return_kind
  | _ -> None

let single_block verified =
  let graph = Ir.X87_stack.graph verified in
  match Graph.blocks graph with
  | [ block ] ->
      if
        not
          (Sequence.Block_id.equal (Graph.block_id block)
             (Graph.block_id (Graph.entry graph)))
      then reject "HCBACK0003" "the sole block must be the graph entry";
      if Graph.successors block <> [] then
        reject "HCBACK0002" "native expressions do not support graph edges";
      block
  | [] -> reject "HCBACK0003" "native expressions require an entry block"
  | _ -> reject "HCBACK0002" "native expressions require exactly one block"

let bounded_length ~max_ir_instructions instructions =
  let rec count length = function
    | [] -> length
    | instruction :: remaining ->
        if length = max_ir_instructions then
          reject ?span:(Sequence.description instruction).span "HCBACK0001"
            (Printf.sprintf
               "IR instruction count exceeds max_ir_instructions (%d)"
               max_ir_instructions);
        count (length + 1) remaining
  in
  count 0 instructions

let preflight ~count instructions =
  if count < 3 then
    reject "HCBACK0003"
      "native expressions require a value followed by IC_RETURN_VAL and IC_RET";
  let values = ref Value_map.empty in
  let instruction_ids = ref Instruction_set.empty in
  let prepared = ref [] in
  let return_type = ref None in
  let operand description position value_id =
    match Value_map.find_opt value_id !values with
    | None ->
        malformed description
          (Printf.sprintf "value %%%d has no earlier definition"
             (Sequence.Value_id.to_int value_id))
    | Some value ->
        (* An instruction index, rather than a decrementing use counter,
           handles repeated operands without releasing their register early. *)
        value.last_use <- position;
        value
  in
  let define description position (result : Sequence.value_definition)
      declared_type computation_type =
    if Value_map.mem result.value_id !values then
      malformed description
        (Printf.sprintf "value %%%d is defined more than once"
           (Sequence.Value_id.to_int result.value_id));
    let value =
      {
        value_id = result.value_id;
        declared_type;
        computation_type;
        last_use = position;
      }
    in
    values := Value_map.add result.value_id value !values;
    value
  in
  let require_type description expected actual =
    if not (Type.equal expected actual) then
      malformed description
        (Printf.sprintf "target type %s does not match required class %s"
           (Sequence.type_name actual)
           (Sequence.type_name expected))
  in
  let promoted_type description left right =
    let raw_id type_ =
      match Type.base type_ with
      | Type.Primitive (_, primitive) -> (Primitive.info primitive).raw_id
      | Type.Aggregate _ ->
          malformed description "operand has no integer computation class"
    in
    let selected =
      if raw_id left.computation_type >= raw_id right.computation_type then
        left.computation_type
      else right.computation_type
    in
    Computation.forward selected
  in
  List.iteri
    (fun position instruction ->
      let description = Sequence.description instruction in
      if Instruction_set.mem description.instruction_id !instruction_ids then
        malformed description "instruction ID is defined more than once";
      instruction_ids :=
        Instruction_set.add description.instruction_id !instruction_ids;
      (match description.span with
      | Some span when span.start < 0 || span.stop < span.start ->
          malformed description "source span is invalid"
      | Some _ | None -> ());
      if not (Int64.equal description.flags 0L) then
        unsupported description
          "native expressions require zero instruction flags";
      let kind =
        match opcode_kind description.opcode with
        | Some kind -> kind
        | None ->
            unsupported description
              "opcode is outside the native expression subset"
      in
      if position < count - 2 then
        match kind with
        | Return_value_kind | Return_kind ->
            malformed description
              "return instructions must be the exact terminal pair"
        | Immediate_kind
        | Unary_kind _
        | Binary_kind _
        | Comparison_kind _
        | Logical_not_kind -> ()
      else if position = count - 2 then (
        if kind <> Return_value_kind then
          malformed description "penultimate instruction must be IC_RETURN_VAL")
      else if kind <> Return_kind then
        malformed description "last instruction must be IC_RET";
      let shape_error () =
        malformed description
          "invalid operands, result, target type, or payload"
      in
      let shape =
        ( description.operands,
          description.result,
          description.target_type,
          description.payload )
      in
      let operation =
        match (kind, shape) with
        | ( Immediate_kind,
            ([], Some result, Some target_type, Some (Sequence.Integer bits)) )
          ->
            let _ = checked_word description target_type in
            let value =
              define description position result target_type
                (Computation.forward target_type)
            in
            Load_immediate (value, bits)
        | Unary_kind unary, ([ operand_id ], Some result, Some target_type, None)
          ->
            let word = checked_word description target_type in
            let input = operand description position operand_id in
            let computation_type =
              match unary with
              | Encoder.Neg ->
                  let expected = Computation.negate input.computation_type in
                  require_type description expected target_type;
                  Computation.forward target_type
              | Encoder.Not ->
                  if word <> I64 then
                    malformed description "complement must declare internal I64";
                  (* IC_COM declares I64 but keeps its operand's forwarded
                     computation class for enclosing operators. *)
                  Computation.forward input.computation_type
            in
            let result =
              define description position result target_type computation_type
            in
            Apply_unary (unary, input, result)
        | Logical_not_kind, ([ operand_id ], Some result, Some target_type, None)
          ->
            let _ = checked_word description target_type in
            let input = operand description position operand_id in
            let computation_type = Computation.forward input.computation_type in
            require_type description computation_type target_type;
            let result =
              define description position result target_type computation_type
            in
            Apply_logical_not (input, result)
        | ( Binary_kind binary,
            ([ left_id; right_id ], Some result, Some target_type, None) ) ->
            let _ = checked_word description target_type in
            let left = operand description position left_id in
            let right = operand description position right_id in
            require_type description
              (promoted_type description left right)
              target_type;
            let result =
              define description position result target_type
                (Computation.forward target_type)
            in
            Apply_binary (binary, left, right, result)
        | ( Comparison_kind (signed, unsigned),
            ([ left_id; right_id ], Some result, Some target_type, None) ) ->
            if checked_word description target_type <> I64 then
              malformed description "comparison must declare internal I64";
            let left = operand description position left_id in
            let right = operand description position right_id in
            (* COM may declare I64 while forwarding U64. Select the condition
               from the operand computation classes before the comparison
               produces its independent I64 Boolean result. *)
            let condition =
              match
                checked_word description (promoted_type description left right)
              with
              | I64 -> signed
              | U64 -> unsigned
            in
            let result =
              define description position result target_type
                (Computation.forward target_type)
            in
            Apply_comparison (condition, left, right, result)
        | Return_value_kind, ([ operand_id ], None, Some target_type, None) ->
            let word = checked_word description target_type in
            let input = operand description position operand_id in
            require_type description input.declared_type target_type;
            return_type := Some word;
            Return_value input
        | Return_kind, ([], None, None, None) -> Return
        | _ -> shape_error ()
      in
      prepared := { operation; span = description.span } :: !prepared)
    instructions;
  match !return_type with
  | Some return_type -> (List.rev !prepared, return_type)
  | None -> reject "HCBACK0003" "native expression has no return value"

type allocation = {
  instructions : Encoder.instruction list;
  code_size : int;
  machine_count : int;
  peak : int;
}

let allocate ~max_code_bytes prepared =
  let registers = Array.of_list Encoder.registers in
  let owners : value option array = Array.make (Array.length registers) None in
  let planned = ref [] in
  let code_size = ref 0 in
  let machine_count = ref 0 in
  let peak = ref 0 in
  let emit span instruction =
    let size = Encoder.size instruction in
    if size > max_code_bytes - !code_size then
      reject ?span "HCBACK0005" "native expression exceeds max_code_bytes";
    code_size := !code_size + size;
    incr machine_count;
    planned := instruction :: !planned
  in
  let locate span value =
    let rec find index =
      if index = Array.length owners then
        reject ?span "HCBACK0003" "prepared operand has no live register";
      match owners.(index) with
      | Some owner when Sequence.Value_id.equal owner.value_id value.value_id ->
          index
      | Some _ | None -> find (index + 1)
    in
    find 0
  in
  let first_fit span position =
    let rec find index =
      if index = Array.length owners then
        reject ?span "HCBACK0004"
          "native expression needs more than seven live registers; spilling is \
           unsupported";
      match owners.(index) with
      | None -> index
      | Some owner when owner.last_use = position -> index
      | Some _ -> find (index + 1)
    in
    find 0
  in
  let note_peak () =
    let occupied =
      Array.fold_left
        (fun count owner -> if Option.is_some owner then count + 1 else count)
        0 owners
    in
    peak := max !peak occupied
  in
  let release_dead position =
    Array.iteri
      (fun index owner ->
        match owner with
        | Some value when value.last_use <= position -> owners.(index) <- None
        | Some _ | None -> ())
      owners
  in
  let assign position destination value =
    owners.(destination) <- Some value;
    note_peak ();
    release_dead position
  in
  List.iteri
    (fun position (instruction : prepared_instruction) ->
      let emit = emit instruction.span in
      match instruction.operation with
      | Load_immediate (result, bits) ->
          let destination = first_fit instruction.span position in
          emit (Encoder.Mov_imm64 (registers.(destination), bits));
          assign position destination result
      | Apply_unary (unary, input, result) ->
          let source = locate instruction.span input in
          let destination = first_fit instruction.span position in
          if destination <> source then
            emit (Encoder.Mov (registers.(destination), registers.(source)));
          emit (Encoder.Unary (unary, registers.(destination)));
          assign position destination result
      | Apply_binary (binary, left, right, result) ->
          (* Capture both source locations before changing any owner. This
             also covers the same value appearing in both operand positions. *)
          let left = locate instruction.span left in
          let right = locate instruction.span right in
          let destination = first_fit instruction.span position in
          let target = registers.(destination) in
          if destination = left then
            emit (Encoder.Binary (binary, target, registers.(right)))
          else if destination = right then (
            emit (Encoder.Binary (binary, target, registers.(left)));
            (* When only the right operand can be reused, -(right-left)
               preserves left-right modulo 2^64 without a temporary register.
               Flags are not values in this subset. *)
            if binary = Encoder.Sub then
              emit (Encoder.Unary (Encoder.Neg, target)))
          else (
            emit (Encoder.Mov (target, registers.(left)));
            emit (Encoder.Binary (binary, target, registers.(right))));
          assign position destination result
      | Apply_comparison (condition, left, right, result) ->
          let left = locate instruction.span left in
          let right = locate instruction.span right in
          let destination = first_fit instruction.span position in
          let target = registers.(destination) in
          (* Both values are still intact when CMP produces the flags. Only
             then may SETcc overwrite a dying input, including the right one.
             MOVZX clears every stale bit above the selected low byte. *)
          emit (Encoder.Cmp (registers.(left), registers.(right)));
          emit (Encoder.Setcc (condition, target));
          emit (Encoder.Movzx8 (target, target));
          assign position destination result
      | Apply_logical_not (input, result) ->
          let source = locate instruction.span input in
          let destination = first_fit instruction.span position in
          let target = registers.(destination) in
          emit (Encoder.Test registers.(source));
          emit (Encoder.Setcc (Encoder.E, target));
          emit (Encoder.Movzx8 (target, target));
          assign position destination result
      | Return_value input ->
          let source = locate instruction.span input in
          if registers.(source) <> Encoder.Rax then (
            (* RAX is the first register in the public allocation order. All
               other values have expired by the exact terminal return pair. *)
            if Option.is_some owners.(0) then
              reject ?span:instruction.span "HCBACK0003"
                "return register still owns a different live value";
            emit (Encoder.Mov (Encoder.Rax, registers.(source)));
            owners.(0) <- Some input;
            note_peak ());
          release_dead position
      | Return -> emit Encoder.Ret)
    prepared;
  {
    instructions = List.rev !planned;
    code_size = !code_size;
    machine_count = !machine_count;
    peak = !peak;
  }

let compile ~max_ir_instructions ~max_code_bytes verified =
  match validate_limits ~max_ir_instructions ~max_code_bytes with
  | Error errors -> Error errors
  | Ok () -> (
      try
        let block = single_block verified in
        let instructions = Graph.instructions block |> Sequence.instructions in
        (* Count with a bound before constructing any value or instruction
           maps. Sparse IDs never determine an allocation size. *)
        let ir_count = bounded_length ~max_ir_instructions instructions in
        let prepared, word_type = preflight ~count:ir_count instructions in
        let allocation = allocate ~max_code_bytes prepared in
        match Encoder.encode_all ~max_code_bytes allocation.instructions with
        | Error message -> reject "HCBACK0005" message
        | Ok encoded ->
            if String.length encoded <> allocation.code_size then
              reject "HCBACK0003"
                "encoded length does not match the allocation plan";
            Ok
              {
                encoded = Bytes.of_string encoded;
                word_type;
                ir_count;
                machine_count = allocation.machine_count;
                peak = allocation.peak;
              }
      with Rejected error -> Error [ error ])

let code (compiled : t) = Bytes.to_string compiled.encoded
let value_type (compiled : t) = compiled.word_type
let ir_instructions (compiled : t) = compiled.ir_count
let machine_instructions (compiled : t) = compiled.machine_count
let register_peak (compiled : t) = compiled.peak
