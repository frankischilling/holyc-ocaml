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

module Block_map = Map.Make (struct
  type t = Block_id.t

  let compare = Block_id.compare
end)

type word_type = I64 | U64
type word = { type_ : word_type; bits : int64 }
type termination = Stream_end | Returned of word option
type error_stage = Configuration | Preflight | Execution

type error = {
  stage : error_stage;
  code : string;
  message : string;
  executed_steps : int;
  block_id : int option;
  instruction_id : int option;
  span : Common.Span.t option;
}

type t = { termination_ : termination; executed_steps_ : int }
type prepared_operand = { value_id : Value_id.t; expected_type : word_type }
type unary_operation = Complement | Logical_not | Negate

type binary_operation =
  | Add
  | Subtract
  | Multiply
  | Bitwise_and
  | Bitwise_or
  | Bitwise_xor
  | Shift_left
  | Shift_right

type branch_condition = Zero | Not_zero

type prepared_operation =
  | Immediate of Value_id.t * word
  | Unary of unary_operation * prepared_operand * Value_id.t * word_type
  | Binary of
      binary_operation
      * prepared_operand
      * prepared_operand
      * Value_id.t
      * word_type
  | Discard of prepared_operand
  | Return_value of prepared_operand
  | Jump of int
  | Branch of branch_condition * prepared_operand * int
  | Return
  | End

type prepared_instruction = {
  instruction_id : Instruction_id.t;
  span : Common.Span.t option;
  operation : prepared_operation;
}

type prepared_block = {
  block_id : Block_id.t;
  instructions : prepared_instruction array;
  fallthrough : int option;
}

type prepared = { blocks : prepared_block array; entry_index : int }

type opcode_kind =
  | Immediate_kind
  | Unary_kind of unary_operation
  | Binary_kind of binary_operation
  | Discard_kind
  | Return_value_kind
  | Jump_kind
  | Branch_kind of branch_condition
  | Return_kind
  | End_kind

type declared_type = Supported of word_type | Unsupported

let reference_commit = Sequence.reference_commit

let make_error ?block_id ?instruction_id ?span ~stage ~executed_steps code
    message =
  {
    stage;
    code;
    message;
    executed_steps;
    block_id = Option.map Block_id.to_int block_id;
    instruction_id = Option.map Instruction_id.to_int instruction_id;
    span;
  }

let preflight_error block_id (description : Sequence.description) code message =
  make_error ~stage:Preflight ~executed_steps:0 ~block_id
    ~instruction_id:description.instruction_id ?span:description.span code
    message

let opcode_kind = function
  | Opcode.Ic_imm_i64 -> Some Immediate_kind
  | Opcode.Ic_com -> Some (Unary_kind Complement)
  | Opcode.Ic_not -> Some (Unary_kind Logical_not)
  | Opcode.Ic_unary_minus -> Some (Unary_kind Negate)
  | Opcode.Ic_add -> Some (Binary_kind Add)
  | Opcode.Ic_sub -> Some (Binary_kind Subtract)
  | Opcode.Ic_mul -> Some (Binary_kind Multiply)
  | Opcode.Ic_and -> Some (Binary_kind Bitwise_and)
  | Opcode.Ic_or -> Some (Binary_kind Bitwise_or)
  | Opcode.Ic_xor -> Some (Binary_kind Bitwise_xor)
  | Opcode.Ic_shl -> Some (Binary_kind Shift_left)
  | Opcode.Ic_shr -> Some (Binary_kind Shift_right)
  | Opcode.Ic_end_exp -> Some Discard_kind
  | Opcode.Ic_return_val -> Some Return_value_kind
  | Opcode.Ic_jmp -> Some Jump_kind
  | Opcode.Ic_br_zero -> Some (Branch_kind Zero)
  | Opcode.Ic_br_not_zero -> Some (Branch_kind Not_zero)
  | Opcode.Ic_ret -> Some Return_kind
  | Opcode.Ic_end -> Some End_kind
  | _ -> None

let scalar_word_type ~allow_public type_ =
  if Sema.Type.pointer_depth type_ <> 0 then None
  else
    match Sema.Type.base type_ with
    | Sema.Type.Primitive (form, primitive)
      when (allow_public || form = Sema.Type.Internal_storage)
           && Sema.Primitive_type.equal primitive Sema.Primitive_type.I64 ->
        Some I64
    | Sema.Type.Primitive (form, primitive)
      when (allow_public || form = Sema.Type.Internal_storage)
           && Sema.Primitive_type.equal primitive Sema.Primitive_type.U64 ->
        Some U64
    | Sema.Type.Primitive _ | Sema.Type.Aggregate _ -> None

let producer_word_type type_ = scalar_word_type ~allow_public:false type_
let return_word_type type_ = scalar_word_type ~allow_public:true type_

let declared_types block =
  Graph.instructions block |> Sequence.instructions
  |> List.fold_left
       (fun types instruction ->
         let description = Sequence.description instruction in
         match description.result with
         | None -> types
         | Some result ->
             let declared =
               match description.target_type with
               | Some type_ -> (
                   match producer_word_type type_ with
                   | Some word_type -> Supported word_type
                   | None -> Unsupported)
               | None -> Unsupported
             in
             Value_map.add result.value_id declared types)
       Value_map.empty

let operand_of_value types value_id =
  match Value_map.find_opt value_id types with
  | Some (Supported expected_type) -> Some { value_id; expected_type }
  | Some Unsupported | None -> None

let expected_binary_type left right =
  match (left, right) with
  | I64, I64 -> I64
  | I64, U64 | U64, I64 | U64, U64 -> U64

let shift_count bits = Int64.to_int (Int64.logand bits 63L)

let malformed block_id description =
  preflight_error block_id description "HCIRVM0004"
    (Printf.sprintf "%s has malformed operands, result, target type, or payload"
       (Opcode.to_source_name description.Sequence.opcode))

let unsupported_type block_id description =
  preflight_error block_id description "HCIRVM0005"
    (Printf.sprintf "%s uses an unsupported word type"
       (Opcode.to_source_name description.Sequence.opcode))

let invalid_type_matrix block_id description =
  preflight_error block_id description "HCIRVM0006"
    (Printf.sprintf "%s has an invalid operand/result word-type relationship"
       (Opcode.to_source_name description.Sequence.opcode))

let prepare_instruction block_index types block_id
    (description : Sequence.description) =
  match opcode_kind description.opcode with
  | None ->
      Error
        (preflight_error block_id description "HCIRVM0002"
           (Printf.sprintf "%s is outside the bounded integer interpreter"
              (Opcode.to_source_name description.opcode)))
  | Some kind ->
      let required_flags =
        match kind with
        | Discard_kind -> 0x000000200L
        | _ -> 0L
      in
      if description.flags <> required_flags then
        Error
          (preflight_error block_id description "HCIRVM0003"
             (Printf.sprintf "%s requires flags=0x%09Lx"
                (Opcode.to_source_name description.opcode)
                required_flags))
      else
        let operation =
          match kind with
          | Immediate_kind -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [], Some result, Some type_, Some (Sequence.Integer bits) -> (
                  match producer_word_type type_ with
                  | Some type_ ->
                      Ok (Immediate (result.value_id, { type_; bits }))
                  | None -> Error (unsupported_type block_id description))
              | _ -> Error (malformed block_id description))
          | Unary_kind unary -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [ operand_id ], Some result, Some result_type, None -> (
                  match producer_word_type result_type with
                  | None -> Error (unsupported_type block_id description)
                  | Some result_type -> (
                      match operand_of_value types operand_id with
                      | None -> Error (invalid_type_matrix block_id description)
                      | Some operand ->
                          let valid =
                            match unary with
                            | Complement | Negate -> result_type = I64
                            | Logical_not -> result_type = operand.expected_type
                          in
                          if valid then
                            Ok
                              (Unary
                                 (unary, operand, result.value_id, result_type))
                          else Error (invalid_type_matrix block_id description))
                  )
              | _ -> Error (malformed block_id description))
          | Binary_kind binary -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [ left_id; right_id ], Some result, Some result_type, None -> (
                  match producer_word_type result_type with
                  | None -> Error (unsupported_type block_id description)
                  | Some result_type -> (
                      match
                        ( operand_of_value types left_id,
                          operand_of_value types right_id )
                      with
                      | Some left, Some right
                        when result_type
                             = expected_binary_type left.expected_type
                                 right.expected_type ->
                          Ok
                            (Binary
                               ( binary,
                                 left,
                                 right,
                                 result.value_id,
                                 result_type ))
                      | Some _, Some _ | None, _ | _, None ->
                          Error (invalid_type_matrix block_id description)))
              | _ -> Error (malformed block_id description))
          | Discard_kind -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [ operand_id ], None, None, None -> (
                  match operand_of_value types operand_id with
                  | Some operand -> Ok (Discard operand)
                  | None -> Error (invalid_type_matrix block_id description))
              | _ -> Error (malformed block_id description))
          | Return_value_kind -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [ operand_id ], None, Some target_type, None -> (
                  match return_word_type target_type with
                  | None -> Error (unsupported_type block_id description)
                  | Some target_type -> (
                      match operand_of_value types operand_id with
                      | Some operand when operand.expected_type = target_type ->
                          Ok (Return_value operand)
                      | Some _ | None ->
                          Error (invalid_type_matrix block_id description)))
              | _ -> Error (malformed block_id description))
          | Jump_kind -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [], None, None, Some (Sequence.Block target) -> (
                  match Block_map.find_opt target block_index with
                  | Some target -> Ok (Jump target)
                  | None -> Error (malformed block_id description))
              | _ -> Error (malformed block_id description))
          | Branch_kind condition -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [ operand_id ], None, None, Some (Sequence.Block target) -> (
                  match
                    ( operand_of_value types operand_id,
                      Block_map.find_opt target block_index )
                  with
                  | Some operand, Some target ->
                      Ok (Branch (condition, operand, target))
                  | None, _ -> Error (invalid_type_matrix block_id description)
                  | Some _, None -> Error (malformed block_id description))
              | _ -> Error (malformed block_id description))
          | Return_kind -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [], None, None, None -> Ok Return
              | _ -> Error (malformed block_id description))
          | End_kind -> (
              match
                ( description.operands,
                  description.result,
                  description.target_type,
                  description.payload )
              with
              | [], None, None, None -> Ok End
              | _ -> Error (malformed block_id description))
        in
        Result.map
          (fun operation ->
            {
              instruction_id = description.instruction_id;
              span = description.span;
              operation;
            })
          operation

let prepare graph =
  let source_blocks = Graph.blocks graph in
  let block_count = List.length source_blocks in
  let block_index =
    source_blocks
    |> List.mapi (fun index block -> (Graph.block_id block, index))
    |> List.fold_left
         (fun map (block_id, index) -> Block_map.add block_id index map)
         Block_map.empty
  in
  let errors_rev = ref [] in
  let blocks =
    source_blocks
    |> List.mapi (fun index block ->
        let block_id = Graph.block_id block in
        let types = declared_types block in
        let instructions_rev = ref [] in
        Graph.instructions block |> Sequence.instructions
        |> List.iter (fun instruction ->
            let description = Sequence.description instruction in
            match
              prepare_instruction block_index types block_id description
            with
            | Ok prepared -> instructions_rev := prepared :: !instructions_rev
            | Error error -> errors_rev := error :: !errors_rev);
        {
          block_id;
          instructions = Array.of_list (List.rev !instructions_rev);
          fallthrough =
            (if index + 1 < block_count then Some (index + 1) else None);
        })
    |> Array.of_list
  in
  match List.rev !errors_rev with
  | _ :: _ as errors -> Error errors
  | [] -> (
      let entry_id = Graph.entry graph |> Graph.block_id in
      match Block_map.find_opt entry_id block_index with
      | Some entry_index -> Ok { blocks; entry_index }
      | None ->
          Error
            [
              make_error ~stage:Preflight ~executed_steps:0 "HCIRVM0004"
                "the verified graph entry is unavailable";
            ])

let runtime_error ?instruction block executed_steps code message =
  match instruction with
  | None ->
      make_error ~stage:Execution ~executed_steps ~block_id:block.block_id code
        message
  | Some instruction ->
      make_error ~stage:Execution ~executed_steps ~block_id:block.block_id
        ~instruction_id:instruction.instruction_id ?span:instruction.span code
        message

let execute_prepared ~max_steps program =
  let current_block = ref program.entry_index in
  let current_instruction = ref 0 in
  let values = ref Value_map.empty in
  let pending_return = ref None in
  let steps = ref 0 in
  let completed = ref None in
  let failed = ref None in
  let transfer target =
    current_block := target;
    current_instruction := 0;
    values := Value_map.empty
  in
  let require_operand block instruction operand =
    match Value_map.find_opt operand.value_id !values with
    | Some word when word.type_ = operand.expected_type -> Some word
    | Some _ | None ->
        failed :=
          Some
            (runtime_error ~instruction block !steps "HCIRVM0008"
               "a prepared operand is unavailable or has the wrong word type");
        None
  in
  while Option.is_none !completed && Option.is_none !failed do
    if !current_block < 0 || !current_block >= Array.length program.blocks then
      failed :=
        Some
          (make_error ~stage:Execution ~executed_steps:!steps "HCIRVM0008"
             "the prepared block cursor is out of bounds")
    else
      let block = program.blocks.(!current_block) in
      if !current_instruction >= Array.length block.instructions then
        match block.fallthrough with
        | Some target -> transfer target
        | None ->
            failed :=
              Some
                (runtime_error block !steps "HCIRVM0008"
                   "execution reached an impossible final-block fallthrough")
      else
        let instruction = block.instructions.(!current_instruction) in
        if !steps >= max_steps then
          failed :=
            Some
              (runtime_error ~instruction block !steps "HCIRVM0007"
                 "the bounded integer execution step limit was exhausted")
        else (
          steps := !steps + 1;
          current_instruction := !current_instruction + 1;
          match instruction.operation with
          | Immediate (result, word) ->
              values := Value_map.add result word !values
          | Unary (operation, operand, result, result_type) -> (
              match require_operand block instruction operand with
              | None -> ()
              | Some operand ->
                  let bits =
                    match operation with
                    | Complement -> Int64.lognot operand.bits
                    | Logical_not ->
                        if Int64.equal operand.bits 0L then 1L else 0L
                    | Negate -> Int64.neg operand.bits
                  in
                  values :=
                    Value_map.add result { type_ = result_type; bits } !values)
          | Binary (operation, left, right, result, result_type) -> (
              match require_operand block instruction left with
              | None -> ()
              | Some left -> (
                  match require_operand block instruction right with
                  | None -> ()
                  | Some right ->
                      let bits =
                        match operation with
                        | Add -> Int64.add left.bits right.bits
                        | Subtract -> Int64.sub left.bits right.bits
                        | Multiply -> Int64.mul left.bits right.bits
                        | Bitwise_and -> Int64.logand left.bits right.bits
                        | Bitwise_or -> Int64.logor left.bits right.bits
                        | Bitwise_xor -> Int64.logxor left.bits right.bits
                        | Shift_left ->
                            Int64.shift_left left.bits (shift_count right.bits)
                        | Shift_right -> (
                            match result_type with
                            | I64 ->
                                Int64.shift_right left.bits
                                  (shift_count right.bits)
                            | U64 ->
                                Int64.shift_right_logical left.bits
                                  (shift_count right.bits))
                      in
                      values :=
                        Value_map.add result
                          { type_ = result_type; bits }
                          !values))
          | Discard operand ->
              ignore (require_operand block instruction operand)
          | Return_value operand -> (
              match require_operand block instruction operand with
              | Some word -> pending_return := Some word
              | None -> ())
          | Jump target -> transfer target
          | Branch (condition, operand, target) -> (
              match require_operand block instruction operand with
              | None -> ()
              | Some word -> (
                  let is_zero = Int64.equal word.bits 0L in
                  let take_target =
                    match condition with
                    | Zero -> is_zero
                    | Not_zero -> not is_zero
                  in
                  if take_target then transfer target
                  else
                    match block.fallthrough with
                    | Some fallthrough -> transfer fallthrough
                    | None ->
                        failed :=
                          Some
                            (runtime_error ~instruction block !steps
                               "HCIRVM0008"
                               "a conditional branch has no physical \
                                fallthrough")))
          | Return -> completed := Some (Returned !pending_return)
          | End -> completed := Some Stream_end)
  done;
  match (!failed, !completed) with
  | Some error, _ -> Error [ error ]
  | None, Some termination ->
      Ok { termination_ = termination; executed_steps_ = !steps }
  | None, None ->
      Error
        [
          make_error ~stage:Execution ~executed_steps:!steps "HCIRVM0008"
            "bounded integer execution stopped without a result";
        ]

let execute ~max_steps checked =
  if max_steps <= 0 then
    Error
      [
        make_error ~stage:Configuration ~executed_steps:0 "HCIRVM0001"
          "max_steps must be greater than zero";
      ]
  else
    match prepare (X87.graph checked) with
    | Error errors -> Error errors
    | Ok program -> execute_prepared ~max_steps program

let termination execution = execution.termination_
let executed_steps execution = execution.executed_steps_

let word_type_name = function
  | I64 -> "i64"
  | U64 -> "u64"

let termination_name = function
  | Stream_end -> "stream-end"
  | Returned None -> "returned:none"
  | Returned (Some word) ->
      Printf.sprintf "returned:%s:0x%016Lx"
        (word_type_name word.type_)
        word.bits

let human execution =
  Printf.sprintf
    "holyc-ir-integer-execution-v1 reference=%s\nsteps=%d\ntermination=%s\n"
    reference_commit execution.executed_steps_
    (termination_name execution.termination_)
