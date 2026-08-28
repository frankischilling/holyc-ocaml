module Sequence = Instruction_sequence
module Expression = Expression_lowering
module Semantic_result = Sema.Function_call_expression_result
module Semantic_source = Sema.Function_call_resolution

type t = {
  sequence_ : Sequence.t;
  return_value_ : Sequence.Value_id.t option;
  return_type_ : Sema.Type.t;
  return_id_ : Sequence.Instruction_id.t option;
  jump_id_ : Sequence.Instruction_id.t;
  leave_ : Sequence.Block_id.t;
  next_instruction_id_ : Sequence.Instruction_id.t;
  next_value_id_ : Sequence.Value_id.t;
}

type lowering_result = Lowered of t | Unsupported_expression

let reference_commit = Opcode.reference_commit

let lowering_error ?span code message =
  { Sequence.code; message; instruction_id = None; span }

let metadata_error ?span message = lowering_error ?span "HCIRL0004" message

let span_of_origin = function
  | Sema.Symbol.Source_location location -> Ok location.span
  | Sema.Symbol.Pinned_source _ | Sema.Symbol.Synthesized _ ->
      let message = "function return has no complete source location" in
      Error (metadata_error message)

let next_instruction_id ~span description instruction_id =
  let current = Sequence.Instruction_id.to_int instruction_id in
  if current = Int.max_int then
    Error
      (lowering_error ~span "HCIRL0005"
         ("cannot allocate " ^ description
        ^ " because the host integer range is exhausted"))
  else Sequence.Instruction_id.of_int (current + 1)

let descriptions sequence =
  sequence |> Sequence.instructions |> List.map Sequence.description

let jump_description ~span ~instruction_id ~leave =
  {
    Sequence.instruction_id;
    opcode = Opcode.Ic_jmp;
    operands = [];
    result = None;
    target_type = None;
    payload = Some (Sequence.Block leave);
    flags = 0L;
    span = Some span;
  }

let create_result ~return_type_ ~return_value_ ~return_id_ ~jump_id_ ~leave_
    ~next_instruction_id_ ~next_value_id_ items =
  match Sequence.create items with
  | Error errors -> Error errors
  | Ok sequence_ ->
      Ok
        (Lowered
           {
             sequence_;
             return_value_;
             return_type_;
             return_id_;
             jump_id_;
             leave_;
             next_instruction_id_;
             next_value_id_;
           })

let lower_without_value ~span ~instruction_id ~value_id ~leave return_type_ =
  match
    next_instruction_id ~span "the instruction after a return jump"
      instruction_id
  with
  | Error item -> Error [ item ]
  | Ok next_instruction_id_ ->
      let jump = jump_description ~span ~instruction_id ~leave in
      create_result ~return_type_ ~return_value_:None ~return_id_:None
        ~jump_id_:instruction_id ~leave_:leave ~next_instruction_id_
        ~next_value_id_:value_id [ jump ]

let allocate_return_tail ~span return_id =
  match next_instruction_id ~span "a function return jump" return_id with
  | Error item -> Error item
  | Ok jump_id_ -> (
      match
        next_instruction_id ~span "the instruction after a return jump" jump_id_
      with
      | Error item -> Error item
      | Ok next_instruction_id_ -> Ok (jump_id_, next_instruction_id_))

let lower_with_value ~span ~instruction_id ~value_id ~leave return_type_ value =
  match Expression.lower_typed_result ~instruction_id ~value_id value with
  | Error errors -> Error errors
  | Ok Expression.Unsupported_expression -> Ok Unsupported_expression
  | Ok (Expression.Lowered expression) -> (
      let return_id = Expression.next_instruction_id expression in
      match allocate_return_tail ~span return_id with
      | Error item -> Error [ item ]
      | Ok (jump_id_, next_instruction_id_) ->
          let return_value = Expression.result_value expression in
          let return_instruction : Sequence.description =
            {
              instruction_id = return_id;
              opcode = Opcode.Ic_return_val;
              operands = [ return_value ];
              result = None;
              target_type = Some return_type_;
              payload = None;
              flags = 0L;
              span = Some span;
            }
          in
          let jump = jump_description ~span ~instruction_id:jump_id_ ~leave in
          let items =
            descriptions (Expression.sequence expression)
            @ [ return_instruction; jump ]
          in
          create_result ~return_type_ ~return_value_:(Some return_value)
            ~return_id_:(Some return_id) ~jump_id_ ~leave_:leave
            ~next_instruction_id_
            ~next_value_id_:(Expression.next_value_id expression)
            items)

let lower_return_value ~span ~instruction_id ~value_id ~leave return_type =
  function
  | None ->
      lower_without_value ~span ~instruction_id ~value_id ~leave return_type
  | Some value ->
      lower_with_value ~span ~instruction_id ~value_id ~leave return_type value

let lower_function_return ~instruction_id ~value_id ~leave return_ =
  let source = Semantic_result.return_source return_ in
  match span_of_origin (Semantic_source.return_origin source) with
  | Error item -> Error [ item ]
  | Ok span ->
      let return_type = Semantic_result.return_declared_type return_ in
      let value = Semantic_result.return_value return_ in
      let lower = lower_return_value ~span ~instruction_id ~value_id ~leave in
      lower return_type value

let sequence lowered = lowered.sequence_
let return_value lowered = lowered.return_value_
let return_type lowered = lowered.return_type_
let return_id lowered = lowered.return_id_
let jump_id lowered = lowered.jump_id_
let leave lowered = lowered.leave_
let next_instruction_id lowered = lowered.next_instruction_id_
let next_value_id lowered = lowered.next_value_id_

let optional_value_name = function
  | None -> "none"
  | Some value -> Printf.sprintf "%%v%d" (Sequence.Value_id.to_int value)

let optional_instruction_name = function
  | None -> "none"
  | Some instruction ->
      Printf.sprintf "!i%d" (Sequence.Instruction_id.to_int instruction)

let human lowered =
  Printf.sprintf
    "holyc-ir-return-v1 reference=%s\n\
     value=%s return-type=%s return=%s jump=!i%d leave=^b%d \
     next-instruction=%d next-value=%d\n\
     %s"
    reference_commit
    (optional_value_name lowered.return_value_)
    (Sequence.type_name lowered.return_type_)
    (optional_instruction_name lowered.return_id_)
    (Sequence.Instruction_id.to_int lowered.jump_id_)
    (Sequence.Block_id.to_int lowered.leave_)
    (Sequence.Instruction_id.to_int lowered.next_instruction_id_)
    (Sequence.Value_id.to_int lowered.next_value_id_)
    (Sequence.human_body lowered.sequence_)
