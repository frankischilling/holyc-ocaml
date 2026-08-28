module Sequence = Instruction_sequence
module Expression = Expression_lowering
module Semantic_result = Sema.Function_call_expression_result
module Semantic_source = Sema.Function_call_resolution

type t = {
  sequence_ : Sequence.t;
  condition_value_ : Sequence.Value_id.t;
  condition_type_ : Sema.Type.t;
  branch_id_ : Sequence.Instruction_id.t;
  target_ : Sequence.Block_id.t;
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
      Error
        (metadata_error
           "function condition does not have a complete source location")

let next_instruction_id ~span instruction_id =
  let current = Sequence.Instruction_id.to_int instruction_id in
  if current = Int.max_int then
    Error
      (lowering_error ~span "HCIRL0005"
         "cannot allocate a condition branch identity because the host integer \
          range is exhausted")
  else Sequence.Instruction_id.of_int (current + 1)

let descriptions sequence =
  sequence |> Sequence.instructions |> List.map Sequence.description

let branch_opcode source =
  match Semantic_source.condition_role source with
  | Semantic_source.If_condition
  | Semantic_source.While_condition
  | Semantic_source.For_condition -> Opcode.Ic_br_zero
  | Semantic_source.Do_while_condition -> Opcode.Ic_br_not_zero

let append_branch ~span ~target source expression =
  let branch_id_ = Expression.next_instruction_id expression in
  match next_instruction_id ~span branch_id_ with
  | Error item -> Error [ item ]
  | Ok next_instruction_id_ -> (
      let condition_value_ = Expression.result_value expression in
      let branch : Sequence.description =
        {
          instruction_id = branch_id_;
          opcode = branch_opcode source;
          operands = [ condition_value_ ];
          result = None;
          target_type = None;
          payload = Some (Sequence.Block target);
          flags = 0L;
          span = Some span;
        }
      in
      let items = descriptions (Expression.sequence expression) @ [ branch ] in
      match Sequence.create items with
      | Error errors -> Error errors
      | Ok sequence_ ->
          Ok
            (Lowered
               {
                 sequence_;
                 condition_value_;
                 condition_type_ = Expression.result_type expression;
                 branch_id_;
                 target_ = target;
                 next_instruction_id_;
                 next_value_id_ = Expression.next_value_id expression;
               }))

let lower_function_condition ~instruction_id ~value_id ~target condition =
  let source = Semantic_result.condition_source condition in
  match span_of_origin (Semantic_source.condition_origin source) with
  | Error item -> Error [ item ]
  | Ok span -> (
      match
        Expression.lower_typed_result ~instruction_id ~value_id
          (Semantic_result.condition_value condition)
      with
      | Error errors -> Error errors
      | Ok Expression.Unsupported_expression -> Ok Unsupported_expression
      | Ok (Expression.Lowered expression) ->
          append_branch ~span ~target source expression)

let sequence lowered = lowered.sequence_
let condition_value lowered = lowered.condition_value_
let condition_type lowered = lowered.condition_type_
let branch_id lowered = lowered.branch_id_
let target lowered = lowered.target_
let next_instruction_id lowered = lowered.next_instruction_id_
let next_value_id lowered = lowered.next_value_id_

let human lowered =
  Printf.sprintf
    "holyc-ir-condition-v1 reference=%s\n\
     value=%%v%d value-type=%s branch=!i%d target=^b%d next-instruction=%d \
     next-value=%d\n\
     %s"
    reference_commit
    (Sequence.Value_id.to_int lowered.condition_value_)
    (Sequence.type_name lowered.condition_type_)
    (Sequence.Instruction_id.to_int lowered.branch_id_)
    (Sequence.Block_id.to_int lowered.target_)
    (Sequence.Instruction_id.to_int lowered.next_instruction_id_)
    (Sequence.Value_id.to_int lowered.next_value_id_)
    (Sequence.human_body lowered.sequence_)
