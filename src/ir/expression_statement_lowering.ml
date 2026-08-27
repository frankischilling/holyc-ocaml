module Sequence = Instruction_sequence
module Expression = Expression_lowering
module Semantic_result = Sema.Function_call_expression_result
module Semantic_source = Sema.Function_call_resolution
module Top_level_source = Sema.Top_level_expression_tree

type t = {
  sequence_ : Sequence.t;
  expression_value_ : Sequence.Value_id.t;
  expression_type_ : Sema.Type.t;
  terminator_id_ : Sequence.Instruction_id.t;
  next_instruction_id_ : Sequence.Instruction_id.t;
  next_value_id_ : Sequence.Value_id.t;
}

type lowering_result = Lowered of t | Unsupported_expression

let reference_commit = Opcode.reference_commit
let result_not_used_flag = 0x000000200L

let lowering_error ?span code message =
  { Sequence.code; message; instruction_id = None; span }

let metadata_error ?span message = lowering_error ?span "HCIRL0004" message

let span_of_origin description = function
  | Sema.Symbol.Source_location location -> Ok location.span
  | Sema.Symbol.Pinned_source _ | Sema.Symbol.Synthesized _ ->
      Error
        (metadata_error
           (Printf.sprintf "%s does not have a source location" description))

let next_instruction_id ~span instruction_id =
  let current = Sequence.Instruction_id.to_int instruction_id in
  if current = Int.max_int then
    Error
      (lowering_error ~span "HCIRL0005"
         "cannot allocate another statement instruction identity because the \
          host integer range is exhausted")
  else Sequence.Instruction_id.of_int (current + 1)

let descriptions sequence =
  sequence |> Sequence.instructions |> List.map Sequence.description

let append_terminator ~span expression =
  let terminator_id_ = Expression.next_instruction_id expression in
  match next_instruction_id ~span terminator_id_ with
  | Error item -> Error [ item ]
  | Ok next_instruction_id_ -> (
      let expression_value_ = Expression.result_value expression in
      let terminator : Sequence.description =
        {
          instruction_id = terminator_id_;
          opcode = Opcode.Ic_end_exp;
          operands = [ expression_value_ ];
          result = None;
          target_type = None;
          payload = None;
          flags = result_not_used_flag;
          span = Some span;
        }
      in
      let items =
        descriptions (Expression.sequence expression) @ [ terminator ]
      in
      match Sequence.create items with
      | Error errors -> Error errors
      | Ok sequence_ ->
          Ok
            (Lowered
               {
                 sequence_;
                 expression_value_;
                 expression_type_ = Expression.result_type expression;
                 terminator_id_;
                 next_instruction_id_;
                 next_value_id_ = Expression.next_value_id expression;
               }))

let lower ~instruction_id ~value_id ~span value =
  match Expression.lower_typed_result ~instruction_id ~value_id value with
  | Error errors -> Error errors
  | Ok Expression.Unsupported_expression -> Ok Unsupported_expression
  | Ok (Expression.Lowered expression) -> append_terminator ~span expression

let lower_function_statement ~instruction_id ~value_id statement =
  let source = Semantic_result.expression_statement_source statement in
  match
    span_of_origin "function expression statement"
      (Semantic_source.expression_statement_origin source)
  with
  | Error item -> Error [ item ]
  | Ok span -> (
      match Semantic_result.expression_statement_result_use statement with
      | Semantic_result.Result_not_used ->
          lower ~instruction_id ~value_id ~span
            (Semantic_result.expression_statement_value statement))

let lower_top_level_statement ~instruction_id ~value_id root =
  let source = Semantic_result.top_level_root_source root in
  let span =
    span_of_origin "top-level expression statement"
      (Top_level_source.root_origin source)
  in
  match
    ( Top_level_source.root_role source,
      Semantic_result.top_level_root_result_use root,
      span )
  with
  | ( Top_level_source.Expression_statement _,
      Some Semantic_result.Result_not_used,
      Ok span ) ->
      lower ~instruction_id ~value_id ~span
        (Semantic_result.top_level_root_value root)
  | _, _, Error item -> Error [ item ]
  | Top_level_source.Expression_statement _, None, Ok span ->
      Error
        [
          metadata_error ~span
            "top-level expression statement is missing unused-result intent";
        ]
  | ( ( Top_level_source.Implicit_output_fixed _
      | Top_level_source.Implicit_output_argument _
      | Top_level_source.Condition _
      | Top_level_source.Switch_selector _
      | Top_level_source.Switch_case_value _
      | Top_level_source.Local_array_dimension _
      | Top_level_source.Local_initializer _
      | Top_level_source.Return_value _ ),
      _,
      Ok span ) ->
      Error
        [
          metadata_error ~span
            "top-level root is not an unused expression statement";
        ]

let sequence lowered = lowered.sequence_
let expression_value lowered = lowered.expression_value_
let expression_type lowered = lowered.expression_type_
let terminator_id lowered = lowered.terminator_id_
let next_instruction_id lowered = lowered.next_instruction_id_
let next_value_id lowered = lowered.next_value_id_

let human lowered =
  Printf.sprintf
    "holyc-ir-expression-statement-v1 reference=%s\n\
     value=%%v%d value-type=%s terminator=!i%d next-instruction=%d next-value=%d\n\
     %s"
    reference_commit
    (Sequence.Value_id.to_int lowered.expression_value_)
    (Sequence.type_name lowered.expression_type_)
    (Sequence.Instruction_id.to_int lowered.terminator_id_)
    (Sequence.Instruction_id.to_int lowered.next_instruction_id_)
    (Sequence.Value_id.to_int lowered.next_value_id_)
    (Sequence.human_body lowered.sequence_)
