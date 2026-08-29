type error = {
  code : string;
  message : string;
  stream_id : int;
  item_position : int;
  block_id : int option;
  instruction_id : int option;
  span : Common.Span.t option;
}

type t
type lowering_result = Lowered of t | Unsupported_statement

val reference_commit : string

val lower_expression :
  stream_id:Top_level_body.Stream_id.t ->
  block_id:Instruction_sequence.Block_id.t ->
  instruction_id:Instruction_sequence.Instruction_id.t ->
  value_id:Instruction_sequence.Value_id.t ->
  compiler_options:int64 ->
  Sema.Function_call_expression_result.top_level_statement_result ->
  (lowering_result, error list) result
(** Lower one exact ordinary expression statement, append the stream-end marker,
    and construct a checked one-block top-level body. *)

val lower_direct_call :
  stream_id:Top_level_body.Stream_id.t ->
  block_id:Instruction_sequence.Block_id.t ->
  instruction_id:Instruction_sequence.Instruction_id.t ->
  value_id:Instruction_sequence.Value_id.t ->
  compiler_options:int64 ->
  target:Sema.Top_level_function_call_target_classification.t ->
  Sema.Function_call_expression_result.top_level_statement_result ->
  (lowering_result, error list) result
(** Lower one exact standalone direct-call statement through the same body
    constructor used by [lower_expression]. *)

val body : t -> Top_level_body.t
val end_instruction_id : t -> Instruction_sequence.Instruction_id.t
val next_instruction_id : t -> Instruction_sequence.Instruction_id.t
val next_value_id : t -> Instruction_sequence.Value_id.t

val human : t -> string
(** Render the checked top-level body and its next unused identities. *)
