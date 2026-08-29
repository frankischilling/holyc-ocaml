type error = {
  code : string;
  message : string;
  statement_index : int option;
  stream_id : int option;
  item_position : int option;
  block_id : int option;
  instruction_id : int option;
  span : Common.Span.t option;
}

type t
type lowering_result = Lowered of t | Unsupported_batch

val reference_commit : string

val lower_direct_calls :
  stream_id:Top_level_body.Stream_id.t ->
  block_id:Instruction_sequence.Block_id.t ->
  instruction_id:Instruction_sequence.Instruction_id.t ->
  value_id:Instruction_sequence.Value_id.t ->
  compiler_options:int64 list ->
  targets:Sema.Top_level_function_call_target_classification.t list ->
  Sema.Function_call_expression_result.top_level_statement_result list ->
  (lowering_result, error list) result
(** Lower an exact source-ordered batch of standalone direct-call statements.
    Input counts must agree. No body is exposed unless the complete batch
    succeeds. *)

val lower_expressions :
  stream_id:Top_level_body.Stream_id.t ->
  block_id:Instruction_sequence.Block_id.t ->
  instruction_id:Instruction_sequence.Instruction_id.t ->
  value_id:Instruction_sequence.Value_id.t ->
  compiler_options:int64 list ->
  Sema.Function_call_expression_result.top_level_statement_result list ->
  (lowering_result, error list) result
(** Lower an exact source-ordered batch of ordinary expression statements. The
    statement and compiler-option counts must agree. No body is exposed unless
    the complete batch succeeds. *)

val bodies : t -> Top_level_body.t list
val next_stream_id : t -> Top_level_body.Stream_id.t
val next_block_id : t -> Instruction_sequence.Block_id.t
val next_instruction_id : t -> Instruction_sequence.Instruction_id.t
val next_value_id : t -> Instruction_sequence.Value_id.t

val human : t -> string
(** Render the ordered bodies and next unused identities. *)
