type prepared_address

val prepare_initializer :
  globals:Integer_globals.t ->
  Sema.Function_call_expression_result.top_level_root_result ->
  (prepared_address, Instruction_sequence.error list) result

type t

val prepare :
  ?frame:Sema.Function_frame_layout.function_layout ->
  globals:Integer_globals.t ->
  Sema.Function_call_expression_result.expression_result ->
  (prepared_address option, Instruction_sequence.error list) result
(** Check a source-visible module global against the exact program object and
    its retained type, rank, publication and source occurrence. Static locals
    additionally require their exact declaring [frame] and retained binding. *)

val lower_prepared :
  instruction_id:Instruction_sequence.Instruction_id.t ->
  value_id:Instruction_sequence.Value_id.t ->
  prepared_address ->
  (t, Instruction_sequence.error list) result

val sequence : t -> Instruction_sequence.t
val result_value : t -> Instruction_sequence.Value_id.t
val result_type : t -> Sema.Type.t
val next_instruction_id : t -> Instruction_sequence.Instruction_id.t
val next_value_id : t -> Instruction_sequence.Value_id.t
