type t
type lowering_result = Lowered of t | Unsupported_call

val reference_commit : string

val lower :
  instruction_id:Instruction_sequence.Instruction_id.t ->
  value_id:Instruction_sequence.Value_id.t ->
  target:Sema.Function_call_target_classification.t ->
  Sema.Function_call_expression_result.expression_result ->
  (lowering_result, Instruction_sequence.error list) result
(** Lower one checked, nonvariadic direct executable call with zero parameters
    or one provided fixed argument supported by [Expression_lowering]. *)

val sequence : t -> Instruction_sequence.t
val result_value : t -> Instruction_sequence.Value_id.t
val result_type : t -> Sema.Type.t
val next_instruction_id : t -> Instruction_sequence.Instruction_id.t
val next_value_id : t -> Instruction_sequence.Value_id.t
val human : t -> string
