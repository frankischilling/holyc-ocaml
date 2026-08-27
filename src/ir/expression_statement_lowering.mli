type t
type lowering_result = Lowered of t | Unsupported_expression

val reference_commit : string

val lower_function_statement :
  instruction_id:Instruction_sequence.Instruction_id.t ->
  value_id:Instruction_sequence.Value_id.t ->
  Sema.Function_call_expression_result.expression_statement_result ->
  (lowering_result, Instruction_sequence.error list) result
(** Lower the checked value of a function expression statement and append one
    [IC_END_EXP] carrying [ICF_RES_NOT_USED]. The terminator consumes the final
    value, produces no value, and does not advance the value identity. *)

val lower_top_level_statement :
  instruction_id:Instruction_sequence.Instruction_id.t ->
  value_id:Instruction_sequence.Value_id.t ->
  Sema.Function_call_expression_result.top_level_root_result ->
  (lowering_result, Instruction_sequence.error list) result
(** Lower an executable top-level expression-statement root through the same
    checked terminator path. A root with any other semantic role is rejected. *)

val sequence : t -> Instruction_sequence.t
val expression_value : t -> Instruction_sequence.Value_id.t
val expression_type : t -> Sema.Type.t
val terminator_id : t -> Instruction_sequence.Instruction_id.t
val next_instruction_id : t -> Instruction_sequence.Instruction_id.t
val next_value_id : t -> Instruction_sequence.Value_id.t

val human : t -> string
(** Render the versioned deterministic expression-statement lowering form. *)
