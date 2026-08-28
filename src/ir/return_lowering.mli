type t
type lowering_result = Lowered of t | Unsupported_expression

val reference_commit : string

val lower_function_return :
  instruction_id:Instruction_sequence.Instruction_id.t ->
  value_id:Instruction_sequence.Value_id.t ->
  leave:Instruction_sequence.Block_id.t ->
  Sema.Function_call_expression_result.return_result ->
  (lowering_result, Instruction_sequence.error list) result
(** Lower an accepted checked function return. A present value is followed by
    [IC_RETURN_VAL], and every return ends with [IC_JMP] to the caller-owned
    leave block. Epilogue placement and graph construction remain outside this
    fragment. *)

val sequence : t -> Instruction_sequence.t
val return_value : t -> Instruction_sequence.Value_id.t option
val return_type : t -> Sema.Type.t
val return_id : t -> Instruction_sequence.Instruction_id.t option
val jump_id : t -> Instruction_sequence.Instruction_id.t
val leave : t -> Instruction_sequence.Block_id.t
val next_instruction_id : t -> Instruction_sequence.Instruction_id.t
val next_value_id : t -> Instruction_sequence.Value_id.t
val human : t -> string
