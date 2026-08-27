type t
type lowering_result = Lowered of t | Unsupported_expression

val reference_commit : string

val lower_typed_result :
  instruction_id:Instruction_sequence.Instruction_id.t ->
  value_id:Instruction_sequence.Value_id.t ->
  Sema.Function_call_expression_result.expression_result ->
  (lowering_result, Instruction_sequence.error list) result
(** Lower an integer or character literal-backed semantic expression tree. The
    module owns source-order traversal and consecutive identity allocation.
    Expressions outside the implemented tree shapes return
    [Unsupported_expression] without returning a partial sequence. *)

val sequence : t -> Instruction_sequence.t
val result_value : t -> Instruction_sequence.Value_id.t
val result_type : t -> Sema.Type.t
val next_instruction_id : t -> Instruction_sequence.Instruction_id.t
val next_value_id : t -> Instruction_sequence.Value_id.t

val human : t -> string
(** Render the versioned deterministic expression-lowering form. *)
