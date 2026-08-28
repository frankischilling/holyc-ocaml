type t
type lowering_result = Lowered of t | Unsupported_expression

val reference_commit : string

val lower_function_condition :
  instruction_id:Instruction_sequence.Instruction_id.t ->
  value_id:Instruction_sequence.Value_id.t ->
  target:Instruction_sequence.Block_id.t ->
  Sema.Function_call_expression_result.condition_result ->
  (lowering_result, Instruction_sequence.error list) result
(** Lower an accepted checked function condition and append its source-selected
    branch. [if], [while], and [for] use [IC_BR_ZERO]; [do...while] uses
    [IC_BR_NOT_ZERO]. The branch consumes the exact expression result and keeps
    the caller-owned block target. Statement placement and graph construction
    remain outside this fragment. *)

val lower_top_level_condition :
  instruction_id:Instruction_sequence.Instruction_id.t ->
  value_id:Instruction_sequence.Value_id.t ->
  target:Instruction_sequence.Block_id.t ->
  Sema.Top_level_condition_result.condition ->
  (lowering_result, Instruction_sequence.error list) result
(** Lower an accepted checked executable top-level condition and append its
    retained zero or nonzero branch test. The branch consumes the exact root
    value and keeps the caller-owned block target. Stream placement and graph
    construction remain outside this fragment. *)

val sequence : t -> Instruction_sequence.t
val condition_value : t -> Instruction_sequence.Value_id.t
val condition_type : t -> Sema.Type.t
val branch_id : t -> Instruction_sequence.Instruction_id.t
val target : t -> Instruction_sequence.Block_id.t
val next_instruction_id : t -> Instruction_sequence.Instruction_id.t
val next_value_id : t -> Instruction_sequence.Value_id.t
val human : t -> string
