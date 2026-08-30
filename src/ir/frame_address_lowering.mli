type t
type lowering_result = Lowered of t | Unsupported_location

val reference_commit : string

val lower :
  instruction_id:Instruction_sequence.Instruction_id.t ->
  value_id:Instruction_sequence.Value_id.t ->
  frame:Sema.Function_frame_layout.function_layout ->
  Sema.Function_call_expression_result.expression_result ->
  (lowering_result, Instruction_sequence.error list) result
(** Lower the checked address of one frame-slot-backed bound identifier. The
    result is an address fragment; loading or otherwise consuming that address
    remains a separate lowering step. Unsupported storage and expression forms
    return [Unsupported_location] without a sequence. Inconsistent semantic or
    frame evidence reports [HCIRL0004], and identity exhaustion reports
    [HCIRL0005]. *)

val sequence : t -> Instruction_sequence.t
val result_value : t -> Instruction_sequence.Value_id.t
val result_type : t -> Sema.Type.t
val next_instruction_id : t -> Instruction_sequence.Instruction_id.t
val next_value_id : t -> Instruction_sequence.Value_id.t
val human : t -> string
