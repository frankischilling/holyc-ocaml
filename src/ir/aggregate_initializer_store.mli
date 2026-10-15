type t

val instruction_count : Sema.Initializer_type.scalar -> int

val lower :
  scalar:Sema.Initializer_type.scalar ->
  byte_offset:int ->
  root_type:Sema.Type.t ->
  root:Instruction_sequence.Value_id.t ->
  word:Instruction_sequence.Value_id.t ->
  span:Common.Span.t option ->
  instruction_id:Instruction_sequence.Instruction_id.t ->
  value_id:Instruction_sequence.Value_id.t ->
  (t, Instruction_sequence.error list) result

val descriptions : t -> Instruction_sequence.description list
val result_value : t -> Instruction_sequence.Value_id.t
val result_type : t -> Sema.Type.t
val next_instruction_id : t -> Instruction_sequence.Instruction_id.t

val next_value_id : t -> Instruction_sequence.Value_id.t
(** Emit a typed source leaf through an owned byte view. The original layout
    validates the full byte window; these ordinary pointer operations preserve
    the root reference and its runtime bounds. *)

val copy_instruction_count : string -> int

val lower_copy :
  bytes:string ->
  byte_offset:int ->
  root_type:Sema.Type.t ->
  root:Instruction_sequence.Value_id.t ->
  span:Common.Span.t option ->
  instruction_id:Instruction_sequence.Instruction_id.t ->
  value_id:Instruction_sequence.Value_id.t ->
  (t, Instruction_sequence.error list) result
