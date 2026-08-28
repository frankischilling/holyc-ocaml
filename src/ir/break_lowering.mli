type t

module Region_id = Sema.Break_resolution.Region_id
module Block_id = Instruction_sequence.Block_id

type region_block = Region_id.t * Block_id.t

val reference_commit : string

val lower_function_breaks :
  instruction_id:Instruction_sequence.Instruction_id.t ->
  block_id:Instruction_sequence.Block_id.t ->
  Sema.Break_resolution.resolved_function ->
  (t, Instruction_sequence.error list) result
(** Assign stable blocks to break regions, then lower resolved breaks in
    occurrence order. *)

val sequence : t -> Instruction_sequence.t
val region_blocks : t -> region_block list
val next_instruction_id : t -> Instruction_sequence.Instruction_id.t
val next_block_id : t -> Instruction_sequence.Block_id.t

val human : t -> string
(** Render the versioned deterministic break-lowering form. *)
