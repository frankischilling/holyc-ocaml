type t
type label_block = Sema.Symbol.t * Instruction_sequence.Block_id.t

val reference_commit : string

val lower_function_labels :
  instruction_id:Instruction_sequence.Instruction_id.t ->
  block_id:Instruction_sequence.Block_id.t ->
  Sema.Label_resolution.resolved_function ->
  (t, Instruction_sequence.error list) result
(** Assign stable blocks to uniquely defined language labels, then lower the
    retained label and goto occurrences in source order. Assembly label forms
    and repeated definitions remain outside this fragment. *)

val sequence : t -> Instruction_sequence.t
val label_blocks : t -> label_block list
val next_instruction_id : t -> Instruction_sequence.Instruction_id.t
val next_block_id : t -> Instruction_sequence.Block_id.t

val human : t -> string
(** Render the versioned deterministic goto and label lowering form. *)
