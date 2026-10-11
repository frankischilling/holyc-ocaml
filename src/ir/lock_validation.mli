type t

val of_instructions : Instruction_sequence.instruction list -> t
val of_graph : Block_graph.t -> t

val scalar_prefix_invalid : t -> Instruction_sequence.description -> bool
(** Recognize the original backend's ordinary immediate ADD/SUB paths and AND
    tests consumed exclusively by a zero/nonzero branch. Literal proof comes
    from immutable instruction producers, never runtime values. Other scalar
    ADD/SUB/AND/OR/XOR operations require a memory destination. *)
