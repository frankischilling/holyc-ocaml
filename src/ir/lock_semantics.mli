val flag : int64
(** Original [ICF_LOCK] from KernelA.HH. *)

val has_flag : int64 -> bool
val clear_flag : int64 -> int64

val update_supported : Opcode.t -> bool
(** Memory updates whose original integer backend emits a LOCK prefix.
    Classification follows source optimization, so a remainder assignment
    reduced to [IC_AND_EQU] belongs to this family. *)

val is_implicit_bit : Opcode.t -> bool
(** Explicit LBts/LBtr/LBtc intrinsics request LOCK without a lexical region. *)

val scalar_prefix_invalid : Opcode.t -> bool
(** Residual scalar forms whose original backend would prefix a register
    destination. Address formation is classified separately by its checked
    operands and does not use this scalar path. *)
