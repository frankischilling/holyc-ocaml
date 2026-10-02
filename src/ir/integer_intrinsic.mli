type unary = To_upper | To_bool | Absolute | Sign | Square_i64 | Square_u64

val unary : Opcode.t -> unary option
val supports : Opcode.t -> bool
val argument_matches : Opcode.t -> Sema.Type.t -> bool
val result_matches : Opcode.t -> Sema.Type.t -> bool

val apply : unary -> int64 -> int64
(** Full-word runtime operations from the pinned internal implementations. This
    does not grant constant-preparation or source-call authority. *)
