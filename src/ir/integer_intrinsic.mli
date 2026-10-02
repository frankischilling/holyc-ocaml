type unary =
  | To_upper
  | To_bool
  | Absolute
  | Sign
  | Square_i64
  | Square_u64
  | Scan_forward
  | Scan_reverse

type binary = Min_i64 | Min_u64 | Max_i64 | Max_u64

val unary : Opcode.t -> unary option
val binary : Opcode.t -> binary option
val arity : Opcode.t -> int option
val supports : Opcode.t -> bool
val argument_matches : Opcode.t -> Sema.Type.t -> bool
val result_matches : Opcode.t -> Sema.Type.t -> bool

val apply : unary -> int64 -> int64
(** Full-word runtime operations from the pinned internal implementations. This
    does not grant constant-preparation or source-call authority. *)

val apply_binary : binary -> int64 -> int64 -> int64
(** Full-word signed/unsigned selection. Equal words retain the first value. *)
