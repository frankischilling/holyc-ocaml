type conversion = To_integer | To_float

val conversion : Opcode.t -> conversion option
val arity : Opcode.t -> int option
val supports : Opcode.t -> bool
val argument_matches : Opcode.t -> index:int -> Sema.Type.t -> bool

val result_matches : Opcode.t -> Sema.Type.t -> bool
(** Exact declaration metadata for the original conversion opcodes. This does
    not authorize a source call, evaluate a conversion, or reinterpret bits. *)

val runtime_supports : Opcode.t -> bool
val runtime_arity : Opcode.t -> int option
val runtime_argument_matches : Opcode.t -> index:int -> Sema.Type.t -> bool

val runtime_result_matches : Opcode.t -> Sema.Type.t -> bool
(** Combined metadata for sealed runtime-call consumers. Integer-only execution
    and preparation consumers retain [Integer_intrinsic]'s narrower admission.
*)
