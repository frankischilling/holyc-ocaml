type t

val of_type : Sema.Type.t -> t option
(** Nonzero scalar integer storage, using generated primitive metadata. *)

val byte_size : t -> int

val compatible_pointer : Sema.Type.t -> Sema.Type.t -> bool
(** Checked scalar pointer conversion, including the I64i/I64 cell view used by
    synthetic argv. KernelA.HH declares I64 as an I64i union. This admits
    one-level I64 pointers at conversion boundaries only; exact producer and
    object ownership checks still use Type.equal. *)

val is_unsigned : t -> bool
val normalize : t -> int64 -> int64

val bounds : t -> (int64 * int64) option
(** Signed host-representable bounds; U64 has no such interval. *)

val fits : t -> int64 -> bool

val public_byte_size : Sema.Type.t -> int option
(** Declared width for admitted public nonzero scalar integer storage types. *)

val narrow_bits : Sema.Type.t -> int64 -> int64
(** Normalize a stored integer payload to its declared width and signedness.
    This does not admit a type or convert an expression's register value. *)
