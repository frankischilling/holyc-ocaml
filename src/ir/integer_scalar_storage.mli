val public_byte_size : Sema.Type.t -> int option
(** Declared width for the admitted public scalar I64/U64/U8 storage types.
    Pointers and every other class remain outside this persistent-storage seam.
*)

val narrow_bits : Sema.Type.t -> int64 -> int64
(** Store the low byte for an admitted U8 destination; otherwise retain the
    payload. This does not admit a type or convert an expression's register
    value. *)
