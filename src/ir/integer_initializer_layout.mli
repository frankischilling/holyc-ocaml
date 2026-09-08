type operation = Scalar_store | Copy_bytes of string
type entry
type t

val create :
  shape:Integer_storage_shape.t ->
  Sema.Initializer_source.t ->
  (t, string) result
(** Consume the retained initializer delimiters with the pinned fixed-array
    recursion. No missing-element fill or inferred dimensions are synthesized.
    Copied strings retain only source-owned bytes, including the terminator. *)

val source : t -> Sema.Initializer_source.t
val entries : t -> entry list
val leaf : entry -> Sema.Initializer_source.leaf
val cell_offset : entry -> int
val byte_offset : entry -> int
val operation : entry -> operation
val find : t -> Sema.Initializer_source.leaf -> entry option
