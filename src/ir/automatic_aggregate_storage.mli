type t

val of_location : Sema.Function_frame_layout.location -> t option
(** Admit one nonempty automatic aggregate object from its immutable checked
    frame location. Its contents use individual, initially unknown byte cells.
    This does not admit aggregate values, array decay, or member projections. *)

val byte_size : t -> int
val byte_type : Sema.Type.t
val byte_scalar : Integer_scalar_storage.t
val aggregate_pointer : Sema.Type.t -> bool
