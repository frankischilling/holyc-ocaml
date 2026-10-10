type t

val of_location : Sema.Function_frame_layout.location -> t option
(** Admit nonempty scalar or array automatic aggregate storage from its exact
    checked frame location. Original dimensions multiply the selected element
    size; all contents use individual, initially unknown byte cells. *)

val element_size : t -> int
val byte_size : t -> int
val byte_type : Sema.Type.t
val byte_scalar : Integer_scalar_storage.t
val aggregate_pointer : Sema.Type.t -> bool
