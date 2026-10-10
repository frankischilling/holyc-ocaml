type t

val of_location : Sema.Function_frame_layout.location -> t option
(** Read scalar or array automatic aggregate storage from its exact checked
    frame location. Locals require nonempty declared extents. A named class
    parameter has eight byte cells independently of its nominal element size;
    its execution consumer must also prove the body's original class ABI.
    Original dimensions multiply the local element size. *)

val element_size : t -> int
val byte_size : t -> int
val byte_type : Sema.Type.t
val byte_scalar : Integer_scalar_storage.t
val aggregate_pointer : Sema.Type.t -> bool
