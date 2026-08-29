val schema : string

val human : Common.Source_manager.t -> Function_frame_layout.t -> string
(** Render completed function layouts and binding locations in source order.
    Static bindings have no frame slot. Displacements and sizes are signed
    64-bit target values. *)

val to_yojson :
  Common.Source_manager.t -> Function_frame_layout.t -> Yojson.Safe.t

val json : Common.Source_manager.t -> Function_frame_layout.t -> string
