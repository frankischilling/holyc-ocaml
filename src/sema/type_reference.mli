type t
(** A source type spelling resolved to a primitive, intrinsic storage type, or
    aggregate identity. *)

val make :
  spelling:string ->
  spelling_origin:Symbol.origin ->
  pointer_origins:Symbol.origin list ->
  resolved_type:Type.t ->
  (t, string) result

val spelling : t -> string
val spelling_origin : t -> Symbol.origin
val pointer_origins : t -> Symbol.origin list
val resolved_type : t -> Type.t
