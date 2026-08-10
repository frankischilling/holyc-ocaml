type t = { source : Source_id.t; start : int; stop : int }

val make :
  source:Source_id.t ->
  length:int ->
  start:int ->
  stop:int ->
  (t, string) result

val unsafe_make : source:Source_id.t -> start:int -> stop:int -> t
val length : t -> int
val contains : t -> int -> bool
val compare : t -> t -> int
val pp : Format.formatter -> t -> unit
