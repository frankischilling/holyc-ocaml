type t

val of_int : int -> (t, string) result
val to_int : t -> int
val compare : t -> t -> int
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
