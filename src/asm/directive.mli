type t

val all : t list
val find : string -> t option
val spelling : t -> string
val templeos_id : t -> int
val source_line : t -> int
val compare : t -> t -> int
