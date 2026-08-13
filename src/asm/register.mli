type kind = R8 | R16 | R32 | R64 | Segment | Float_stack | Mm | Xmm
type t

val all : t list
val find : string -> t option
val spelling : t -> string
val kind : t -> kind
val number : t -> int
val source_line : t -> int
