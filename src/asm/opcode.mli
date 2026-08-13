type t
type resolved

val all : t list
val spelling : t -> string
val source_line : t -> int
val aliases : t -> string list
val first_form_argument_count : t -> int
val resolve : string -> resolved option
val resolved_opcode : resolved -> t
val resolved_source_spelling : resolved -> string
val resolved_is_alias : resolved -> bool
