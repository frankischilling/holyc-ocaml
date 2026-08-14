type kind = Allocate | Disable
type position = Before_type | After_type

type explicit_register = private {
  spelling : string;
  number : int;
  origin : Symbol.origin;
}

type t

type selection =
  | Unspecified
  | Allocatable
  | Disabled
  | Explicit of explicit_register

val make :
  kind:kind ->
  position:position ->
  spelling:string ->
  origin:Symbol.origin ->
  ?explicit_register:string ->
  ?explicit_register_number:int ->
  ?explicit_register_origin:Symbol.origin ->
  unit ->
  (t, string) result
(** Build one checked [reg] or [noreg] request. Explicit register fields must be
    supplied together and must identify the same canonical [ST_U64_REGS]
    entry. *)

val kind : t -> kind
val position : t -> position
val spelling : t -> string
val origin : t -> Symbol.origin
val explicit_register : t -> explicit_register option
val explicit_register_spelling : explicit_register -> string
val explicit_register_number : explicit_register -> int
val explicit_register_origin : explicit_register -> Symbol.origin
val effective : t list -> selection
val source_code : selection -> int
val equal : t -> t -> bool
val kind_name : kind -> string
val position_name : position -> string
val selection_name : selection -> string
val canonical_u64_registers : (string * int) list
val canonical_u64_register_number : string -> int option
val is_canonical_u64_register : string -> bool
