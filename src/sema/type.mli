type primitive_form = Public_spelling | Internal_storage

type base =
  | Primitive of primitive_form * Primitive_type.t
  | Aggregate of Symbol.t

type t = private { base : base; pointer_depth : int }

val max_pointer_depth : int

val make_primitive :
  form:primitive_form ->
  primitive:Primitive_type.t ->
  pointer_depth:int ->
  (t, string) result

val make_aggregate :
  symbol:Symbol.t -> pointer_depth:int -> (t, string) result

val base : t -> base
val pointer_depth : t -> int
val primitive_form_name : primitive_form -> string
