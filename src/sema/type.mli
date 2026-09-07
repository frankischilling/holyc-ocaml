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

val make_aggregate : symbol:Symbol.t -> pointer_depth:int -> (t, string) result
val base : t -> base
val pointer_depth : t -> int

val equal : t -> t -> bool
(** Compare checked type identity, including primitive spelling form, aggregate
    symbol-object identity, and pointer depth. Aggregate symbols from separate
    semantic sessions are distinct even if their table-local IDs match. *)

val pointer_to : t -> (t, string) result
(** Add one pointer layer without changing the source-visible base identity. *)

val dereference : t -> (t, string) result
(** Remove one pointer layer. A nonpointer type is rejected. *)

val primitive_form_name : primitive_form -> string
