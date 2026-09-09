type t
type sizeof_read

val seed_primitive :
  table:Symbol_table.t ->
  entry:Frontend.Symbol_visibility.entry ->
  symbol:Symbol.t ->
  primitive:Primitive_type.t ->
  (t, string) result
(** Establish the frontend/semantic association at primitive seeding. *)

val rebind_primitive :
  table:Symbol_table.t -> symbol:Symbol.t -> t -> (t, string) result
(** Bind the known seeded record to a fork's fresh symbol without another
    spelling lookup. *)

val published_scalar :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  Declaration_collection.publication ->
  (t, string) result
(** Read original published type children, independently of storage admission.
    Arrays, aggregate layouts and function-pointer signatures need separate
    checked metadata. *)

val bind_retained_scalar :
  table:Symbol_table.t ->
  entry:Frontend.Symbol_visibility.entry ->
  Global_type_resolution.global ->
  (t, string) result
(** Associate a newly published retained frontend entry with its original
    checked global. Call at publication, never recover an association by name.
*)

val read_sizeof :
  table:Symbol_table.t ->
  root:Frontend.Parser.query_root ->
  t ->
  (sizeof_read, string) result

val complete_sizeof :
  table:Symbol_table.t ->
  receipt:Frontend.Parser.completed_query ->
  sizeof_read ->
  (unit, string) result

val sizeof_value : sizeof_read -> pointer:bool -> int64
val sizeof_primitive : sizeof_read -> Primitive_type.t option
val sizeof_is_internal : sizeof_read -> bool
