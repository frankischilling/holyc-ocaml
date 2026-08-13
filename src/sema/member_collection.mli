type member
(** A checked direct-member fact before it is inserted into a semantic scope. *)

type aggregate
(** One aggregate definition and the direct members collected from it. *)

type entry
type collected_aggregate
type t

val make_member :
  name:string ->
  origin:Symbol.origin ->
  member_path:int list ->
  declarator_index:int ->
  (member, string) result
(** Build one member fact without mutating the symbol table. The path records
    aggregate-member indexes, including each containing anonymous union. *)

val make_aggregate :
  symbol:Symbol.t -> item_index:int -> member list -> (aggregate, string) result
(** Associate checked member facts with a top-level aggregate-definition symbol.
*)

val collect :
  table:Symbol_table.t ->
  parent:Symbol_table.scope ->
  aggregate list ->
  (t, string) result
(** Create one aggregate scope per definition and insert its direct members in
    list order. Anonymous-union members must already be flattened into that
    order. *)

val aggregates : t -> collected_aggregate list
val aggregate_symbol : collected_aggregate -> Symbol.t
val aggregate_scope : collected_aggregate -> Symbol_table.scope
val aggregate_item_index : collected_aggregate -> int
val aggregate_entries : collected_aggregate -> entry list
val entry_symbol : entry -> Symbol.t
val entry_member_path : entry -> int list
val entry_declarator_index : entry -> int
