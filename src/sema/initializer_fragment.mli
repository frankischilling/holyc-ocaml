type t
type authority

val authorize :
  namespace:Declaration_collection.namespace -> t -> (authority, string) result
(** Internal source-ledger authority. The opaque namespace must own the exact
    declared object and the original leaf callback must still be current. *)

val authorized_fragment : authority -> t

val create :
  table:Symbol_table.t ->
  declaration:Compiler_record.declared_global ->
  leaf:Initializer_source.leaf ->
  environment:Outer_environment.t ->
  references:(Frontend.Ast.identifier * Reference_selection.t) list ->
  queries:Query_selection.t list ->
  (t, string) result
(** Checked expression source for one original parser leaf. References and
    queries must cover its exact ordered transcript in one retained environment.
    This evidence grants neither storage admission nor execution authority. *)

val owns_table : t -> Symbol_table.t -> bool
val declaration : t -> Compiler_record.declared_global
val leaf : t -> Initializer_source.leaf
val environment : t -> Outer_environment.t
val references : t -> (Frontend.Ast.identifier * Reference_selection.t) list
val queries : t -> Query_selection.t list

val reference_for :
  t -> Frontend.Ast.identifier -> (Reference_selection.t, string) result

val query_for :
  t -> Frontend.Ast.expression -> (Query_selection.t, string) result
