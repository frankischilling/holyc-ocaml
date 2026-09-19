type t

val create :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  publication:Declaration_collection.publication ->
  receipt:Frontend.Parser.static_initializer_preparation ->
  environment:Outer_environment.t ->
  queries:Query_selection.t list ->
  (t, string) result

(** Retain one original scalar static initializer during its live parser
    callback. The function publication must belong to the source namespace.
    Value references are excluded; original query selections remain mandatory.
    This fragment alone grants no native image authority. *)

val owns_table : t -> Symbol_table.t -> bool
val namespace : t -> Declaration_collection.namespace
val publication : t -> Declaration_collection.publication
val receipt : t -> Frontend.Parser.static_initializer_preparation
val expression : t -> Frontend.Ast.expression
val type_ : t -> Type.t
val origin : t -> Symbol.origin
val environment : t -> Outer_environment.t
val references : t -> (Frontend.Ast.identifier * Reference_selection.t) list
val queries : t -> Query_selection.t list

val reference_for :
  t -> Frontend.Ast.identifier -> (Reference_selection.t, string) result

val query_for :
  t -> Frontend.Ast.expression -> (Query_selection.t, string) result
