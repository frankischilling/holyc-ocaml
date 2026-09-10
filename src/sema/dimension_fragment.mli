type t
type authority

val create :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  receipt:Frontend.Parser.array_dimension_preparation ->
  environment:Outer_environment.t ->
  references:(Frontend.Ast.identifier * Reference_selection.t) list ->
  queries:Query_selection.t list ->
  (t, string) result

val authorize : t -> (authority, string) result
val authorized_fragment : authority -> t
val owns_table : t -> Symbol_table.t -> bool
val namespace : t -> Declaration_collection.namespace
val receipt : t -> Frontend.Parser.array_dimension_preparation
val expression : t -> Frontend.Ast.expression
val origin : t -> Symbol.origin
val environment : t -> Outer_environment.t
val references : t -> (Frontend.Ast.identifier * Reference_selection.t) list
val queries : t -> Query_selection.t list

val reference_for :
  t -> Frontend.Ast.identifier -> (Reference_selection.t, string) result

val query_for :
  t -> Frontend.Ast.expression -> (Query_selection.t, string) result
