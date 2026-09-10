type t
type authority

val create :
  table:Symbol_table.t ->
  publication:Declaration_collection.publication ->
  receipt:Frontend.Parser.completed_parameter_default ->
  environment:Outer_environment.t ->
  references:(Frontend.Ast.identifier * Reference_selection.t) list ->
  queries:Query_selection.t list ->
  (t, string) result
(** Pure original-default expression evidence. The transcript owns every exact
    selected identifier and query in one retained environment. No effects or
    argument materialization are authorized by this value. *)

val owns_table : t -> Symbol_table.t -> bool
val publication : t -> Declaration_collection.publication
val receipt : t -> Frontend.Parser.completed_parameter_default
val expression : t -> Frontend.Ast.expression
val origin : t -> Symbol.origin
val environment : t -> Outer_environment.t
val references : t -> (Frontend.Ast.identifier * Reference_selection.t) list
val queries : t -> Query_selection.t list

val reference_for :
  t -> Frontend.Ast.identifier -> (Reference_selection.t, string) result

val query_for :
  t -> Frontend.Ast.expression -> (Query_selection.t, string) result

val authorize :
  namespace:Declaration_collection.namespace -> t -> (authority, string) result

val authorized_fragment : authority -> t
