type source =
  | Named of
      Declaration_collection.publication
      * Frontend.Parser.completed_parameter_default
  | Callback of
      Declaration_collection.namespace
      * Frontend.Parser.completed_callback_default

type t
type authority

val table : t -> Symbol_table.t

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
  ?activation:Source_activation.t ->
  namespace:Declaration_collection.namespace ->
  t ->
  (authority, string) result

val authorized_fragment : authority -> t

val create_callback :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  receipt:Frontend.Parser.completed_callback_default ->
  environment:Outer_environment.t ->
  references:(Frontend.Ast.identifier * Reference_selection.t) list ->
  queries:Query_selection.t list ->
  (t, string) result

val source : t -> source
val ast : t -> Frontend.Ast.parameter_default
val index : t -> int

val parameter_parts :
  t ->
  Frontend.Ast.type_specifier
  * Frontend.Ast.pointer_layer list
  * Frontend.Ast.function_pointer_declarator option

val symbol_opt : t -> Symbol.t option
val in_scope : t -> Symbol_table.scope -> bool
val same_source : source -> source -> bool

val current_source :
  ?allow_activation:bool ->
  activation:Source_activation.t option ->
  source ->
  bool
