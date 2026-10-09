type t

val create :
  ?activation:Source_activation.t ->
  ?callback:
    Frontend.Parser.completed_callback_signature
    * Function_type_resolution.function_pointer ->
  ?selected_aggregate:Function_type_resolution.selected_aggregate_resolver ->
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  publication:Declaration_collection.publication ->
  receipt:Frontend.Parser.static_initializer_preparation ->
  dimensions:int64 list ->
  environment:Outer_environment.t ->
  queries:Query_selection.t list ->
  unit ->
  (t, string) result

(** Retain one exact original static initializer leaf during its live parser
    callback. [dimensions] are the declaration ledger's already checked array
    bounds in source order. The function publication must belong to the source
    namespace. Value references are excluded; original query selections remain
    mandatory. This fragment alone grants no native image authority. *)

val owns_table : t -> Symbol_table.t -> bool

val create_selected :
  ?activation:Source_activation.t ->
  ?callback:
    Frontend.Parser.completed_callback_signature
    * Function_type_resolution.function_pointer ->
  ?selected_aggregate:Function_type_resolution.selected_aggregate_resolver ->
  references:(Frontend.Ast.identifier * Reference_selection.t) list ->
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  publication:Declaration_collection.publication ->
  receipt:Frontend.Parser.static_initializer_preparation ->
  dimensions:int64 list ->
  environment:Outer_environment.t ->
  queries:Query_selection.t list ->
  unit ->
  (t, string) result
(** Retain the ordered original identifier selections, including private statics
    in the declaring function. Each selection must belong to the original source
    namespace and allocation. This manifest grants no native entry authority. *)

val namespace : t -> Declaration_collection.namespace
val publication : t -> Declaration_collection.publication
val receipt : t -> Frontend.Parser.static_initializer_preparation
val is_current : t -> bool

(* The original live parser callback or exact active journal event. *)
val expression : t -> Frontend.Ast.expression
val type_ : t -> Type.t

val callback_source :
  t ->
  (Frontend.Parser.completed_callback_signature
  * Function_type_resolution.function_pointer)
  option

val dimensions : t -> int64 list
val leaf_path : t -> int list
val leaf_delimiters : t -> Frontend.Parser.initializer_delimiter list
val origin : t -> Symbol.origin
val environment : t -> Outer_environment.t
val references : t -> (Frontend.Ast.identifier * Reference_selection.t) list
val queries : t -> Query_selection.t list

val reference_for :
  t -> Frontend.Ast.identifier -> (Reference_selection.t, string) result

val query_for :
  t -> Frontend.Ast.expression -> (Query_selection.t, string) result
