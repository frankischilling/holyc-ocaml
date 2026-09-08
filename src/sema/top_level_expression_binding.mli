type event

val make_identifier :
  name:string -> origin:Symbol.origin -> (event, string) result

val make_selected_identifier :
  selection:Reference_selection.t ->
  name:string ->
  origin:Symbol.origin ->
  (event, string) result

val make_initializer_identifier :
  leaf:Initializer_source.leaf ->
  name:string ->
  origin:Symbol.origin ->
  (event, string) result

val make_selected_initializer_identifier :
  selection:Reference_selection.t ->
  leaf:Initializer_source.leaf ->
  name:string ->
  origin:Symbol.origin ->
  (event, string) result

val make_name_query :
  role:Function_expression_binding.query_role ->
  name:string ->
  origin:Symbol.origin ->
  (event, string) result

type input

val make_statement :
  statement_index:int ->
  item_index:int ->
  origin:Symbol.origin ->
  event list ->
  (input, string) result

val make_global_initializer :
  statement_index:int ->
  initializers:Global_initializer_binding.t ->
  global:Global_initializer_binding.resolved_global ->
  event list ->
  (input, string) result
(** Retain a declaration-owned module expression group. Its visible prefix
    includes the exact owning global publication and earlier declarators. *)

type resolution =
  | Module_binding of Module_expression_binding.publication
  | Outer_candidate

type occurrence
type query
type statement
type t
type error_kind = Invalid_input of string
type error

val resolve :
  table:Symbol_table.t ->
  parent:Symbol_table.scope ->
  module_expressions:Module_expression_binding.t ->
  input list ->
  (t, error) result
(** Replay the checked module publication stream through executable top-level
    statements. Ordinary names and specialized queries absent from the visible
    publication prefix remain outer candidates. *)

val owns_table : t -> Symbol_table.t -> bool
val owns_module_expressions : t -> Module_expression_binding.t -> bool
val module_expressions : t -> Module_expression_binding.t
val statements : t -> statement list
val all_occurrences : t -> occurrence list
val all_queries : t -> query list
val statement_source : statement -> input
val statement_index : statement -> int
val statement_item_index : statement -> int
val statement_origin : statement -> Symbol.origin
val statement_occurrences : statement -> occurrence list
val statement_queries : statement -> query list

val statement_initializer :
  statement -> Global_initializer_binding.resolved_global option

val initializer_bindings : t -> Global_initializer_binding.t option
val occurrence_index : occurrence -> int
val occurrence_name : occurrence -> string
val occurrence_origin : occurrence -> Symbol.origin
val occurrence_resolution : occurrence -> resolution
val occurrence_selection : occurrence -> Reference_selection.t option

val occurrence_initializer_binding :
  occurrence -> Outer_environment.binding option

val query_index : query -> int
val query_role : query -> Function_expression_binding.query_role
val query_name : query -> string
val query_origin : query -> Symbol.origin
val query_resolution : query -> resolution
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
