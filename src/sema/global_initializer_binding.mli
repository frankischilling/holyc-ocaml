type resolution = Global_binding_environment.resolution =
  | Module_binding of Module_expression_binding.publication
  | Outer_binding of Outer_environment.binding

type event
type global_input
type occurrence
type query
type resolved_global
type t

type error_kind =
  | Invalid_input of string
  | Unresolved_identifier of {
      global_symbol : Symbol.t;
      name : string;
      compilation_mode : Outer_environment.compilation_mode;
    }

type error

val make_identifier :
  name:string ->
  origin:Symbol.origin ->
  occurrence_index:int ->
  initializer_path:int list ->
  (event, string) result

val make_global :
  ?queries:(Initializer_source.leaf * Query_selection.t) list ->
  record:Global_resolution.global_record ->
  event list ->
  (global_input, string) result

val resolve :
  table:Symbol_table.t ->
  environment:Outer_environment.t ->
  expressions:Module_expression_binding.t ->
  globals:Global_resolution.t ->
  global_input list ->
  (t, error) result
(** Resolve ordinary identifiers in global initializers after publishing the
    owning global record. Module publications remain source ordered, and names
    missing from the visible module prefix continue through the complete outer
    table chain. *)

val globals : t -> resolved_global list
val environment : t -> Outer_environment.t
val expressions : t -> Module_expression_binding.t
val source_globals : t -> Global_resolution.t
val owns_table : t -> Symbol_table.t -> bool
val find_global : t -> Symbol.t -> resolved_global option
val global_record : resolved_global -> Global_resolution.global_record

val global_publication :
  resolved_global -> Module_expression_binding.publication

val global_symbol : resolved_global -> Symbol.t
val global_item_index : resolved_global -> int
val global_declarator_index : resolved_global -> int option
val global_initializer_origin : resolved_global -> Symbol.origin option
val global_source : resolved_global -> Initializer_source.t option
val global_leaves : resolved_global -> Initializer_source.leaf list
val global_occurrences : resolved_global -> occurrence list
val global_queries : resolved_global -> query list
val query_leaf : query -> Initializer_source.leaf
val query_expression : query -> Frontend.Ast.expression
val query_selection : query -> Query_selection.t option

val query_for :
  global:resolved_global ->
  leaf:Initializer_source.leaf ->
  expression:Frontend.Ast.expression ->
  (query, string) result
(** Exact source descriptors include non-name defined operands. A descriptor
    without selection represents the AST-only path; missing evidence is an
    error. *)

val occurrence_index : occurrence -> int
val occurrence_name : occurrence -> string
val occurrence_origin : occurrence -> Symbol.origin
val occurrence_initializer_path : occurrence -> int list
val occurrence_resolution : occurrence -> resolution
val occurrence_selection : occurrence -> Reference_selection.t option

val make_selected_identifier :
  selection:Reference_selection.t ->
  name:string ->
  origin:Symbol.origin ->
  occurrence_index:int ->
  initializer_path:int list ->
  (event, string) result

val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
