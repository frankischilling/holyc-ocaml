type publication_kind = Aggregate | Function | Global_variable
type publication

val make_publication :
  source_symbol:Symbol.t ->
  canonical_symbol:Symbol.t ->
  publication_kind:publication_kind ->
  declaration_index:int ->
  item_index:int ->
  ?declarator_index:int ->
  unit ->
  (publication, string) result
(** Describe one checked compilation-unit hash publication. The source symbol
    identifies the declaration site, while the canonical symbol identifies a
    joined aggregate or function identity. *)

type resolution =
  | Local_binding of Function_binding_index.binding
  | Module_binding of publication
  | Outer_candidate

type occurrence
type resolved_function
type t

type error_kind =
  | Invalid_input of string
  | Missing_function_publication of { function_symbol : Symbol.t }

type error

val resolve :
  table:Symbol_table.t ->
  parent:Symbol_table.scope ->
  compilation_mode:Function_resolution.compilation_mode ->
  expressions:Function_expression_binding.t ->
  publication list ->
  (t, error) result
(** Replay source-ordered module publications through each function header, then
    resolve the function's nonlocal candidates. Names absent from the module
    remain outer-environment candidates. *)

val publications : t -> publication list
val functions : t -> resolved_function list
val compilation_mode : t -> Function_resolution.compilation_mode
val owns_table : t -> Symbol_table.t -> bool
val parent_scope : t -> Symbol_table.scope
val find_function : t -> Symbol.t -> resolved_function option
val publication_kind : publication -> publication_kind
val publication_source_symbol : publication -> Symbol.t
val publication_canonical_symbol : publication -> Symbol.t
val publication_declaration_index : publication -> int
val publication_item_index : publication -> int
val publication_declarator_index : publication -> int option
val publication_kind_name : publication_kind -> string
val function_symbol : resolved_function -> Symbol.t
val function_scope : resolved_function -> Symbol_table.scope
val function_item_index : resolved_function -> int
val function_occurrences : resolved_function -> occurrence list

val function_queries :
  resolved_function -> Function_expression_binding.query list

val occurrence_source : occurrence -> Function_expression_binding.occurrence
val occurrence_index : occurrence -> int
val occurrence_name : occurrence -> string
val occurrence_origin : occurrence -> Symbol.origin
val occurrence_resolution : occurrence -> resolution
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
