type arm = Try_body | Catch_body
type t

val bind_headers :
  table:Symbol_table.t ->
  namespace:Declaration_collection.namespace ->
  ?selected_aggregate:Function_type_resolution.selected_aggregate_resolver ->
  records:Function_record_classification.t ->
  Compiler_record.declared_function list ->
  unit
(** Associate exact checked declarations with original live header witnesses.
    The original table, source binding, parameter children and complete header
    types must agree. Retained declarations keep that association across units;
    copied headers, names and symbol numbers cannot supply it. *)

val create :
  ?environment:Outer_environment.t ->
  table:Symbol_table.t ->
  records:Function_record_classification.t ->
  ast:Frontend.Ast.module_ ->
  source:Frontend.Ast.try_catch_statement ->
  unit ->
  (t, Common.Diagnostic.t list) result

val source : t -> Frontend.Ast.try_catch_statement
val ancestry : t -> (Frontend.Ast.try_catch_statement * arm) list
val try_declaration : t -> Function_resolution.resolved_declaration
val untry_declaration : t -> Function_resolution.resolved_declaration
val owns_table : t -> Symbol_table.t -> bool

val matches_header :
  t -> Function_type_resolution.resolved_function option -> bool
(** Require the original checked containing definition, or [None] for entry
    source. Equal names and a foreign frame cannot replace that owner. *)

val matches_records : t -> Function_record_classification.t -> bool
(** The exact parser-selected source headers must still describe ordinary
    unresolved extern SysTry with two U8 pointer parameters and SysUntry with no
    parameters. A same-name registration, a foreign semantic table or a source
    body cannot supply this authority. Ancestry is the original containing
    try/catch path, innermost first; lowering separately checks its graph and
    marker identities. *)
