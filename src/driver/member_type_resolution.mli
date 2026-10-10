val resolve :
  ?selected_types:
    Sema.Declaration_collection.namespace
    * Sema.Function_type_resolution.selected_aggregate_resolver ->
  ?original_definitions:
    (Frontend.Ast.aggregate_definition * Frontend.Ast.aggregate_definition) list ->
  ?inherited_metadata:Sema.Compiler_record.inherited_metadata list ->
  ?inherited_storage:Inherited_metadata.storage ->
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  aggregates:Sema.Aggregate_resolution.t ->
  headers:Sema.Aggregate_header_resolution.t ->
  members:Sema.Member_collection.t ->
  Frontend.Ast.module_ ->
  (Sema.Member_type_resolution.t, string) result
(** Bind aggregate member type references to the canonical identities visible at
    each definition. Array extents and recursive callback signatures remain
    unresolved. Original inherited metadata definitions do not acquire an object
    member index. *)
