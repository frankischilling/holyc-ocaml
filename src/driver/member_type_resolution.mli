val resolve :
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  aggregates:Sema.Aggregate_resolution.t ->
  headers:Sema.Aggregate_header_resolution.t ->
  members:Sema.Member_collection.t ->
  Frontend.Ast.module_ ->
  (Sema.Member_type_resolution.t, string) result
(** Bind aggregate member type references to the canonical identities visible at
    each definition. Array extents and recursive callback signatures remain
    unresolved. *)
