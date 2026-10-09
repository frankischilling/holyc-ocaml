val resolve :
  ?selected_types:
    Sema.Declaration_collection.namespace
    * Sema.Function_type_resolution.selected_aggregate_resolver ->
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  aggregates:Sema.Aggregate_resolution.t ->
  functions:Sema.Function_collection.t ->
  Frontend.Ast.module_ ->
  (Sema.Local_type_resolution.t, string) result
