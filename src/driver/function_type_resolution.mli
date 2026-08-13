val resolve :
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  aggregates:Sema.Aggregate_resolution.t ->
  functions:Sema.Function_collection.t ->
  Frontend.Ast.module_ ->
  (Sema.Function_type_resolution.t, string) result
