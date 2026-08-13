val resolve :
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  aggregates:Sema.Aggregate_resolution.t ->
  Frontend.Ast.module_ ->
  (Sema.Aggregate_header_resolution.t, string) result
(** Resolve aggregate backing and base types at their TempleOS publication
    points, using canonical identities from [aggregates]. *)
