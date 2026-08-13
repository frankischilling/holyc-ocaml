val layout :
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  aggregates:Sema.Aggregate_resolution.t ->
  headers:Sema.Aggregate_header_resolution.t ->
  members:Sema.Member_type_resolution.t ->
  Frontend.Ast.module_ ->
  (Sema.Aggregate_layout.t, string) result
(** Validate that all semantic inputs describe the same AST, retain the layout
    expressions from that AST, and calculate the closed aggregate layouts. *)
