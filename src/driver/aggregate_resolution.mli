val resolve :
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  Frontend.Ast.module_ ->
  (Sema.Aggregate_resolution.t, string) result
(** Reconcile aggregate forwards and definitions from the same AST and semantic
    declaration collection. *)
