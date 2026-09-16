val resolve :
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  Frontend.Ast.module_ ->
  (Sema.Aggregate_resolution.t, string) result
(** Reconcile aggregate forwards and definitions from the same AST and semantic
    declaration collection. Publication-backed command views may retain an
    earlier canonical aggregate identity; ordinary collections derive the same
    first-forward identity from source order. Declaration-site symbols remain
    distinct in both cases. *)
