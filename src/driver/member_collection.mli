val collect :
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  Frontend.Ast.module_ ->
  (Sema.Member_collection.t, string) result
(** Collect aggregate members from the same AST used to produce [declarations].
    Layout, inheritance, and duplicate checks are not applied. *)
