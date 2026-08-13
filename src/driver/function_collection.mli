val collect :
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  Frontend.Ast.module_ ->
  (Sema.Function_collection.t, string) result
(** Collect parameters and function-wide locals from the same AST used to
    produce [declarations]. Types, storage, and duplicate checks are not
    applied. *)
