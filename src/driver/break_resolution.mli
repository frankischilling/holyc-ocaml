val resolve :
  table:Sema.Symbol_table.t ->
  functions:Sema.Function_collection.t ->
  Frontend.Ast.module_ ->
  (Sema.Break_resolution.t, string) result
(** Bind breaks to control regions. Reject targetless breaks. *)
