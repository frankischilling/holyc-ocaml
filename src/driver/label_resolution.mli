val resolve :
  table:Sema.Symbol_table.t ->
  functions:Sema.Function_collection.t ->
  Frontend.Ast.module_ ->
  (Sema.Label_resolution.t, string) result
(** Resolve language [goto] occurrences and language or assembly-block label
    definitions against the function scopes created from the same AST. *)
