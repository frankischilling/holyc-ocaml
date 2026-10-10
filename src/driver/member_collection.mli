val collect :
  ?original_definitions:
    (Frontend.Ast.aggregate_definition * Frontend.Ast.aggregate_definition) list ->
  ?inherited_metadata:Sema.Compiler_record.inherited_metadata list ->
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  Frontend.Ast.module_ ->
  (Sema.Member_collection.t, string) result
(** Collect aggregate members from the same AST used to produce [declarations].
    Layout, inheritance, and duplicate checks are not applied. An original
    inherited metadata proof excludes its definition from object member
    admission. *)
