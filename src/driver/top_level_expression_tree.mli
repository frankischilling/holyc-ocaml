val build :
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  compilation_mode:Sema.Outer_environment.compilation_mode ->
  expressions:Sema.Top_level_outer_expression_binding.t ->
  Frontend.Ast.module_ ->
  (Sema.Top_level_expression_tree.t, string) result
(** Build deterministic semantic expression trees for executable top-level
    statements from their complete module and outer bindings. *)
