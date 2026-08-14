val resolve :
  table:Sema.Symbol_table.t ->
  environment:Sema.Outer_environment.t ->
  expressions:Sema.Module_expression_binding.t ->
  globals:Sema.Global_resolution.t ->
  Frontend.Ast.module_ ->
  (Sema.Global_initializer_binding.t, string) result
(** Collect ordinary identifier occurrences from global initializers and bind
    them at the source point immediately after the owning global publication. *)
