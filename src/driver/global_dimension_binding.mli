val resolve :
  table:Sema.Symbol_table.t ->
  environment:Sema.Outer_environment.t ->
  expressions:Sema.Module_expression_binding.t ->
  globals:Sema.Global_resolution.t ->
  Frontend.Ast.module_ ->
  (Sema.Global_dimension_binding.t, string) result
(** Collect ordinary identifiers from global array extents and bind them at the
    source point immediately before the owning global publication. *)
