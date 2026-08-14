val resolve :
  table:Sema.Symbol_table.t ->
  environment:Sema.Outer_environment.t ->
  expressions:Sema.Module_expression_binding.t ->
  functions:Sema.Function_resolution.t ->
  Frontend.Ast.module_ ->
  (Sema.Function_default_binding.t, string) result
(** Collect ordinary identifier occurrences from defaults on top-level named
    function headers and bind them after the owning header publication. *)
