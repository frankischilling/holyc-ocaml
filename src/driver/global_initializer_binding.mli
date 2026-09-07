val resolve :
  table:Sema.Symbol_table.t ->
  environment:Sema.Outer_environment.t ->
  expressions:Sema.Module_expression_binding.t ->
  globals:Sema.Global_resolution.t ->
  Frontend.Ast.module_ ->
  (Sema.Global_initializer_binding.t, string) result
(** Collect ordinary identifier occurrences from global initializers and bind
    them at the source point immediately after the owning global publication. *)

val scalar_initializers :
  table:Sema.Symbol_table.t ->
  bindings:Sema.Global_initializer_binding.t ->
  Frontend.Ast.module_ ->
  ( (Sema.Global_initializer_binding.resolved_global
    * Frontend.Ast.global_initializer)
    list,
    string )
  result
(** Join exact checked owners to their original scalar initializer ASTs. *)
