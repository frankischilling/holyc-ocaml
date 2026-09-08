val resolve :
  table:Sema.Symbol_table.t ->
  environment:Sema.Outer_environment.t ->
  expressions:Sema.Module_expression_binding.t ->
  globals:Sema.Global_resolution.t ->
  ?selections:
    (Frontend.Ast.identifier -> (Sema.Reference_selection.t, string) result) ->
  Frontend.Ast.module_ ->
  (Sema.Global_initializer_binding.t, string) result
(** Collect ordinary identifier occurrences from global initializers and bind
    them at the source point immediately after the owning global publication. *)

val initializers :
  table:Sema.Symbol_table.t ->
  bindings:Sema.Global_initializer_binding.t ->
  Frontend.Ast.module_ ->
  ( (Sema.Global_initializer_binding.resolved_global
    * Frontend.Ast.global_initializer)
    list,
    string )
  result
(** Join exact checked owners to their complete original initializer ASTs. *)

val scalar_initializers :
  table:Sema.Symbol_table.t ->
  bindings:Sema.Global_initializer_binding.t ->
  Frontend.Ast.module_ ->
  ( (Sema.Global_initializer_binding.resolved_global
    * Frontend.Ast.global_initializer)
    list,
    string )
  result
(** Compatibility name for [initializers]; declaration groups may have several
    retained array leaves. *)
