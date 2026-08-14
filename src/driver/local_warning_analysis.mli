val analyze :
  ?compiler_option_mask:int64 ->
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  function_types:Sema.Function_type_resolution.t ->
  local_types:Sema.Local_type_resolution.t ->
  bindings:Sema.Function_binding_index.t ->
  expressions:Sema.Function_expression_binding.t ->
  Frontend.Ast.module_ ->
  (Sema.Local_warning_analysis.t, string) result
(** Validate the typed binding pipeline against the AST, derive each binding's
    initial member flags, and classify function-completion warnings. *)
