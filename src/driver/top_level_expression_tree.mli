val build :
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  compilation_mode:Sema.Outer_environment.compilation_mode ->
  expressions:Sema.Top_level_outer_expression_binding.t ->
  Frontend.Ast.module_ ->
  (Sema.Top_level_expression_tree.t, string) result
(** Build deterministic semantic expression trees for executable top-level
    statements from their complete module and outer bindings, including exact
    [defined] query evidence. *)

val build_initializer_fragment :
  table:Sema.Symbol_table.t ->
  expressions:Sema.Top_level_outer_expression_binding.t ->
  Sema.Initializer_fragment.t ->
  (Sema.Top_level_expression_tree.t, string) result

val build_default_fragment :
  table:Sema.Symbol_table.t ->
  expressions:Sema.Top_level_outer_expression_binding.t ->
  Sema.Default_fragment.t ->
  (Sema.Top_level_expression_tree.t, string) result

val build_dimension_fragment :
  table:Sema.Symbol_table.t ->
  expressions:Sema.Top_level_outer_expression_binding.t ->
  Sema.Dimension_fragment.t ->
  (Sema.Top_level_expression_tree.t, string) result
