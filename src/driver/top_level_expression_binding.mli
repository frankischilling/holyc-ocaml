val resolve :
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  module_expressions:Sema.Module_expression_binding.t ->
  ?initializers:Sema.Global_initializer_binding.t ->
  ?selections:
    (Frontend.Ast.identifier -> (Sema.Reference_selection.t, string) result) ->
  ?queries:(Frontend.Ast.expression -> (Sema.Query_selection.t, string) result) ->
  Frontend.Ast.module_ ->
  (Sema.Top_level_expression_binding.t, string) result
(** Traverse executable top-level statement expressions in source order. Bind
    ordinary names and retain specialized [defined] queries against the checked
    module publication prefix. [initializers] also retains declaration-owned
    scalar groups after their exact owning publication. *)

val resolve_initializer_fragment :
  table:Sema.Symbol_table.t ->
  parent:Sema.Symbol_table.scope ->
  module_expressions:Sema.Module_expression_binding.t ->
  Sema.Initializer_fragment.t ->
  (Sema.Top_level_expression_binding.t, string) result

val resolve_default_fragment :
  table:Sema.Symbol_table.t ->
  parent:Sema.Symbol_table.scope ->
  module_expressions:Sema.Module_expression_binding.t ->
  Sema.Default_fragment.t ->
  (Sema.Top_level_expression_binding.t, string) result
