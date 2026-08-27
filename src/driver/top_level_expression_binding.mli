val resolve :
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  module_expressions:Sema.Module_expression_binding.t ->
  Frontend.Ast.module_ ->
  (Sema.Top_level_expression_binding.t, string) result
(** Traverse executable top-level statement expressions in source order. Bind
    ordinary names and retain specialized [defined] queries against the checked
    module publication prefix. *)
