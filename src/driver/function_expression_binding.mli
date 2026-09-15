val resolve :
  table:Sema.Symbol_table.t ->
  declarations:Sema.Declaration_collection.t ->
  functions:Sema.Function_collection.t ->
  local_types:Sema.Local_type_resolution.t ->
  bindings:Sema.Function_binding_index.t ->
  ?selections:
    (Frontend.Ast.identifier -> (Sema.Reference_selection.t, string) result) ->
  ?queries:(Frontend.Ast.expression -> (Sema.Query_selection.t, string) result) ->
  Frontend.Ast.module_ ->
  (Sema.Function_expression_binding.t, string) result
(** Traverse ordinary function-body expressions in source order, publish locals
    at their declarator boundaries, and bind visible parameters and locals. *)
