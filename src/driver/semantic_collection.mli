val collect :
  sources:Common.Source_manager.t ->
  table:Sema.Symbol_table.t ->
  Frontend.Ast.module_ ->
  (Sema.Declaration_collection.t, string) result
