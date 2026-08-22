val classify :
  table:Sema.Symbol_table.t ->
  globals:Sema.Global_type_resolution.t ->
  functions:Sema.Function_resolution.t ->
  expressions:Sema.Top_level_expression_tree.t ->
  (Sema.Top_level_identifier_resolution.t, string) result
