type target =
  | Module of Module_expression_binding.publication
  | Outer of Outer_environment.binding
  | Unavailable

val resolve :
  table:Symbol_table.t ->
  environment:Outer_environment.t ->
  publications:Module_expression_binding.publication list ->
  name:string ->
  Reference_selection.t ->
  (target, string) result
