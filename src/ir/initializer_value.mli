val is_void : Sema.Function_call_expression_result.expression_result -> bool

val discards :
  scalar:Sema.Initializer_type.scalar option ->
  Sema.Function_call_expression_result.expression_result ->
  bool
