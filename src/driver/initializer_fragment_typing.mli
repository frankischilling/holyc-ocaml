type context

val create_context :
  table:Sema.Symbol_table.t ->
  parent:Sema.Symbol_table.scope ->
  (context, string) result
(** Pure semantic context with no declarations or AST commands. The retained
    environment in each fragment supplies all source-selected bindings. *)

val prepare :
  context ->
  Sema.Initializer_fragment.t ->
  (Sema.Function_call_expression_result.top_level_t, string) result
