type context

val records : context -> Sema.Function_record_classification.t
val function_sources : context -> Sema.Function_call_expression_result.t

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

val prepare_default :
  context ->
  Sema.Default_fragment.t ->
  (Sema.Function_call_expression_result.top_level_t, string) result
