val diagnostic : span:Common.Span.t -> string -> string -> Common.Diagnostic.t
val source_span : Common.Source_file.t -> Common.Span.t

val prepare :
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  span:Common.Span.t ->
  Frontend.Ast.module_ ->
  ( Sema.Function_call_expression_result.top_level_t,
    Common.Diagnostic.t list )
  result
