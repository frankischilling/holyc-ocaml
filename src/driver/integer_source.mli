val diagnostic : span:Common.Span.t -> string -> string -> Common.Diagnostic.t
val source_span : Common.Source_file.t -> Common.Span.t

type prepared

val prepare_unit :
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  span:Common.Span.t ->
  Frontend.Ast.module_ ->
  (prepared, Common.Diagnostic.t list) result

val top_level : prepared -> Sema.Function_call_expression_result.top_level_t
val functions : prepared -> Sema.Function_call_expression_result.t
val frames : prepared -> Sema.Function_frame_layout.t
val records : prepared -> Sema.Function_record_classification.t

val prepare :
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  span:Common.Span.t ->
  Frontend.Ast.module_ ->
  ( Sema.Function_call_expression_result.top_level_t,
    Common.Diagnostic.t list )
  result
