val diagnostic : span:Common.Span.t -> string -> string -> Common.Diagnostic.t
val source_span : Common.Source_file.t -> Common.Span.t
val message_diagnostic : span:Common.Span.t -> string -> Common.Diagnostic.t

type prepared

val prepare_unit :
  ?environment:Sema.Outer_environment.t ->
  ?declaration_command:Task_declarations.command ->
  ?source_command:Task_declarations.source_command ->
  ?selections:
    (Frontend.Ast.identifier -> (Sema.Reference_selection.t, string) result) ->
  ?implicit_selections:
    (Frontend.Ast.implicit_output_statement ->
    (Sema.Reference_selection.t, string) result) ->
  ?include_global_initializers:bool ->
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

val global_records : prepared -> Sema.Global_record_classification.t
val global_layouts : prepared -> Sema.Global_array_layout.t
val initializers : prepared -> Sema.Global_initializer_binding.t option
val function_outputs : prepared -> Sema.Implicit_output_argument_binding.t

val top_level_outputs :
  prepared -> Sema.Top_level_implicit_output_argument_binding.t
