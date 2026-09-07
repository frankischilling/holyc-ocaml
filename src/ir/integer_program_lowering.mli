type statement =
  | Empty of Common.Span.t
  | Expression of Sema.Function_call_expression_result.expression_result
  | Initialize of Sema.Function_call_expression_result.initializer_result
  | Initialize_global of
      Sema.Function_call_expression_result.top_level_root_result
  | Return of Sema.Function_call_expression_result.return_result
  | Block of statement list
  | If of
      Sema.Function_call_expression_result.expression_result
      * statement
      * statement option
  | While of Sema.Function_call_expression_result.expression_result * statement
  | Do_while of
      statement * Sema.Function_call_expression_result.expression_result
  | For of
      statement
      * Sema.Function_call_expression_result.expression_result
      * statement option
      * statement
  | Break of Common.Span.t

val lower_with_initializers :
  ?frame:Sema.Function_frame_layout.function_layout ->
  ?globals:Integer_globals.t ->
  ?top_calls:Sema.Top_level_function_call_target_classification.t list ->
  ?function_calls:Sema.Function_call_target_classification.t list ->
  span:Common.Span.t ->
  statement list ->
  ( X87_stack.t * Global_initialization.region_description list,
    Common.Diagnostic.t list )
  result

val lower :
  ?frame:Sema.Function_frame_layout.function_layout ->
  ?globals:Integer_globals.t ->
  ?top_calls:Sema.Top_level_function_call_target_classification.t list ->
  ?function_calls:Sema.Function_call_target_classification.t list ->
  span:Common.Span.t ->
  statement list ->
  (X87_stack.t, Common.Diagnostic.t list) result
(** Lower integer statements with block-local values and conditional
    short-circuit branches. A checked frame enables initializer stores and
    returns through a shared leave block; top-level graphs end the stream.
    Classified direct calls compose through the shared expression planner. The
    graph passes graph and x87 verification; integer VM preflight belongs to
    execution. *)
