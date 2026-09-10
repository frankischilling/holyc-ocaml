type t

val create_source :
  Sema.Function_call_expression_result.top_level_t -> (t, string) result

val create :
  task_view:Integer_globals.task_view ->
  Sema.Function_call_expression_result.top_level_t ->
  (t, string) result

val fragment : t -> Sema.Default_fragment.t
val typed : t -> Sema.Function_call_expression_result.top_level_t
val root : t -> Sema.Function_call_expression_result.top_level_root_result
val globals : t -> Integer_globals.t
val type_ : t -> Sema.Type.t
val symbol : t -> Sema.Symbol.t
val span : t -> Common.Span.t
