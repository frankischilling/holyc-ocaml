type t

val create :
  task_view:Integer_globals.task_view ->
  Sema.Function_call_expression_result.top_level_t ->
  (t, string) result

val create_native :
  task_view:Integer_globals.task_view ->
  Sema.Function_call_expression_result.top_level_t ->
  (t, string) result

val fragment : t -> Sema.Dimension_fragment.t
val typed : t -> Sema.Function_call_expression_result.top_level_t
val root : t -> Sema.Function_call_expression_result.top_level_root_result
val globals : t -> Integer_globals.t
val type_ : t -> Sema.Type.t
val native_f64_source : t -> bool
val convert_native_word : t -> int64 -> int64
val span : t -> Common.Span.t
