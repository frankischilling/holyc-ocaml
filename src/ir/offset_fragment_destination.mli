type t

val create :
  task_view:Integer_globals.task_view ->
  Sema.Function_call_expression_result.top_level_t ->
  (t, string) result

val create_native :
  task_view:Integer_globals.task_view ->
  Sema.Function_call_expression_result.top_level_t ->
  (t, string) result
(** Admit native numeric results. LexExpression preserves the completed word,
    including F64 bits; this destination does not apply the array-bound
    conversion. *)

val fragment : t -> Sema.Offset_fragment.t
val typed : t -> Sema.Function_call_expression_result.top_level_t
val root : t -> Sema.Function_call_expression_result.top_level_root_result
val globals : t -> Integer_globals.t
val type_ : t -> Sema.Type.t
val span : t -> Common.Span.t
