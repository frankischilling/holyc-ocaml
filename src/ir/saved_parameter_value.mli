type t

val word : int64 -> t
val word_bits : t -> int64 option

val accepts_callback_expression :
  Sema.Function_call_expression_result.expression_result -> bool

val callback :
  source:Sema.Function_call_expression_result.expression_result ->
  link:Retained_function.t ->
  (t, string) result
(** Preserve the checked expression and selected original function identity.
    This value contains no executable address or native entry permission. *)

val callback_source :
  t ->
  (Retained_function.t * Sema.Function_call_expression_result.expression_result)
  option

val same : t -> t -> bool

val undefined_callback :
  source:Sema.Function_call_expression_result.expression_result ->
  (t, string) result

val undefined_callback_source :
  t -> Sema.Function_call_expression_result.expression_result option
(** Original declaration-time placeholder capture. It has no body link, machine
    PC, or permission to execute a source function. *)
