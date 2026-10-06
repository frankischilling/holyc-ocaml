type t =
  | Function of Sema.Function_call_expression_result.indirect_call
  | Global of
      Sema.Function_call_expression_result.top_level_global_callback_call
  | Outer of Sema.Function_call_expression_result.top_level_outer_callback_call
  | Static of
      Sema.Function_call_expression_result.top_level_static_callback_call
  | Indexed_global of
      Sema.Function_call_expression_result
      .top_level_indexed_global_callback_call

val callable : t -> Sema.Function_call_resolution.callable
val origin : t -> Sema.Symbol.origin
val callee : t -> Sema.Function_call_expression_result.expression_result option

val callee_source :
  t -> Sema.Function_call_resolution.argument_expression option

val fixed_arguments :
  t ->
  (Sema.Function_type_resolution.parameter
  * Sema.Function_call_expression_result.expression_result option)
  list
(** Each pair retains the original signature parameter and its checked provided
    value. [None] selects that parameter's original prepared declaration
    default. *)

val variadic_arguments :
  t -> Sema.Function_call_expression_result.expression_result list

val matches_result :
  t -> Sema.Function_call_expression_result.expression_result -> bool

val function_member :
  t -> Sema.Function_call_expression_result.resolved_function -> bool

val top_level_member :
  t -> Sema.Function_call_expression_result.top_level_t -> bool

val top_level_calls : Sema.Function_call_expression_result.top_level_t -> t list
(** Views retain the original typed call records. They do not grant execution
    authority; the runtime context checks exact source-batch membership and
    seals the original callee, argument producers and instruction graph. *)
