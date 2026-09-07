type classification = Prepared_constant of int64 | Scheduled
type item
type t

val prepare :
  max_steps:int ->
  span:Common.Span.t ->
  globals:Ir.Integer_globals.t ->
  top_calls:Sema.Top_level_function_call_target_classification.t list ->
  functions:Ir.Integer_interpreter.function_definition list ->
  (t, Common.Diagnostic.t list) result
(** Classify original value instructions before destination insertion, check the
    supported optimizer domain through initializer callees, and execute pure
    constant values with a shared positive preparation budget. *)

val globals : t -> Ir.Integer_globals.t
val items : t -> item list
val executed_steps : t -> int
val root : item -> Sema.Function_call_expression_result.top_level_root_result
val value_graph : item -> Ir.X87_stack.t
val classification : item -> classification
val item_steps : item -> int
val human : t -> string
