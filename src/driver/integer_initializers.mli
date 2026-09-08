type classification = Prepared_constant of int64 | Scheduled
type item
type static_item
type t

val prepare :
  ?function_calls:Sema.Function_call_target_classification.t list ->
  ?allow_zero_budget:bool ->
  ?retained_function_source:
    (Ir.Retained_function.t ->
    Ir.Integer_interpreter.task_function_source option) ->
  ?on_progress:(int -> unit) ->
  max_steps:int ->
  span:Common.Span.t ->
  globals:Ir.Integer_globals.t ->
  top_calls:Sema.Top_level_function_call_target_classification.t list ->
  functions:Ir.Integer_interpreter.function_definition list ->
  unit ->
  (t, Common.Diagnostic.t list) result
(** Classify original value instructions before destination insertion, check the
    supported optimizer domain through initializer callees, and execute pure
    constant values with a shared positive preparation budget. Global and static
    work follows semantic source order independently of storage indices. Static
    values may be scheduled with their declaring owner; actual containing-frame
    reads and nonconstant AOT globals-on-data-heap phases are rejected. Constant
    preparation includes unused/unreachable declarations. Ordered numeric array
    leaves share this budget; direct owned byte copies charge one preparation
    work unit per copied byte and do not create runtime literal sites. *)

val globals : t -> Ir.Integer_globals.t
val items : t -> item list
val static_items : t -> static_item list

val static_root :
  static_item -> Sema.Function_call_expression_result.initializer_result

val static_slot : static_item -> Ir.Integer_globals.static_slot
val static_value_graph : static_item -> Ir.X87_stack.t
val static_item_steps : static_item -> int
val static_classification : static_item -> classification
val executed_steps : t -> int
val root : item -> Sema.Function_call_expression_result.top_level_root_result
val value_graph : item -> Ir.X87_stack.t
val classification : item -> classification
val item_steps : item -> int
val human : t -> string
