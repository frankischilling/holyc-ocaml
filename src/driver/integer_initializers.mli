type classification = Prepared_constant of int64 | Scheduled

val prepare_default :
  ?retained_function_source:
    (Ir.Retained_function.t ->
    Ir.Integer_interpreter.task_function_source option) ->
  ?on_progress:(int -> unit) ->
  max_steps:int ->
  top_calls:Sema.Top_level_function_call_target_classification.t list ->
  Ir.Default_fragment_destination.t ->
  (classification * int, Common.Diagnostic.t list) result

val prepare_dimension :
  ?retained_function_source:
    (Ir.Retained_function.t ->
    Ir.Integer_interpreter.task_function_source option) ->
  ?on_progress:(int -> unit) ->
  max_steps:int ->
  top_calls:Sema.Top_level_function_call_target_classification.t list ->
  Ir.Dimension_fragment_destination.t ->
  (classification * int, Common.Diagnostic.t list) result

type item
type static_item
type t
type native_static_preparation
type native_preparation

val prepare_native :
  authority:Sema.Initializer_fragment.authority ->
  typed:Sema.Function_call_expression_result.top_level_t ->
  on_progress:(int -> unit) ->
  max_steps:int ->
  (native_preparation, Common.Diagnostic.t list) result
(** Execute an original closed scalar initializer only during its current parser
    callback. The receipt retains the exact leaf, checked type, result and work.
*)

val native_leaf : native_preparation -> Sema.Initializer_source.leaf
val native_steps : native_preparation -> int
val native_evidence : t -> native_preparation list

val native_complete : span:Common.Span.t -> t -> bool
(** Every scalar initial value has its original successful native preparation.
    Caller-supplied image bits cannot satisfy this check. *)

type fragment_preparation

val prepare_fragment :
  ?retained_function_source:
    (Ir.Retained_function.t ->
    Ir.Integer_interpreter.task_function_source option) ->
  ?on_progress:(int -> unit) ->
  max_steps:int ->
  top_calls:Sema.Top_level_function_call_target_classification.t list ->
  functions:Ir.Integer_interpreter.function_definition list ->
  Ir.Initializer_fragment_destination.t ->
  (fragment_preparation, Common.Diagnostic.t list) result
(** Share the ordinary initializer optimizer-domain, transitive call and update
    checks. Constant preparation and owned copies consume the same work budget;
    the exact retained storage context is preserved and no image is published.
*)

val fragment_destination :
  fragment_preparation -> Ir.Initializer_fragment_destination.t

val fragment_payload :
  fragment_preparation -> Ir.Integer_array_initializers.payload option

val fragment_steps : fragment_preparation -> int

val prepare :
  ?native_preparations:native_preparation list ->
  ?native_static_preparations:native_static_preparation list ->
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

val prepare_offset :
  ?retained_function_source:
    (Ir.Retained_function.t ->
    Ir.Integer_interpreter.task_function_source option) ->
  ?on_progress:(int -> unit) ->
  max_steps:int ->
  top_calls:Sema.Top_level_function_call_target_classification.t list ->
  Ir.Offset_fragment_destination.t ->
  (classification * int, Common.Diagnostic.t list) result

val prepare_native_static :
  fragment:Sema.Static_initializer_fragment.t ->
  typed:Sema.Function_call_expression_result.top_level_t ->
  on_progress:(int -> unit) ->
  max_steps:int ->
  (native_static_preparation, Common.Diagnostic.t list) result

val native_static_evidence : t -> native_static_preparation list

val native_static_receipt :
  native_static_preparation -> Frontend.Parser.static_initializer_preparation

val native_statics_complete : span:Common.Span.t -> t -> bool
