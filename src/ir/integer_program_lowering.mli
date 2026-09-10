type statement =
  | Empty of Common.Span.t
  | Expression of Sema.Function_call_expression_result.expression_result
  | Function_output of Sema.Implicit_output_argument_binding.bound_output
  | Top_level_output of
      Sema.Top_level_implicit_output_argument_binding.bound_output
  | Initialize of Sema.Function_call_expression_result.initializer_result
  | Initialize_global of
      Sema.Function_call_expression_result.top_level_root_result
  | Initialize_fragment of Initializer_fragment_destination.t
  | Initialize_static of Integer_globals.static_slot
  | Initialize_static_leaf of
      Integer_globals.static_slot
      * Sema.Function_call_expression_result.initializer_result
  | Publish_array of Global_initialization.prepared_root
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

type t

val lower_complete :
  ?frame:Sema.Function_frame_layout.function_layout ->
  ?globals:Integer_globals.t ->
  ?records:Sema.Function_record_classification.t ->
  ?top_calls:Sema.Top_level_function_call_target_classification.t list ->
  ?function_calls:Sema.Function_call_target_classification.t list ->
  span:Common.Span.t ->
  statement list ->
  (t, Common.Diagnostic.t list) result

val graph : t -> X87_stack.t
val publications : t -> Global_initialization.publication_description list

val publication_evidence :
  t -> Global_initialization.publication_evidence option

val initializer_regions : t -> Global_initialization.region_description list

val static_initializer_regions :
  t -> Global_initialization.static_region_description list

val runtime_calls : t -> Runtime_call_context.description list
(** Complete lowering retains checked call origins and implicit-output discard
    identities for graph-owned runtime validation. The graph-only wrappers do
    not provide implicit-output execution authority. *)

val lower_with_storage_initializers :
  ?frame:Sema.Function_frame_layout.function_layout ->
  ?globals:Integer_globals.t ->
  ?top_calls:Sema.Top_level_function_call_target_classification.t list ->
  ?function_calls:Sema.Function_call_target_classification.t list ->
  span:Common.Span.t ->
  statement list ->
  ( X87_stack.t
    * Global_initialization.region_description list
    * Global_initialization.static_region_description list,
    Common.Diagnostic.t list )
  result

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
