module Id : sig
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_int : t -> int
end

type value_category =
  | Object_value
  | Address_value
  | Array_value
  | Callback_value
  | Function_value
  | Offset_value
  | Lvalue
  | Unavailable

type result_class = Integer_result | F64_result | Unresolved_actual_class

type intrinsic_conversion =
  | No_intrinsic_conversion
  | Result_to_f64
  | Result_to_int

type result_use = Result_not_used
type expression_result
type aggregate_offset_segment
type aggregate_offset_path
type declared_default_kind = Expression_default_kind | Lastclass_default_kind
type declared_default_materialization = Immediate_default | Aot_string_default
type declared_default_result

type fixed_path =
  | Provided_result of expression_result
  | Declared_default_result of declared_default_result

type lastclass_substitution
type fixed_result
type top_level_fixed_result
type outer_callback_fixed_result
type outer_callback_call
type top_level_direct_call
type top_level_global_callback_call
type top_level_outer_callback_call
type top_level_indexed_global_callback_call
type top_level_member_callback_call
type direct_call
type indirect_call

type call_result =
  | Direct_call_result of direct_call
  | Indirect_call_result of indirect_call
  | Deferred_call_result of Function_call_resolution.call_resolution

type return_presence =
  | Matching_value
  | Matching_no_value
  | Unexpected_value
  | Missing_value

type condition_result
type expression_statement_result
type implicit_output_argument_result
type implicit_output_result
type selector_result
type switch_case_value

type switch_case_pattern_result =
  | Implicit_case_result
  | Single_case_result of switch_case_value
  | Ranged_case_result of {
      start_value : switch_case_value;
      end_value : switch_case_value;
    }

type switch_case_result
type return_result
type resolved_function
type t
type top_level_root_result
type top_level_statement_result
type top_level_t
type error_kind = Invalid_input of string
type error

val analyze :
  table:Symbol_table.t ->
  members:Aggregate_member_index.t ->
  ?outer:Outer_expression_binding.t ->
  Function_call_conversion_policy.t ->
  (t, error) result
(** Derive immutable source-expression results for call arguments, ordinary and
    implicit-output function statements, conditions, switch selectors, case
    values, and returns. When [outer] is supplied, it must be the exact
    outer-expression batch for this policy traversal; checked global metadata
    supplies the type, value shape, and array rank of the selected outer leaf.
    Each checked expression has a deterministic identity, source origin,
    semantic type, value category, remaining array rank, conversion intent,
    forwarded result class, and any separate execution class needed by later
    lowering. A selected default has a result with its exact source and
    parameter, semantic parameter type, forwarded class, kind, and JIT or AOT
    materialization path; it does not receive an expression identity. A selected
    [lastclass] default also retains the previous provided result and derived
    base spelling. Each function expression statement records the
    [ICF_RES_NOT_USED] intent emitted by [PrsExpression] without mutating the
    parser AST. A source-visible aggregate base is accepted only under direct
    member access and retains the publication, current-or-inherited lookups,
    intermediate types, and cumulative offsets of [PrsOffsetOf]. Function
    conditions retain their statement role without an invented Boolean
    conversion. Switch selectors retain their bounded or no-bound mode without
    applying range arithmetic. Switch cases retain implicit, single, or ranged
    structure; explicit F64 values carry the conversion performed by
    [LexExpressionI64] without being evaluated. Function returns retain the
    declared type, integer or F64 conversion intent, and warning facts for
    missing or unexpected values. *)

val analyze_top_level :
  table:Symbol_table.t ->
  members:Aggregate_member_index.t ->
  policies:Function_call_conversion_policy.t ->
  identifiers:Top_level_identifier_resolution.t ->
  Top_level_expression_tree.t ->
  (top_level_t, error) result
(** Type scalar roots, aggregate object members, aggregate offset paths, direct
    calls, scalar global callbacks, fully indexed global callback arrays, scalar
    or fully indexed outer callbacks, ordinary aggregate-member callbacks, and
    fully indexed aggregate callback-member arrays in executable top-level
    statements through the same expression engine used for function bodies.
    Partial callback arrays, partial or unindexed callback-member arrays,
    unrelated computed callees, and outer records without type payloads stay
    explicit. *)

val owns_table : t -> Symbol_table.t -> bool
val owns_members : t -> Aggregate_member_index.t -> bool
val owns_policies : t -> Function_call_conversion_policy.t -> bool
val owns_outer : t -> Outer_expression_binding.t -> bool
val compilation_mode : t -> Function_resolution.compilation_mode
val functions : t -> resolved_function list
val all_results : t -> expression_result list
val top_level_owns_table : top_level_t -> Symbol_table.t -> bool
val top_level_owns_members : top_level_t -> Aggregate_member_index.t -> bool

val top_level_owns_policies :
  top_level_t -> Function_call_conversion_policy.t -> bool

val top_level_owns_identifiers :
  top_level_t -> Top_level_identifier_resolution.t -> bool

val top_level_source : top_level_t -> Top_level_expression_tree.t

val top_level_compilation_mode :
  top_level_t -> Function_resolution.compilation_mode

val top_level_statements : top_level_t -> top_level_statement_result list
val top_level_direct_calls : top_level_t -> top_level_direct_call list

val top_level_global_callback_calls :
  top_level_t -> top_level_global_callback_call list

val top_level_outer_callback_calls :
  top_level_t -> top_level_outer_callback_call list

val top_level_indexed_global_callback_calls :
  top_level_t -> top_level_indexed_global_callback_call list

val top_level_member_callback_calls :
  top_level_t -> top_level_member_callback_call list

val top_level_all_results : top_level_t -> expression_result list

val top_level_statement_source :
  top_level_statement_result -> Top_level_expression_tree.statement

val top_level_statement_roots :
  top_level_statement_result -> top_level_root_result list

val top_level_root_source :
  top_level_root_result -> Top_level_expression_tree.root

val top_level_root_value : top_level_root_result -> expression_result
val top_level_root_result_use : top_level_root_result -> result_use option
val function_symbol : resolved_function -> Symbol.t
val function_scope : resolved_function -> Symbol_table.scope
val function_item_index : resolved_function -> int
val function_calls : resolved_function -> call_result list

val function_outer_callback_calls :
  resolved_function -> outer_callback_call list
(** Source-ordered calls through scalar or fully indexed outer callbacks. Each
    record keeps its recursive signature, exact outer occurrence, selected table
    entry, and an optional completed callee result. This analysis does not read
    or invoke the stored callback address. *)

val function_expression_statements :
  resolved_function -> expression_statement_result list

val expression_statement_source :
  expression_statement_result ->
  Function_call_resolution.expression_statement_input

val expression_statement_value :
  expression_statement_result -> expression_result

val expression_statement_result_use : expression_statement_result -> result_use
val function_implicit_outputs : resolved_function -> implicit_output_result list

val implicit_output_source :
  implicit_output_result -> Function_call_resolution.implicit_output_input

val implicit_output_fixed_value : implicit_output_result -> expression_result

val implicit_output_arguments :
  implicit_output_result -> implicit_output_argument_result list

val implicit_output_result_use : implicit_output_result -> result_use

val implicit_output_argument_source :
  implicit_output_argument_result ->
  Function_call_resolution.implicit_output_argument

val implicit_output_argument_value :
  implicit_output_argument_result -> expression_result

val function_conditions : resolved_function -> condition_result list

val condition_source :
  condition_result -> Function_call_resolution.condition_input

val condition_value : condition_result -> expression_result
val function_selectors : resolved_function -> selector_result list
val selector_source : selector_result -> Function_call_resolution.selector_input
val selector_value : selector_result -> expression_result
val function_switch_cases : resolved_function -> switch_case_result list

val switch_case_source :
  switch_case_result -> Function_call_resolution.switch_case_input

val switch_case_pattern : switch_case_result -> switch_case_pattern_result
val switch_case_value_result : switch_case_value -> expression_result
val switch_case_value_conversion : switch_case_value -> intrinsic_conversion
val function_returns : resolved_function -> return_result list
val return_source : return_result -> Function_call_resolution.return_input
val return_declared_type : return_result -> Type.t
val return_declared_class : return_result -> result_class
val return_value : return_result -> expression_result option
val return_conversion : return_result -> intrinsic_conversion
val return_presence : return_result -> return_presence
val direct_source : direct_call -> Function_call_conversion_policy.direct_call
val direct_fixed_results : direct_call -> fixed_result list
val direct_variadic_results : direct_call -> expression_result list

val indirect_source :
  indirect_call -> Function_call_conversion_policy.indirect_call

val indirect_callee_result : indirect_call -> expression_result option
val indirect_fixed_results : indirect_call -> fixed_result list
val indirect_variadic_results : indirect_call -> expression_result list
val fixed_source : fixed_result -> Function_call_conversion_policy.fixed_policy
val fixed_path : fixed_result -> fixed_path
val fixed_lastclass_substitution : fixed_result -> lastclass_substitution option

val top_level_fixed_source :
  top_level_fixed_result -> Function_call_resolution.fixed_argument

val top_level_fixed_target_class : top_level_fixed_result -> result_class
val top_level_fixed_path : top_level_fixed_result -> fixed_path

val top_level_fixed_lastclass_substitution :
  top_level_fixed_result -> lastclass_substitution option

val outer_callback_fixed_source :
  outer_callback_fixed_result -> Function_call_resolution.fixed_argument

val outer_callback_fixed_target_class :
  outer_callback_fixed_result -> result_class

val outer_callback_fixed_path : outer_callback_fixed_result -> fixed_path

val outer_callback_fixed_lastclass_substitution :
  outer_callback_fixed_result -> lastclass_substitution option

val outer_callback_source : outer_callback_call -> Function_call_resolution.call

val outer_callback_occurrence :
  outer_callback_call -> Outer_expression_binding.occurrence

val outer_callback_binding : outer_callback_call -> Outer_environment.binding

val outer_callback_callee_result :
  outer_callback_call -> expression_result option

val outer_callback_callable :
  outer_callback_call -> Function_call_resolution.callable

val outer_callback_fixed_results :
  outer_callback_call -> outer_callback_fixed_result list

val outer_callback_variadic_results :
  outer_callback_call -> expression_result list

val outer_callback_variadic_count : outer_callback_call -> int64
val outer_callback_result_id : outer_callback_call -> Id.t

val top_level_direct_source :
  top_level_direct_call -> Top_level_expression_tree.call

val top_level_direct_declaration :
  top_level_direct_call -> Function_resolution.resolved_declaration

val top_level_direct_header :
  top_level_direct_call -> Function_type_resolution.resolved_function

val top_level_direct_target_symbol : top_level_direct_call -> Symbol.t

val top_level_direct_fixed_results :
  top_level_direct_call -> top_level_fixed_result list

val top_level_direct_variadic_results :
  top_level_direct_call -> expression_result list

val top_level_direct_variadic_count : top_level_direct_call -> int64
val top_level_direct_result_id : top_level_direct_call -> Id.t

val top_level_global_callback_source :
  top_level_global_callback_call -> Top_level_expression_tree.call

val top_level_global_callback_global :
  top_level_global_callback_call -> Global_type_resolution.global

val top_level_global_callback_value :
  top_level_global_callback_call -> Function_call_resolution.identifier_value

val top_level_global_callback_callable :
  top_level_global_callback_call -> Function_call_resolution.callable

val top_level_global_callback_fixed_results :
  top_level_global_callback_call -> top_level_fixed_result list

val top_level_global_callback_variadic_results :
  top_level_global_callback_call -> expression_result list

val top_level_global_callback_variadic_count :
  top_level_global_callback_call -> int64

val top_level_global_callback_result_id : top_level_global_callback_call -> Id.t

val top_level_outer_callback_source :
  top_level_outer_callback_call -> Top_level_expression_tree.call

val top_level_outer_callback_occurrence :
  top_level_outer_callback_call -> Top_level_outer_expression_binding.occurrence

val top_level_outer_callback_binding :
  top_level_outer_callback_call -> Outer_environment.binding

val top_level_outer_callback_callee_result :
  top_level_outer_callback_call -> expression_result option

val top_level_outer_callback_callable :
  top_level_outer_callback_call -> Function_call_resolution.callable

val top_level_outer_callback_fixed_results :
  top_level_outer_callback_call -> top_level_fixed_result list

val top_level_outer_callback_variadic_results :
  top_level_outer_callback_call -> expression_result list

val top_level_outer_callback_variadic_count :
  top_level_outer_callback_call -> int64

val top_level_outer_callback_result_id : top_level_outer_callback_call -> Id.t

val top_level_indexed_global_callback_source :
  top_level_indexed_global_callback_call -> Top_level_expression_tree.call

val top_level_indexed_global_callback_global :
  top_level_indexed_global_callback_call -> Global_type_resolution.global

val top_level_indexed_global_callback_value :
  top_level_indexed_global_callback_call ->
  Function_call_resolution.identifier_value

val top_level_indexed_global_callback_callee_result :
  top_level_indexed_global_callback_call -> expression_result

val top_level_indexed_global_callback_callable :
  top_level_indexed_global_callback_call -> Function_call_resolution.callable

val top_level_indexed_global_callback_fixed_results :
  top_level_indexed_global_callback_call -> top_level_fixed_result list

val top_level_indexed_global_callback_variadic_results :
  top_level_indexed_global_callback_call -> expression_result list

val top_level_indexed_global_callback_variadic_count :
  top_level_indexed_global_callback_call -> int64

val top_level_indexed_global_callback_result_id :
  top_level_indexed_global_callback_call -> Id.t

val top_level_member_callback_source :
  top_level_member_callback_call -> Top_level_expression_tree.call

val top_level_member_callback_global :
  top_level_member_callback_call -> Global_type_resolution.global

val top_level_member_callback_value :
  top_level_member_callback_call -> Function_call_resolution.identifier_value

val top_level_member_callback_callee_result :
  top_level_member_callback_call -> expression_result

val top_level_member_callback_lookup :
  top_level_member_callback_call -> Aggregate_member_index.lookup

val top_level_member_callback_callable :
  top_level_member_callback_call -> Function_call_resolution.callable

val top_level_member_callback_fixed_results :
  top_level_member_callback_call -> top_level_fixed_result list

val top_level_member_callback_variadic_results :
  top_level_member_callback_call -> expression_result list

val top_level_member_callback_variadic_count :
  top_level_member_callback_call -> int64

val top_level_member_callback_result_id : top_level_member_callback_call -> Id.t

val declared_default_source :
  declared_default_result -> Function_call_resolution.default_use

val declared_default_parameter :
  declared_default_result -> Function_type_resolution.parameter

val declared_default_type : declared_default_result -> Type.t
val declared_default_class : declared_default_result -> result_class
val declared_default_kind : declared_default_result -> declared_default_kind

val declared_default_materialization :
  declared_default_result -> declared_default_materialization

val lastclass_previous_result :
  lastclass_substitution -> expression_result option

val lastclass_class_name : lastclass_substitution -> string option
val result_id : expression_result -> Id.t

val result_source :
  expression_result -> Function_call_resolution.argument_expression

val result_origin : expression_result -> Symbol.origin

val result_operand : expression_result -> expression_result option
(** Return the exact checked operand for a parenthesized, prefix, or
    postfix-cast result. Other expression forms currently return [None]. *)

val result_binary_operands :
  expression_result -> (expression_result * expression_result) option
(** Return the exact checked left and right children of a binary result in
    source order. Other expression forms return [None]. *)

val result_type : expression_result -> Type.t option
val result_category : expression_result -> value_category
val result_class : expression_result -> result_class
val result_execution_class : expression_result -> result_class option
val result_array_rank : expression_result -> int
val result_intrinsic_conversion : expression_result -> intrinsic_conversion

val result_member_lookup :
  expression_result -> Aggregate_member_index.lookup option

val result_outer_occurrence :
  expression_result -> Outer_expression_binding.occurrence option

val result_top_level_outer_occurrence :
  expression_result -> Top_level_outer_expression_binding.occurrence option

val result_outer_binding : expression_result -> Outer_environment.binding option

val result_aggregate_offset_path :
  expression_result -> aggregate_offset_path option

val aggregate_offset_base :
  aggregate_offset_path -> Module_expression_binding.publication

val aggregate_offset_current_type : aggregate_offset_path -> Type.t

val aggregate_offset_segments :
  aggregate_offset_path -> aggregate_offset_segment list

val aggregate_offset_value : aggregate_offset_path -> int64

val aggregate_offset_segment_lookup :
  aggregate_offset_segment -> Aggregate_member_index.lookup

val aggregate_offset_segment_cumulative_offset :
  aggregate_offset_segment -> int64

val result_call_resolution :
  expression_result -> Function_call_resolution.call_resolution option

val result_function_declaration :
  expression_result -> Function_resolution.resolved_declaration option

val result_function_address_path :
  expression_result ->
  Function_call_resolution.direct_function_address_path option

val value_category_name : value_category -> string
val result_class_name : result_class -> string
val intrinsic_conversion_name : intrinsic_conversion -> string
val result_use_name : result_use -> string
val condition_role_name : Function_call_resolution.condition_role -> string
val selector_mode_name : Function_call_resolution.selector_mode -> string
val return_presence_name : return_presence -> string
val declared_default_kind_name : declared_default_kind -> string

val declared_default_materialization_name :
  declared_default_materialization -> string

val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
