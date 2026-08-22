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
  | Lvalue
  | Unavailable

type result_class = Integer_result | F64_result | Unresolved_actual_class

type intrinsic_conversion =
  | No_intrinsic_conversion
  | Result_to_f64
  | Result_to_int

type result_use = Result_not_used
type expression_result
type declared_default_kind = Expression_default_kind | Lastclass_default_kind
type declared_default_materialization = Immediate_default | Aot_string_default
type declared_default_result

type fixed_path =
  | Provided_result of expression_result
  | Declared_default_result of declared_default_result

type lastclass_substitution
type fixed_result
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
type error_kind = Invalid_input of string
type error

val analyze :
  table:Symbol_table.t ->
  members:Aggregate_member_index.t ->
  Function_call_conversion_policy.t ->
  (t, error) result
(** Derive immutable source-expression results for call arguments, ordinary
    function expression statements, conditions, switch selectors, case values,
    and returns. Each checked expression has a deterministic identity, source
    origin, semantic type, value category, remaining array rank, conversion
    intent, forwarded result class, and any separate execution class needed by
    later lowering. A selected default has a result with its exact source and
    parameter, semantic parameter type, forwarded class, kind, and JIT or AOT
    materialization path; it does not receive an expression identity. A selected
    [lastclass] default also retains the previous provided result and derived
    base spelling. Each function expression statement records the
    [ICF_RES_NOT_USED] intent emitted by [PrsExpression] without mutating the
    parser AST. Function conditions retain their statement role without an
    invented Boolean conversion. Switch selectors retain their bounded or
    no-bound mode without applying range arithmetic. Switch cases retain
    implicit, single, or ranged structure; explicit F64 values carry the
    conversion performed by [LexExpressionI64] without being evaluated. Function
    returns retain the declared type, integer or F64 conversion intent, and
    warning facts for missing or unexpected values. *)

val owns_table : t -> Symbol_table.t -> bool
val owns_members : t -> Aggregate_member_index.t -> bool
val owns_policies : t -> Function_call_conversion_policy.t -> bool
val compilation_mode : t -> Function_resolution.compilation_mode
val functions : t -> resolved_function list
val all_results : t -> expression_result list
val function_symbol : resolved_function -> Symbol.t
val function_scope : resolved_function -> Symbol_table.scope
val function_item_index : resolved_function -> int
val function_calls : resolved_function -> call_result list

val function_expression_statements :
  resolved_function -> expression_statement_result list

val expression_statement_source :
  expression_statement_result ->
  Function_call_resolution.expression_statement_input

val expression_statement_value :
  expression_statement_result -> expression_result

val expression_statement_result_use : expression_statement_result -> result_use
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

val indirect_fixed_results : indirect_call -> fixed_result list
val indirect_variadic_results : indirect_call -> expression_result list
val fixed_source : fixed_result -> Function_call_conversion_policy.fixed_policy
val fixed_path : fixed_result -> fixed_path
val fixed_lastclass_substitution : fixed_result -> lastclass_substitution option

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
val result_type : expression_result -> Type.t option
val result_category : expression_result -> value_category
val result_class : expression_result -> result_class
val result_execution_class : expression_result -> result_class option
val result_array_rank : expression_result -> int
val result_intrinsic_conversion : expression_result -> intrinsic_conversion

val result_member_lookup :
  expression_result -> Aggregate_member_index.lookup option

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
