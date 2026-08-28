module Id = struct
  type t = int

  let compare = Int.compare
  let equal = Int.equal
  let to_int value = value
end

module Top_level_id = Top_level_identifier_resolution

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

type aggregate_offset_segment = {
  offset_lookup : Aggregate_member_index.lookup;
  offset_cumulative : int64;
}

type aggregate_offset_path = {
  offset_base : Module_expression_binding.publication;
  offset_current_type : Type.t;
  offset_segments : aggregate_offset_segment list;
  offset_value : int64;
}

type expression_result = {
  id : Id.t;
  source : Function_call_resolution.argument_expression;
  origin : Symbol.origin;
  operand_result : expression_result option;
  binary_operands : (expression_result * expression_result) option;
  source_type : Type.t option;
  category : value_category;
  result_class : result_class;
  execution_class : result_class option;
  array_rank : int;
  intrinsic_conversion : intrinsic_conversion;
  member_lookup : Aggregate_member_index.lookup option;
  aggregate_offset_path : aggregate_offset_path option;
  outer_occurrence : Outer_expression_binding.occurrence option;
  top_level_outer_occurrence :
    Top_level_outer_expression_binding.occurrence option;
  outer_binding : Outer_environment.binding option;
  call_resolution : Function_call_resolution.call_resolution option;
  function_declaration : Function_resolution.resolved_declaration option;
  function_address_path :
    Function_call_resolution.direct_function_address_path option;
}

type declared_default_kind = Expression_default_kind | Lastclass_default_kind
type declared_default_materialization = Immediate_default | Aot_string_default

type declared_default_result = {
  default_source : Function_call_resolution.default_use;
  default_parameter : Function_type_resolution.parameter;
  default_type : Type.t;
  default_class : result_class;
  default_kind : declared_default_kind;
  default_materialization : declared_default_materialization;
}

type fixed_path =
  | Provided_result of expression_result
  | Declared_default_result of declared_default_result

type lastclass_substitution = {
  previous_result_ : expression_result option;
  class_name_ : string option;
}

type fixed_result = {
  source : Function_call_conversion_policy.fixed_policy;
  path : fixed_path;
  lastclass_substitution : lastclass_substitution option;
}

type top_level_fixed_result = {
  top_level_fixed_source : Function_call_resolution.fixed_argument;
  top_level_fixed_target_class : result_class;
  top_level_fixed_path : fixed_path;
  top_level_fixed_lastclass_substitution : lastclass_substitution option;
}

type outer_callback_fixed_result = top_level_fixed_result

type outer_callback_call = {
  outer_callback_source : Function_call_resolution.call;
  outer_callback_occurrence : Outer_expression_binding.occurrence;
  outer_callback_binding : Outer_environment.binding;
  outer_callback_callee_result : expression_result option;
  outer_callback_callable : Function_call_resolution.callable;
  outer_callback_fixed_results : outer_callback_fixed_result list;
  outer_callback_variadic_results : expression_result list;
  outer_callback_variadic_count : int64;
  outer_callback_result_id : Id.t;
}

type top_level_direct_call = {
  top_level_direct_source : Top_level_expression_tree.call;
  top_level_direct_declaration : Function_resolution.resolved_declaration;
  top_level_direct_header : Function_type_resolution.resolved_function;
  top_level_direct_target_symbol : Symbol.t;
  top_level_direct_fixed_results : top_level_fixed_result list;
  top_level_direct_variadic_results : expression_result list;
  top_level_direct_variadic_count : int64;
  top_level_direct_result_id : Id.t;
}

type top_level_global_callback_call = {
  top_level_global_callback_source : Top_level_expression_tree.call;
  top_level_global_callback_global : Global_type_resolution.global;
  top_level_global_callback_value : Function_call_resolution.identifier_value;
  top_level_global_callback_callable : Function_call_resolution.callable;
  top_level_global_callback_fixed_results : top_level_fixed_result list;
  top_level_global_callback_variadic_results : expression_result list;
  top_level_global_callback_variadic_count : int64;
  top_level_global_callback_result_id : Id.t;
}

type top_level_outer_callback_call = {
  top_level_outer_callback_source : Top_level_expression_tree.call;
  top_level_outer_callback_occurrence :
    Top_level_outer_expression_binding.occurrence;
  top_level_outer_callback_binding : Outer_environment.binding;
  top_level_outer_callback_callee_result : expression_result option;
  top_level_outer_callback_callable : Function_call_resolution.callable;
  top_level_outer_callback_fixed_results : top_level_fixed_result list;
  top_level_outer_callback_variadic_results : expression_result list;
  top_level_outer_callback_variadic_count : int64;
  top_level_outer_callback_result_id : Id.t;
}

type top_level_indexed_global_callback_call = {
  top_level_indexed_global_callback_source : Top_level_expression_tree.call;
  top_level_indexed_global_callback_global : Global_type_resolution.global;
  top_level_indexed_global_callback_value :
    Function_call_resolution.identifier_value;
  top_level_indexed_global_callback_callee_result : expression_result;
  top_level_indexed_global_callback_callable :
    Function_call_resolution.callable;
  top_level_indexed_global_callback_fixed_results : top_level_fixed_result list;
  top_level_indexed_global_callback_variadic_results : expression_result list;
  top_level_indexed_global_callback_variadic_count : int64;
  top_level_indexed_global_callback_result_id : Id.t;
}

type top_level_member_callback_call = {
  top_level_member_callback_source : Top_level_expression_tree.call;
  top_level_member_callback_global : Global_type_resolution.global;
  top_level_member_callback_value : Function_call_resolution.identifier_value;
  top_level_member_callback_callee_result : expression_result;
  top_level_member_callback_lookup : Aggregate_member_index.lookup;
  top_level_member_callback_callable : Function_call_resolution.callable;
  top_level_member_callback_fixed_results : top_level_fixed_result list;
  top_level_member_callback_variadic_results : expression_result list;
  top_level_member_callback_variadic_count : int64;
  top_level_member_callback_result_id : Id.t;
}

type direct_call = {
  source : Function_call_conversion_policy.direct_call;
  fixed_results : fixed_result list;
  variadic_results : expression_result list;
}

type indirect_call = {
  source : Function_call_conversion_policy.indirect_call;
  callee_result : expression_result option;
  fixed_results : fixed_result list;
  variadic_results : expression_result list;
}

type call_result =
  | Direct_call_result of direct_call
  | Indirect_call_result of indirect_call
  | Deferred_call_result of Function_call_resolution.call_resolution

type return_presence =
  | Matching_value
  | Matching_no_value
  | Unexpected_value
  | Missing_value

type condition_result = {
  condition_source : Function_call_resolution.condition_input;
  condition_value : expression_result;
}

type expression_statement_result = {
  expression_statement_source :
    Function_call_resolution.expression_statement_input;
  expression_statement_value : expression_result;
  expression_statement_result_use : result_use;
}

type implicit_output_argument_result = {
  implicit_output_argument_source :
    Function_call_resolution.implicit_output_argument;
  implicit_output_argument_value : expression_result;
}

type implicit_output_result = {
  implicit_output_source : Function_call_resolution.implicit_output_input;
  implicit_output_fixed_value : expression_result;
  implicit_output_arguments : implicit_output_argument_result list;
  implicit_output_result_use : result_use;
}

type selector_result = {
  selector_source : Function_call_resolution.selector_input;
  selector_value : expression_result;
}

type switch_case_value = {
  switch_case_value_result : expression_result;
  switch_case_value_conversion : intrinsic_conversion;
}

type switch_case_pattern_result =
  | Implicit_case_result
  | Single_case_result of switch_case_value
  | Ranged_case_result of {
      start_value : switch_case_value;
      end_value : switch_case_value;
    }

type switch_case_result = {
  switch_case_source : Function_call_resolution.switch_case_input;
  switch_case_pattern : switch_case_pattern_result;
}

type return_result = {
  return_source : Function_call_resolution.return_input;
  return_declared_type : Type.t;
  return_declared_class : result_class;
  return_value : expression_result option;
  return_conversion : intrinsic_conversion;
  return_presence : return_presence;
}

type resolved_function = {
  symbol : Symbol.t;
  scope : Symbol_table.scope;
  item_index : int;
  calls : call_result list;
  outer_callback_calls : outer_callback_call list;
  expression_statements : expression_statement_result list;
  implicit_outputs : implicit_output_result list;
  conditions : condition_result list;
  selectors : selector_result list;
  switch_cases : switch_case_result list;
  returns : return_result list;
}

type t = {
  table : Symbol_table.t;
  members : Aggregate_member_index.t;
  policies : Function_call_conversion_policy.t;
  outer : Outer_expression_binding.t option;
  compilation_mode : Function_resolution.compilation_mode;
  functions : resolved_function list;
  all_results : expression_result list;
}

type top_level_root_result = {
  top_level_root_source : Top_level_expression_tree.root;
  top_level_root_value : expression_result;
  top_level_root_result_use : result_use option;
}

type top_level_statement_result = {
  top_level_statement_source : Top_level_expression_tree.statement;
  top_level_statement_roots : top_level_root_result list;
}

type top_level_t = {
  top_level_table : Symbol_table.t;
  top_level_members : Aggregate_member_index.t;
  top_level_policies : Function_call_conversion_policy.t;
  top_level_identifiers : Top_level_identifier_resolution.t;
  top_level_source : Top_level_expression_tree.t;
  top_level_compilation_mode : Function_resolution.compilation_mode;
  top_level_statements : top_level_statement_result list;
  top_level_direct_calls : top_level_direct_call list;
  top_level_global_callback_calls : top_level_global_callback_call list;
  top_level_outer_callback_calls : top_level_outer_callback_call list;
  top_level_indexed_global_callback_calls :
    top_level_indexed_global_callback_call list;
  top_level_member_callback_calls : top_level_member_callback_call list;
  top_level_all_results : expression_result list;
}

type error_kind = Invalid_input of string
type error = { code : string; kind : error_kind; origin : Symbol.origin option }
type expression_context = Value_context | Lvalue_context

type build_state = {
  next_id : int;
  results_rev : expression_result list;
  outer_function : Outer_expression_binding.resolved_function option;
  outer_callback_calls_rev : outer_callback_call list;
  top_level_identifier_batch : Top_level_identifier_resolution.t option;
  top_level_direct_calls_rev : top_level_direct_call list;
  top_level_global_callback_calls_rev : top_level_global_callback_call list;
  top_level_outer_callback_calls_rev : top_level_outer_callback_call list;
  top_level_indexed_global_callback_calls_rev :
    top_level_indexed_global_callback_call list;
  top_level_member_callback_calls_rev : top_level_member_callback_call list;
}

let owns_table result table = result.table == table
let owns_members result members = result.members == members
let owns_policies result policies = result.policies == policies

let owns_outer result outer =
  match result.outer with
  | Some owned -> owned == outer
  | None -> false

let compilation_mode result = result.compilation_mode
let functions result = result.functions
let all_results result = result.all_results
let top_level_owns_table result table = result.top_level_table == table
let top_level_owns_members result members = result.top_level_members == members

let top_level_owns_policies result policies =
  result.top_level_policies == policies

let top_level_owns_identifiers result identifiers =
  result.top_level_identifiers == identifiers

let top_level_source result = result.top_level_source
let top_level_compilation_mode result = result.top_level_compilation_mode
let top_level_statements result = result.top_level_statements
let top_level_direct_calls result = result.top_level_direct_calls

let top_level_global_callback_calls result =
  result.top_level_global_callback_calls

let top_level_outer_callback_calls result =
  result.top_level_outer_callback_calls

let top_level_indexed_global_callback_calls result =
  result.top_level_indexed_global_callback_calls

let top_level_member_callback_calls result =
  result.top_level_member_callback_calls

let top_level_all_results result = result.top_level_all_results
let top_level_statement_source result = result.top_level_statement_source
let top_level_statement_roots result = result.top_level_statement_roots
let top_level_root_source result = result.top_level_root_source
let top_level_root_value result = result.top_level_root_value
let top_level_root_result_use result = result.top_level_root_result_use
let function_symbol (function_ : resolved_function) = function_.symbol
let function_scope (function_ : resolved_function) = function_.scope
let function_item_index (function_ : resolved_function) = function_.item_index
let function_calls (function_ : resolved_function) = function_.calls

let function_outer_callback_calls (function_ : resolved_function) =
  function_.outer_callback_calls

let function_expression_statements (function_ : resolved_function) =
  function_.expression_statements

let expression_statement_source result = result.expression_statement_source
let expression_statement_value result = result.expression_statement_value

let expression_statement_result_use result =
  result.expression_statement_result_use

let function_implicit_outputs (function_ : resolved_function) =
  function_.implicit_outputs

let implicit_output_source result = result.implicit_output_source
let implicit_output_fixed_value result = result.implicit_output_fixed_value
let implicit_output_arguments result = result.implicit_output_arguments
let implicit_output_result_use result = result.implicit_output_result_use

let implicit_output_argument_source result =
  result.implicit_output_argument_source

let implicit_output_argument_value result =
  result.implicit_output_argument_value

let function_conditions (function_ : resolved_function) = function_.conditions
let condition_source result = result.condition_source
let condition_value result = result.condition_value
let function_selectors (function_ : resolved_function) = function_.selectors
let selector_source result = result.selector_source
let selector_value result = result.selector_value

let function_switch_cases (function_ : resolved_function) =
  function_.switch_cases

let switch_case_source result = result.switch_case_source
let switch_case_pattern result = result.switch_case_pattern
let switch_case_value_result result = result.switch_case_value_result
let switch_case_value_conversion result = result.switch_case_value_conversion
let function_returns (function_ : resolved_function) = function_.returns
let return_source result = result.return_source
let return_declared_type result = result.return_declared_type
let return_declared_class result = result.return_declared_class
let return_value result = result.return_value
let return_conversion result = result.return_conversion
let return_presence result = result.return_presence
let direct_source (call : direct_call) = call.source
let direct_fixed_results (call : direct_call) = call.fixed_results
let direct_variadic_results (call : direct_call) = call.variadic_results
let indirect_source (call : indirect_call) = call.source
let indirect_callee_result (call : indirect_call) = call.callee_result
let indirect_fixed_results (call : indirect_call) = call.fixed_results
let indirect_variadic_results (call : indirect_call) = call.variadic_results
let fixed_source (fixed : fixed_result) = fixed.source
let fixed_path (fixed : fixed_result) = fixed.path
let declared_default_source result = result.default_source
let declared_default_parameter result = result.default_parameter
let declared_default_type result = result.default_type
let declared_default_class result = result.default_class
let declared_default_kind result = result.default_kind
let declared_default_materialization result = result.default_materialization

let fixed_lastclass_substitution (fixed : fixed_result) =
  fixed.lastclass_substitution

let top_level_fixed_source (fixed : top_level_fixed_result) =
  fixed.top_level_fixed_source

let top_level_fixed_target_class (fixed : top_level_fixed_result) =
  fixed.top_level_fixed_target_class

let top_level_fixed_path (fixed : top_level_fixed_result) =
  fixed.top_level_fixed_path

let top_level_fixed_lastclass_substitution (fixed : top_level_fixed_result) =
  fixed.top_level_fixed_lastclass_substitution

let outer_callback_fixed_source (fixed : outer_callback_fixed_result) =
  fixed.top_level_fixed_source

let outer_callback_fixed_target_class (fixed : outer_callback_fixed_result) =
  fixed.top_level_fixed_target_class

let outer_callback_fixed_path (fixed : outer_callback_fixed_result) =
  fixed.top_level_fixed_path

let outer_callback_fixed_lastclass_substitution
    (fixed : outer_callback_fixed_result) =
  fixed.top_level_fixed_lastclass_substitution

let outer_callback_source (call : outer_callback_call) =
  call.outer_callback_source

let outer_callback_occurrence (call : outer_callback_call) =
  call.outer_callback_occurrence

let outer_callback_binding (call : outer_callback_call) =
  call.outer_callback_binding

let outer_callback_callee_result (call : outer_callback_call) =
  call.outer_callback_callee_result

let outer_callback_callable (call : outer_callback_call) =
  call.outer_callback_callable

let outer_callback_fixed_results (call : outer_callback_call) =
  call.outer_callback_fixed_results

let outer_callback_variadic_results (call : outer_callback_call) =
  call.outer_callback_variadic_results

let outer_callback_variadic_count (call : outer_callback_call) =
  call.outer_callback_variadic_count

let outer_callback_result_id (call : outer_callback_call) =
  call.outer_callback_result_id

let top_level_direct_source (call : top_level_direct_call) =
  call.top_level_direct_source

let top_level_direct_declaration (call : top_level_direct_call) =
  call.top_level_direct_declaration

let top_level_direct_header (call : top_level_direct_call) =
  call.top_level_direct_header

let top_level_direct_target_symbol (call : top_level_direct_call) =
  call.top_level_direct_target_symbol

let top_level_direct_fixed_results (call : top_level_direct_call) =
  call.top_level_direct_fixed_results

let top_level_direct_variadic_results (call : top_level_direct_call) =
  call.top_level_direct_variadic_results

let top_level_direct_variadic_count (call : top_level_direct_call) =
  call.top_level_direct_variadic_count

let top_level_direct_result_id (call : top_level_direct_call) =
  call.top_level_direct_result_id

let top_level_global_callback_source (call : top_level_global_callback_call) =
  call.top_level_global_callback_source

let top_level_global_callback_global (call : top_level_global_callback_call) =
  call.top_level_global_callback_global

let top_level_global_callback_value (call : top_level_global_callback_call) =
  call.top_level_global_callback_value

let top_level_global_callback_callable (call : top_level_global_callback_call) =
  call.top_level_global_callback_callable

let top_level_global_callback_fixed_results
    (call : top_level_global_callback_call) =
  call.top_level_global_callback_fixed_results

let top_level_global_callback_variadic_results
    (call : top_level_global_callback_call) =
  call.top_level_global_callback_variadic_results

let top_level_global_callback_variadic_count
    (call : top_level_global_callback_call) =
  call.top_level_global_callback_variadic_count

let top_level_global_callback_result_id (call : top_level_global_callback_call)
    =
  call.top_level_global_callback_result_id

let top_level_outer_callback_source (call : top_level_outer_callback_call) =
  call.top_level_outer_callback_source

let top_level_outer_callback_occurrence (call : top_level_outer_callback_call) =
  call.top_level_outer_callback_occurrence

let top_level_outer_callback_binding (call : top_level_outer_callback_call) =
  call.top_level_outer_callback_binding

let top_level_outer_callback_callee_result
    (call : top_level_outer_callback_call) =
  call.top_level_outer_callback_callee_result

let top_level_outer_callback_callable (call : top_level_outer_callback_call) =
  call.top_level_outer_callback_callable

let top_level_outer_callback_fixed_results
    (call : top_level_outer_callback_call) =
  call.top_level_outer_callback_fixed_results

let top_level_outer_callback_variadic_results
    (call : top_level_outer_callback_call) =
  call.top_level_outer_callback_variadic_results

let top_level_outer_callback_variadic_count
    (call : top_level_outer_callback_call) =
  call.top_level_outer_callback_variadic_count

let top_level_outer_callback_result_id (call : top_level_outer_callback_call) =
  call.top_level_outer_callback_result_id

let top_level_outer_callback_call_index (call : top_level_outer_callback_call) =
  call.top_level_outer_callback_source |> Top_level_expression_tree.call_source
  |> Function_call_resolution.call_index

let top_level_indexed_global_callback_source
    (call : top_level_indexed_global_callback_call) =
  call.top_level_indexed_global_callback_source

let top_level_indexed_global_callback_global
    (call : top_level_indexed_global_callback_call) =
  call.top_level_indexed_global_callback_global

let top_level_indexed_global_callback_value
    (call : top_level_indexed_global_callback_call) =
  call.top_level_indexed_global_callback_value

let top_level_indexed_global_callback_callee_result
    (call : top_level_indexed_global_callback_call) =
  call.top_level_indexed_global_callback_callee_result

let top_level_indexed_global_callback_callable
    (call : top_level_indexed_global_callback_call) =
  call.top_level_indexed_global_callback_callable

let top_level_indexed_global_callback_fixed_results
    (call : top_level_indexed_global_callback_call) =
  call.top_level_indexed_global_callback_fixed_results

let top_level_indexed_global_callback_variadic_results
    (call : top_level_indexed_global_callback_call) =
  call.top_level_indexed_global_callback_variadic_results

let top_level_indexed_global_callback_variadic_count
    (call : top_level_indexed_global_callback_call) =
  call.top_level_indexed_global_callback_variadic_count

let top_level_indexed_global_callback_result_id
    (call : top_level_indexed_global_callback_call) =
  call.top_level_indexed_global_callback_result_id

let top_level_member_callback_source (call : top_level_member_callback_call) =
  call.top_level_member_callback_source

let top_level_member_callback_global (call : top_level_member_callback_call) =
  call.top_level_member_callback_global

let top_level_member_callback_value (call : top_level_member_callback_call) =
  call.top_level_member_callback_value

let top_level_member_callback_callee_result
    (call : top_level_member_callback_call) =
  call.top_level_member_callback_callee_result

let top_level_member_callback_lookup (call : top_level_member_callback_call) =
  call.top_level_member_callback_lookup

let top_level_member_callback_callable (call : top_level_member_callback_call) =
  call.top_level_member_callback_callable

let top_level_member_callback_fixed_results
    (call : top_level_member_callback_call) =
  call.top_level_member_callback_fixed_results

let top_level_member_callback_variadic_results
    (call : top_level_member_callback_call) =
  call.top_level_member_callback_variadic_results

let top_level_member_callback_variadic_count
    (call : top_level_member_callback_call) =
  call.top_level_member_callback_variadic_count

let top_level_member_callback_result_id (call : top_level_member_callback_call)
    =
  call.top_level_member_callback_result_id

let lastclass_previous_result substitution = substitution.previous_result_
let lastclass_class_name substitution = substitution.class_name_
let result_id (result : expression_result) = result.id
let result_source (result : expression_result) = result.source
let result_origin (result : expression_result) = result.origin
let result_operand (result : expression_result) = result.operand_result
let result_binary_operands (result : expression_result) = result.binary_operands
let result_type (result : expression_result) = result.source_type
let result_category (result : expression_result) = result.category
let result_class (result : expression_result) = result.result_class
let result_execution_class (result : expression_result) = result.execution_class
let result_array_rank (result : expression_result) = result.array_rank

let result_intrinsic_conversion (result : expression_result) =
  result.intrinsic_conversion

let result_member_lookup (result : expression_result) = result.member_lookup
let result_outer_occurrence result = result.outer_occurrence
let result_top_level_outer_occurrence result = result.top_level_outer_occurrence
let result_outer_binding result = result.outer_binding

let result_aggregate_offset_path (result : expression_result) =
  result.aggregate_offset_path

let aggregate_offset_base path = path.offset_base
let aggregate_offset_current_type path = path.offset_current_type
let aggregate_offset_segments path = path.offset_segments
let aggregate_offset_value path = path.offset_value
let aggregate_offset_segment_lookup segment = segment.offset_lookup

let aggregate_offset_segment_cumulative_offset segment =
  segment.offset_cumulative

let result_call_resolution (result : expression_result) = result.call_resolution
let result_function_declaration result = result.function_declaration
let result_function_address_path result = result.function_address_path

let result_is_direct_function (result : expression_result) =
  match Function_call_resolution.argument_expression_kind result.source with
  | Function_call_resolution.Bound_identifier_expression identifier ->
      Function_call_resolution.bound_identifier_shape identifier
      = Function_call_resolution.Direct_function_value
  | Function_call_resolution.Top_level_bound_identifier_expression _ ->
      result.category = Function_value
      && Option.is_some result.function_declaration
  | _ -> false

let result_direct_function_name (result : expression_result) =
  match Function_call_resolution.argument_expression_kind result.source with
  | Function_call_resolution.Bound_identifier_expression identifier ->
      identifier |> Function_call_resolution.bound_identifier_occurrence
      |> Module_expression_binding.occurrence_name
  | Function_call_resolution.Top_level_bound_identifier_expression identifier ->
      identifier
      |> Function_call_resolution.top_level_bound_identifier_occurrence
      |> Top_level_outer_expression_binding.occurrence_name
  | _ -> "<function>"

let value_category_name = function
  | Object_value -> "object-value"
  | Address_value -> "address-value"
  | Array_value -> "array-value"
  | Callback_value -> "callback-value"
  | Function_value -> "function-value"
  | Offset_value -> "offset-value"
  | Lvalue -> "lvalue"
  | Unavailable -> "unavailable"

let result_class_name = function
  | Integer_result -> "integer-result"
  | F64_result -> "f64-result"
  | Unresolved_actual_class -> "unresolved"

let intrinsic_conversion_name = function
  | No_intrinsic_conversion -> "none"
  | Result_to_f64 -> "ICF_RES_TO_F64"
  | Result_to_int -> "ICF_RES_TO_INT"

let result_use_name Result_not_used = "ICF_RES_NOT_USED"

let condition_role_name = function
  | Function_call_resolution.If_condition -> "if"
  | Function_call_resolution.While_condition -> "while"
  | Function_call_resolution.Do_while_condition -> "do-while"
  | Function_call_resolution.For_condition -> "for"

let selector_mode_name = function
  | Function_call_resolution.Bounded_switch -> "bounded"
  | Function_call_resolution.No_bound_switch -> "no-bound"

let return_presence_name = function
  | Matching_value -> "matching-value"
  | Matching_no_value -> "matching-no-value"
  | Unexpected_value -> "unexpected-value"
  | Missing_value -> "missing-value"

let declared_default_kind_name = function
  | Expression_default_kind -> "expression"
  | Lastclass_default_kind -> "lastclass"

let declared_default_materialization_name = function
  | Immediate_default -> "immediate"
  | Aot_string_default -> "aot-string-constant"

let invalid_input ?origin message =
  let kind = Invalid_input message in
  { code = "HCSEMA0046"; kind; origin }

let invalid_top_level_input ?origin message =
  let kind = Invalid_input message in
  { code = "HCSEMA0057"; kind; origin }

let error_code error = error.code
let error_kind error = error.kind
let error_origin error = error.origin

let error_message error =
  match error.kind with
  | Invalid_input message -> message

let error_to_string error = error.code ^ ": " ^ error_message error

let primitive_type ?(form = Type.Internal_storage) primitive pointer_depth =
  match Type.make_primitive ~form ~primitive ~pointer_depth with
  | Ok type_ -> Some type_
  | Error _ -> None

let integer_type = primitive_type Primitive_type.I64 0
let float_type = primitive_type Primitive_type.F64 0
let string_type = primitive_type Primitive_type.U8 1
let rip_address_type = primitive_type Primitive_type.I64 0

let type_is_owned table type_ =
  match Type.base type_ with
  | Type.Primitive _ -> true
  | Type.Aggregate symbol -> Symbol_table.owns_symbol table symbol

let forwarded_class policies ~before_item_index type_ =
  match
    Function_call_conversion_policy.forwarded_type_class policies
      ~before_item_index type_
  with
  | Function_call_conversion_policy.Integer_result -> Integer_result
  | Function_call_conversion_policy.F64_result -> F64_result

let allocate state =
  if state.next_id = max_int then
    Error (invalid_input "expression identity space is exhausted")
  else Ok (state.next_id, { state with next_id = state.next_id + 1 })

let record state result =
  (result, { state with results_rev = result :: state.results_rev })

let make_result ?operand_result ?binary_operands ?(array_rank = 0)
    ?execution_class ?member_lookup ?aggregate_offset_path ?outer_occurrence
    ?top_level_outer_occurrence ?outer_binding ?call_resolution
    ?function_declaration ?function_address_path
    ?(intrinsic_conversion = No_intrinsic_conversion) state ~id ~source
    ~source_type ~category ~result_class =
  record state
    {
      id;
      source;
      origin = Function_call_resolution.argument_expression_origin source;
      operand_result;
      binary_operands;
      source_type;
      category;
      result_class;
      execution_class;
      array_rank;
      intrinsic_conversion;
      member_lookup;
      aggregate_offset_path;
      outer_occurrence;
      top_level_outer_occurrence;
      outer_binding;
      call_resolution;
      function_declaration;
      function_address_path;
    }

let known_type table type_ =
  if type_is_owned table type_ then Ok type_
  else Error (invalid_input "expression type belongs to another symbol table")

let replace_result state replacement =
  let replaced = ref false in
  let results_rev =
    List.map
      (fun result ->
        if Id.equal result.id replacement.id then (
          replaced := true;
          replacement)
        else result)
      state.results_rev
  in
  if !replaced then Ok { state with results_rev }
  else Error (invalid_input "typed expression result is absent from its owner")

let set_intrinsic_conversion state result intrinsic_conversion =
  if result.intrinsic_conversion = intrinsic_conversion then Ok (result, state)
  else
    let replacement = { result with intrinsic_conversion } in
    match replace_result state replacement with
    | Error _ as error -> error
    | Ok state -> Ok (replacement, state)

let select_known_binary_type left right result_class =
  match result_class with
  | F64_result -> float_type
  | Unresolved_actual_class -> None
  | Integer_result -> (
      match (left.source_type, right.source_type) with
      | Some left_type, Some right_type -> (
          match (Type.base left_type, Type.base right_type) with
          | ( Type.Primitive (_, left_primitive),
              Type.Primitive (_, right_primitive) )
            when Type.pointer_depth left_type = 0
                 && Type.pointer_depth right_type = 0 ->
              let left_id = (Primitive_type.info left_primitive).raw_id in
              let right_id = (Primitive_type.info right_primitive).raw_id in
              if left_id >= right_id then Some left_type else Some right_type
          | _ -> None)
      | None, _ | _, None -> None)

let is_writable_storage_type = function
  | Some type_ when Type.pointer_depth type_ > 0 -> true
  | Some type_ -> (
      match Type.base type_ with
      | Type.Primitive _ -> true
      | Type.Aggregate _ -> false)
  | None -> false

let validate_update_operand operand ~operator_origin ~operator_name =
  let invalid message = Error (invalid_input ~origin:operator_origin message) in
  match (operand.category, operand.source_type) with
  | Lvalue, Some _ when is_writable_storage_type operand.source_type -> Ok ()
  | Unavailable, _ -> invalid (operator_name ^ " operand is unavailable")
  | Lvalue, None -> invalid (operator_name ^ " operand has no checked type")
  | Lvalue, Some _ ->
      invalid
        (operator_name ^ " operand is not a pointer or internal storage value")
  | ( ( Object_value
      | Address_value
      | Array_value
      | Callback_value
      | Function_value
      | Offset_value ),
      _ ) -> invalid (operator_name ^ " operand is not an lvalue")

let policy_call_resolution = function
  | Function_call_conversion_policy.Direct_call_policy policy ->
      Function_call_resolution.Direct_call
        (Function_call_conversion_policy.direct_source policy)
  | Function_call_conversion_policy.Indirect_call_policy policy ->
      Function_call_resolution.Indirect_call
        (Function_call_conversion_policy.indirect_source policy)
  | Function_call_conversion_policy.Deferred_call_policy resolution ->
      resolution

let source_call = function
  | Function_call_resolution.Direct_call direct ->
      Function_call_resolution.direct_source direct
  | Function_call_resolution.Indirect_call indirect ->
      Function_call_resolution.indirect_source indirect
  | Function_call_resolution.Deferred_call { call; _ } -> call

let nested_call_resolution policies ~before_item_index origin =
  let owners =
    policies |> Function_call_conversion_policy.functions
    |> List.filter (fun function_ ->
        Function_call_conversion_policy.function_item_index function_
        = before_item_index)
  in
  match owners with
  | [] -> Error (invalid_input ~origin "nested call has no owning function")
  | _ :: _ :: _ ->
      Error (invalid_input ~origin "nested call has multiple owning functions")
  | [ owner ] -> (
      let matches =
        owner |> Function_call_conversion_policy.function_calls
        |> List.map policy_call_resolution
        |> List.filter (fun resolution ->
            Function_call_resolution.call_origin (source_call resolution)
            = origin)
      in
      match matches with
      | [] -> Ok None
      | [ resolution ] -> Ok (Some resolution)
      | _ ->
          Error
            (invalid_input ~origin
               "nested call matches multiple call-resolution records"))

let top_level_call_for_expression state source =
  match state.top_level_identifier_batch with
  | None -> Ok None
  | Some identifiers -> (
      let matches =
        identifiers |> Top_level_identifier_resolution.source
        |> Top_level_expression_tree.all_calls
        |> List.filter (fun call ->
            Top_level_expression_tree.call_result_expression call == source)
      in
      match matches with
      | [] ->
          Error
            (invalid_top_level_input
               ~origin:
                 (Function_call_resolution.argument_expression_origin source)
               "top-level call result is absent from its checked tree")
      | [ call ] -> Ok (Some call)
      | _ ->
          Error
            (invalid_top_level_input
               ~origin:
                 (Function_call_resolution.argument_expression_origin source)
               "top-level call result matches multiple checked calls"))

let lastclass_name policies ~before_item_index result =
  match result.source_type with
  | None -> None
  | Some source_type -> (
      let forwarded =
        Function_call_conversion_policy.forwarded_type policies
          ~before_item_index source_type
      in
      match Type.base forwarded with
      | Type.Aggregate symbol -> Some (Symbol.name symbol)
      | Type.Primitive (form, primitive) ->
          let info = Primitive_type.info primitive in
          Some
            (match form with
            | Type.Internal_storage -> info.storage_spelling
            | Type.Public_spelling -> (
                if Type.pointer_depth source_type <> 0 then info.spelling
                else
                  match info.declaration_form with
                  | Primitive_type.Internal_type -> info.spelling
                  | Primitive_type.Public_union -> info.storage_spelling)))

let lastclass_substitution policies ~before_item_index previous default =
  match Function_call_resolution.default_parameter_default default with
  | Function_type_resolution.Expression_default _ -> None
  | Function_type_resolution.Lastclass_default _ ->
      Some
        {
          previous_result_ = previous;
          class_name_ =
            Option.bind previous (lastclass_name policies ~before_item_index);
        }

let declared_default_result policies ~before_item_index fixed default =
  let parameter = Function_call_resolution.fixed_parameter fixed in
  let type_ =
    parameter |> Function_type_resolution.parameter_type_reference
    |> Type_reference.resolved_type
  in
  let kind, contains_string_literal =
    match Function_call_resolution.default_parameter_default default with
    | Function_type_resolution.Expression_default { contains_string_literal; _ }
      -> (Expression_default_kind, contains_string_literal)
    | Function_type_resolution.Lastclass_default _ ->
        (Lastclass_default_kind, true)
  in
  let materialization =
    match
      ( Function_call_conversion_policy.compilation_mode policies,
        contains_string_literal )
    with
    | Function_resolution.Aot, true -> Aot_string_default
    | Function_resolution.Aot, false | Function_resolution.Jit, _ ->
        Immediate_default
  in
  {
    default_source = default;
    default_parameter = parameter;
    default_type = type_;
    default_class = forwarded_class policies ~before_item_index type_;
    default_kind = kind;
    default_materialization = materialization;
  }

let result_class_of_target = function
  | Function_call_conversion_policy.Integer_result -> Integer_result
  | Function_call_conversion_policy.F64_result -> F64_result

let resolve_member_lookup members ~before_item_index ~aggregate_symbol
    ~member_name ~member_origin =
  match Aggregate_member_index.find_aggregate members aggregate_symbol with
  | None ->
      Error
        (invalid_input ~origin:member_origin
           (Printf.sprintf "aggregate `%s` has no completed member index"
              (Symbol.name aggregate_symbol)))
  | Some aggregate
    when Aggregate_member_index.aggregate_item_index aggregate
         >= before_item_index ->
      Error
        (invalid_input ~origin:member_origin
           (Printf.sprintf
              "aggregate `%s` is not complete before this member access"
              (Symbol.name aggregate_symbol)))
  | Some _ -> (
      match
        Aggregate_member_index.lookup members ~aggregate:aggregate_symbol
          ~name:member_name
      with
      | Error error ->
          Error
            (invalid_input ~origin:member_origin
               (Aggregate_member_index.error_message error))
      | Ok None ->
          Error
            (invalid_input ~origin:member_origin
               (Printf.sprintf "aggregate `%s` has no member `%s`"
                  (Symbol.name aggregate_symbol)
                  member_name))
      | Ok (Some lookup) -> Ok lookup)

let rec callback_array_index_depth ~callee expression =
  match Function_call_resolution.argument_expression_kind expression with
  | Function_call_resolution.Parenthesized_expression grouped ->
      callback_array_index_depth ~callee grouped
  | Function_call_resolution.Index_expression index ->
      Option.map Int.succ
        (callback_array_index_depth ~callee
           (Function_call_resolution.index_base index))
  | Function_call_resolution.Top_level_bound_identifier_expression identifier ->
      let occurrence =
        Function_call_resolution.top_level_bound_identifier_occurrence
          identifier
      in
      if occurrence == callee then Some 0 else None
  | Function_call_resolution.Integer_literal _
  | Function_call_resolution.Float_literal _
  | Function_call_resolution.Character_literal _
  | Function_call_resolution.String_literal _
  | Function_call_resolution.Prefix_expression _
  | Function_call_resolution.Postfix_expression _
  | Function_call_resolution.Postfix_cast_expression _
  | Function_call_resolution.Binary_expression _
  | Function_call_resolution.Member_access_expression _
  | Function_call_resolution.Bound_identifier_expression _
  | Function_call_resolution.Aggregate_offset_base_expression _
  | Function_call_resolution.Sizeof_expression _
  | Function_call_resolution.Defined_expression _
  | Function_call_resolution.Unresolved_expression _ -> None

let rec function_callback_array_index_depth ~callee expression =
  match Function_call_resolution.argument_expression_kind expression with
  | Function_call_resolution.Parenthesized_expression grouped ->
      function_callback_array_index_depth ~callee grouped
  | Function_call_resolution.Index_expression index ->
      Option.map Int.succ
        (function_callback_array_index_depth ~callee
           (Function_call_resolution.index_base index))
  | Function_call_resolution.Bound_identifier_expression identifier ->
      let occurrence =
        Function_call_resolution.bound_identifier_occurrence identifier
      in
      if occurrence == callee then Some 0 else None
  | Function_call_resolution.Unresolved_expression
      Function_call_resolution.Identifier_expression ->
      if
        Function_call_resolution.argument_expression_origin expression
        = Module_expression_binding.occurrence_origin callee
      then Some 0
      else None
  | Function_call_resolution.Integer_literal _
  | Function_call_resolution.Float_literal _
  | Function_call_resolution.Character_literal _
  | Function_call_resolution.String_literal _
  | Function_call_resolution.Prefix_expression _
  | Function_call_resolution.Postfix_expression _
  | Function_call_resolution.Postfix_cast_expression _
  | Function_call_resolution.Binary_expression _
  | Function_call_resolution.Member_access_expression _
  | Function_call_resolution.Aggregate_offset_base_expression _
  | Function_call_resolution.Top_level_bound_identifier_expression _
  | Function_call_resolution.Sizeof_expression _
  | Function_call_resolution.Defined_expression _
  | Function_call_resolution.Unresolved_expression _ -> None

let outer_binding_for_expression state source =
  match state.outer_function with
  | None -> Ok None
  | Some function_ -> (
      let source_origin =
        Function_call_resolution.argument_expression_origin source
      in
      let matches =
        function_ |> Outer_expression_binding.function_occurrences
        |> List.filter_map (fun occurrence ->
            if
              Outer_expression_binding.occurrence_origin occurrence
              <> source_origin
            then None
            else
              match
                Outer_expression_binding.occurrence_resolution occurrence
              with
              | Outer_expression_binding.Outer_binding binding ->
                  Some (occurrence, binding)
              | Outer_expression_binding.Local_binding _
              | Outer_expression_binding.Module_binding _ -> None)
      in
      match matches with
      | [] -> Ok None
      | [ match_ ] -> Ok (Some match_)
      | _ ->
          Error
            (invalid_input ~origin:source_origin
               "outer expression binding repeats one source occurrence"))

let outer_binding_for_occurrence state source =
  match state.outer_function with
  | None -> Ok None
  | Some function_ -> (
      let matches =
        function_ |> Outer_expression_binding.function_occurrences
        |> List.filter_map (fun occurrence ->
            if Outer_expression_binding.occurrence_source occurrence != source
            then None
            else
              match
                Outer_expression_binding.occurrence_resolution occurrence
              with
              | Outer_expression_binding.Outer_binding binding ->
                  Some (occurrence, binding)
              | Outer_expression_binding.Local_binding _
              | Outer_expression_binding.Module_binding _ -> None)
      in
      match matches with
      | [] -> Ok None
      | [ match_ ] -> Ok (Some match_)
      | _ ->
          Error
            (invalid_input
               ~origin:(Module_expression_binding.occurrence_origin source)
               "outer expression binding repeats one source occurrence"))

let rec type_expression table members policies ~before_item_index ~context
    ?(allow_aggregate_offset_base = false)
    ?(intrinsic_conversion = No_intrinsic_conversion) state source =
  match allocate state with
  | Error _ as error -> error
  | Ok (id, state) -> (
      let finish ?operand_result ?(source_type = None) ?(array_rank = 0)
          ?member_lookup ?aggregate_offset_path ?outer_occurrence
          ?top_level_outer_occurrence ?outer_binding ?call_resolution
          ?function_declaration ?function_address_path category result_class
          state =
        Ok
          (make_result ?operand_result ~array_rank ?member_lookup
             ?aggregate_offset_path ?outer_occurrence
             ?top_level_outer_occurrence ?outer_binding ?call_resolution
             ?function_declaration ?function_address_path ~intrinsic_conversion
             state ~id ~source ~source_type ~category ~result_class)
      in
      match Function_call_resolution.argument_expression_kind source with
      | Function_call_resolution.Integer_literal _
      | Function_call_resolution.Character_literal _ ->
          finish ~source_type:integer_type Object_value Integer_result state
      | Function_call_resolution.Float_literal _ ->
          finish ~source_type:float_type Object_value F64_result state
      | Function_call_resolution.String_literal _ ->
          finish ~source_type:string_type Address_value Integer_result state
      | Function_call_resolution.Parenthesized_expression grouped -> (
          match
            type_expression table members policies ~before_item_index ~context
              ~allow_aggregate_offset_base state grouped
          with
          | Error _ as error -> error
          | Ok (grouped_result, state) ->
              finish ~operand_result:grouped_result
                ~source_type:grouped_result.source_type
                ~array_rank:grouped_result.array_rank
                ?member_lookup:grouped_result.member_lookup
                ?aggregate_offset_path:grouped_result.aggregate_offset_path
                ?outer_occurrence:grouped_result.outer_occurrence
                ?top_level_outer_occurrence:
                  grouped_result.top_level_outer_occurrence
                ?outer_binding:grouped_result.outer_binding
                grouped_result.category grouped_result.result_class state)
      | Function_call_resolution.Prefix_expression prefix ->
          type_prefix table members policies ~before_item_index ~context
            ~intrinsic_conversion state id source prefix
      | Function_call_resolution.Postfix_expression postfix ->
          type_postfix table members policies ~before_item_index
            ~intrinsic_conversion state id source postfix
      | Function_call_resolution.Binary_expression binary ->
          type_binary table members policies ~before_item_index
            ~intrinsic_conversion state id source binary
      | Function_call_resolution.Index_expression index ->
          type_index table members policies ~before_item_index ~context
            ~intrinsic_conversion state id source index
      | Function_call_resolution.Member_access_expression member ->
          type_member table members policies ~before_item_index ~context
            ~intrinsic_conversion state id source member
      | Function_call_resolution.Postfix_cast_expression (operand, target) -> (
          match
            type_expression table members policies ~before_item_index
              ~context:Value_context state operand
          with
          | Error _ as error -> error
          | Ok (operand_result, state) -> (
              match known_type table (Type_reference.resolved_type target) with
              | Error _ as error -> error
              | Ok target_type ->
                  let category =
                    if Type.pointer_depth target_type > 0 then Address_value
                    else Object_value
                  in
                  finish ~operand_result ~source_type:(Some target_type)
                    category
                    (forwarded_class policies ~before_item_index target_type)
                    state))
      | Function_call_resolution.Aggregate_offset_base_expression base -> (
          if not allow_aggregate_offset_base then
            Error
              (invalid_input
                 ~origin:
                   (Function_call_resolution.argument_expression_origin source)
                 "aggregate offset base requires a member path")
          else
            let publication =
              Function_call_resolution.aggregate_offset_base_publication base
            in
            let occurrence =
              Function_call_resolution.aggregate_offset_base_occurrence base
            in
            if
              match
                Module_expression_binding.occurrence_resolution occurrence
              with
              | Module_expression_binding.Module_binding selected ->
                  selected != publication
                  || Module_expression_binding.publication_kind publication
                     <> Module_expression_binding.Aggregate
              | Module_expression_binding.Local_binding _
              | Module_expression_binding.Outer_candidate -> true
            then
              Error
                (invalid_input
                   ~origin:
                     (Function_call_resolution.argument_expression_origin source)
                   "aggregate offset base does not match its checked \
                    publication")
            else
              let symbol =
                Module_expression_binding.publication_canonical_symbol
                  publication
              in
              match Type.make_aggregate ~symbol ~pointer_depth:0 with
              | Error message ->
                  Error
                    (invalid_input
                       ~origin:
                         (Function_call_resolution.argument_expression_origin
                            source)
                       message)
              | Ok offset_current_type -> (
                  match known_type table offset_current_type with
                  | Error _ as error -> error
                  | Ok offset_current_type ->
                      let aggregate_offset_path =
                        {
                          offset_base = publication;
                          offset_current_type;
                          offset_segments = [];
                          offset_value = 0L;
                        }
                      in
                      finish ~source_type:integer_type ~aggregate_offset_path
                        Offset_value Integer_result state))
      | Function_call_resolution.Bound_identifier_expression identifier -> (
          match
            known_type table
              (Function_call_resolution.bound_identifier_type identifier)
          with
          | Error _ as error -> error
          | Ok source_type ->
              let category, result_class =
                match
                  Function_call_resolution.bound_identifier_shape identifier
                with
                | Function_call_resolution.Array_value ->
                    (Array_value, Integer_result)
                | Function_call_resolution.Function_pointer_value ->
                    (Callback_value, Integer_result)
                | Function_call_resolution.Direct_function_value ->
                    (Function_value, Integer_result)
                | Function_call_resolution.Object_value ->
                    ( (match context with
                      | Value_context -> Object_value
                      | Lvalue_context -> Lvalue),
                      forwarded_class policies ~before_item_index source_type )
              in
              let array_rank =
                Function_call_resolution.bound_identifier_array_rank identifier
              in
              finish ~source_type:(Some source_type) ~array_rank
                ?function_declaration:
                  (Function_call_resolution
                   .bound_identifier_function_declaration identifier)
                ?function_address_path:
                  (Function_call_resolution
                   .bound_identifier_function_address_path identifier)
                category result_class state)
      | Function_call_resolution.Top_level_bound_identifier_expression
          identifier -> (
          let occurrence =
            Function_call_resolution.top_level_bound_identifier_occurrence
              identifier
          in
          match state.top_level_identifier_batch with
          | None -> finish Unavailable Unresolved_actual_class state
          | Some identifiers -> (
              match
                Top_level_identifier_resolution.find_leaf identifiers occurrence
              with
              | None ->
                  Error
                    (invalid_top_level_input
                       ~origin:
                         (Top_level_outer_expression_binding.occurrence_origin
                            occurrence)
                       "top-level identifier result is absent from its checked \
                        batch")
              | Some leaf -> (
                  match
                    Top_level_identifier_resolution.leaf_resolution leaf
                  with
                  | Top_level_id.Outer_type_required binding ->
                      finish ~top_level_outer_occurrence:occurrence
                        ~outer_binding:binding Unavailable
                        Unresolved_actual_class state
                  | Top_level_id.Outer_value binding -> (
                      let entry = Outer_environment.binding_entry binding in
                      match Outer_environment.entry_global_metadata entry with
                      | None ->
                          let origin =
                            Top_level_outer_expression_binding.occurrence_origin
                              occurrence
                          in
                          let message =
                            "typed top-level outer global has no checked \
                             metadata"
                          in
                          Error (invalid_top_level_input ~origin message)
                      | Some metadata -> (
                          match
                            metadata |> Outer_environment.global_type_reference
                            |> Type_reference.resolved_type |> known_type table
                          with
                          | Error _ as error -> error
                          | Ok source_type ->
                              let array_rank =
                                Outer_environment.global_array_rank metadata
                              in
                              let category, result_class =
                                if array_rank > 0 then
                                  (Array_value, Integer_result)
                                else
                                  match
                                    Outer_environment.global_declarator_kind
                                      metadata
                                  with
                                  | Outer_environment.Function_pointer_global _
                                    -> (Callback_value, Integer_result)
                                  | Outer_environment.Object_global ->
                                      ( (match context with
                                        | Value_context -> Object_value
                                        | Lvalue_context -> Lvalue),
                                        forwarded_class policies
                                          ~before_item_index source_type )
                              in
                              finish ~source_type:(Some source_type) ~array_rank
                                ~top_level_outer_occurrence:occurrence
                                ~outer_binding:binding category result_class
                                state))
                  | Top_level_id.Module_value
                      (Top_level_id.Aggregate_offset_base publication) -> (
                      if not allow_aggregate_offset_base then
                        Error
                          (invalid_input
                             ~origin:
                               (Top_level_outer_expression_binding
                                .occurrence_origin occurrence)
                             "aggregate offset base requires a member path")
                      else
                        let symbol =
                          Module_expression_binding.publication_canonical_symbol
                            publication
                        in
                        match Type.make_aggregate ~symbol ~pointer_depth:0 with
                        | Error message ->
                            Error
                              (invalid_top_level_input
                                 ~origin:
                                   (Top_level_outer_expression_binding
                                    .occurrence_origin occurrence)
                                 message)
                        | Ok offset_current_type ->
                            let aggregate_offset_path =
                              {
                                offset_base = publication;
                                offset_current_type;
                                offset_segments = [];
                                offset_value = 0L;
                              }
                            in
                            finish ~source_type:integer_type
                              ~aggregate_offset_path Offset_value Integer_result
                              state)
                  | Top_level_identifier_resolution.Module_value
                      (Top_level_identifier_resolution.Global_value { value; _ })
                  | Top_level_identifier_resolution.Module_value
                      (Top_level_identifier_resolution.Direct_function_value
                         { value; _ }) -> (
                      let source_type =
                        Function_call_resolution.identifier_value_type value
                      in
                      match known_type table source_type with
                      | Error _ as error -> error
                      | Ok source_type ->
                          let category, result_class =
                            match
                              Function_call_resolution.identifier_value_shape
                                value
                            with
                            | Function_call_resolution.Array_value ->
                                (Array_value, Integer_result)
                            | Function_call_resolution.Function_pointer_value ->
                                (Callback_value, Integer_result)
                            | Function_call_resolution.Direct_function_value ->
                                (Function_value, Integer_result)
                            | Function_call_resolution.Object_value ->
                                ( (match context with
                                  | Value_context -> Object_value
                                  | Lvalue_context -> Lvalue),
                                  forwarded_class policies ~before_item_index
                                    source_type )
                          in
                          finish ~source_type:(Some source_type)
                            ~array_rank:
                              (Function_call_resolution
                               .identifier_value_array_rank value)
                            ?function_declaration:
                              (Function_call_resolution
                               .identifier_value_function_declaration value)
                            ?function_address_path:
                              (Function_call_resolution
                               .identifier_value_function_address_path value)
                            category result_class state))))
      | Function_call_resolution.Sizeof_expression _ ->
          finish ~source_type:integer_type Object_value Integer_result state
      | Function_call_resolution.Defined_expression _ ->
          finish ~source_type:integer_type Object_value Integer_result state
      | Function_call_resolution.Unresolved_expression kind -> (
          match kind with
          | Function_call_resolution.Current_position_expression ->
              finish ~source_type:rip_address_type Address_value Integer_result
                state
          | Function_call_resolution.Offset_expression ->
              finish ~source_type:integer_type Object_value Integer_result state
          | Function_call_resolution.Identifier_expression -> (
              match outer_binding_for_expression state source with
              | Error _ as error -> error
              | Ok None -> finish Unavailable Unresolved_actual_class state
              | Ok (Some (outer_occurrence, outer_binding)) -> (
                  let entry = Outer_environment.binding_entry outer_binding in
                  let unavailable () =
                    finish ~outer_occurrence ~outer_binding Unavailable
                      Unresolved_actual_class state
                  in
                  match Outer_environment.entry_global_metadata entry with
                  | None -> unavailable ()
                  | Some metadata -> (
                      match
                        metadata |> Outer_environment.global_type_reference
                        |> Type_reference.resolved_type |> known_type table
                      with
                      | Error _ as error -> error
                      | Ok source_type ->
                          let array_rank =
                            Outer_environment.global_array_rank metadata
                          in
                          let category, result_class =
                            if array_rank > 0 then (Array_value, Integer_result)
                            else
                              match
                                Outer_environment.global_declarator_kind
                                  metadata
                              with
                              | Outer_environment.Function_pointer_global _ ->
                                  (Callback_value, Integer_result)
                              | Outer_environment.Object_global ->
                                  ( (match context with
                                    | Value_context -> Object_value
                                    | Lvalue_context -> Lvalue),
                                    forwarded_class policies ~before_item_index
                                      source_type )
                          in
                          finish ~source_type:(Some source_type) ~array_rank
                            ~outer_occurrence ~outer_binding category
                            result_class state)))
          | Function_call_resolution.Postfix_cast_expression ->
              finish Unavailable Unresolved_actual_class state
          | Function_call_resolution.Call_expression
            when Option.is_some state.top_level_identifier_batch -> (
              match top_level_call_for_expression state source with
              | Error _ as error -> error
              | Ok None -> finish Unavailable Unresolved_actual_class state
              | Ok (Some call) ->
                  type_top_level_call table members policies ~before_item_index
                    ~intrinsic_conversion state id source call)
          | Function_call_resolution.Call_expression -> (
              match
                nested_call_resolution policies ~before_item_index
                  (Function_call_resolution.argument_expression_origin source)
              with
              | Error _ as error -> error
              | Ok None -> finish Unavailable Unresolved_actual_class state
              | Ok
                  (Some
                     (Function_call_resolution.Deferred_call
                        { call; occurrence; reason = Outer_callee } as
                      resolution)) -> (
                  match outer_binding_for_occurrence state occurrence with
                  | Error _ as error -> error
                  | Ok None ->
                      finish ~call_resolution:resolution Unavailable
                        Unresolved_actual_class state
                  | Ok (Some (outer_occurrence, outer_binding)) ->
                      type_outer_callback_call table members policies
                        ~before_item_index ~intrinsic_conversion state id source
                        resolution call outer_occurrence outer_binding)
              | Ok (Some (Function_call_resolution.Deferred_call _ as call)) ->
                  finish ~call_resolution:call Unavailable
                    Unresolved_actual_class state
              | Ok
                  (Some
                     (Function_call_resolution.Indirect_call indirect as call))
                -> (
                  let source_type =
                    indirect |> Function_call_resolution.indirect_callable
                    |> Function_call_resolution.callable_return_type
                    |> Type_reference.resolved_type
                  in
                  match known_type table source_type with
                  | Error _ as error -> error
                  | Ok source_type ->
                      let category =
                        if Type.pointer_depth source_type > 0 then Address_value
                        else Object_value
                      in
                      finish ~source_type:(Some source_type)
                        ~call_resolution:call category
                        (forwarded_class policies ~before_item_index source_type)
                        state)
              | Ok (Some (Function_call_resolution.Direct_call direct as call))
                -> (
                  let source_type =
                    direct |> Function_call_resolution.direct_active_header
                    |> Function_type_resolution.function_return_type
                    |> Type_reference.resolved_type
                  in
                  match known_type table source_type with
                  | Error _ as error -> error
                  | Ok source_type ->
                      let category =
                        if Type.pointer_depth source_type > 0 then Address_value
                        else Object_value
                      in
                      finish ~source_type:(Some source_type)
                        ~call_resolution:call category
                        (forwarded_class policies ~before_item_index source_type)
                        state))))

and type_prefix table members policies ~before_item_index ~context
    ~intrinsic_conversion state id source prefix =
  let operator = Function_call_resolution.prefix_operator prefix in
  let operand_context =
    match operator with
    | Function_call_resolution.Address_of
    | Function_call_resolution.Pre_increment
    | Function_call_resolution.Pre_decrement -> Lvalue_context
    | Function_call_resolution.Unary_plus
    | Function_call_resolution.Unary_minus
    | Function_call_resolution.Logical_not
    | Function_call_resolution.Bitwise_not
    | Function_call_resolution.Dereference -> Value_context
  in
  match
    type_expression table members policies ~before_item_index
      ~context:operand_context state
      (Function_call_resolution.prefix_operand prefix)
  with
  | Error _ as error -> error
  | Ok (operand, state) -> (
      let finish ?(source_type = None) ?(array_rank = 0) ?function_declaration
          ?function_address_path category result_class =
        Ok
          (make_result ~operand_result:operand ~array_rank ?function_declaration
             ?function_address_path ~intrinsic_conversion state ~id ~source
             ~source_type ~category ~result_class)
      in
      match operator with
      | Function_call_resolution.Unary_plus
      | Function_call_resolution.Unary_minus
      | Function_call_resolution.Logical_not ->
          finish ~source_type:operand.source_type Object_value
            operand.result_class
      | Function_call_resolution.Bitwise_not ->
          finish ~source_type:integer_type Object_value Integer_result
      | Function_call_resolution.Address_of -> (
          match
            ( operand.category,
              operand.source_type,
              result_is_direct_function operand )
          with
          | Offset_value, _, _ ->
              Error
                (invalid_input
                   ~origin:
                     (Function_call_resolution.prefix_operator_origin prefix)
                   "aggregate offset value has no addressable storage")
          | _, Some source_type, true -> (
              let name = result_direct_function_name operand in
              match operand.function_address_path with
              | Some Function_call_resolution.Jit_extern_slot
              | Some Function_call_resolution.Jit_immediate
              | Some Function_call_resolution.Aot_absolute ->
                  finish ~source_type:(Some source_type)
                    ?function_declaration:operand.function_declaration
                    ?function_address_path:operand.function_address_path
                    Address_value Integer_result
              | Some Function_call_resolution.Reject_aot_extern ->
                  Error
                    (invalid_input
                       ~origin:
                         (Function_call_resolution.prefix_operator_origin prefix)
                       (Printf.sprintf
                          "cannot take the address of unresolved AOT function \
                           %S outside assembly"
                          name))
              | Some Function_call_resolution.Reject_aot_import ->
                  Error
                    (invalid_input
                       ~origin:
                         (Function_call_resolution.prefix_operator_origin prefix)
                       (Printf.sprintf
                          "cannot take the address of imported AOT function %S \
                           outside assembly"
                          name))
              | Some Function_call_resolution.Reject_internal ->
                  Error
                    (invalid_input
                       ~origin:
                         (Function_call_resolution.prefix_operator_origin prefix)
                       (Printf.sprintf
                          "cannot use internal compiler function %S as a \
                           direct function address"
                          name))
              | None ->
                  Error
                    (invalid_input
                       ~origin:
                         (Function_call_resolution.prefix_operator_origin prefix)
                       "direct function address has no checked JIT or AOT path")
              )
          | _, None, _ -> finish Address_value Integer_result
          | _, Some source_type, false -> (
              match Type.pointer_to source_type with
              | Ok source_type ->
                  finish ~source_type:(Some source_type) Address_value
                    Integer_result
              | Error _ -> finish Address_value Integer_result))
      | Function_call_resolution.Pre_increment
      | Function_call_resolution.Pre_decrement -> (
          let operator_name =
            Function_call_resolution.prefix_operator_name operator
          in
          match
            validate_update_operand operand
              ~operator_origin:
                (Function_call_resolution.prefix_operator_origin prefix)
              ~operator_name
          with
          | Error _ as error -> error
          | Ok () ->
              finish ~source_type:operand.source_type Object_value
                operand.result_class)
      | Function_call_resolution.Dereference -> (
          let value_category =
            match context with
            | Value_context -> Object_value
            | Lvalue_context -> Lvalue
          in
          match (operand.source_type, operand.category) with
          | _, Offset_value ->
              Error
                (invalid_input
                   ~origin:
                     (Function_call_resolution.prefix_operator_origin prefix)
                   "aggregate offset value cannot be dereferenced")
          | None, _ -> finish Unavailable Unresolved_actual_class
          | Some source_type, Array_value ->
              let array_rank = max 0 (operand.array_rank - 1) in
              let category =
                if array_rank = 0 then value_category else Array_value
              in
              let result_class =
                if array_rank = 0 then
                  forwarded_class policies ~before_item_index source_type
                else Integer_result
              in
              finish ~source_type:(Some source_type) ~array_rank category
                result_class
          | Some source_type, (Callback_value | Function_value) ->
              finish ~source_type:(Some source_type) Function_value
                Integer_result
          | Some source_type, _ ->
              let source_type =
                match Type.dereference source_type with
                | Ok source_type -> source_type
                | Error _ -> source_type
              in
              finish ~source_type:(Some source_type) value_category
                (forwarded_class policies ~before_item_index source_type)))

and type_index table members policies ~before_item_index ~context
    ~intrinsic_conversion state id source index =
  match
    type_expression table members policies ~before_item_index
      ~context:Value_context state
      (Function_call_resolution.index_base index)
  with
  | Error _ as error -> error
  | Ok (base, state) -> (
      let value_category =
        match context with
        | Value_context -> Object_value
        | Lvalue_context -> Lvalue
      in
      let type_index_value state ~source_type ~array_rank ~member_lookup
          ~category ~result_class =
        match
          type_expression table members policies ~before_item_index
            ~context:Value_context ~intrinsic_conversion:Result_to_int state
            (Function_call_resolution.index_value index)
        with
        | Error _ as error -> error
        | Ok (_, state) ->
            Ok
              (make_result ~array_rank ?member_lookup ~intrinsic_conversion
                 state ~id ~source ~source_type ~category ~result_class)
      in
      match (base.source_type, base.category) with
      | None, _ ->
          type_index_value state ~source_type:None ~array_rank:0
            ~member_lookup:None ~category:Unavailable
            ~result_class:Unresolved_actual_class
      | Some source_type, Array_value ->
          if base.array_rank = 0 then
            Error
              (invalid_input
                 ~origin:(Function_call_resolution.index_opening_origin index)
                 "array index base has no remaining dimensions")
          else
            let array_rank = base.array_rank - 1 in
            let category =
              if array_rank = 0 then value_category else Array_value
            in
            let result_class =
              if array_rank = 0 then
                forwarded_class policies ~before_item_index source_type
              else Integer_result
            in
            type_index_value state ~source_type:(Some source_type) ~array_rank
              ~member_lookup:base.member_lookup ~category ~result_class
      | Some source_type, Callback_value when base.array_rank > 0 ->
          let array_rank = base.array_rank - 1 in
          type_index_value state ~source_type:(Some source_type) ~array_rank
            ~member_lookup:base.member_lookup ~category:Callback_value
            ~result_class:Integer_result
      | Some source_type, _ -> (
          match Type.dereference source_type with
          | Ok source_type ->
              type_index_value state ~source_type:(Some source_type)
                ~array_rank:0 ~member_lookup:base.member_lookup
                ~category:value_category
                ~result_class:
                  (forwarded_class policies ~before_item_index source_type)
          | Error _ ->
              Error
                (invalid_input
                   ~origin:(Function_call_resolution.index_opening_origin index)
                   "index base is neither an array nor a pointer")))

and type_member table members policies ~before_item_index ~context
    ~intrinsic_conversion state id source member =
  let access_kind = Function_call_resolution.member_access_kind member in
  let base_context =
    match access_kind with
    | Function_call_resolution.Direct_member -> Lvalue_context
    | Function_call_resolution.Pointer_member -> Value_context
  in
  match
    type_expression table members policies ~before_item_index
      ~context:base_context ~allow_aggregate_offset_base:true state
      (Function_call_resolution.member_base member)
  with
  | Error _ as error -> error
  | Ok (base, state) -> (
      let finish ?(source_type = None) ?(array_rank = 0) ?member_lookup
          ?aggregate_offset_path category result_class =
        Ok
          (make_result ~array_rank ~intrinsic_conversion ?member_lookup
             ?aggregate_offset_path state ~id ~source ~source_type ~category
             ~result_class)
      in
      let operator_origin =
        Function_call_resolution.member_operator_origin member
      in
      let member_origin = Function_call_resolution.member_origin member in
      let invalid_operator message =
        Error (invalid_input ~origin:operator_origin message)
      in
      let lookup aggregate_type =
        match Type.base aggregate_type with
        | Type.Primitive _ ->
            invalid_operator "member access base is not an aggregate"
        | Type.Aggregate aggregate_symbol ->
            resolve_member_lookup members ~before_item_index ~aggregate_symbol
              ~member_name:(Function_call_resolution.member_name member)
              ~member_origin
      in
      let resolve_aggregate source_type =
        match access_kind with
        | Function_call_resolution.Direct_member ->
            if Type.pointer_depth source_type = 0 then Ok source_type
            else
              invalid_operator
                "direct member access requires an aggregate object, not a \
                 pointer"
        | Function_call_resolution.Pointer_member -> (
            match Type.dereference source_type with
            | Error _ ->
                invalid_operator
                  "pointer member access requires a pointer to an aggregate"
            | Ok pointee when Type.pointer_depth pointee = 0 -> Ok pointee
            | Ok _ ->
                invalid_operator
                  "pointer member access leaves another pointer layer before \
                   the aggregate")
      in
      match base.aggregate_offset_path with
      | Some path -> (
          match access_kind with
          | Function_call_resolution.Pointer_member ->
              invalid_operator
                "aggregate offset paths require direct member access"
          | Function_call_resolution.Direct_member -> (
              let aggregate_type = path.offset_current_type in
              if Type.pointer_depth aggregate_type <> 0 then
                invalid_operator
                  "aggregate offset path leaves a pointer before this member"
              else
                match lookup aggregate_type with
                | Error _ as error -> error
                | Ok member_lookup ->
                    let indexed_member =
                      Aggregate_member_index.lookup_member member_lookup
                    in
                    let member_type =
                      Aggregate_member_index.member_type indexed_member
                    in
                    let member_offset =
                      let layout =
                        Aggregate_member_index.member_layout indexed_member
                      in
                      layout.offset
                    in
                    let offset_value =
                      Int64.add path.offset_value member_offset
                    in
                    let segment =
                      {
                        offset_lookup = member_lookup;
                        offset_cumulative = offset_value;
                      }
                    in
                    let aggregate_offset_path =
                      {
                        path with
                        offset_current_type = member_type;
                        offset_segments = path.offset_segments @ [ segment ];
                        offset_value;
                      }
                    in
                    finish ~source_type:integer_type ~member_lookup
                      ~aggregate_offset_path Offset_value Integer_result))
      | None -> (
          match base.source_type with
          | None -> finish Unavailable Unresolved_actual_class
          | Some source_type -> (
              match resolve_aggregate source_type with
              | Error _ as error -> error
              | Ok aggregate_type -> (
                  match lookup aggregate_type with
                  | Error _ as error -> error
                  | Ok lookup ->
                      let indexed_member =
                        Aggregate_member_index.lookup_member lookup
                      in
                      let member_type =
                        Aggregate_member_index.member_type indexed_member
                      in
                      let layout =
                        Aggregate_member_index.member_layout indexed_member
                      in
                      let array_rank = List.length layout.dimensions in
                      let category, result_class =
                        if
                          Aggregate_member_index.member_is_function_pointer
                            indexed_member
                        then (Callback_value, Integer_result)
                        else if array_rank > 0 then (Array_value, Integer_result)
                        else
                          ( (match context with
                            | Value_context -> Object_value
                            | Lvalue_context -> Lvalue),
                            forwarded_class policies ~before_item_index
                              member_type )
                      in
                      finish ~source_type:(Some member_type) ~array_rank
                        ~member_lookup:lookup category result_class))))

and type_postfix table members policies ~before_item_index ~intrinsic_conversion
    state id source postfix =
  match
    type_expression table members policies ~before_item_index
      ~context:Lvalue_context state
      (Function_call_resolution.postfix_operand postfix)
  with
  | Error _ as error -> error
  | Ok (operand, state) -> (
      let operator = Function_call_resolution.postfix_operator postfix in
      let operator_name =
        Function_call_resolution.postfix_operator_name operator
      in
      match
        validate_update_operand operand
          ~operator_origin:
            (Function_call_resolution.postfix_operator_origin postfix)
          ~operator_name
      with
      | Error _ as error -> error
      | Ok () ->
          Ok
            (make_result ~intrinsic_conversion state ~id ~source
               ~source_type:operand.source_type ~category:Object_value
               ~result_class:operand.result_class))

and type_assignment table members policies ~before_item_index
    ~intrinsic_conversion state id source binary assignment_kind =
  let operator_origin =
    Function_call_resolution.binary_operator_origin binary
  in
  let invalid_destination message =
    Error (invalid_input ~origin:operator_origin message)
  in
  match
    type_expression table members policies ~before_item_index
      ~context:Lvalue_context state
      (Function_call_resolution.binary_left binary)
  with
  | Error _ as error -> error
  | Ok (left, state) -> (
      let destination_type = left.source_type in
      let valid_storage_type = is_writable_storage_type destination_type in
      match (left.category, destination_type, valid_storage_type) with
      | Lvalue, Some destination_type, true -> (
          match
            type_expression table members policies ~before_item_index
              ~context:Value_context state
              (Function_call_resolution.binary_right binary)
          with
          | Error _ as error -> error
          | Ok (right, state) -> (
              let destination_class =
                forwarded_class policies ~before_item_index destination_type
              in
              let right_conversion, execution_class =
                match assignment_kind with
                | `Simple ->
                    let conversion =
                      match (destination_class, right.result_class) with
                      | F64_result, Integer_result -> Result_to_f64
                      | Integer_result, F64_result -> Result_to_int
                      | F64_result, (F64_result | Unresolved_actual_class)
                      | ( Integer_result,
                          (Integer_result | Unresolved_actual_class) )
                      | Unresolved_actual_class, _ -> No_intrinsic_conversion
                    in
                    (conversion, destination_class)
                | `Arithmetic ->
                    let conversion =
                      match (destination_class, right.result_class) with
                      | F64_result, Integer_result -> Result_to_f64
                      | _ -> No_intrinsic_conversion
                    in
                    let execution_class =
                      match (destination_class, right.result_class) with
                      | F64_result, _ | _, F64_result -> F64_result
                      | Integer_result, Integer_result -> Integer_result
                      | _ -> Unresolved_actual_class
                    in
                    (conversion, execution_class)
                | `Integer ->
                    let conversion =
                      match right.result_class with
                      | F64_result -> Result_to_int
                      | Integer_result | Unresolved_actual_class ->
                          No_intrinsic_conversion
                    in
                    (conversion, Integer_result)
              in
              match set_intrinsic_conversion state right right_conversion with
              | Error _ as error -> error
              | Ok (right, state) ->
                  Ok
                    (make_result ~binary_operands:(left, right) ~execution_class
                       ~intrinsic_conversion state ~id ~source
                       ~source_type:(Some destination_type)
                       ~category:Object_value ~result_class:destination_class)))
      | Unavailable, _, _ ->
          invalid_destination "assignment destination is unavailable"
      | Lvalue, None, _ ->
          invalid_destination "assignment destination has no checked type"
      | Lvalue, Some _, false ->
          invalid_destination
            "assignment destination is not a pointer or internal storage value"
      | ( ( Object_value
          | Address_value
          | Array_value
          | Callback_value
          | Function_value
          | Offset_value ),
          _,
          _ ) -> invalid_destination "assignment destination is not an lvalue")

and type_binary table members policies ~before_item_index ~intrinsic_conversion
    state id source binary =
  match Function_call_resolution.binary_operator binary with
  | Generated.Intermediate_codes.Ic_assign ->
      type_assignment table members policies ~before_item_index
        ~intrinsic_conversion state id source binary `Simple
  | Generated.Intermediate_codes.Ic_mul_equ
  | Generated.Intermediate_codes.Ic_div_equ
  | Generated.Intermediate_codes.Ic_mod_equ
  | Generated.Intermediate_codes.Ic_add_equ
  | Generated.Intermediate_codes.Ic_sub_equ ->
      type_assignment table members policies ~before_item_index
        ~intrinsic_conversion state id source binary `Arithmetic
  | Generated.Intermediate_codes.Ic_shl_equ
  | Generated.Intermediate_codes.Ic_shr_equ
  | Generated.Intermediate_codes.Ic_and_equ
  | Generated.Intermediate_codes.Ic_or_equ
  | Generated.Intermediate_codes.Ic_xor_equ ->
      type_assignment table members policies ~before_item_index
        ~intrinsic_conversion state id source binary `Integer
  | _ -> (
      match
        type_expression table members policies ~before_item_index
          ~context:Value_context state
          (Function_call_resolution.binary_left binary)
      with
      | Error _ as error -> error
      | Ok (left, state) -> (
          match
            type_expression table members policies ~before_item_index
              ~context:Value_context state
              (Function_call_resolution.binary_right binary)
          with
          | Error _ as error -> error
          | Ok (right, state) ->
              let result_class, source_type =
                match Function_call_resolution.binary_operator binary with
                | Generated.Intermediate_codes.Ic_power ->
                    (F64_result, float_type)
                | Generated.Intermediate_codes.Ic_equ_equ
                | Generated.Intermediate_codes.Ic_not_equ
                | Generated.Intermediate_codes.Ic_less
                | Generated.Intermediate_codes.Ic_greater_equ
                | Generated.Intermediate_codes.Ic_greater
                | Generated.Intermediate_codes.Ic_less_equ
                | Generated.Intermediate_codes.Ic_and_and
                | Generated.Intermediate_codes.Ic_or_or
                | Generated.Intermediate_codes.Ic_xor_xor ->
                    (Integer_result, integer_type)
                | Generated.Intermediate_codes.Ic_shl
                | Generated.Intermediate_codes.Ic_shr
                | Generated.Intermediate_codes.Ic_mul
                | Generated.Intermediate_codes.Ic_div
                | Generated.Intermediate_codes.Ic_mod
                | Generated.Intermediate_codes.Ic_and
                | Generated.Intermediate_codes.Ic_or
                | Generated.Intermediate_codes.Ic_xor
                | Generated.Intermediate_codes.Ic_add
                | Generated.Intermediate_codes.Ic_sub ->
                    let result_class =
                      match (left.result_class, right.result_class) with
                      | F64_result, _ | _, F64_result -> F64_result
                      | Integer_result, Integer_result -> Integer_result
                      | Integer_result, Unresolved_actual_class
                      | Unresolved_actual_class, Integer_result
                      | Unresolved_actual_class, Unresolved_actual_class ->
                          Unresolved_actual_class
                    in
                    ( result_class,
                      select_known_binary_type left right result_class )
                | _ -> (Unresolved_actual_class, None)
              in
              Ok
                (make_result ~binary_operands:(left, right)
                   ~intrinsic_conversion state ~id ~source ~source_type
                   ~category:Object_value ~result_class)))

and type_outer_callback_call table members policies ~before_item_index
    ~intrinsic_conversion state id source resolution call occurrence binding =
  let unavailable state =
    Ok
      (make_result ~intrinsic_conversion state ~id ~source ~source_type:None
         ~category:Unavailable ~outer_occurrence:occurrence
         ~outer_binding:binding ~call_resolution:resolution
         ~result_class:Unresolved_actual_class)
  in
  let entry = Outer_environment.binding_entry binding in
  if
    Outer_environment.entry_record_kind entry
    <> Outer_environment.Global_variable
  then unavailable state
  else
    match Outer_environment.entry_global_metadata entry with
    | None -> unavailable state
    | Some metadata -> (
        match Outer_environment.global_declarator_kind metadata with
        | Outer_environment.Object_global -> unavailable state
        | Outer_environment.Function_pointer_global function_pointer -> (
            let expected_rank = Outer_environment.global_array_rank metadata in
            let computed = Function_call_resolution.call_computed_callee call in
            let actual_rank =
              match computed with
              | None -> Some 0
              | Some expression ->
                  function_callback_array_index_depth
                    ~callee:
                      (Outer_expression_binding.occurrence_source occurrence)
                    expression
            in
            match actual_rank with
            | None -> unavailable state
            | Some actual_rank when actual_rank <> expected_rank ->
                Error
                  (invalid_input
                     ~origin:
                       (match computed with
                       | Some expression ->
                           Function_call_resolution.argument_expression_origin
                             expression
                       | None -> Function_call_resolution.call_origin call)
                     (Printf.sprintf
                        "indexed outer callback callee uses %d bracket%s, but \
                         `%s` has %d dimension%s"
                        actual_rank
                        (if actual_rank = 1 then "" else "s")
                        (Function_call_resolution.call_callee_name call)
                        expected_rank
                        (if expected_rank = 1 then "" else "s")))
            | Some _ -> (
                let callee_result =
                  match computed with
                  | None -> Ok (None, state)
                  | Some expression ->
                      type_expression table members policies ~before_item_index
                        ~context:Value_context state expression
                      |> Result.map (fun (result, state) ->
                          (Some result, state))
                in
                match callee_result with
                | Error _ as error -> error
                | Ok (callee_result, state) ->
                    let callable =
                      Function_call_resolution.make_callable
                        ~return_type:
                          (Outer_environment.global_type_reference metadata)
                        ~function_pointer
                    in
                    type_outer_callback_arguments table members policies
                      ~before_item_index ~intrinsic_conversion state id source
                      resolution call occurrence binding callee_result callable)
            ))

and type_outer_callback_arguments table members policies ~before_item_index
    ~intrinsic_conversion state id source resolution call occurrence binding
    callee_result callable =
  match Function_call_resolution.bind_indirect_arguments call callable with
  | Error error ->
      Error
        (invalid_input
           ?origin:(Function_call_resolution.error_origin error)
           (Function_call_resolution.error_message error))
  | Ok (fixed_arguments, variadic_arguments, variadic_count) -> (
      let origin = Function_call_resolution.call_origin call in
      match
        type_top_level_bound_arguments table members policies ~before_item_index
          ~origin state fixed_arguments variadic_arguments
      with
      | Error error ->
          Error
            (invalid_input ?origin:(error_origin error) (error_message error))
      | Ok (fixed_results, variadic_results, state) ->
          type_outer_callback_result table policies ~before_item_index
            ~intrinsic_conversion state id source resolution call occurrence
            binding callee_result callable fixed_results variadic_results
            variadic_count)

and type_outer_callback_result table policies ~before_item_index
    ~intrinsic_conversion state id source resolution call occurrence binding
    callee_result callable fixed_results variadic_results variadic_count =
  let unresolved_source_type =
    callable |> Function_call_resolution.callable_return_type
    |> Type_reference.resolved_type
  in
  match known_type table unresolved_source_type with
  | Error _ as error -> error
  | Ok source_type ->
      let category =
        if Type.pointer_depth source_type > 0 then Address_value
        else Object_value
      in
      let callback_call =
        {
          outer_callback_source = call;
          outer_callback_occurrence = occurrence;
          outer_callback_binding = binding;
          outer_callback_callee_result = callee_result;
          outer_callback_callable = callable;
          outer_callback_fixed_results = fixed_results;
          outer_callback_variadic_results = variadic_results;
          outer_callback_variadic_count = variadic_count;
          outer_callback_result_id = id;
        }
      in
      let state =
        {
          state with
          outer_callback_calls_rev =
            callback_call :: state.outer_callback_calls_rev;
        }
      in
      Ok
        (make_result ~intrinsic_conversion state ~id ~source
           ~source_type:(Some source_type) ~category
           ~outer_occurrence:occurrence ~outer_binding:binding
           ~call_resolution:resolution
           ~result_class:
             (forwarded_class policies ~before_item_index source_type))

and type_top_level_call table members policies ~before_item_index
    ~intrinsic_conversion state id source call =
  let callee = Top_level_expression_tree.call_callee call in
  match state.top_level_identifier_batch with
  | None ->
      Error
        (invalid_top_level_input
           ~origin:(Top_level_outer_expression_binding.occurrence_origin callee)
           "top-level call has no identifier classification batch")
  | Some identifiers -> (
      match Top_level_identifier_resolution.find_leaf identifiers callee with
      | None ->
          Error
            (invalid_top_level_input
               ~origin:
                 (Top_level_outer_expression_binding.occurrence_origin callee)
               "top-level call callee is absent from its identifier batch")
      | Some leaf -> (
          match Top_level_identifier_resolution.leaf_resolution leaf with
          | Top_level_identifier_resolution.Module_value
              (Top_level_identifier_resolution.Direct_function_value
                 { declaration; _ }) ->
              type_top_level_direct_call table members policies
                ~before_item_index ~intrinsic_conversion state id source call
                declaration
          | Top_level_identifier_resolution.Module_value
              (Top_level_identifier_resolution.Global_value { global; value })
            when Function_call_resolution.identifier_value_shape value
                 = Function_call_resolution.Function_pointer_value
                 && Function_call_resolution.call_callee_form
                      (Top_level_expression_tree.call_source call)
                    = Function_call_resolution.Identifier_callee ->
              type_top_level_global_callback_call table members policies
                ~before_item_index ~intrinsic_conversion state id source call
                global value
          | Top_level_identifier_resolution.Module_value
              (Top_level_identifier_resolution.Global_value { global; value })
            when Function_call_resolution.identifier_value_shape value
                 = Function_call_resolution.Array_value
                 && Function_call_resolution.call_callee_form
                      (Top_level_expression_tree.call_source call)
                    = Function_call_resolution.Member_callee -> (
              match
                ( Global_type_resolution.global_declarator_kind global,
                  callback_array_index_depth ~callee
                    (Top_level_expression_tree.call_callee_expression call) )
              with
              | Global_type_resolution.Function_pointer _, Some _ ->
                  type_top_level_indexed_global_callback_call table members
                    policies ~before_item_index ~intrinsic_conversion state id
                    source call global value
              | Global_type_resolution.Object, _ ->
                  type_top_level_member_callback_call table members policies
                    ~before_item_index ~intrinsic_conversion state id source
                    call global value
              | Global_type_resolution.Function_pointer _, None ->
                  Ok
                    (make_result ~intrinsic_conversion state ~id ~source
                       ~source_type:None ~category:Unavailable
                       ~result_class:Unresolved_actual_class))
          | Top_level_identifier_resolution.Module_value
              (Top_level_identifier_resolution.Global_value { global; value })
            when Function_call_resolution.call_callee_form
                   (Top_level_expression_tree.call_source call)
                 = Function_call_resolution.Member_callee ->
              type_top_level_member_callback_call table members policies
                ~before_item_index ~intrinsic_conversion state id source call
                global value
          | Top_level_identifier_resolution.Module_value
              ( Top_level_identifier_resolution.Global_value _
              | Top_level_identifier_resolution.Aggregate_offset_base _ ) ->
              Ok
                (make_result ~intrinsic_conversion state ~id ~source
                   ~source_type:None ~category:Unavailable
                   ~result_class:Unresolved_actual_class)
          | Top_level_identifier_resolution.Outer_type_required binding ->
              Ok
                (make_result ~intrinsic_conversion state ~id ~source
                   ~source_type:None ~category:Unavailable
                   ~top_level_outer_occurrence:callee ~outer_binding:binding
                   ~result_class:Unresolved_actual_class)
          | Top_level_identifier_resolution.Outer_value binding -> (
              let entry = Outer_environment.binding_entry binding in
              match Outer_environment.entry_global_metadata entry with
              | None ->
                  Error
                    (invalid_top_level_input
                       ~origin:
                         (Top_level_outer_expression_binding.occurrence_origin
                            callee)
                       "typed top-level outer callback has no checked metadata")
              | Some metadata -> (
                  match
                    ( Outer_environment.global_array_rank metadata,
                      Outer_environment.global_declarator_kind metadata )
                  with
                  | _, Outer_environment.Function_pointer_global _ ->
                      type_top_level_outer_callback_call table members policies
                        ~before_item_index ~intrinsic_conversion state id source
                        call callee binding
                  | _, Outer_environment.Object_global ->
                      Ok
                        (make_result ~intrinsic_conversion state ~id ~source
                           ~source_type:None ~category:Unavailable
                           ~top_level_outer_occurrence:callee
                           ~outer_binding:binding
                           ~result_class:Unresolved_actual_class)))))

and type_top_level_direct_call table members policies ~before_item_index
    ~intrinsic_conversion state id source call declaration =
  let source_call = Top_level_expression_tree.call_source call in
  let origin = Function_call_resolution.call_origin source_call in
  let invalid message = Error (invalid_top_level_input ~origin message) in
  if
    Function_call_resolution.call_callee_form source_call
    <> Function_call_resolution.Identifier_callee
  then invalid "top-level direct call does not use an identifier callee"
  else if Option.is_some (Function_call_resolution.call_callable source_call)
  then invalid "top-level direct call unexpectedly carries a callback header"
  else
    let site = Function_resolution.resolved_declaration_site declaration in
    let header = Function_resolution.declaration_site_function site in
    match Function_call_resolution.bind_direct_arguments source_call header with
    | Error error ->
        Error
          (invalid_top_level_input
             ?origin:(Function_call_resolution.error_origin error)
             (Function_call_resolution.error_message error))
    | Ok (fixed_arguments, variadic_arguments, variadic_count) -> (
        match
          type_top_level_bound_arguments table members policies
            ~before_item_index ~origin state fixed_arguments variadic_arguments
        with
        | Error _ as error -> error
        | Ok (fixed_results, variadic_results, state) -> (
            let source_type =
              header |> Function_type_resolution.function_return_type
              |> Type_reference.resolved_type
            in
            match known_type table source_type with
            | Error _ as error -> error
            | Ok source_type ->
                let category =
                  if Type.pointer_depth source_type > 0 then Address_value
                  else Object_value
                in
                let direct_call =
                  {
                    top_level_direct_source = call;
                    top_level_direct_declaration = declaration;
                    top_level_direct_header = header;
                    top_level_direct_target_symbol =
                      Function_resolution.resolved_declaration_identity_symbol
                        declaration;
                    top_level_direct_fixed_results = fixed_results;
                    top_level_direct_variadic_results = variadic_results;
                    top_level_direct_variadic_count = variadic_count;
                    top_level_direct_result_id = id;
                  }
                in
                let state =
                  {
                    state with
                    top_level_direct_calls_rev =
                      direct_call :: state.top_level_direct_calls_rev;
                  }
                in
                Ok
                  (make_result ~intrinsic_conversion state ~id ~source
                     ~source_type:(Some source_type) ~category
                     ~result_class:
                       (forwarded_class policies ~before_item_index source_type))
            ))

and type_top_level_global_callback_call table members policies
    ~before_item_index ~intrinsic_conversion state id source call global value =
  let source_call = Top_level_expression_tree.call_source call in
  let origin = Function_call_resolution.call_origin source_call in
  let invalid message = Error (invalid_top_level_input ~origin message) in
  if Global_type_resolution.global_array_dimensions global <> [] then
    invalid "top-level scalar callback unexpectedly has array dimensions"
  else
    match Global_type_resolution.global_declarator_kind global with
    | Global_type_resolution.Object ->
        invalid "top-level callback global has no function-pointer signature"
    | Global_type_resolution.Function_pointer function_pointer -> (
        let callable =
          Function_call_resolution.make_callable
            ~return_type:(Global_type_resolution.global_type_reference global)
            ~function_pointer
        in
        match
          Function_call_resolution.bind_indirect_arguments source_call callable
        with
        | Error error ->
            Error
              (invalid_top_level_input
                 ?origin:(Function_call_resolution.error_origin error)
                 (Function_call_resolution.error_message error))
        | Ok (fixed_arguments, variadic_arguments, variadic_count) -> (
            match
              type_top_level_bound_arguments table members policies
                ~before_item_index ~origin state fixed_arguments
                variadic_arguments
            with
            | Error _ as error -> error
            | Ok (fixed_results, variadic_results, state) -> (
                let source_type =
                  callable |> Function_call_resolution.callable_return_type
                  |> Type_reference.resolved_type
                in
                match known_type table source_type with
                | Error _ as error -> error
                | Ok source_type ->
                    let category =
                      if Type.pointer_depth source_type > 0 then Address_value
                      else Object_value
                    in
                    let callback_call =
                      {
                        top_level_global_callback_source = call;
                        top_level_global_callback_global = global;
                        top_level_global_callback_value = value;
                        top_level_global_callback_callable = callable;
                        top_level_global_callback_fixed_results = fixed_results;
                        top_level_global_callback_variadic_results =
                          variadic_results;
                        top_level_global_callback_variadic_count =
                          variadic_count;
                        top_level_global_callback_result_id = id;
                      }
                    in
                    let state =
                      {
                        state with
                        top_level_global_callback_calls_rev =
                          callback_call
                          :: state.top_level_global_callback_calls_rev;
                      }
                    in
                    Ok
                      (make_result ~intrinsic_conversion state ~id ~source
                         ~source_type:(Some source_type) ~category
                         ~result_class:
                           (forwarded_class policies ~before_item_index
                              source_type)))))

and type_top_level_outer_callback_call table members policies =
 fun ~before_item_index ~intrinsic_conversion state id source call occurrence
     binding ->
  let source_call = Top_level_expression_tree.call_source call in
  let origin = Function_call_resolution.call_origin source_call in
  let invalid message = Error (invalid_top_level_input ~origin message) in
  if
    match
      Top_level_outer_expression_binding.occurrence_resolution occurrence
    with
    | Top_level_outer_expression_binding.Outer_binding selected ->
        selected != binding
    | Top_level_outer_expression_binding.Module_binding _ -> true
  then invalid "top-level outer callback does not retain its selected binding"
  else
    let entry = Outer_environment.binding_entry binding in
    if
      Outer_environment.entry_record_kind entry
      <> Outer_environment.Global_variable
    then invalid "top-level outer callback binding is not a global record"
    else
      match Outer_environment.entry_global_metadata entry with
      | None -> invalid "top-level outer callback has no checked metadata"
      | Some metadata ->
          type_top_level_outer_callback_metadata table members policies
            ~before_item_index ~intrinsic_conversion state id source call
            occurrence binding source_call origin metadata

and type_top_level_outer_callback_metadata table members policies =
 fun ~before_item_index ~intrinsic_conversion state id source call occurrence
     binding source_call origin metadata ->
  let invalid message = Error (invalid_top_level_input ~origin message) in
  match Outer_environment.global_declarator_kind metadata with
  | Outer_environment.Object_global ->
      invalid "top-level outer global has no function-pointer signature"
  | Outer_environment.Function_pointer_global function_pointer ->
      let callable =
        Function_call_resolution.make_callable
          ~return_type:(Outer_environment.global_type_reference metadata)
          ~function_pointer
      in
      let expected_rank = Outer_environment.global_array_rank metadata in
      if expected_rank = 0 then
        if
          Function_call_resolution.call_callee_form source_call
          <> Function_call_resolution.Identifier_callee
        then
          invalid "top-level outer callback does not use an identifier callee"
        else
          type_top_level_outer_callback_arguments table members policies
            ~before_item_index ~intrinsic_conversion state id source call
            occurrence binding source_call origin callable ~callee_result:None
      else
        type_top_level_indexed_outer_callback_callee table members policies
          ~before_item_index ~intrinsic_conversion state id source call
          occurrence binding source_call origin callable expected_rank

and type_top_level_indexed_outer_callback_callee table members policies =
 fun ~before_item_index ~intrinsic_conversion state id source call occurrence
     binding source_call origin callable expected_rank ->
  let invalid message = Error (invalid_top_level_input ~origin message) in
  let callee_expression =
    Top_level_expression_tree.call_callee_expression call
  in
  let callee = Top_level_expression_tree.call_callee call in
  match callback_array_index_depth ~callee callee_expression with
  | None ->
      invalid
        "indexed top-level outer callback callee is not an exact bracket chain"
  | Some actual_rank when actual_rank <> expected_rank ->
      invalid
        (Printf.sprintf
           "indexed top-level outer callback callee uses %d bracket%s, but \
            `%s` has %d dimension%s"
           actual_rank
           (if actual_rank = 1 then "" else "s")
           (Function_call_resolution.call_callee_name source_call)
           expected_rank
           (if expected_rank = 1 then "" else "s"))
  | Some _ -> (
      match Function_call_resolution.call_computed_callee source_call with
      | Some computed when computed == callee_expression -> (
          match
            type_expression table members policies ~before_item_index
              ~context:Value_context state callee_expression
          with
          | Error _ as error -> error
          | Ok (callee_result, state) ->
              if callee_result.array_rank <> 0 then
                invalid
                  "indexed top-level outer callback callee retains an array \
                   dimension"
              else
                type_top_level_outer_callback_arguments table members policies
                  ~before_item_index ~intrinsic_conversion state id source call
                  occurrence binding source_call origin callable
                  ~callee_result:(Some callee_result))
      | Some _ ->
          invalid
            "indexed top-level outer callback call carries another computed \
             callee"
      | None ->
          invalid "indexed top-level outer callback call has no computed callee"
      )

and type_top_level_outer_callback_arguments table members policies =
 fun ~before_item_index ~intrinsic_conversion state id source call occurrence
     binding source_call origin callable ~callee_result ->
  match
    Function_call_resolution.bind_indirect_arguments source_call callable
  with
  | Error error ->
      Error
        (invalid_top_level_input
           ?origin:(Function_call_resolution.error_origin error)
           (Function_call_resolution.error_message error))
  | Ok (fixed_arguments, variadic_arguments, variadic_count) ->
      type_top_level_outer_callback_argument_results table members policies
        ~before_item_index ~intrinsic_conversion state id source call occurrence
        binding origin callable ~callee_result fixed_arguments
        variadic_arguments variadic_count

and type_top_level_outer_callback_argument_results table members policies =
 fun ~before_item_index ~intrinsic_conversion state id source call occurrence
     binding origin callable ~callee_result fixed_arguments variadic_arguments
     variadic_count ->
  match
    type_top_level_bound_arguments table members policies ~before_item_index
      ~origin state fixed_arguments variadic_arguments
  with
  | Error _ as error -> error
  | Ok (fixed_results, variadic_results, state) ->
      type_top_level_outer_callback_result table policies ~before_item_index
        ~intrinsic_conversion state id source call occurrence binding callable
        ~callee_result fixed_results variadic_results variadic_count

and type_top_level_outer_callback_result table policies =
 fun ~before_item_index ~intrinsic_conversion state id source call occurrence
     binding callable ~callee_result fixed_results variadic_results
     variadic_count ->
  let unresolved_source_type =
    callable |> Function_call_resolution.callable_return_type
    |> Type_reference.resolved_type
  in
  match known_type table unresolved_source_type with
  | Error _ as error -> error
  | Ok source_type ->
      let category =
        if Type.pointer_depth source_type > 0 then Address_value
        else Object_value
      in
      let callback_call =
        {
          top_level_outer_callback_source = call;
          top_level_outer_callback_occurrence = occurrence;
          top_level_outer_callback_binding = binding;
          top_level_outer_callback_callee_result = callee_result;
          top_level_outer_callback_callable = callable;
          top_level_outer_callback_fixed_results = fixed_results;
          top_level_outer_callback_variadic_results = variadic_results;
          top_level_outer_callback_variadic_count = variadic_count;
          top_level_outer_callback_result_id = id;
        }
      in
      let state =
        {
          state with
          top_level_outer_callback_calls_rev =
            callback_call :: state.top_level_outer_callback_calls_rev;
        }
      in
      let result_class =
        forwarded_class policies ~before_item_index source_type
      in
      Ok
        (make_result ~intrinsic_conversion state ~id ~source
           ~source_type:(Some source_type) ~category
           ~top_level_outer_occurrence:occurrence ~outer_binding:binding
           ~result_class)

and type_top_level_indexed_global_callback_call table members policies
    ~before_item_index ~intrinsic_conversion state id source call global value =
  let source_call = Top_level_expression_tree.call_source call in
  let origin = Function_call_resolution.call_origin source_call in
  let invalid message = Error (invalid_top_level_input ~origin message) in
  let callee_expression =
    Top_level_expression_tree.call_callee_expression call
  in
  let callee = Top_level_expression_tree.call_callee call in
  let expected_rank =
    Global_type_resolution.global_array_dimensions global |> List.length
  in
  match callback_array_index_depth ~callee callee_expression with
  | None -> invalid "indexed callback callee is not an exact bracket chain"
  | Some actual_rank when actual_rank <> expected_rank ->
      invalid
        (Printf.sprintf
           "indexed callback callee uses %d bracket%s, but global `%s` has %d \
            dimension%s"
           actual_rank
           (if actual_rank = 1 then "" else "s")
           (Function_call_resolution.call_callee_name source_call)
           expected_rank
           (if expected_rank = 1 then "" else "s"))
  | Some _ -> (
      match Function_call_resolution.call_computed_callee source_call with
      | Some computed when computed == callee_expression -> (
          match Global_type_resolution.global_declarator_kind global with
          | Global_type_resolution.Object ->
              invalid
                "indexed callback global has no function-pointer signature"
          | Global_type_resolution.Function_pointer function_pointer -> (
              match
                type_expression table members policies ~before_item_index
                  ~context:Value_context state callee_expression
              with
              | Error _ as error -> error
              | Ok (callee_result, state) -> (
                  if callee_result.array_rank <> 0 then
                    invalid "indexed callback callee retains an array dimension"
                  else
                    let callable =
                      Function_call_resolution.make_callable
                        ~return_type:
                          (Global_type_resolution.global_type_reference global)
                        ~function_pointer
                    in
                    match
                      Function_call_resolution.bind_indirect_arguments
                        source_call callable
                    with
                    | Error error ->
                        Error
                          (invalid_top_level_input
                             ?origin:
                               (Function_call_resolution.error_origin error)
                             (Function_call_resolution.error_message error))
                    | Ok (fixed_arguments, variadic_arguments, variadic_count)
                      -> (
                        match
                          type_top_level_bound_arguments table members policies
                            ~before_item_index ~origin state fixed_arguments
                            variadic_arguments
                        with
                        | Error _ as error -> error
                        | Ok (fixed_results, variadic_results, state) -> (
                            let source_type =
                              callable
                              |> Function_call_resolution.callable_return_type
                              |> Type_reference.resolved_type
                            in
                            match known_type table source_type with
                            | Error _ as error -> error
                            | Ok source_type ->
                                let category =
                                  if Type.pointer_depth source_type > 0 then
                                    Address_value
                                  else Object_value
                                in
                                let indexed_call =
                                  {
                                    top_level_indexed_global_callback_source =
                                      call;
                                    top_level_indexed_global_callback_global =
                                      global;
                                    top_level_indexed_global_callback_value =
                                      value;
                                    top_level_indexed_global_callback_callee_result =
                                      callee_result;
                                    top_level_indexed_global_callback_callable =
                                      callable;
                                    top_level_indexed_global_callback_fixed_results =
                                      fixed_results;
                                    top_level_indexed_global_callback_variadic_results =
                                      variadic_results;
                                    top_level_indexed_global_callback_variadic_count =
                                      variadic_count;
                                    top_level_indexed_global_callback_result_id =
                                      id;
                                  }
                                in
                                let state =
                                  {
                                    state with
                                    top_level_indexed_global_callback_calls_rev =
                                      indexed_call
                                      :: state
                                           .top_level_indexed_global_callback_calls_rev;
                                  }
                                in
                                Ok
                                  (make_result ~intrinsic_conversion state ~id
                                     ~source ~source_type:(Some source_type)
                                     ~category
                                     ~result_class:
                                       (forwarded_class policies
                                          ~before_item_index source_type)))))))
      | Some _ ->
          invalid "indexed callback call carries another computed callee"
      | None -> invalid "indexed callback call has no computed callee")

and type_top_level_member_callback_call table members policies
    ~before_item_index ~intrinsic_conversion state id source call global value =
  let source_call = Top_level_expression_tree.call_source call in
  let origin = Function_call_resolution.call_origin source_call in
  let invalid ?origin:source_origin message =
    Error
      (invalid_top_level_input
         ~origin:(Option.value source_origin ~default:origin)
         message)
  in
  let callee_expression =
    Top_level_expression_tree.call_callee_expression call
  in
  let resolve_callable state =
    match
      Function_call_resolution.member_callable_base_expression callee_expression
    with
    | Error error ->
        Error
          ( Function_call_resolution.error_origin error,
            Function_call_resolution.error_message error )
    | Ok base_expression -> (
        match
          type_expression table members policies ~before_item_index
            ~context:Value_context state base_expression
        with
        | Error error -> Error (error_origin error, error_message error)
        | Ok (base_result, _) -> (
            match base_result.member_lookup with
            | None ->
                Error
                  ( Some origin,
                    "top-level member callback base has no exact member lookup"
                  )
            | Some lookup -> (
                match
                  Function_call_resolution.validate_member_callable
                    callee_expression lookup
                with
                | Error error ->
                    Error
                      ( Function_call_resolution.error_origin error,
                        Function_call_resolution.error_message error )
                | Ok callable -> Ok (lookup, callable))))
  in
  match Function_call_resolution.call_computed_callee source_call with
  | Some computed when computed == callee_expression -> (
      match resolve_callable state with
      | Error (source_origin, message) -> invalid ?origin:source_origin message
      | Ok (lookup, callable) -> (
          match
            type_expression table members policies ~before_item_index
              ~context:Value_context state callee_expression
          with
          | Error error ->
              invalid ?origin:(error_origin error) (error_message error)
          | Ok (callee_result, state) -> (
              match
                Function_call_resolution.bind_indirect_arguments source_call
                  callable
              with
              | Error error ->
                  invalid
                    ?origin:(Function_call_resolution.error_origin error)
                    (Function_call_resolution.error_message error)
              | Ok (fixed_arguments, variadic_arguments, variadic_count) -> (
                  match
                    type_top_level_bound_arguments table members policies
                      ~before_item_index ~origin state fixed_arguments
                      variadic_arguments
                  with
                  | Error _ as error -> error
                  | Ok (fixed_results, variadic_results, state) -> (
                      let source_type =
                        callable
                        |> Function_call_resolution.callable_return_type
                        |> Type_reference.resolved_type
                      in
                      match known_type table source_type with
                      | Error _ as error -> error
                      | Ok source_type ->
                          let category =
                            if Type.pointer_depth source_type > 0 then
                              Address_value
                            else Object_value
                          in
                          let member_call =
                            {
                              top_level_member_callback_source = call;
                              top_level_member_callback_global = global;
                              top_level_member_callback_value = value;
                              top_level_member_callback_callee_result =
                                callee_result;
                              top_level_member_callback_lookup = lookup;
                              top_level_member_callback_callable = callable;
                              top_level_member_callback_fixed_results =
                                fixed_results;
                              top_level_member_callback_variadic_results =
                                variadic_results;
                              top_level_member_callback_variadic_count =
                                variadic_count;
                              top_level_member_callback_result_id = id;
                            }
                          in
                          let state =
                            {
                              state with
                              top_level_member_callback_calls_rev =
                                member_call
                                :: state.top_level_member_callback_calls_rev;
                            }
                          in
                          Ok
                            (make_result ~intrinsic_conversion state ~id ~source
                               ~source_type:(Some source_type) ~category
                               ~result_class:
                                 (forwarded_class policies ~before_item_index
                                    source_type)))))))
  | Some _ -> invalid "member callback call carries another computed callee"
  | None -> invalid "member callback call has no computed callee"

and type_top_level_bound_arguments table members policies ~before_item_index
    ~origin state fixed_arguments variadic_arguments =
  let invalid message = Error (invalid_top_level_input ~origin message) in
  let rec type_fixed previous state rev = function
    | [] -> Ok (List.rev rev, state)
    | fixed :: rest -> (
        let parameter = Function_call_resolution.fixed_parameter fixed in
        let target_class =
          Function_call_conversion_policy.parameter_target_class policies
            ~before_item_index parameter
          |> result_class_of_target
        in
        match Function_call_resolution.fixed_value fixed with
        | Function_call_resolution.Declared_default default ->
            let result =
              {
                top_level_fixed_source = fixed;
                top_level_fixed_target_class = target_class;
                top_level_fixed_path =
                  Declared_default_result
                    (declared_default_result policies ~before_item_index fixed
                       default);
                top_level_fixed_lastclass_substitution =
                  lastclass_substitution policies ~before_item_index previous
                    default;
              }
            in
            type_fixed previous state (result :: rev) rest
        | Function_call_resolution.Provided_argument argument -> (
            match Function_call_resolution.argument_expression argument with
            | None -> invalid "provided fixed argument has no expression"
            | Some expression -> (
                match
                  type_expression table members policies ~before_item_index
                    ~context:Value_context state expression
                with
                | Error _ as error -> error
                | Ok (value, state) ->
                    let result =
                      {
                        top_level_fixed_source = fixed;
                        top_level_fixed_target_class = target_class;
                        top_level_fixed_path = Provided_result value;
                        top_level_fixed_lastclass_substitution = None;
                      }
                    in
                    type_fixed (Some value) state (result :: rev) rest)))
  in
  let rec type_variadic state rev = function
    | [] -> Ok (List.rev rev, state)
    | argument :: rest -> (
        match Function_call_resolution.argument_expression argument with
        | None -> invalid "provided variadic argument has no expression"
        | Some expression -> (
            match
              type_expression table members policies ~before_item_index
                ~context:Value_context state expression
            with
            | Error _ as error -> error
            | Ok (value, state) -> type_variadic state (value :: rev) rest))
  in
  match type_fixed None state [] fixed_arguments with
  | Error _ as error -> error
  | Ok (fixed_results, state) -> (
      match type_variadic state [] variadic_arguments with
      | Error _ as error -> error
      | Ok (variadic_results, state) ->
          Ok (fixed_results, variadic_results, state))

let map_state apply state values =
  let rec loop state rev = function
    | [] -> Ok (List.rev rev, state)
    | value :: rest -> (
        match apply state value with
        | Error _ as error -> error
        | Ok (result, state) -> loop state (result :: rev) rest)
  in
  loop state [] values

let type_fixed table members policies ~before_item_index previous state source =
  match
    ( Function_call_conversion_policy.fixed_path source,
      source |> Function_call_conversion_policy.fixed_source
      |> Function_call_resolution.fixed_value )
  with
  | ( Function_call_conversion_policy.Declared_default,
      Function_call_resolution.Declared_default default ) ->
      Ok
        ( {
            source;
            path =
              Declared_default_result
                (declared_default_result policies ~before_item_index
                   (Function_call_conversion_policy.fixed_source source)
                   default);
            lastclass_substitution =
              lastclass_substitution policies ~before_item_index previous
                default;
          },
          previous,
          state )
  | ( Function_call_conversion_policy.Provided_expression _,
      Function_call_resolution.Provided_argument argument ) -> (
      match Function_call_resolution.argument_expression argument with
      | None ->
          Error (invalid_input "provided fixed argument has no expression")
      | Some expression -> (
          match
            type_expression table members policies ~before_item_index
              ~context:Value_context state expression
          with
          | Error _ as error -> error
          | Ok (result, state) ->
              Ok
                ( {
                    source;
                    path = Provided_result result;
                    lastclass_substitution = None;
                  },
                  Some result,
                  state )))
  | ( Function_call_conversion_policy.Declared_default,
      Function_call_resolution.Provided_argument _ )
  | ( Function_call_conversion_policy.Provided_expression _,
      Function_call_resolution.Declared_default _ ) ->
      Error (invalid_input "fixed call policy has an inconsistent source path")

let type_fixed_results table members policies ~before_item_index state values =
  let rec loop previous state rev = function
    | [] -> Ok (List.rev rev, state)
    | value :: rest -> (
        match
          type_fixed table members policies ~before_item_index previous state
            value
        with
        | Error _ as error -> error
        | Ok (result, previous, state) ->
            loop previous state (result :: rev) rest)
  in
  loop None state [] values

let type_variadic table members policies ~before_item_index state argument =
  match Function_call_resolution.argument_expression argument with
  | None -> Error (invalid_input "provided variadic argument has no expression")
  | Some expression ->
      type_expression table members policies ~before_item_index
        ~context:Value_context state expression

let type_call table members policies ~before_item_index state = function
  | Function_call_conversion_policy.Direct_call_policy source -> (
      match
        source |> Function_call_conversion_policy.direct_fixed_policies
        |> type_fixed_results table members policies ~before_item_index state
      with
      | Error _ as error -> error
      | Ok (fixed_results, state) -> (
          match
            source |> Function_call_conversion_policy.direct_variadic_arguments
            |> map_state
                 (type_variadic table members policies ~before_item_index)
                 state
          with
          | Error _ as error -> error
          | Ok (variadic_results, state) ->
              Ok
                ( Direct_call_result { source; fixed_results; variadic_results },
                  state )))
  | Function_call_conversion_policy.Indirect_call_policy source -> (
      let resolution = Function_call_conversion_policy.indirect_source source in
      let source_call = Function_call_resolution.indirect_source resolution in
      let callee_result =
        match
          ( Function_call_resolution.indirect_member_lookup resolution,
            Function_call_resolution.call_computed_callee source_call )
        with
        | None, Some computed ->
            type_expression table members policies ~before_item_index
              ~context:Value_context state computed
            |> Result.map (fun (result, state) -> (Some result, state))
        | Some _, _ | None, None -> Ok (None, state)
      in
      match callee_result with
      | Error _ as error -> error
      | Ok (callee_result, state) -> (
          match
            source |> Function_call_conversion_policy.indirect_fixed_policies
            |> type_fixed_results table members policies ~before_item_index
                 state
          with
          | Error _ as error -> error
          | Ok (fixed_results, state) -> (
              match
                source
                |> Function_call_conversion_policy.indirect_variadic_arguments
                |> map_state
                     (type_variadic table members policies ~before_item_index)
                     state
              with
              | Error _ as error -> error
              | Ok (variadic_results, state) ->
                  Ok
                    ( Indirect_call_result
                        {
                          source;
                          callee_result;
                          fixed_results;
                          variadic_results;
                        },
                      state ))))
  | Function_call_conversion_policy.Deferred_call_policy call ->
      Ok (Deferred_call_result call, state)

let return_type_is_zero_sized members ~before_item_index type_ =
  if Type.pointer_depth type_ <> 0 then false
  else
    match Type.base type_ with
    | Type.Primitive (_, primitive) -> Primitive_type.is_zero_sized primitive
    | Type.Aggregate symbol -> (
        match Aggregate_member_index.find_aggregate members symbol with
        | Some aggregate
          when Aggregate_member_index.aggregate_item_index aggregate
               < before_item_index ->
            Int64.equal (Aggregate_member_index.aggregate_size aggregate) 0L
        | Some _ | None -> true)

let select_return_conversion declared_class value_class =
  match (declared_class, value_class) with
  | F64_result, Integer_result -> Result_to_f64
  | Integer_result, F64_result -> Result_to_int
  | F64_result, (F64_result | Unresolved_actual_class)
  | Integer_result, (Integer_result | Unresolved_actual_class)
  | Unresolved_actual_class, _ -> No_intrinsic_conversion

let type_condition table members policies ~before_item_index state source =
  match
    type_expression table members policies ~before_item_index
      ~context:Value_context state
      (Function_call_resolution.condition_expression source)
  with
  | Error _ as error -> error
  | Ok (condition_value, state) ->
      Ok ({ condition_source = source; condition_value }, state)

let type_expression_statement table members policies ~before_item_index state
    source =
  match
    type_expression table members policies ~before_item_index
      ~context:Value_context state
      (Function_call_resolution.expression_statement_expression source)
  with
  | Error _ as error -> error
  | Ok (expression_statement_value, state) ->
      Ok
        ( {
            expression_statement_source = source;
            expression_statement_value;
            expression_statement_result_use = Result_not_used;
          },
          state )

let type_implicit_output_argument table members policies ~before_item_index
    state source =
  match
    type_expression table members policies ~before_item_index
      ~context:Value_context state
      (Function_call_resolution.implicit_output_argument_expression source)
  with
  | Error _ as error -> error
  | Ok (implicit_output_argument_value, state) ->
      Ok
        ( {
            implicit_output_argument_source = source;
            implicit_output_argument_value;
          },
          state )

let type_implicit_output table members policies ~before_item_index state source
    =
  match
    type_expression table members policies ~before_item_index
      ~context:Value_context state
      (Function_call_resolution.implicit_output_fixed_expression source)
  with
  | Error _ as error -> error
  | Ok (implicit_output_fixed_value, state) -> (
      match
        source |> Function_call_resolution.implicit_output_arguments
        |> map_state
             (type_implicit_output_argument table members policies
                ~before_item_index)
             state
      with
      | Error _ as error -> error
      | Ok (implicit_output_arguments, state) ->
          Ok
            ( {
                implicit_output_source = source;
                implicit_output_fixed_value;
                implicit_output_arguments;
                implicit_output_result_use = Result_not_used;
              },
              state ))

let type_selector table members policies ~before_item_index state source =
  match
    type_expression table members policies ~before_item_index
      ~context:Value_context state
      (Function_call_resolution.selector_expression source)
  with
  | Error _ as error -> error
  | Ok (selector_value, state) ->
      Ok ({ selector_source = source; selector_value }, state)

let type_switch_case_value table members policies ~before_item_index state
    expression =
  match
    type_expression table members policies ~before_item_index
      ~context:Value_context state expression
  with
  | Error _ as error -> error
  | Ok (value, state) -> (
      let conversion =
        match value.result_class with
        | F64_result -> Result_to_int
        | Integer_result | Unresolved_actual_class -> No_intrinsic_conversion
      in
      match set_intrinsic_conversion state value conversion with
      | Error _ as error -> error
      | Ok (value, state) ->
          Ok
            ( {
                switch_case_value_result = value;
                switch_case_value_conversion = conversion;
              },
              state ))

let type_switch_case table members policies ~before_item_index state source =
  match Function_call_resolution.switch_case_pattern source with
  | Function_call_resolution.Implicit_case ->
      Ok
        ( {
            switch_case_source = source;
            switch_case_pattern = Implicit_case_result;
          },
          state )
  | Function_call_resolution.Single_case expression -> (
      match
        type_switch_case_value table members policies ~before_item_index state
          expression
      with
      | Error _ as error -> error
      | Ok (value, state) ->
          Ok
            ( {
                switch_case_source = source;
                switch_case_pattern = Single_case_result value;
              },
              state ))
  | Function_call_resolution.Ranged_case { start_expression; end_expression; _ }
    -> (
      match
        type_switch_case_value table members policies ~before_item_index state
          start_expression
      with
      | Error _ as error -> error
      | Ok (start_value, state) -> (
          match
            type_switch_case_value table members policies ~before_item_index
              state end_expression
          with
          | Error _ as error -> error
          | Ok (end_value, state) ->
              Ok
                ( {
                    switch_case_source = source;
                    switch_case_pattern =
                      Ranged_case_result { start_value; end_value };
                  },
                  state )))

let type_return table members policies ~before_item_index ~declared_type state
    source =
  match known_type table declared_type with
  | Error _ as error -> error
  | Ok declared_type -> (
      let declared_class =
        forwarded_class policies ~before_item_index declared_type
      in
      let zero_sized =
        return_type_is_zero_sized members ~before_item_index declared_type
      in
      match Function_call_resolution.return_expression source with
      | None ->
          Ok
            ( {
                return_source = source;
                return_declared_type = declared_type;
                return_declared_class = declared_class;
                return_value = None;
                return_conversion = No_intrinsic_conversion;
                return_presence =
                  (if zero_sized then Matching_no_value else Missing_value);
              },
              state )
      | Some expression -> (
          match
            type_expression table members policies ~before_item_index
              ~context:Value_context state expression
          with
          | Error _ as error -> error
          | Ok (value, state) -> (
              let conversion =
                select_return_conversion declared_class value.result_class
              in
              match set_intrinsic_conversion state value conversion with
              | Error _ as error -> error
              | Ok (value, state) ->
                  Ok
                    ( {
                        return_source = source;
                        return_declared_type = declared_type;
                        return_declared_class = declared_class;
                        return_value = Some value;
                        return_conversion = conversion;
                        return_presence =
                          (if zero_sized then Unexpected_value
                           else Matching_value);
                      },
                      state ))))

let type_function table members policies outer state source =
  let outer_function =
    Option.bind outer (fun outer ->
        Outer_expression_binding.find_function outer
          (Function_call_conversion_policy.function_symbol source))
  in
  let state = { state with outer_function; outer_callback_calls_rev = [] } in
  let item_index = Function_call_conversion_policy.function_item_index source in
  match
    source |> Function_call_conversion_policy.function_calls
    |> map_state
         (type_call table members policies ~before_item_index:item_index)
         state
  with
  | Error _ as error -> error
  | Ok (calls, state) -> (
      match
        source |> Function_call_conversion_policy.function_conditions
        |> map_state
             (type_condition table members policies
                ~before_item_index:item_index)
             state
      with
      | Error _ as error -> error
      | Ok (conditions, state) -> (
          match
            source |> Function_call_conversion_policy.function_selectors
            |> map_state
                 (type_selector table members policies
                    ~before_item_index:item_index)
                 state
          with
          | Error _ as error -> error
          | Ok (selectors, state) -> (
              match
                source |> Function_call_conversion_policy.function_switch_cases
                |> map_state
                     (type_switch_case table members policies
                        ~before_item_index:item_index)
                     state
              with
              | Error _ as error -> error
              | Ok (switch_cases, state) -> (
                  let declared_type =
                    source
                    |> Function_call_conversion_policy.function_return_type
                    |> Type_reference.resolved_type
                  in
                  match
                    source |> Function_call_conversion_policy.function_returns
                    |> map_state
                         (type_return table members policies
                            ~before_item_index:item_index ~declared_type)
                         state
                  with
                  | Error _ as error -> error
                  | Ok (returns, state) -> (
                      match
                        source
                        |> Function_call_conversion_policy
                           .function_expression_statements
                        |> map_state
                             (type_expression_statement table members policies
                                ~before_item_index:item_index)
                             state
                      with
                      | Error _ as error -> error
                      | Ok (expression_statements, state) -> (
                          match
                            source
                            |> Function_call_conversion_policy
                               .function_implicit_outputs
                            |> map_state
                                 (type_implicit_output table members policies
                                    ~before_item_index:item_index)
                                 state
                          with
                          | Error _ as error -> error
                          | Ok (implicit_outputs, state) ->
                              Ok
                                ( {
                                    symbol =
                                      Function_call_conversion_policy
                                      .function_symbol source;
                                    scope =
                                      Function_call_conversion_policy
                                      .function_scope source;
                                    item_index;
                                    calls;
                                    outer_callback_calls =
                                      List.sort
                                        (fun left right ->
                                          Int.compare
                                            (left.outer_callback_source
                                           |> Function_call_resolution
                                              .call_index)
                                            (right.outer_callback_source
                                           |> Function_call_resolution
                                              .call_index))
                                        state.outer_callback_calls_rev;
                                    expression_statements;
                                    implicit_outputs;
                                    conditions;
                                    selectors;
                                    switch_cases;
                                    returns;
                                  },
                                  state )))))))

let validate_outer policies outer =
  if
    Outer_expression_binding.source outer
    != Function_call_conversion_policy.expressions policies
  then
    Error
      (invalid_input
         "outer expression bindings describe another expression batch")
  else if
    (match
       outer |> Outer_expression_binding.environment
       |> Outer_environment.compilation_mode
     with
      | Outer_environment.Jit -> Function_resolution.Jit
      | Outer_environment.Aot -> Function_resolution.Aot)
    <> Function_call_conversion_policy.compilation_mode policies
  then
    Error
      (invalid_input
         "outer expression bindings and conversion policies use different \
          compilation modes")
  else
    let missing =
      policies |> Function_call_conversion_policy.functions
      |> List.find_opt (fun function_ ->
          function_ |> Function_call_conversion_policy.function_symbol
          |> Outer_expression_binding.find_function outer
          |> Option.is_none)
    in
    match missing with
    | None -> Ok ()
    | Some function_ ->
        Error
          (invalid_input
             ~origin:
               (function_ |> Function_call_conversion_policy.function_symbol
              |> Symbol.origin)
             "outer expression bindings omit a checked function")

let analyze ~table ~members ?outer policies =
  if not (Function_call_conversion_policy.owns_table policies table) then
    Error
      (invalid_input "call conversion policies belong to another symbol table")
  else if not (Aggregate_member_index.owns_table members table) then
    Error
      (invalid_input "aggregate member index belongs to another symbol table")
  else if
    not
      (Function_call_conversion_policy.owns_parent policies
         (Aggregate_member_index.parent_scope members))
  then
    Error
      (invalid_input
         "aggregate member index and conversion policies describe different \
          modules")
  else if
    match outer with
    | None -> false
    | Some outer -> not (Outer_expression_binding.owns_table outer table)
  then
    Error
      (invalid_input "outer expression bindings belong to another symbol table")
  else
    let outer_validation =
      match outer with
      | None -> Ok ()
      | Some outer -> validate_outer policies outer
    in
    match outer_validation with
    | Error _ as error -> error
    | Ok () -> (
        match
          policies |> Function_call_conversion_policy.functions
          |> map_state
               (type_function table members policies outer)
               {
                 next_id = 0;
                 results_rev = [];
                 outer_function = None;
                 outer_callback_calls_rev = [];
                 top_level_identifier_batch = None;
                 top_level_direct_calls_rev = [];
                 top_level_global_callback_calls_rev = [];
                 top_level_outer_callback_calls_rev = [];
                 top_level_indexed_global_callback_calls_rev = [];
                 top_level_member_callback_calls_rev = [];
               }
        with
        | Error _ as error -> error
        | Ok (functions, state) ->
            Ok
              {
                table;
                members;
                policies;
                outer;
                compilation_mode =
                  Function_call_conversion_policy.compilation_mode policies;
                functions;
                all_results =
                  List.sort
                    (fun left right -> Id.compare left.id right.id)
                    state.results_rev;
              })

let type_top_level_root table members policies ~before_item_index state source =
  match
    type_expression table members policies ~before_item_index
      ~context:Value_context state
      (Top_level_expression_tree.root_expression source)
  with
  | Error _ as error -> error
  | Ok (top_level_root_value, state) ->
      if
        match top_level_root_value.aggregate_offset_path with
        | Some path -> path.offset_segments = []
        | None -> false
      then
        Error
          (invalid_input ~origin:top_level_root_value.origin
             "aggregate offset base requires a member path")
      else
        let top_level_root_result_use =
          match Top_level_expression_tree.root_role source with
          | Top_level_expression_tree.Expression_statement _ ->
              Some Result_not_used
          | Top_level_expression_tree.Implicit_output_fixed _
          | Top_level_expression_tree.Implicit_output_argument _
          | Top_level_expression_tree.Condition _
          | Top_level_expression_tree.Switch_selector _
          | Top_level_expression_tree.Switch_case_value _
          | Top_level_expression_tree.Local_array_dimension _
          | Top_level_expression_tree.Local_initializer _
          | Top_level_expression_tree.Return_value _ -> None
        in
        Ok
          ( {
              top_level_root_source = source;
              top_level_root_value;
              top_level_root_result_use;
            },
            state )

let type_top_level_statement table members policies state source =
  let before_item_index =
    source |> Top_level_expression_tree.statement_source
    |> Top_level_outer_expression_binding.statement_item_index
  in
  match
    source |> Top_level_expression_tree.statement_roots
    |> map_state
         (type_top_level_root table members policies ~before_item_index)
         state
  with
  | Error _ as error -> error
  | Ok (top_level_statement_roots, state) ->
      Ok
        ( { top_level_statement_source = source; top_level_statement_roots },
          state )

let function_mode_of_outer_mode = function
  | Outer_environment.Jit -> Function_resolution.Jit
  | Outer_environment.Aot -> Function_resolution.Aot

let analyze_top_level ~table ~members ~policies ~identifiers source =
  let module_parent =
    source |> Top_level_expression_tree.source
    |> Top_level_outer_expression_binding.source
    |> Top_level_expression_binding.module_expressions
    |> Module_expression_binding.parent_scope
  in
  let source_mode =
    source |> Top_level_expression_tree.source
    |> Top_level_outer_expression_binding.environment
    |> Outer_environment.compilation_mode |> function_mode_of_outer_mode
  in
  if not (Top_level_expression_tree.owns_table source table) then
    Error
      (invalid_top_level_input
         "top-level expression tree belongs to another symbol table")
  else if not (Top_level_identifier_resolution.owns_table identifiers table)
  then
    Error
      (invalid_top_level_input
         "top-level identifier results belong to another symbol table")
  else if Top_level_identifier_resolution.source identifiers != source then
    Error
      (invalid_top_level_input
         "top-level identifier results describe another expression tree")
  else if not (Aggregate_member_index.owns_table members table) then
    Error
      (invalid_top_level_input
         "aggregate member index belongs to another symbol table")
  else if not (Function_call_conversion_policy.owns_table policies table) then
    Error
      (invalid_top_level_input
         "call conversion policies belong to another symbol table")
  else if not (Aggregate_member_index.owns_parent members module_parent) then
    Error
      (invalid_top_level_input "aggregate member index describes another module")
  else if
    not (Function_call_conversion_policy.owns_parent policies module_parent)
  then
    Error
      (invalid_top_level_input
         "call conversion policies describe another module")
  else if
    Function_call_conversion_policy.compilation_mode policies <> source_mode
  then
    Error
      (invalid_top_level_input
         "top-level expressions and conversion policies use different \
          compilation modes")
  else
    match
      source |> Top_level_expression_tree.statements
      |> map_state
           (type_top_level_statement table members policies)
           {
             next_id = 0;
             results_rev = [];
             outer_function = None;
             outer_callback_calls_rev = [];
             top_level_identifier_batch = Some identifiers;
             top_level_direct_calls_rev = [];
             top_level_global_callback_calls_rev = [];
             top_level_outer_callback_calls_rev = [];
             top_level_indexed_global_callback_calls_rev = [];
             top_level_member_callback_calls_rev = [];
           }
    with
    | Error _ as error -> error
    | Ok (top_level_statements, state) ->
        Ok
          {
            top_level_table = table;
            top_level_members = members;
            top_level_policies = policies;
            top_level_identifiers = identifiers;
            top_level_source = source;
            top_level_compilation_mode = source_mode;
            top_level_statements;
            top_level_direct_calls =
              List.sort
                (fun left right ->
                  Int.compare
                    (left.top_level_direct_source
                   |> Top_level_expression_tree.call_source
                   |> Function_call_resolution.call_index)
                    (right.top_level_direct_source
                   |> Top_level_expression_tree.call_source
                   |> Function_call_resolution.call_index))
                state.top_level_direct_calls_rev;
            top_level_global_callback_calls =
              List.sort
                (fun left right ->
                  Int.compare
                    (left.top_level_global_callback_source
                   |> Top_level_expression_tree.call_source
                   |> Function_call_resolution.call_index)
                    (right.top_level_global_callback_source
                   |> Top_level_expression_tree.call_source
                   |> Function_call_resolution.call_index))
                state.top_level_global_callback_calls_rev;
            top_level_outer_callback_calls =
              List.sort
                (fun left right ->
                  Int.compare
                    (top_level_outer_callback_call_index left)
                    (top_level_outer_callback_call_index right))
                state.top_level_outer_callback_calls_rev;
            top_level_indexed_global_callback_calls =
              List.sort
                (fun left right ->
                  Int.compare
                    (left.top_level_indexed_global_callback_source
                   |> Top_level_expression_tree.call_source
                   |> Function_call_resolution.call_index)
                    (right.top_level_indexed_global_callback_source
                   |> Top_level_expression_tree.call_source
                   |> Function_call_resolution.call_index))
                state.top_level_indexed_global_callback_calls_rev;
            top_level_member_callback_calls =
              List.sort
                (fun left right ->
                  Int.compare
                    (left.top_level_member_callback_source
                   |> Top_level_expression_tree.call_source
                   |> Function_call_resolution.call_index)
                    (right.top_level_member_callback_source
                   |> Top_level_expression_tree.call_source
                   |> Function_call_resolution.call_index))
                state.top_level_member_callback_calls_rev;
            top_level_all_results =
              List.sort
                (fun left right -> Id.compare left.id right.id)
                state.results_rev;
          }
