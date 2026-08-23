type conversion =
  | No_conversion
  | Result_to_f64
  | Result_to_int
  | Unresolved_conversion

type 'value provided
type defaulted
type 'value fixed_path = Provided of 'value provided | Defaulted of defaulted
type 'value fixed_slot
type 'value plan
type outer_headers
type outer_header_error = Foreign_header | Duplicate_header

type 'value error =
  | Missing_required_parameter of {
      parameter : Function_type_resolution.parameter;
      position : int;
    }
  | Extra_nonvariadic_argument of {
      provided : 'value;
      position : int;
      fixed_count : int;
    }

val plan :
  Function_type_resolution.resolved_function ->
  'value list ->
  ('value plan, 'value error) result
(** Split source values into fixed parameter paths and a variadic tail. *)

val fixed_slots : 'value plan -> 'value fixed_slot list
val variadic_values : 'value plan -> 'value list
val fixed_parameter : 'value fixed_slot -> Function_type_resolution.parameter
val fixed_path : 'value fixed_slot -> 'value fixed_path
val provided_value : 'value provided -> 'value
val provided_position : 'value provided -> int
val default_source : defaulted -> Function_type_resolution.parameter_default
val default_position : defaulted -> int

val collect_outer_headers :
  Symbol_table.t ->
  Function_type_resolution.resolved_function list ->
  (outer_headers, outer_header_error) result

val find_outer_header :
  outer_headers -> Symbol.t -> Function_type_resolution.resolved_function option

val conversion :
  Function_call_conversion_policy.target_class ->
  Function_call_expression_result.result_class ->
  conversion

val materialization :
  Function_resolution.compilation_mode ->
  Function_type_resolution.parameter_default ->
  Function_call_expression_result.declared_default_materialization
