type actual_class = Function_call_expression_result.result_class =
  | Integer_result
  | F64_result
  | Unresolved_actual_class

type conversion =
  | No_conversion
  | Result_to_f64
  | Result_to_int
  | Unresolved_conversion

type default_materialization =
  Function_call_expression_result.declared_default_materialization =
  | Immediate_default
  | Aot_string_default

type provided_source =
  | Fixed_expression of Function_call_expression_result.expression_result
  | Following_argument of
      Function_call_expression_result.implicit_output_argument_result

type omission
type provided_binding
type default_binding
type fixed_path = Provided_path of provided_binding | Defaulted_path of default_binding
type fixed_slot
type bound_output
type deferred_output

type output_result =
  | Bound_output of bound_output
  | Deferred_outer_output of deferred_output

type resolved_function
type t

type error_kind =
  | Invalid_input of string
  | Missing_required_parameter of omission
  | Extra_nonvariadic_argument of {
      output : Implicit_output_target_resolution.output;
      provided : provided_source;
      position : int;
      fixed_count : int;
    }

type error

val bind :
  table:Symbol_table.t ->
  policies:Function_call_conversion_policy.t ->
  ?outer_headers:Function_type_resolution.resolved_function list ->
  Implicit_output_target_resolution.t ->
  (t, error) result
(** Bind implicit output values against each selected source-visible header.
    A module target always has a checked header. An outer target is bound only
    when [outer_headers] contains a checked header for the exact resolved outer
    symbol; otherwise the result remains explicitly deferred. *)

val owns_table : t -> Symbol_table.t -> bool
val policies : t -> Function_call_conversion_policy.t
val targets : t -> Implicit_output_target_resolution.t
val compilation_mode : t -> Function_resolution.compilation_mode
val functions : t -> resolved_function list
val find_function : t -> Symbol.t -> resolved_function option

val function_source :
  resolved_function -> Implicit_output_target_resolution.resolved_function

val function_outputs : resolved_function -> output_result list
val output_source : output_result -> Implicit_output_target_resolution.output
val output_result_name : output_result -> string
val bound_source : bound_output -> Implicit_output_target_resolution.output
val bound_header : bound_output -> Function_type_resolution.resolved_function
val bound_fixed_slots : bound_output -> fixed_slot list

val bound_variadic_values :
  bound_output -> Function_call_expression_result.expression_result list

val deferred_source : deferred_output -> Implicit_output_target_resolution.output
val deferred_outer_binding : deferred_output -> Outer_environment.binding
val fixed_parameter : fixed_slot -> Function_type_resolution.parameter
val fixed_path : fixed_slot -> fixed_path
val omission_output : omission -> Implicit_output_target_resolution.output
val omission_parameter : omission -> Function_type_resolution.parameter
val omission_position : omission -> int
val provided_source : provided_binding -> provided_source
val provided_position : provided_binding -> int
val provided_result : provided_binding -> Function_call_expression_result.expression_result
val provided_target : provided_binding -> Function_call_conversion_policy.target_class
val provided_actual : provided_binding -> actual_class
val provided_conversion : provided_binding -> conversion
val default_parameter : default_binding -> Function_type_resolution.parameter
val default_source : default_binding -> Function_type_resolution.parameter_default
val default_omission : default_binding -> omission
val default_target : default_binding -> Function_call_conversion_policy.target_class
val default_materialization : default_binding -> default_materialization
val actual_class_name : actual_class -> string
val conversion_name : conversion -> string
val default_materialization_name : default_materialization -> string
val fixed_path_name : fixed_path -> string
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
