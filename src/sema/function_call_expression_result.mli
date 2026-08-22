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

type expression_result

type fixed_path =
  | Provided_result of expression_result
  | Declared_default_result

type lastclass_substitution
type fixed_result
type direct_call
type indirect_call

type call_result =
  | Direct_call_result of direct_call
  | Indirect_call_result of indirect_call
  | Deferred_call_result of Function_call_resolution.call_resolution

type resolved_function
type t
type error_kind = Invalid_input of string
type error

val analyze :
  table:Symbol_table.t ->
  members:Aggregate_member_index.t ->
  Function_call_conversion_policy.t ->
  (t, error) result
(** Derive immutable source-expression results for provided fixed and variadic
    arguments on resolved direct and typed indirect calls. Each result has a
    deterministic identity, source origin, known semantic type, value category,
    remaining array rank, intrinsic conversion requirement, forwarded result
    class, and any distinct execution class needed by later lowering. A selected
    [lastclass] default retains the previous provided result and derived base
    spelling. *)

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

val value_category_name : value_category -> string
val result_class_name : result_class -> string
val intrinsic_conversion_name : intrinsic_conversion -> string
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
