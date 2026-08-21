type actual_class = Function_call_expression_result.result_class =
  | Integer_result
  | F64_result
  | Unresolved_actual_class

type conversion =
  | No_conversion
  | Result_to_f64
  | Result_to_int
  | Unresolved_conversion

type provided_decision
type fixed_path = Provided_path of provided_decision | Declared_default_path
type fixed_decision
type variadic_decision
type direct_call
type indirect_call

type call_decision =
  | Direct_call_decision of direct_call
  | Indirect_call_decision of indirect_call
  | Deferred_call_decision of Function_call_resolution.call_resolution

type resolved_function
type t
type error_kind = Invalid_input of string
type error

val decide :
  table:Symbol_table.t ->
  policies:Function_call_conversion_policy.t ->
  Function_call_expression_result.t ->
  (t, error) result
(** Select the pinned fixed-call conversion intent for audited source classes.
    Literals keep their exact result class. Current position, [sizeof],
    [offset], and [defined] use the pinned non-F64 side of the comparison.
    Postfix casts use the source-visible class of their explicit target,
    including aggregate backing chains. Unary plus, unary minus, and logical not
    keep the recursively forwarded operand class. Address-of uses the integer
    side. Pre-increment, pre-decrement, post-increment, and post-decrement keep
    the recursively forwarded operand class. Power uses F64; logical and
    comparison operators use the integer side; and ordinary arithmetic, shifts,
    and bitwise operators combine audited operand classes. Assignment families
    use the checked destination class while retaining right-operand conversions
    and a distinct compound execution class. Parentheses preserve these classes.
    Bound parameters, locals, and source-visible globals use their resolved
    scalar type; arrays, callbacks, and synthetic [argv] use the integer side.
    Other expression shapes remain explicit unresolved results. *)

val functions : t -> resolved_function list
val find_function : t -> Symbol.t -> resolved_function option
val compilation_mode : t -> Function_resolution.compilation_mode
val owns_table : t -> Symbol_table.t -> bool
val function_symbol : resolved_function -> Symbol.t
val function_scope : resolved_function -> Symbol_table.scope
val function_item_index : resolved_function -> int
val function_calls : resolved_function -> call_decision list
val direct_source : direct_call -> Function_call_conversion_policy.direct_call
val direct_fixed_decisions : direct_call -> fixed_decision list
val direct_variadic_decisions : direct_call -> variadic_decision list

val indirect_source :
  indirect_call -> Function_call_conversion_policy.indirect_call

val indirect_fixed_decisions : indirect_call -> fixed_decision list
val indirect_variadic_decisions : indirect_call -> variadic_decision list

val fixed_source :
  fixed_decision -> Function_call_conversion_policy.fixed_policy

val fixed_path : fixed_decision -> fixed_path

val provided_target :
  provided_decision -> Function_call_conversion_policy.target_class

val provided_actual_result :
  provided_decision -> Function_call_expression_result.expression_result

val provided_actual : provided_decision -> actual_class
val provided_conversion : provided_decision -> conversion

val variadic_actual_result :
  variadic_decision -> Function_call_expression_result.expression_result

val variadic_actual : variadic_decision -> actual_class
val actual_class_name : actual_class -> string
val conversion_name : conversion -> string
val fixed_path_name : fixed_path -> string
val error_code : error -> string
val error_kind : error -> error_kind
val error_message : error -> string
val error_to_string : error -> string
