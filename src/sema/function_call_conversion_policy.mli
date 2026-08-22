type target_class = Integer_result | F64_result
type fixed_path = Provided_expression of target_class | Declared_default
type fixed_policy
type direct_call
type indirect_call

type call_policy =
  | Direct_call_policy of direct_call
  | Indirect_call_policy of indirect_call
  | Deferred_call_policy of Function_call_resolution.call_resolution

type resolved_function
type t

type error_kind =
  | Invalid_input of string
  | Aggregate_backing_cycle of Symbol.t list

type error

val analyze :
  table:Symbol_table.t ->
  parent:Symbol_table.scope ->
  headers:Aggregate_header_resolution.t ->
  calls:Function_call_resolution.t ->
  (t, error) result
(** Retain the fixed expression conversion path selected by [PrsFunCall]. A
    provided fixed expression receives the target class forwarded through
    aggregate headers visible before its caller. Defaults, variadic expressions,
    and deferred callees remain distinct source paths. Each resolved function
    also carries its checked return type and source-ordered return inputs for
    the following expression-result pass. *)

val functions : t -> resolved_function list
val find_function : t -> Symbol.t -> resolved_function option
val compilation_mode : t -> Function_resolution.compilation_mode
val owns_table : t -> Symbol_table.t -> bool
val function_symbol : resolved_function -> Symbol.t
val function_scope : resolved_function -> Symbol_table.scope
val function_item_index : resolved_function -> int
val function_return_type : resolved_function -> Type_reference.t

val function_conditions :
  resolved_function -> Function_call_resolution.condition_input list

val function_selectors :
  resolved_function -> Function_call_resolution.selector_input list

val function_returns :
  resolved_function -> Function_call_resolution.return_input list

val function_calls : resolved_function -> call_policy list
val direct_source : direct_call -> Function_call_resolution.direct_call
val direct_fixed_policies : direct_call -> fixed_policy list

val direct_variadic_arguments :
  direct_call -> Function_call_resolution.argument list

val indirect_source : indirect_call -> Function_call_resolution.indirect_call
val indirect_fixed_policies : indirect_call -> fixed_policy list

val indirect_variadic_arguments :
  indirect_call -> Function_call_resolution.argument list

val fixed_source : fixed_policy -> Function_call_resolution.fixed_argument
val fixed_path : fixed_policy -> fixed_path

val forwarded_type_class : t -> before_item_index:int -> Type.t -> target_class
(** Follow the validated by-value aggregate backing relation visible before the
    supplied module item. Pointers and unbacked aggregates use the integer
    result path. *)

val forwarded_type : t -> before_item_index:int -> Type.t -> Type.t
(** Return the source-visible result of the same checked backing traversal.
    Pointer types are not forwarded. *)

val target_class_name : target_class -> string
val fixed_path_name : fixed_path -> string
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
