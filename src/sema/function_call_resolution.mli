type call_syntax = Parenthesized | Parenthesis_free
type argument_kind = Provided | Omitted

type unresolved_expression_kind =
  | Identifier_expression
  | Current_position_expression
  | Sizeof_expression
  | Offset_expression
  | Defined_expression
  | Prefix_expression
  | Postfix_expression
  | Postfix_cast_expression
  | Binary_expression
  | Call_expression
  | Index_expression
  | Member_expression

type argument_expression_kind =
  | Integer_literal
  | Float_literal
  | Character_literal
  | String_literal
  | Parenthesized_expression of argument_expression
  | Postfix_cast_expression of argument_expression * Type_reference.t
  | Unresolved_expression of unresolved_expression_kind

and argument_expression

type argument
type call
type function_input

val make_argument_expression :
  kind:argument_expression_kind -> origin:Symbol.origin -> argument_expression

val make_argument :
  index:int ->
  kind:argument_kind ->
  expression:argument_expression option ->
  origin:Symbol.origin ->
  (argument, string) result

val make_call :
  index:int ->
  callee_occurrence_index:int ->
  callee_name:string ->
  callee_origin:Symbol.origin ->
  origin:Symbol.origin ->
  syntax:call_syntax ->
  argument list ->
  (call, string) result

val make_function :
  symbol:Symbol.t ->
  scope:Symbol_table.scope ->
  item_index:int ->
  call list ->
  (function_input, string) result

type default_use

type fixed_value =
  | Provided_argument of argument
  | Declared_default of default_use

type fixed_argument
type direct_call

type deferred_reason =
  | Local_callee of Function_binding_index.binding
  | Global_callee of Module_expression_binding.publication
  | Aggregate_callee of Module_expression_binding.publication
  | Outer_callee

type call_resolution =
  | Direct_call of direct_call
  | Deferred_call of {
      call : call;
      occurrence : Module_expression_binding.occurrence;
      reason : deferred_reason;
    }

type resolved_function
type t

type error_kind =
  | Invalid_input of string
  | Missing_required_argument of {
      call : call;
      parameter : Function_type_resolution.parameter;
      omission : argument option;
    }
  | Extra_fixed_argument of {
      call : call;
      argument : argument;
      fixed_count : int;
    }
  | Omitted_variadic_argument of { call : call; argument : argument }

type error

val resolve :
  table:Symbol_table.t ->
  parent:Symbol_table.scope ->
  function_types:Function_type_resolution.t ->
  functions:Function_resolution.t ->
  expressions:Module_expression_binding.t ->
  function_input list ->
  (t, error) result
(** Resolve syntactically direct function-body calls. A module function target
    receives the source header visible to the caller and its canonical joined
    identity. Named aggregate cast targets must carry the exact identity visible
    before the caller. Other callee categories remain explicit deferred calls.
*)

val functions : t -> resolved_function list
val find_function : t -> Symbol.t -> resolved_function option
val compilation_mode : t -> Function_resolution.compilation_mode
val owns_table : t -> Symbol_table.t -> bool
val function_symbol : resolved_function -> Symbol.t
val function_scope : resolved_function -> Symbol_table.scope
val function_item_index : resolved_function -> int
val function_calls : resolved_function -> call_resolution list
val call_index : call -> int
val call_callee_occurrence_index : call -> int
val call_callee_name : call -> string
val call_callee_origin : call -> Symbol.origin
val call_origin : call -> Symbol.origin
val call_syntax : call -> call_syntax
val call_arguments : call -> argument list
val argument_index : argument -> int
val argument_kind : argument -> argument_kind
val argument_expression : argument -> argument_expression option
val argument_origin : argument -> Symbol.origin
val argument_expression_kind : argument_expression -> argument_expression_kind
val argument_expression_origin : argument_expression -> Symbol.origin

val default_parameter_default :
  default_use -> Function_type_resolution.parameter_default

val default_omission : default_use -> argument option
val fixed_parameter : fixed_argument -> Function_type_resolution.parameter
val fixed_value : fixed_argument -> fixed_value
val direct_source : direct_call -> call
val direct_occurrence : direct_call -> Module_expression_binding.occurrence

val direct_active_header :
  direct_call -> Function_type_resolution.resolved_function

val direct_target_symbol : direct_call -> Symbol.t
val direct_fixed_arguments : direct_call -> fixed_argument list
val direct_variadic_arguments : direct_call -> argument list
val direct_variadic_count : direct_call -> int64
val call_syntax_name : call_syntax -> string
val argument_kind_name : argument_kind -> string
val unresolved_expression_kind_name : unresolved_expression_kind -> string
val argument_expression_kind_name : argument_expression_kind -> string
val deferred_reason_name : deferred_reason -> string
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
