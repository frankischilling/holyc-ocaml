type evaluated_default = Bits of int64 | String_bytes of string
type parameter_input
type function_input

type warning_kind = Return_type_mismatch | Argument_list_mismatch
type warning
type comparison
type t

type error_kind = Invalid_input of string
type error

val make_parameter_input :
  parameter:Function_type_resolution.parameter ->
  evaluated_default:evaluated_default option ->
  (parameter_input, error) result
(** Attach the result of compile-time default evaluation to one fixed
    parameter. [lastclass] supplies its source-grounded zero payload without an
    evaluator result. *)

val make_function_input :
  declaration:Function_resolution.resolved_declaration ->
  parameter_input list ->
  (function_input, error) result

val analyze :
  table:Symbol_table.t ->
  functions:Function_resolution.t ->
  function_input list ->
  (t, error) result
(** Apply [PrsFunJoin] and [MemberLstCmp] compatibility checks to each joined
    header. The analysis is immutable and requires an input for every resolved
    declaration. *)

val source_functions : t -> Function_resolution.t
val comparisons : t -> comparison list
val warnings : t -> warning list

val comparison_declaration :
  comparison -> Function_resolution.resolved_declaration

val comparison_replaced_header :
  comparison -> Function_resolution.declaration_site

val comparison_option_enabled : comparison -> bool
val comparison_return_types_match : comparison -> bool option
val comparison_arguments_match : comparison -> bool option
val comparison_warnings : comparison -> warning list

val parameter_input_parameter :
  parameter_input -> Function_type_resolution.parameter

val parameter_input_comparison_default :
  parameter_input -> evaluated_default option

val function_input_declaration :
  function_input -> Function_resolution.resolved_declaration

val function_input_parameters : function_input -> parameter_input list
val warning_kind : warning -> warning_kind
val warning_code : warning -> string

val warning_declaration :
  warning -> Function_resolution.resolved_declaration

val warning_replaced_header :
  warning -> Function_resolution.declaration_site

val warning_origin : warning -> Symbol.origin
val warning_message : warning -> string
val warning_kind_name : warning_kind -> string
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
