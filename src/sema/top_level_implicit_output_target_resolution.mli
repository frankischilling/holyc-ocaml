type module_target

type target_binding =
  | Module_function of module_target
  | Outer_function of Outer_environment.binding

type output
type t

type error_kind =
  | Invalid_input of string
  | Missing_header of { target_name : string; output_index : int }

type error

val resolve :
  table:Symbol_table.t ->
  function_types:Function_type_resolution.t ->
  functions:Function_resolution.t ->
  Function_call_expression_result.top_level_t ->
  (t, error) result
(** Group typed executable top-level output values and select the source-visible
    [Print] or [PutChars] function. Module lookup considers only function
    publications before the containing statement, then falls through the exact
    JIT or AOT outer environment already owned by the expression batch. *)

val owns_table : t -> Symbol_table.t -> bool
val environment : t -> Outer_environment.t
val source : t -> Function_call_expression_result.top_level_t
val compilation_mode : t -> Function_resolution.compilation_mode
val outputs : t -> output list
val output_index : output -> int

val output_statement :
  output -> Function_call_expression_result.top_level_statement_result

val output_target : output -> Function_call_resolution.implicit_output_target

val output_fixed_source :
  output -> Function_call_resolution.implicit_output_fixed_source

val output_marker_origin : output -> Symbol.origin

val output_fixed_value :
  output -> Function_call_expression_result.top_level_root_result

val output_arguments :
  output -> Function_call_expression_result.top_level_root_result list

val output_target_name : output -> string
val output_binding : output -> target_binding
val module_publication : module_target -> Module_expression_binding.publication
val module_header : module_target -> Function_type_resolution.resolved_function

val module_declaration :
  module_target -> Function_resolution.resolved_declaration

val module_target_symbol : module_target -> Symbol.t
val target_binding_name : target_binding -> string
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
