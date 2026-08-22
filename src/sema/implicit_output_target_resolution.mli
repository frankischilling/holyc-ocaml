type module_target

type target_binding =
  | Module_function of module_target
  | Outer_function of Outer_environment.binding

type output
type resolved_function
type t

type error_kind =
  | Invalid_input of string
  | Missing_header of {
      target_name : string;
      output : Function_call_expression_result.implicit_output_result;
    }

type error

val resolve :
  table:Symbol_table.t ->
  environment:Outer_environment.t ->
  module_expressions:Module_expression_binding.t ->
  function_types:Function_type_resolution.t ->
  functions:Function_resolution.t ->
  expressions:Function_call_expression_result.t ->
  (t, error) result
(** Resolve the implicit [Print] and [PutChars] targets through source-visible
    module function publications, then through the supplied outer hash-table
    snapshot. Same-name records of other kinds are ignored, matching the
    [HTT_FUN] mask passed to TempleOS [HashFind]. *)

val owns_table : t -> Symbol_table.t -> bool
val environment : t -> Outer_environment.t
val source : t -> Function_call_expression_result.t
val compilation_mode : t -> Function_resolution.compilation_mode
val functions : t -> resolved_function list
val find_function : t -> Symbol.t -> resolved_function option

val function_source :
  resolved_function -> Function_call_expression_result.resolved_function

val function_outputs : resolved_function -> output list

val output_source :
  output -> Function_call_expression_result.implicit_output_result

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
