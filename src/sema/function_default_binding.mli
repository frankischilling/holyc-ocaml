type resolution = Module_binding_environment.resolution =
  | Module_binding of Module_expression_binding.publication
  | Outer_binding of Outer_environment.binding

type event
type parameter_input
type function_input
type occurrence
type resolved_parameter
type resolved_function
type t

type error_kind =
  | Invalid_input of string
  | Unresolved_identifier of {
      function_symbol : Symbol.t;
      parameter_index : int;
      name : string;
      compilation_mode : Outer_environment.compilation_mode;
    }

type error

val make_identifier :
  name:string ->
  origin:Symbol.origin ->
  occurrence_index:int ->
  parameter_index:int ->
  (event, string) result

val make_parameter :
  parameter:Function_type_resolution.parameter ->
  event list ->
  (parameter_input, string) result

val make_function :
  declaration:Function_resolution.resolved_declaration ->
  parameter_input list ->
  (function_input, string) result

val resolve :
  table:Symbol_table.t ->
  environment:Outer_environment.t ->
  expressions:Module_expression_binding.t ->
  functions:Function_resolution.t ->
  function_input list ->
  (t, error) result
(** Bind ordinary identifiers in defaults on top-level named function headers.
    The current header is published before its defaults, while parameter locals
    remain unavailable. *)

val functions : t -> resolved_function list
val environment : t -> Outer_environment.t
val expressions : t -> Module_expression_binding.t
val source_functions : t -> Function_resolution.t
val compilation_mode : t -> Function_resolution.compilation_mode
val owns_table : t -> Symbol_table.t -> bool
val find_function : t -> Symbol.t -> resolved_function option

val function_declaration :
  resolved_function -> Function_resolution.resolved_declaration

val function_publication :
  resolved_function -> Module_expression_binding.publication

val function_source_symbol : resolved_function -> Symbol.t
val function_canonical_symbol : resolved_function -> Symbol.t
val function_item_index : resolved_function -> int
val function_parameters : resolved_function -> resolved_parameter list

val parameter_source :
  resolved_parameter -> Function_type_resolution.parameter

val parameter_index : resolved_parameter -> int

val parameter_default :
  resolved_parameter -> Function_type_resolution.parameter_default option

val parameter_occurrences : resolved_parameter -> occurrence list
val occurrence_index : occurrence -> int
val occurrence_parameter_index : occurrence -> int
val occurrence_name : occurrence -> string
val occurrence_origin : occurrence -> Symbol.origin
val occurrence_resolution : occurrence -> resolution
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
