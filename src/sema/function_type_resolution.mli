type parameter_default =
  | Expression_default of {
      origin : Symbol.origin;
      equals_origin : Symbol.origin;
      expression_origin : Symbol.origin;
      contains_string_literal : bool;
    }
  | Lastclass_default of {
      origin : Symbol.origin;
      equals_origin : Symbol.origin;
      keyword_origin : Symbol.origin;
    }

type declarator_kind = Object | Function_pointer of function_pointer
and function_pointer
and parameter
and signature

type parameter_binding
type synthetic_parameter = Argc | Argv

type synthetic_shape =
  | Scalar
  | Array of { source_extent : int option; compiler_placeholder_extent : int }

type synthetic_binding
type variadic_bindings
type function_declaration
type resolved_function
type t

val make_parameter :
  index:int ->
  origin:Symbol.origin ->
  ?register_requests:Register_request.t list ->
  ?name:string ->
  ?name_origin:Symbol.origin ->
  type_reference:Type_reference.t ->
  declarator_kind:declarator_kind ->
  default:parameter_default option ->
  ?delimiter_origin:Symbol.origin ->
  unit ->
  (parameter, string) result

val make_function_pointer :
  origin:Symbol.origin ->
  opening_origin:Symbol.origin ->
  indirection_origins:Symbol.origin list ->
  closing_origin:Symbol.origin ->
  signature:signature ->
  (function_pointer, string) result

val make_signature :
  opening_origin:Symbol.origin ->
  parameters:parameter list ->
  ?variadic_origin:Symbol.origin ->
  ?variadic_register_requests:Register_request.t list ->
  closing_origin:Symbol.origin ->
  unit ->
  (signature, string) result

val make_parameter_binding :
  parameter_index:int -> symbol:Symbol.t -> (parameter_binding, string) result

val make_synthetic_binding :
  synthetic_parameter ->
  symbol:Symbol.t ->
  parameter_index:int ->
  resolved_type:Type.t ->
  ?register_requests:Register_request.t list ->
  shape:synthetic_shape ->
  unit ->
  (synthetic_binding, string) result

val make_variadic_bindings :
  marker_origin:Symbol.origin ->
  argc:synthetic_binding ->
  argv:synthetic_binding ->
  (variadic_bindings, string) result

val make_function :
  symbol:Symbol.t ->
  scope:Symbol_table.scope ->
  item_index:int ->
  return_type:Type_reference.t ->
  signature:signature ->
  parameter_bindings:parameter_binding list ->
  variadic_bindings:variadic_bindings option ->
  (function_declaration, string) result

val resolve :
  table:Symbol_table.t ->
  parent:Symbol_table.scope ->
  function_declaration list ->
  (t, string) result
(** Validate source-ordered function signatures without changing the symbol
    table. *)

val functions : t -> resolved_function list
val function_symbol : resolved_function -> Symbol.t
val function_scope : resolved_function -> Symbol_table.scope
val function_item_index : resolved_function -> int
val function_return_type : resolved_function -> Type_reference.t
val function_signature : resolved_function -> signature
val function_parameter_bindings : resolved_function -> parameter_binding list
val function_variadic_bindings : resolved_function -> variadic_bindings option
val signature_opening_origin : signature -> Symbol.origin
val signature_parameters : signature -> parameter list
val signature_variadic_origin : signature -> Symbol.origin option
val signature_variadic_register_requests : signature -> Register_request.t list

val signature_variadic_register_selection :
  signature -> Register_request.selection

val signature_closing_origin : signature -> Symbol.origin
val parameter_index : parameter -> int
val parameter_origin : parameter -> Symbol.origin
val parameter_register_requests : parameter -> Register_request.t list
val parameter_register_selection : parameter -> Register_request.selection
val parameter_name : parameter -> string option
val parameter_name_origin : parameter -> Symbol.origin option
val parameter_type_reference : parameter -> Type_reference.t
val parameter_declarator_kind : parameter -> declarator_kind
val parameter_default : parameter -> parameter_default option
val parameter_flag_mask : parameter -> int64
val parameter_has_flag : parameter -> Member_flag.t -> bool
val parameter_delimiter_origin : parameter -> Symbol.origin option
val function_pointer_origin : function_pointer -> Symbol.origin
val function_pointer_opening_origin : function_pointer -> Symbol.origin

val function_pointer_indirection_origins :
  function_pointer -> Symbol.origin list

val function_pointer_closing_origin : function_pointer -> Symbol.origin
val function_pointer_signature : function_pointer -> signature
val parameter_binding_index : parameter_binding -> int
val parameter_binding_symbol : parameter_binding -> Symbol.t
val variadic_marker_origin : variadic_bindings -> Symbol.origin
val variadic_argc : variadic_bindings -> synthetic_binding
val variadic_argv : variadic_bindings -> synthetic_binding
val synthetic_binding_kind : synthetic_binding -> synthetic_parameter
val synthetic_binding_symbol : synthetic_binding -> Symbol.t
val synthetic_binding_index : synthetic_binding -> int
val synthetic_binding_type : synthetic_binding -> Type.t
val synthetic_binding_shape : synthetic_binding -> synthetic_shape
val synthetic_binding_register_requests : synthetic_binding -> Register_request.t list

val synthetic_binding_register_selection :
  synthetic_binding -> Register_request.selection

val synthetic_binding_flag_mask : synthetic_binding -> int64
val synthetic_binding_has_flag : synthetic_binding -> Member_flag.t -> bool
val synthetic_parameter_name : synthetic_parameter -> string
