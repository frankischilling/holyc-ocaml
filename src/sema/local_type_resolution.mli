type function_pointer = Function_type_resolution.function_pointer
type declarator_kind = Object | Function_pointer of function_pointer
type storage = Automatic | Static
type register_request_kind = Allocate | Disable
type register_position = Before_type | After_type
type register_request
type array_dimension
type delimiter_kind = Comma | Semicolon
type delimiter
type initializer_kind = Scalar_initializer | Braced_initializer
type initial_value
type local
type function_declaration
type resolved_function
type t

val make_register_request :
  kind:register_request_kind ->
  position:register_position ->
  spelling:string ->
  origin:Symbol.origin ->
  ?explicit_register:string ->
  ?explicit_register_origin:Symbol.origin ->
  unit ->
  (register_request, string) result

val make_array_dimension :
  index:int ->
  origin:Symbol.origin ->
  opening_origin:Symbol.origin ->
  ?expression_origin:Symbol.origin ->
  closing_origin:Symbol.origin ->
  unit ->
  (array_dimension, string) result

val make_delimiter : kind:delimiter_kind -> origin:Symbol.origin -> delimiter

val make_initializer :
  kind:initializer_kind ->
  origin:Symbol.origin ->
  equals_origin:Symbol.origin ->
  value_origin:Symbol.origin ->
  initial_value

val make_local :
  symbol:Symbol.t ->
  declaration_index:int ->
  declarator_index:int ->
  declaration_origin:Symbol.origin ->
  declarator_origin:Symbol.origin ->
  storage:storage ->
  storage_origins:Symbol.origin list ->
  type_reference:Type_reference.t ->
  register_requests:register_request list ->
  declarator_kind:declarator_kind ->
  array_dimensions:array_dimension list ->
  initial_value:initial_value option ->
  delimiter:delimiter ->
  unit ->
  (local, string) result

val make_function :
  symbol:Symbol.t ->
  scope:Symbol_table.scope ->
  item_index:int ->
  local list ->
  (function_declaration, string) result

val resolve :
  table:Symbol_table.t ->
  parent:Symbol_table.scope ->
  function_declaration list ->
  (t, string) result
(** Validate source-ordered local declaration types without changing the symbol
    table. *)

val functions : t -> resolved_function list
val function_symbol : resolved_function -> Symbol.t
val function_scope : resolved_function -> Symbol_table.scope
val function_item_index : resolved_function -> int
val function_locals : resolved_function -> local list
val local_symbol : local -> Symbol.t
val local_declaration_index : local -> int
val local_declarator_index : local -> int
val local_declaration_origin : local -> Symbol.origin
val local_declarator_origin : local -> Symbol.origin
val local_storage : local -> storage
val local_storage_origins : local -> Symbol.origin list
val local_type_reference : local -> Type_reference.t
val local_register_requests : local -> register_request list
val local_declarator_kind : local -> declarator_kind
val local_flag_mask : local -> int64
val local_has_flag : local -> Member_flag.t -> bool
val local_array_dimensions : local -> array_dimension list
val local_initializer : local -> initial_value option
val local_delimiter : local -> delimiter
val register_request_kind : register_request -> register_request_kind
val register_request_position : register_request -> register_position
val register_request_spelling : register_request -> string
val register_request_origin : register_request -> Symbol.origin
val register_request_explicit_register : register_request -> string option

val register_request_explicit_register_origin :
  register_request -> Symbol.origin option

val array_dimension_index : array_dimension -> int
val array_dimension_origin : array_dimension -> Symbol.origin
val array_dimension_opening_origin : array_dimension -> Symbol.origin
val array_dimension_expression_origin : array_dimension -> Symbol.origin option
val array_dimension_closing_origin : array_dimension -> Symbol.origin
val delimiter_kind : delimiter -> delimiter_kind
val delimiter_origin : delimiter -> Symbol.origin
val initializer_kind : initial_value -> initializer_kind
val initializer_origin : initial_value -> Symbol.origin
val initializer_equals_origin : initial_value -> Symbol.origin
val initializer_value_origin : initial_value -> Symbol.origin
val storage_name : storage -> string
val register_request_kind_name : register_request_kind -> string
val register_position_name : register_position -> string
val delimiter_kind_name : delimiter_kind -> string
val initializer_kind_name : initializer_kind -> string
val function_pointer_origin : function_pointer -> Symbol.origin
val function_pointer_opening_origin : function_pointer -> Symbol.origin

val function_pointer_indirection_origins :
  function_pointer -> Symbol.origin list

val function_pointer_closing_origin : function_pointer -> Symbol.origin

val function_pointer_signature :
  function_pointer -> Function_type_resolution.signature
