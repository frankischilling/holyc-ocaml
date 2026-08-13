type function_pointer = Function_type_resolution.function_pointer
type declarator_kind = Object | Function_pointer of function_pointer
type array_dimension
type delimiter_kind = Comma | Semicolon
type delimiter
type initializer_kind = Scalar_initializer | Braced_initializer
type initial_value
type global
type t

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

val make_global :
  symbol:Symbol.t ->
  item_index:int ->
  ?declarator_index:int ->
  declarator_origin:Symbol.origin ->
  type_reference:Type_reference.t ->
  declarator_kind:declarator_kind ->
  array_dimensions:array_dimension list ->
  initial_value:initial_value option ->
  delimiter:delimiter ->
  unit ->
  (global, string) result

val resolve :
  table:Symbol_table.t ->
  parent:Symbol_table.scope ->
  global list ->
  (t, string) result
(** Validate source-ordered global types without changing the symbol table. *)

val globals : t -> global list
val global_symbol : global -> Symbol.t
val global_item_index : global -> int
val global_declarator_index : global -> int option
val global_declarator_origin : global -> Symbol.origin
val global_type_reference : global -> Type_reference.t
val global_declarator_kind : global -> declarator_kind
val global_array_dimensions : global -> array_dimension list
val global_initializer : global -> initial_value option
val global_delimiter : global -> delimiter
val array_dimension_index : array_dimension -> int
val array_dimension_origin : array_dimension -> Symbol.origin
val array_dimension_opening_origin : array_dimension -> Symbol.origin
val array_dimension_expression_origin : array_dimension -> Symbol.origin option
val array_dimension_closing_origin : array_dimension -> Symbol.origin
val delimiter_kind : delimiter -> delimiter_kind
val delimiter_origin : delimiter -> Symbol.origin
val delimiter_kind_name : delimiter_kind -> string
val initializer_kind : initial_value -> initializer_kind
val initializer_origin : initial_value -> Symbol.origin
val initializer_equals_origin : initial_value -> Symbol.origin
val initializer_value_origin : initial_value -> Symbol.origin
val initializer_kind_name : initializer_kind -> string
val function_pointer_origin : function_pointer -> Symbol.origin
val function_pointer_opening_origin : function_pointer -> Symbol.origin

val function_pointer_indirection_origins :
  function_pointer -> Symbol.origin list

val function_pointer_closing_origin : function_pointer -> Symbol.origin

val function_pointer_signature :
  function_pointer -> Function_type_resolution.signature
