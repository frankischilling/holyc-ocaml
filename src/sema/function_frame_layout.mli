type location_kind =
  | Named_parameter
  | Variadic_argc
  | Variadic_argv
  | Automatic_local
  | Static_local

type declarator_shape = Object | Function_pointer
type value_shape = Scalar | Array
type dimension_kind = Source_extent | Compiler_placeholder_extent
type dimension
type frame_slot
type location
type function_layout
type t

type dimension_expression =
  | Empty_dimension
  | Closed_expression of Aggregate_layout.expression
  | Non_integral_expression of { detail : string; origin : Symbol.origin }

type dimension_input = {
  dimension : Local_type_resolution.array_dimension;
  expression_origin : Symbol.origin option;
  expression : dimension_expression;
}

type local_input = {
  local : Local_type_resolution.local;
  dimensions : dimension_input list;
}

type function_input = {
  indexed_function : Function_binding_index.indexed_function;
  typed_function : Function_type_resolution.resolved_function;
  local_function : Local_type_resolution.resolved_function;
  locals : local_input list;
}

type error_kind =
  | Invalid_input of string
  | Unresolved_local_extent of {
      symbol : Symbol.t;
      dimension_index : int;
      detail : string;
    }
  | Non_integral_local_extent of {
      symbol : Symbol.t;
      dimension_index : int;
      detail : string;
    }
  | Invalid_local_extent of {
      symbol : Symbol.t;
      dimension_index : int;
      detail : string;
    }
  | Metadata_overflow of { symbol : Symbol.t; detail : string }
  | Incomplete_aggregate_layout of Symbol.t

type error

val layout :
  table:Symbol_table.t ->
  parent:Symbol_table.scope ->
  aggregate_layouts:Aggregate_layout.t ->
  function_input list ->
  (t, error) result
(** Join the completed binding and type evidence and lay out every function in
    source order. Parameters occupy positive eight-byte slots. The
    automatic-local cursor starts at zero and moves toward negative
    displacements; static locals remain locations without frame slots. *)

val functions : t -> function_layout list
val find_function : t -> Symbol.t -> function_layout option
val function_symbol : function_layout -> Symbol.t
val function_scope : function_layout -> Symbol_table.scope
val function_item_index : function_layout -> int
val function_locations : function_layout -> location list
val function_frame_size : function_layout -> int64

val find_location : function_layout -> Symbol.t -> location option
(** Exact-identity lookup. A static local returns [Some location], while an
    unknown or foreign symbol returns [None]. *)

val find_binding_location :
  function_layout -> Function_binding_index.binding -> location option
(** Look up the location for the exact binding object retained by the binding
    index. A binding rebuilt around the same symbol does not match. *)

val location_binding : location -> Function_binding_index.binding
val location_symbol : location -> Symbol.t
val location_kind : location -> location_kind
val location_type_reference : location -> Type_reference.t option
val location_checked_type : location -> Type.t
val location_declarator_shape : location -> declarator_shape
val location_value_shape : location -> value_shape
val location_dimensions : location -> dimension list
val location_element_size : location -> int64
val location_allocated_size : location -> int64
val location_alignment : location -> int
val location_frame_slot : location -> frame_slot option
val dimension_kind : dimension -> dimension_kind
val dimension_value : dimension -> int64
val frame_slot_displacement : frame_slot -> int64
val frame_slot_size : frame_slot -> int64
val location_kind_name : location_kind -> string
val declarator_shape_name : declarator_shape -> string
val value_shape_name : value_shape -> string
val dimension_kind_name : dimension_kind -> string
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
