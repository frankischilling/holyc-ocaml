type unary_operator = Identity | Negate | Logical_not | Bitwise_not

type binary_operator =
  | Power
  | Shift_left
  | Shift_right
  | Multiply
  | Divide
  | Modulo
  | Bit_and
  | Bit_xor
  | Bit_or
  | Add
  | Subtract
  | Less
  | Greater
  | Less_equal
  | Greater_equal
  | Equal
  | Not_equal
  | Logical_and
  | Logical_xor
  | Logical_or

type dependency_kind =
  | Identifier_dependency
  | Sizeof_dependency
  | Offset_dependency
  | Defined_dependency
  | Call_dependency
  | Aggregate_dependency

type expression =
  | Integer_expression of {
      value : int64;
      origin : Symbol.origin;
    }
  | Current_position_expression of Symbol.origin
  | Unary_expression of {
      operator : unary_operator;
      operand : expression;
      origin : Symbol.origin;
    }
  | Binary_expression of {
      operator : binary_operator;
      left : expression;
      right : expression;
      origin : Symbol.origin;
    }
  | Dependency_expression of {
      dependency_kind : dependency_kind;
      detail : string;
      origin : Symbol.origin;
    }
  | Unsupported_expression of {
      description : string;
      origin : Symbol.origin;
    }

type expression_context = Array_dimension | Aggregate_offset

type dimension = {
  dimension_expression : expression option;
  dimension_origin : Symbol.origin;
}

type member_input = {
  member_symbol : Symbol.t;
  member_path : int list;
  member_declarator_index : int;
  member_origin : Symbol.origin;
  member_type : Type.t;
  member_is_function_pointer : bool;
  member_dimensions : dimension list;
}

type item =
  | Field of member_input
  | Offset_directive of expression
  | Anonymous_union of {
      union_origin : Symbol.origin;
      union_items : item list;
    }
  | Empty_member of Symbol.origin

type aggregate_kind = Class | Union
type base_input = { base_symbol : Symbol.t; base_origin : Symbol.origin }

type aggregate_input = {
  aggregate_symbol : Symbol.t;
  aggregate_scope : Symbol_table.scope;
  aggregate_kind : aggregate_kind;
  aggregate_item_index : int;
  aggregate_origin : Symbol.origin;
  aggregate_base : base_input option;
  aggregate_items : item list;
}

type signedness = Signed | Unsigned | Not_applicable

type member_layout = private {
  symbol : Symbol.t;
  path : int list;
  declarator_index : int;
  origin : Symbol.origin;
  offset : int64;
  size : int64;
  element_size : int64;
  dimensions : int64 list;
  signedness : signedness;
  alignment : int;
}

type base_layout = private {
  symbol : Symbol.t;
  origin : Symbol.origin;
  offset : int64;
  size : int64;
}

type aggregate_layout = private {
  symbol : Symbol.t;
  kind : aggregate_kind;
  item_index : int;
  origin : Symbol.origin;
  size : int64;
  alignment : int;
  negative_offset : int64;
  base : base_layout option;
  members : member_layout list;
}

type t

type error_kind =
  | Invalid_input of string
  | Unresolved_dependency of {
      dependency_kind : dependency_kind;
      detail : string;
    }
  | Invalid_array_dimension of int64
  | Division_by_zero
  | Signed_division_overflow
  | Non_finite_layout_value
  | Numeric_conversion_overflow
  | Metadata_overflow of string
  | Invalid_layout_expression of string

type error

val evaluate_expression :
  context:expression_context ->
  current_position:int64 ->
  expression ->
  (int64, error) result
(** Evaluate one closed layout expression. Array dimensions force a floating
    result to [I64], while an aggregate offset retains the raw [F64] bits used
    by [LexExpression]. *)

val layout :
  table:Symbol_table.t ->
  parent:Symbol_table.scope ->
  aggregate_input list ->
  (t, error) result
(** Lay out definitions in source order. A by-value aggregate or base must have
    a completed earlier layout; pointers do not require the pointee layout. *)

val layouts : t -> aggregate_layout list
val find : t -> Symbol.t -> aggregate_layout option
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
val dependency_kind_name : dependency_kind -> string
val signedness_name : signedness -> string
