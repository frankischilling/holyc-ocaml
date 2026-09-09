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

type 'query expression =
  | Selected_query_expression of 'query
  | Integer_expression of { value : int64; origin : Symbol.origin }
  | Unsigned_integer_expression of { value : int64; origin : Symbol.origin }
  | Floating_expression of { value : float; origin : Symbol.origin }
  | Current_position_expression of Symbol.origin
  | Unary_expression of {
      operator : unary_operator;
      operand : 'query expression;
      origin : Symbol.origin;
    }
  | Binary_expression of {
      operator : binary_operator;
      left : 'query expression;
      right : 'query expression;
      origin : Symbol.origin;
    }
  | Dependency_expression of {
      dependency_kind : dependency_kind;
      detail : string;
      origin : Symbol.origin;
    }
  | Unsupported_expression of { description : string; origin : Symbol.origin }

type expression_context = Array_dimension | Aggregate_offset

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

val make_error :
  ?origin:Symbol.origin -> string -> error_kind -> string -> error

val dependency_kind_name : dependency_kind -> string
val invalid_input : ?origin:Symbol.origin -> string -> error
val unresolved : Symbol.origin -> dependency_kind -> string -> error
val invalid_dimension : Symbol.origin -> int64 -> error
val metadata_overflow : Symbol.origin -> string -> error
val error_code : error -> string
val error_kind : error -> error_kind
val error_origin : error -> Symbol.origin option
val error_message : error -> string
val error_to_string : error -> string
val origin : Frontend.Ast.location -> Symbol.origin

val expression_origin :
  query_origin:('q -> Symbol.origin) -> 'q expression -> Symbol.origin

val evaluate_expression :
  ?consume:(unit -> (unit, error) result) ->
  query_origin:('q -> Symbol.origin) ->
  query_value:('q -> int64 option) ->
  context:expression_context ->
  current_position:int64 ->
  'q expression ->
  (int64, error) result
(** Shared numeric evaluation. Consume once per evaluated leaf or operator,
    before doing its work; grouped expressions have no node, and short-circuit
    operands that are not evaluated consume nothing. This is numeric work, not a
    count of VM instructions and not a source-ownership certificate. *)

val equal :
  equal_query:('q -> 'q -> bool) -> 'q expression -> 'q expression -> bool

val unary : Frontend.Ast.unary_operator_kind -> unary_operator option
val binary : Frontend.Operator.binary_operator -> binary_operator option

val of_ast :
  ?allow_floating:bool ->
  query_expression:('q -> Frontend.Ast.expression) ->
  queries:'q list ->
  Frontend.Ast.expression ->
  'q expression
