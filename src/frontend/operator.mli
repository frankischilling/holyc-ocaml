type t =
  | Increment
  | Decrement
  | Arrow
  | Double_colon
  | Shift_left
  | Shift_right
  | Equal
  | Not_equal
  | Less_equal
  | Greater_equal
  | Logical_and
  | Logical_or
  | Logical_xor
  | Shift_left_assign
  | Shift_right_assign
  | Multiply_assign
  | Divide_assign
  | Modulo_assign
  | Bit_and_assign
  | Bit_or_assign
  | Bit_xor_assign
  | Add_assign
  | Subtract_assign
  | Dot_dot
  | Ellipsis
  | Current_position

type provenance =
  | Dual_table of int
  | Shift_assignment
  | Dot_sequence
  | Dollar_sequence

type association = Unspecified | Left | Right
type precedence = private { name : string; value : int; source_line : int }

type binary_operator = private {
  spelling : string;
  token_name : string option;
  token_id : int;
  precedence_name : string;
  precedence_value : int;
  association : association;
  ic_name : string;
  ic_id : int;
  source_line : int;
}

val reference_commit : string
val precedence_source_path : string
val binary_source_path : string
val all : (string * t) list
val find_prefix : string -> offset:int -> (t * int) option
val spelling : t -> string
val templeos_token_id : t -> int option
val source_path : t -> string
val source_line : t -> int
val provenance : t -> provenance
val precedences : precedence list
val binary_operators : binary_operator list
