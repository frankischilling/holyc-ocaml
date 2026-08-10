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

val all : (string * t) list
val find_prefix : string -> offset:int -> (t * int) option
val spelling : t -> string
val templeos_token_id : t -> int option
