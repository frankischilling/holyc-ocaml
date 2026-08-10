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

let all =
  [
    ("<<=", Shift_left_assign);
    (">>=", Shift_right_assign);
    ("...", Ellipsis);
    ("++", Increment);
    ("--", Decrement);
    ("->", Arrow);
    ("::", Double_colon);
    ("<<", Shift_left);
    (">>", Shift_right);
    ("==", Equal);
    ("!=", Not_equal);
    ("<=", Less_equal);
    (">=", Greater_equal);
    ("&&", Logical_and);
    ("||", Logical_or);
    ("^^", Logical_xor);
    ("*=", Multiply_assign);
    ("/=", Divide_assign);
    ("%=", Modulo_assign);
    ("&=", Bit_and_assign);
    ("|=", Bit_or_assign);
    ("^=", Bit_xor_assign);
    ("+=", Add_assign);
    ("-=", Subtract_assign);
    ("..", Dot_dot);
    ("$$", Current_position);
  ]

let has_prefix text ~offset spelling =
  let width = String.length spelling in
  offset + width <= String.length text
  && String.sub text offset width |> String.equal spelling

let find_prefix text ~offset =
  List.find_map
    (fun (spelling, operator) ->
      if has_prefix text ~offset spelling then
        Some (operator, String.length spelling)
      else None)
    all

let spelling operator =
  List.find_map
    (fun (candidate, value) ->
      if value = operator then Some candidate else None)
    all
  |> Option.get

let templeos_token_id = function
  | Increment -> Some 0x105
  | Decrement -> Some 0x106
  | Arrow -> Some 0x107
  | Double_colon -> Some 0x108
  | Shift_left -> Some 0x109
  | Shift_right -> Some 0x10a
  | Equal -> Some 0x10b
  | Not_equal -> Some 0x10c
  | Less_equal -> Some 0x10d
  | Greater_equal -> Some 0x10e
  | Logical_and -> Some 0x10f
  | Logical_or -> Some 0x110
  | Logical_xor -> Some 0x111
  | Shift_left_assign -> Some 0x112
  | Shift_right_assign -> Some 0x113
  | Multiply_assign -> Some 0x114
  | Divide_assign -> Some 0x115
  | Bit_and_assign -> Some 0x116
  | Bit_or_assign -> Some 0x117
  | Bit_xor_assign -> Some 0x118
  | Add_assign -> Some 0x119
  | Subtract_assign -> Some 0x11a
  | Modulo_assign -> Some 0x122
  | Dot_dot -> Some 0x123
  | Ellipsis -> Some 0x124
  | Current_position -> None
