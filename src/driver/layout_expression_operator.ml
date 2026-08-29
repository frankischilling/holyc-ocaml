let unary = function
  | Frontend.Ast.Unary_plus -> Some Sema.Aggregate_layout.Identity
  | Frontend.Ast.Unary_minus -> Some Sema.Aggregate_layout.Negate
  | Frontend.Ast.Logical_not -> Some Sema.Aggregate_layout.Logical_not
  | Frontend.Ast.Bitwise_not -> Some Sema.Aggregate_layout.Bitwise_not
  | Frontend.Ast.Dereference
  | Frontend.Ast.Address_of
  | Frontend.Ast.Pre_increment
  | Frontend.Ast.Pre_decrement -> None

let binary (operator : Frontend.Operator.binary_operator) =
  match operator.ic_name with
  | "IC_POWER" -> Some Sema.Aggregate_layout.Power
  | "IC_SHL" -> Some Sema.Aggregate_layout.Shift_left
  | "IC_SHR" -> Some Sema.Aggregate_layout.Shift_right
  | "IC_MUL" -> Some Sema.Aggregate_layout.Multiply
  | "IC_DIV" -> Some Sema.Aggregate_layout.Divide
  | "IC_MOD" -> Some Sema.Aggregate_layout.Modulo
  | "IC_AND" -> Some Sema.Aggregate_layout.Bit_and
  | "IC_XOR" -> Some Sema.Aggregate_layout.Bit_xor
  | "IC_OR" -> Some Sema.Aggregate_layout.Bit_or
  | "IC_ADD" -> Some Sema.Aggregate_layout.Add
  | "IC_SUB" -> Some Sema.Aggregate_layout.Subtract
  | "IC_LESS" -> Some Sema.Aggregate_layout.Less
  | "IC_GREATER" -> Some Sema.Aggregate_layout.Greater
  | "IC_LESS_EQU" -> Some Sema.Aggregate_layout.Less_equal
  | "IC_GREATER_EQU" -> Some Sema.Aggregate_layout.Greater_equal
  | "IC_EQU_EQU" -> Some Sema.Aggregate_layout.Equal
  | "IC_NOT_EQU" -> Some Sema.Aggregate_layout.Not_equal
  | "IC_AND_AND" -> Some Sema.Aggregate_layout.Logical_and
  | "IC_XOR_XOR" -> Some Sema.Aggregate_layout.Logical_xor
  | "IC_OR_OR" -> Some Sema.Aggregate_layout.Logical_or
  | _ -> None
