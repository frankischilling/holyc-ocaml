let rec contains_string_literal (expression : Ast.expression) =
  match expression with
  | Ast.String_literal _ -> true
  | Ast.Integer_literal _
  | Ast.Float_literal _
  | Ast.Character_literal _
  | Ast.Identifier_expression _
  | Ast.Current_position_expression _
  | Ast.Sizeof_expression _
  | Ast.Offset_expression _
  | Ast.Defined_expression _ -> false
  | Ast.Parenthesized_expression grouped ->
      contains_string_literal grouped.grouped_expression
  | Ast.Prefix_expression prefix ->
      contains_string_literal prefix.prefix_operand
  | Ast.Postfix_expression postfix ->
      contains_string_literal postfix.postfix_operand
  | Ast.Postfix_cast_expression cast ->
      contains_string_literal cast.cast_operand
  | Ast.Binary_expression binary ->
      contains_string_literal binary.binary_left
      || contains_string_literal binary.binary_right
  | Ast.Call_expression call ->
      contains_string_literal call.call_callee
      || List.exists
           (fun (argument : Ast.call_argument) ->
             match argument.call_argument_value with
             | Ast.Omitted_call_argument -> false
             | Ast.Provided_call_argument expression ->
                 contains_string_literal expression)
           call.call_arguments
  | Ast.Index_expression index ->
      contains_string_literal index.index_base
      || contains_string_literal index.index_value
  | Ast.Member_expression member -> contains_string_literal member.member_base
