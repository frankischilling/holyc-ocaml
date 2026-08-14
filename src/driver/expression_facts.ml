let rec contains_string_literal (expression : Frontend.Ast.expression) =
  match expression with
  | Frontend.Ast.String_literal _ -> true
  | Frontend.Ast.Integer_literal _
  | Frontend.Ast.Float_literal _
  | Frontend.Ast.Character_literal _
  | Frontend.Ast.Identifier_expression _
  | Frontend.Ast.Current_position_expression _
  | Frontend.Ast.Sizeof_expression _
  | Frontend.Ast.Offset_expression _
  | Frontend.Ast.Defined_expression _ -> false
  | Frontend.Ast.Parenthesized_expression grouped ->
      contains_string_literal grouped.grouped_expression
  | Frontend.Ast.Prefix_expression prefix ->
      contains_string_literal prefix.prefix_operand
  | Frontend.Ast.Postfix_expression postfix ->
      contains_string_literal postfix.postfix_operand
  | Frontend.Ast.Postfix_cast_expression cast ->
      contains_string_literal cast.cast_operand
  | Frontend.Ast.Binary_expression binary ->
      contains_string_literal binary.binary_left
      || contains_string_literal binary.binary_right
  | Frontend.Ast.Call_expression call ->
      contains_string_literal call.call_callee
      || List.exists
           (fun (argument : Frontend.Ast.call_argument) ->
             match argument.call_argument_value with
             | Frontend.Ast.Omitted_call_argument -> false
             | Frontend.Ast.Provided_call_argument expression ->
                 contains_string_literal expression)
           call.call_arguments
  | Frontend.Ast.Index_expression index ->
      contains_string_literal index.index_base
      || contains_string_literal index.index_value
  | Frontend.Ast.Member_expression member ->
      contains_string_literal member.member_base
