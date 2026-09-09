let rec equal (left : Aggregate_layout.expression)
    (right : Aggregate_layout.expression) =
  match (left, right) with
  | Selected_query_expression left, Selected_query_expression right ->
      left == right
  | Floating_expression left, Floating_expression right ->
      Int64.bits_of_float left.value = Int64.bits_of_float right.value
      && left.origin = right.origin
  | Unary_expression left, Unary_expression right ->
      left.operator = right.operator
      && left.origin = right.origin
      && equal left.operand right.operand
  | Binary_expression left, Binary_expression right ->
      left.operator = right.operator
      && left.origin = right.origin && equal left.left right.left
      && equal left.right right.right
  | _ -> left = right

let origin (location : Frontend.Ast.location) =
  Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

let unary = function
  | Frontend.Ast.Unary_plus -> Some Aggregate_layout.Identity
  | Frontend.Ast.Unary_minus -> Some Aggregate_layout.Negate
  | Frontend.Ast.Logical_not -> Some Aggregate_layout.Logical_not
  | Frontend.Ast.Bitwise_not -> Some Aggregate_layout.Bitwise_not
  | Frontend.Ast.Dereference
  | Frontend.Ast.Address_of
  | Frontend.Ast.Pre_increment
  | Frontend.Ast.Pre_decrement -> None

let binary (operator : Frontend.Operator.binary_operator) =
  match operator.ic_name with
  | "IC_POWER" -> Some Aggregate_layout.Power
  | "IC_SHL" -> Some Aggregate_layout.Shift_left
  | "IC_SHR" -> Some Aggregate_layout.Shift_right
  | "IC_MUL" -> Some Aggregate_layout.Multiply
  | "IC_DIV" -> Some Aggregate_layout.Divide
  | "IC_MOD" -> Some Aggregate_layout.Modulo
  | "IC_AND" -> Some Aggregate_layout.Bit_and
  | "IC_XOR" -> Some Aggregate_layout.Bit_xor
  | "IC_OR" -> Some Aggregate_layout.Bit_or
  | "IC_ADD" -> Some Aggregate_layout.Add
  | "IC_SUB" -> Some Aggregate_layout.Subtract
  | "IC_LESS" -> Some Aggregate_layout.Less
  | "IC_GREATER" -> Some Aggregate_layout.Greater
  | "IC_LESS_EQU" -> Some Aggregate_layout.Less_equal
  | "IC_GREATER_EQU" -> Some Aggregate_layout.Greater_equal
  | "IC_EQU_EQU" -> Some Aggregate_layout.Equal
  | "IC_NOT_EQU" -> Some Aggregate_layout.Not_equal
  | "IC_AND_AND" -> Some Aggregate_layout.Logical_and
  | "IC_XOR_XOR" -> Some Aggregate_layout.Logical_xor
  | "IC_OR_OR" -> Some Aggregate_layout.Logical_or
  | _ -> None

let unsupported description location =
  Aggregate_layout.Unsupported_expression
    { description; origin = origin location }

let dependency dependency_kind detail location =
  Aggregate_layout.Dependency_expression
    { dependency_kind; detail; origin = origin location }

let is_comparison (operator : Frontend.Operator.binary_operator) =
  match operator.ic_name with
  | "IC_LESS"
  | "IC_GREATER"
  | "IC_LESS_EQU"
  | "IC_GREATER_EQU"
  | "IC_EQU_EQU"
  | "IC_NOT_EQU" -> true
  | _ -> false

let is_ungrouped_comparison = function
  | Frontend.Ast.Binary_expression expression ->
      is_comparison expression.binary_operator_spec
  | _ -> false

let literal_expression ~allow_floating description literal =
  let literal_origin = origin literal.Frontend.Ast.literal_location in
  match literal.literal_value with
  | Frontend.Ast.Integer_value value ->
      (* PrsExp.HC:679-685 selects internal U64 when the payload's high bit is set. *)
      if value < 0L then
        Aggregate_layout.Unsigned_integer_expression
          { value; origin = literal_origin }
      else
        Aggregate_layout.Integer_expression { value; origin = literal_origin }
  | Frontend.Ast.Float_value value when allow_floating ->
      Aggregate_layout.Floating_expression { value; origin = literal_origin }
  | Frontend.Ast.Float_value _ | Frontend.Ast.Bytes_value _ ->
      unsupported description literal.literal_location

let rec comparison_chain_location = function
  | Frontend.Ast.Binary_expression expression
    when is_comparison expression.binary_operator_spec
         && (is_ungrouped_comparison expression.binary_left
            || is_ungrouped_comparison expression.binary_right) ->
      Some expression.binary_location
  | Frontend.Ast.Binary_expression expression -> (
      match comparison_chain_location expression.binary_left with
      | Some _ as location -> location
      | None -> comparison_chain_location expression.binary_right)
  | Frontend.Ast.Prefix_expression prefix ->
      comparison_chain_location prefix.prefix_operand
  | Frontend.Ast.Parenthesized_expression grouped ->
      comparison_chain_location grouped.grouped_expression
  | _ -> None

let rec convert_ast ~allow_floating ~queries ast =
  let convert = convert_ast ~allow_floating ~queries in
  match ast with
  | Frontend.Ast.Sizeof_expression _
  | Frontend.Ast.Offset_expression _
  | Frontend.Ast.Defined_expression _
    when List.exists
           (fun query -> Query_selection.expression query == ast)
           queries ->
      Aggregate_layout.Selected_query_expression
        (List.find
           (fun query -> Query_selection.expression query == ast)
           queries)
  | Frontend.Ast.Integer_literal literal ->
      literal_expression ~allow_floating "integer literal" literal
  | Frontend.Ast.Character_literal literal ->
      literal_expression ~allow_floating "character literal" literal
  | Frontend.Ast.Float_literal literal ->
      literal_expression ~allow_floating "floating literal" literal
  | Frontend.Ast.String_literal literal ->
      unsupported "string literal" literal.literal_location
  | Frontend.Ast.Identifier_expression identifier ->
      dependency Aggregate_layout.Identifier_dependency
        (Printf.sprintf "`%s`" identifier.spelling)
        identifier.location
  | Frontend.Ast.Current_position_expression operator ->
      Aggregate_layout.Current_position_expression
        (origin operator.operator_location)
  | Frontend.Ast.Sizeof_expression sizeof ->
      dependency Aggregate_layout.Sizeof_dependency
        (Printf.sprintf "for `%s`" sizeof.sizeof_target.spelling)
        sizeof.sizeof_location
  | Frontend.Ast.Offset_expression offset ->
      let path =
        offset.offset_target.spelling
        :: List.map
             (fun member -> member.Frontend.Ast.offset_member_name.spelling)
             offset.offset_members
        |> String.concat "."
      in
      dependency Aggregate_layout.Offset_dependency
        (Printf.sprintf "for `%s`" path)
        offset.offset_location
  | Frontend.Ast.Defined_expression defined ->
      dependency Aggregate_layout.Defined_dependency
        (Printf.sprintf "for `%s`"
           defined.defined_operand.defined_operand_spelling)
        defined.defined_location
  | Frontend.Ast.Parenthesized_expression grouped ->
      convert grouped.grouped_expression
  | Frontend.Ast.Prefix_expression prefix -> (
      match unary prefix.prefix_operator_kind with
      | Some operator ->
          Aggregate_layout.Unary_expression
            {
              operator;
              operand = convert prefix.prefix_operand;
              origin = origin prefix.prefix_operator.operator_location;
            }
      | None ->
          unsupported
            (Printf.sprintf "prefix operator `%s`"
               prefix.prefix_operator.operator_spelling)
            prefix.prefix_location)
  | Frontend.Ast.Binary_expression expression -> (
      match binary expression.binary_operator_spec with
      | Some operator ->
          Aggregate_layout.Binary_expression
            {
              operator;
              left = convert expression.binary_left;
              right = convert expression.binary_right;
              origin = origin expression.binary_operator.operator_location;
            }
      | None ->
          unsupported
            (Printf.sprintf "operator `%s`"
               expression.binary_operator.operator_spelling)
            expression.binary_location)
  | Frontend.Ast.Call_expression call ->
      dependency Aggregate_layout.Call_dependency "expression"
        call.call_location
  | Frontend.Ast.Postfix_expression postfix ->
      unsupported
        (Printf.sprintf "postfix operator `%s`"
           postfix.postfix_operator.operator_spelling)
        postfix.postfix_location
  | Frontend.Ast.Postfix_cast_expression cast ->
      unsupported "postfix cast" cast.cast_location
  | Frontend.Ast.Index_expression index ->
      unsupported "index expression" index.index_location
  | Frontend.Ast.Member_expression member ->
      unsupported "member expression" member.member_location

let of_ast ?(allow_floating = true) ?(queries = []) ast =
  (* PrsExp/OptPass012 preserve operands for native comparison chains.
     Reject those before evaluation, including inside short-circuit branches. *)
  match comparison_chain_location ast with
  | Some location -> unsupported "unparenthesized chained comparison" location
  | None -> convert_ast ~allow_floating ~queries ast
