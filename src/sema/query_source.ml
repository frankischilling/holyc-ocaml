type role = Sizeof_root | Offset_root | Defined_operand

let origin (location : Frontend.Ast.location) =
  Symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

let rec source_queries expression =
  let open Frontend.Ast in
  match expression with
  | Sizeof_expression _ | Offset_expression _ | Defined_expression _ ->
      [ expression ]
  | Parenthesized_expression grouped ->
      source_queries grouped.grouped_expression
  | Prefix_expression prefix -> source_queries prefix.prefix_operand
  | Postfix_expression postfix -> source_queries postfix.postfix_operand
  | Postfix_cast_expression cast -> source_queries cast.cast_operand
  | Binary_expression binary ->
      source_queries binary.binary_left @ source_queries binary.binary_right
  | Index_expression index ->
      source_queries index.index_base @ source_queries index.index_value
  | Member_expression member -> source_queries member.member_base
  | Call_expression call ->
      source_queries call.call_callee
      @ List.concat_map
          (fun argument ->
            match argument.call_argument_value with
            | Omitted_call_argument -> []
            | Provided_call_argument value -> source_queries value)
          call.call_arguments
  | Integer_literal _
  | Float_literal _
  | Character_literal _
  | String_literal _
  | Current_position_expression _
  | Identifier_expression _ -> []

let name_query_facts = function
  | Frontend.Ast.Sizeof_expression expression ->
      Some
        ( Sizeof_root,
          expression.sizeof_target.spelling,
          origin expression.sizeof_target.location )
  | Frontend.Ast.Offset_expression expression ->
      Some
        ( Offset_root,
          expression.offset_target.spelling,
          origin expression.offset_target.location )
  | Frontend.Ast.Defined_expression expression
    when expression.defined_operand.defined_operand_kind
         = Frontend.Ast.Defined_name ->
      let operand = expression.defined_operand in
      Some
        ( Defined_operand,
          operand.defined_operand_spelling,
          origin operand.defined_operand_location )
  | _ -> None
