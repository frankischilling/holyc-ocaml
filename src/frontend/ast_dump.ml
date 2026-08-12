let schema = "holyc-ast-v1"

let source_position sources span offset =
  match Common.Source_manager.find sources span.Common.Span.source with
  | None -> ("<unknown>", 1, 1)
  | Some source ->
      let position =
        Common.Source_file.position source offset
        |> Result.value
             ~default:{ Common.Source_file.offset = 0; line = 1; column = 1 }
      in
      (Common.Source_file.display_path source, position.line, position.column)

let span_text sources span =
  let path, start_line, start_column =
    source_position sources span span.Common.Span.start
  in
  let _, stop_line, stop_column =
    source_position sources span span.Common.Span.stop
  in
  Printf.sprintf "%s:%d:%d..%d:%d" path start_line start_column stop_line
    stop_column

let segments_are_covered (location : Ast.location) =
  List.for_all
    (fun segment ->
      Common.Source_id.equal segment.Common.Span.source location.Ast.span.source
      && location.span.start <= segment.start
      && segment.stop <= location.span.stop)
    location.source_segments

let location_text sources (location : Ast.location) =
  let primary = span_text sources location.Ast.span in
  let segments =
    if segments_are_covered location then ""
    else
      Printf.sprintf " segments=[%s]"
        (location.source_segments
        |> List.map (span_text sources)
        |> String.concat ",")
  in
  let generated_from =
    match location.generated_from with
    | None -> ""
    | Some span -> Printf.sprintf " generated_from=%s" (span_text sources span)
  in
  let defined_at =
    match location.defined_at with
    | None -> ""
    | Some span -> Printf.sprintf " defined_at=%s" (span_text sources span)
  in
  primary ^ segments ^ generated_from ^ defined_at

let delimiter_kind_name = function
  | Ast.Comma -> "comma"
  | Ast.Semicolon -> "semicolon"

let modifier_kind_name = function
  | Ast.Public -> "public"
  | Ast.Static -> "static"
  | Ast.Interrupt -> "interrupt"
  | Ast.Has_error_code -> "haserrcode"
  | Ast.Argument_pop -> "argpop"
  | Ast.No_argument_pop -> "noargpop"

let register_qualifier_kind_name = function
  | Ast.Reg -> "reg"
  | Ast.Noreg -> "noreg"

let register_qualifier_position_name = function
  | Ast.Before_type -> "before_type"
  | Ast.After_type -> "after_type"

let unary_operator_kind_name = function
  | Ast.Unary_plus -> "unary_plus"
  | Ast.Unary_minus -> "unary_minus"
  | Ast.Logical_not -> "logical_not"
  | Ast.Bitwise_not -> "bitwise_not"
  | Ast.Dereference -> "dereference"
  | Ast.Address_of -> "address_of"
  | Ast.Pre_increment -> "pre_increment"
  | Ast.Pre_decrement -> "pre_decrement"

let postfix_operator_kind_name = function
  | Ast.Post_increment -> "post_increment"
  | Ast.Post_decrement -> "post_decrement"

let member_access_kind_name = function
  | Ast.Direct_member -> "direct"
  | Ast.Pointer_member -> "pointer"

let defined_operand_kind_name = function
  | Ast.Defined_name -> "name"
  | Ast.Defined_non_name -> "non_name"

let implicit_output_target_name = function
  | Ast.Print_target -> "print"
  | Ast.Put_chars_target -> "put_chars"

let local_storage_name = function
  | Ast.Automatic_local -> "automatic"
  | Ast.Static_local -> "static"

let association_name = function
  | Operator.Unspecified -> "unspecified"
  | Operator.Left -> "left"
  | Operator.Right -> "right"

let escaped_bytes bytes =
  let buffer = Buffer.create (String.length bytes) in
  String.iter
    (fun byte ->
      let code = Char.code byte in
      if code >= 0x20 && code <= 0x7e && not (Char.equal byte '\\') then
        Buffer.add_char buffer byte
      else Printf.bprintf buffer "\\x%02x" code)
    bytes;
  Buffer.contents buffer

let literal_value_text = function
  | Ast.Integer_value value -> Printf.sprintf "0x%016Lx" value
  | Ast.Float_value value -> Printf.sprintf "%.17g" value
  | Ast.Bytes_value value -> escaped_bytes value

let binding_kind_name = function
  | Ast.Extern -> "extern"
  | Ast.Import -> "import"
  | Ast.Intern -> "intern"

let aggregate_kind_name = function
  | Ast.Class_aggregate -> "class"
  | Ast.Union_aggregate -> "union"

let print_modifiers buffer sources ~indent modifiers =
  List.iter
    (fun (modifier : Ast.declaration_modifier) ->
      Printf.bprintf buffer "%smodifier kind=%s spelling=%S span=%s\n" indent
        (modifier_kind_name modifier.Ast.kind)
        modifier.spelling
        (location_text sources modifier.location))
    modifiers

let print_register_qualifiers buffer sources ~indent ~position qualifiers =
  List.iter
    (fun (qualifier : Ast.register_qualifier) ->
      if qualifier.position = position then (
        Printf.bprintf buffer
          "%sregister_qualifier kind=%s position=%s spelling=%S span=%s\n"
          indent
          (register_qualifier_kind_name qualifier.kind)
          (register_qualifier_position_name qualifier.position)
          qualifier.spelling
          (location_text sources qualifier.location);
        Option.iter
          (fun (register : Ast.identifier) ->
            Printf.bprintf buffer "%s  explicit_register spelling=%S span=%s\n"
              indent register.spelling
              (location_text sources register.location))
          qualifier.explicit_register))
    qualifiers

let print_type buffer sources ~indent = function
  | Ast.Primitive_type_specifier primitive ->
      Printf.bprintf buffer "%stype primitive=%s spelling=%S span=%s\n" indent
        (Sema.Primitive_type.to_string primitive.Ast.primitive)
        primitive.spelling
        (location_text sources primitive.location)
  | Ast.Named_type_specifier name ->
      Printf.bprintf buffer "%stype named spelling=%S span=%s\n" indent
        name.Ast.spelling
        (location_text sources name.location)

let print_return_type buffer sources ~indent = function
  | Ast.Primitive_type_specifier primitive ->
      Printf.bprintf buffer "%sreturn_type primitive=%s spelling=%S span=%s\n"
        indent
        (Sema.Primitive_type.to_string primitive.Ast.primitive)
        primitive.spelling
        (location_text sources primitive.location)
  | Ast.Named_type_specifier name ->
      Printf.bprintf buffer "%sreturn_type named spelling=%S span=%s\n" indent
        name.Ast.spelling
        (location_text sources name.location)

let print_pointer_layers buffer sources ~indent pointer_layers =
  List.iter
    (fun pointer ->
      Printf.bprintf buffer "%spointer_layer depth=%d spelling=%S span=%s\n"
        indent pointer.Ast.depth pointer.spelling
        (location_text sources pointer.location))
    pointer_layers

let print_parameter_name buffer sources ~indent = function
  | Some (name : Ast.identifier) ->
      Printf.bprintf buffer "%sname spelling=%S span=%s\n" indent name.spelling
        (location_text sources name.location)
  | None -> Printf.bprintf buffer "%sname omitted\n" indent

let rec print_expression buffer sources ~indent expression =
  let child_indent = indent ^ "  " in
  match expression with
  | Ast.Integer_literal literal ->
      print_literal buffer sources ~indent ~kind:"integer_literal" literal
  | Ast.Float_literal literal ->
      print_literal buffer sources ~indent ~kind:"float_literal" literal
  | Ast.Character_literal literal ->
      print_literal buffer sources ~indent ~kind:"character_literal" literal
  | Ast.String_literal literal ->
      print_literal buffer sources ~indent ~kind:"string_literal" literal
  | Ast.Identifier_expression identifier ->
      Printf.bprintf buffer "%sexpression kind=identifier spelling=%S span=%s\n"
        indent identifier.spelling
        (location_text sources identifier.location)
  | Ast.Current_position_expression operator ->
      Printf.bprintf buffer
        "%sexpression kind=current_position spelling=%S span=%s\n" indent
        operator.operator_spelling
        (location_text sources operator.operator_location)
  | Ast.Sizeof_expression sizeof_expression ->
      let wrapper_count =
        List.length sizeof_expression.sizeof_opening_parentheses
      in
      Printf.bprintf buffer
        "%sexpression kind=sizeof wrappers=%d members=%d pointer_layers=%d \
         span=%s\n"
        indent wrapper_count
        (List.length sizeof_expression.sizeof_members)
        (List.length sizeof_expression.sizeof_pointer_layers)
        (location_text sources sizeof_expression.sizeof_location);
      Printf.bprintf buffer "%skeyword spelling=%S span=%s\n" child_indent
        sizeof_expression.sizeof_keyword_spelling
        (location_text sources sizeof_expression.sizeof_keyword_location);
      List.iteri
        (fun index location ->
          Printf.bprintf buffer "%sopening_parenthesis depth=%d span=%s\n"
            child_indent (index + 1)
            (location_text sources location))
        sizeof_expression.sizeof_opening_parentheses;
      Printf.bprintf buffer "%starget spelling=%S span=%s\n" child_indent
        sizeof_expression.sizeof_target.spelling
        (location_text sources sizeof_expression.sizeof_target.location);
      List.iter
        (fun (member : Ast.sizeof_member) ->
          Printf.bprintf buffer "%smember span=%s\n" child_indent
            (location_text sources member.sizeof_member_location);
          Printf.bprintf buffer "%s  dot span=%s\n" child_indent
            (location_text sources member.sizeof_member_dot);
          Printf.bprintf buffer "%s  name spelling=%S span=%s\n" child_indent
            member.sizeof_member_name.spelling
            (location_text sources member.sizeof_member_name.location))
        sizeof_expression.sizeof_members;
      print_pointer_layers buffer sources ~indent:child_indent
        sizeof_expression.sizeof_pointer_layers;
      List.iteri
        (fun index location ->
          Printf.bprintf buffer "%sclosing_parenthesis depth=%d span=%s\n"
            child_indent (wrapper_count - index)
            (location_text sources location))
        sizeof_expression.sizeof_closing_parentheses
  | Ast.Offset_expression offset_expression ->
      let wrapper_count =
        List.length offset_expression.offset_opening_parentheses
      in
      Printf.bprintf buffer
        "%sexpression kind=offset wrappers=%d members=%d span=%s\n" indent
        wrapper_count
        (List.length offset_expression.offset_members)
        (location_text sources offset_expression.offset_location);
      Printf.bprintf buffer "%skeyword spelling=%S span=%s\n" child_indent
        offset_expression.offset_keyword_spelling
        (location_text sources offset_expression.offset_keyword_location);
      List.iteri
        (fun index location ->
          Printf.bprintf buffer "%sopening_parenthesis depth=%d span=%s\n"
            child_indent (index + 1)
            (location_text sources location))
        offset_expression.offset_opening_parentheses;
      Printf.bprintf buffer "%starget spelling=%S span=%s\n" child_indent
        offset_expression.offset_target.spelling
        (location_text sources offset_expression.offset_target.location);
      List.iter
        (fun (member : Ast.offset_member) ->
          Printf.bprintf buffer "%smember span=%s\n" child_indent
            (location_text sources member.offset_member_location);
          Printf.bprintf buffer "%s  dot span=%s\n" child_indent
            (location_text sources member.offset_member_dot);
          Printf.bprintf buffer "%s  name spelling=%S span=%s\n" child_indent
            member.offset_member_name.spelling
            (location_text sources member.offset_member_name.location))
        offset_expression.offset_members;
      List.iteri
        (fun index location ->
          Printf.bprintf buffer "%sclosing_parenthesis depth=%d span=%s\n"
            child_indent (wrapper_count - index)
            (location_text sources location))
        offset_expression.offset_closing_parentheses
  | Ast.Defined_expression defined_expression ->
      let wrapper_count =
        List.length defined_expression.defined_opening_parentheses
      in
      Printf.bprintf buffer "%sexpression kind=defined wrappers=%d span=%s\n"
        indent wrapper_count
        (location_text sources defined_expression.defined_location);
      Printf.bprintf buffer "%skeyword spelling=%S span=%s\n" child_indent
        defined_expression.defined_keyword_spelling
        (location_text sources defined_expression.defined_keyword_location);
      List.iteri
        (fun index location ->
          Printf.bprintf buffer "%sopening_parenthesis depth=%d span=%s\n"
            child_indent (index + 1)
            (location_text sources location))
        defined_expression.defined_opening_parentheses;
      Printf.bprintf buffer "%soperand kind=%s spelling=%S span=%s\n"
        child_indent
        (defined_operand_kind_name
           defined_expression.defined_operand.defined_operand_kind)
        defined_expression.defined_operand.defined_operand_spelling
        (location_text sources
           defined_expression.defined_operand.defined_operand_location);
      List.iteri
        (fun index location ->
          Printf.bprintf buffer "%sclosing_parenthesis depth=%d span=%s\n"
            child_indent (wrapper_count - index)
            (location_text sources location))
        defined_expression.defined_closing_parentheses
  | Ast.Parenthesized_expression grouped ->
      Printf.bprintf buffer "%sexpression kind=parenthesized span=%s\n" indent
        (location_text sources grouped.parenthesized_location);
      Printf.bprintf buffer "%sopening_parenthesis span=%s\n" child_indent
        (location_text sources grouped.opening_parenthesis);
      print_expression buffer sources ~indent:child_indent
        grouped.grouped_expression;
      Printf.bprintf buffer "%sclosing_parenthesis span=%s\n" child_indent
        (location_text sources grouped.closing_parenthesis)
  | Ast.Prefix_expression prefix ->
      Printf.bprintf buffer
        "%sexpression kind=prefix operator_kind=%s span=%s\n" indent
        (unary_operator_kind_name prefix.prefix_operator_kind)
        (location_text sources prefix.prefix_location);
      print_expression_operator buffer sources ~indent:child_indent
        prefix.prefix_operator;
      print_expression buffer sources ~indent:child_indent prefix.prefix_operand
  | Ast.Postfix_expression postfix ->
      Printf.bprintf buffer
        "%sexpression kind=postfix operator_kind=%s span=%s\n" indent
        (postfix_operator_kind_name postfix.postfix_operator_kind)
        (location_text sources postfix.postfix_location);
      print_expression buffer sources ~indent:child_indent
        postfix.postfix_operand;
      print_expression_operator buffer sources ~indent:child_indent
        postfix.postfix_operator
  | Ast.Postfix_cast_expression cast ->
      Printf.bprintf buffer "%sexpression kind=postfix_cast span=%s\n" indent
        (location_text sources cast.cast_location);
      Printf.bprintf buffer "%soperand\n" child_indent;
      print_expression buffer sources ~indent:(child_indent ^ "  ")
        cast.cast_operand;
      Printf.bprintf buffer "%sopening_parenthesis span=%s\n" child_indent
        (location_text sources cast.cast_opening_parenthesis);
      Printf.bprintf buffer "%starget\n" child_indent;
      print_type buffer sources ~indent:(child_indent ^ "  ") cast.cast_type;
      print_pointer_layers buffer sources ~indent:(child_indent ^ "  ")
        cast.cast_pointer_layers;
      Printf.bprintf buffer "%sclosing_parenthesis span=%s\n" child_indent
        (location_text sources cast.cast_closing_parenthesis)
  | Ast.Binary_expression binary ->
      Printf.bprintf buffer
        "%sexpression kind=binary precedence=%s precedence_value=0x%02x \
         association=%s ic=%s source_line=%d span=%s\n"
        indent binary.binary_operator_spec.precedence_name
        binary.binary_operator_spec.precedence_value
        (association_name binary.binary_operator_spec.association)
        binary.binary_operator_spec.ic_name
        binary.binary_operator_spec.source_line
        (location_text sources binary.binary_location);
      Printf.bprintf buffer "%sleft\n" child_indent;
      print_expression buffer sources ~indent:(child_indent ^ "  ")
        binary.binary_left;
      print_expression_operator buffer sources ~indent:child_indent
        binary.binary_operator;
      Printf.bprintf buffer "%sright\n" child_indent;
      print_expression buffer sources ~indent:(child_indent ^ "  ")
        binary.binary_right
  | Ast.Call_expression call -> (
      (match call.call_syntax with
      | Ast.Parenthesized_call _ ->
          Printf.bprintf buffer "%sexpression kind=call arguments=%d span=%s\n"
            indent
            (List.length call.call_arguments)
            (location_text sources call.call_location)
      | Ast.Parenthesis_free_call ->
          Printf.bprintf buffer
            "%sexpression kind=call syntax=parenthesis-free arguments=%d span=%s\n"
            indent
            (List.length call.call_arguments)
            (location_text sources call.call_location));
      Printf.bprintf buffer "%scallee\n" child_indent;
      print_expression buffer sources ~indent:(child_indent ^ "  ")
        call.call_callee;
      (match call.call_syntax with
      | Ast.Parenthesized_call { opening_parenthesis; _ } ->
          Printf.bprintf buffer "%sopening_parenthesis span=%s\n" child_indent
            (location_text sources opening_parenthesis)
      | Ast.Parenthesis_free_call -> ());
      List.iteri
        (print_call_argument buffer sources ~indent:child_indent)
        call.call_arguments;
      match call.call_syntax with
      | Ast.Parenthesized_call { closing_parenthesis; _ } ->
          Printf.bprintf buffer "%sclosing_parenthesis span=%s\n" child_indent
            (location_text sources closing_parenthesis)
      | Ast.Parenthesis_free_call -> ())
  | Ast.Index_expression index ->
      Printf.bprintf buffer "%sexpression kind=index span=%s\n" indent
        (location_text sources index.index_location);
      Printf.bprintf buffer "%sbase\n" child_indent;
      print_expression buffer sources ~indent:(child_indent ^ "  ")
        index.index_base;
      Printf.bprintf buffer "%sopening_bracket span=%s\n" child_indent
        (location_text sources index.index_opening_bracket);
      Printf.bprintf buffer "%sindex\n" child_indent;
      print_expression buffer sources ~indent:(child_indent ^ "  ")
        index.index_value;
      Printf.bprintf buffer "%sclosing_bracket span=%s\n" child_indent
        (location_text sources index.index_closing_bracket)
  | Ast.Member_expression member ->
      Printf.bprintf buffer "%sexpression kind=member access_kind=%s span=%s\n"
        indent
        (member_access_kind_name member.member_access_kind)
        (location_text sources member.member_location);
      Printf.bprintf buffer "%sbase\n" child_indent;
      print_expression buffer sources ~indent:(child_indent ^ "  ")
        member.member_base;
      print_expression_operator buffer sources ~indent:child_indent
        member.member_operator;
      Printf.bprintf buffer "%smember spelling=%S span=%s\n" child_indent
        member.member_name.spelling
        (location_text sources member.member_name.location)

and print_call_argument buffer sources ~indent index
    (argument : Ast.call_argument) =
  let kind =
    match argument.call_argument_value with
    | Ast.Omitted_call_argument -> "omitted"
    | Ast.Provided_call_argument _ -> "provided"
  in
  Printf.bprintf buffer "%sargument index=%d kind=%s span=%s\n" indent index
    kind
    (location_text sources argument.call_argument_location);
  (match argument.call_argument_value with
  | Ast.Omitted_call_argument -> ()
  | Ast.Provided_call_argument expression ->
      print_expression buffer sources ~indent:(indent ^ "  ") expression);
  Option.iter
    (fun comma ->
      Printf.bprintf buffer "%s  comma span=%s\n" indent
        (location_text sources comma))
    argument.following_comma

and print_literal buffer sources ~indent ~kind
    (literal : Ast.expression_literal) =
  Printf.bprintf buffer "%sexpression kind=%s spelling=%S value=%S span=%s\n"
    indent kind literal.literal_spelling
    (literal_value_text literal.literal_value)
    (location_text sources literal.literal_location)

and print_expression_operator buffer sources ~indent
    (operator : Ast.expression_operator) =
  Printf.bprintf buffer "%soperator spelling=%S span=%s\n" indent
    operator.operator_spelling
    (location_text sources operator.operator_location)

let rec print_statement buffer sources ~indent = function
  | Ast.Block_statement statement ->
      let child_indent = indent ^ "  " in
      Printf.bprintf buffer "%sblock_statement span=%s statements=%d\n" indent
        (location_text sources statement.block_location)
        (List.length statement.block_statements);
      Printf.bprintf buffer "%sopening_brace span=%s\n" child_indent
        (location_text sources statement.block_opening_brace);
      List.iteri
        (fun index child ->
          Printf.bprintf buffer "%sstatement index=%d\n" child_indent index;
          print_statement buffer sources ~indent:(child_indent ^ "  ") child)
        statement.block_statements;
      Printf.bprintf buffer "%sclosing_brace span=%s\n" child_indent
        (location_text sources statement.block_closing_brace)
  | Ast.Break_statement statement ->
      Printf.bprintf buffer "%sbreak_statement span=%s semicolon=%b\n" indent
        (location_text sources statement.break_location)
        (Option.is_some statement.break_semicolon);
      Printf.bprintf buffer "%s  keyword span=%s\n" indent
        (location_text sources statement.break_keyword);
      Option.iter
        (fun semicolon ->
          Printf.bprintf buffer "%s  semicolon span=%s\n" indent
            (location_text sources semicolon))
        statement.break_semicolon
  | Ast.Do_while_statement statement ->
      let child_indent = indent ^ "  " in
      Printf.bprintf buffer "%sdo_while_statement span=%s\n" indent
        (location_text sources statement.do_while_location);
      Printf.bprintf buffer "%sdo_keyword span=%s\n" child_indent
        (location_text sources statement.do_keyword);
      Printf.bprintf buffer "%sbody\n" child_indent;
      print_statement buffer sources ~indent:(child_indent ^ "  ")
        statement.do_body;
      Printf.bprintf buffer "%swhile_keyword span=%s\n" child_indent
        (location_text sources statement.do_while_keyword);
      Printf.bprintf buffer "%sopening_parenthesis span=%s\n" child_indent
        (location_text sources statement.do_while_opening_parenthesis);
      Printf.bprintf buffer "%scondition\n" child_indent;
      print_expression buffer sources ~indent:(child_indent ^ "  ")
        statement.do_while_condition;
      Printf.bprintf buffer "%sclosing_parenthesis span=%s\n" child_indent
        (location_text sources statement.do_while_closing_parenthesis);
      Printf.bprintf buffer "%ssemicolon span=%s\n" child_indent
        (location_text sources statement.do_while_semicolon)
  | Ast.Empty_statement statement ->
      Printf.bprintf buffer "%sempty_statement span=%s\n" indent
        (location_text sources statement.empty_statement_location);
      Printf.bprintf buffer "%s  semicolon span=%s\n" indent
        (location_text sources statement.empty_statement_semicolon)
  | Ast.Expression_statement statement ->
      Printf.bprintf buffer "%sexpression_statement span=%s semicolon=%b\n"
        indent
        (location_text sources statement.expression_statement_location)
        (Option.is_some statement.expression_statement_semicolon);
      print_expression buffer sources ~indent:(indent ^ "  ")
        statement.expression_statement_expression;
      Option.iter
        (fun semicolon ->
          Printf.bprintf buffer "%s  semicolon span=%s\n" indent
            (location_text sources semicolon))
        statement.expression_statement_semicolon
  | Ast.For_statement statement ->
      let child_indent = indent ^ "  " in
      Printf.bprintf buffer "%sfor_statement span=%s update=%b\n" indent
        (location_text sources statement.for_location)
        (Option.is_some statement.for_update);
      Printf.bprintf buffer "%skeyword span=%s\n" child_indent
        (location_text sources statement.for_keyword);
      Printf.bprintf buffer "%sopening_parenthesis span=%s\n" child_indent
        (location_text sources statement.for_opening_parenthesis);
      Printf.bprintf buffer "%sinitializer\n" child_indent;
      print_statement buffer sources ~indent:(child_indent ^ "  ")
        statement.for_initializer;
      Printf.bprintf buffer "%scondition\n" child_indent;
      print_expression buffer sources ~indent:(child_indent ^ "  ")
        statement.for_condition;
      Printf.bprintf buffer "%scondition_semicolon span=%s\n" child_indent
        (location_text sources statement.for_condition_semicolon);
      Option.iter
        (fun update ->
          Printf.bprintf buffer "%supdate\n" child_indent;
          print_statement buffer sources ~indent:(child_indent ^ "  ") update)
        statement.for_update;
      Printf.bprintf buffer "%sclosing_parenthesis span=%s\n" child_indent
        (location_text sources statement.for_closing_parenthesis);
      Printf.bprintf buffer "%sbody\n" child_indent;
      print_statement buffer sources ~indent:(child_indent ^ "  ")
        statement.for_body
  | Ast.Goto_statement statement ->
      let child_indent = indent ^ "  " in
      Printf.bprintf buffer "%sgoto_statement span=%s target=%S semicolon=%b\n"
        indent
        (location_text sources statement.goto_location)
        statement.goto_target.spelling
        (Option.is_some statement.goto_semicolon);
      Printf.bprintf buffer "%skeyword span=%s\n" child_indent
        (location_text sources statement.goto_keyword);
      Printf.bprintf buffer "%starget spelling=%S span=%s\n" child_indent
        statement.goto_target.spelling
        (location_text sources statement.goto_target.location);
      Option.iter
        (fun semicolon ->
          Printf.bprintf buffer "%ssemicolon span=%s\n" child_indent
            (location_text sources semicolon))
        statement.goto_semicolon
  | Ast.If_statement statement ->
      let child_indent = indent ^ "  " in
      Printf.bprintf buffer "%sif_statement span=%s else=%b\n" indent
        (location_text sources statement.if_location)
        (Option.is_some statement.if_else_clause);
      Printf.bprintf buffer "%skeyword span=%s\n" child_indent
        (location_text sources statement.if_keyword);
      Printf.bprintf buffer "%sopening_parenthesis span=%s\n" child_indent
        (location_text sources statement.if_opening_parenthesis);
      Printf.bprintf buffer "%scondition\n" child_indent;
      print_expression buffer sources ~indent:(child_indent ^ "  ")
        statement.if_condition;
      Printf.bprintf buffer "%sclosing_parenthesis span=%s\n" child_indent
        (location_text sources statement.if_closing_parenthesis);
      Printf.bprintf buffer "%sthen_branch\n" child_indent;
      print_statement buffer sources ~indent:(child_indent ^ "  ")
        statement.if_then_branch;
      Option.iter
        (fun (else_clause : Ast.else_clause) ->
          Printf.bprintf buffer "%selse_clause span=%s\n" child_indent
            (location_text sources else_clause.else_location);
          Printf.bprintf buffer "%s  keyword span=%s\n" child_indent
            (location_text sources else_clause.else_keyword);
          Printf.bprintf buffer "%s  branch\n" child_indent;
          print_statement buffer sources ~indent:(child_indent ^ "    ")
            else_clause.else_branch)
        statement.if_else_clause
  | Ast.While_statement statement ->
      let child_indent = indent ^ "  " in
      Printf.bprintf buffer "%swhile_statement span=%s\n" indent
        (location_text sources statement.while_location);
      Printf.bprintf buffer "%skeyword span=%s\n" child_indent
        (location_text sources statement.while_keyword);
      Printf.bprintf buffer "%sopening_parenthesis span=%s\n" child_indent
        (location_text sources statement.while_opening_parenthesis);
      Printf.bprintf buffer "%scondition\n" child_indent;
      print_expression buffer sources ~indent:(child_indent ^ "  ")
        statement.while_condition;
      Printf.bprintf buffer "%sclosing_parenthesis span=%s\n" child_indent
        (location_text sources statement.while_closing_parenthesis);
      Printf.bprintf buffer "%sbody\n" child_indent;
      print_statement buffer sources ~indent:(child_indent ^ "  ")
        statement.while_body
  | Ast.Implicit_output_statement statement ->
      print_implicit_output_statement buffer sources ~indent statement
  | Ast.Label_statement statement ->
      let child_indent = indent ^ "  " in
      Printf.bprintf buffer "%slabel_statement span=%s name=%S\n" indent
        (location_text sources statement.label_location)
        statement.label_name.spelling;
      Printf.bprintf buffer "%sname spelling=%S span=%s\n" child_indent
        statement.label_name.spelling
        (location_text sources statement.label_name.location);
      Printf.bprintf buffer "%scolon span=%s\n" child_indent
        (location_text sources statement.label_colon)
  | Ast.Local_declaration_statement declaration ->
      let child_indent = indent ^ "  " in
      Printf.bprintf buffer
        "%slocal_declaration span=%s storage=%s declarators=%d\n" indent
        (location_text sources declaration.local_declaration_location)
        (local_storage_name declaration.local_storage)
        (List.length declaration.local_declarators);
      print_modifiers buffer sources ~indent:child_indent
        declaration.local_modifiers;
      print_type buffer sources ~indent:child_indent
        declaration.local_type_specifier;
      List.iteri
        (fun index (declarator : Ast.local_declarator) ->
          let declarator_indent = child_indent ^ "  " in
          Printf.bprintf buffer "%sdeclarator index=%d span=%s\n" child_indent
            index
            (location_text sources declarator.local_declarator_location);
          print_register_qualifiers buffer sources ~indent:declarator_indent
            ~position:Ast.After_type declarator.local_register_qualifiers;
          print_pointer_layers buffer sources ~indent:declarator_indent
            declarator.local_pointer_layers;
          Printf.bprintf buffer "%sname spelling=%S span=%s\n" declarator_indent
            declarator.local_name.spelling
            (location_text sources declarator.local_name.location);
          List.iteri
            (fun dimension_index (dimension : Ast.array_dimension) ->
              let dimension_indent = declarator_indent ^ "  " in
              Printf.bprintf buffer
                "%sarray_dimension index=%d sized=%b span=%s\n"
                declarator_indent dimension_index
                (Option.is_some dimension.dimension_expression)
                (location_text sources dimension.location);
              Printf.bprintf buffer "%sopening_bracket span=%s\n"
                dimension_indent
                (location_text sources dimension.opening_bracket);
              (match dimension.dimension_expression with
              | None ->
                  Printf.bprintf buffer "%sexpression omitted\n"
                    dimension_indent
              | Some expression ->
                  print_expression buffer sources ~indent:dimension_indent
                    expression);
              Printf.bprintf buffer "%sclosing_bracket span=%s\n"
                dimension_indent
                (location_text sources dimension.closing_bracket))
            declarator.local_array_dimensions;
          Option.iter
            (fun (initial_value : Ast.local_initializer) ->
              Printf.bprintf buffer "%sinitializer span=%s\n" declarator_indent
                (location_text sources initial_value.local_initializer_location);
              Printf.bprintf buffer "%s  equals span=%s\n" declarator_indent
                (location_text sources initial_value.local_initializer_equals);
              print_expression buffer sources ~indent:(declarator_indent ^ "  ")
                initial_value.local_initializer_value)
            declarator.local_initializer;
          Printf.bprintf buffer "%sdelimiter kind=%s spelling=%S span=%s\n"
            declarator_indent
            (delimiter_kind_name declarator.local_delimiter.kind)
            declarator.local_delimiter.spelling
            (location_text sources declarator.local_delimiter.location))
        declaration.local_declarators
  | Ast.Lock_statement statement ->
      let child_indent = indent ^ "  " in
      Printf.bprintf buffer "%slock_statement span=%s\n" indent
        (location_text sources statement.lock_location);
      Printf.bprintf buffer "%skeyword span=%s\n" child_indent
        (location_text sources statement.lock_keyword);
      Printf.bprintf buffer "%sbody\n" child_indent;
      print_statement buffer sources ~indent:(child_indent ^ "  ")
        statement.lock_body
  | Ast.Switch_statement statement ->
      let child_indent = indent ^ "  " in
      let mode =
        match statement.switch_mode with
        | Ast.Bounded_switch -> "bounded"
        | Ast.No_bound_switch -> "no_bound"
      in
      Printf.bprintf buffer "%sswitch_statement span=%s mode=%s elements=%d\n"
        indent
        (location_text sources statement.switch_location)
        mode
        (List.length statement.switch_elements);
      Printf.bprintf buffer "%skeyword span=%s\n" child_indent
        (location_text sources statement.switch_keyword);
      Printf.bprintf buffer "%sopening_delimiter span=%s\n" child_indent
        (location_text sources statement.switch_opening_delimiter);
      Printf.bprintf buffer "%sexpression\n" child_indent;
      print_expression buffer sources ~indent:(child_indent ^ "  ")
        statement.switch_expression;
      Printf.bprintf buffer "%sclosing_delimiter span=%s\n" child_indent
        (location_text sources statement.switch_closing_delimiter);
      Printf.bprintf buffer "%sopening_brace span=%s\n" child_indent
        (location_text sources statement.switch_opening_brace);
      List.iteri
        (fun index element ->
          Printf.bprintf buffer "%selement index=%d\n" child_indent index;
          print_switch_element buffer sources ~indent:(child_indent ^ "  ")
            element)
        statement.switch_elements;
      Printf.bprintf buffer "%sclosing_brace span=%s\n" child_indent
        (location_text sources statement.switch_closing_brace)
  | Ast.Try_catch_statement statement ->
      let child_indent = indent ^ "  " in
      Printf.bprintf buffer "%stry_catch_statement span=%s\n" indent
        (location_text sources statement.try_catch_location);
      Printf.bprintf buffer "%stry_keyword span=%s\n" child_indent
        (location_text sources statement.try_keyword);
      Printf.bprintf buffer "%stry_body\n" child_indent;
      print_statement buffer sources ~indent:(child_indent ^ "  ")
        statement.try_body;
      Printf.bprintf buffer "%scatch_keyword span=%s\n" child_indent
        (location_text sources statement.catch_keyword);
      Printf.bprintf buffer "%scatch_body\n" child_indent;
      print_statement buffer sources ~indent:(child_indent ^ "  ")
        statement.catch_body
  | Ast.Return_statement statement ->
      let child_indent = indent ^ "  " in
      Printf.bprintf buffer "%sreturn_statement span=%s value=%b semicolon=%b\n"
        indent
        (location_text sources statement.return_location)
        (Option.is_some statement.return_value)
        (Option.is_some statement.return_semicolon);
      Printf.bprintf buffer "%skeyword span=%s\n" child_indent
        (location_text sources statement.return_keyword);
      Option.iter
        (fun value ->
          Printf.bprintf buffer "%svalue\n" child_indent;
          print_expression buffer sources ~indent:(child_indent ^ "  ") value)
        statement.return_value;
      Option.iter
        (fun semicolon ->
          Printf.bprintf buffer "%ssemicolon span=%s\n" child_indent
            (location_text sources semicolon))
        statement.return_semicolon
  | Ast.Sequence_statement sequence ->
      let child_indent = indent ^ "  " in
      Printf.bprintf buffer
        "%sstatement_sequence span=%s leading_commas=%d elements=%d\n" indent
        (location_text sources sequence.sequence_location)
        (List.length sequence.sequence_leading_commas)
        (List.length sequence.sequence_elements);
      List.iteri
        (fun index comma ->
          Printf.bprintf buffer "%sleading_comma index=%d span=%s\n"
            child_indent index
            (location_text sources comma))
        sequence.sequence_leading_commas;
      List.iteri
        (fun index (element : Ast.statement_sequence_element) ->
          Printf.bprintf buffer
            "%selement index=%d span=%s following_commas=%d\n" child_indent
            index
            (location_text sources element.sequence_element_location)
            (List.length element.sequence_following_commas);
          print_statement buffer sources ~indent:(child_indent ^ "  ")
            element.sequence_statement;
          List.iteri
            (fun comma_index comma ->
              Printf.bprintf buffer "%s  following_comma index=%d span=%s\n"
                child_indent comma_index
                (location_text sources comma))
            element.sequence_following_commas)
        sequence.sequence_elements

and print_switch_element buffer sources ~indent = function
  | Ast.Switch_case_element case_label ->
      let child_indent = indent ^ "  " in
      let pattern =
        match case_label.switch_case_pattern with
        | Ast.Implicit_case -> "implicit"
        | Ast.Single_case _ -> "single"
        | Ast.Ranged_case _ -> "range"
      in
      Printf.bprintf buffer "%scase_label span=%s pattern=%s\n" indent
        (location_text sources case_label.switch_case_location)
        pattern;
      Printf.bprintf buffer "%skeyword span=%s\n" child_indent
        (location_text sources case_label.switch_case_keyword);
      (match case_label.switch_case_pattern with
      | Ast.Implicit_case -> ()
      | Ast.Single_case expression ->
          Printf.bprintf buffer "%svalue\n" child_indent;
          print_expression buffer sources ~indent:(child_indent ^ "  ")
            expression
      | Ast.Ranged_case range ->
          Printf.bprintf buffer "%srange span=%s\n" child_indent
            (location_text sources range.case_range_location);
          Printf.bprintf buffer "%s  start\n" child_indent;
          print_expression buffer sources ~indent:(child_indent ^ "    ")
            range.case_range_start;
          Printf.bprintf buffer "%s  ellipsis span=%s\n" child_indent
            (location_text sources range.case_range_ellipsis);
          Printf.bprintf buffer "%s  end\n" child_indent;
          print_expression buffer sources ~indent:(child_indent ^ "    ")
            range.case_range_end);
      Printf.bprintf buffer "%scolon span=%s\n" child_indent
        (location_text sources case_label.switch_case_colon)
  | Ast.Switch_default_element default_label ->
      let child_indent = indent ^ "  " in
      Printf.bprintf buffer "%sdefault_label span=%s\n" indent
        (location_text sources default_label.switch_default_location);
      Printf.bprintf buffer "%skeyword span=%s\n" child_indent
        (location_text sources default_label.switch_default_keyword);
      Printf.bprintf buffer "%scolon span=%s\n" child_indent
        (location_text sources default_label.switch_default_colon)
  | Ast.Switch_subswitch_element subswitch ->
      let child_indent = indent ^ "  " in
      Printf.bprintf buffer "%ssub_switch span=%s elements=%d\n" indent
        (location_text sources subswitch.subswitch_location)
        (List.length subswitch.subswitch_elements);
      Printf.bprintf buffer "%sstart_keyword span=%s\n" child_indent
        (location_text sources subswitch.subswitch_start_keyword);
      Printf.bprintf buffer "%sstart_colon span=%s\n" child_indent
        (location_text sources subswitch.subswitch_start_colon);
      List.iteri
        (fun index element ->
          Printf.bprintf buffer "%selement index=%d\n" child_indent index;
          print_switch_element buffer sources ~indent:(child_indent ^ "  ")
            element)
        subswitch.subswitch_elements;
      Printf.bprintf buffer "%send_keyword span=%s\n" child_indent
        (location_text sources subswitch.subswitch_end_keyword);
      Printf.bprintf buffer "%send_colon span=%s\n" child_indent
        (location_text sources subswitch.subswitch_end_colon)
  | Ast.Switch_statement_element statement ->
      Printf.bprintf buffer "%sstatement\n" indent;
      print_statement buffer sources ~indent:(indent ^ "  ") statement

and print_implicit_output_statement buffer sources ~indent
    (statement : Ast.implicit_output_statement) =
  let child_indent = indent ^ "  " in
  let marker_kind =
    match statement.target with
    | Ast.Print_target -> "string_literal"
    | Ast.Put_chars_target -> "character_literal"
  in
  Printf.bprintf buffer "%simplicit_output target=%s span=%s arguments=%d\n"
    indent
    (implicit_output_target_name statement.target)
    (location_text sources statement.location)
    (List.length statement.arguments);
  Printf.bprintf buffer "%smarker kind=%s spelling=%S value=%S span=%s\n"
    child_indent marker_kind statement.marker.literal_spelling
    (literal_value_text statement.marker.literal_value)
    (location_text sources statement.marker.literal_location);
  (match statement.fixed_argument with
  | Ast.Marker_fixed_argument expression ->
      Printf.bprintf buffer "%sfixed_argument kind=marker_expression\n"
        child_indent;
      print_expression buffer sources ~indent:(child_indent ^ "  ") expression
  | Ast.Expression_fixed_argument expression ->
      Printf.bprintf buffer "%sfixed_argument kind=following_expression\n"
        child_indent;
      print_expression buffer sources ~indent:(child_indent ^ "  ") expression);
  List.iteri
    (fun index (argument : Ast.implicit_output_argument) ->
      Printf.bprintf buffer "%sargument index=%d span=%s\n" child_indent index
        (location_text sources argument.location);
      Printf.bprintf buffer "%s  comma span=%s\n" child_indent
        (location_text sources argument.leading_comma);
      print_expression buffer sources ~indent:(child_indent ^ "  ")
        argument.value)
    statement.arguments;
  match statement.semicolon with
  | None -> Printf.bprintf buffer "%ssemicolon omitted\n" child_indent
  | Some semicolon ->
      Printf.bprintf buffer "%ssemicolon span=%s\n" child_indent
        (location_text sources semicolon)

let print_binding buffer sources ~indent = function
  | None -> ()
  | Some (binding : Ast.declaration_binding) -> (
      Printf.bprintf buffer "%sbinding kind=%s spelling=%S span=%s\n" indent
        (binding_kind_name binding.kind)
        binding.spelling
        (location_text sources binding.location);
      match binding.target with
      | Ast.No_binding_target -> ()
      | Ast.Symbol_binding_target target ->
          Printf.bprintf buffer "%s  target spelling=%S span=%s\n" indent
            target.spelling
            (location_text sources target.location)
      | Ast.Expression_binding_target expression ->
          Printf.bprintf buffer "%s  target_expression\n" indent;
          print_expression buffer sources ~indent:(indent ^ "    ") expression)

let print_array_dimensions buffer sources ~indent dimensions =
  List.iteri
    (fun index (dimension : Ast.array_dimension) ->
      let child_indent = indent ^ "  " in
      Printf.bprintf buffer "%sarray_dimension index=%d sized=%b span=%s\n"
        indent index
        (Option.is_some dimension.dimension_expression)
        (location_text sources dimension.location);
      Printf.bprintf buffer "%sopening_bracket span=%s\n" child_indent
        (location_text sources dimension.opening_bracket);
      (match dimension.dimension_expression with
      | None -> Printf.bprintf buffer "%sexpression omitted\n" child_indent
      | Some expression ->
          print_expression buffer sources ~indent:child_indent expression);
      Printf.bprintf buffer "%sclosing_bracket span=%s\n" child_indent
        (location_text sources dimension.closing_bracket))
    dimensions

let rec print_aggregate_member buffer sources ~indent index = function
  | Ast.Aggregate_member_declaration declaration ->
      Printf.bprintf buffer
        "%smember_declaration index=%d span=%s declarators=%d\n" indent index
        (location_text sources declaration.member_declaration_location)
        (List.length declaration.member_declarators);
      let child_indent = indent ^ "  " in
      print_type buffer sources ~indent:child_indent
        declaration.member_type_specifier;
      List.iteri
        (fun declarator_index (declarator : Ast.aggregate_member_declarator) ->
          let declarator_indent = child_indent ^ "  " in
          Printf.bprintf buffer "%sdeclarator index=%d span=%s\n" child_indent
            declarator_index
            (location_text sources declarator.member_declarator_location);
          print_pointer_layers buffer sources ~indent:declarator_indent
            declarator.member_pointer_layers;
          Printf.bprintf buffer "%sname spelling=%S span=%s\n" declarator_indent
            declarator.member_name.spelling
            (location_text sources declarator.member_name.location);
          print_array_dimensions buffer sources ~indent:declarator_indent
            declarator.member_array_dimensions;
          Printf.bprintf buffer "%sdelimiter kind=%s spelling=%S span=%s\n"
            declarator_indent
            (delimiter_kind_name declarator.member_delimiter.kind)
            declarator.member_delimiter.spelling
            (location_text sources declarator.member_delimiter.location))
        declaration.member_declarators
  | Ast.Anonymous_union_member anonymous_union -> (
      Printf.bprintf buffer "%sanonymous_union index=%d span=%s members=%d\n"
        indent index
        (location_text sources anonymous_union.anonymous_union_location)
        (List.length anonymous_union.anonymous_union_members);
      let child_indent = indent ^ "  " in
      Printf.bprintf buffer "%skeyword spelling=%S span=%s\n" child_indent
        anonymous_union.anonymous_union_keyword_spelling
        (location_text sources anonymous_union.anonymous_union_keyword_location);
      Printf.bprintf buffer "%sopening_brace span=%s\n" child_indent
        (location_text sources anonymous_union.anonymous_union_opening_brace);
      List.iteri
        (print_aggregate_member buffer sources ~indent:(child_indent ^ "  "))
        anonymous_union.anonymous_union_members;
      Printf.bprintf buffer "%sclosing_brace span=%s\n" child_indent
        (location_text sources anonymous_union.anonymous_union_closing_brace);
      match anonymous_union.anonymous_union_semicolon with
      | None -> Printf.bprintf buffer "%ssemicolon omitted\n" child_indent
      | Some semicolon ->
          Printf.bprintf buffer "%ssemicolon span=%s\n" child_indent
            (location_text sources semicolon))
  | Ast.Empty_aggregate_member semicolon ->
      Printf.bprintf buffer "%sempty_member index=%d semicolon=%s\n" indent
        index
        (location_text sources semicolon)

let print_parameter_default buffer sources ~indent
    (default : Ast.parameter_default) =
  let child_indent = indent ^ "  " in
  Printf.bprintf buffer "%sdefault span=%s\n" indent
    (location_text sources default.location);
  Printf.bprintf buffer "%sequals span=%s\n" child_indent
    (location_text sources default.equals);
  match default.value with
  | Ast.Expression_default expression ->
      print_expression buffer sources ~indent:child_indent expression
  | Ast.Lastclass_default lastclass ->
      Printf.bprintf buffer "%slastclass spelling=%S span=%s\n" child_indent
        lastclass.lastclass_spelling
        (location_text sources lastclass.lastclass_location)

let rec print_function_parameter buffer sources ~indent index
    (parameter : Ast.function_parameter) =
  let child_indent = indent ^ "  " in
  Printf.bprintf buffer "%sparameter index=%d span=%s\n" indent index
    (location_text sources parameter.location);
  print_register_qualifiers buffer sources ~indent:child_indent
    ~position:Ast.Before_type parameter.register_qualifiers;
  print_type buffer sources ~indent:child_indent parameter.type_specifier;
  print_register_qualifiers buffer sources ~indent:child_indent
    ~position:Ast.After_type parameter.register_qualifiers;
  print_pointer_layers buffer sources ~indent:child_indent
    parameter.pointer_layers;
  (match parameter.function_pointer with
  | None ->
      print_parameter_name buffer sources ~indent:child_indent parameter.name
  | Some function_pointer ->
      print_function_pointer buffer sources ~indent:child_indent
        ~name:parameter.name function_pointer);
  Option.iter
    (print_parameter_default buffer sources ~indent:child_indent)
    parameter.default;
  Option.iter
    (fun (delimiter : Ast.declaration_delimiter) ->
      Printf.bprintf buffer "%sdelimiter kind=%s spelling=%S span=%s\n"
        child_indent
        (delimiter_kind_name delimiter.kind)
        delimiter.spelling
        (location_text sources delimiter.location))
    parameter.delimiter

and print_function_pointer buffer sources ~indent ~name
    (function_pointer : Ast.function_pointer_declarator) =
  let child_indent = indent ^ "  " in
  Printf.bprintf buffer "%sfunction_pointer span=%s parameters=%d variadic=%b\n"
    indent
    (location_text sources function_pointer.function_pointer_location)
    (List.length function_pointer.signature_parameters)
    (Option.is_some function_pointer.signature_variadic);
  Printf.bprintf buffer "%sopening_parenthesis span=%s\n" child_indent
    (location_text sources function_pointer.declarator_opening_parenthesis);
  print_pointer_layers buffer sources ~indent:child_indent
    function_pointer.indirection_layers;
  print_parameter_name buffer sources ~indent:child_indent name;
  Printf.bprintf buffer "%sclosing_parenthesis span=%s\n" child_indent
    (location_text sources function_pointer.declarator_closing_parenthesis);
  Printf.bprintf buffer "%ssignature_opening_parenthesis span=%s\n" child_indent
    (location_text sources function_pointer.signature_opening_parenthesis);
  List.iteri
    (print_function_parameter buffer sources ~indent:child_indent)
    function_pointer.signature_parameters;
  Option.iter
    (print_variadic_marker buffer sources ~indent:child_indent)
    function_pointer.signature_variadic;
  Printf.bprintf buffer "%ssignature_closing_parenthesis span=%s\n" child_indent
    (location_text sources function_pointer.signature_closing_parenthesis)

and print_variadic_marker buffer sources ~indent
    (variadic : Ast.variadic_marker) =
  Printf.bprintf buffer "%svariadic spelling=%S span=%s\n" indent
    variadic.spelling
    (location_text sources variadic.location);
  print_register_qualifiers buffer sources ~indent:(indent ^ "  ")
    ~position:Ast.Before_type variadic.register_qualifiers

let human sources module_ =
  let buffer = Buffer.create 256 in
  Printf.bprintf buffer "schema %s\n" schema;
  Printf.bprintf buffer "reference %s\n" Sema.Primitive_type.reference_commit;
  Printf.bprintf buffer "module span=%s items=%d\n"
    (span_text sources module_.Ast.span)
    (List.length module_.items);
  List.iter
    (function
      | Ast.Aggregate_forward_declaration declaration ->
          Printf.bprintf buffer
            "  aggregate_forward_declaration aggregate_kind=%s span=%s\n"
            (aggregate_kind_name declaration.aggregate_kind)
            (location_text sources declaration.location);
          print_modifiers buffer sources ~indent:"    " declaration.modifiers;
          print_binding buffer sources ~indent:"    " (Some declaration.binding);
          Printf.bprintf buffer "    aggregate_keyword spelling=%S span=%s\n"
            declaration.aggregate_keyword_spelling
            (location_text sources declaration.aggregate_keyword_location);
          Printf.bprintf buffer "    name spelling=%S span=%s\n"
            declaration.name.spelling
            (location_text sources declaration.name.location);
          Printf.bprintf buffer "    semicolon span=%s\n"
            (location_text sources declaration.semicolon)
      | Ast.Aggregate_definition definition ->
          Printf.bprintf buffer
            "  aggregate_definition aggregate_kind=%s span=%s members=%d\n"
            (aggregate_kind_name definition.aggregate_kind)
            (location_text sources definition.location)
            (List.length definition.members);
          print_modifiers buffer sources ~indent:"    " definition.modifiers;
          Printf.bprintf buffer "    aggregate_keyword spelling=%S span=%s\n"
            definition.aggregate_keyword_spelling
            (location_text sources definition.aggregate_keyword_location);
          Printf.bprintf buffer "    name spelling=%S span=%s\n"
            definition.name.spelling
            (location_text sources definition.name.location);
          Printf.bprintf buffer "    opening_brace span=%s\n"
            (location_text sources definition.opening_brace);
          List.iteri
            (print_aggregate_member buffer sources ~indent:"    ")
            definition.members;
          Printf.bprintf buffer "    closing_brace span=%s\n"
            (location_text sources definition.closing_brace);
          Printf.bprintf buffer "    semicolon span=%s\n"
            (location_text sources definition.semicolon)
      | Ast.Global_variable variable ->
          Printf.bprintf buffer "  global_variable span=%s\n"
            (location_text sources variable.location);
          print_modifiers buffer sources ~indent:"    " variable.modifiers;
          print_binding buffer sources ~indent:"    " variable.binding;
          print_type buffer sources ~indent:"    " variable.type_specifier;
          print_pointer_layers buffer sources ~indent:"    "
            variable.pointer_layers;
          Printf.bprintf buffer "    name spelling=%S span=%s\n"
            variable.name.spelling
            (location_text sources variable.name.location);
          print_array_dimensions buffer sources ~indent:"    "
            variable.array_dimensions;
          Printf.bprintf buffer "    semicolon span=%s\n"
            (span_text sources variable.semicolon)
      | Ast.Global_declaration declaration ->
          Printf.bprintf buffer "  global_declaration span=%s declarators=%d\n"
            (location_text sources declaration.location)
            (List.length declaration.declarators);
          print_modifiers buffer sources ~indent:"    " declaration.modifiers;
          print_binding buffer sources ~indent:"    " declaration.binding;
          print_type buffer sources ~indent:"    " declaration.type_specifier;
          List.iteri
            (fun index (declarator : Ast.global_declarator) ->
              Printf.bprintf buffer "    declarator index=%d span=%s\n" index
                (location_text sources declarator.Ast.location);
              print_pointer_layers buffer sources ~indent:"      "
                declarator.pointer_layers;
              Printf.bprintf buffer "      name spelling=%S span=%s\n"
                declarator.name.spelling
                (location_text sources declarator.name.location);
              print_array_dimensions buffer sources ~indent:"      "
                declarator.array_dimensions;
              Printf.bprintf buffer
                "      delimiter kind=%s spelling=%S span=%s\n"
                (delimiter_kind_name declarator.delimiter.kind)
                declarator.delimiter.spelling
                (location_text sources declarator.delimiter.location))
            declaration.declarators
      | Ast.Function_prototype prototype ->
          Printf.bprintf buffer
            "  function_prototype span=%s parameters=%d variadic=%b\n"
            (location_text sources prototype.location)
            (List.length prototype.parameters)
            (Option.is_some prototype.variadic);
          print_modifiers buffer sources ~indent:"    " prototype.modifiers;
          print_binding buffer sources ~indent:"    " (Some prototype.binding);
          print_return_type buffer sources ~indent:"    " prototype.return_type;
          print_pointer_layers buffer sources ~indent:"    "
            prototype.return_pointer_layers;
          Printf.bprintf buffer "    name spelling=%S span=%s\n"
            prototype.name.spelling
            (location_text sources prototype.name.location);
          Printf.bprintf buffer "    opening_parenthesis span=%s\n"
            (location_text sources prototype.opening_parenthesis);
          List.iteri
            (print_function_parameter buffer sources ~indent:"    ")
            prototype.parameters;
          Option.iter
            (print_variadic_marker buffer sources ~indent:"    ")
            prototype.variadic;
          Printf.bprintf buffer "    closing_parenthesis span=%s\n"
            (location_text sources prototype.closing_parenthesis);
          Printf.bprintf buffer "    semicolon span=%s\n"
            (location_text sources prototype.semicolon)
      | Ast.Function_definition definition ->
          Printf.bprintf buffer
            "  function_definition span=%s parameters=%d variadic=%b body=%s\n"
            (location_text sources definition.location)
            (List.length definition.parameters)
            (Option.is_some definition.variadic)
            (if Option.is_some definition.body then "present" else "absent");
          print_modifiers buffer sources ~indent:"    " definition.modifiers;
          print_return_type buffer sources ~indent:"    " definition.return_type;
          print_pointer_layers buffer sources ~indent:"    "
            definition.return_pointer_layers;
          Printf.bprintf buffer "    name spelling=%S span=%s\n"
            definition.name.spelling
            (location_text sources definition.name.location);
          Printf.bprintf buffer "    opening_parenthesis span=%s\n"
            (location_text sources definition.opening_parenthesis);
          List.iteri
            (print_function_parameter buffer sources ~indent:"    ")
            definition.parameters;
          Option.iter
            (print_variadic_marker buffer sources ~indent:"    ")
            definition.variadic;
          Printf.bprintf buffer "    closing_parenthesis span=%s\n"
            (location_text sources definition.closing_parenthesis);
          Option.iter
            (fun body ->
              Printf.bprintf buffer "    body span=%s\n"
                (location_text sources (Ast.statement_location body));
              print_statement buffer sources ~indent:"      " body)
            definition.body
      | Ast.Top_level_statement statement ->
          Printf.bprintf buffer "  top_level_statement span=%s\n"
            (location_text sources (Ast.statement_location statement));
          print_statement buffer sources ~indent:"    " statement)
    module_.items;
  Buffer.contents buffer

let span_to_yojson sources span =
  let base =
    [
      ("source_id", `Int (Common.Source_id.to_int span.Common.Span.source));
      ("start", `Int span.start);
      ("stop", `Int span.stop);
    ]
  in
  match Common.Source_manager.find sources span.source with
  | None -> `Assoc base
  | Some source ->
      let position = Common.Source_file.position source span.start in
      let location =
        match position with
        | Error _ -> []
        | Ok item -> [ ("line", `Int item.line); ("column", `Int item.column) ]
      in
      `Assoc
        ((("path", `String (Common.Source_file.display_path source)) :: location)
        @ base)

let location_to_yojson sources (location : Ast.location) =
  `Assoc
    ([
       ("span", span_to_yojson sources location.Ast.span);
       ( "source_segments",
         `List (List.map (span_to_yojson sources) location.source_segments) );
     ]
    @ (match location.generated_from with
      | None -> []
      | Some span -> [ ("generated_from", span_to_yojson sources span) ])
    @
    match location.defined_at with
    | None -> []
    | Some span -> [ ("defined_at", span_to_yojson sources span) ])

let primitive_to_yojson sources primitive =
  `Assoc
    [
      ("kind", `String "primitive");
      ( "primitive",
        `String (Sema.Primitive_type.to_string primitive.Ast.primitive) );
      ("spelling", `String primitive.spelling);
      ("location", location_to_yojson sources primitive.location);
    ]

let type_to_yojson sources = function
  | Ast.Primitive_type_specifier primitive ->
      primitive_to_yojson sources primitive
  | Ast.Named_type_specifier name ->
      `Assoc
        [
          ("kind", `String "named");
          ("spelling", `String name.Ast.spelling);
          ("location", location_to_yojson sources name.location);
        ]

let identifier_to_yojson sources (identifier : Ast.identifier) =
  `Assoc
    [
      ("spelling", `String identifier.Ast.spelling);
      ("location", location_to_yojson sources identifier.location);
    ]

let pointer_layer_to_yojson sources (pointer : Ast.pointer_layer) =
  `Assoc
    [
      ("depth", `Int pointer.Ast.depth);
      ("spelling", `String pointer.spelling);
      ("location", location_to_yojson sources pointer.location);
    ]

let pointer_layer_fields sources pointer_layers =
  match pointer_layers with
  | [] -> []
  | pointers ->
      [
        ( "pointer_layers",
          `List (List.map (pointer_layer_to_yojson sources) pointers) );
      ]

let modifier_to_yojson sources (modifier : Ast.declaration_modifier) =
  `Assoc
    [
      ("kind", `String (modifier_kind_name modifier.kind));
      ("spelling", `String modifier.spelling);
      ("location", location_to_yojson sources modifier.location);
    ]

let modifier_fields sources modifiers =
  match modifiers with
  | [] -> []
  | modifiers ->
      [ ("modifiers", `List (List.map (modifier_to_yojson sources) modifiers)) ]

let register_qualifier_to_yojson sources (qualifier : Ast.register_qualifier) =
  `Assoc
    ([
       ("kind", `String (register_qualifier_kind_name qualifier.kind));
       ( "position",
         `String (register_qualifier_position_name qualifier.position) );
       ("spelling", `String qualifier.spelling);
     ]
    @ (match qualifier.explicit_register with
      | None -> []
      | Some register ->
          [ ("explicit_register", identifier_to_yojson sources register) ])
    @ [ ("location", location_to_yojson sources qualifier.location) ])

let register_qualifier_fields sources qualifiers =
  match qualifiers with
  | [] -> []
  | qualifiers ->
      [
        ( "register_qualifiers",
          `List (List.map (register_qualifier_to_yojson sources) qualifiers) );
      ]

let delimiter_to_yojson sources (delimiter : Ast.declaration_delimiter) =
  `Assoc
    [
      ("kind", `String (delimiter_kind_name delimiter.kind));
      ("spelling", `String delimiter.spelling);
      ("location", location_to_yojson sources delimiter.location);
    ]

let variadic_to_yojson sources (variadic : Ast.variadic_marker) =
  `Assoc
    ([ ("spelling", `String variadic.spelling) ]
    @ register_qualifier_fields sources variadic.register_qualifiers
    @ [ ("location", location_to_yojson sources variadic.location) ])

let literal_value_to_yojson = function
  | Ast.Integer_value value -> `String (Printf.sprintf "0x%016Lx" value)
  | Ast.Float_value value -> `Float value
  | Ast.Bytes_value value -> `String (escaped_bytes value)

let literal_to_yojson sources ~kind (literal : Ast.expression_literal) =
  `Assoc
    [
      ("kind", `String kind);
      ("spelling", `String literal.literal_spelling);
      ("value", literal_value_to_yojson literal.literal_value);
      ("location", location_to_yojson sources literal.literal_location);
    ]

let expression_operator_to_yojson sources (operator : Ast.expression_operator) =
  `Assoc
    [
      ("spelling", `String operator.operator_spelling);
      ("location", location_to_yojson sources operator.operator_location);
    ]

let sizeof_member_to_yojson sources (member : Ast.sizeof_member) =
  `Assoc
    [
      ("dot", location_to_yojson sources member.sizeof_member_dot);
      ("name", identifier_to_yojson sources member.sizeof_member_name);
      ("location", location_to_yojson sources member.sizeof_member_location);
    ]

let offset_member_to_yojson sources (member : Ast.offset_member) =
  `Assoc
    [
      ("dot", location_to_yojson sources member.offset_member_dot);
      ("name", identifier_to_yojson sources member.offset_member_name);
      ("location", location_to_yojson sources member.offset_member_location);
    ]

let rec expression_to_yojson sources = function
  | Ast.Integer_literal literal ->
      literal_to_yojson sources ~kind:"integer_literal" literal
  | Ast.Float_literal literal ->
      literal_to_yojson sources ~kind:"float_literal" literal
  | Ast.Character_literal literal ->
      literal_to_yojson sources ~kind:"character_literal" literal
  | Ast.String_literal literal ->
      literal_to_yojson sources ~kind:"string_literal" literal
  | Ast.Identifier_expression identifier ->
      `Assoc
        [
          ("kind", `String "identifier");
          ("spelling", `String identifier.spelling);
          ("location", location_to_yojson sources identifier.location);
        ]
  | Ast.Current_position_expression operator ->
      `Assoc
        [
          ("kind", `String "current_position");
          ("spelling", `String operator.operator_spelling);
          ("location", location_to_yojson sources operator.operator_location);
        ]
  | Ast.Sizeof_expression sizeof_expression ->
      `Assoc
        ([
           ("kind", `String "sizeof");
           ( "keyword",
             `Assoc
               [
                 ("spelling", `String sizeof_expression.sizeof_keyword_spelling);
                 ( "location",
                   location_to_yojson sources
                     sizeof_expression.sizeof_keyword_location );
               ] );
           ( "opening_parentheses",
             `List
               (List.map
                  (location_to_yojson sources)
                  sizeof_expression.sizeof_opening_parentheses) );
           ( "target",
             identifier_to_yojson sources sizeof_expression.sizeof_target );
           ( "members",
             `List
               (List.map
                  (sizeof_member_to_yojson sources)
                  sizeof_expression.sizeof_members) );
         ]
        @ pointer_layer_fields sources sizeof_expression.sizeof_pointer_layers
        @ [
            ( "closing_parentheses",
              `List
                (List.map
                   (location_to_yojson sources)
                   sizeof_expression.sizeof_closing_parentheses) );
            ( "location",
              location_to_yojson sources sizeof_expression.sizeof_location );
          ])
  | Ast.Offset_expression offset_expression ->
      `Assoc
        [
          ("kind", `String "offset");
          ( "keyword",
            `Assoc
              [
                ("spelling", `String offset_expression.offset_keyword_spelling);
                ( "location",
                  location_to_yojson sources
                    offset_expression.offset_keyword_location );
              ] );
          ( "opening_parentheses",
            `List
              (List.map
                 (location_to_yojson sources)
                 offset_expression.offset_opening_parentheses) );
          ( "target",
            identifier_to_yojson sources offset_expression.offset_target );
          ( "members",
            `List
              (List.map
                 (offset_member_to_yojson sources)
                 offset_expression.offset_members) );
          ( "closing_parentheses",
            `List
              (List.map
                 (location_to_yojson sources)
                 offset_expression.offset_closing_parentheses) );
          ( "location",
            location_to_yojson sources offset_expression.offset_location );
        ]
  | Ast.Defined_expression defined_expression ->
      `Assoc
        [
          ("kind", `String "defined");
          ( "keyword",
            `Assoc
              [
                ("spelling", `String defined_expression.defined_keyword_spelling);
                ( "location",
                  location_to_yojson sources
                    defined_expression.defined_keyword_location );
              ] );
          ( "opening_parentheses",
            `List
              (List.map
                 (location_to_yojson sources)
                 defined_expression.defined_opening_parentheses) );
          ( "operand",
            `Assoc
              [
                ( "kind",
                  `String
                    (defined_operand_kind_name
                       defined_expression.defined_operand.defined_operand_kind)
                );
                ( "spelling",
                  `String
                    defined_expression.defined_operand.defined_operand_spelling
                );
                ( "location",
                  location_to_yojson sources
                    defined_expression.defined_operand.defined_operand_location
                );
              ] );
          ( "closing_parentheses",
            `List
              (List.map
                 (location_to_yojson sources)
                 defined_expression.defined_closing_parentheses) );
          ( "location",
            location_to_yojson sources defined_expression.defined_location );
        ]
  | Ast.Parenthesized_expression grouped ->
      `Assoc
        [
          ("kind", `String "parenthesized");
          ( "opening_parenthesis",
            location_to_yojson sources grouped.opening_parenthesis );
          ("expression", expression_to_yojson sources grouped.grouped_expression);
          ( "closing_parenthesis",
            location_to_yojson sources grouped.closing_parenthesis );
          ("location", location_to_yojson sources grouped.parenthesized_location);
        ]
  | Ast.Prefix_expression prefix ->
      `Assoc
        [
          ("kind", `String "prefix");
          ( "operator_kind",
            `String (unary_operator_kind_name prefix.prefix_operator_kind) );
          ( "operator",
            expression_operator_to_yojson sources prefix.prefix_operator );
          ("operand", expression_to_yojson sources prefix.prefix_operand);
          ("location", location_to_yojson sources prefix.prefix_location);
        ]
  | Ast.Postfix_expression postfix ->
      `Assoc
        [
          ("kind", `String "postfix");
          ( "operator_kind",
            `String (postfix_operator_kind_name postfix.postfix_operator_kind)
          );
          ("operand", expression_to_yojson sources postfix.postfix_operand);
          ( "operator",
            expression_operator_to_yojson sources postfix.postfix_operator );
          ("location", location_to_yojson sources postfix.postfix_location);
        ]
  | Ast.Postfix_cast_expression cast ->
      `Assoc
        ([
           ("kind", `String "postfix_cast");
           ("operand", expression_to_yojson sources cast.cast_operand);
           ( "opening_parenthesis",
             location_to_yojson sources cast.cast_opening_parenthesis );
           ("target_type", type_to_yojson sources cast.cast_type);
         ]
        @ pointer_layer_fields sources cast.cast_pointer_layers
        @ [
            ( "closing_parenthesis",
              location_to_yojson sources cast.cast_closing_parenthesis );
            ("location", location_to_yojson sources cast.cast_location);
          ])
  | Ast.Binary_expression binary ->
      `Assoc
        [
          ("kind", `String "binary");
          ("left", expression_to_yojson sources binary.binary_left);
          ( "operator",
            expression_operator_to_yojson sources binary.binary_operator );
          ("precedence", `String binary.binary_operator_spec.precedence_name);
          ("precedence_value", `Int binary.binary_operator_spec.precedence_value);
          ( "association",
            `String (association_name binary.binary_operator_spec.association)
          );
          ("ic", `String binary.binary_operator_spec.ic_name);
          ("source_line", `Int binary.binary_operator_spec.source_line);
          ("right", expression_to_yojson sources binary.binary_right);
          ("location", location_to_yojson sources binary.binary_location);
        ]
  | Ast.Call_expression call ->
      let opening_parenthesis, closing_parenthesis =
        match call.call_syntax with
        | Ast.Parenthesized_call { opening_parenthesis; closing_parenthesis } ->
            ( location_to_yojson sources opening_parenthesis,
              location_to_yojson sources closing_parenthesis )
        | Ast.Parenthesis_free_call -> (`Null, `Null)
      in
      `Assoc
        [
          ("kind", `String "call");
          ("callee", expression_to_yojson sources call.call_callee);
          ("opening_parenthesis", opening_parenthesis);
          ( "arguments",
            `List
              (List.map (call_argument_to_yojson sources) call.call_arguments)
          );
          ("closing_parenthesis", closing_parenthesis);
          ("location", location_to_yojson sources call.call_location);
        ]
  | Ast.Index_expression index ->
      `Assoc
        [
          ("kind", `String "index");
          ("base", expression_to_yojson sources index.index_base);
          ( "opening_bracket",
            location_to_yojson sources index.index_opening_bracket );
          ("index", expression_to_yojson sources index.index_value);
          ( "closing_bracket",
            location_to_yojson sources index.index_closing_bracket );
          ("location", location_to_yojson sources index.index_location);
        ]
  | Ast.Member_expression member ->
      `Assoc
        [
          ("kind", `String "member");
          ( "access_kind",
            `String (member_access_kind_name member.member_access_kind) );
          ("base", expression_to_yojson sources member.member_base);
          ( "operator",
            expression_operator_to_yojson sources member.member_operator );
          ("member", identifier_to_yojson sources member.member_name);
          ("location", location_to_yojson sources member.member_location);
        ]

and call_argument_to_yojson sources (argument : Ast.call_argument) =
  `Assoc
    ([
       ( "kind",
         `String
           (match argument.call_argument_value with
           | Ast.Omitted_call_argument -> "omitted"
           | Ast.Provided_call_argument _ -> "provided") );
     ]
    @ (match argument.call_argument_value with
      | Ast.Omitted_call_argument -> []
      | Ast.Provided_call_argument expression ->
          [ ("expression", expression_to_yojson sources expression) ])
    @ [
        ( "comma",
          match argument.following_comma with
          | None -> `Null
          | Some comma -> location_to_yojson sources comma );
        ("location", location_to_yojson sources argument.call_argument_location);
      ])

let implicit_output_argument_to_yojson sources
    (argument : Ast.implicit_output_argument) =
  `Assoc
    [
      ("comma", location_to_yojson sources argument.leading_comma);
      ("expression", expression_to_yojson sources argument.value);
      ("location", location_to_yojson sources argument.location);
    ]

let implicit_output_statement_to_yojson sources
    (statement : Ast.implicit_output_statement) =
  let marker_kind =
    match statement.target with
    | Ast.Print_target -> "string_literal"
    | Ast.Put_chars_target -> "character_literal"
  in
  let fixed_argument =
    match statement.fixed_argument with
    | Ast.Marker_fixed_argument expression ->
        `Assoc
          [
            ("kind", `String "marker_expression");
            ("expression", expression_to_yojson sources expression);
          ]
    | Ast.Expression_fixed_argument expression ->
        `Assoc
          [
            ("kind", `String "following_expression");
            ("expression", expression_to_yojson sources expression);
          ]
  in
  `Assoc
    [
      ("kind", `String "implicit_output_statement");
      ("target", `String (implicit_output_target_name statement.target));
      ("marker", literal_to_yojson sources ~kind:marker_kind statement.marker);
      ("fixed_argument", fixed_argument);
      ( "arguments",
        `List
          (List.map
             (implicit_output_argument_to_yojson sources)
             statement.arguments) );
      ( "semicolon",
        match statement.semicolon with
        | None -> `Null
        | Some semicolon -> location_to_yojson sources semicolon );
      ("location", location_to_yojson sources statement.location);
    ]

let rec statement_to_yojson sources = function
  | Ast.Block_statement statement ->
      `Assoc
        [
          ("kind", `String "block_statement");
          ( "opening_brace",
            location_to_yojson sources statement.block_opening_brace );
          ( "statements",
            `List
              (List.map
                 (statement_to_yojson sources)
                 statement.block_statements) );
          ( "closing_brace",
            location_to_yojson sources statement.block_closing_brace );
          ("location", location_to_yojson sources statement.block_location);
        ]
  | Ast.Break_statement statement ->
      `Assoc
        [
          ("kind", `String "break_statement");
          ("keyword", location_to_yojson sources statement.break_keyword);
          ( "semicolon",
            match statement.break_semicolon with
            | None -> `Null
            | Some semicolon -> location_to_yojson sources semicolon );
          ("location", location_to_yojson sources statement.break_location);
        ]
  | Ast.Do_while_statement statement ->
      `Assoc
        [
          ("kind", `String "do_while_statement");
          ("do_keyword", location_to_yojson sources statement.do_keyword);
          ("body", statement_to_yojson sources statement.do_body);
          ( "while_keyword",
            location_to_yojson sources statement.do_while_keyword );
          ( "opening_parenthesis",
            location_to_yojson sources statement.do_while_opening_parenthesis );
          ( "condition",
            expression_to_yojson sources statement.do_while_condition );
          ( "closing_parenthesis",
            location_to_yojson sources statement.do_while_closing_parenthesis );
          ("semicolon", location_to_yojson sources statement.do_while_semicolon);
          ("location", location_to_yojson sources statement.do_while_location);
        ]
  | Ast.Empty_statement statement ->
      `Assoc
        [
          ("kind", `String "empty_statement");
          ( "semicolon",
            location_to_yojson sources statement.empty_statement_semicolon );
          ( "location",
            location_to_yojson sources statement.empty_statement_location );
        ]
  | Ast.Expression_statement statement ->
      `Assoc
        [
          ("kind", `String "expression_statement");
          ( "expression",
            expression_to_yojson sources
              statement.expression_statement_expression );
          ( "semicolon",
            match statement.expression_statement_semicolon with
            | None -> `Null
            | Some semicolon -> location_to_yojson sources semicolon );
          ( "location",
            location_to_yojson sources statement.expression_statement_location
          );
        ]
  | Ast.For_statement statement ->
      `Assoc
        [
          ("kind", `String "for_statement");
          ("keyword", location_to_yojson sources statement.for_keyword);
          ( "opening_parenthesis",
            location_to_yojson sources statement.for_opening_parenthesis );
          ("initializer", statement_to_yojson sources statement.for_initializer);
          ("condition", expression_to_yojson sources statement.for_condition);
          ( "condition_semicolon",
            location_to_yojson sources statement.for_condition_semicolon );
          ( "update",
            match statement.for_update with
            | None -> `Null
            | Some update -> statement_to_yojson sources update );
          ( "closing_parenthesis",
            location_to_yojson sources statement.for_closing_parenthesis );
          ("body", statement_to_yojson sources statement.for_body);
          ("location", location_to_yojson sources statement.for_location);
        ]
  | Ast.Goto_statement statement ->
      `Assoc
        [
          ("kind", `String "goto_statement");
          ("keyword", location_to_yojson sources statement.goto_keyword);
          ("target", identifier_to_yojson sources statement.goto_target);
          ( "semicolon",
            match statement.goto_semicolon with
            | None -> `Null
            | Some semicolon -> location_to_yojson sources semicolon );
          ("location", location_to_yojson sources statement.goto_location);
        ]
  | Ast.If_statement statement ->
      `Assoc
        [
          ("kind", `String "if_statement");
          ("keyword", location_to_yojson sources statement.if_keyword);
          ( "opening_parenthesis",
            location_to_yojson sources statement.if_opening_parenthesis );
          ("condition", expression_to_yojson sources statement.if_condition);
          ( "closing_parenthesis",
            location_to_yojson sources statement.if_closing_parenthesis );
          ("then_branch", statement_to_yojson sources statement.if_then_branch);
          ( "else_clause",
            match statement.if_else_clause with
            | None -> `Null
            | Some else_clause ->
                `Assoc
                  [
                    ( "keyword",
                      location_to_yojson sources else_clause.else_keyword );
                    ( "branch",
                      statement_to_yojson sources else_clause.else_branch );
                    ( "location",
                      location_to_yojson sources else_clause.else_location );
                  ] );
          ("location", location_to_yojson sources statement.if_location);
        ]
  | Ast.While_statement statement ->
      `Assoc
        [
          ("kind", `String "while_statement");
          ("keyword", location_to_yojson sources statement.while_keyword);
          ( "opening_parenthesis",
            location_to_yojson sources statement.while_opening_parenthesis );
          ("condition", expression_to_yojson sources statement.while_condition);
          ( "closing_parenthesis",
            location_to_yojson sources statement.while_closing_parenthesis );
          ("body", statement_to_yojson sources statement.while_body);
          ("location", location_to_yojson sources statement.while_location);
        ]
  | Ast.Implicit_output_statement statement ->
      implicit_output_statement_to_yojson sources statement
  | Ast.Label_statement statement ->
      `Assoc
        [
          ("kind", `String "label_statement");
          ("name", identifier_to_yojson sources statement.label_name);
          ("colon", location_to_yojson sources statement.label_colon);
          ("location", location_to_yojson sources statement.label_location);
        ]
  | Ast.Local_declaration_statement declaration ->
      let dimension_to_yojson (dimension : Ast.array_dimension) =
        `Assoc
          [
            ( "opening_bracket",
              location_to_yojson sources dimension.opening_bracket );
            ( "expression",
              match dimension.dimension_expression with
              | None -> `Null
              | Some expression -> expression_to_yojson sources expression );
            ( "closing_bracket",
              location_to_yojson sources dimension.closing_bracket );
            ("location", location_to_yojson sources dimension.location);
          ]
      in
      let initializer_to_yojson (initial_value : Ast.local_initializer) =
        `Assoc
          [
            ( "equals",
              location_to_yojson sources initial_value.local_initializer_equals
            );
            ( "value",
              expression_to_yojson sources initial_value.local_initializer_value
            );
            ( "location",
              location_to_yojson sources
                initial_value.local_initializer_location );
          ]
      in
      let declarator_to_yojson (declarator : Ast.local_declarator) =
        `Assoc
          (register_qualifier_fields sources
             declarator.local_register_qualifiers
          @ pointer_layer_fields sources declarator.local_pointer_layers
          @ [ ("name", identifier_to_yojson sources declarator.local_name) ]
          @ (match declarator.local_array_dimensions with
            | [] -> []
            | dimensions ->
                [
                  ( "array_dimensions",
                    `List (List.map dimension_to_yojson dimensions) );
                ])
          @ (match declarator.local_initializer with
            | None -> []
            | Some initial_value ->
                [ ("initializer", initializer_to_yojson initial_value) ])
          @ [
              ( "delimiter",
                delimiter_to_yojson sources declarator.local_delimiter );
              ( "location",
                location_to_yojson sources declarator.local_declarator_location
              );
            ])
      in
      `Assoc
        ([
           ("kind", `String "local_declaration_statement");
           ("storage", `String (local_storage_name declaration.local_storage));
         ]
        @ modifier_fields sources declaration.local_modifiers
        @ [
            ("type", type_to_yojson sources declaration.local_type_specifier);
            ( "declarators",
              `List
                (List.map declarator_to_yojson declaration.local_declarators) );
            ( "location",
              location_to_yojson sources declaration.local_declaration_location
            );
          ])
  | Ast.Lock_statement statement ->
      `Assoc
        [
          ("kind", `String "lock_statement");
          ("keyword", location_to_yojson sources statement.lock_keyword);
          ("body", statement_to_yojson sources statement.lock_body);
          ("location", location_to_yojson sources statement.lock_location);
        ]
  | Ast.Switch_statement statement ->
      `Assoc
        [
          ("kind", `String "switch_statement");
          ( "mode",
            `String
              (match statement.switch_mode with
              | Ast.Bounded_switch -> "bounded"
              | Ast.No_bound_switch -> "no_bound") );
          ("keyword", location_to_yojson sources statement.switch_keyword);
          ( "opening_delimiter",
            location_to_yojson sources statement.switch_opening_delimiter );
          ( "expression",
            expression_to_yojson sources statement.switch_expression );
          ( "closing_delimiter",
            location_to_yojson sources statement.switch_closing_delimiter );
          ( "opening_brace",
            location_to_yojson sources statement.switch_opening_brace );
          ( "elements",
            `List
              (List.map
                 (switch_element_to_yojson sources)
                 statement.switch_elements) );
          ( "closing_brace",
            location_to_yojson sources statement.switch_closing_brace );
          ("location", location_to_yojson sources statement.switch_location);
        ]
  | Ast.Try_catch_statement statement ->
      `Assoc
        [
          ("kind", `String "try_catch_statement");
          ("try_keyword", location_to_yojson sources statement.try_keyword);
          ("try_body", statement_to_yojson sources statement.try_body);
          ("catch_keyword", location_to_yojson sources statement.catch_keyword);
          ("catch_body", statement_to_yojson sources statement.catch_body);
          ("location", location_to_yojson sources statement.try_catch_location);
        ]
  | Ast.Return_statement statement ->
      `Assoc
        [
          ("kind", `String "return_statement");
          ("keyword", location_to_yojson sources statement.return_keyword);
          ( "value",
            match statement.return_value with
            | None -> `Null
            | Some value -> expression_to_yojson sources value );
          ( "semicolon",
            match statement.return_semicolon with
            | None -> `Null
            | Some semicolon -> location_to_yojson sources semicolon );
          ("location", location_to_yojson sources statement.return_location);
        ]
  | Ast.Sequence_statement sequence ->
      let element_to_yojson (element : Ast.statement_sequence_element) =
        `Assoc
          [
            ("statement", statement_to_yojson sources element.sequence_statement);
            ( "following_commas",
              `List
                (List.map
                   (location_to_yojson sources)
                   element.sequence_following_commas) );
            ( "location",
              location_to_yojson sources element.sequence_element_location );
          ]
      in
      `Assoc
        [
          ("kind", `String "statement_sequence");
          ( "leading_commas",
            `List
              (List.map
                 (location_to_yojson sources)
                 sequence.sequence_leading_commas) );
          ( "elements",
            `List (List.map element_to_yojson sequence.sequence_elements) );
          ("location", location_to_yojson sources sequence.sequence_location);
        ]

and switch_element_to_yojson sources = function
  | Ast.Switch_case_element case_label ->
      let pattern =
        match case_label.switch_case_pattern with
        | Ast.Implicit_case -> `Assoc [ ("kind", `String "implicit") ]
        | Ast.Single_case expression ->
            `Assoc
              [
                ("kind", `String "single");
                ("value", expression_to_yojson sources expression);
              ]
        | Ast.Ranged_case range ->
            `Assoc
              [
                ("kind", `String "range");
                ("start", expression_to_yojson sources range.case_range_start);
                ( "ellipsis",
                  location_to_yojson sources range.case_range_ellipsis );
                ("end", expression_to_yojson sources range.case_range_end);
                ( "location",
                  location_to_yojson sources range.case_range_location );
              ]
      in
      `Assoc
        [
          ("kind", `String "case_label");
          ("keyword", location_to_yojson sources case_label.switch_case_keyword);
          ("pattern", pattern);
          ("colon", location_to_yojson sources case_label.switch_case_colon);
          ( "location",
            location_to_yojson sources case_label.switch_case_location );
        ]
  | Ast.Switch_default_element default_label ->
      `Assoc
        [
          ("kind", `String "default_label");
          ( "keyword",
            location_to_yojson sources default_label.switch_default_keyword );
          ( "colon",
            location_to_yojson sources default_label.switch_default_colon );
          ( "location",
            location_to_yojson sources default_label.switch_default_location );
        ]
  | Ast.Switch_subswitch_element subswitch ->
      `Assoc
        [
          ("kind", `String "sub_switch");
          ( "start_keyword",
            location_to_yojson sources subswitch.subswitch_start_keyword );
          ( "start_colon",
            location_to_yojson sources subswitch.subswitch_start_colon );
          ( "elements",
            `List
              (List.map
                 (switch_element_to_yojson sources)
                 subswitch.subswitch_elements) );
          ( "end_keyword",
            location_to_yojson sources subswitch.subswitch_end_keyword );
          ("end_colon", location_to_yojson sources subswitch.subswitch_end_colon);
          ("location", location_to_yojson sources subswitch.subswitch_location);
        ]
  | Ast.Switch_statement_element statement ->
      `Assoc
        [
          ("kind", `String "statement");
          ("statement", statement_to_yojson sources statement);
        ]

let binding_to_yojson sources (binding : Ast.declaration_binding) =
  `Assoc
    ([
       ("kind", `String (binding_kind_name binding.kind));
       ("spelling", `String binding.spelling);
       ("location", location_to_yojson sources binding.location);
     ]
    @
    match binding.target with
    | Ast.No_binding_target -> []
    | Ast.Symbol_binding_target target ->
        [ ("target", identifier_to_yojson sources target) ]
    | Ast.Expression_binding_target expression ->
        [ ("target_expression", expression_to_yojson sources expression) ])

let binding_fields sources = function
  | None -> []
  | Some binding -> [ ("binding", binding_to_yojson sources binding) ]

let array_dimension_to_yojson sources (dimension : Ast.array_dimension) =
  `Assoc
    [
      ("opening_bracket", location_to_yojson sources dimension.opening_bracket);
      ( "expression",
        match dimension.dimension_expression with
        | None -> `Null
        | Some expression -> expression_to_yojson sources expression );
      ("closing_bracket", location_to_yojson sources dimension.closing_bracket);
      ("location", location_to_yojson sources dimension.location);
    ]

let array_dimension_fields sources dimensions =
  match dimensions with
  | [] -> []
  | dimensions ->
      [
        ( "array_dimensions",
          `List (List.map (array_dimension_to_yojson sources) dimensions) );
      ]

let declarator_to_yojson sources (declarator : Ast.global_declarator) =
  `Assoc
    (pointer_layer_fields sources declarator.pointer_layers
    @ [ ("name", identifier_to_yojson sources declarator.name) ]
    @ array_dimension_fields sources declarator.array_dimensions
    @ [
        ("delimiter", delimiter_to_yojson sources declarator.delimiter);
        ("location", location_to_yojson sources declarator.location);
      ])

let aggregate_member_declarator_to_yojson sources
    (declarator : Ast.aggregate_member_declarator) =
  `Assoc
    (pointer_layer_fields sources declarator.member_pointer_layers
    @ [ ("name", identifier_to_yojson sources declarator.member_name) ]
    @ array_dimension_fields sources declarator.member_array_dimensions
    @ [
        ("delimiter", delimiter_to_yojson sources declarator.member_delimiter);
        ( "location",
          location_to_yojson sources declarator.member_declarator_location );
      ])

let rec aggregate_member_to_yojson sources = function
  | Ast.Aggregate_member_declaration declaration ->
      `Assoc
        [
          ("kind", `String "member_declaration");
          ("type", type_to_yojson sources declaration.member_type_specifier);
          ( "declarators",
            `List
              (List.map
                 (aggregate_member_declarator_to_yojson sources)
                 declaration.member_declarators) );
          ( "location",
            location_to_yojson sources declaration.member_declaration_location
          );
        ]
  | Ast.Anonymous_union_member anonymous_union ->
      `Assoc
        ([
           ("kind", `String "anonymous_union");
           ( "keyword",
             `Assoc
               [
                 ( "spelling",
                   `String anonymous_union.anonymous_union_keyword_spelling );
                 ( "location",
                   location_to_yojson sources
                     anonymous_union.anonymous_union_keyword_location );
               ] );
           ( "opening_brace",
             location_to_yojson sources
               anonymous_union.anonymous_union_opening_brace );
           ( "members",
             `List
               (List.map
                  (aggregate_member_to_yojson sources)
                  anonymous_union.anonymous_union_members) );
           ( "closing_brace",
             location_to_yojson sources
               anonymous_union.anonymous_union_closing_brace );
         ]
        @ (match anonymous_union.anonymous_union_semicolon with
          | None -> []
          | Some semicolon ->
              [ ("semicolon", location_to_yojson sources semicolon) ])
        @ [
            ( "location",
              location_to_yojson sources
                anonymous_union.anonymous_union_location );
          ])
  | Ast.Empty_aggregate_member semicolon ->
      `Assoc
        [
          ("kind", `String "empty_member");
          ("semicolon", location_to_yojson sources semicolon);
        ]

let parameter_default_to_yojson sources (default : Ast.parameter_default) =
  let value =
    match default.value with
    | Ast.Expression_default expression ->
        expression_to_yojson sources expression
    | Ast.Lastclass_default lastclass ->
        `Assoc
          [
            ("kind", `String "lastclass_default");
            ("spelling", `String lastclass.lastclass_spelling);
            ("location", location_to_yojson sources lastclass.lastclass_location);
          ]
  in
  `Assoc
    [
      ("equals", location_to_yojson sources default.equals);
      ("value", value);
      ("location", location_to_yojson sources default.location);
    ]

let rec parameter_to_yojson sources (parameter : Ast.function_parameter) =
  `Assoc
    (register_qualifier_fields sources parameter.register_qualifiers
    @ [ ("type", type_to_yojson sources parameter.type_specifier) ]
    @ pointer_layer_fields sources parameter.pointer_layers
    @ (match parameter.function_pointer with
      | None -> (
          match parameter.name with
          | None -> []
          | Some name -> [ ("name", identifier_to_yojson sources name) ])
      | Some function_pointer ->
          [
            ( "function_pointer",
              function_pointer_to_yojson sources ~name:parameter.name
                function_pointer );
          ])
    @ (match parameter.default with
      | None -> []
      | Some default ->
          [ ("default", parameter_default_to_yojson sources default) ])
    @ (match parameter.delimiter with
      | None -> []
      | Some delimiter ->
          [ ("delimiter", delimiter_to_yojson sources delimiter) ])
    @ [ ("location", location_to_yojson sources parameter.location) ])

and function_pointer_to_yojson sources ~name
    (function_pointer : Ast.function_pointer_declarator) =
  `Assoc
    ([
       ("kind", `String "function_pointer");
       ( "opening_parenthesis",
         location_to_yojson sources
           function_pointer.declarator_opening_parenthesis );
     ]
    @ pointer_layer_fields sources function_pointer.indirection_layers
    @ (match name with
      | None -> []
      | Some name -> [ ("name", identifier_to_yojson sources name) ])
    @ [
        ( "closing_parenthesis",
          location_to_yojson sources
            function_pointer.declarator_closing_parenthesis );
        ( "signature_opening_parenthesis",
          location_to_yojson sources
            function_pointer.signature_opening_parenthesis );
        ( "parameters",
          `List
            (List.map
               (parameter_to_yojson sources)
               function_pointer.signature_parameters) );
      ]
    @ (match function_pointer.signature_variadic with
      | None -> []
      | Some variadic -> [ ("variadic", variadic_to_yojson sources variadic) ])
    @ [
        ( "signature_closing_parenthesis",
          location_to_yojson sources
            function_pointer.signature_closing_parenthesis );
        ( "location",
          location_to_yojson sources function_pointer.function_pointer_location
        );
      ])

let item_to_yojson sources = function
  | Ast.Aggregate_forward_declaration declaration ->
      `Assoc
        ([
           ("kind", `String "aggregate_forward_declaration");
           ( "aggregate_kind",
             `String (aggregate_kind_name declaration.aggregate_kind) );
         ]
        @ modifier_fields sources declaration.modifiers
        @ [
            ("binding", binding_to_yojson sources declaration.binding);
            ( "aggregate_keyword",
              `Assoc
                [
                  ("spelling", `String declaration.aggregate_keyword_spelling);
                  ( "location",
                    location_to_yojson sources
                      declaration.aggregate_keyword_location );
                ] );
            ("name", identifier_to_yojson sources declaration.name);
            ("semicolon", location_to_yojson sources declaration.semicolon);
            ("location", location_to_yojson sources declaration.location);
          ])
  | Ast.Aggregate_definition definition ->
      `Assoc
        ([
           ("kind", `String "aggregate_definition");
           ( "aggregate_kind",
             `String (aggregate_kind_name definition.aggregate_kind) );
         ]
        @ modifier_fields sources definition.modifiers
        @ [
            ( "aggregate_keyword",
              `Assoc
                [
                  ("spelling", `String definition.aggregate_keyword_spelling);
                  ( "location",
                    location_to_yojson sources
                      definition.aggregate_keyword_location );
                ] );
            ("name", identifier_to_yojson sources definition.name);
            ( "opening_brace",
              location_to_yojson sources definition.opening_brace );
            ( "members",
              `List
                (List.map
                   (aggregate_member_to_yojson sources)
                   definition.members) );
            ( "closing_brace",
              location_to_yojson sources definition.closing_brace );
            ("semicolon", location_to_yojson sources definition.semicolon);
            ("location", location_to_yojson sources definition.location);
          ])
  | Ast.Global_variable variable ->
      `Assoc
        ([ ("kind", `String "global_variable") ]
        @ modifier_fields sources variable.modifiers
        @ binding_fields sources variable.binding
        @ [ ("type", type_to_yojson sources variable.type_specifier) ]
        @ pointer_layer_fields sources variable.pointer_layers
        @ [ ("name", identifier_to_yojson sources variable.name) ]
        @ array_dimension_fields sources variable.array_dimensions
        @ [
            ("semicolon", span_to_yojson sources variable.semicolon);
            ("location", location_to_yojson sources variable.location);
          ])
  | Ast.Global_declaration declaration ->
      `Assoc
        ([ ("kind", `String "global_declaration") ]
        @ modifier_fields sources declaration.modifiers
        @ binding_fields sources declaration.binding
        @ [
            ("type", type_to_yojson sources declaration.type_specifier);
            ( "declarators",
              `List
                (List.map
                   (declarator_to_yojson sources)
                   declaration.declarators) );
            ("location", location_to_yojson sources declaration.location);
          ])
  | Ast.Function_prototype prototype ->
      `Assoc
        ([ ("kind", `String "function_prototype") ]
        @ modifier_fields sources prototype.modifiers
        @ [ ("binding", binding_to_yojson sources prototype.binding) ]
        @ [ ("return_type", type_to_yojson sources prototype.return_type) ]
        @ pointer_layer_fields sources prototype.return_pointer_layers
        @ [
            ("name", identifier_to_yojson sources prototype.name);
            ( "opening_parenthesis",
              location_to_yojson sources prototype.opening_parenthesis );
            ( "parameters",
              `List
                (List.map (parameter_to_yojson sources) prototype.parameters) );
          ]
        @ (match prototype.variadic with
          | None -> []
          | Some variadic ->
              [ ("variadic", variadic_to_yojson sources variadic) ])
        @ [
            ( "closing_parenthesis",
              location_to_yojson sources prototype.closing_parenthesis );
            ("semicolon", location_to_yojson sources prototype.semicolon);
            ("location", location_to_yojson sources prototype.location);
          ])
  | Ast.Function_definition definition ->
      `Assoc
        ([ ("kind", `String "function_definition") ]
        @ modifier_fields sources definition.modifiers
        @ [ ("return_type", type_to_yojson sources definition.return_type) ]
        @ pointer_layer_fields sources definition.return_pointer_layers
        @ [
            ("name", identifier_to_yojson sources definition.name);
            ( "opening_parenthesis",
              location_to_yojson sources definition.opening_parenthesis );
            ( "parameters",
              `List
                (List.map (parameter_to_yojson sources) definition.parameters)
            );
          ]
        @ (match definition.variadic with
          | None -> []
          | Some variadic ->
              [ ("variadic", variadic_to_yojson sources variadic) ])
        @ [
            ( "closing_parenthesis",
              location_to_yojson sources definition.closing_parenthesis );
            ( "body",
              match definition.body with
              | None -> `Null
              | Some body -> statement_to_yojson sources body );
            ("location", location_to_yojson sources definition.location);
          ])
  | Ast.Top_level_statement statement ->
      `Assoc
        [
          ("kind", `String "top_level_statement");
          ("statement", statement_to_yojson sources statement);
          ( "location",
            location_to_yojson sources (Ast.statement_location statement) );
        ]

let to_yojson sources module_ =
  `Assoc
    [
      ("schema", `String schema);
      ("reference_commit", `String Sema.Primitive_type.reference_commit);
      ( "module",
        `Assoc
          [
            ("source_id", `Int (Common.Source_id.to_int module_.Ast.source));
            ("span", span_to_yojson sources module_.span);
            ("items", `List (List.map (item_to_yojson sources) module_.items));
          ] );
    ]

let json sources module_ =
  to_yojson sources module_ |> Yojson.Safe.pretty_to_string
