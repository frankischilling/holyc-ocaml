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

let print_type buffer sources ~indent primitive =
  Printf.bprintf buffer "%stype primitive=%s spelling=%S span=%s\n" indent
    (Sema.Primitive_type.to_string primitive.Ast.primitive)
    primitive.spelling
    (location_text sources primitive.location)

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
  | Ast.Call_expression call ->
      Printf.bprintf buffer "%sexpression kind=call arguments=%d span=%s\n"
        indent
        (List.length call.call_arguments)
        (location_text sources call.call_location);
      Printf.bprintf buffer "%scallee\n" child_indent;
      print_expression buffer sources ~indent:(child_indent ^ "  ")
        call.call_callee;
      Printf.bprintf buffer "%sopening_parenthesis span=%s\n" child_indent
        (location_text sources call.call_opening_parenthesis);
      List.iteri
        (print_call_argument buffer sources ~indent:child_indent)
        call.call_arguments;
      Printf.bprintf buffer "%sclosing_parenthesis span=%s\n" child_indent
        (location_text sources call.call_closing_parenthesis)
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

let print_parameter_default buffer sources ~indent
    (default : Ast.parameter_default) =
  let child_indent = indent ^ "  " in
  Printf.bprintf buffer "%sdefault span=%s\n" indent
    (location_text sources default.location);
  Printf.bprintf buffer "%sequals span=%s\n" child_indent
    (location_text sources default.equals);
  print_expression buffer sources ~indent:child_indent default.value

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
          Printf.bprintf buffer
            "    return_type primitive=%s spelling=%S span=%s\n"
            (Sema.Primitive_type.to_string prototype.return_type.primitive)
            prototype.return_type.spelling
            (location_text sources prototype.return_type.location);
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
            (location_text sources prototype.semicolon))
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
           ("target_type", primitive_to_yojson sources cast.cast_type);
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
      `Assoc
        [
          ("kind", `String "call");
          ("callee", expression_to_yojson sources call.call_callee);
          ( "opening_parenthesis",
            location_to_yojson sources call.call_opening_parenthesis );
          ( "arguments",
            `List
              (List.map (call_argument_to_yojson sources) call.call_arguments)
          );
          ( "closing_parenthesis",
            location_to_yojson sources call.call_closing_parenthesis );
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

let parameter_default_to_yojson sources (default : Ast.parameter_default) =
  `Assoc
    [
      ("equals", location_to_yojson sources default.equals);
      ("value", expression_to_yojson sources default.value);
      ("location", location_to_yojson sources default.location);
    ]

let rec parameter_to_yojson sources (parameter : Ast.function_parameter) =
  `Assoc
    (register_qualifier_fields sources parameter.register_qualifiers
    @ [ ("type", primitive_to_yojson sources parameter.type_specifier) ]
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
  | Ast.Global_variable variable ->
      `Assoc
        ([ ("kind", `String "global_variable") ]
        @ modifier_fields sources variable.modifiers
        @ binding_fields sources variable.binding
        @ [ ("type", primitive_to_yojson sources variable.type_specifier) ]
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
            ("type", primitive_to_yojson sources declaration.type_specifier);
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
        @ [ ("return_type", primitive_to_yojson sources prototype.return_type) ]
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
