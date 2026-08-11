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

let binding_kind_name = function
  | Ast.Extern -> "extern"
  | Ast.Import -> "import"

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

let print_binding buffer sources ~indent = function
  | None -> ()
  | Some (binding : Ast.declaration_binding) ->
      Printf.bprintf buffer "%sbinding kind=%s spelling=%S span=%s\n" indent
        (binding_kind_name binding.kind)
        binding.spelling
        (location_text sources binding.location);
      Option.iter
        (fun (target : Ast.identifier) ->
          Printf.bprintf buffer "%s  target spelling=%S span=%s\n" indent
            target.spelling
            (location_text sources target.location))
        binding.target

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
            (fun index (parameter : Ast.function_parameter) ->
              Printf.bprintf buffer "    parameter index=%d span=%s\n" index
                (location_text sources parameter.location);
              print_register_qualifiers buffer sources ~indent:"      "
                ~position:Ast.Before_type parameter.register_qualifiers;
              print_type buffer sources ~indent:"      "
                parameter.type_specifier;
              print_register_qualifiers buffer sources ~indent:"      "
                ~position:Ast.After_type parameter.register_qualifiers;
              print_pointer_layers buffer sources ~indent:"      "
                parameter.pointer_layers;
              (match parameter.name with
              | Some name ->
                  Printf.bprintf buffer "      name spelling=%S span=%s\n"
                    name.spelling
                    (location_text sources name.location)
              | None -> Buffer.add_string buffer "      name omitted\n");
              Option.iter
                (fun (delimiter : Ast.declaration_delimiter) ->
                  Printf.bprintf buffer
                    "      delimiter kind=%s spelling=%S span=%s\n"
                    (delimiter_kind_name delimiter.kind)
                    delimiter.spelling
                    (location_text sources delimiter.location))
                parameter.delimiter)
            prototype.parameters;
          Option.iter
            (fun variadic ->
              Printf.bprintf buffer "    variadic spelling=%S span=%s\n"
                variadic.Ast.spelling
                (location_text sources variadic.location);
              print_register_qualifiers buffer sources ~indent:"      "
                ~position:Ast.Before_type variadic.register_qualifiers)
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

let binding_to_yojson sources (binding : Ast.declaration_binding) =
  `Assoc
    ([
       ("kind", `String (binding_kind_name binding.kind));
       ("spelling", `String binding.spelling);
       ("location", location_to_yojson sources binding.location);
     ]
    @
    match binding.target with
    | None -> []
    | Some target -> [ ("target", identifier_to_yojson sources target) ])

let binding_fields sources = function
  | None -> []
  | Some binding -> [ ("binding", binding_to_yojson sources binding) ]

let delimiter_to_yojson sources (delimiter : Ast.declaration_delimiter) =
  `Assoc
    [
      ("kind", `String (delimiter_kind_name delimiter.kind));
      ("spelling", `String delimiter.spelling);
      ("location", location_to_yojson sources delimiter.location);
    ]

let declarator_to_yojson sources (declarator : Ast.global_declarator) =
  `Assoc
    (pointer_layer_fields sources declarator.pointer_layers
    @ [
        ("name", identifier_to_yojson sources declarator.name);
        ("delimiter", delimiter_to_yojson sources declarator.delimiter);
        ("location", location_to_yojson sources declarator.location);
      ])

let parameter_to_yojson sources (parameter : Ast.function_parameter) =
  `Assoc
    (register_qualifier_fields sources parameter.register_qualifiers
    @ [ ("type", primitive_to_yojson sources parameter.type_specifier) ]
    @ pointer_layer_fields sources parameter.pointer_layers
    @ (match parameter.name with
      | None -> []
      | Some name -> [ ("name", identifier_to_yojson sources name) ])
    @ (match parameter.delimiter with
      | None -> []
      | Some delimiter ->
          [ ("delimiter", delimiter_to_yojson sources delimiter) ])
    @ [ ("location", location_to_yojson sources parameter.location) ])

let variadic_to_yojson sources (variadic : Ast.variadic_marker) =
  `Assoc
    ([ ("spelling", `String variadic.spelling) ]
    @ register_qualifier_fields sources variadic.register_qualifiers
    @ [ ("location", location_to_yojson sources variadic.location) ])

let item_to_yojson sources = function
  | Ast.Global_variable variable ->
      `Assoc
        ([ ("kind", `String "global_variable") ]
        @ modifier_fields sources variable.modifiers
        @ binding_fields sources variable.binding
        @ [ ("type", primitive_to_yojson sources variable.type_specifier) ]
        @ pointer_layer_fields sources variable.pointer_layers
        @ [
            ("name", identifier_to_yojson sources variable.name);
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
