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
          Printf.bprintf buffer "    type primitive=%s spelling=%S span=%s\n"
            (Sema.Primitive_type.to_string variable.type_specifier.primitive)
            variable.type_specifier.spelling
            (location_text sources variable.type_specifier.location);
          List.iter
            (fun pointer ->
              Printf.bprintf buffer
                "    pointer_layer depth=%d spelling=%S span=%s\n"
                pointer.Ast.depth pointer.spelling
                (location_text sources pointer.location))
            variable.pointer_layers;
          Printf.bprintf buffer "    name spelling=%S span=%s\n"
            variable.name.spelling
            (location_text sources variable.name.location);
          Printf.bprintf buffer "    semicolon span=%s\n"
            (span_text sources variable.semicolon))
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

let item_to_yojson sources = function
  | Ast.Global_variable variable ->
      let pointer_layers =
        match variable.pointer_layers with
        | [] -> []
        | pointers ->
            [
              ( "pointer_layers",
                `List (List.map (pointer_layer_to_yojson sources) pointers) );
            ]
      in
      `Assoc
        ([
           ("kind", `String "global_variable");
           ("type", primitive_to_yojson sources variable.type_specifier);
         ]
        @ pointer_layers
        @ [
            ("name", identifier_to_yojson sources variable.name);
            ("semicolon", span_to_yojson sources variable.semicolon);
            ("location", location_to_yojson sources variable.location);
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
