let schema = "holyc-function-frame-layout-v1"
let int64_to_yojson value = `Intlit (Int64.to_string value)

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

let segments_are_covered source =
  List.for_all
    (fun segment ->
      Common.Source_id.equal segment.Common.Span.source
        source.Symbol.span.source
      && source.span.start <= segment.start
      && segment.stop <= source.span.stop)
    source.Symbol.source_segments

let origin_text sources = function
  | Symbol.Pinned_source { path; line } -> Printf.sprintf "%s:%d" path line
  | Symbol.Synthesized description -> Printf.sprintf "<%s>" description
  | Symbol.Source_location source ->
      let primary = span_text sources source.span in
      let segments =
        if segments_are_covered source then ""
        else
          Printf.sprintf " segments=[%s]"
            (source.source_segments
            |> List.map (span_text sources)
            |> String.concat ",")
      in
      let generated_from =
        match source.generated_from with
        | None -> ""
        | Some span ->
            Printf.sprintf " generated_from=%s" (span_text sources span)
      in
      let defined_at =
        match source.defined_at with
        | None -> ""
        | Some span -> Printf.sprintf " defined_at=%s" (span_text sources span)
      in
      primary ^ segments ^ generated_from ^ defined_at

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
        (("path", `String (Common.Source_file.display_path source)) :: location
        @ base)

let origin_to_yojson sources = function
  | Symbol.Pinned_source { path; line } ->
      `Assoc
        [
          ("kind", `String "pinned-source");
          ("path", `String path);
          ("line", `Int line);
        ]
  | Symbol.Synthesized description ->
      `Assoc
        [
          ("kind", `String "synthesized"); ("description", `String description);
        ]
  | Symbol.Source_location source ->
      `Assoc
        ([
           ("kind", `String "source");
           ("span", span_to_yojson sources source.span);
           ( "source_segments",
             `List (List.map (span_to_yojson sources) source.source_segments) );
         ]
        @ (match source.generated_from with
          | None -> []
          | Some span -> [ ("generated_from", span_to_yojson sources span) ])
        @
        match source.defined_at with
        | None -> []
        | Some span -> [ ("defined_at", span_to_yojson sources span) ])

let primitive_spelling form primitive =
  match form with
  | Type.Public_spelling -> Primitive_type.to_string primitive
  | Type.Internal_storage -> (Primitive_type.info primitive).storage_spelling

let type_fields type_ =
  let pointer_depth = Type.pointer_depth type_ in
  match Type.base type_ with
  | Type.Primitive (form, primitive) ->
      ( "primitive",
        Some (Type.primitive_form_name form),
        primitive_spelling form primitive,
        None,
        pointer_depth )
  | Type.Aggregate symbol ->
      ( "aggregate",
        None,
        Symbol.name symbol,
        Some (Symbol.Id.to_int (Symbol.id symbol)),
        pointer_depth )

let type_to_yojson type_ =
  let kind, form, name, symbol_id, pointer_depth = type_fields type_ in
  `Assoc
    ([ ("kind", `String kind); ("name", `String name) ]
    @ (match form with
      | None -> []
      | Some form -> [ ("form", `String form) ])
    @ (match symbol_id with
      | None -> []
      | Some symbol_id -> [ ("symbol_id", `Int symbol_id) ])
    @ [ ("pointer_depth", `Int pointer_depth) ])

let type_reference_to_yojson sources = function
  | None -> `Null
  | Some type_reference ->
      `Assoc
        [
          ("spelling", `String (Type_reference.spelling type_reference));
          ( "spelling_origin",
            origin_to_yojson sources
              (Type_reference.spelling_origin type_reference) );
          ( "pointer_origins",
            `List
              (List.map (origin_to_yojson sources)
                 (Type_reference.pointer_origins type_reference)) );
        ]

let optional_int_to_yojson = function
  | None -> `Null
  | Some value -> `Int value

let dimension_to_yojson dimension =
  `Assoc
    [
      ( "kind",
        `String
          (Function_frame_layout.dimension_kind_name
             (Function_frame_layout.dimension_kind dimension)) );
      ( "value",
        int64_to_yojson (Function_frame_layout.dimension_value dimension) );
    ]

let frame_slot_to_yojson = function
  | None -> `Null
  | Some slot ->
      `Assoc
        [
          ( "displacement",
            int64_to_yojson
              (Function_frame_layout.frame_slot_displacement slot) );
          ( "size",
            int64_to_yojson (Function_frame_layout.frame_slot_size slot) );
        ]

let location_to_yojson sources location =
  let binding = Function_frame_layout.location_binding location in
  let symbol = Function_frame_layout.location_symbol location in
  let frame_slot = Function_frame_layout.location_frame_slot location in
  `Assoc
    [
      ("symbol_id", `Int (Symbol.Id.to_int (Symbol.id symbol)));
      ("name", `String (Symbol.name symbol));
      ( "binding_ordinal",
        `Int (Function_binding_index.binding_ordinal binding) );
      ( "kind",
        `String
          (Function_frame_layout.location_kind_name
             (Function_frame_layout.location_kind location)) );
      ( "parameter_index",
        optional_int_to_yojson
          (Function_binding_index.binding_parameter_index binding) );
      ( "local_declaration_index",
        optional_int_to_yojson
          (Function_binding_index.binding_local_declaration_index binding) );
      ( "local_declarator_index",
        optional_int_to_yojson
          (Function_binding_index.binding_local_declarator_index binding) );
      ( "type_reference",
        type_reference_to_yojson sources
          (Function_frame_layout.location_type_reference location) );
      ( "type",
        type_to_yojson (Function_frame_layout.location_checked_type location) );
      ( "declarator_shape",
        `String
          (Function_frame_layout.declarator_shape_name
             (Function_frame_layout.location_declarator_shape location)) );
      ( "value_shape",
        `String
          (Function_frame_layout.value_shape_name
             (Function_frame_layout.location_value_shape location)) );
      ( "dimensions",
        `List
          (List.map dimension_to_yojson
             (Function_frame_layout.location_dimensions location)) );
      ( "element_size",
        int64_to_yojson
          (Function_frame_layout.location_element_size location) );
      ( "allocated_size",
        int64_to_yojson
          (Function_frame_layout.location_allocated_size location) );
      ("alignment", `Int (Function_frame_layout.location_alignment location));
      ( "storage",
        `String (if Option.is_some frame_slot then "frame" else "static") );
      ("frame_slot", frame_slot_to_yojson frame_slot);
      ("origin", origin_to_yojson sources (Symbol.origin symbol));
    ]

let function_to_yojson sources function_ =
  let symbol = Function_frame_layout.function_symbol function_ in
  `Assoc
    [
      ("symbol_id", `Int (Symbol.Id.to_int (Symbol.id symbol)));
      ("name", `String (Symbol.name symbol));
      ( "scope_id",
        `Int
          (Symbol.Scope_id.to_int
             (Symbol_table.scope_id
                (Function_frame_layout.function_scope function_))) );
      ("item_index", `Int (Function_frame_layout.function_item_index function_));
      ( "frame_size",
        int64_to_yojson
          (Function_frame_layout.function_frame_size function_) );
      ("origin", origin_to_yojson sources (Symbol.origin symbol));
      ( "locations",
        `List
          (List.map (location_to_yojson sources)
             (Function_frame_layout.function_locations function_)) );
    ]

let to_yojson sources layouts =
  let functions = Function_frame_layout.functions layouts in
  `Assoc
    [
      ("schema", `String schema);
      ("reference_commit", `String Symbol.reference_commit);
      ("functions", `List (List.map (function_to_yojson sources) functions));
    ]

let json sources layouts =
  to_yojson sources layouts |> Yojson.Safe.pretty_to_string

let list_text render values =
  Printf.sprintf "[%s]" (values |> List.map render |> String.concat ",")

let option_text render = function
  | None -> "none"
  | Some value -> render value

let type_text type_ =
  let kind, form, name, symbol_id, pointer_depth = type_fields type_ in
  let form = Option.value form ~default:"none" in
  let symbol = option_text string_of_int symbol_id in
  Printf.sprintf
    "type_kind=%s type_form=%s type_name=%S type_symbol_id=%s pointer_depth=%d"
    kind form name symbol pointer_depth

let type_reference_text sources = function
  | None -> "type_reference=none"
  | Some type_reference ->
      Printf.sprintf
        "type_spelling=%S type_spelling_origin=%s type_pointer_origins=%s"
        (Type_reference.spelling type_reference)
        (origin_text sources (Type_reference.spelling_origin type_reference))
        (list_text (origin_text sources)
           (Type_reference.pointer_origins type_reference))

let dimension_text dimension =
  Printf.sprintf "%s:%Ld"
    (Function_frame_layout.dimension_kind_name
       (Function_frame_layout.dimension_kind dimension))
    (Function_frame_layout.dimension_value dimension)

let frame_slot_text = function
  | None -> ("static", "none", "none")
  | Some slot ->
      ( "frame",
        Int64.to_string
          (Function_frame_layout.frame_slot_displacement slot),
        Int64.to_string (Function_frame_layout.frame_slot_size slot) )

let print_location buffer sources location =
  let binding = Function_frame_layout.location_binding location in
  let symbol = Function_frame_layout.location_symbol location in
  let storage, displacement, slot_size =
    frame_slot_text (Function_frame_layout.location_frame_slot location)
  in
  Printf.bprintf buffer
    "  location symbol_id=%d name=%S binding_ordinal=%d kind=%s \
     parameter_index=%s local_declaration_index=%s local_declarator_index=%s \
     %s %s declarator_shape=%s value_shape=%s dimensions=%s \
     element_size=%Ld allocated_size=%Ld storage=%s displacement=%s \
     slot_size=%s alignment=%d origin=%s\n"
    (Symbol.Id.to_int (Symbol.id symbol))
    (Symbol.name symbol)
    (Function_binding_index.binding_ordinal binding)
    (Function_frame_layout.location_kind_name
       (Function_frame_layout.location_kind location))
    (option_text string_of_int
       (Function_binding_index.binding_parameter_index binding))
    (option_text string_of_int
       (Function_binding_index.binding_local_declaration_index binding))
    (option_text string_of_int
       (Function_binding_index.binding_local_declarator_index binding))
    (type_text (Function_frame_layout.location_checked_type location))
    (type_reference_text sources
       (Function_frame_layout.location_type_reference location))
    (Function_frame_layout.declarator_shape_name
       (Function_frame_layout.location_declarator_shape location))
    (Function_frame_layout.value_shape_name
       (Function_frame_layout.location_value_shape location))
    (list_text dimension_text
       (Function_frame_layout.location_dimensions location))
    (Function_frame_layout.location_element_size location)
    (Function_frame_layout.location_allocated_size location)
    storage displacement slot_size
    (Function_frame_layout.location_alignment location)
    (origin_text sources (Symbol.origin symbol))

let print_function buffer sources function_ =
  let symbol = Function_frame_layout.function_symbol function_ in
  let locations = Function_frame_layout.function_locations function_ in
  Printf.bprintf buffer
    "function symbol_id=%d name=%S scope_id=%d item_index=%d frame_size=%Ld \
     location_count=%d origin=%s\n"
    (Symbol.Id.to_int (Symbol.id symbol))
    (Symbol.name symbol)
    (Symbol.Scope_id.to_int
       (Symbol_table.scope_id (Function_frame_layout.function_scope function_)))
    (Function_frame_layout.function_item_index function_)
    (Function_frame_layout.function_frame_size function_)
    (List.length locations)
    (origin_text sources (Symbol.origin symbol));
  List.iter (print_location buffer sources) locations

let human sources layouts =
  let functions = Function_frame_layout.functions layouts in
  let buffer = Buffer.create 1024 in
  Printf.bprintf buffer "%s\nreference_commit=%s\nfunctions=%d\n" schema
    Symbol.reference_commit (List.length functions);
  List.iter (print_function buffer sources) functions;
  Buffer.contents buffer
