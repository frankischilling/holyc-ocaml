let schema = "holyc-function-frame-layout-v1"
let int64_to_yojson value = `Intlit (Int64.to_string value)

module Binding = Function_binding_index
module Frame = Function_frame_layout

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
      let path = ("path", `String (Common.Source_file.display_path source)) in
      `Assoc ((path :: location) @ base)

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
  let kind = Frame.dimension_kind_name (Frame.dimension_kind dimension) in
  let value = int64_to_yojson (Frame.dimension_value dimension) in
  `Assoc [ ("kind", `String kind); ("value", value) ]

let frame_slot_to_yojson = function
  | None -> `Null
  | Some slot ->
      let displacement = Frame.frame_slot_displacement slot in
      let size = Frame.frame_slot_size slot in
      `Assoc
        [
          ("displacement", int64_to_yojson displacement);
          ("size", int64_to_yojson size);
        ]

let location_to_yojson sources location =
  let binding = Frame.location_binding location in
  let symbol = Frame.location_symbol location in
  let frame_slot = Frame.location_frame_slot location in
  let binding_ordinal = Binding.binding_ordinal binding in
  let kind = Frame.location_kind_name (Frame.location_kind location) in
  let parameter_index = Binding.binding_parameter_index binding in
  let parameter_index = optional_int_to_yojson parameter_index in
  let declaration_index = Binding.binding_local_declaration_index binding in
  let declaration_index = optional_int_to_yojson declaration_index in
  let declarator_index = Binding.binding_local_declarator_index binding in
  let declarator_index = optional_int_to_yojson declarator_index in
  let type_reference = Frame.location_type_reference location in
  let type_reference = type_reference_to_yojson sources type_reference in
  let checked_type = type_to_yojson (Frame.location_checked_type location) in
  let declarator_shape = Frame.location_declarator_shape location in
  let declarator_shape = Frame.declarator_shape_name declarator_shape in
  let value_shape = Frame.location_value_shape location in
  let value_shape = Frame.value_shape_name value_shape in
  let dimensions = Frame.location_dimensions location in
  let dimensions = List.map dimension_to_yojson dimensions in
  let element_size = Frame.location_element_size location in
  let allocated_size = Frame.location_allocated_size location in
  let alignment = Frame.location_alignment location in
  let storage = if Option.is_some frame_slot then "frame" else "static" in
  let frame_slot = frame_slot_to_yojson frame_slot in
  let origin = origin_to_yojson sources (Symbol.origin symbol) in
  `Assoc
    [
      ("symbol_id", `Int (Symbol.Id.to_int (Symbol.id symbol)));
      ("name", `String (Symbol.name symbol));
      ("binding_ordinal", `Int binding_ordinal);
      ("kind", `String kind);
      ("parameter_index", parameter_index);
      ("local_declaration_index", declaration_index);
      ("local_declarator_index", declarator_index);
      ("type_reference", type_reference);
      ("type", checked_type);
      ("declarator_shape", `String declarator_shape);
      ("value_shape", `String value_shape);
      ("dimensions", `List dimensions);
      ("element_size", int64_to_yojson element_size);
      ("allocated_size", int64_to_yojson allocated_size);
      ("alignment", `Int alignment);
      ("storage", `String storage);
      ("frame_slot", frame_slot);
      ("origin", origin);
    ]

let function_to_yojson sources function_ =
  let symbol = Frame.function_symbol function_ in
  let scope = Frame.function_scope function_ in
  let scope_id = Symbol.Scope_id.to_int (Symbol_table.scope_id scope) in
  let item_index = Frame.function_item_index function_ in
  let frame_size = int64_to_yojson (Frame.function_frame_size function_) in
  let origin = origin_to_yojson sources (Symbol.origin symbol) in
  let locations = Frame.function_locations function_ in
  let locations = List.map (location_to_yojson sources) locations in
  `Assoc
    [
      ("symbol_id", `Int (Symbol.Id.to_int (Symbol.id symbol)));
      ("name", `String (Symbol.name symbol));
      ("scope_id", `Int scope_id);
      ("item_index", `Int item_index);
      ("frame_size", frame_size);
      ("origin", origin);
      ("locations", `List locations);
    ]

let to_yojson sources layouts =
  let functions = Frame.functions layouts in
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
    (Frame.dimension_kind_name (Frame.dimension_kind dimension))
    (Frame.dimension_value dimension)

let frame_slot_text = function
  | None -> ("static", "none", "none")
  | Some slot ->
      ( "frame",
        Int64.to_string (Frame.frame_slot_displacement slot),
        Int64.to_string (Frame.frame_slot_size slot) )

let print_location buffer sources location =
  let binding = Frame.location_binding location in
  let symbol = Frame.location_symbol location in
  let storage, displacement, slot_size =
    frame_slot_text (Frame.location_frame_slot location)
  in
  let symbol_id = Symbol.Id.to_int (Symbol.id symbol) in
  let name = Symbol.name symbol in
  let binding_ordinal = Binding.binding_ordinal binding in
  let kind = Frame.location_kind_name (Frame.location_kind location) in
  let parameter_index = Binding.binding_parameter_index binding in
  let parameter_index = option_text string_of_int parameter_index in
  let declaration_index = Binding.binding_local_declaration_index binding in
  let declaration_index = option_text string_of_int declaration_index in
  let declarator_index = Binding.binding_local_declarator_index binding in
  let declarator_index = option_text string_of_int declarator_index in
  let checked_type = type_text (Frame.location_checked_type location) in
  let type_reference = Frame.location_type_reference location in
  let type_reference = type_reference_text sources type_reference in
  let declarator_shape = Frame.location_declarator_shape location in
  let declarator_shape = Frame.declarator_shape_name declarator_shape in
  let value_shape = Frame.location_value_shape location in
  let value_shape = Frame.value_shape_name value_shape in
  let dimensions = Frame.location_dimensions location in
  let dimensions = list_text dimension_text dimensions in
  let element_size = Frame.location_element_size location in
  let allocated_size = Frame.location_allocated_size location in
  let alignment = Frame.location_alignment location in
  let origin = origin_text sources (Symbol.origin symbol) in
  Printf.bprintf buffer "  location symbol_id=%d name=%S " symbol_id name;
  Printf.bprintf buffer "binding_ordinal=%d kind=%s " binding_ordinal kind;
  Printf.bprintf buffer
    "parameter_index=%s local_declaration_index=%s local_declarator_index=%s \
     %s %s declarator_shape=%s value_shape=%s dimensions=%s "
    parameter_index declaration_index declarator_index checked_type
    type_reference declarator_shape value_shape dimensions;
  Printf.bprintf buffer
    "element_size=%Ld allocated_size=%Ld storage=%s displacement=%s \
     slot_size=%s alignment=%d origin=%s\n"
    element_size allocated_size storage displacement slot_size alignment origin

let print_function buffer sources function_ =
  let symbol = Frame.function_symbol function_ in
  let locations = Frame.function_locations function_ in
  Printf.bprintf buffer
    "function symbol_id=%d name=%S scope_id=%d item_index=%d frame_size=%Ld \
     location_count=%d origin=%s\n"
    (Symbol.Id.to_int (Symbol.id symbol))
    (Symbol.name symbol)
    (Symbol.Scope_id.to_int
       (Symbol_table.scope_id (Frame.function_scope function_)))
    (Frame.function_item_index function_)
    (Frame.function_frame_size function_)
    (List.length locations)
    (origin_text sources (Symbol.origin symbol));
  List.iter (print_location buffer sources) locations

let human sources layouts =
  let functions = Frame.functions layouts in
  let buffer = Buffer.create 1024 in
  Printf.bprintf buffer "%s\nreference_commit=%s\nfunctions=%d\n" schema
    Symbol.reference_commit (List.length functions);
  List.iter (print_function buffer sources) functions;
  Buffer.contents buffer
