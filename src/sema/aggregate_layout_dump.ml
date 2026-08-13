let schema = "holyc-aggregate-layout-v1"
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
        ((("path", `String (Common.Source_file.display_path source)) :: location)
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

let aggregate_kind_name = function
  | Aggregate_layout.Class -> "class"
  | Aggregate_layout.Union -> "union"

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

let list_text render values =
  Printf.sprintf "[%s]" (values |> List.map render |> String.concat ",")

let type_text type_ =
  let kind, form, name, symbol_id, pointer_depth = type_fields type_ in
  let form = Option.value form ~default:"none" in
  let symbol =
    match symbol_id with
    | None -> "none"
    | Some value -> string_of_int value
  in
  Printf.sprintf "type_kind=%s type_form=%s type_name=%S type_symbol=%s ptr=%d"
    kind form name symbol pointer_depth

let base_to_yojson sources (base : Aggregate_layout.base_layout) =
  `Assoc
    [
      ("symbol_id", `Int (Symbol.Id.to_int (Symbol.id base.symbol)));
      ("name", `String (Symbol.name base.symbol));
      ("offset", int64_to_yojson base.offset);
      ("size", int64_to_yojson base.size);
      ("origin", origin_to_yojson sources base.origin);
    ]

let member_to_yojson sources (member : Aggregate_member_index.member) =
  let layout = member.layout in
  `Assoc
    [
      ("symbol_id", `Int (Symbol.Id.to_int (Symbol.id member.symbol)));
      ("name", `String (Symbol.name member.symbol));
      ( "declaring_aggregate_id",
        `Int (Symbol.Id.to_int (Symbol.id member.declaring_aggregate)) );
      ("path", `List (List.map (fun value -> `Int value) layout.path));
      ("declarator_index", `Int layout.declarator_index);
      ("type", type_to_yojson member.member_type);
      ("function_pointer", `Bool member.is_function_pointer);
      ("offset", int64_to_yojson layout.offset);
      ("size", int64_to_yojson layout.size);
      ("element_size", int64_to_yojson layout.element_size);
      ("dimensions", `List (List.map int64_to_yojson layout.dimensions));
      ( "signedness",
        `String (Aggregate_layout.signedness_name layout.signedness) );
      ("alignment", `Int layout.alignment);
      ("origin", origin_to_yojson sources layout.origin);
    ]

let aggregate_to_yojson sources (aggregate : Aggregate_member_index.aggregate) =
  let layout = aggregate.layout in
  `Assoc
    [
      ("symbol_id", `Int (Symbol.Id.to_int (Symbol.id aggregate.symbol)));
      ("name", `String (Symbol.name aggregate.symbol));
      ("kind", `String (aggregate_kind_name layout.kind));
      ("item_index", `Int aggregate.item_index);
      ("size", int64_to_yojson layout.size);
      ("alignment", `Int layout.alignment);
      ("negative_offset", int64_to_yojson layout.negative_offset);
      ( "base",
        match layout.base with
        | None -> `Null
        | Some base -> base_to_yojson sources base );
      ("origin", origin_to_yojson sources layout.origin);
      ( "members",
        `List (List.map (member_to_yojson sources) aggregate.direct_members) );
    ]

let to_yojson sources index =
  let aggregates = Aggregate_member_index.aggregates index in
  `Assoc
    [
      ("schema", `String schema);
      ("reference_commit", `String Symbol.reference_commit);
      ("aggregates", `List (List.map (aggregate_to_yojson sources) aggregates));
    ]

let json sources index = to_yojson sources index |> Yojson.Safe.pretty_to_string

let print_base buffer sources = function
  | None -> Buffer.add_string buffer "  base none\n"
  | Some (base : Aggregate_layout.base_layout) ->
      Printf.bprintf buffer
        "  base id=%d name=%S offset=%Ld size=%Ld origin=%s\n"
        (Symbol.Id.to_int (Symbol.id base.symbol))
        (Symbol.name base.symbol) base.offset base.size
        (origin_text sources base.origin)

let print_member buffer sources (member : Aggregate_member_index.member) =
  let layout = member.layout in
  Printf.bprintf buffer
    "  member id=%d name=%S declaring=%d path=%s declarator=%d %s callback=%B \
     offset=%Ld size=%Ld element_size=%Ld dimensions=%s signedness=%s \
     alignment=%d origin=%s\n"
    (Symbol.Id.to_int (Symbol.id member.symbol))
    (Symbol.name member.symbol)
    (Symbol.Id.to_int (Symbol.id member.declaring_aggregate))
    (list_text string_of_int layout.path)
    layout.declarator_index
    (type_text member.member_type)
    member.is_function_pointer layout.offset layout.size layout.element_size
    (list_text Int64.to_string layout.dimensions)
    (Aggregate_layout.signedness_name layout.signedness)
    layout.alignment
    (origin_text sources layout.origin)

let print_aggregate buffer sources
    (aggregate : Aggregate_member_index.aggregate) =
  let layout = aggregate.layout in
  Printf.bprintf buffer
    "aggregate id=%d name=%S kind=%s item=%d size=%Ld alignment=%d \
     negative_offset=%Ld origin=%s\n"
    (Symbol.Id.to_int (Symbol.id aggregate.symbol))
    (Symbol.name aggregate.symbol)
    (aggregate_kind_name layout.kind)
    aggregate.item_index layout.size layout.alignment layout.negative_offset
    (origin_text sources layout.origin);
  print_base buffer sources layout.base;
  List.iter (print_member buffer sources) aggregate.direct_members

let human sources index =
  let aggregates = Aggregate_member_index.aggregates index in
  let buffer = Buffer.create 1024 in
  Printf.bprintf buffer "%s\nreference_commit=%s\naggregates=%d\n" schema
    Symbol.reference_commit (List.length aggregates);
  List.iter (print_aggregate buffer sources) aggregates;
  Buffer.contents buffer
