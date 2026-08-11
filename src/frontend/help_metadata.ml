type provenance = {
  directive_span : Common.Span.t;
  value_spans : Common.Span.t list;
  include_stack : Common.Diagnostic.related list;
  definition_trace : Common.Diagnostic.related list;
}

type index_entry = { value : string; provenance : provenance }

type file_entry = {
  declared_path : string;
  effective_path : string;
  resolved_path : string;
  source_link : string;
  index : index_entry option;
  provenance : provenance;
}

type t = {
  current_index : index_entry option;
  index_events_rev : index_entry list;
  help_files_rev : file_entry list;
}

let empty = { current_index = None; index_events_rev = []; help_files_rev = [] }

let record_index metadata entry =
  {
    metadata with
    current_index = (if String.equal entry.value "" then None else Some entry);
    index_events_rev = entry :: metadata.index_events_rev;
  }

let record_file metadata entry =
  { metadata with help_files_rev = entry :: metadata.help_files_rev }

let current_index metadata = metadata.current_index
let index_events metadata = List.rev metadata.index_events_rev
let help_files metadata = List.rev metadata.help_files_rev

let has_templeos_extension path =
  let length = String.length path in
  let offset = ref 0 in
  let found = ref false in
  while !offset < length && not !found do
    (match path.[!offset] with
    | '.' ->
        let next = !offset + 1 in
        if
          next = length
          || not (Char.equal path.[next] '/' || Char.equal path.[next] '.')
        then found := true
    | _ -> ());
    incr offset
  done;
  !found

let with_default_extension path =
  if has_templeos_extension path then path else path ^ ".DD.Z"

let is_templeos_whitespace = function
  | '\t' | '\n' | '\r' | '\x1f' | ' ' -> true
  | _ -> false

let sanitize_file_name path =
  let length = String.length path in
  let start = ref 0 in
  while !start < length && is_templeos_whitespace path.[!start] do
    incr start
  done;
  let stop = ref length in
  while !stop > 0 && is_templeos_whitespace path.[!stop - 1] do
    decr stop
  done;
  let buffer = Buffer.create (max 0 (!stop - !start)) in
  for offset = !start to !stop - 1 do
    let byte = path.[offset] in
    let code = Char.code byte in
    if code = 0 || is_templeos_whitespace byte || code >= 0x1f then
      Buffer.add_char buffer byte
  done;
  Buffer.contents buffer

let span_position sources span =
  match Common.Source_manager.find sources span.Common.Span.source with
  | None ->
      Printf.sprintf "source-%d:%d..%d"
        (Common.Source_id.to_int span.source)
        span.start span.stop
  | Some source -> (
      match
        ( Common.Source_file.position source span.start,
          Common.Source_file.position source span.stop )
      with
      | Ok start, Ok stop ->
          Printf.sprintf "%s:%d:%d..%d:%d"
            (Common.Source_file.display_path source)
            start.line start.column stop.line stop.column
      | _ ->
          Printf.sprintf "%s:%d..%d"
            (Common.Source_file.display_path source)
            span.start span.stop)

let source_link sources span =
  match Common.Source_manager.find sources span.Common.Span.source with
  | None ->
      Printf.sprintf "FL:source-%d,1" (Common.Source_id.to_int span.source)
  | Some source ->
      let line =
        match Common.Source_file.position source span.start with
        | Ok position -> position.line
        | Error _ -> 1
      in
      Printf.sprintf "FL:%s,%d" (Common.Source_file.path source) line

let print_provenance sources buffer provenance =
  Printf.bprintf buffer " directive=%s"
    (span_position sources provenance.directive_span);
  List.iter
    (fun span ->
      Printf.bprintf buffer "\n  value=%s" (span_position sources span))
    provenance.value_spans;
  List.iter
    (fun (related : Common.Diagnostic.related) ->
      Printf.bprintf buffer "\n  included_from=%s message=%S"
        (span_position sources related.span)
        related.message)
    provenance.include_stack;
  List.iter
    (fun (related : Common.Diagnostic.related) ->
      Printf.bprintf buffer "\n  definition=%s message=%S"
        (span_position sources related.span)
        related.message)
    provenance.definition_trace

let human sources metadata =
  let buffer = Buffer.create 256 in
  Buffer.add_string buffer "holyc-help-metadata-v1\n";
  (match current_index metadata with
  | None -> Buffer.add_string buffer "current_index=none\n"
  | Some entry -> Printf.bprintf buffer "current_index=%S\n" entry.value);
  List.iteri
    (fun position entry ->
      Printf.bprintf buffer "index %d value=%S" position entry.value;
      print_provenance sources buffer entry.provenance;
      Buffer.add_char buffer '\n')
    (index_events metadata);
  List.iteri
    (fun position entry ->
      let index =
        match entry.index with
        | None -> "none"
        | Some item -> Printf.sprintf "%S" item.value
      in
      Printf.bprintf buffer
        "file %d declared=%S effective=%S resolved=%S source_link=%S index=%s"
        position entry.declared_path entry.effective_path entry.resolved_path
        entry.source_link index;
      print_provenance sources buffer entry.provenance;
      Buffer.add_char buffer '\n')
    (help_files metadata);
  Buffer.contents buffer

let span_json sources span =
  let fields =
    [
      ("source_id", `Int (Common.Source_id.to_int span.Common.Span.source));
      ("start", `Int span.start);
      ("stop", `Int span.stop);
    ]
  in
  match Common.Source_manager.find sources span.source with
  | None -> `Assoc fields
  | Some source -> (
      match Common.Source_file.position source span.start with
      | Error _ ->
          `Assoc
            (("path", `String (Common.Source_file.display_path source))
            :: fields)
      | Ok position ->
          `Assoc
            ([
               ("path", `String (Common.Source_file.display_path source));
               ("line", `Int position.line);
               ("column", `Int position.column);
             ]
            @ fields))

let related_json sources (related : Common.Diagnostic.related) =
  `Assoc
    [
      ("message", `String related.message);
      ("span", span_json sources related.span);
    ]

let provenance_json sources provenance =
  `Assoc
    [
      ("directive", span_json sources provenance.directive_span);
      ("values", `List (List.map (span_json sources) provenance.value_spans));
      ( "include_stack",
        `List (List.map (related_json sources) provenance.include_stack) );
      ( "definition_trace",
        `List (List.map (related_json sources) provenance.definition_trace) );
    ]

let index_json sources entry =
  `Assoc
    [
      ("value", `String entry.value);
      ("provenance", provenance_json sources entry.provenance);
    ]

let file_json sources entry =
  `Assoc
    [
      ("declared_path", `String entry.declared_path);
      ("effective_path", `String entry.effective_path);
      ("resolved_path", `String entry.resolved_path);
      ("source_link", `String entry.source_link);
      ( "index",
        match entry.index with
        | None -> `Null
        | Some index -> index_json sources index );
      ("provenance", provenance_json sources entry.provenance);
    ]

let to_yojson sources metadata =
  `Assoc
    [
      ("schema", `String "holyc-help-metadata-v1");
      ( "current_index",
        match current_index metadata with
        | None -> `Null
        | Some entry -> index_json sources entry );
      ( "index_events",
        `List (List.map (index_json sources) (index_events metadata)) );
      ("help_files", `List (List.map (file_json sources) (help_files metadata)));
    ]

let json sources metadata =
  to_yojson sources metadata |> Yojson.Safe.pretty_to_string
