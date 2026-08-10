type severity = Error | Warning | Note
type related = { span : Span.t; message : string }

type t = {
  code : string;
  severity : severity;
  message : string;
  primary : Span.t;
  secondary : related list;
  notes : string list;
  help : string option;
}

let make ?(secondary = []) ?(notes = []) ?help ~code ~severity ~message ~primary
    () =
  { code; severity; message; primary; secondary; notes; help }

let severity_name = function
  | Error -> "error"
  | Warning -> "warning"
  | Note -> "note"

let span_to_yojson sources span =
  let base =
    [
      ("source_id", `Int (Source_id.to_int span.Span.source));
      ("start", `Int span.start);
      ("stop", `Int span.stop);
    ]
  in
  match Source_manager.find sources span.source with
  | None -> `Assoc base
  | Some source ->
      let position = Source_file.position source span.start in
      let location =
        match position with
        | Error _ -> []
        | Ok item -> [ ("line", `Int item.line); ("column", `Int item.column) ]
      in
      `Assoc
        ((("path", `String (Source_file.display_path source)) :: location)
        @ base)

let related_to_yojson sources (item : related) =
  `Assoc
    [
      ("message", `String item.message);
      ("span", span_to_yojson sources item.span);
    ]

let to_yojson sources diagnostic =
  `Assoc
    [
      ("code", `String diagnostic.code);
      ("severity", `String (severity_name diagnostic.severity));
      ("message", `String diagnostic.message);
      ("primary", span_to_yojson sources diagnostic.primary);
      ( "secondary",
        `List (List.map (related_to_yojson sources) diagnostic.secondary) );
      ("notes", `List (List.map (fun note -> `String note) diagnostic.notes));
      ( "help",
        match diagnostic.help with
        | None -> `Null
        | Some help -> `String help );
    ]
