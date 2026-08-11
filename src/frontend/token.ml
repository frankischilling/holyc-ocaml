type mode = Holyc | Assembler | Raw

type value =
  | No_value
  | Text of string
  | Int64 of int64
  | Float64 of float
  | Bytes of string

type origin = {
  frame : Common.Source_id.t;
  generated_from : Common.Span.t option;
  defined_at : Common.Span.t option;
}

type t = {
  kind : Token_kind.t;
  raw : string;
  value : value;
  span : Common.Span.t;
  source_segments : Common.Span.t list;
  origin : origin;
  leading_trivia : Trivia.t list;
  mode : mode;
}

let mode_name = function
  | Holyc -> "holyc"
  | Assembler -> "assembler"
  | Raw -> "raw"

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

let value_text = function
  | No_value -> None
  | Text text -> Some text
  | Int64 value -> Some (Printf.sprintf "0x%016Lx" value)
  | Float64 value -> Some (Printf.sprintf "%.17g" value)
  | Bytes bytes -> Some (escaped_bytes bytes)

let span_location sources span =
  match Common.Source_manager.find sources span.Common.Span.source with
  | None -> ("<unknown>", 1, 1, 1, 1)
  | Some source ->
      let start = Common.Source_file.position source span.start in
      let stop = Common.Source_file.position source span.stop in
      let get result =
        Result.value result
          ~default:{ Common.Source_file.offset = 0; line = 1; column = 1 }
      in
      let start = get start in
      let stop = get stop in
      ( Common.Source_file.display_path source,
        start.line,
        start.column,
        stop.line,
        stop.column )

let location sources token = span_location sources token.span

let span_text sources span =
  let path, start_line, start_column, stop_line, stop_column =
    span_location sources span
  in
  Printf.sprintf "%s:%d:%d..%d:%d" path start_line start_column stop_line
    stop_column

let segments_text sources label spans =
  match spans with
  | [] | [ _ ] -> ""
  | spans ->
      Printf.sprintf " %s=[%s]" label
        (spans |> List.map (span_text sources) |> String.concat ",")

let human sources token =
  let path, start_line, start_column, stop_line, stop_column =
    location sources token
  in
  let value =
    match value_text token.value with
    | None -> ""
    | Some text -> Printf.sprintf " value=%S" text
  in
  let source_segments =
    segments_text sources "source_segments" token.source_segments
  in
  let trivia_segments =
    token.leading_trivia
    |> List.filter_map (fun trivia ->
        let differs_from_token =
          match trivia.Trivia.source_segments with
          | [] -> false
          | span :: _ ->
              not
                (Common.Source_id.equal span.Common.Span.source
                   token.span.source)
        in
        if List.length trivia.source_segments > 1 || differs_from_token then
          Some
            (Printf.sprintf "%s:{%s}"
               (Trivia.kind_name trivia.kind)
               (trivia.source_segments
               |> List.map (span_text sources)
               |> String.concat ","))
        else None)
  in
  let trivia_segments =
    match trivia_segments with
    | [] -> ""
    | segments ->
        Printf.sprintf " leading_trivia_segments=[%s]"
          (String.concat ";" segments)
  in
  Printf.sprintf "%s:%d:%d..%d:%d %s raw=%S%s%s%s" path start_line start_column
    stop_line stop_column
    (Token_kind.name token.kind)
    token.raw value source_segments trivia_segments

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
  | Some source ->
      let position = Common.Source_file.position source span.start in
      let location =
        match position with
        | Error _ -> []
        | Ok item -> [ ("line", `Int item.line); ("column", `Int item.column) ]
      in
      `Assoc
        ((("path", `String (Common.Source_file.display_path source)) :: location)
        @ fields)

let value_json = function
  | No_value -> `Null
  | Text text -> `String text
  | Int64 value -> `String (Printf.sprintf "0x%016Lx" value)
  | Float64 value -> `Float value
  | Bytes bytes -> `String (escaped_bytes bytes)

let to_yojson sources token =
  let source_segments =
    match token.source_segments with
    | [ _ ] | [] -> []
    | spans ->
        [ ("source_segments", `List (List.map (span_json sources) spans)) ]
  in
  `Assoc
    ([
       ("kind", `String (Token_kind.name token.kind));
       ("raw", `String token.raw);
       ("value", value_json token.value);
       ("span", span_json sources token.span);
     ]
    @ source_segments
    @ [
        ( "origin",
          `Assoc
            [
              ("frame", `Int (Common.Source_id.to_int token.origin.frame));
              ( "generated_from",
                match token.origin.generated_from with
                | None -> `Null
                | Some span -> span_json sources span );
              ( "defined_at",
                match token.origin.defined_at with
                | None -> `Null
                | Some span -> span_json sources span );
            ] );
        ( "leading_trivia",
          `List
            (List.map
               (Trivia.to_yojson ~containing_source:token.span.source sources)
               token.leading_trivia) );
        ("mode", `String (mode_name token.mode));
        ( "templeos_token_id",
          match Token_kind.templeos_token_id token.kind with
          | None -> `Null
          | Some value -> `Int value );
      ])

let json sources tokens =
  `List (List.map (to_yojson sources) tokens) |> Yojson.Safe.pretty_to_string
