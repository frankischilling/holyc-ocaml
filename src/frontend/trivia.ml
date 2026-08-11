type kind =
  | Whitespace
  | Line_continuation
  | Line_comment
  | Block_comment
  | Dollar_comment

type t = {
  kind : kind;
  raw : string;
  span : Common.Span.t;
  source_segments : Common.Span.t list;
}

let kind_name = function
  | Whitespace -> "whitespace"
  | Line_continuation -> "line-continuation"
  | Line_comment -> "line-comment"
  | Block_comment -> "block-comment"
  | Dollar_comment -> "dollar-comment"

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
      let location =
        match Common.Source_file.position source span.start with
        | Error _ -> []
        | Ok item -> [ ("line", `Int item.line); ("column", `Int item.column) ]
      in
      `Assoc
        (("path", `String (Common.Source_file.display_path source)) :: location
       @ fields)

let to_yojson ?containing_source sources trivia =
  let needs_source_segments =
    match (trivia.source_segments, containing_source) with
    | [], _ -> false
    | [ span ], Some source ->
        not (Common.Source_id.equal span.Common.Span.source source)
    | [ _ ], None -> false
    | _ :: _ :: _, _ -> true
  in
  let source_segments =
    if needs_source_segments then
      [
        ( "source_segments",
          `List (List.map (span_json sources) trivia.source_segments) );
      ]
    else []
  in
  `Assoc
    ([
       ("kind", `String (kind_name trivia.kind));
       ("raw", `String trivia.raw);
       ("start", `Int trivia.span.start);
       ("stop", `Int trivia.span.stop);
     ]
    @ source_segments)
