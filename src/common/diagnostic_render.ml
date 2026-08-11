let repeat character count = String.make (max 0 count) character

let source_context sources diagnostic =
  match Source_manager.find sources diagnostic.Diagnostic.primary.source with
  | None -> ("<unknown>", 1, 1, "", "^")
  | Some source -> (
      match Source_file.position source diagnostic.primary.start with
      | Error _ -> (Source_file.display_path source, 1, 1, "", "^")
      | Ok position ->
          let line_text =
            Source_file.line_text source ~line:position.line
            |> Result.value ~default:""
          in
          let available =
            max 1 (String.length line_text - position.column + 2)
          in
          let width = min available (max 1 (Span.length diagnostic.primary)) in
          let marker =
            repeat ' ' (position.column - 1) ^ repeat '^' (max 1 width)
          in
          ( Source_file.display_path source,
            position.line,
            position.column,
            line_text,
            marker ))

let span_location sources span =
  match Source_manager.find sources span.Span.source with
  | None -> ("<unknown>", 1, 1)
  | Some source -> (
      match Source_file.position source span.start with
      | Error _ -> (Source_file.display_path source, 1, 1)
      | Ok position ->
          (Source_file.display_path source, position.line, position.column))

let human sources diagnostic =
  let path, line, column, text, marker = source_context sources diagnostic in
  let buffer = Buffer.create 256 in
  Printf.bprintf buffer "%s:%d:%d: %s[%s]: %s\n" path line column
    (Diagnostic.severity_name diagnostic.severity)
    diagnostic.code diagnostic.message;
  if not (String.equal text "") then
    Printf.bprintf buffer "%5d | %s\n      | %s\n" line text marker;
  List.iter
    (fun (item : Diagnostic.related) ->
      let path, line, column = span_location sources item.span in
      Printf.bprintf buffer "included from %s:%d:%d: %s\n" path line column
        item.message)
    diagnostic.include_stack;
  List.iter
    (fun (item : Diagnostic.related) ->
      let path, line, column = span_location sources item.span in
      Printf.bprintf buffer "%s:%d:%d: note: %s\n" path line column item.message)
    diagnostic.secondary;
  List.iter
    (fun note -> Printf.bprintf buffer "note: %s\n" note)
    diagnostic.notes;
  Option.iter
    (fun help -> Printf.bprintf buffer "help: %s\n" help)
    diagnostic.help;
  Buffer.contents buffer

let json sources diagnostics =
  `List (List.map (Diagnostic.to_yojson sources) diagnostics)
  |> Yojson.Safe.pretty_to_string
