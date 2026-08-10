open Holyc_lib

let source text =
  let session = Session.create () in
  let source = Session.add_source session ~path:"fixture.hc" ~contents:text in
  (session, source)

let require_ok = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let positions () =
  let _, input = source "one\r\ntwo\n" in
  let first = Source_file.position input 0 |> require_ok in
  let second = Source_file.position input 5 |> require_ok in
  let final = Source_file.position input 9 |> require_ok in
  Alcotest.(check int) "first line" 1 first.line;
  Alcotest.(check int) "first column" 1 first.column;
  Alcotest.(check int) "second line" 2 second.line;
  Alcotest.(check int) "second column" 1 second.column;
  Alcotest.(check int) "final empty line" 3 final.line;
  Alcotest.(check int) "final column" 1 final.column

let line_text () =
  let _, input = source "one\r\ntwo\n" in
  Alcotest.(check string)
    "strip CRLF" "one"
    (Source_file.line_text input ~line:1 |> require_ok);
  Alcotest.(check string)
    "second line" "two"
    (Source_file.line_text input ~line:2 |> require_ok);
  Alcotest.(check string)
    "empty final line" ""
    (Source_file.line_text input ~line:3 |> require_ok)

let span_checks () =
  let _, input = source "abcd" in
  let id = Source_file.id input in
  let valid = Span.make ~source:id ~length:4 ~start:1 ~stop:4 in
  let invalid = Span.make ~source:id ~length:4 ~start:3 ~stop:2 in
  Alcotest.(check bool) "valid span" true (Result.is_ok valid);
  Alcotest.(check bool) "invalid span" true (Result.is_error invalid)

let expected_position text offset =
  let line = ref 1 in
  let line_start = ref 0 in
  for index = 0 to offset - 1 do
    if Char.equal text.[index] '\n' then (
      incr line;
      line_start := index + 1)
  done;
  (!line, offset - !line_start + 1)

let position_property =
  QCheck.Test.make ~count:1_000 ~name:"source positions match LF byte scans"
    QCheck.(pair string nat)
    (fun (text, raw_offset) ->
      let _, input = source text in
      let offset = raw_offset mod (String.length text + 1) in
      let expected_line, expected_column = expected_position text offset in
      match Source_file.position input offset with
      | Error _ -> false
      | Ok actual ->
          actual.offset = offset
          && actual.line = expected_line
          && actual.column = expected_column)

let tests =
  [
    Alcotest.test_case "byte positions" `Quick positions;
    Alcotest.test_case "line text" `Quick line_text;
    Alcotest.test_case "span validation" `Quick span_checks;
    QCheck_alcotest.to_alcotest position_property;
  ]
