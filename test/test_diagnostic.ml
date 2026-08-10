open Holyc_lib

let rendered_context () =
  let session = Session.create () in
  let source = Session.add_source session ~path:"bad.hc" ~contents:"ok\n\\\n" in
  match Holyc_lib.lex session ~source with
  | Ok _ -> Alcotest.fail "expected an invalid-byte diagnostic"
  | Error [ diagnostic ] ->
      let rendered =
        Diagnostic_render.human (Session.sources session) diagnostic
      in
      Alcotest.(check string)
        "rendered diagnostic"
        "bad.hc:2:1: error[HCLEX0001]: invalid source byte 0x5c\n\
        \    2 | \\\n\
        \      | ^\n\
         help: Remove the byte or place it inside a string or character literal.\n"
        rendered
  | Error _ -> Alcotest.fail "expected one diagnostic"

let json_shape () =
  let session = Session.create () in
  let source = Session.add_source session ~path:"bad.hc" ~contents:"\\" in
  match Holyc_lib.lex session ~source with
  | Ok _ -> Alcotest.fail "expected an invalid-byte diagnostic"
  | Error diagnostics ->
      let json = Diagnostic_render.json (Session.sources session) diagnostics in
      let parsed = Yojson.Safe.from_string json in
      let open Yojson.Safe.Util in
      let code = parsed |> index 0 |> member "code" |> to_string in
      let line =
        parsed |> index 0 |> member "primary" |> member "line" |> to_int
      in
      Alcotest.(check string) "stable code" "HCLEX0001" code;
      Alcotest.(check int) "line" 1 line

let tests =
  [
    Alcotest.test_case "human context" `Quick rendered_context;
    Alcotest.test_case "JSON shape" `Quick json_shape;
  ]
