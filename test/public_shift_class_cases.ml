open Yojson.Safe.Util

let fixture () =
  [
    "oracle/public-shift-classes.json";
    "../oracle/public-shift-classes.json";
    "test/oracle/public-shift-classes.json";
    "../test/oracle/public-shift-classes.json";
  ]
  |> List.find_opt Sys.file_exists
  |> function
  | Some path -> Yojson.Safe.from_file path
  | None -> Alcotest.fail "public shift-class native fixture is missing"

let field json name = json |> member name |> to_string

let projections fixture =
  fixture |> member "hosted_value_projections" |> to_list

let label projection = "public shift class " ^ field projection "field"

let observed fixture projection case_field =
  let check =
    fixture |> member "checks" |> to_list
    |> List.find (fun check -> field check "id" = field projection case_field)
  in
  check |> member "observed_fields"
  |> member (field projection "field")
  |> to_string
  |> fun bits -> Int64.of_string ("0x" ^ bits)

let cases () =
  let fixture = fixture () in
  projections fixture
  |> List.map (fun projection ->
      let expected = observed fixture projection "case_id" in
      Alcotest.(check int64)
        "same-boot native repeat" expected
        (observed fixture projection "repeat_case_id");
      (label projection, field projection "holy_c_source", expected))

let result_type label_ =
  let fixture = fixture () in
  projections fixture
  |> List.find_opt (fun projection -> label projection = label_)
  |> Option.map (fun projection -> field projection "result_type")
