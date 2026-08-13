open Holyc_lib

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let expect_ast = function
  | Ok ast -> ast
  | Error diagnostics ->
      Alcotest.failf "expected an AST, got %s"
        (diagnostics
        |> List.map (fun diagnostic ->
            Printf.sprintf "%s: %s" diagnostic.Diagnostic.code
              diagnostic.message)
        |> String.concat ", ")

let dump ?(mode = Preprocessor.Jit) ~path contents =
  let session = Session.create () in
  let source = Session.add_source session ~path ~contents in
  let config = checked (Preprocessor.Config.create ~compilation_mode:mode ()) in
  let ast = Holyc_lib.parse_with_config session ~config ~source |> expect_ast in
  let layouts = checked (Holyc_lib.analyze_aggregate_layouts session ast) in
  ( session,
    layouts,
    Semantic_aggregate_layout_dump.human (Session.sources session) layouts,
    Semantic_aggregate_layout_dump.json (Session.sources session) layouts )

let contains text needle =
  let text_length = String.length text in
  let needle_length = String.length needle in
  let rec search offset =
    if offset + needle_length > text_length then false
    else if String.sub text offset needle_length = needle then true
    else search (offset + 1)
  in
  search 0

let json_aggregate json name =
  let open Yojson.Safe.Util in
  json |> member "aggregates" |> to_list
  |> List.find (fun aggregate ->
      aggregate |> member "name" |> to_string |> String.equal name)

let json_member aggregate name =
  let open Yojson.Safe.Util in
  aggregate |> member "members" |> to_list
  |> List.find (fun item ->
      item |> member "name" |> to_string |> String.equal name)

let representative_layout_facts () =
  let source =
    "class Base { U8 tag; };\n\
     class Derived : Base {\n\
     union { I16 small; I32 wide; };\n\
     U8 data[2][3];\n\
     I64 (*callback)();\n\
     };\n\
     union Choice { I64 signed_value; U8 bytes[9]; };\n\
     class Negative { $$=-2; U8 before; $$=0; U8 origin; };"
  in
  let session, layouts, human, json_text =
    dump ~path:"aggregate-layout-dump.HC" source
  in
  let repeated_human =
    Semantic_aggregate_layout_dump.human (Session.sources session) layouts
  in
  let repeated_json =
    Semantic_aggregate_layout_dump.json (Session.sources session) layouts
  in
  Alcotest.(check string) "repeat human dump" human repeated_human;
  Alcotest.(check string) "repeat JSON dump" json_text repeated_json;
  Alcotest.(check bool)
    "human reference" true
    (contains human
       "reference_commit=c26482bb6ad3f80106d28504ec5db3c6a360732c");
  Alcotest.(check bool)
    "human callback" true
    (contains human "name=\"callback\"" && contains human "callback=true");
  let json = Yojson.Safe.from_string json_text in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "schema" "holyc-aggregate-layout-v1"
    (json |> member "schema" |> to_string);
  Alcotest.(check string)
    "reference commit" "c26482bb6ad3f80106d28504ec5db3c6a360732c"
    (json |> member "reference_commit" |> to_string);
  Alcotest.(check (list string))
    "aggregate order" [ "Base"; "Derived"; "Choice"; "Negative" ]
    (json |> member "aggregates" |> to_list
    |> List.map (fun aggregate -> aggregate |> member "name" |> to_string));
  let derived = json_aggregate json "Derived" in
  Alcotest.(check string)
    "base name" "Base"
    (derived |> member "base" |> member "name" |> to_string);
  Alcotest.(check int)
    "base size" 1
    (derived |> member "base" |> member "size" |> to_int);
  Alcotest.(check (list string))
    "direct member order" [ "small"; "wide"; "data"; "callback" ]
    (derived |> member "members" |> to_list
    |> List.map (fun item -> item |> member "name" |> to_string));
  let small = json_member derived "small" in
  let wide = json_member derived "wide" in
  Alcotest.(check int)
    "anonymous union short offset" 1
    (small |> member "offset" |> to_int);
  Alcotest.(check int)
    "anonymous union wide offset" 1
    (wide |> member "offset" |> to_int);
  Alcotest.(check (list int))
    "anonymous union path" [ 0; 1 ]
    (wide |> member "path" |> to_list |> List.map to_int);
  let data = json_member derived "data" in
  Alcotest.(check (list int))
    "array dimensions" [ 2; 3 ]
    (data |> member "dimensions" |> to_list |> List.map to_int);
  Alcotest.(check int)
    "array element size" 1
    (data |> member "element_size" |> to_int);
  let callback = json_member derived "callback" in
  Alcotest.(check bool)
    "callback marker" true
    (callback |> member "function_pointer" |> to_bool);
  Alcotest.(check int)
    "callback storage" 8
    (callback |> member "size" |> to_int);
  let choice = json_aggregate json "Choice" in
  Alcotest.(check string)
    "union kind" "union" (choice |> member "kind" |> to_string);
  Alcotest.(check int)
    "union largest member" 9 (choice |> member "size" |> to_int);
  let negative = json_aggregate json "Negative" in
  Alcotest.(check int)
    "negative adjustment" 2
    (negative |> member "negative_offset" |> to_int);
  Alcotest.(check int)
    "negative member offset" (-2)
    (json_member negative "before" |> member "offset" |> to_int)

let generated_provenance () =
  let _, _, human, json_text =
    dump ~path:"aggregate-layout-generated.HC"
      "#define AGG class\n#define FIELD U8\nAGG Generated { FIELD value; };"
  in
  Alcotest.(check bool)
    "human generated origin" true
    (contains human "generated_from=" && contains human "defined_at=");
  let json = Yojson.Safe.from_string json_text in
  let open Yojson.Safe.Util in
  let generated = json_aggregate json "Generated" in
  let origin = generated |> member "origin" in
  Alcotest.(check bool)
    "generated source" true (origin |> member "generated_from" <> `Null);
  Alcotest.(check bool)
    "definition source" true (origin |> member "defined_at" <> `Null)

let empty_dump () =
  let _, _, human, json_text = dump ~path:"aggregate-layout-empty.HC" "" in
  Alcotest.(check string)
    "empty human"
    ("holyc-aggregate-layout-v1\nreference_commit="
    ^ Version.reference_commit ^ "\naggregates=0\n")
    human;
  let open Yojson.Safe.Util in
  Alcotest.(check int)
    "empty JSON" 0
    (Yojson.Safe.from_string json_text |> member "aggregates" |> to_list
    |> List.length)

let modes_and_large_target_values () =
  let source = "class Huge { U8 bytes[4294967296]; };" in
  let render mode =
    let _, _, human, json =
      dump ~mode ~path:"aggregate-layout-mode.HC" source
    in
    (human, json)
  in
  let jit_human, jit_json = render Preprocessor.Jit in
  let aot_human, aot_json = render Preprocessor.Aot in
  Alcotest.(check string) "JIT/AOT human" jit_human aot_human;
  Alcotest.(check string) "JIT/AOT JSON" jit_json aot_json;
  Alcotest.(check bool)
    "full target size" true (contains jit_human "size=4294967296")

let tests =
  [
    Alcotest.test_case "representative layout facts" `Quick
      representative_layout_facts;
    Alcotest.test_case "generated provenance" `Quick generated_provenance;
    Alcotest.test_case "empty dump" `Quick empty_dump;
    Alcotest.test_case "modes and target values" `Quick
      modes_and_large_target_values;
  ]
