module Source = Primitive_type_source

let raw_definitions =
  [
    "#define RT_I0 2";
    "#define RT_U0 3";
    "#define RT_I8 4";
    "#define RT_U8 5";
    "#define RT_I16 6";
    "#define RT_U16 7";
    "#define RT_I32 8";
    "#define RT_U32 9";
    "#define RT_I64 10";
    "#define RT_PTR 10 //Signed to allow negative err codes.";
    "#define RT_U64 11";
    "#define RT_F32 12 //Not implemented";
    "#define RT_UF32 13 //Not implemented, Fictitious";
    "#define RT_F64 14";
    "#define RT_UF64 15 //Fictitious";
    "#define RT_RTS_NUM 16";
    "#define RTF_UNSIGNED 1";
    "#define RTG_MASK 0xFF";
  ]

let public_unions =
  [
    "U16i union U16";
    "I16i union I16";
    "U32i union U32";
    "I32i union I32";
    "U64i union U64";
    "I64i union I64";
  ]

let internal_records =
  [
    ("RT_I0", 0, "I0i");
    ("RT_I0", 0, "I0");
    ("RT_U0", 0, "U0i");
    ("RT_U0", 0, "U0");
    ("RT_I8", 1, "I8i");
    ("RT_I8", 1, "I8");
    ("RT_I8", 1, "Bool");
    ("RT_U8", 1, "U8i");
    ("RT_U8", 1, "U8");
    ("RT_I16", 2, "I16i");
    ("RT_U16", 2, "U16i");
    ("RT_I32", 4, "I32i");
    ("RT_U32", 4, "U32i");
    ("RT_I64", 8, "I64i");
    ("RT_U64", 8, "U64i");
    ("RT_F64", 8, "F64i");
    ("RT_F64", 8, "F64");
  ]

let kernel_source ?(definitions = raw_definitions) ?(unions = public_unions) ()
    =
  String.concat "\n" ([ "// synthetic header" ] @ unions @ definitions @ [ "" ])

let render_record (raw_name, byte_size, spelling) =
  Printf.sprintf "  {%s,%d,%S}" raw_name byte_size spelling

let cinit_source ?(count = 17) ?(records = internal_records) () =
  String.concat "\n"
    ([
       Printf.sprintf "#define INTERNAL_TYPES_NUM %d" count;
       "";
       "class CInternalType";
       "{";
       "  U8 type,size,name[8];";
       "} internal_types_table[INTERNAL_TYPES_NUM]={";
     ]
    @ List.mapi
        (fun index record ->
          let suffix = if index + 1 = List.length records then "" else "," in
          render_record record ^ suffix)
        records
    @ [ "};"; "" ])

let parse_ok ?definitions ?unions ?count ?records () =
  let kernel_source = kernel_source ?definitions ?unions () in
  let cinit_source = cinit_source ?count ?records () in
  match Source.parse ~kernel_source ~cinit_source with
  | Ok tables -> tables
  | Error problem -> Alcotest.fail (Source.error_to_string problem)

let contains ~needle text =
  let needle_length = String.length needle in
  let text_length = String.length text in
  let rec search offset =
    if offset + needle_length > text_length then false
    else if String.sub text offset needle_length = needle then true
    else search (offset + 1)
  in
  needle_length = 0 || search 0

let expect_error ?definitions ?unions ?count ?records needle =
  let kernel_source = kernel_source ?definitions ?unions () in
  let cinit_source = cinit_source ?count ?records () in
  match Source.parse ~kernel_source ~cinit_source with
  | Ok _ ->
      Alcotest.fail "expected the primitive type parser to reject the fixture"
  | Error problem ->
      Alcotest.(check bool)
        (Printf.sprintf "error mentions %S" needle)
        true
        (contains ~needle (Source.error_to_string problem))

let replace index replacement values =
  List.mapi
    (fun current value -> if current = index then replacement else value)
    values

let swap first second values =
  let first_value = List.nth values first in
  let second_value = List.nth values second in
  values |> replace first second_value |> replace second first_value

let parses_complete_tables () =
  let tables = parse_ok () in
  Alcotest.(check int) "raw type count" 14 (List.length tables.raw_types);
  Alcotest.(check int) "public union count" 6 (List.length tables.public_unions);
  Alcotest.(check int)
    "internal type count" 17
    (List.length tables.internal_types);
  Alcotest.(check int) "RT_RTS_NUM" 16 tables.raw_types_count;
  Alcotest.(check int) "RTF_UNSIGNED" 1 tables.unsigned_flag;
  Alcotest.(check int) "RTG_MASK" 255 tables.raw_group_mask;
  Alcotest.(check string)
    "pointer target" "RT_I64" tables.pointer_alias.target_name;
  Alcotest.(check int) "pointer raw ID" 10 tables.pointer_alias.templeos_id

let rejects_malformed_definition () =
  let definitions = raw_definitions |> replace 2 "#define RT_I8 four" in
  expect_error ~definitions "RT_I8 has invalid value"

let rejects_duplicate_definition () =
  let definitions = raw_definitions |> replace 1 "#define RT_I0 3" in
  expect_error ~definitions "RT_I0 appears more than once"

let rejects_missing_definition () =
  let definitions = List.filteri (fun index _ -> index <> 2) raw_definitions in
  expect_error ~definitions "requires RT_I8 here, but found RT_U8"

let rejects_reordered_definitions () =
  let definitions = swap 2 3 raw_definitions in
  expect_error ~definitions "requires RT_I8 here, but found RT_U8"

let rejects_conflicting_pointer_alias () =
  let definitions =
    raw_definitions
    |> replace 9 "#define RT_PTR 11 //Signed to allow negative err codes."
  in
  expect_error ~definitions "RT_PTR must have value 10"

let rejects_changed_availability () =
  let definitions = raw_definitions |> replace 11 "#define RT_F32 12" in
  expect_error ~definitions "RT_F32 availability markers"

let rejects_missing_public_union () =
  let unions = List.filteri (fun index _ -> index <> 2) public_unions in
  expect_error ~unions "requires `U32i union U32` here"

let rejects_changed_public_union_storage () =
  let unions = public_unions |> replace 1 "U16i union I16" in
  expect_error ~unions "requires `I16i union I16`"

let rejects_malformed_internal_record () =
  let cinit_source =
    cinit_source () |> String.split_on_char '\n'
    |> replace 8 "  {RT_U0 0,\"U0i\"},"
    |> String.concat "\n"
  in
  let kernel_source = kernel_source () in
  match Source.parse ~kernel_source ~cinit_source with
  | Ok _ -> Alcotest.fail "expected a malformed internal record to fail"
  | Error problem ->
      Alcotest.(check bool)
        "comma diagnostic" true
        (contains ~needle:"`,` after the raw type name"
           (Source.error_to_string problem))

let rejects_duplicate_internal_spelling () =
  let records = internal_records |> replace 6 ("RT_I8", 1, "I8") in
  expect_error ~records "internal type spelling \"I8\" appears more than once"

let rejects_missing_internal_type () =
  let records = List.filteri (fun index _ -> index <> 4) internal_records in
  expect_error ~records "requires {RT_I8,1,\"I8i\"} here"

let rejects_reordered_internal_types () =
  let records = swap 4 5 internal_records in
  expect_error ~records "requires {RT_I8,1,\"I8i\"} here"

let rejects_wrong_internal_count () =
  expect_error ~count:16 "INTERNAL_TYPES_NUM must be 17"

let checks_unavailable_float_slots () =
  let tables = parse_ok () in
  let raw name =
    List.find
      (fun (entry : Source.raw_type) -> String.equal entry.name name)
      tables.raw_types
  in
  let f32 = raw "RT_F32" in
  let uf32 = raw "RT_UF32" in
  let uf64 = raw "RT_UF64" in
  Alcotest.(check bool) "F32 not implemented" true f32.not_implemented;
  Alcotest.(check bool) "F32 is not fictitious" false f32.fictitious;
  Alcotest.(check bool) "UF32 not implemented" true uf32.not_implemented;
  Alcotest.(check bool) "UF32 fictitious" true uf32.fictitious;
  Alcotest.(check bool)
    "UF64 lacks not-implemented marker" false uf64.not_implemented;
  Alcotest.(check bool) "UF64 fictitious" true uf64.fictitious

let rejects_checksum_mismatch () =
  match
    Source.verify_sha256 ~expected:(String.make 64 '0') (kernel_source ())
  with
  | Ok () -> Alcotest.fail "expected a source checksum mismatch"
  | Error problem ->
      Alcotest.(check bool)
        "checksum diagnostic" true
        (contains ~needle:"source SHA-256" (Source.error_to_string problem))

let normalizes_checkout_line_endings () =
  let expected =
    "911169ddaaf146aff539f58c26c489af3b892dff0fe283c1c264c65ae5aa59a2"
  in
  Alcotest.(check bool)
    "LF checksum" true
    (Result.is_ok (Source.verify_sha256 ~expected "a\nb\n"));
  Alcotest.(check bool)
    "CRLF checksum" true
    (Result.is_ok (Source.verify_sha256 ~expected "a\r\nb\r\n"))

let deterministic_parse () =
  let kernel_source = kernel_source () in
  let cinit_source = cinit_source () in
  let first = Source.parse ~kernel_source ~cinit_source in
  let second = Source.parse ~kernel_source ~cinit_source in
  Alcotest.(check bool) "same parsed records" true (first = second)

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let parses_pinned_sources () =
  let kernel_source = read_file "../third_party/TempleOS/Kernel/KernelA.HH" in
  let cinit_source = read_file "../third_party/TempleOS/Compiler/CInit.HC" in
  let tables =
    match Source.parse ~kernel_source ~cinit_source with
    | Ok tables -> tables
    | Error problem -> Alcotest.fail (Source.error_to_string problem)
  in
  Alcotest.(check bool)
    "Kernel checksum" true
    (Result.is_ok
       (Source.verify_sha256
          ~expected:
            "1b4b6d8b6aeeaedfd2b11536b84557d9d2efc05ff38200020cd7a4a94dcd7d41"
          kernel_source));
  Alcotest.(check bool)
    "CInit checksum" true
    (Result.is_ok
       (Source.verify_sha256
          ~expected:
            "f187d11043dcceb8791409a3e6809ea26e9c3b4f182fe2cbe5c5e644e6938b19"
          cinit_source));
  Alcotest.(check (list int))
    "raw IDs"
    (List.init 14 (fun index -> index + 2))
    (List.map
       (fun (entry : Source.raw_type) -> entry.templeos_id)
       tables.raw_types);
  Alcotest.(check (list string))
    "internal spellings"
    (List.map (fun (_, _, spelling) -> spelling) internal_records)
    (List.map
       (fun (entry : Source.internal_type) -> entry.spelling)
       tables.internal_types);
  Alcotest.(check (list string))
    "public union spellings"
    [ "U16"; "I16"; "U32"; "I32"; "U64"; "I64" ]
    (List.map
       (fun (entry : Source.public_union) -> entry.public_spelling)
       tables.public_unions);
  Alcotest.(check (list int))
    "source lines"
    [ 67; 73; 79; 87; 95; 105; 1564; 1573; 1578; 7; 13 ]
    [
      (List.nth tables.public_unions 0).source_line;
      (List.nth tables.public_unions 1).source_line;
      (List.nth tables.public_unions 2).source_line;
      (List.nth tables.public_unions 3).source_line;
      (List.nth tables.public_unions 4).source_line;
      (List.nth tables.public_unions 5).source_line;
      (List.hd tables.raw_types).source_line;
      tables.pointer_alias.source_line;
      (List.hd (List.rev tables.raw_types)).source_line;
      (List.hd tables.internal_types).source_line;
      (List.hd (List.rev tables.internal_types)).source_line;
    ]

let tests =
  [
    Alcotest.test_case "complete synthetic tables" `Quick parses_complete_tables;
    Alcotest.test_case "malformed raw definition" `Quick
      rejects_malformed_definition;
    Alcotest.test_case "duplicate raw definition" `Quick
      rejects_duplicate_definition;
    Alcotest.test_case "missing raw definition" `Quick
      rejects_missing_definition;
    Alcotest.test_case "reordered raw definitions" `Quick
      rejects_reordered_definitions;
    Alcotest.test_case "conflicting pointer alias" `Quick
      rejects_conflicting_pointer_alias;
    Alcotest.test_case "changed availability markers" `Quick
      rejects_changed_availability;
    Alcotest.test_case "missing public union" `Quick
      rejects_missing_public_union;
    Alcotest.test_case "changed public union storage" `Quick
      rejects_changed_public_union_storage;
    Alcotest.test_case "malformed internal record" `Quick
      rejects_malformed_internal_record;
    Alcotest.test_case "duplicate internal spelling" `Quick
      rejects_duplicate_internal_spelling;
    Alcotest.test_case "missing internal type" `Quick
      rejects_missing_internal_type;
    Alcotest.test_case "reordered internal types" `Quick
      rejects_reordered_internal_types;
    Alcotest.test_case "internal type count" `Quick rejects_wrong_internal_count;
    Alcotest.test_case "unavailable floating slots" `Quick
      checks_unavailable_float_slots;
    Alcotest.test_case "checksum mismatch" `Quick rejects_checksum_mismatch;
    Alcotest.test_case "checkout line endings" `Quick
      normalizes_checkout_line_endings;
    Alcotest.test_case "deterministic parse" `Quick deterministic_parse;
    Alcotest.test_case "pinned source tables" `Quick parses_pinned_sources;
  ]
