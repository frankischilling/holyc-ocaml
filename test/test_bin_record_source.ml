module Source = Bin_record_source

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let pinned path = read_file ("../third_party/TempleOS/" ^ path)

let pinned_sources () =
  List.map (fun path -> (path, pinned path)) Source.required_source_paths

let replace_once text ~needle ~replacement =
  let needle_length = String.length needle in
  let rec find offset =
    if offset + needle_length > String.length text then
      Alcotest.failf "test fixture does not contain %S" needle
    else if String.sub text offset needle_length = needle then offset
    else find (offset + 1)
  in
  let offset = find 0 in
  String.sub text 0 offset ^ replacement
  ^ String.sub text (offset + needle_length)
      (String.length text - offset - needle_length)

let normalize_line_endings text =
  let length = String.length text in
  let buffer = Buffer.create length in
  let rec copy offset =
    if offset < length then
      if
        Char.equal text.[offset] '\r'
        && offset + 1 < length
        && Char.equal text.[offset + 1] '\n'
      then (
        Buffer.add_char buffer '\n';
        copy (offset + 2))
      else (
        Buffer.add_char buffer text.[offset];
        copy (offset + 1))
  in
  copy 0;
  Buffer.contents buffer

let map_source path transform sources =
  List.map
    (fun (candidate, source) ->
      if String.equal candidate path then (candidate, transform source)
      else (candidate, source))
    sources

let parse_ok sources =
  match Source.parse ~sources with
  | Ok tables -> tables
  | Error problem -> Alcotest.fail (Source.error_to_string problem)

let expect_error ~needle sources =
  match Source.parse ~sources with
  | Ok _ -> Alcotest.fail "source parser unexpectedly accepted changed input"
  | Error problem ->
      let message = Source.error_to_string problem in
      let contains =
        let needle_length = String.length needle in
        let rec search offset =
          if offset + needle_length > String.length message then false
          else if String.sub message offset needle_length = needle then true
          else search (offset + 1)
        in
        needle_length = 0 || search 0
      in
      Alcotest.(check bool) message true contains

let complete_registry () =
  let tables = parse_ok (pinned_sources ()) in
  Alcotest.(check (list string))
    "entry names"
    [
      "IET_END";
      "IET_REL_I0";
      "IET_IMM_U0";
      "IET_REL_I8";
      "IET_IMM_U8";
      "IET_REL_I16";
      "IET_IMM_U16";
      "IET_REL_I32";
      "IET_IMM_U32";
      "IET_REL_I64";
      "IET_IMM_I64";
      "IET_REL32_EXPORT";
      "IET_IMM32_EXPORT";
      "IET_REL64_EXPORT";
      "IET_IMM64_EXPORT";
      "IET_ABS_ADDR";
      "IET_CODE_HEAP";
      "IET_ZEROED_CODE_HEAP";
      "IET_DATA_HEAP";
      "IET_ZEROED_DATA_HEAP";
      "IET_MAIN";
    ]
    (List.map (fun (entry : Source.entry) -> entry.name) tables.entries);
  Alcotest.(check (list int))
    "entry codes"
    [
      0; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 16; 17; 18; 19; 20; 21; 22; 23; 24; 25;
    ]
    (List.map (fun (entry : Source.entry) -> entry.code) tables.entries);
  Alcotest.(check (list int))
    "reserved codes" [ 1; 12; 13; 14; 15 ] tables.reserved_codes

let header_layout () =
  let tables = parse_ok (pinned_sources ()) in
  Alcotest.(check string)
    "signature spelling" "'TOSB'" tables.signature_spelling;
  Alcotest.(check int32) "signature value" 0x42534f54l tables.signature_value;
  Alcotest.(check int) "header size" 32 tables.header_size;
  Alcotest.(check (list string))
    "field names"
    [
      "jmp";
      "module_align_bits";
      "reserved";
      "bin_signature";
      "org";
      "patch_table_offset";
      "file_size";
    ]
    (List.map
       (fun (field : Source.header_field) -> field.name)
       tables.header_fields);
  Alcotest.(check (list int))
    "field offsets" [ 0; 2; 3; 4; 8; 16; 24 ]
    (List.map
       (fun (field : Source.header_field) -> field.offset)
       tables.header_fields);
  Alcotest.(check (list int))
    "field widths" [ 2; 1; 1; 4; 8; 8; 8 ]
    (List.map
       (fun (field : Source.header_field) -> field.width_bytes)
       tables.header_fields)

let status_classification () =
  let tables = parse_ok (pinned_sources ()) in
  let status name =
    (List.find
       (fun (entry : Source.entry) -> String.equal entry.name name)
       tables.entries)
      .status
  in
  Alcotest.(check bool)
    "fictitious relative zero" true
    (status "IET_REL_I0" = Source.Source_fictitious);
  Alcotest.(check bool)
    "unimplemented 64-bit export" true
    (status "IET_REL64_EXPORT" = Source.Source_not_implemented);
  Alcotest.(check bool)
    "unused code heap" true
    (status "IET_CODE_HEAP" = Source.Source_not_really_used);
  Alcotest.(check bool)
    "active data heap" true
    (status "IET_DATA_HEAP" = Source.Source_active)

let relocation_formulas () =
  let tables = parse_ok (pinned_sources ()) in
  let relocation name =
    let entry =
      List.find
        (fun (entry : Source.entry) -> String.equal entry.name name)
        tables.entries
    in
    match entry.relocation with
    | Some relocation -> relocation
    | None -> Alcotest.failf "%s has no relocation" name
  in
  List.iter
    (fun (name, width, bias) ->
      let relocation = relocation name in
      Alcotest.(check int) name width (Source.width_bytes relocation.width);
      Alcotest.(check int) (name ^ " bias") bias relocation.displacement_bias;
      Alcotest.(check bool)
        (name ^ " relative") true
        (relocation.kind = Source.Relative))
    [
      ("IET_REL_I0", 0, 0);
      ("IET_REL_I8", 1, 1);
      ("IET_REL_I16", 2, 2);
      ("IET_REL_I32", 4, 4);
      ("IET_REL_I64", 8, 8);
    ];
  List.iter
    (fun name ->
      let relocation = relocation name in
      Alcotest.(check bool)
        (name ^ " immediate") true
        (relocation.kind = Source.Immediate);
      Alcotest.(check int) (name ^ " bias") 0 relocation.displacement_bias)
    [ "IET_IMM_U0"; "IET_IMM_U8"; "IET_IMM_U16"; "IET_IMM_U32"; "IET_IMM_I64" ]

let loader_actions () =
  let tables = parse_ok (pinned_sources ()) in
  let entry name =
    List.find
      (fun (entry : Source.entry) -> String.equal entry.name name)
      tables.entries
  in
  let absolute = entry "IET_ABS_ADDR" in
  Alcotest.(check bool)
    "absolute payload" true
    (absolute.payload = Source.U32_offsets);
  Alcotest.(check bool)
    "absolute pass one" true
    (absolute.pass1 = Source.Apply_module_base_u32);
  let code = entry "IET_CODE_HEAP" in
  Alcotest.(check bool)
    "code payload" true
    (code.payload = Source.I32_size_then_u32_offsets);
  let data = entry "IET_DATA_HEAP" in
  Alcotest.(check bool)
    "data payload" true
    (data.payload = Source.I64_size_then_u32_offsets);
  let main = entry "IET_MAIN" in
  Alcotest.(check bool) "main pass" true (main.pass2 = Source.Execute_main);
  let terminator = entry "IET_END" in
  Alcotest.(check bool)
    "terminator prefix" true
    (terminator.leading_value = Source.No_leading_value)

let adjustment_table () =
  let tables = parse_ok (pinned_sources ()) in
  Alcotest.(check (list int))
    "adjustment codes" [ 0; 1; 2; 3; 4; 5; 6; 7 ]
    (List.map
       (fun (adjustment : Source.adjustment) -> adjustment.code)
       tables.adjustments);
  Alcotest.(check (list int))
    "adjustment widths" [ 1; 1; 2; 2; 4; 4; 8; 8 ]
    (List.map
       (fun (adjustment : Source.adjustment) ->
         Source.width_bytes adjustment.width)
       tables.adjustments);
  Alcotest.(check (list bool))
    "add/subtract pairs"
    [ true; false; true; false; true; false; true; false ]
    (List.map
       (fun (adjustment : Source.adjustment) ->
         adjustment.operation = Source.Add)
       tables.adjustments)

let behavior_provenance () =
  let behavior = (parse_ok (pinned_sources ())).behavior in
  let pair (reference : Source.source_reference) =
    (reference.path, reference.line)
  in
  Alcotest.(check (pair string int))
    "loader patches" ("Kernel/KLoad.HC", 41)
    (pair behavior.import_patches);
  Alcotest.(check (pair string int))
    "writer header" ("Compiler/CMain.HC", 540)
    (pair behavior.header_write);
  Alcotest.(check (pair string int))
    "boot patches" ("Kernel/KStart32.HC", 96)
    (pair behavior.boot_absolute_patch)

let consumer_provenance () =
  let tables = parse_ok (pinned_sources ()) in
  let entry name =
    List.find
      (fun (entry : Source.entry) -> String.equal entry.name name)
      tables.entries
  in
  let has path line references =
    List.exists
      (fun (reference : Source.source_reference) ->
        String.equal reference.path path && reference.line = line)
      references
  in
  Alcotest.(check bool)
    "assembler relative import" true
    (has "Compiler/Asm.HC" 333 (entry "IET_REL_I32").consumers);
  Alcotest.(check bool)
    "top-level main" true
    (has "Compiler/PrsVar.HC" 83 (entry "IET_MAIN").consumers)

let rejects_changed_code () =
  pinned_sources ()
  |> map_source "Kernel/KernelA.HH" (fun source ->
      replace_once source ~needle:"#define IET_MAIN\t\t25"
        ~replacement:"#define IET_MAIN\t\t26")
  |> expect_error ~needle:"IET_MAIN must remain 25"

let rejects_reordered_entries () =
  pinned_sources ()
  |> map_source "Kernel/KernelA.HH" (fun source ->
      let source = normalize_line_endings source in
      replace_once source
        ~needle:"#define IET_REL_I8\t\t4\n#define IET_IMM_U8\t\t5"
        ~replacement:"#define IET_IMM_U8\t\t5\n#define IET_REL_I8\t\t4")
  |> expect_error ~needle:"requires IET_REL_I8 here"

let rejects_duplicate_entry () =
  pinned_sources ()
  |> map_source "Kernel/KernelA.HH" (fun source ->
      let source = normalize_line_endings source in
      replace_once source ~needle:"#define IET_REL_I8\t\t4"
        ~replacement:"#define IET_REL_I8\t\t4\n#define IET_REL_I8\t\t4")
  |> expect_error ~needle:"IET_REL_I8 is defined more than once"

let rejects_missing_entry () =
  pinned_sources ()
  |> map_source "Kernel/KernelA.HH" (fun source ->
      let source = normalize_line_endings source in
      replace_once source ~needle:"#define IET_REL_I8\t\t4\n" ~replacement:"")
  |> expect_error ~needle:"requires IET_REL_I8 here"

let rejects_reserved_gap_drift () =
  pinned_sources ()
  |> map_source "Kernel/KernelA.HH" (fun source ->
      replace_once source ~needle:"//reserved" ~replacement:"//changed")
  |> expect_error ~needle:"retain both reserved gaps"

let rejects_status_drift () =
  pinned_sources ()
  |> map_source "Kernel/KernelA.HH" (fun source ->
      replace_once source ~needle:"2 //Fictitious" ~replacement:"2 //Active")
  |> expect_error ~needle:"source status comment"

let rejects_signature_drift () =
  pinned_sources ()
  |> map_source "Kernel/KernelA.HH" (fun source ->
      replace_once source ~needle:"'TOSB'" ~replacement:"'BOST'")
  |> expect_error ~needle:"must remain 'TOSB'"

let rejects_header_drift () =
  pinned_sources ()
  |> map_source "Kernel/KernelA.HH" (fun source ->
      replace_once source ~needle:"U16\tjmp;" ~replacement:"U32\tjmp;")
  |> expect_error ~needle:"CBinFile"

let rejects_import_formula_drift () =
  pinned_sources ()
  |> map_source "Kernel/KLoad.HC" (fun source ->
      replace_once source ~needle:"i-ptr2-4; break;"
        ~replacement:"i-ptr2-5; break;")
  |> expect_error ~needle:"required source behavior"

let rejects_adjustment_drift () =
  pinned_sources ()
  |> map_source "Compiler/CMain.HC" (fun source ->
      replace_once source ~needle:"*ptr(U8 *) +=rip2; break;"
        ~replacement:"*ptr(U8 *) -=rip2; break;")
  |> expect_error ~needle:"complete AOT adjustment switch"

let ignores_comments_and_literals () =
  let sources =
    pinned_sources ()
    |> map_source "Compiler/BackC.HC" (fun source ->
        source ^ "\n// IET_UNKNOWN\n\"AAT_UNKNOWN\";\n")
  in
  ignore (parse_ok sources)

let rejects_unknown_consumer () =
  pinned_sources ()
  |> map_source "Compiler/BackC.HC" (fun source -> source ^ "\nIET_UNKNOWN;\n")
  |> expect_error ~needle:"unknown BIN symbol IET_UNKNOWN"

let rejects_unterminated_consumer_comment () =
  let path = "Compiler/BackC.HC" in
  let prefix = normalize_line_endings (pinned path) ^ "\n" in
  let expected_line =
    String.fold_left
      (fun line byte -> if Char.equal byte '\n' then line + 1 else line)
      1 prefix
  in
  let sources =
    pinned_sources () |> map_source path (fun _ -> prefix ^ "/* IET_REL_I8")
  in
  match Source.parse ~sources with
  | Ok _ -> Alcotest.fail "source parser accepted an unterminated comment"
  | Error problem ->
      Alcotest.(check (option string)) "path" (Some path) problem.path;
      Alcotest.(check (option int)) "line" (Some expected_line) problem.line;
      Alcotest.(check string)
        "message" "unterminated block comment" problem.message

let source_set_is_exact () =
  let missing = List.tl (pinned_sources ()) in
  expect_error ~needle:"required source was not supplied" missing;
  expect_error ~needle:"unexpected source was supplied"
    (("Elsewhere.HC", "") :: pinned_sources ())

let checksum_and_line_endings () =
  let kernel = pinned "Kernel/KernelA.HH" |> normalize_line_endings in
  let expected =
    "1b4b6d8b6aeeaedfd2b11536b84557d9d2efc05ff38200020cd7a4a94dcd7d41"
  in
  Alcotest.(check bool)
    "pinned checksum" true
    (Source.verify_sha256 ~expected kernel = Ok ());
  let crlf = String.split_on_char '\n' kernel |> String.concat "\r\n" in
  Alcotest.(check bool)
    "checkout line endings" true
    (Source.verify_sha256 ~expected crlf = Ok ());
  match Source.verify_sha256 ~expected (kernel ^ " ") with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "changed source retained the pinned checksum"

let deterministic_parse () =
  let sources = pinned_sources () in
  Alcotest.(check bool)
    "deterministic tables" true
    (parse_ok sources = parse_ok sources)

let tests =
  [
    Alcotest.test_case "complete registry" `Quick complete_registry;
    Alcotest.test_case "header layout" `Quick header_layout;
    Alcotest.test_case "source statuses" `Quick status_classification;
    Alcotest.test_case "relocation formulas" `Quick relocation_formulas;
    Alcotest.test_case "loader actions" `Quick loader_actions;
    Alcotest.test_case "adjustment table" `Quick adjustment_table;
    Alcotest.test_case "behavior provenance" `Quick behavior_provenance;
    Alcotest.test_case "consumer provenance" `Quick consumer_provenance;
    Alcotest.test_case "changed code" `Quick rejects_changed_code;
    Alcotest.test_case "entry order" `Quick rejects_reordered_entries;
    Alcotest.test_case "duplicate entry" `Quick rejects_duplicate_entry;
    Alcotest.test_case "missing entry" `Quick rejects_missing_entry;
    Alcotest.test_case "reserved gaps" `Quick rejects_reserved_gap_drift;
    Alcotest.test_case "status drift" `Quick rejects_status_drift;
    Alcotest.test_case "signature drift" `Quick rejects_signature_drift;
    Alcotest.test_case "header drift" `Quick rejects_header_drift;
    Alcotest.test_case "import formula" `Quick rejects_import_formula_drift;
    Alcotest.test_case "adjustment drift" `Quick rejects_adjustment_drift;
    Alcotest.test_case "comments and literals" `Quick
      ignores_comments_and_literals;
    Alcotest.test_case "unknown consumer" `Quick rejects_unknown_consumer;
    Alcotest.test_case "unterminated consumer comment" `Quick
      rejects_unterminated_consumer_comment;
    Alcotest.test_case "source set" `Quick source_set_is_exact;
    Alcotest.test_case "checksums and line endings" `Quick
      checksum_and_line_endings;
    Alcotest.test_case "deterministic parse" `Quick deterministic_parse;
  ]
