module Source = Intermediate_code_source

let argument_definitions =
  [
    "#define IS_0_ARG 0";
    "#define IS_1_ARG 1";
    "#define IS_2_ARG 2";
    "#define IS_V_ARG 3";
  ]

let structural_definitions =
  [
    "#define IST_NULL 0";
    "#define IST_DEREF 1";
    "#define IST_ASSIGN 2";
    "#define IST_CMP 3";
  ]

let opcode_definition code =
  Printf.sprintf "#define IC_OP_%03d 0x%02X" code code

let opcode_definitions = List.init 0xB9 opcode_definition

let struct_definition =
  [
    "class CIntermediateStruct";
    "{";
    "  U8 arg_cnt,res_cnt,type;";
    "  Bool fpop,not_const,pad[3];";
    "  U8 *name;";
    "};";
  ]

let compiler_source ?(arguments = argument_definitions)
    ?(structural = structural_definitions) ?(structure = struct_definition)
    ?(opcodes = opcode_definitions) ?(sentinel = "#define IC_ICS_NUM 0xB9") () =
  String.concat "\n"
    ([ "// synthetic compiler header" ]
    @ arguments @ structural @ structure @ opcodes @ [ sentinel; "" ])

let option value fallback = Option.value ~default:fallback value

let metadata_record ?argument ?result_count ?structural ?pops_float
    ?prevents_constant_folding ?padding ?display_name code =
  let argument_names = [| "IS_0_ARG"; "IS_1_ARG"; "IS_2_ARG"; "IS_V_ARG" |] in
  let structural_names =
    [| "IST_NULL"; "IST_DEREF"; "IST_ASSIGN"; "IST_CMP" |]
  in
  let argument = option argument argument_names.(code mod 4) in
  let result_count = option result_count (code mod 2) in
  let structural = option structural structural_names.(code mod 4) in
  let pops_float = option pops_float (code mod 3 = 0) in
  let prevents_constant_folding =
    option prevents_constant_folding (code mod 5 = 0)
  in
  let padding = option padding (0, 0, 0) in
  let display_name = option display_name (Printf.sprintf "OP_%03d" code) in
  let boolean value = if value then "TRUE" else "FALSE" in
  let pad1, pad2, pad3 = padding in
  Printf.sprintf "  {%s,%d,%s,%s,%s,%d,%d,%d,%S}," argument result_count
    structural (boolean pops_float)
    (boolean prevents_constant_folding)
    pad1 pad2 pad3 display_name

let metadata_records = List.init 0xB9 metadata_record

let cinit_source ?(records = metadata_records) () =
  String.concat "\n"
    ([ "CIntermediateStruct intermediate_code_table[IC_ICS_NUM]={" ]
    @ records @ [ "};"; "" ])

let parse ?compiler ?cinit () =
  Source.parse
    ~compiler_source:(option compiler (compiler_source ()))
    ~cinit_source:(option cinit (cinit_source ()))

let parse_ok ?compiler ?cinit () =
  match parse ?compiler ?cinit () with
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

let expect_error ?compiler ?cinit needle =
  match parse ?compiler ?cinit () with
  | Ok _ -> Alcotest.fail "expected the intermediate-code source to be rejected"
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
  Alcotest.(check int) "entry count" 0xB9 (List.length tables.entries);
  Alcotest.(check int) "count sentinel" 0xB9 tables.count;
  Alcotest.(check (list int))
    "contiguous codes" (List.init 0xB9 Fun.id)
    (List.map (fun (entry : Source.entry) -> entry.code) tables.entries);
  let first = List.hd tables.entries in
  let last = List.hd (List.rev tables.entries) in
  Alcotest.(check string) "first constructor" "Ic_op_000" first.constructor_name;
  Alcotest.(check string) "last constructor" "Ic_op_184" last.constructor_name

let rejects_malformed_opcode () =
  let opcodes = opcode_definitions |> replace 10 "#define IC_OP_010 ten" in
  expect_error ~compiler:(compiler_source ~opcodes ()) "invalid numeric value"

let rejects_duplicate_opcode_name () =
  let opcodes = opcode_definitions |> replace 1 "#define IC_OP_000 0x01" in
  expect_error
    ~compiler:(compiler_source ~opcodes ())
    "IC_OP_000 is defined more than once"

let rejects_duplicate_opcode_value () =
  let opcodes = opcode_definitions |> replace 1 "#define IC_OP_001 0x00" in
  expect_error
    ~compiler:(compiler_source ~opcodes ())
    "opcode value 0x00 is assigned more than once"

let rejects_missing_opcode () =
  let opcodes = List.filteri (fun index _ -> index <> 10) opcode_definitions in
  expect_error
    ~compiler:(compiler_source ~opcodes ())
    "found 184 opcode definitions"

let rejects_reordered_opcodes () =
  let opcodes = swap 10 11 opcode_definitions in
  expect_error
    ~compiler:(compiler_source ~opcodes ())
    "requires value 0x0A here"

let rejects_out_of_range_opcode () =
  let opcodes = opcode_definitions |> replace 184 "#define IC_OP_184 0xB9" in
  expect_error
    ~compiler:(compiler_source ~opcodes ())
    "requires value 0xB8 here"

let rejects_changed_sentinel () =
  expect_error
    ~compiler:(compiler_source ~sentinel:"#define IC_ICS_NUM 0xB8" ())
    "IC_ICS_NUM must be 0xB9"

let rejects_changed_argument_constants () =
  let arguments = argument_definitions |> replace 2 "#define IS_2_ARG 3" in
  expect_error
    ~compiler:(compiler_source ~arguments ())
    "IS_2_ARG must have value 2"

let rejects_changed_structural_constants () =
  let structural = structural_definitions |> replace 1 "#define IST_DEREF 2" in
  expect_error
    ~compiler:(compiler_source ~structural ())
    "IST_DEREF must have value 1"

let rejects_changed_struct_layout () =
  let structure = struct_definition |> replace 3 "  Bool fpop,not_const;" in
  expect_error
    ~compiler:(compiler_source ~structure ())
    "no longer has the audited field layout"

let rejects_malformed_metadata_record () =
  let records =
    metadata_records
    |> replace 7 "  {IS_2_ARG 1,IST_CMP,FALSE,FALSE,0,0,0,\"OP_007\"},"
  in
  expect_error ~cinit:(cinit_source ~records ()) "`,` after the argument shape"

let rejects_extra_metadata_field () =
  let records =
    metadata_records
    |> replace 7 "  {IS_2_ARG,1,IST_CMP,FALSE,FALSE,0,0,0,\"OP_007\",0},"
  in
  expect_error ~cinit:(cinit_source ~records ())
    "`}` at the end of a metadata record"

let rejects_unknown_argument_shape () =
  let records =
    metadata_records |> replace 7 (metadata_record ~argument:"IS_3_ARG" 7)
  in
  expect_error ~cinit:(cinit_source ~records ())
    "unknown argument shape IS_3_ARG"

let rejects_invalid_result_count () =
  let records =
    metadata_records |> replace 7 (metadata_record ~result_count:2 7)
  in
  expect_error ~cinit:(cinit_source ~records ()) "unsupported result count 2"

let rejects_unknown_structural_type () =
  let records =
    metadata_records |> replace 7 (metadata_record ~structural:"IST_BRANCH" 7)
  in
  expect_error ~cinit:(cinit_source ~records ())
    "unknown structural type IST_BRANCH"

let rejects_invalid_boolean () =
  let records =
    metadata_records
    |> replace 7 "  {IS_2_ARG,1,IST_CMP,MAYBE,FALSE,0,0,0,\"OP_007\"},"
  in
  expect_error ~cinit:(cinit_source ~records ())
    "expected TRUE or FALSE, but found MAYBE"

let rejects_nonzero_padding () =
  let records =
    metadata_records |> replace 7 (metadata_record ~padding:(0, 1, 0) 7)
  in
  expect_error ~cinit:(cinit_source ~records ())
    "padding field 2 must remain zero"

let rejects_duplicate_display_name () =
  let records =
    metadata_records |> replace 1 (metadata_record ~display_name:"OP_000" 1)
  in
  expect_error ~cinit:(cinit_source ~records ())
    "display name \"OP_000\" appears more than once"

let rejects_missing_metadata_record () =
  let records = List.filteri (fun index _ -> index <> 10) metadata_records in
  expect_error ~cinit:(cinit_source ~records ())
    "requires 185 records, but found 184"

let rejects_extra_metadata_record () =
  let records = metadata_records @ [ metadata_record 185 ] in
  expect_error ~cinit:(cinit_source ~records ())
    "requires 185 records, but found 186"

let rejects_checksum_mismatch () =
  match
    Source.verify_sha256 ~expected:(String.make 64 '0') (compiler_source ())
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
  Alcotest.(check bool) "same parsed table" true (parse () = parse ())

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let pinned_tables () =
  let compiler_source =
    read_file "../third_party/TempleOS/Compiler/CompilerA.HH"
  in
  let cinit_source = read_file "../third_party/TempleOS/Compiler/CInit.HC" in
  match Source.parse ~compiler_source ~cinit_source with
  | Ok tables -> tables
  | Error problem -> Alcotest.fail (Source.error_to_string problem)

let parses_pinned_sources () =
  let tables = pinned_tables () in
  Alcotest.(check int) "pinned entry count" 185 (List.length tables.entries);
  let first = List.hd tables.entries in
  let last = List.hd (List.rev tables.entries) in
  Alcotest.(check (pair string int))
    "first opcode" ("IC_END", 0)
    (first.source_name, first.code);
  Alcotest.(check (pair string int))
    "last opcode" ("IC_ATAN", 0xB8)
    (last.source_name, last.code);
  Alcotest.(check (pair int int))
    "source boundary lines" (20, 201)
    (first.definition_line, last.metadata_line)

let pinned_name_differences () =
  let differences =
    (pinned_tables ()).entries
    |> List.filter_map (fun (entry : Source.entry) ->
        let expected =
          String.sub entry.source_name 3 (String.length entry.source_name - 3)
        in
        if String.equal expected entry.display_name then None
        else Some (entry.source_name, entry.display_name))
  in
  Alcotest.(check (list (pair string string)))
    "constant and display names"
    [
      ("IC_BR_EQU_EQU2", "BR_2EQU_EQU");
      ("IC_BR_NOT_EQU2", "BR_2NOT_EQU");
      ("IC_BR_LESS2", "BR_2LESS");
      ("IC_BR_GREATER_EQU2", "BR_2GREATER_EQU");
      ("IC_BR_GREATER2", "BR_2GREATER");
      ("IC_BR_LESS_EQU2", "BR_2LESS_EQU");
      ("IC_SWAP_I64", "SWAP_U64");
      ("IC_MIN_I64", "I64_MIN");
      ("IC_MIN_U64", "U64_MIN");
      ("IC_MAX_I64", "I64_MAX");
      ("IC_MAX_U64", "U64_MAX");
      ("IC_SQR_I64", "SQRI64");
      ("IC_SQR_U64", "SQRU64");
    ]
    differences

let tests =
  [
    Alcotest.test_case "complete synthetic tables" `Quick parses_complete_tables;
    Alcotest.test_case "malformed opcode" `Quick rejects_malformed_opcode;
    Alcotest.test_case "duplicate opcode name" `Quick
      rejects_duplicate_opcode_name;
    Alcotest.test_case "duplicate opcode value" `Quick
      rejects_duplicate_opcode_value;
    Alcotest.test_case "missing opcode" `Quick rejects_missing_opcode;
    Alcotest.test_case "reordered opcodes" `Quick rejects_reordered_opcodes;
    Alcotest.test_case "opcode range" `Quick rejects_out_of_range_opcode;
    Alcotest.test_case "count sentinel" `Quick rejects_changed_sentinel;
    Alcotest.test_case "argument constants" `Quick
      rejects_changed_argument_constants;
    Alcotest.test_case "structural constants" `Quick
      rejects_changed_structural_constants;
    Alcotest.test_case "metadata structure" `Quick rejects_changed_struct_layout;
    Alcotest.test_case "malformed metadata" `Quick
      rejects_malformed_metadata_record;
    Alcotest.test_case "extra metadata field" `Quick
      rejects_extra_metadata_field;
    Alcotest.test_case "unknown argument shape" `Quick
      rejects_unknown_argument_shape;
    Alcotest.test_case "result count" `Quick rejects_invalid_result_count;
    Alcotest.test_case "unknown structural type" `Quick
      rejects_unknown_structural_type;
    Alcotest.test_case "metadata Boolean" `Quick rejects_invalid_boolean;
    Alcotest.test_case "metadata padding" `Quick rejects_nonzero_padding;
    Alcotest.test_case "duplicate display name" `Quick
      rejects_duplicate_display_name;
    Alcotest.test_case "missing metadata" `Quick rejects_missing_metadata_record;
    Alcotest.test_case "extra metadata" `Quick rejects_extra_metadata_record;
    Alcotest.test_case "checksum mismatch" `Quick rejects_checksum_mismatch;
    Alcotest.test_case "checkout line endings" `Quick
      normalizes_checkout_line_endings;
    Alcotest.test_case "deterministic parse" `Quick deterministic_parse;
    Alcotest.test_case "pinned source tables" `Quick parses_pinned_sources;
    Alcotest.test_case "pinned display differences" `Quick
      pinned_name_differences;
  ]
