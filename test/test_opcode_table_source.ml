module Source = Opcode_table_source

let expected_language_spellings =
  [
    "include";
    "define";
    "union";
    "catch";
    "class";
    "try";
    "if";
    "else";
    "for";
    "while";
    "extern";
    "_extern";
    "return";
    "sizeof";
    "_intern";
    "do";
    "asm";
    "goto";
    "exe";
    "break";
    "switch";
    "start";
    "end";
    "case";
    "default";
    "public";
    "offset";
    "import";
    "_import";
    "ifdef";
    "ifndef";
    "ifaot";
    "ifjit";
    "endif";
    "assert";
    "reg";
    "noreg";
    "lastclass";
    "no_warn";
    "help_index";
    "help_file";
    "static";
    "lock";
    "defined";
    "interrupt";
    "haserrcode";
    "argpop";
    "noargpop";
  ]

let expected_assembly_spellings =
  [
    "ALIGN";
    "ORG";
    "I0";
    "I8";
    "I16";
    "I32";
    "I64";
    "U0";
    "U8";
    "U16";
    "U32";
    "U64";
    "F64";
    "DU8";
    "DU16";
    "DU32";
    "DU64";
    "DUP";
    "USE16";
    "USE32";
    "USE64";
    "IMPORT";
    "LIST";
    "NOLIST";
    "BINFILE";
  ]

let language_line index = Printf.sprintf "KEYWORD keyword_%d %d;" index index

let assembly_line index =
  Printf.sprintf "ASM_KEYWORD DIRECTIVE_%d %d;" index (index + 64)

let table_source ?(language = List.init 48 language_line)
    ?(assembly = List.init 25 assembly_line) () =
  String.concat "\n"
    ([ "R64 RAX 0;" ] @ language @ [ "" ] @ assembly
   @ [ ""; "OPCODE PUSH"; " 0x50,+R R64" ])

let parse_ok source =
  match Source.parse source with
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

let expect_error needle source =
  match Source.parse source with
  | Ok _ -> Alcotest.fail "expected the opcode table parser to reject the fixture"
  | Error problem ->
      Alcotest.(check bool)
        (Printf.sprintf "error mentions %S" needle) true
        (contains ~needle (Source.error_to_string problem))

let replace index replacement lines =
  List.mapi (fun current line -> if current = index then replacement else line) lines

let parses_complete_tables () =
  let tables = parse_ok (table_source ()) in
  Alcotest.(check int) "language count" 48 (List.length tables.language);
  Alcotest.(check int) "assembly count" 25 (List.length tables.assembly);
  let first_language = List.hd tables.language in
  let last_assembly = List.hd (List.rev tables.assembly) in
  Alcotest.(check int) "first language ID" 0 first_language.templeos_id;
  Alcotest.(check int) "last assembly ID" 88 last_assembly.templeos_id

let rejects_malformed_record () =
  let language =
    List.init 48 language_line |> replace 3 "KEYWORD keyword_3 3"
  in
  expect_error "must end with one decimal ID and a semicolon"
    (table_source ~language ())

let rejects_duplicate_id () =
  let language =
    List.init 48 language_line |> replace 2 "KEYWORD keyword_2 1;"
  in
  expect_error "language keyword ID 1 appears more than once"
    (table_source ~language ())

let rejects_duplicate_spelling () =
  let assembly =
    List.init 25 assembly_line
    |> replace 2 "ASM_KEYWORD DIRECTIVE_1 66;"
  in
  expect_error "assembler directive spelling \"DIRECTIVE_1\" appears more than once"
    (table_source ~assembly ())

let rejects_unexpected_statement () =
  let language = List.init 48 language_line @ [ "R8 misplaced 0;" ] in
  expect_error "inside the language keyword table" (table_source ~language ())

let rejects_wrong_range () =
  let assembly =
    List.init 25 assembly_line
    |> replace 24 "ASM_KEYWORD DIRECTIVE_24 89;"
  in
  expect_error "requires ID 88 here, but found 89" (table_source ~assembly ())

let rejects_missing_range_member () =
  let language =
    List.init 48 language_line |> List.filteri (fun index _ -> index <> 12)
  in
  expect_error "requires ID 12 here, but found 13" (table_source ~language ())

let rejects_checksum_mismatch () =
  match Source.verify_sha256 ~expected:(String.make 64 '0') (table_source ()) with
  | Ok () -> Alcotest.fail "expected a source checksum mismatch"
  | Error problem ->
      Alcotest.(check bool)
        "checksum diagnostic names the mismatch" true
        (contains ~needle:"source SHA-256" (Source.error_to_string problem))

let normalizes_checkout_line_endings () =
  let lf = Source.verify_sha256 ~expected:"911169ddaaf146aff539f58c26c489af3b892dff0fe283c1c264c65ae5aa59a2" "a\nb\n" in
  let crlf =
    Source.verify_sha256
      ~expected:"911169ddaaf146aff539f58c26c489af3b892dff0fe283c1c264c65ae5aa59a2"
      "a\r\nb\r\n"
  in
  Alcotest.(check bool) "LF source checksum" true (Result.is_ok lf);
  Alcotest.(check bool) "CRLF checkout checksum" true (Result.is_ok crlf)

let rejects_late_record () =
  let source = table_source () ^ "\nKEYWORD late 48;" in
  expect_error "single ordered table sections" source

let parses_pinned_source () =
  let source =
    let channel = open_in_bin "../third_party/TempleOS/Compiler/OpCodes.DD" in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () -> really_input_string channel (in_channel_length channel))
  in
  let tables = parse_ok source in
  let first_language = List.hd tables.language in
  let last_language = List.hd (List.rev tables.language) in
  let first_assembly = List.hd tables.assembly in
  let last_assembly = List.hd (List.rev tables.assembly) in
  Alcotest.(check (list string))
    "language spellings" expected_language_spellings
    (List.map (fun entry -> entry.Source.spelling) tables.language);
  Alcotest.(check (list string))
    "assembler directive spellings" expected_assembly_spellings
    (List.map (fun entry -> entry.Source.spelling) tables.assembly);
  Alcotest.(check bool)
    "language record kinds" true
    (List.for_all
       (fun entry -> entry.Source.kind = Source.Language)
       tables.language);
  Alcotest.(check bool)
    "assembler record kinds" true
    (List.for_all
       (fun entry -> entry.Source.kind = Source.Assembly)
       tables.assembly);
  Alcotest.(check string) "first language spelling" "include"
    first_language.spelling;
  Alcotest.(check string) "last language spelling" "noargpop"
    last_language.spelling;
  Alcotest.(check string) "first assembly spelling" "ALIGN"
    first_assembly.spelling;
  Alcotest.(check string) "last assembly spelling" "BINFILE"
    last_assembly.spelling;
  Alcotest.(check (list int))
    "boundary source lines" [ 140; 187; 189; 213 ]
    [
      first_language.source_line;
      last_language.source_line;
      first_assembly.source_line;
      last_assembly.source_line;
    ]

let tests =
  [
    Alcotest.test_case "complete synthetic tables" `Quick parses_complete_tables;
    Alcotest.test_case "missing record semicolon" `Quick rejects_malformed_record;
    Alcotest.test_case "duplicate numeric ID" `Quick rejects_duplicate_id;
    Alcotest.test_case "duplicate spelling" `Quick rejects_duplicate_spelling;
    Alcotest.test_case "unexpected table statement" `Quick
      rejects_unexpected_statement;
    Alcotest.test_case "numeric range" `Quick rejects_wrong_range;
    Alcotest.test_case "missing range member" `Quick
      rejects_missing_range_member;
    Alcotest.test_case "checksum mismatch" `Quick rejects_checksum_mismatch;
    Alcotest.test_case "checkout line endings" `Quick
      normalizes_checkout_line_endings;
    Alcotest.test_case "record after opcode section" `Quick rejects_late_record;
    Alcotest.test_case "pinned OpCodes.DD" `Quick parses_pinned_source;
  ]
