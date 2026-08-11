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

let register_lines =
  [
    "R8 AL 0;";
    "R16 AX 0;";
    "R32 EAX 0;";
    "R64 RAX 0;";
    "SEG FS 4;";
    "FSTK ST3 3;";
    "MM MM7 7;";
    "XMM XMM7 7;";
  ]

let language_line index = Printf.sprintf "KEYWORD keyword_%d %d;" index index

let assembly_line index =
  Printf.sprintf "ASM_KEYWORD DIRECTIVE_%d %d;" index (index + 64)

let table_source ?(registers = register_lines)
    ?(language = List.init 48 language_line)
    ?(assembly = List.init 25 assembly_line)
    ?(opcodes = [ "OPCODE PUSH 0x50,+R R64: PUSH_ALIAS;" ]) () =
  String.concat "\n"
    (registers @ [ "" ] @ language @ [ "" ] @ assembly @ [ "" ] @ opcodes)

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
  | Ok _ ->
      Alcotest.fail "expected the opcode table parser to reject the fixture"
  | Error problem ->
      let diagnostic = Source.error_to_string problem in
      Alcotest.(check bool)
        (Printf.sprintf "%S appears in %S" needle diagnostic)
        true
        (contains ~needle diagnostic)

let replace index replacement lines =
  List.mapi
    (fun current line -> if current = index then replacement else line)
    lines

let find_opcode spelling opcodes =
  List.find
    (fun (opcode : Source.opcode) -> String.equal opcode.spelling spelling)
    opcodes

let parses_complete_tables () =
  let tables = parse_ok (table_source ()) in
  Alcotest.(check int) "register count" 8 (List.length tables.registers);
  Alcotest.(check int) "language count" 48 (List.length tables.language);
  Alcotest.(check int) "assembly count" 25 (List.length tables.assembly);
  Alcotest.(check int) "opcode count" 1 (List.length tables.opcodes);
  let first_language : Source.entry = List.hd tables.language in
  let last_assembly : Source.entry = List.hd (List.rev tables.assembly) in
  let push = List.hd tables.opcodes in
  Alcotest.(check int) "first language ID" 0 first_language.templeos_id;
  Alcotest.(check int) "last assembly ID" 88 last_assembly.templeos_id;
  Alcotest.(check string) "canonical opcode" "PUSH" push.spelling;
  Alcotest.(check (list string))
    "opcode aliases" [ "PUSH_ALIAS" ]
    (List.map
       (fun (alias : Source.opcode_alias) -> alias.spelling)
       push.aliases)

let parses_register_kinds () =
  let registers = (parse_ok (table_source ())).registers in
  Alcotest.(check (list bool))
    "register kinds"
    [ true; true; true; true; true; true; true; true ]
    (List.map2
       (fun (register : Source.register) expected ->
         register.register_kind = expected)
       registers
       [
         Source.R8;
         Source.R16;
         Source.R32;
         Source.R64;
         Source.Segment;
         Source.Float_stack;
         Source.Mm;
         Source.Xmm;
       ]);
  Alcotest.(check (list int))
    "register type IDs" [ 1; 2; 3; 4; 5; 6; 7; 8 ]
    (List.map
       (fun (register : Source.register) -> register.register_type)
       registers);
  Alcotest.(check (list int))
    "register numbers" [ 0; 0; 0; 0; 4; 3; 7; 7 ]
    (List.map
       (fun (register : Source.register) -> register.register_number)
       registers)

let parses_instruction_fields () =
  let source =
    table_source
      ~opcodes:
        [ "OPCODE FLAGS 0xAA 0xBB,ID 16 32 +R ! & % = ` ^ * $$ R64 IMM8;" ]
      ()
  in
  let instruction = List.hd (List.hd (parse_ok source).opcodes).instructions in
  Alcotest.(check (list int))
    "opcode bytes" [ 0xaa; 0xbb ] instruction.opcode_bytes;
  Alcotest.(check int) "entry index" 0 instruction.entry_index;
  Alcotest.(check int) "all instruction flags" 0x7ff instruction.flags;
  Alcotest.(check int) "slash value" 8 instruction.slash_value;
  Alcotest.(check int) "UAsm STI slash value" 10 instruction.uasm_slash_value;
  Alcotest.(check int) "opcode modifier" 7 instruction.opcode_modifier;
  Alcotest.(check (pair int int))
    "argument IDs" (15, 4)
    (instruction.argument1, instruction.argument2);
  Alcotest.(check (pair int int))
    "argument sizes" (64, 8)
    (instruction.size1, instruction.size2)

let parses_modifiers_and_slash_targets () =
  let tables =
    table_source
      ~opcodes:
        [
          "OPCODE MODIFIERS 0x00,NO 0x01,CB 0x02,CW 0x03,CD 0x04,CP 0x05,IB \
           0x06,IW 0x07,ID;";
          "OPCODE SLASHES 0x10,/0 0x11,/7 0x12,/R 0x13,/I;";
          "OPCODE PLUS 0x20,+0 0x21,+7 0x22,+R 0x23,+I;";
        ]
      ()
    |> parse_ok
  in
  let modifiers = find_opcode "MODIFIERS" tables.opcodes in
  Alcotest.(check (list int))
    "opcode modifiers" [ 0; 1; 2; 3; 4; 5; 6; 7 ]
    (List.map
       (fun (instruction : Source.instruction) -> instruction.opcode_modifier)
       modifiers.instructions);
  let slashes = find_opcode "SLASHES" tables.opcodes in
  Alcotest.(check (list int))
    "slash selectors" [ 0; 7; 8; 9 ]
    (List.map
       (fun (instruction : Source.instruction) -> instruction.slash_value)
       slashes.instructions);
  let plus = find_opcode "PLUS" tables.opcodes in
  Alcotest.(check (list int))
    "plus selectors" [ 0; 7; 8; 9 ]
    (List.map
       (fun (instruction : Source.instruction) -> instruction.slash_value)
       plus.instructions);
  Alcotest.(check (list int))
    "plus flags"
    [ 0x004; 0x004; 0x004; 0x004 ]
    (List.map
       (fun (instruction : Source.instruction) -> instruction.flags)
       plus.instructions)

let parses_comments () =
  let source =
    "/* outer\n/* nested */\ncomment */\n// register section\n"
    ^ table_source ()
  in
  let tables = parse_ok source in
  let first : Source.register = List.hd tables.registers in
  Alcotest.(check int) "line after comments" 5 first.source_line

let rejects_missing_semicolon () =
  let language =
    List.init 48 language_line |> replace 3 "KEYWORD keyword_3 3"
  in
  expect_error "expected ';'" (table_source ~language ())

let rejects_duplicate_id () =
  let language =
    List.init 48 language_line |> replace 2 "KEYWORD keyword_2 1;"
  in
  expect_error "language keyword ID 1 appears more than once"
    (table_source ~language ())

let rejects_duplicate_spelling () =
  let assembly =
    List.init 25 assembly_line |> replace 2 "ASM_KEYWORD DIRECTIVE_1 66;"
  in
  expect_error
    "assembler directive spelling \"DIRECTIVE_1\" appears more than once"
    (table_source ~assembly ())

let rejects_duplicate_register () =
  expect_error "register spelling \"AL\" appears more than once"
    (table_source ~registers:("R8 AL 1;" :: register_lines) ())

let rejects_duplicate_opcode_alias () =
  expect_error "opcode spelling \"SAME\" appears more than once"
    (table_source
       ~opcodes:[ "OPCODE FIRST 0x90,: SAME;"; "OPCODE SAME 0x91;" ]
       ())

let rejects_unexpected_statement () =
  expect_error "unknown opcode-table statement \"NOT_A_RECORD\""
    (table_source ~registers:(register_lines @ [ "NOT_A_RECORD X 0;" ]) ())

let rejects_late_register () =
  let language = List.init 48 language_line @ [ "R8 misplaced 0;" ] in
  expect_error "register records must precede" (table_source ~language ())

let rejects_late_keyword () =
  let source = table_source () ^ "\nKEYWORD late 48;" in
  expect_error "language keyword records must stay" source

let rejects_wrong_range () =
  let assembly =
    List.init 25 assembly_line |> replace 24 "ASM_KEYWORD DIRECTIVE_24 89;"
  in
  expect_error "requires ID 88 here, but found 89" (table_source ~assembly ())

let rejects_missing_range_member () =
  let language =
    List.init 48 language_line |> List.filteri (fun index _ -> index <> 12)
  in
  expect_error "requires ID 12 here, but found 13" (table_source ~language ())

let rejects_register_number () =
  expect_error "register number 256 is outside 0 through 255"
    (table_source ~registers:[ "R64 RAX 256;" ] ())

let rejects_opcode_byte () =
  expect_error "opcode byte 256 is outside 0 through 255"
    (table_source ~opcodes:[ "OPCODE BAD 0x100;" ] ())

let rejects_too_many_opcode_bytes () =
  expect_error "cannot contain more than four bytes"
    (table_source ~opcodes:[ "OPCODE BAD 1 2 3 4 5;" ] ())

let rejects_too_many_instruction_forms () =
  let forms = List.init 33 (fun _ -> "0x90,") |> String.concat " " in
  expect_error "exceeds the 32-form source limit"
    (table_source ~opcodes:[ "OPCODE MANY " ^ forms ^ ";" ] ())

let rejects_unknown_argument () =
  expect_error "unknown ST_ARG_TYPES name \"MYSTERY\""
    (table_source ~opcodes:[ "OPCODE BAD 0x90,MYSTERY;" ] ())

let rejects_empty_alias_list () =
  expect_error "alias list cannot be empty"
    (table_source ~opcodes:[ "OPCODE BAD 0x90,:;" ] ())

let rejects_unterminated_comment () =
  expect_error "unterminated block comment" (table_source () ^ "\n/* open")

let rejects_checksum_mismatch () =
  match
    Source.verify_sha256 ~expected:(String.make 64 '0') (table_source ())
  with
  | Ok () -> Alcotest.fail "expected a source checksum mismatch"
  | Error problem ->
      Alcotest.(check bool)
        "checksum diagnostic names the mismatch" true
        (contains ~needle:"source SHA-256" (Source.error_to_string problem))

let normalizes_checkout_line_endings () =
  let expected =
    "911169ddaaf146aff539f58c26c489af3b892dff0fe283c1c264c65ae5aa59a2"
  in
  let lf = Source.verify_sha256 ~expected "a\nb\n" in
  let crlf = Source.verify_sha256 ~expected "a\r\nb\r\n" in
  Alcotest.(check bool) "LF source checksum" true (Result.is_ok lf);
  Alcotest.(check bool) "CRLF checkout checksum" true (Result.is_ok crlf)

let read_pinned_source () =
  let candidates =
    [
      "../third_party/TempleOS/Compiler/OpCodes.DD";
      "third_party/TempleOS/Compiler/OpCodes.DD";
    ]
  in
  let path =
    match List.find_opt Sys.file_exists candidates with
    | Some path -> path
    | None -> Alcotest.fail "cannot find the pinned Compiler/OpCodes.DD"
  in
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let parses_pinned_table_shape () =
  let tables = parse_ok (read_pinned_source ()) in
  let aliases =
    List.fold_left
      (fun count (opcode : Source.opcode) -> count + List.length opcode.aliases)
      0 tables.opcodes
  in
  let instruction_forms =
    List.fold_left
      (fun count (opcode : Source.opcode) ->
        count + List.length opcode.instructions)
      0 tables.opcodes
  in
  Alcotest.(check int)
    "pinned register count" 106
    (List.length tables.registers);
  Alcotest.(check int) "pinned language count" 48 (List.length tables.language);
  Alcotest.(check int) "pinned assembly count" 25 (List.length tables.assembly);
  Alcotest.(check int)
    "pinned canonical opcode count" 325
    (List.length tables.opcodes);
  Alcotest.(check int) "pinned alias count" 49 aliases;
  Alcotest.(check int) "pinned instruction form count" 924 instruction_forms;
  Alcotest.(check (list int))
    "register-kind counts"
    [ 20; 16; 16; 24; 6; 8; 8; 8 ]
    (List.map
       (fun kind ->
         List.fold_left
           (fun count (register : Source.register) ->
             if register.register_kind = kind then count + 1 else count)
           0 tables.registers)
       [
         Source.R8;
         Source.R16;
         Source.R32;
         Source.R64;
         Source.Segment;
         Source.Float_stack;
         Source.Mm;
         Source.Xmm;
       ])

let parses_pinned_names_and_provenance () =
  let tables = parse_ok (read_pinned_source ()) in
  Alcotest.(check (list string))
    "language spellings" expected_language_spellings
    (List.map (fun (entry : Source.entry) -> entry.spelling) tables.language);
  Alcotest.(check (list string))
    "assembler directive spellings" expected_assembly_spellings
    (List.map (fun (entry : Source.entry) -> entry.spelling) tables.assembly);
  let first_register : Source.register = List.hd tables.registers in
  let last_register : Source.register = List.hd (List.rev tables.registers) in
  let first_language : Source.entry = List.hd tables.language in
  let last_language : Source.entry = List.hd (List.rev tables.language) in
  let first_assembly : Source.entry = List.hd tables.assembly in
  let last_assembly : Source.entry = List.hd (List.rev tables.assembly) in
  let first_opcode : Source.opcode = List.hd tables.opcodes in
  let last_opcode : Source.opcode = List.hd (List.rev tables.opcodes) in
  Alcotest.(check (list string))
    "table boundaries"
    [
      "AL";
      "XMM7";
      "include";
      "noargpop";
      "ALIGN";
      "BINFILE";
      "PUSH";
      "MOV_RAX_CR4";
    ]
    [
      first_register.spelling;
      last_register.spelling;
      first_language.spelling;
      last_language.spelling;
      first_assembly.spelling;
      last_assembly.spelling;
      first_opcode.spelling;
      last_opcode.spelling;
    ];
  Alcotest.(check (list int))
    "boundary source lines"
    [ 26; 138; 140; 187; 189; 213; 215; 1297 ]
    [
      first_register.source_line;
      last_register.source_line;
      first_language.source_line;
      last_language.source_line;
      first_assembly.source_line;
      last_assembly.source_line;
      first_opcode.source_line;
      last_opcode.source_line;
    ]

let parses_pinned_instruction_forms () =
  let tables = parse_ok (read_pinned_source ()) in
  let mov = find_opcode "MOV" tables.opcodes in
  let mov_rm64 = List.nth mov.instructions 7 in
  Alcotest.(check (list int)) "MOV bytes" [ 0x8b ] mov_rm64.opcode_bytes;
  Alcotest.(check int) "MOV flags" 0x002 mov_rm64.flags;
  Alcotest.(check int) "MOV slash" 8 mov_rm64.slash_value;
  Alcotest.(check (pair int int))
    "MOV arguments" (15, 19)
    (mov_rm64.argument1, mov_rm64.argument2);
  Alcotest.(check (pair int int))
    "MOV sizes" (64, 64)
    (mov_rm64.size1, mov_rm64.size2);
  let enter =
    find_opcode "ENTER" tables.opcodes |> fun opcode ->
    List.hd opcode.instructions
  in
  Alcotest.(check int) "ENTER ending-zero flag" 0x400 enter.flags;
  Alcotest.(check int) "ENTER immediate-word modifier" 6 enter.opcode_modifier;
  let fld =
    find_opcode "FLD" tables.opcodes |> fun opcode ->
    List.nth opcode.instructions 2
  in
  Alcotest.(check int) "FLD plus-I and STI flags" 0x204 fld.flags;
  Alcotest.(check int) "FLD slash" 9 fld.slash_value;
  Alcotest.(check int) "FLD UAsm slash" 9 fld.uasm_slash_value;
  let je = find_opcode "JE" tables.opcodes in
  Alcotest.(check (list string))
    "JE alias" [ "JZ" ]
    (List.map (fun (alias : Source.opcode_alias) -> alias.spelling) je.aliases);
  let shl = find_opcode "SHL" tables.opcodes in
  Alcotest.(check (list string))
    "SHL alias" [ "SAL" ]
    (List.map (fun (alias : Source.opcode_alias) -> alias.spelling) shl.aliases)

let preserves_pinned_empty_instruction_form () =
  let tables = parse_ok (read_pinned_source ()) in
  let rep_outsb = find_opcode "REP_OUTSB" tables.opcodes in
  Alcotest.(check int)
    "REP_OUTSB form count" 3
    (List.length rep_outsb.instructions);
  let empty = List.hd rep_outsb.instructions in
  Alcotest.(check (list int)) "empty opcode bytes" [] empty.opcode_bytes;
  Alcotest.(check int) "empty form source line" 920 empty.source_line

let parses_deterministically () =
  let source = read_pinned_source () in
  Alcotest.(check bool)
    "repeatable parsed tables" true
    (parse_ok source = parse_ok source)

let tests =
  [
    Alcotest.test_case "complete synthetic tables" `Quick parses_complete_tables;
    Alcotest.test_case "register kinds" `Quick parses_register_kinds;
    Alcotest.test_case "instruction fields" `Quick parses_instruction_fields;
    Alcotest.test_case "modifiers and slash targets" `Quick
      parses_modifiers_and_slash_targets;
    Alcotest.test_case "nested comments" `Quick parses_comments;
    Alcotest.test_case "missing record semicolon" `Quick
      rejects_missing_semicolon;
    Alcotest.test_case "duplicate numeric ID" `Quick rejects_duplicate_id;
    Alcotest.test_case "duplicate keyword spelling" `Quick
      rejects_duplicate_spelling;
    Alcotest.test_case "duplicate register spelling" `Quick
      rejects_duplicate_register;
    Alcotest.test_case "duplicate opcode alias" `Quick
      rejects_duplicate_opcode_alias;
    Alcotest.test_case "unknown statement" `Quick rejects_unexpected_statement;
    Alcotest.test_case "late register record" `Quick rejects_late_register;
    Alcotest.test_case "late keyword record" `Quick rejects_late_keyword;
    Alcotest.test_case "numeric range" `Quick rejects_wrong_range;
    Alcotest.test_case "missing range member" `Quick
      rejects_missing_range_member;
    Alcotest.test_case "register number range" `Quick rejects_register_number;
    Alcotest.test_case "opcode byte range" `Quick rejects_opcode_byte;
    Alcotest.test_case "opcode byte count" `Quick rejects_too_many_opcode_bytes;
    Alcotest.test_case "opcode form count" `Quick
      rejects_too_many_instruction_forms;
    Alcotest.test_case "unknown argument type" `Quick rejects_unknown_argument;
    Alcotest.test_case "empty alias list" `Quick rejects_empty_alias_list;
    Alcotest.test_case "unterminated block comment" `Quick
      rejects_unterminated_comment;
    Alcotest.test_case "checksum mismatch" `Quick rejects_checksum_mismatch;
    Alcotest.test_case "checkout line endings" `Quick
      normalizes_checkout_line_endings;
    Alcotest.test_case "pinned table shape" `Quick parses_pinned_table_shape;
    Alcotest.test_case "pinned names and provenance" `Quick
      parses_pinned_names_and_provenance;
    Alcotest.test_case "pinned instruction forms" `Quick
      parses_pinned_instruction_forms;
    Alcotest.test_case "pinned empty instruction form" `Quick
      preserves_pinned_empty_instruction_form;
    Alcotest.test_case "deterministic pinned parse" `Quick
      parses_deterministically;
  ]
