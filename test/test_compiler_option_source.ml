module Source = Compiler_option_source

let definitions =
  [
    "#define OPTf_ECHO 0x00";
    "#define OPTf_TRACE 0x01";
    "#define OPTf_WARN_UNUSED_VAR 0x10 //Applied to funs, not stmts";
    "#define OPTf_WARN_PAREN 0x11 //Warn unnecessary parens";
    "#define OPTf_WARN_DUP_TYPES 0x12 //Warn dup local var type stmts";
    "#define OPTf_WARN_HEADER_MISMATCH 0x13";
    "#define OPTf_EXTERNS_TO_IMPORTS 0x20";
    "#define OPTf_KEEP_PRIVATE 0x21";
    "#define OPTf_NO_REG_VAR 0x22 //Applied to funs, not stmts";
    "#define OPTf_GLBLS_ON_DATA_HEAP 0x23";
    "#define OPTf_NO_BUILTIN_CONST 0x24 //Applied to funs, not stmts";
    "#define OPTf_USE_IMM64 0x25 //Not completely implemented";
  ]

let kernel_source ?(definitions = definitions) ?(echo_mask = true) () =
  String.concat "\n"
    ([ "// synthetic header" ] @ definitions
    @ (if echo_mask then [ "#define OPTF_ECHO (1<<OPTf_ECHO)" ] else [])
    @ [ "" ])

let lex_source
    ?(defaults = "1<<OPTf_WARN_UNUSED_VAR|1<<OPTf_WARN_HEADER_MISMATCH") () =
  String.concat "\n"
    [
      "CCmpCtrl *CmpCtrlNew()";
      "{";
      "  CCmpCtrl *cc;";
      Printf.sprintf "  cc->opts=%s;" defaults;
      "  if (cc->opts & OPTF_ECHO) {}";
      "}";
      "";
    ]

let cmisc_source =
  String.concat "\n"
    [
      "Bool Option(I64 num,Bool val)";
      "{";
      "  return BEqu(&Fs->last_cc->opts,num,val);";
      "}";
      "";
      "Bool GetOption(I64 num)";
      "{";
      "  return Bt(&Fs->last_cc->opts,num);";
      "}";
      "";
      "Bool Trace(Bool val=ON) { return Option(OPTf_TRACE,val); }";
      "Bool Echo(Bool val) { return Option(OPTf_ECHO,val); }";
      "";
    ]

let bequ_source =
  String.concat "\n"
    [
      "_BEQU::";
      "\tBTS\tU64 [RBX],RCX";
      "\tBTR\tU64 [RBX],RCX";
      "\tADC\tAL,0";
      "\tRET1\t24";
      "_LBEQU::";
      "";
    ]

let consumer_source =
  String.concat "\n"
    [
      "OPTF_ECHO";
      "OPTf_TRACE";
      "OPTf_WARN_UNUSED_VAR";
      "OPTf_WARN_PAREN";
      "OPTf_WARN_DUP_TYPES";
      "OPTf_WARN_HEADER_MISMATCH";
      "OPTf_EXTERNS_TO_IMPORTS";
      "OPTf_KEEP_PRIVATE";
      "OPTf_NO_REG_VAR";
      "OPTf_GLBLS_ON_DATA_HEAP";
      "OPTf_NO_BUILTIN_CONST";
      "OPTf_USE_IMM64";
      "";
    ]

let parse ?kernel ?lex ?(cmisc = cmisc_source) ?(bequ = bequ_source)
    ?(consumers = [ ("Consumer.HC", consumer_source) ]) () =
  Source.parse
    ~kernel_source:(Option.value ~default:(kernel_source ()) kernel)
    ~lex_source:(Option.value ~default:(lex_source ()) lex)
    ~cmisc_source:cmisc ~bequ_source:bequ ~consumers

let parse_ok ?kernel ?lex ?cmisc ?bequ ?consumers () =
  match parse ?kernel ?lex ?cmisc ?bequ ?consumers () with
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

let expect_error ?kernel ?lex ?cmisc ?bequ ?consumers needle =
  match parse ?kernel ?lex ?cmisc ?bequ ?consumers () with
  | Ok _ -> Alcotest.fail "expected the compiler-option source to be rejected"
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

let parses_complete_registry () =
  let tables = parse_ok () in
  Alcotest.(check int) "option count" 12 (List.length tables.options);
  Alcotest.(check (list int))
    "bit indices"
    [ 0; 1; 16; 17; 18; 19; 32; 33; 34; 35; 36; 37 ]
    (List.map
       (fun (entry : Source.option_entry) -> entry.bit_index)
       tables.options);
  Alcotest.(check (list string))
    "initial options"
    [ "OPTf_WARN_UNUSED_VAR"; "OPTf_WARN_HEADER_MISMATCH" ]
    (tables.options
    |> List.filter (fun (entry : Source.option_entry) ->
        entry.initially_enabled)
    |> List.map (fun (entry : Source.option_entry) -> entry.name));
  Alcotest.(check (list (pair int int)))
    "documented gaps"
    [ (2, 15); (20, 31) ]
    (List.map (fun (gap : Source.gap) -> (gap.first, gap.last)) tables.gaps);
  Alcotest.(check int) "default line" 4 tables.default_source_line;
  Alcotest.(check bool)
    "Option returns old state" true tables.api.set_returns_previous

let recognizes_echo_mask_consumer () =
  let tables = parse_ok () in
  let echo = List.hd tables.options in
  Alcotest.(check (list (pair string int)))
    "echo consumers"
    [ ("Consumer.HC", 1) ]
    (List.map
       (fun (reference : Source.source_reference) ->
         (reference.path, reference.line))
       echo.consumers)

let rejects_malformed_index () =
  let changed = definitions |> replace 3 "#define OPTf_WARN_PAREN seventeen" in
  expect_error
    ~kernel:(kernel_source ~definitions:changed ())
    "invalid bit index"

let rejects_duplicate_name () =
  let changed = definitions |> replace 1 "#define OPTf_ECHO 0x01" in
  expect_error
    ~kernel:(kernel_source ~definitions:changed ())
    "OPTf_ECHO is defined more than once"

let rejects_duplicate_index () =
  let changed = definitions |> replace 1 "#define OPTf_TRACE 0x00" in
  expect_error
    ~kernel:(kernel_source ~definitions:changed ())
    "bit 0x0 is assigned more than once"

let rejects_missing_definition () =
  let changed = List.filteri (fun index _ -> index <> 3) definitions in
  expect_error
    ~kernel:(kernel_source ~definitions:changed ())
    "requires OPTf_WARN_PAREN here"

let rejects_reordered_definitions () =
  let changed = swap 3 4 definitions in
  expect_error
    ~kernel:(kernel_source ~definitions:changed ())
    "requires OPTf_WARN_PAREN here"

let rejects_out_of_range_index () =
  let changed =
    definitions
    |> replace 11 "#define OPTf_USE_IMM64 64 //Not completely implemented"
  in
  expect_error
    ~kernel:(kernel_source ~definitions:changed ())
    "outside the 64-bit option field"

let rejects_changed_scope_comment () =
  let changed = definitions |> replace 8 "#define OPTf_NO_REG_VAR 0x22" in
  expect_error
    ~kernel:(kernel_source ~definitions:changed ())
    "Applied to funs, not stmts"

let rejects_missing_echo_mask () =
  expect_error
    ~kernel:(kernel_source ~echo_mask:false ())
    "OPTF_ECHO is missing"

let rejects_changed_defaults () =
  expect_error
    ~lex:(lex_source ~defaults:"1<<OPTf_WARN_UNUSED_VAR" ())
    "initial option state no longer enables"

let rejects_unknown_default () =
  expect_error
    ~lex:
      (lex_source ~defaults:"1<<OPTf_WARN_UNUSED_VAR|1<<OPTf_NOT_AN_OPTION" ())
    "unknown option OPTf_NOT_AN_OPTION"

let rejects_duplicate_default () =
  expect_error
    ~lex:
      (lex_source ~defaults:"1<<OPTf_WARN_UNUSED_VAR|1<<OPTf_WARN_UNUSED_VAR" ())
    "more than once"

let ignores_noncode_consumer_text () =
  let source =
    consumer_source ^ "// OPTf_UNKNOWN_IN_COMMENT\n"
    ^ "/* OPTf_UNKNOWN_IN_BLOCK /* OPTf_UNKNOWN_IN_NESTED_BLOCK */ */\n"
    ^ "\"OPTf_UNKNOWN_IN_STRING\";\n" ^ "'OPTf_UNKNOWN_IN_CHAR';\n"
  in
  ignore (parse_ok ~consumers:[ ("Consumer.HC", source) ] ())

let rejects_unknown_consumer () =
  expect_error
    ~consumers:[ ("Consumer.HC", consumer_source ^ "OPTf_UNKNOWN;\n") ]
    "unknown compiler option OPTf_UNKNOWN"

let rejects_changed_option_api () =
  let changed =
    String.concat "\n"
      [
        "Bool Option(I64 num,Bool val)";
        "{";
        "  return Bt(&Fs->last_cc->opts,num);";
        "}";
        "Bool GetOption(I64 num)";
        "{";
        "  return Bt(&Fs->last_cc->opts,num);";
        "}";
      ]
  in
  expect_error ~cmisc:changed "must update `Fs->last_cc->opts` through BEqu"

let rejects_changed_bequ_contract () =
  let changed =
    String.concat "\n" [ "_BEQU::"; "\tBTS\tU64 [RBX],RCX"; "_LBEQU::" ]
  in
  expect_error ~bequ:changed "previous bit through the carry flag"

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
  Alcotest.(check bool) "same parsed registry" true (parse () = parse ())

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let pinned_consumer_paths =
  [
    "Compiler/Lex.HC";
    "Compiler/CMisc.HC";
    "Compiler/CExcept.HC";
    "Compiler/CMain.HC";
    "Compiler/LexLib.HC";
    "Compiler/PrsLib.HC";
    "Compiler/PrsStmt.HC";
    "Compiler/PrsVar.HC";
    "Compiler/OptPass3.HC";
    "Compiler/OptPass6.HC";
    "Compiler/OptPass789A.HC";
    "Compiler/BackFA.HC";
    "Compiler/BackLib.HC";
    "Kernel/KHashB.HC";
  ]

let pinned path = read_file ("../third_party/TempleOS/" ^ path)

let parses_pinned_sources () =
  let tables =
    parse_ok
      ~kernel:(pinned "Kernel/KernelA.HH")
      ~lex:(pinned "Compiler/Lex.HC")
      ~cmisc:(pinned "Compiler/CMisc.HC")
      ~bequ:(pinned "Kernel/KUtils.HC")
      ~consumers:
        (List.map (fun path -> (path, pinned path)) pinned_consumer_paths)
      ()
  in
  Alcotest.(check int) "pinned option count" 12 (List.length tables.options);
  let use_imm64 = List.nth tables.options 11 in
  Alcotest.(check (list (pair string int)))
    "USE_IMM64 consumers"
    [ ("Compiler/OptPass789A.HC", 359); ("Compiler/BackFA.HC", 285) ]
    (List.map
       (fun (reference : Source.source_reference) ->
         (reference.path, reference.line))
       use_imm64.consumers)

let tests =
  [
    Alcotest.test_case "complete registry" `Quick parses_complete_registry;
    Alcotest.test_case "echo mask consumer" `Quick recognizes_echo_mask_consumer;
    Alcotest.test_case "malformed bit index" `Quick rejects_malformed_index;
    Alcotest.test_case "duplicate name" `Quick rejects_duplicate_name;
    Alcotest.test_case "duplicate bit index" `Quick rejects_duplicate_index;
    Alcotest.test_case "missing definition" `Quick rejects_missing_definition;
    Alcotest.test_case "definition order" `Quick rejects_reordered_definitions;
    Alcotest.test_case "64-bit range" `Quick rejects_out_of_range_index;
    Alcotest.test_case "scope comments" `Quick rejects_changed_scope_comment;
    Alcotest.test_case "echo mask" `Quick rejects_missing_echo_mask;
    Alcotest.test_case "default state" `Quick rejects_changed_defaults;
    Alcotest.test_case "unknown default" `Quick rejects_unknown_default;
    Alcotest.test_case "duplicate default" `Quick rejects_duplicate_default;
    Alcotest.test_case "comments and literals" `Quick
      ignores_noncode_consumer_text;
    Alcotest.test_case "unknown consumer" `Quick rejects_unknown_consumer;
    Alcotest.test_case "Option API" `Quick rejects_changed_option_api;
    Alcotest.test_case "BEqu contract" `Quick rejects_changed_bequ_contract;
    Alcotest.test_case "checksum mismatch" `Quick rejects_checksum_mismatch;
    Alcotest.test_case "checkout line endings" `Quick
      normalizes_checkout_line_endings;
    Alcotest.test_case "deterministic parse" `Quick deterministic_parse;
    Alcotest.test_case "pinned sources" `Quick parses_pinned_sources;
  ]
