open Holyc_lib
module VM = Ir_integer_interpreter

let inputs ?(mode = Preprocessor.Jit) text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"expression.hc" ~contents:text
  in
  let config =
    match Preprocessor.Config.create ~compilation_mode:mode () with
    | Ok config -> config
    | Error message -> Alcotest.fail message
  in
  (session, config, source)

let evaluate ?mode ?(max_steps = 100) text =
  let session, config, source = inputs ?mode text in
  evaluate_integer_expression session ~config ~source ~max_steps

let checked = function
  | Ok value -> value
  | Error diagnostics ->
      diagnostics
      |> List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
      |> String.concat "; " |> Alcotest.fail

let word result =
  match VM.termination result with
  | VM.Returned (Some word) -> word
  | _ -> Alcotest.fail "expected a returned integer word"

let expect_word ?mode text expected_type expected_bits () =
  let actual = evaluate ?mode text |> checked |> word in
  Alcotest.(check bool) "word type" true (actual.type_ = expected_type);
  Alcotest.(check int64) "word bits" expected_bits actual.bits

let error ?(max_steps = 100) text =
  match evaluate ~max_steps text with
  | Ok _ -> Alcotest.fail "expected a diagnostic and no result"
  | Error [] -> Alcotest.fail "expected at least one diagnostic"
  | Error (first :: _) ->
      Alcotest.(check bool)
        "source-positioned" true
        (first.primary.start >= 0 && first.primary.stop >= first.primary.start);
      first

let budget () =
  let result = evaluate ~max_steps:5 "(6*7);" |> checked in
  Alcotest.(check int64) "returned bits" 42L (word result).bits;
  Alcotest.(check int) "return tail consumes steps" 5 (VM.executed_steps result);
  Alcotest.(check string)
    "exhaustion code" "HCIRVM0007" (error ~max_steps:4 "(6*7);").code;
  List.iter
    (fun max_steps ->
      Alcotest.(check string)
        "budget precedes parsing" "HCIRVM0001"
        (error ~max_steps "@ invalid syntax").code)
    [ 0; -1 ]

let boundary () =
  List.iter
    (fun text -> Alcotest.(check string) text "HCEVAL0001" (error text).code)
    [ ""; ";"; "1;2;"; "I64 x;"; "if(1) 2;"; "\"hello\";"; "{1;}" ];
  Alcotest.(check string)
    "parser code survives" "HCPARSE0018" (error "(6*);").code;
  Alcotest.(check string)
    "existing parser requires a delimiter" "HCPARSE0047" (error "6*7").code

let unsupported () =
  Alcotest.(check string)
    "unsupported expression tree" "HCEVAL0002" (error "~2.0;").code;
  let session, config, source = inputs "1.0;" in
  ignore (lower_integer_expression session ~config ~source |> checked);
  let diagnostic = error "1.0;" in
  Alcotest.(check bool)
    "VM owns floating domain rejection" true
    (String.starts_with ~prefix:"HCIRVM" diagnostic.code);
  ignore (error "1.0/0.0;" : Diagnostic.t)

let arithmetic_faults () =
  List.iter
    (fun text -> Alcotest.(check string) text "HCIRVM0009" (error text).code)
    [ "1/0;"; "1%0;"; "0&&(1/0);"; "1||(1%0);" ];
  List.iter
    (fun text -> Alcotest.(check string) text "HCIRVM0010" (error text).code)
    [ "(-9223372036854775807-1)/-1;"; "(-9223372036854775807-1)%-1;" ]

let replay_and_folding () =
  let lower () =
    let session, config, source = inputs "~(-42);" in
    lower_integer_expression session ~config ~source |> checked
  in
  let graph = lower () in
  Alcotest.(check string)
    "deterministic graph replay"
    (Ir_x87_stack.graph graph |> Ir_block_graph.human)
    (Ir_x87_stack.graph (lower ()) |> Ir_block_graph.human);
  let folded =
    match Ir_integer_unary_folding.fold graph with
    | Ok value -> Ir_integer_unary_folding.x87 value
    | Error _ -> Alcotest.fail "folding failed"
  in
  let execute graph =
    match VM.execute ~max_steps:100 graph with
    | Ok result -> result
    | Error _ -> Alcotest.fail "interpreter rejected verified expression"
  in
  let original = execute graph and folded = execute folded in
  Alcotest.(check int64) "original result" 41L (word original).bits;
  Alcotest.(check int64) "folded result" 41L (word folded).bits;
  Alcotest.(check bool)
    "folding reduced execution work" true
    (VM.executed_steps folded < VM.executed_steps original)

let tests =
  let cases =
    [
      ("multiply", "(6*7);", VM.I64, 42L);
      ("nested arithmetic", "(2+3)*(11-4);", VM.I64, 35L);
      ("division and remainder", "(85/2)+(85%2);", VM.I64, 43L);
      ("signed truncation", "-7/3;", VM.I64, -2L);
      ("signed remainder", "-7%3;", VM.I64, -1L);
      ( "unsigned division",
        "0xFFFFFFFFFFFFFFFF/3;",
        VM.U64,
        6148914691236517205L );
      ("mixed division", "-1/0x8000000000000000;", VM.U64, 1L);
      ( "unsigned remainder",
        "0xFFFFFFFFFFFFFFFF%0x8000000000000000;",
        VM.U64,
        Int64.max_int );
      ("negative", "-42;", VM.I64, -42L);
      ("high bit", "0x8000000000000000;", VM.U64, Int64.min_int);
      ("unsigned maximum", "0xFFFFFFFFFFFFFFFF;", VM.U64, -1L);
      ("bitwise", "(15&6)|(8^3);", VM.I64, 15L);
      ("shift", "(3<<4)>>2;", VM.I64, 12L);
      ("runtime masked shift", "1<<64;", VM.I64, 1L);
      ("unsigned right shift", "0x8000000000000000>>63;", VM.U64, 1L);
      ("unsigned wrap", "0xFFFFFFFFFFFFFFFF+1;", VM.U64, 0L);
      ("signed comparison", "-1<0;", VM.I64, 1L);
      ("unsigned comparison", "0xFFFFFFFFFFFFFFFF>1;", VM.I64, 1L);
      ("logical values", "(3&&4)+(0||2)+(1^^1);", VM.I64, 2L);
      ("definition expansion", "#define ANSWER (6*7)\nANSWER;", VM.I64, 42L);
    ]
  in
  List.map
    (fun (name, text, type_, bits) ->
      Alcotest.test_case name `Quick (expect_word text type_ bits))
    cases
  @ [
      Alcotest.test_case "JIT preprocessing" `Quick
        (expect_word ~mode:Preprocessor.Jit "#ifjit\n42;\n#else\n7;\n#endif\n"
           VM.I64 42L);
      Alcotest.test_case "AOT preprocessing" `Quick
        (expect_word ~mode:Preprocessor.Aot "#ifjit\n42;\n#else\n7;\n#endif\n"
           VM.I64 7L);
      Alcotest.test_case "exact budget" `Quick budget;
      Alcotest.test_case "input boundary" `Quick boundary;
      Alcotest.test_case "unsupported operations" `Quick unsupported;
      Alcotest.test_case "division faults and eager logical values" `Quick
        arithmetic_faults;
      Alcotest.test_case "replay and unary folding" `Quick replay_and_folding;
    ]
