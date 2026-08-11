module Source = Operator_table_source

type sources = {
  kernel : string;
  compiler : string;
  cinit : string;
  lex : string;
}

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let pinned_sources () =
  {
    kernel = read_file "../third_party/TempleOS/Kernel/KernelA.HH";
    compiler = read_file "../third_party/TempleOS/Compiler/CompilerA.HH";
    cinit = read_file "../third_party/TempleOS/Compiler/CInit.HC";
    lex = read_file "../third_party/TempleOS/Compiler/Lex.HC";
  }

let contains ~needle text =
  let needle_length = String.length needle in
  let text_length = String.length text in
  let rec search offset =
    if offset + needle_length > text_length then false
    else if String.sub text offset needle_length = needle then true
    else search (offset + 1)
  in
  needle_length = 0 || search 0

let find ~needle text =
  let needle_length = String.length needle in
  let text_length = String.length text in
  let rec search offset =
    if offset + needle_length > text_length then None
    else if String.sub text offset needle_length = needle then Some offset
    else search (offset + 1)
  in
  search 0

let replace_once ~needle ~replacement text =
  match find ~needle text with
  | None ->
      Alcotest.fail (Printf.sprintf "fixture text does not contain %S" needle)
  | Some offset ->
      let buffer = Buffer.create (String.length text) in
      Buffer.add_substring buffer text 0 offset;
      Buffer.add_string buffer replacement;
      let suffix = offset + String.length needle in
      Buffer.add_substring buffer text suffix (String.length text - suffix);
      Buffer.contents buffer

let swap_once first second text =
  let marker = "__HOLYC_OPERATOR_SWAP__" in
  text
  |> replace_once ~needle:first ~replacement:marker
  |> replace_once ~needle:second ~replacement:first
  |> replace_once ~needle:marker ~replacement:second

let parse sources =
  Source.parse ~kernel_source:sources.kernel ~compiler_source:sources.compiler
    ~cinit_source:sources.cinit ~lex_source:sources.lex

let parse_ok sources =
  match parse sources with
  | Ok tables -> tables
  | Error problem -> Alcotest.fail (Source.error_to_string problem)

let expect_error needle sources =
  match parse sources with
  | Ok _ ->
      Alcotest.fail "expected the operator table parser to reject the fixture"
  | Error problem ->
      Alcotest.(check bool)
        (Printf.sprintf "error mentions %S" needle)
        true
        (contains ~needle (Source.error_to_string problem))

let expected_operator_spellings =
  [
    "<<=";
    ">>=";
    "...";
    "++";
    "--";
    "->";
    "::";
    "<<";
    ">>";
    "==";
    "!=";
    "<=";
    ">=";
    "&&";
    "||";
    "^^";
    "*=";
    "/=";
    "%=";
    "&=";
    "|=";
    "^=";
    "+=";
    "-=";
    "..";
    "$$";
  ]

let expected_binary_spellings =
  [
    "`";
    "<<";
    ">>";
    "*";
    "/";
    "%";
    "&";
    "^";
    "|";
    "+";
    "-";
    "<";
    ">";
    "<=";
    ">=";
    "==";
    "!=";
    "&&";
    "^^";
    "||";
    "=";
    "<<=";
    ">>=";
    "*=";
    "/=";
    "%=";
    "&=";
    "|=";
    "^=";
    "+=";
    "-=";
  ]

let parses_pinned_tables () =
  let sources = pinned_sources () in
  let tables = parse_ok sources in
  Alcotest.(check int) "token constants" 44 (List.length tables.tokens);
  Alcotest.(check int)
    "association constants" 3
    (List.length tables.association_flags);
  Alcotest.(check int)
    "precedence constants" 17
    (List.length tables.precedences);
  Alcotest.(check int) "dual sequences" 23 (List.length tables.dual_sequences);
  Alcotest.(check int) "operators" 26 (List.length tables.operators);
  Alcotest.(check int)
    "binary operators" 31
    (List.length tables.binary_operators);
  Alcotest.(check (list string))
    "operator order" expected_operator_spellings
    (List.map
       (fun (entry : Source.operator) -> entry.spelling)
       tables.operators);
  Alcotest.(check (list string))
    "binary order" expected_binary_spellings
    (List.map
       (fun (entry : Source.binary_operator) -> entry.spelling)
       tables.binary_operators)

let checks_dual_groups_and_comments () =
  let tables = parse_ok (pinned_sources ()) in
  Alcotest.(check (list int))
    "dual group sizes" [ 13; 8; 2 ]
    [
      List.length
        (List.filter
           (fun (entry : Source.dual_sequence) -> entry.group = 1)
           tables.dual_sequences);
      List.length
        (List.filter
           (fun (entry : Source.dual_sequence) -> entry.group = 2)
           tables.dual_sequences);
      List.length
        (List.filter
           (fun (entry : Source.dual_sequence) -> entry.group = 3)
           tables.dual_sequences);
    ];
  let block = List.nth tables.dual_sequences 5 in
  let line = List.nth tables.dual_sequences 16 in
  Alcotest.(check string) "block opener" "/*" block.spelling;
  Alcotest.(check bool) "block kind" true (block.kind = Source.Block_comment);
  Alcotest.(check string) "line opener" "//" line.spelling;
  Alcotest.(check bool) "line kind" true (line.kind = Source.Line_comment)

let checks_precedence_and_ic_boundaries () =
  let tables = parse_ok (pinned_sources ()) in
  Alcotest.(check (list int))
    "precedence values"
    (List.init 17 (fun index -> index * 4))
    (List.map
       (fun (entry : Source.named_constant) -> entry.value)
       tables.precedences);
  let power = List.hd tables.binary_operators in
  let shift = List.nth tables.binary_operators 1 in
  let multiply = List.nth tables.binary_operators 3 in
  let modulo_assign = List.nth tables.binary_operators 25 in
  Alcotest.(check bool)
    "power is right associative" true
    (power.association = Source.Right);
  Alcotest.(check string) "power IC" "IC_POWER" power.ic_name;
  Alcotest.(check int) "power IC ID" 0x2F power.ic_id;
  Alcotest.(check bool)
    "shift carries left flag" true
    (shift.association = Source.Left);
  Alcotest.(check bool)
    "multiply has no association flag" true
    (multiply.association = Source.Unspecified);
  Alcotest.(check string)
    "modulo assignment is source-backed" "%=" modulo_assign.spelling;
  Alcotest.(check string)
    "modulo assignment IC" "IC_MOD_EQU" modulo_assign.ic_name;
  Alcotest.(check int)
    "modulo assignment source line" 324 modulo_assign.source_line

let checks_special_lexer_provenance () =
  let tables = parse_ok (pinned_sources ()) in
  let lookup spelling =
    List.find
      (fun (entry : Source.operator) -> String.equal entry.spelling spelling)
      tables.operators
  in
  let shl_assign = lookup "<<=" in
  let dots = lookup ".." in
  let ellipsis = lookup "..." in
  let dollar = lookup "$$" in
  Alcotest.(check bool)
    "shift assignment origin" true
    (shl_assign.origin = Source.Shift_assignment);
  Alcotest.(check int)
    "shift assignment source line" 1166 shl_assign.source_line;
  Alcotest.(check bool) "dot origin" true (dots.origin = Source.Dot_sequence);
  Alcotest.(check int) "dot source line" 1077 dots.source_line;
  Alcotest.(check int) "ellipsis source line" 1079 ellipsis.source_line;
  Alcotest.(check bool)
    "current position origin" true
    (dollar.origin = Source.Current_position);
  Alcotest.(check (option int))
    "current position has no compound token ID" None dollar.token_id

let rejects_duplicate_token_definition () =
  let sources = pinned_sources () in
  let kernel =
    replace_once ~needle:"#define TK_SUPERSCRIPT\t0x001"
      ~replacement:"#define TK_EOF\t0x001" sources.kernel
  in
  expect_error "TK_EOF appears more than once" { sources with kernel }

let rejects_missing_token_definition () =
  let sources = pinned_sources () in
  let kernel =
    replace_once ~needle:"#define TK_PLUS_PLUS\t0x105" ~replacement:""
      sources.kernel
  in
  expect_error "requires TK_PLUS_PLUS here" { sources with kernel }

let rejects_changed_precedence () =
  let sources = pinned_sources () in
  let compiler =
    replace_once ~needle:"#define PREC_EXP\t\t0x10"
      ~replacement:"#define PREC_EXP\t\t0x11" sources.compiler
  in
  expect_error "PREC_EXP must have value 0x10" { sources with compiler }

let rejects_malformed_dual_assignment () =
  let sources = pinned_sources () in
  let cinit =
    replace_once ~needle:"d['!']=TK_NOT_EQU<<16+'=';"
      ~replacement:"d['!']=TK_NOT_EQU+'=';" sources.cinit
  in
  expect_error "expected a one-byte character" { sources with cinit }

let rejects_reordered_dual_assignments () =
  let sources = pinned_sources () in
  let cinit =
    swap_once "d['!']=TK_NOT_EQU<<16+'=';" "d['&']=TK_AND_AND<<16+'&';"
      sources.cinit
  in
  expect_error "requires group 1 spelling != here" { sources with cinit }

let rejects_unknown_dual_token () =
  let sources = pinned_sources () in
  let cinit =
    replace_once ~needle:"TK_NOT_EQU<<16+'='" ~replacement:"TK_UNKNOWN<<16+'='"
      sources.cinit
  in
  expect_error "requires group 1 spelling != here" { sources with cinit }

let rejects_missing_binary_operator () =
  let sources = pinned_sources () in
  let cinit =
    replace_once ~needle:"d['%']=(PREC_MUL+ASSOCF_LEFT)<<16+IC_MOD;"
      ~replacement:"" sources.cinit
  in
  expect_error "requires % with PREC_MUL and IC_MOD here" { sources with cinit }

let rejects_unknown_binary_ic () =
  let sources = pinned_sources () in
  let cinit =
    replace_once ~needle:"PREC_EXP+ASSOCF_RIGHT)<<16+IC_POWER"
      ~replacement:"PREC_EXP+ASSOCF_RIGHT)<<16+IC_UNKNOWN" sources.cinit
  in
  expect_error "PREC_EXP and IC_POWER" { sources with cinit }

let rejects_missing_lexer_special () =
  let sources = pinned_sources () in
  let lex =
    replace_once ~needle:"cc->token=TK_ELLIPSIS;" ~replacement:"" sources.lex
  in
  expect_error "TK_ELLIPSIS assignment must appear 1 time" { sources with lex }

let rejects_checksum_mismatch () =
  match
    Source.verify_sha256 ~expected:(String.make 64 '0')
      (pinned_sources ()).cinit
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
  let sources = pinned_sources () in
  Alcotest.(check bool) "same parsed tables" true (parse sources = parse sources)

let tests =
  [
    Alcotest.test_case "pinned operator tables" `Quick parses_pinned_tables;
    Alcotest.test_case "dual groups and comments" `Quick
      checks_dual_groups_and_comments;
    Alcotest.test_case "precedence and IC boundaries" `Quick
      checks_precedence_and_ic_boundaries;
    Alcotest.test_case "special lexer provenance" `Quick
      checks_special_lexer_provenance;
    Alcotest.test_case "duplicate token definition" `Quick
      rejects_duplicate_token_definition;
    Alcotest.test_case "missing token definition" `Quick
      rejects_missing_token_definition;
    Alcotest.test_case "changed precedence" `Quick rejects_changed_precedence;
    Alcotest.test_case "malformed dual assignment" `Quick
      rejects_malformed_dual_assignment;
    Alcotest.test_case "reordered dual assignments" `Quick
      rejects_reordered_dual_assignments;
    Alcotest.test_case "unknown dual token" `Quick rejects_unknown_dual_token;
    Alcotest.test_case "missing binary operator" `Quick
      rejects_missing_binary_operator;
    Alcotest.test_case "unknown binary IC" `Quick rejects_unknown_binary_ic;
    Alcotest.test_case "missing lexer special" `Quick
      rejects_missing_lexer_special;
    Alcotest.test_case "checksum mismatch" `Quick rejects_checksum_mismatch;
    Alcotest.test_case "checkout line endings" `Quick
      normalizes_checkout_line_endings;
    Alcotest.test_case "deterministic parse" `Quick deterministic_parse;
  ]
