open Holyc_lib

let lex text =
  let session = Session.create () in
  let source = Session.add_source session ~path:"fixture.hc" ~contents:text in
  match Holyc_lib.lex session ~source with
  | Ok tokens -> (session, tokens)
  | Error diagnostics ->
      List.iter
        (fun diagnostic ->
          Diagnostic_render.human (Session.sources session) diagnostic
          |> prerr_string)
        diagnostics;
      Alcotest.fail "lexing failed"

let without_eof tokens =
  List.filter (fun token -> token.Token.kind <> Token_kind.Eof) tokens

let names tokens =
  without_eof tokens |> List.map (fun token -> Token_kind.name token.Token.kind)

let check_string_list message expected actual =
  Alcotest.(check (list string)) message expected actual

let keyword_table () =
  let text =
    Keyword.all |> List.map (fun (name, _, _) -> name) |> String.concat " "
  in
  let _, tokens = lex text in
  let actual =
    without_eof tokens
    |> List.map (fun token ->
        match token.Token.kind with
        | Token_kind.Keyword keyword -> Keyword.templeos_id keyword
        | _ -> -1)
  in
  Alcotest.(check (list int)) "keyword IDs" (List.init 48 Fun.id) actual

let keyword_provenance () =
  Alcotest.(check int)
    "include source line" 140 (Keyword.source_line Keyword.Include);
  Alcotest.(check int)
    "noargpop source line" 187 (Keyword.source_line Keyword.Noargpop);
  Alcotest.(check (option string))
    "lookup keeps source case" None
    (Keyword.find "Include" |> Option.map Keyword.spelling)

let assembler_directive_table () =
  let actual = Asm_directive.all |> List.map Asm_directive.templeos_id in
  Alcotest.(check (list int))
    "assembler directive IDs" (List.init 25 (fun index -> index + 64)) actual;
  let align = Asm_directive.find "ALIGN" |> Option.get in
  let binfile = Asm_directive.find "BINFILE" |> Option.get in
  Alcotest.(check int) "ALIGN source line" 189 (Asm_directive.source_line align);
  Alcotest.(check int)
    "BINFILE source line" 213 (Asm_directive.source_line binfile);
  Alcotest.(check (option string))
    "lookup keeps source case" None
    (Asm_directive.find "align" |> Option.map Asm_directive.spelling)

let operator_table () =
  let text = Operator.all |> List.map fst |> String.concat " " in
  let _, tokens = lex text in
  let expected = Operator.all |> List.map (fun (_, operator) -> operator) in
  let actual =
    without_eof tokens
    |> List.map (fun token ->
        match token.Token.kind with
        | Token_kind.Operator operator -> operator
        | _ -> Alcotest.fail "expected an operator token")
  in
  Alcotest.(check int)
    "operator count" (List.length expected) (List.length actual);
  List.iter2
    (fun expected actual ->
      Alcotest.(check string)
        "operator spelling"
        (Operator.spelling expected)
        (Operator.spelling actual))
    expected actual

let identifiers_and_bytes () =
  let _, tokens = lex "name @local _x a9 \x80tail" in
  check_string_list "identifier kinds"
    [ "identifier"; "identifier"; "identifier"; "identifier"; "identifier" ]
    (names tokens);
  let values =
    without_eof tokens
    |> List.map (fun token -> Token.value_text token.Token.value |> Option.get)
  in
  check_string_list "identifier bytes"
    [ "name"; "@local"; "_x"; "a9"; "\x80tail" ]
    values

let integer_wrapping () =
  let _, tokens = lex "0xFFFFFFFFFFFFFFFF 18446744073709551615 0b1011" in
  let values =
    without_eof tokens
    |> List.map (fun token ->
        match token.Token.value with
        | Token.Int64 value -> value
        | _ -> Alcotest.fail "expected an integer")
  in
  Alcotest.(check (list int64)) "wrapped values" [ -1L; -1L; 11L ] values

let floating_literals () =
  let _, tokens = lex "1.25e-2 .5 1e+2 1..2" in
  check_string_list "float boundary tokens"
    [
      "float";
      "float";
      "float";
      "punctuation('+')";
      "integer";
      "integer";
      "operator(..)";
      "integer";
    ]
    (names tokens);
  let floats =
    without_eof tokens
    |> List.filter_map (fun token ->
        match token.Token.value with
        | Token.Float64 value -> Some value
        | _ -> None)
  in
  match floats with
  | [ first; second; third ] ->
      Alcotest.(check (float 0.0000001)) "fraction and exponent" 0.0125 first;
      Alcotest.(check (float 0.0000001)) "leading dot" 0.5 second;
      Alcotest.(check (float 0.0000001)) "plus is not exponent syntax" 1.0 third
  | _ -> Alcotest.fail "expected three float values"

let strings_and_characters () =
  let _, tokens = lex "\"a\\n\\x42\\d\" 'ABC' ''" in
  match without_eof tokens with
  | [ string_token; character; empty ] ->
      Alcotest.(check string)
        "decoded string" "a\nB$"
        (match string_token.Token.value with
        | Token.Bytes value -> value
        | _ -> Alcotest.fail "expected a string");
      Alcotest.(check int64)
        "little-endian character" 0x434241L
        (match character.Token.value with
        | Token.Int64 value -> value
        | _ -> Alcotest.fail "expected a character");
      Alcotest.(check int64)
        "empty character" 0L
        (match empty.Token.value with
        | Token.Int64 value -> value
        | _ -> Alcotest.fail "expected an empty character")
  | _ -> Alcotest.fail "unexpected literal token count"

let nested_comments () =
  let _, tokens = lex "/* outer /* inner */ end */ // line\n$dollar$ public" in
  match without_eof tokens with
  | [ token ] ->
      Alcotest.(check string)
        "remaining token" "keyword(public)"
        (Token_kind.name token.Token.kind);
      Alcotest.(check int)
        "retained trivia" 6
        (List.length token.Token.leading_trivia)
  | _ -> Alcotest.fail "unexpected token count after comments"

let trailing_dollar () =
  let _, tokens = lex "$" in
  check_string_list "trailing dollar token" [ "punctuation('$')" ]
    (names tokens)

let error_code text =
  let session = Session.create () in
  let source = Session.add_source session ~path:"bad.hc" ~contents:text in
  match Holyc_lib.lex session ~source with
  | Ok _ -> Alcotest.fail "expected a lexical diagnostic"
  | Error [ diagnostic ] -> diagnostic.Diagnostic.code
  | Error _ -> Alcotest.fail "expected one lexical diagnostic"

let malformed_input () =
  Alcotest.(check string) "block comment" "HCLEX0002" (error_code "/* open");
  Alcotest.(check string) "string" "HCLEX0003" (error_code "\"open");
  Alcotest.(check string) "character" "HCLEX0004" (error_code "'open");
  Alcotest.(check string)
    "long character" "HCLEX0005" (error_code "'123456789'");
  Alcotest.(check string) "NUL" "HCLEX0006" (error_code "a\x00b")

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let golden_dump () =
  let session = Session.create () in
  let source =
    Session.load_source session ~path:"golden/lexer-tour.hc" |> Result.get_ok
  in
  let tokens = Holyc_lib.lex session ~source |> Result.get_ok in
  let actual =
    tokens
    |> List.map (Token.human (Session.sources session))
    |> String.concat "\n"
    |> fun text -> text ^ "\n"
  in
  let expected = read_file "golden/lexer-tour.tokens" in
  Alcotest.(check string) "stable token dump" expected actual

let tests =
  [
    Alcotest.test_case "keyword table" `Quick keyword_table;
    Alcotest.test_case "keyword provenance" `Quick keyword_provenance;
    Alcotest.test_case "assembler directive table" `Quick
      assembler_directive_table;
    Alcotest.test_case "operator table" `Quick operator_table;
    Alcotest.test_case "identifier bytes" `Quick identifiers_and_bytes;
    Alcotest.test_case "integer wrapping" `Quick integer_wrapping;
    Alcotest.test_case "floating literals" `Quick floating_literals;
    Alcotest.test_case "strings and characters" `Quick strings_and_characters;
    Alcotest.test_case "nested comments" `Quick nested_comments;
    Alcotest.test_case "trailing dollar" `Quick trailing_dollar;
    Alcotest.test_case "malformed input" `Quick malformed_input;
    Alcotest.test_case "golden token dump" `Quick golden_dump;
  ]
