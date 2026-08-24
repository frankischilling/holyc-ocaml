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
    "include source line" 140
    (Keyword.source_line Keyword.Include);
  Alcotest.(check int)
    "noargpop source line" 187
    (Keyword.source_line Keyword.Noargpop);
  Alcotest.(check (option string))
    "lookup keeps source case" None
    (Keyword.find "Include" |> Option.map Keyword.spelling)

let assembler_directive_table () =
  let actual = Asm_directive.all |> List.map Asm_directive.templeos_id in
  Alcotest.(check (list int))
    "assembler directive IDs"
    (List.init 25 (fun index -> index + 64))
    actual;
  let align = Asm_directive.find "ALIGN" |> Option.get in
  let binfile = Asm_directive.find "BINFILE" |> Option.get in
  Alcotest.(check int) "ALIGN source line" 189 (Asm_directive.source_line align);
  Alcotest.(check int)
    "BINFILE source line" 213
    (Asm_directive.source_line binfile);
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

let line_continuations () =
  let _, tokens = lex "one\\\ntwo\\\r\nthree\\\rfour" in
  let tokens = without_eof tokens in
  let values =
    List.map
      (fun token -> Token.value_text token.Token.value |> Option.get)
      tokens
  in
  check_string_list "continued identifiers"
    [ "one"; "two"; "three"; "four" ]
    values;
  let continuation_raw =
    List.tl tokens
    |> List.map (fun token ->
        match token.Token.leading_trivia with
        | [ trivia ] ->
            Alcotest.(check string)
              "continuation kind" "line-continuation"
              (Trivia.kind_name trivia.Trivia.kind);
            trivia.Trivia.raw
        | _ -> Alcotest.fail "expected one continuation trivia item")
  in
  check_string_list "continuation spelling"
    [ "\\\n"; "\\\r\n"; "\\\r" ]
    continuation_raw;
  Alcotest.(check string) "lone backslash" "HCLEX0001" (error_code "\\")

let nul_termination () =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"payload.hc" ~contents:"name\x00payload"
  in
  let lexer = Lexer.create ~nul_terminates:true source in
  let first = Lexer.next lexer in
  let eof = Lexer.next lexer in
  (match first with
  | Lexer.Token token ->
      Alcotest.(check string)
        "token before NUL" "identifier"
        (Token_kind.name token.Token.kind)
  | Lexer.Diagnostic _ -> Alcotest.fail "unexpected diagnostic before NUL");
  (match eof with
  | Lexer.Token token ->
      Alcotest.(check bool)
        "NUL emits EOF" true
        (token.Token.kind = Token_kind.Eof)
  | Lexer.Diagnostic _ -> Alcotest.fail "expected EOF at NUL");
  match Lexer.termination lexer with
  | Some (Lexer.Nul_terminated { terminator_offset; trailing_bytes }) ->
      Alcotest.(check int) "terminator offset" 4 terminator_offset;
      Alcotest.(check int) "payload bytes" 7 trailing_bytes
  | Some Lexer.Physical_eof -> Alcotest.fail "expected NUL termination"
  | None -> Alcotest.fail "expected recorded termination"

let add_uint32_le buffer value =
  for shift = 0 to 3 do
    Buffer.add_char buffer (Char.chr ((value lsr (shift * 8)) land 0xff))
  done

let saved_doldoc text records =
  let buffer = Buffer.create (String.length text + 64) in
  Buffer.add_string buffer text;
  Buffer.add_char buffer '\x00';
  List.iter
    (fun (number, payload) ->
      add_uint32_le buffer number;
      add_uint32_le buffer 0;
      add_uint32_le buffer (String.length payload);
      add_uint32_le buffer 1;
      Buffer.add_string buffer payload)
    records;
  Buffer.contents buffer

let lex_saved ?(recover_normalized_doldoc = false) contents =
  let session = Session.create () in
  let source = Session.add_source session ~path:"saved.HC" ~contents in
  let lexer =
    Lexer.create ~mode:Token.Holyc ~nul_terminates:true
      ~recover_normalized_doldoc source
  in
  let rec collect tokens diagnostics =
    match Lexer.next lexer with
    | Lexer.Token token when token.Token.kind = Token_kind.Eof ->
        (session, List.rev tokens, List.rev diagnostics)
    | Lexer.Token token -> collect (token :: tokens) diagnostics
    | Lexer.Diagnostic diagnostic -> collect tokens (diagnostic :: diagnostics)
  in
  collect [] []

let inserted_binary_tokens () =
  let source =
    saved_doldoc "U8 *data[2]={$IB,\"<two>\",BI=2$,$IS,\"<one>\",BI=1$};"
      [ (2, "\x10\x20"); (1, "abc") ]
  in
  let session, tokens, diagnostics = lex_saved source in
  Alcotest.(check int) "valid table diagnostics" 0 (List.length diagnostics);
  let inserted =
    List.filter
      (fun token ->
        token.Token.kind = Token_kind.Inserted_binary
        || token.Token.kind = Token_kind.Inserted_binary_size)
      tokens
  in
  match inserted with
  | [ binary; size ] ->
      Alcotest.(check string)
        "inserted bytes" "\x10\x20"
        (match binary.value with
        | Token.Bytes value -> value
        | _ -> Alcotest.fail "expected an inserted byte value");
      Alcotest.(check int64)
        "inserted size" 3L
        (match size.value with
        | Token.Int64 value -> value
        | _ -> Alcotest.fail "expected an inserted size value");
      let binary_record = Option.get binary.binary_record in
      Alcotest.(check int64) "binary record number" 2L binary_record.number;
      Alcotest.(check bool)
        "binary payload is complete" true binary_record.payload_complete;
      let dump = Token.json (Session.sources session) inserted in
      let open Yojson.Safe.Util in
      let complete =
        Yojson.Safe.from_string dump
        |> to_list |> List.hd |> member "binary_record"
        |> member "payload_complete" |> to_bool
      in
      Alcotest.(check bool) "token JSON records completeness" true complete
  | _ -> Alcotest.fail "expected one IB token and one IS token"

let inserted_binary_failures () =
  let _, _, missing =
    lex_saved (saved_doldoc "$IB,\"missing\",BI=7$;" [ (1, "ok") ])
  in
  Alcotest.(check (list string))
    "missing record diagnostic" [ "HCLEX0010" ]
    (List.map (fun item -> item.Diagnostic.code) missing);
  let _, _, malformed = lex_saved (saved_doldoc "$IB,\"bad\",BI=-1$;" []) in
  Alcotest.(check (list string))
    "malformed BI diagnostic" [ "HCLEX0009" ]
    (List.map (fun item -> item.Diagnostic.code) malformed);
  let duplicate = saved_doldoc "name" [ (1, "a"); (1, "b") ] in
  match Holyc_lib.Doldoc_binary.decode duplicate with
  | Error error ->
      Alcotest.(check bool)
        "duplicate record kind" true
        (error.kind = Holyc_lib.Doldoc_binary.Duplicate_record)
  | Ok _ -> Alcotest.fail "expected a duplicate-record error"

let normalized_binary_recovery () =
  let buffer = Buffer.create 64 in
  Buffer.add_string buffer "$IB,\"short\",BI=1$;";
  Buffer.add_char buffer '\x00';
  add_uint32_le buffer 1;
  add_uint32_le buffer 0;
  add_uint32_le buffer 4;
  add_uint32_le buffer 1;
  Buffer.add_string buffer "abc";
  let source = Buffer.contents buffer in
  (match Holyc_lib.Doldoc_binary.decode source with
  | Error error ->
      Alcotest.(check bool)
        "strict truncated-payload kind" true
        (error.kind = Holyc_lib.Doldoc_binary.Truncated_payload)
  | Ok _ -> Alcotest.fail "strict decoding accepted a shortened payload");
  let _, tokens, diagnostics =
    lex_saved ~recover_normalized_doldoc:true source
  in
  Alcotest.(check int) "recovery diagnostics" 0 (List.length diagnostics);
  let token = List.hd tokens in
  let record = Option.get token.Token.binary_record in
  Alcotest.(check int64) "declared size" 4L record.declared_size;
  Alcotest.(check bool) "payload is incomplete" false record.payload_complete;
  Alcotest.(check string)
    "archived bytes are retained" "abc"
    (match token.value with
    | Token.Bytes value -> value
    | _ -> Alcotest.fail "expected recovered bytes")

let inserted_command_detection_is_lexical () =
  let _, tokens, diagnostics = lex_saved "\"$IB,\";\x00not-a-binary-table" in
  Alcotest.(check (list string))
    "string content does not decode a table" []
    (List.map (fun item -> item.Diagnostic.code) diagnostics);
  Alcotest.(check (list string))
    "ordinary token kinds"
    [ "string"; "punctuation(';')" ]
    (List.map (fun token -> Token_kind.name token.Token.kind) tokens)

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
    Alcotest.test_case "line continuations" `Quick line_continuations;
    Alcotest.test_case "NUL termination" `Quick nul_termination;
    Alcotest.test_case "DolDoc inserted binary tokens" `Quick
      inserted_binary_tokens;
    Alcotest.test_case "DolDoc inserted binary failures" `Quick
      inserted_binary_failures;
    Alcotest.test_case "normalized DolDoc binary recovery" `Quick
      normalized_binary_recovery;
    Alcotest.test_case "DolDoc command detection is lexical" `Quick
      inserted_command_detection_is_lexical;
    Alcotest.test_case "golden token dump" `Quick golden_dump;
  ]
