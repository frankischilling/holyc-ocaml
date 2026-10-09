open Holyc_lib

let checked = Test_declaration_collection.checked

let warnings diagnostics =
  List.filter (fun d -> d.Diagnostic.code = "HCSEMA0077") diagnostics

let fixture ?(enabled = true) text =
  let session = Session.create () in
  let counts = ref [] in
  let checkpoint = function
    | Parser.Sequence_started context ->
        ignore
          (Parser.context_set_option context ~bit_index:17L enabled |> checked);
        Ok ()
    | Parser.Command_completed command ->
        counts :=
          (Parser.context_warning_count command.command_start.command_context
          |> checked)
          :: !counts;
        Ok ()
    | _ -> Ok ()
  in
  let commands : Parser.command_sink =
    {
      lexical_lookup = None;
      checkpoint = Some checkpoint;
      reference = None;
      call = None;
      implicit_output = None;
      query = None;
      declaration = None;
      dimension_count = None;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let source =
    Session.add_source session ~path:"parenthesis-warnings.hc" ~contents:text
  in
  let output =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session)
      ~config:(Preprocessor.Config.create () |> checked)
      source
  in
  (output, List.rev !counts)

let expression ?enabled text expected =
  let output, _ =
    fixture ?enabled ("I64 F(I64 a,I64 b,I64 c){return " ^ text ^ ";}")
  in
  if Parser.has_errors output then
    Alcotest.fail
      (String.concat "; "
         (List.map (fun d -> d.Diagnostic.message) output.diagnostics));
  Alcotest.(check int) text expected (List.length (warnings output.diagnostics))

let disabled () =
  expression ~enabled:false "((a+b))" 0;
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"default-parentheses.hc"
      ~contents:"I64 F(){return (42);}"
  in
  let output =
    Parser.parse ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session)
      ~config:(Preprocessor.Config.create () |> checked)
      source
  in
  Alcotest.(check int)
    "root default remains off" 0
    (List.length (warnings output.diagnostics))

let terms_and_unary () =
  List.iter
    (fun (text, count) -> expression text count)
    [
      ("(a)", 1);
      ("((a))", 2);
      ("-(a)", 1);
      ("+(a)", 1);
      ("!(a)", 1);
      ("~(a)", 1);
      ("(a+b)", 1);
      ("-(a+b)", 0);
      ("!(a&&b)", 0);
      ("(-a)", 1);
      ("-(-a)", 1);
      ("(sizeof(I64))", 1);
      ("sizeof((I64))", 0);
      ("defined((a))", 0);
    ];
  let output, counts = fixture "(40+2);(42);" in
  Alcotest.(check (list int64))
    "actual input count increments at each reached phase" [ 1L; 2L ] counts;
  Alcotest.(check int)
    "one warning per original group" 2
    (List.length (warnings output.diagnostics))

let modifiers () =
  List.iter
    (fun (text, count) -> expression text count)
    [
      ("(a)(I64)", 0);
      ("-(a)(I64)", 0);
      ("(a)++", 0);
      ("++(a)", 1);
      ("(a+b)(I64)", 0);
      ("((a+b))(I64)", 1);
    ];
  let output, _ = fixture "I64 F(I64 *p){return (p)[(0)];}" in
  Alcotest.(check int)
    "index owns a distinct original expression phase" 1
    (List.length (warnings output.diagnostics))

let binary_precedence () =
  List.iter
    (fun (text, count) -> expression text count)
    [
      ("(a+b)*c", 0);
      ("(a*b)+c", 1);
      ("a*(b+c)", 0);
      ("a+(b*c)", 1);
      ("a+(b+c)", 1);
      ("a-(b+c)", 0);
      ("a+(b-c)", 0);
      ("(a-b)+c", 1);
      ("a/(b*c)", 0);
      ("a*(b/c)", 0);
      ("(a`b)`c", 0);
      ("a`(b`c)", 1);
      ("a<<(b<<c)", 0);
      ("(a<<b)<<c", 1);
      ("a=(b=c)", 1);
      ("(a=b)=c", 0);
    ]

let operator_families () =
  (* Explicit predictions from CInit.HC:288-329 and the four PrsExp sites.
     In each row, inner parentheses are necessary on the stronger side. *)
  List.iter
    (fun (text, count) -> expression text count)
    [
      ("(a|b)+c", 1);
      ("a|(b+c)", 0);
      ("(a&b)^c", 1);
      ("a&(b^c)", 0);
      ("(a==b)&&c", 1);
      ("a==(b&&c)", 0);
      ("(a&&b)||c", 1);
      ("a&&(b||c)", 0);
      ("(a^^b)||c", 1);
      ("a^^(b||c)", 0);
      ("(a>=b)==c", 1);
      ("a>=(b==c)", 0);
      ("a+=(b=c)", 1);
      ("a<<=(b+=c)", 1);
      ("a=(b&=c)", 1);
      ("a=(b|=c)", 1);
      ("a=(b^=c)", 1);
      ("a=(b*=c)", 1);
      ("a=(b/=c)", 1);
      ("a=(b%=c)", 1);
      ("a=(b>>=c)", 1);
      ("a=(b-=c)", 1);
    ]

let original_lookahead () =
  let text = "I64 F(I64 a,I64 b,I64 c){return (a*b)+c;}" in
  let output, _ = fixture text in
  let warning = List.hd (warnings output.diagnostics) in
  Alcotest.(check int)
    "binary warning follows operator lookahead"
    (String.index text '+' + 1)
    warning.primary.start;
  let text = "I64 F(){return ((42));}" in
  let output, _ = fixture text in
  let reached =
    warnings output.diagnostics
    |> List.map (fun d ->
        String.sub text d.Diagnostic.primary.start
          (d.primary.stop - d.primary.start))
  in
  Alcotest.(check (list string))
    "nested phases preserve delimiter order" [ ")"; ";" ] reached

let definition_start () =
  let output, _ = fixture "#define VALUE (40+2)\nI64 F(){return VALUE;}" in
  Alcotest.(check int)
    "definition-start group stays suppressed after frame exit" 0
    (List.length (warnings output.diagnostics));
  let output, _ = fixture "#define VALUE 42\nI64 F(){return (VALUE);}" in
  Alcotest.(check int)
    "numeric lookahead exits exhausted definition" 1
    (List.length (warnings output.diagnostics));
  let output, _ = fixture "#define VALUE 42 \nI64 F(){return (VALUE);}" in
  Alcotest.(check int)
    "first operand lookahead still in definition suppresses group" 0
    (List.length (warnings output.diagnostics));
  let output, _ = fixture "#define OPEN (\nI64 F(){return OPEN 42);}" in
  Alcotest.(check int)
    "opening-token origin does not replace first-operand input" 1
    (List.length (warnings output.diagnostics))

let definition_lookahead () =
  let output, _ = fixture "#define END ;\nI64 F(){return (42) END}" in
  Alcotest.(check int)
    "warning lookahead in replacement suppresses warning" 0
    (List.length (warnings output.diagnostics));
  let output, _ =
    fixture "#define RHS c \nI64 F(I64 a,I64 b,I64 c){return (a*b)+RHS;}"
  in
  Alcotest.(check int)
    "binary lookahead in replacement suppresses warning" 0
    (List.length (warnings output.diagnostics));
  let output, _ =
    fixture "#define RHS c\nI64 F(I64 a,I64 b,I64 c){return (a*b)+RHS;}"
  in
  Alcotest.(check int)
    "identifier lookahead exits exhausted definition" 1
    (List.length (warnings output.diagnostics));
  let output, _ =
    fixture "#define PLUS +\nI64 F(I64 a,I64 b,I64 c){return (a*b)PLUS c;}"
  in
  Alcotest.(check int)
    "binary warning checks input after operator consumption" 1
    (List.length (warnings output.diagnostics))

let lexer_input_lookahead () =
  let cases =
    [
      ("42", false);
      ("42 ", true);
      ("a", false);
      ("a ", true);
      ("(", true);
      (")", true);
      ("+", false);
      ("=", false);
      ("==", true);
      ("<<", false);
      ("<<=", true);
      ("..", false);
      ("...", true);
    ]
  in
  List.iter
    (fun (text, remains) ->
      let session = Session.create () in
      let outer =
        Session.add_source session ~path:"outer-input.hc" ~contents:" ;"
      in
      let inner =
        Session.add_source session ~path:"inner-input.hc" ~contents:text
      in
      let caller = Lexer.create ~mode:Token.Holyc outer in
      let lexer = Lexer.create ~mode:Token.Holyc ~caller inner in
      let token =
        match Lexer.next lexer with
        | Lexer.Token token -> token
        | Lexer.Diagnostic diagnostic -> Alcotest.fail diagnostic.message
      in
      Alcotest.(check bool)
        "token keeps its original first input" true
        (Source_id.equal token.span.source (Source_file.id inner));
      Alcotest.(check bool)
        text remains
        (Source_id.equal (Lexer.input_source lexer) (Source_file.id inner)))
    cases

let reached_failure () =
  let output, _ = fixture "I64 F(){return (40+2)+;}" in
  Alcotest.(check bool)
    "later missing operand fails" true (Parser.has_errors output);
  Alcotest.(check int)
    "warning precedes right operand failure" 1
    (List.length (warnings output.diagnostics));
  let output, _ = fixture "I64 F(){return (40+2;}" in
  Alcotest.(check int)
    "missing close never reaches group warning" 0
    (List.length (warnings output.diagnostics));
  let output, _ = fixture "I64 F(){return (42)(I64;}" in
  Alcotest.(check int)
    "unfinished modifier never reaches term warning" 0
    (List.length (warnings output.diagnostics))

let run mode text =
  let session = Session.create () in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> checked
  in
  let source =
    Session.add_source session ~path:"source-parentheses.hc" ~contents:text
  in
  let report =
    run_integer_program_report ~max_steps:10000 session ~config ~source
  in
  match integer_program_report_outcome report with
  | Ok result -> (true, result.diagnostics)
  | Error diagnostics -> (false, diagnostics)

let live_options_and_children () =
  let cases =
    [
      ("#exe {Option(16,0);Option(17,1);}I64 F(){return (42);}F();", 1, true);
      ( "#exe {Option(16,0);Option(17,0);}I64 F(){return (40*1)+ #exe \
         {Option(17,1);} 2;}F();",
        1,
        true );
      ( "#exe {Option(16,0);Option(17,1);}I64 F(){return (42) #exe \
         {Option(17,0);} ;}F();",
        0,
        true );
      ( {|#exe {Option(16,0);Option(17,1);StreamExePrint("Option(17,0);(40+2);");(42);}42;|},
        1,
        true );
      ("#exe {Option(16,0);Option(17,1);(40+2)+;}42;", 1, false);
      ( "#exe {Option(16,0);Option(17,1);I64 F(){return (42);}I64 \
         zero=0;42/zero;}42;",
        1,
        false );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (text, count, success) ->
          let actual_success, diagnostics = run mode text in
          Alcotest.(check bool)
            "source completion or reached failure" success actual_success;
          Alcotest.(check int) text count (List.length (warnings diagnostics)))
        cases)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let tests =
  [
    Alcotest.test_case "default and disabled option" `Quick disabled;
    Alcotest.test_case "original terms and unary prefixes" `Quick
      terms_and_unary;
    Alcotest.test_case "postfix modifiers and independent index phase" `Quick
      modifiers;
    Alcotest.test_case "binary precedence and association" `Quick
      binary_precedence;
    Alcotest.test_case "all binary operator families" `Quick operator_families;
    Alcotest.test_case "warning retains original phase lookahead" `Quick
      original_lookahead;
    Alcotest.test_case "original definition input at group entry" `Quick
      definition_start;
    Alcotest.test_case "original definition input at warning lookahead" `Quick
      definition_lookahead;
    Alcotest.test_case "character lookahead retains actual original lexer input"
      `Quick lexer_input_lookahead;
    Alcotest.test_case "warnings survive later parser failures" `Quick
      reached_failure;
    Alcotest.test_case "live source options and ordinary child isolation" `Quick
      live_options_and_children;
  ]
