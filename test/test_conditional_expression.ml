open Holyc_lib

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let config ?max_expression_nodes () =
  Preprocessor.Config.create ~working_directory:(Sys.getcwd ())
    ?max_expression_nodes ()
  |> checked

let run ?max_expression_nodes ?(prepare = fun _ -> ()) contents =
  let session = Session.create () in
  prepare session;
  let source = Session.add_source session ~path:"conditional.HC" ~contents in
  let result =
    Holyc_lib.preprocess session
      ~config:(config ?max_expression_nodes ())
      ~source
  in
  (session, result)

let without_eof tokens =
  List.filter (fun token -> token.Token.kind <> Token_kind.Eof) tokens

let words tokens =
  without_eof tokens
  |> List.map (fun token ->
      match token.Token.value with
      | Token.Text value -> value
      | _ -> token.Token.raw)

let expect_words expected (_, result) =
  Alcotest.(check (list string))
    "selected tokens" expected
    (result |> Result.get_ok |> words)

let error_with_code expected (_, result) =
  match result with
  | Ok _ -> Alcotest.failf "expected diagnostic %s" expected
  | Error diagnostics -> (
      match
        List.find_opt
          (fun item -> String.equal item.Diagnostic.code expected)
          diagnostics
      with
      | Some item -> item
      | None ->
          Alcotest.failf "expected diagnostic %s, got %s" expected
            (diagnostics
            |> List.map (fun item -> item.Diagnostic.code)
            |> String.concat ", "))

let literal_branches_and_lookahead () =
  run
    "#if 1 true_branch #else wrong #endif tail #if 0 wrong_two #else \
     false_branch #endif"
  |> expect_words [ "true_branch"; "tail"; "false_branch" ];
  run "#if 0#else adjacent #endif #if 1#endif tail"
  |> expect_words [ "adjacent"; "tail" ]

let definition_expansion () =
  run
    "#define TRUE 1\n\
     #define FALSE 0\n\
     #define CHAIN TRUE\n\
     #if CHAIN selected #else wrong #endif #if FALSE wrong_two #else \
     false_selected #endif"
  |> expect_words [ "selected"; "false_selected" ]

let literal_types () =
  run
    "#if 'A'==65 character #endif #if 'AB'==0x4241 multichar #endif #if \
     1.5*2==3 floating #endif #if -0.0 negative_zero #else wrong #endif #if \
     0xFFFFFFFFFFFFFFFF+0.0==-1.0 signed_float_promotion #endif #if 0.0==-0.0 \
     wrong_two #else signed_zero_equality #endif #if 0.0!=-0.0 \
     signed_zero_inequality #endif #if (1.0&1.0)==1.0 floating_and #endif #if \
     1.0^1.0 wrong_three #else floating_xor #endif #if (1.0|-0.0)==-1.0 \
     floating_or #endif"
  |> expect_words
       [
         "character";
         "multichar";
         "floating";
         "negative_zero";
         "signed_float_promotion";
         "signed_zero_equality";
         "signed_zero_inequality";
         "floating_and";
         "floating_xor";
         "floating_or";
       ]

let unary_operations () =
  run
    "#if ~0==-1 complement #endif #if !0 logical_not #endif #if -5+6 negation \
     #endif #if +4==4 identity #endif #if (~0xFFFFFFFFFFFFFFFF+-2)<0 \
     complement_type #endif #if ~1.0<0 floating_complement #endif #if +2`2==4 \
     identity_power #endif #if !0.0==1.0 wrong #else floating_not_bits #endif"
  |> expect_words
       [
         "complement";
         "logical_not";
         "negation";
         "identity";
         "complement_type";
         "floating_complement";
         "identity_power";
         "floating_not_bits";
       ]

let wrapping_and_signedness () =
  run
    "#if 0xFFFFFFFFFFFFFFFF+1==0 wrapped #endif #if 0xFFFFFFFFFFFFFFFF>0 \
     unsigned_compare #endif #if -1<0 signed_compare #endif #if 17/5==3 \
     division #endif #if 17%5==2 modulo #endif"
  |> expect_words
       [ "wrapped"; "unsigned_compare"; "signed_compare"; "division"; "modulo" ]

let shifts_and_power () =
  run
    "#if 1<<64==1 masked_shift #endif #if 0x8000000000000000>>63==1 \
     logical_shift #endif #if -8>>2==-2 arithmetic_shift #endif #if 2`3`2==512 \
     right_power #endif #if -2`2==-4 unary_power #endif #if (-2)`2==4 \
     grouped_power #endif #if 2<<1`2==2 mixed_right #endif #if 2`3<<1==8 \
     mixed_left #endif #if (0x8000000000000000<<0)<0 shift_result_type #endif \
     #if 8>>1>>1==2 shift_left_association #endif"
  |> expect_words
       [
         "masked_shift";
         "logical_shift";
         "arithmetic_shift";
         "right_power";
         "unary_power";
         "grouped_power";
         "mixed_right";
         "mixed_left";
         "shift_result_type";
         "shift_left_association";
       ]

let source_precedence () =
  run
    "#if 2*3`2==18 power_before_multiply #endif #if 20/5*2==8 \
     multiplicative_left #endif #if 2&3*4==0 multiply_before_and #endif #if \
     1|2^3&1==3 bitwise_order #endif #if 1+1|1==2 bitwise_before_add #endif \
     #if 10-3+1==8 additive_left #endif #if 3+1<4 wrong_add #else \
     add_before_relational #endif #if 0==1&&0 wrong #else \
     equality_before_logical_and #endif #if 1^^1&&0 logical_and_before_xor \
     #endif #if 1||1^^1 logical_xor_before_or #endif"
  |> expect_words
       [
         "power_before_multiply";
         "multiplicative_left";
         "multiply_before_and";
         "bitwise_order";
         "bitwise_before_add";
         "additive_left";
         "add_before_relational";
         "equality_before_logical_and";
         "logical_and_before_xor";
         "logical_xor_before_or";
       ]

let comparison_chains () =
  run
    "#if 1<2<3 ascending #endif #if 3>2>1 descending #endif #if 1==1==1 \
     equality_chain #endif #if 1<2==2 mixed_chain #endif #if 0==1<2 wrong \
     #else tighter_rhs #endif"
  |> expect_words
       [
         "ascending";
         "descending";
         "equality_chain";
         "mixed_chain";
         "tighter_rhs";
       ]

let logical_operations () =
  run
    "#if 1&&2 logical_and #endif #if 0||3 logical_or #endif #if 1^^0 \
     logical_xor #endif #if 1^^2 wrong #else equal_truth #endif"
  |> expect_words [ "logical_and"; "logical_or"; "logical_xor"; "equal_truth" ];
  ignore (run "#if 0&&1/0 wrong #endif" |> error_with_code "HCPP0025")

let add_symbol session name kind =
  ignore
    (Symbol_visibility.Environment.add (Session.symbols session) ~name ~kind ())

let defined_terms () =
  let prepare session =
    add_symbol session "Known" Symbol_visibility.Function;
    add_symbol session "ImportOnly" Symbol_visibility.Import_system_symbol;
    add_symbol session "Shadowed" Symbol_visibility.Global_variable;
    let context =
      Symbol_visibility.Environment.begin_local_context
        (Session.symbols session)
    in
    checked
      (Symbol_visibility.Environment.add_local (Session.symbols session) context
         ~name:"LocalOnly");
    checked
      (Symbol_visibility.Environment.add_local (Session.symbols session) context
         ~name:"Shadowed")
  in
  run ~prepare
    "#define Alias Known\n\
     #define Number 1\n\
     #if defined(Known) known #endif #if defined LocalOnly local #endif #if \
     defined(Shadowed) shadowed #endif #if defined(Missing) wrong #else \
     missing #endif #if defined(ImportOnly) wrong_two #else import_absent \
     #endif #if defined(Alias) expanded_symbol #endif #if defined(Number) \
     wrong_three #else expanded_literal #endif #if defined(if) wrong_four \
     #else keyword_false #endif #if defined(((Known))) nested_defined #endif"
  |> expect_words
       [
         "known";
         "local";
         "shadowed";
         "missing";
         "import_absent";
         "expanded_symbol";
         "expanded_literal";
         "keyword_false";
         "nested_defined";
       ]

let mixed_conditionals () =
  let prepare session =
    add_symbol session "Present" Symbol_visibility.Function
  in
  run ~prepare
    "#if 1 outer #ifjit jit #endif #ifdef Present symbol #endif #ifaot wrong \
     #endif #endif tail"
  |> expect_words [ "outer"; "jit"; "symbol"; "tail" ]

let malformed_expressions_are_inert () =
  let session, missing = run "#if #else #define HIDDEN 1 #endif tail" in
  ignore (error_with_code "HCPP0021" (session, missing));
  Alcotest.(check bool)
    "invalid branches stay inert" true
    (Definition.Environment.find (Session.definitions session) "HIDDEN"
    |> Option.is_none);
  ignore
    (run "#if mp_cnt>1 wrong #else wrong_two #endif"
    |> error_with_code "HCPP0022");
  ignore (run "#if 1=2 wrong #endif" |> error_with_code "HCPP0022");
  ignore (run "#if (1+2 wrong #endif" |> error_with_code "HCPP0023");
  ignore (run "#if defined((Known) wrong #endif" |> error_with_code "HCPP0023");
  ignore (run "#if 1/0 wrong #endif" |> error_with_code "HCPP0025");
  ignore
    (run "#if (-0x7FFFFFFFFFFFFFFF-1)/-1 wrong #endif"
    |> error_with_code "HCPP0027");
  ignore
    (run ~max_expression_nodes:1 "#if 1+1 wrong #endif"
    |> error_with_code "HCPP0026")

let configuration_limit () =
  let selected = config ~max_expression_nodes:19 () in
  Alcotest.(check int)
    "selected node limit" 19
    (Preprocessor.Config.max_expression_nodes selected);
  match Preprocessor.Config.create ~max_expression_nodes:(-1) () with
  | Ok _ -> Alcotest.fail "accepted a negative expression node limit"
  | Error message ->
      Alcotest.(check string)
        "configuration diagnostic"
        "conditional expression node limit must be nonnegative" message

let deterministic_output_and_diagnostics () =
  let render_tokens () =
    let session, result = run "#if 2+2==4 selected #endif" in
    Token.json (Session.sources session) (Result.get_ok result)
  in
  Alcotest.(check string) "token output" (render_tokens ()) (render_tokens ());
  let render_error () =
    let session, result = run "#if RuntimeValue wrong #endif" in
    match result with
    | Ok _ -> Alcotest.fail "expected a runtime-value diagnostic"
    | Error diagnostics ->
        let human =
          diagnostics
          |> List.map (Diagnostic_render.human (Session.sources session))
          |> String.concat ""
        in
        (human, Diagnostic_render.json (Session.sources session) diagnostics)
  in
  let human, json = render_error () in
  Alcotest.(check (pair string string))
    "diagnostic output" (human, json) (render_error ());
  Alcotest.(check bool)
    "human diagnostic" true
    (String.starts_with
       ~prefix:
         "conditional.HC:1:5: error[HCPP0022]: identifier \"RuntimeValue\" \
          requires compile-time execution in #if\n"
       human);
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "diagnostic code" "HCPP0022"
    (json |> Yojson.Safe.from_string |> index 0 |> member "code" |> to_string)

let unsupported_postfix () =
  ignore (run "#if 1() wrong #endif" |> error_with_code "HCPP0022")

let tests =
  [
    Alcotest.test_case "literal branches and lookahead" `Quick
      literal_branches_and_lookahead;
    Alcotest.test_case "definition expansion" `Quick definition_expansion;
    Alcotest.test_case "literal types" `Quick literal_types;
    Alcotest.test_case "unary operations" `Quick unary_operations;
    Alcotest.test_case "wrapping and signedness" `Quick wrapping_and_signedness;
    Alcotest.test_case "shifts and power" `Quick shifts_and_power;
    Alcotest.test_case "source precedence" `Quick source_precedence;
    Alcotest.test_case "comparison chains" `Quick comparison_chains;
    Alcotest.test_case "logical operations" `Quick logical_operations;
    Alcotest.test_case "defined terms" `Quick defined_terms;
    Alcotest.test_case "mixed conditionals" `Quick mixed_conditionals;
    Alcotest.test_case "malformed expressions are inert" `Quick
      malformed_expressions_are_inert;
    Alcotest.test_case "configuration limit" `Quick configuration_limit;
    Alcotest.test_case "deterministic output and diagnostics" `Quick
      deterministic_output_and_diagnostics;
    Alcotest.test_case "unsupported postfix" `Quick unsupported_postfix;
  ]
