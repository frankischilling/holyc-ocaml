open Holyc_lib
module D = Task_declarations

let expect = Test_integer_program.checked

let parse ?(observe = fun _ -> true) ?(on_default = fun _ _ _ -> Ok ())
    ?execute_stream text =
  let session, source, ledger = Test_source_promotion.inputs text in
  let receipts = ref [] in
  let declaration event =
    let observed = if observe event then D.observe ledger event else Ok () in
    Result.bind observed (fun () ->
        match event with
        | Parser.Parameter_default_completed receipt ->
            receipts := receipt :: !receipts;
            on_default ledger receipt event
        | _ -> Ok ())
  in
  let parsed =
    Test_source_promotion.parse ~declaration ?execute_stream session source
      ledger
  in
  (parsed, ledger, List.rev !receipts)

let exact_sources () =
  let parsed, _, receipts =
    parse {|I64 F(I64 a,I64 b=1+2;I64 c=4){return b+c;};|}
  in
  let ast = Test_parser.expect_ast parsed in
  let parameters =
    match ast.items with
    | Ast.Function_definition definition :: _ -> definition.parameters
    | _ -> Alcotest.fail "expected original named function"
  in
  Alcotest.(check (list int))
    "default positions preserve ordinary parameters" [ 1; 2 ]
    (List.map (fun receipt -> receipt.Parser.default_parameter_index) receipts);
  List.iter
    (fun receipt ->
      let parameter =
        List.nth parameters receipt.Parser.default_parameter_index
      in
      Alcotest.(check bool)
        "exact original default AST" true
        (Option.get parameter.default == receipt.default_ast);
      Alcotest.(check bool)
        "exact original parameter type" true
        (parameter.type_specifier == receipt.default_type_specifier);
      Alcotest.(check bool)
        "completed callbacks are no longer current" false
        (Parser.parameter_default_is_current receipt))
    receipts;
  Alcotest.(check bool)
    "defaults retain original predecessor" true
    (Option.get (List.nth receipts 1).default_predecessor == List.hd receipts)

let stops_before_next_parameter () =
  let entered = ref 0 in
  let execute_stream span =
    incr entered;
    Error
      [
        Diagnostic.make ~code:"TESTENTER" ~severity:Diagnostic.Error
          ~primary:span ~message:"unexpected directive" ();
      ]
  in
  let on_default _ receipt _ =
    Alcotest.(check bool)
      "default preparation owns its synchronous callback" true
      (Parser.parameter_default_is_current receipt);
    Error
      [
        Diagnostic.make ~code:"TESTDEFAULT" ~severity:Diagnostic.Error
          ~primary:receipt.default_ast.location.span
          ~message:"default preparation stopped" ();
      ]
  in
  let parsed, _, receipts =
    parse ~execute_stream ~on_default {|I64 F(I64 a=1,#exe {}I64 b=2);|}
  in
  Alcotest.(check bool)
    "default failure stops parsing" true (Parser.has_errors parsed);
  Alcotest.(check int)
    "only original first default reached" 1 (List.length receipts);
  Alcotest.(check int) "following directive did not execute" 0 !entered

let replay () =
  let saved = ref [] in
  let on_default ledger _ event =
    saved := event :: !saved;
    Alcotest.(check bool)
      "same live default cannot be observed twice" true
      (Result.is_error (D.observe ledger event));
    Ok ()
  in
  let parsed, ledger, _ = parse ~on_default {|I64 F(I64 a=21){return a;};|} in
  ignore (Test_parser.expect_ast parsed);
  List.iter
    (fun event ->
      Alcotest.(check bool)
        "stale default observation rejected" true
        (Result.is_error (D.observe ledger event)))
    !saved

let missing () =
  List.iter
    (fun index ->
      let observe = function
        | Parser.Parameter_default_completed receipt ->
            receipt.default_parameter_index <> index
        | _ -> true
      in
      let parsed, _, _ =
        parse ~observe {|I64 F(I64 a=1,I64 b=2){return a+b;};|}
      in
      Alcotest.(check bool)
        "missing default cannot join completed header" true
        (Parser.has_errors parsed))
    [ 0; 1 ]

let retained_lastclass () =
  let parsed, _, receipts = parse {|extern U0 F(I64 x,U8 *name=lastclass);|} in
  ignore (Test_parser.expect_ast parsed);
  Alcotest.(check bool)
    "lastclass keeps its distinct original source" true
    (match (List.hd receipts).default_ast.value with
    | Ast.Lastclass_default _ -> true
    | _ -> false)

let typed_defaults text expected_calls expected_queries () =
  let reached = ref [] in
  let on_observed task = function
    | Parser.Parameter_default_completed receipt ->
        let table = Session.semantic_symbols (Integer_task.frontend task) in
        let counts () =
          ( List.length (Semantic_symbol_table.all_symbols table),
            List.length (Semantic_symbol_table.all_scopes table) )
        in
        let before = Integer_task.progress task and symbols = counts () in
        let typed =
          Integer_task.prepare_parameter_default task receipt |> expect
        in
        let root = Test_initializer_fragment_typing.root typed in
        let fragment =
          match
            Semantic_top_level_expression_tree.root_role
              (Semantic_function_call_expression_result.top_level_root_source
                 root)
          with
          | Semantic_top_level_expression_tree.Default_fragment fragment ->
              fragment
          | _ -> Alcotest.fail "default needs its own original expression role"
        in
        Alcotest.(check bool)
          "typed default retains original parser receipt" true
          (Semantic_default_fragment.receipt fragment == receipt);
        Alcotest.(check bool)
          "typing adds no effects or work" true
          (before = Integer_task.progress task);
        Alcotest.(check (pair int int))
          "typing adds no symbols or scopes" symbols (counts ());
        reached :=
          ( List.length
              (Semantic_function_call_expression_result.top_level_direct_calls
                 typed),
            List.length (Semantic_default_fragment.queries fragment) )
          :: !reached
    | _ -> ()
  in
  let parsed, _ =
    Test_live_initializer_execution.run_result ~on_observed text
  in
  ignore (Test_parser.expect_ast parsed);
  Alcotest.(check (list (pair int int)))
    "shared default call and query typing"
    [ (expected_calls, expected_queries) ]
    !reached

let exact_environment () =
  let on_observed task = function
    | Parser.Parameter_default_completed receipt ->
        let table = Session.semantic_symbols (Integer_task.frontend task) in
        let typed =
          Integer_task.prepare_parameter_default task receipt |> expect
        in
        let tree =
          Semantic_function_call_expression_result.top_level_source typed
        in
        let outer = Semantic_top_level_expression_tree.source tree in
        let root = Test_initializer_fragment_typing.root typed in
        let fragment =
          match
            Semantic_top_level_expression_tree.root_role
              (Semantic_function_call_expression_result.top_level_root_source
                 root)
          with
          | Semantic_top_level_expression_tree.Default_fragment fragment ->
              fragment
          | _ -> Alcotest.fail "expected default fragment"
        in
        let environment = Semantic_default_fragment.environment fragment in
        let foreign =
          Semantic_outer_environment.create ~table ~compilation_mode:Jit
            (Semantic_outer_environment.tables environment)
          |> Result.get_ok
        in
        Alcotest.(check bool)
          "same-table replacement snapshot rejected" true
          (Result.is_error
             (Semantic_top_level_outer_expression_binding.resolve ~table
                ~environment:foreign
                ~expressions:
                  (Semantic_top_level_outer_expression_binding.source outer)))
    | _ -> ()
  in
  let parsed, _ =
    Test_live_initializer_execution.run_result ~on_observed {|I64 F(I64 a=42);|}
  in
  ignore (Test_parser.expect_ast parsed)

let tests =
  [
    Alcotest.test_case "constant defaults require the exact outer snapshot"
      `Quick exact_environment;
    Alcotest.test_case "default types retained arithmetic without effects"
      `Quick
      (typed_defaults {|I64 N=40;I64 F(I64 a=++N+2){return a;};N;|} 0 0);
    Alcotest.test_case "default types nested retained calls" `Quick
      (typed_defaults
         {|I64 Seed(I64 n){return n;};I64 F(I64 a=Seed(21)+Seed(21)){return a;};|}
         2 0);
    Alcotest.test_case "default types original query transcript" `Quick
      (typed_defaults
         {|I64 A[2]={1,2};I64 F(I64 a=sizeof(A)+defined(A)){return a;};|} 0 2);
    Alcotest.test_case "exact default source and parameter positions" `Quick
      exact_sources;
    Alcotest.test_case "default boundary precedes next parameter directive"
      `Quick stops_before_next_parameter;
    Alcotest.test_case "live and delayed default replay rejection" `Quick replay;
    Alcotest.test_case "completed headers require all original defaults" `Quick
      missing;
    Alcotest.test_case "lastclass retains distinct original source" `Quick
      retained_lastclass;
  ]
