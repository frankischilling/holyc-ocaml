open Holyc_lib
module D = Task_declarations
module T = Test_task_declarations
module VM = Ir_integer_interpreter

let compile session runtime ledger ast =
  let declaration_command = D.seal ledger ast |> T.expect in
  (compile_integer_task_ast ~task:runtime ~declaration_command session
     ~config:(T.config ()) ast
  |> T.expect)
    .value

let parse ?on_event session ledger text =
  let commands = ref [] in
  let output, _ =
    T.parse
      ~checkpoint:(fun event ->
        Result.bind (D.observe_command ledger event) (fun () ->
            (match event with
            | Parser.Command_completed receipt ->
                commands := receipt.command_ast :: !commands
            | _ -> ());
            Option.fold ~none:(Ok ()) ~some:(fun f -> f event) on_event))
      session ledger text
  in
  (Test_parser.expect_ast output, List.rev !commands)

let rejected runtime program =
  let steps = VM.task_executed_steps runtime in
  let preparation = VM.task_initializer_steps runtime in
  let output = VM.task_output_bytes runtime in
  let work = VM.task_output_work runtime in
  let generated = VM.task_generated_bytes runtime in
  let prior = VM.latest_task_admission runtime in
  (match T.execute_runtime runtime program with
  | Error (error :: _) ->
      Alcotest.(check string) error.message "HCIRVM0026" error.code;
      Alcotest.(check bool)
        "rejection is preflight" true
        (error.stage = VM.Preflight)
  | _ -> Alcotest.fail "source command admitted without its original order");
  Alcotest.(check int) "no runtime work" steps (VM.task_executed_steps runtime);
  Alcotest.(check int)
    "no preparation work" preparation
    (VM.task_initializer_steps runtime);
  Alcotest.(check string)
    "ordinary capture unchanged" output
    (VM.task_output_bytes runtime);
  Alcotest.(check int)
    "output work unchanged" work
    (VM.task_output_work runtime);
  Alcotest.(check int)
    "generation unchanged" generated
    (VM.task_generated_bytes runtime);
  Alcotest.(check bool)
    "no new admission" true
    (match (prior, VM.latest_task_admission runtime) with
    | None, None -> true
    | Some a, Some b -> a == b
    | _ -> false)

let reversed_siblings () =
  let session, runtime, ledger = T.runtime_setup () in
  let _, commands = parse session ledger "40;42;" in
  let first = compile session runtime ledger (List.nth commands 0) in
  let second = compile session runtime ledger (List.nth commands 1) in
  rejected runtime second;
  T.execute_runtime_ok runtime first;
  T.execute_runtime_ok runtime second;
  rejected runtime first

let original_receipt_replay () =
  List.iter
    (fun text ->
      let session, runtime, ledger = T.runtime_setup () in
      let ast, _ = parse session ledger text in
      let first = compile session runtime ledger ast in
      let duplicate = compile session runtime ledger ast in
      T.execute_runtime_ok runtime first;
      rejected runtime duplicate)
    [ "42;"; "40;42;"; "" ]

let resume_before_execution () =
  let session, runtime, ledger = T.runtime_setup () in
  let pending = ref None in
  let on_event = function
    | Parser.Command_completed receipt ->
        let program = compile session runtime ledger receipt.command_ast in
        pending := Some program;
        rejected runtime program;
        Ok ()
    | Parser.Command_resumed _ ->
        T.execute_runtime_ok runtime (Option.get !pending);
        Ok ()
    | _ -> Ok ()
  in
  ignore (parse ~on_event session ledger "42;")

let nested ?(aot = false) session ledger text =
  let resumed = ref [] in
  let parents = ref [] in
  let sink : Parser.command_sink =
    {
      checkpoint =
        Some
          (fun event ->
            Result.map
              (fun () ->
                match event with
                | Parser.Sequence_started context ->
                    Option.iter
                      (fun parent -> parents := parent :: !parents)
                      (Parser.context_parent context)
                | Parser.Command_resumed receipt ->
                    resumed := receipt.command_ast :: !resumed
                | _ -> ())
              (D.observe_command ledger event));
      reference = Some (D.observe_reference ledger);
      query = Some (D.observe_query ledger);
      declaration = Some (D.observe ledger);
      dimension_count = Some (D.grammar_dimension_count ledger);
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let source =
    Session.add_source session ~path:"nested-order.hc" ~contents:text
  in
  let config =
    Preprocessor.Config.create
      ~compilation_mode:(if aot then Preprocessor.Aot else Preprocessor.Jit)
      ()
    |> T.checked
  in
  let outer_symbols =
    if aot then Session.symbols (Session.fork_frontend session)
    else Session.symbols session
  in
  let output =
    Parser.parse
      ?commands:(if aot then None else Some sink)
      ~execute_stream:(fun _ ->
        Ok
          Parser.
            {
              definitions = Session.definitions session;
              symbols = Session.symbols session;
              commands = sink;
              finish = (fun () -> Ok "");
              abort = (fun () -> ());
            })
      ~sources:(Session.sources session) ~symbols:outer_symbols
      ~definitions:(Session.definitions session)
      ~config source
  in
  (Test_parser.expect_ast output, List.rev !resumed, List.rev !parents)

let nested_order () =
  List.iter
    (fun (text, phase) ->
      let session, runtime, ledger = T.runtime_setup () in
      let _, asts, parents = nested session ledger text in
      Alcotest.(check bool)
        "original suspended parser phase" true
        (List.exists
           (fun parent ->
             match parent with
             | Parser.Before_first_command _ -> phase = 0
             | Parser.Reading_command _ -> phase = 1
             | Parser.Awaiting_resume _ -> phase = 2)
           parents);
      let programs = List.map (compile session runtime ledger) asts in
      Alcotest.(check bool)
        "multiple real commands" true
        (List.length programs >= 2);
      List.iter (rejected runtime) (List.tl programs);
      List.iter (T.execute_runtime_ok runtime) programs;
      List.iter (rejected runtime) programs)
    [
      ("#exe {1;}#exe {2;}3;", 0);
      ("1+#exe {2;}41;", 1);
      ("I64 B;#exe {2;}3;", 2);
    ]

let grouped_nested_order () =
  List.iter
    (fun text ->
      let session, runtime, ledger = T.runtime_setup () in
      let ast, children, _ = nested session ledger text in
      let whole = compile session runtime ledger ast in
      rejected runtime whole;
      let children =
        List.filter
          (fun child ->
            not
              (List.exists
                 (fun item -> List.exists (( == ) item) ast.Ast.items)
                 child.Ast.items))
          children
      in
      List.iter
        (fun ast ->
          compile session runtime ledger ast |> T.execute_runtime_ok runtime)
        children;
      T.execute_runtime_ok runtime whole;
      let duplicate = compile session runtime ledger ast in
      rejected runtime duplicate)
    [ "#exe {1;}"; "#exe {1;}#exe {2;}42;" ];
  let session, runtime, ledger = T.runtime_setup () in
  let ast, children, _ = nested session ledger ";;#exe {1;}42;" in
  let whole = compile session runtime ledger ast in
  let child =
    List.find
      (fun ast ->
        match ast.Ast.items with
        | [ Ast.Top_level_statement (Ast.Expression_statement statement) ] -> (
            match statement.expression_statement_expression with
            | Ast.Integer_literal _ -> true
            | _ -> false)
        | _ -> false)
      children
  in
  let child = compile session runtime ledger child in
  rejected runtime child;
  rejected runtime whole

let independent_roots () =
  let session, runtime, ledger = T.runtime_setup () in
  let a, _ = parse session ledger "42;" in
  let a = compile session runtime ledger a in
  let b, _ = parse session ledger "42;" in
  let b = compile session runtime ledger b in
  T.execute_runtime_ok runtime b;
  T.execute_runtime_ok runtime a;
  let session, runtime, ledger = T.runtime_setup () in
  let _, children, _ =
    nested ~aot:true session ledger "99;#exe {1;}#exe {2;}"
  in
  let programs = List.map (compile session runtime ledger) children in
  Alcotest.(check int)
    "only task children require task admission" 2 (List.length programs);
  rejected runtime (List.nth programs 1);
  List.iter (T.execute_runtime_ok runtime) programs

let fault_and_preflight () =
  let session, runtime, ledger = T.runtime_setup () in
  let _, asts = parse session ledger "1/0;42;" in
  let programs = List.map (compile session runtime ledger) asts in
  (match T.execute_runtime runtime (List.hd programs) with
  | Error (error :: _) ->
      Alcotest.(check string) "reached fault" "HCIRVM0009" error.code
  | _ -> Alcotest.fail "expected reached division fault");
  T.execute_runtime_ok runtime (List.nth programs 1);
  rejected runtime (compile session runtime ledger (List.hd asts));
  let session, runtime, ledger = T.runtime_setup ~max_global_bytes:8 () in
  let _, asts = parse session ledger "I64 A[2];42;" in
  let programs = List.map (compile session runtime ledger) asts in
  List.iter
    (fun () ->
      (match T.execute_runtime runtime (List.hd programs) with
      | Error (error :: _) ->
          Alcotest.(check string)
            "failed preflight remains retryable" "HCIRVM0016" error.code
      | _ -> Alcotest.fail "expected global byte preflight rejection");
      rejected runtime (List.nth programs 1))
    [ (); () ]

let original_mode () =
  let session, runtime, ledger = T.runtime_setup () in
  let source =
    Session.add_source session ~path:"aot-order.hc" ~contents:"42;"
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:Preprocessor.Aot ()
    |> T.checked
  in
  let commands : Parser.command_sink =
    {
      checkpoint = Some (D.observe_command ledger);
      declaration = Some (D.observe ledger);
      reference = None;
      query = None;
      dimension_count = None;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let result =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~symbols:(Session.symbols session)
      ~definitions:(Session.definitions session)
      ~config source
  in
  Alcotest.(check bool)
    "actual AOT source cannot acquire task order" true
    (Option.is_none result.ast);
  Alcotest.(check bool)
    "source-mode diagnostic" true
    (List.exists
       (fun (d : Diagnostic.t) -> d.code = "HCRUN0004")
       result.diagnostics);
  Alcotest.(check int)
    "mode rejection does not execute" 0
    (VM.task_executed_steps runtime)

let substituted_bundle () =
  let session, runtime, ledger = T.runtime_setup () in
  let ast, _ = parse session ledger "42;" in
  let program = compile session runtime ledger ast in
  let other = T.compile_runtime session runtime "99;" in
  let globals = integer_program_globals program in
  let prepared =
    Test_function_call_conversion_policy.prepare ~path:"bundle.hc" "99;"
  in
  let _, function_sources =
    Test_function_call_expression_result.analyze prepared
  in
  let top_level, records =
    Test_top_level_function_call_target_classification.analyze prepared
  in
  List.iter
    (fun entry ->
      let initialization =
        Ir_global_initialization.create ~span:ast.span ~globals ~entry []
        |> T.expect
      in
      let runtime_calls =
        Ir_runtime_call_context.create ~records ~function_sources ~top_level
          ~initialization ~entry ~entry_calls:[] ~functions:[]
        |> T.expect
      in
      Alcotest.(check bool)
        "rebuilt contexts match the candidate graph" true
        (Ir_runtime_call_context.matches runtime_calls ~entry
           ~initialization:(Some initialization) ~functions:[]);
      (match
         VM.execute_task_program runtime ~runtime_calls ~globals ~initialization
           ~functions:[] entry
       with
      | Error (error :: _) ->
          Alcotest.(check string)
            "source order rejects replacement bundle" "HCIRVM0026" error.code
      | _ -> Alcotest.fail "source proof accepted another compiled bundle");
      Alcotest.(check bool)
        "original binding cannot be replaced" true
        (Result.is_error
           (VM.bind_task_source_program runtime ~runtime_calls ~globals
              ~initialization ~functions:[] entry)))
    [ integer_program_entry program; integer_program_entry other ];
  Alcotest.(check int)
    "substitutions perform no work" 0
    (VM.task_executed_steps runtime);
  T.execute_runtime_ok runtime program

let source_cannot_drop_its_receipt () =
  List.iter
    (fun text ->
      let session, runtime, ledger = T.runtime_setup () in
      let whole, commands = parse session ledger text in
      let last =
        Option.value (List.nth_opt (List.rev commands) 0) ~default:whole
      in
      let wrapper =
        Ast.make_module ~source:last.source ~span:last.span ~items:last.items
      in
      let check ast =
        let count =
          List.length
            (Semantic_symbol_table.all_symbols
               (Session.semantic_symbols session))
        in
        let preparation = VM.task_initializer_steps runtime in
        (match
           compile_integer_task_ast ~task:runtime session ~config:(T.config ())
             ast
         with
        | Error (diagnostic :: _) ->
            Alcotest.(check string)
              "source-owned AST requires its declaration receipt" "HCRUN0004"
              diagnostic.code
        | _ ->
            Alcotest.fail
              "source-owned syntax was downgraded to a legacy command");
        Alcotest.(check int)
          "rejection precedes semantic collection" count
          (List.length
             (Semantic_symbol_table.all_symbols
                (Session.semantic_symbols session)));
        Alcotest.(check int)
          "rejection precedes preparation" preparation
          (VM.task_initializer_steps runtime)
      in
      check whole;
      check last;
      if last.items <> [] then check wrapper;
      let program = compile session runtime ledger whole in
      T.execute_runtime_ok runtime program;
      check whole;
      if last.items <> [] then check wrapper;
      let source =
        Source_manager.find (Session.sources session) whole.source |> Option.get
      in
      let fresh =
        Parser.parse ~sources:(Session.sources session)
          ~symbols:(Session.symbols session)
          ~definitions:(Session.definitions session)
          ~config:(T.config ()) source
        |> Test_parser.expect_ast
      in
      let fresh =
        (compile_integer_task_ast ~task:runtime session ~config:(T.config ())
           fresh
        |> T.expect)
          .value
      in
      T.execute_runtime_ok runtime fresh)
    [ "40;42;"; "" ]

let tests =
  [
    Alcotest.test_case "source siblings require actual predecessor admission"
      `Quick reversed_siblings;
    Alcotest.test_case "recompiled source and empty sequences cannot replay"
      `Quick original_receipt_replay;
    Alcotest.test_case "pending source execution waits for parser resume" `Quick
      resume_before_execution;
    Alcotest.test_case "nested resumes preserve all three suspended phases"
      `Quick nested_order;
    Alcotest.test_case "whole sequences retain pending nested work" `Quick
      grouped_nested_order;
    Alcotest.test_case "independent roots and foreign AOT parents" `Quick
      independent_roots;
    Alcotest.test_case "admission distinguishes reached faults and preflight"
      `Quick fault_and_preflight;
    Alcotest.test_case "task source order retains actual parser mode" `Quick
      original_mode;
    Alcotest.test_case "source order requires the exact compiled bundle" `Quick
      substituted_bundle;
    Alcotest.test_case "source-owned syntax cannot omit its declaration receipt"
      `Quick source_cannot_drop_its_receipt;
  ]
