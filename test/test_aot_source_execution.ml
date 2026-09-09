open Holyc_lib
module Output = Test_integer_output
module VM = Ir_integer_interpreter

let run ?max_steps ?max_initializer_steps ?max_dimension_work ?max_global_bytes
    ?max_literal_bytes ?max_output_bytes ?max_output_work ?max_generated_bytes
    source =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"aot-stream.hc" ~contents:source
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:Aot ?max_generated_bytes ()
    |> Test_task_stream.checked
  in
  run_integer_program_report ?max_initializer_steps ?max_dimension_work
    ?max_global_bytes ?max_literal_bytes ?max_output_bytes ?max_output_work
    ~max_steps:(Option.value max_steps ~default:10000)
    session ~config ~source

let generates source () =
  ignore (run source |> Output.expect ~value:(Some 42L) "")

let reached_failures () =
  List.iter
    (fun (code, source) ->
      let report = run source in
      ignore (Output.fault ~output:"A" code report);
      let progress = Option.get (integer_program_report_progress report) in
      Alcotest.(check bool)
        "reached instructions remain observed" true
        (progress.runtime.executed_steps > 0);
      Alcotest.(check bool)
        "checked task units survive" true
        (integer_program_report_task_units report <> []))
    [
      ("HCPARSE0127", {|#exe {Print("A");}I64 Broken=;|});
      ("HCRUN0003", {|#exe {Print("A");}40+Missing;|});
      ("HCIRVM0009", {|#exe {Print("A");StreamPrint("discard");1/0;}|});
      ("HCIRVM0009", {|#exe {Print("A");}1/0;|});
    ]

let namespace_separation () =
  ignore
    (run {|I64 Print=7;#exe {Print("A");StreamPrint("42;");}|}
    |> Output.expect "A");
  ignore
    (run {|I64 N=40;#exe {1+N+#exe {Print("late");}2;}|}
    |> Output.fault "HCRUN0003");
  ignore
    (run {|I64 F(){I64 N=40;#exe {1+N+#exe {Print("late");}2;}}|}
    |> Output.fault "HCRUN0003")

let inactive_text () =
  List.iter
    (fun source ->
      let report = run ~max_steps:3 source in
      let result = Output.expect "" report in
      Alcotest.(check int)
        "ordinary instructions unchanged" 3 (VM.executed_steps result);
      Alcotest.(check int)
        "ordinary preparation unchanged" 0
        (VM.compiled_initializer_steps result);
      Alcotest.(check bool)
        "no task activated" true
        (integer_program_report_progress report = None);
      Alcotest.(check int)
        "no task units" 0
        (List.length (integer_program_report_task_units report)))
    [
      "42;";
      "// #exe {Print(\"late\");}\n42;";
      "#if 0\n#exe {Print(\"late\");}\n#endif\n42;";
    ];
  let report =
    run {|I64 F(){U8 *p="#exe {Print(\"late\");}";return 42;}F();|}
  in
  ignore (Output.expect "" report);
  Alcotest.(check bool)
    "string content does not activate a task" true
    (integer_program_report_progress report = None)

let isolated_artifact () =
  let report = run {|#exe {Print("A");StreamPrint("I64 N=40;N+=2;N;");}|} in
  let result = Output.expect "A" report in
  let program = Option.get (integer_program_report_program report) in
  let units = integer_program_report_task_units report in
  Alcotest.(check bool)
    "outer artifact is distinct from every task unit" true
    (List.for_all
       (fun unit -> integer_program_entry unit != integer_program_entry program)
       units);
  List.iter
    (fun () ->
      let execution =
        VM.execute_program_report
          ~runtime_calls:(integer_program_runtime_calls program)
          ~globals:(integer_program_globals program)
          ~initialization:(integer_program_initialization program)
          ~max_steps:10000 ~max_frame_bytes:1048576 ~max_call_depth:128
          ~functions:(integer_program_functions program)
          (integer_program_entry program)
      in
      Alcotest.(check string)
        "isolated execution does not replay directive output" ""
        (VM.report_output_bytes execution);
      match VM.report_outcome execution with
      | Error _ -> Alcotest.fail "isolated artifact execution failed"
      | Ok outer ->
          Alcotest.(check (option int64))
            "fresh isolated storage" (Some 42L)
            (Option.map (fun (w : VM.word) -> w.bits) (VM.final_value outer));
          Alcotest.(check bool)
            "invocation includes earlier task work" true
            (VM.executed_steps result > VM.executed_steps outer))
    [ (); () ]

let resource_source =
  {|#exe {I64 N=20+20;Print("A");StreamPrint("%d;",N+2);}extern U0 Print(U8 *fmt,...);Print("B");I64 G=1+1;G+40;|}

let combined_limits () =
  let report = run resource_source in
  let result = Output.expect "AB" report in
  let measured =
    (Option.get (integer_program_report_progress report)).runtime
  in
  let counts =
    [
      measured.executed_steps;
      measured.initializer_steps;
      measured.global_bytes;
      measured.literal_bytes;
      measured.generated_bytes;
      measured.output_work;
      String.length measured.output_bytes;
    ]
  in
  Alcotest.(check (list int))
    "measured cumulative resources"
    [ 37; 10; 16; 8; 3; 13; 2 ]
    counts;
  Alcotest.(check int)
    "VM reports cumulative instruction work" measured.executed_steps
    (VM.executed_steps result);
  Alcotest.(check int)
    "VM reports cumulative preparation" measured.initializer_steps
    (VM.compiled_initializer_steps result);
  ignore
    (run ~max_steps:measured.executed_steps
       ~max_initializer_steps:measured.initializer_steps
       ~max_global_bytes:measured.global_bytes
       ~max_literal_bytes:measured.literal_bytes
       ~max_output_work:measured.output_work ~max_output_bytes:2 resource_source
    |> Output.expect "AB");
  List.iter
    (fun (code, output, report) -> ignore (Output.fault ~output code report))
    [
      ( "HCIRVM0007",
        "AB",
        run ~max_steps:(measured.executed_steps - 1) resource_source );
      ( "HCIRVM0007",
        "A",
        run
          ~max_initializer_steps:(measured.initializer_steps - 1)
          resource_source );
      ( "HCIRVM0016",
        "A",
        run ~max_global_bytes:(measured.global_bytes - 1) resource_source );
      ( "HCIRVM0021",
        "A",
        run ~max_literal_bytes:(measured.literal_bytes - 1) resource_source );
      ( "HCIRVM0023",
        "A",
        run ~max_output_work:(measured.output_work - 1) resource_source );
      ("HCIRVM0022", "A", run ~max_output_bytes:1 resource_source);
    ]

let exhausted_preparation_without_more_work () =
  ignore
    (run ~max_initializer_steps:5 {|#exe {I64 N=20+20;StreamPrint("%d;",N+2);}|}
    |> Output.expect "")

let outer_read_timing () =
  List.iter
    (fun (prefix, output) ->
      let report = run (prefix ^ {|40+Missing #exe {Print("late");};|}) in
      Alcotest.(check string)
        "outer unavailable read prevents later directive effects" output
        (integer_program_report_output_bytes report);
      ignore (Output.fault ~output "HCRUN0003" report))
    [ ("", ""); ({|#exe {Print("A");}|}, "A") ]

let earlier_read_precedes_bad_opener () =
  ignore (run {|40+Missing #exe 42|} |> Output.fault "HCRUN0003")

let inactive_generation_budget () =
  let report =
    run ~max_generated_bytes:1
      {|#exe {StreamPrint("1");} ; extern U0 StreamPrint(U8 *fmt,...);StreamPrint("X");|}
  in
  ignore (Output.fault "HCIRVM0028" report);
  let progress = Option.get (integer_program_report_progress report) in
  Alcotest.(check int)
    "earlier generated capacity remains charged" 1
    progress.runtime.generated_bytes

let prepare_isolated task contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"isolated-output.hc" ~contents
  in
  let ledger =
    Task_declarations.create_source session ~source |> Test_task_stream.checked
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:Aot ()
    |> Test_task_stream.checked
  in
  let commands : Parser.command_sink =
    {
      checkpoint = Some (Task_declarations.observe_command ledger);
      reference = None;
      query = Some (Task_declarations.observe_query ledger);
      declaration = Some (Task_declarations.observe ledger);
      dimension_count = Some (Task_declarations.grammar_dimension_count ledger);
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let parsed =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config source
  in
  let source_command =
    Task_declarations.seal_source ledger (Test_parser.expect_ast parsed)
    |> Test_integer_program.checked
  in
  let compiled =
    Integer_task.compile_isolated task ~source_command session ~config parsed
    |> Test_integer_program.checked
  in
  compiled.value

let isolated_preparation_owner () =
  let task =
    Integer_task.create (Session.create ()) |> Test_task_stream.checked
  in
  let other =
    Integer_task.create (Session.create ()) |> Test_task_stream.checked
  in
  let program = prepare_isolated other "I64 N=40;N+2;" in
  let before = Integer_task.progress task in
  (match Integer_task.execute_isolated task program with
  | Error (error :: _) ->
      Alcotest.(check string) error.message "HCIRVM0026" error.code
  | _ -> Alcotest.fail "unaccounted isolated preparation was accepted");
  Alcotest.(check bool)
    "unowned preparation cannot spend task resources" true
    (before = Integer_task.progress task)

let isolated_lifecycle () =
  let session = Session.create () in
  let task = Integer_task.create session |> Test_task_stream.checked in
  let program = prepare_isolated task "I64 N=40;I64 F(){return ++N;}F()+1;" in
  let stream = Integer_task.begin_stream task |> Test_task_stream.checked in
  let before = Integer_task.progress task in
  (match Integer_task.execute_isolated task program with
  | Error (error :: _) ->
      Alcotest.(check string) error.message "HCIRVM0027" error.code
  | _ -> Alcotest.fail "isolated image executed in an active stream");
  Alcotest.(check bool)
    "failed preflight leaves every counter unchanged" true
    (before = Integer_task.progress task);
  Integer_task.abort_stream task stream |> Test_task_stream.checked;
  (match Integer_task.execute_isolated task program with
  | Ok result ->
      Alcotest.(check (option int64))
        "same image executes after preflight recovery" (Some 42L)
        (Option.map (fun (w : VM.word) -> w.bits) (VM.final_value result))
  | Error _ ->
      Alcotest.fail "valid isolated image failed after preflight recovery");
  Test_integer_task.value 0L
    (Test_integer_task.run session task "defined N || defined F;");
  let before = Integer_task.progress task in
  (match Integer_task.execute_isolated task program with
  | Error (error :: _) ->
      Alcotest.(check string) error.message "HCIRVM0026" error.code
  | _ -> Alcotest.fail "isolated image replay succeeded");
  Alcotest.(check bool)
    "replay does not alter later task progress" true
    (before = Integer_task.progress task);
  let program = prepare_isolated task "I64 N=40;N++;1/0;" in
  (match Integer_task.execute_isolated task program with
  | Error (error :: _) ->
      Alcotest.(check string) error.message "HCIRVM0009" error.code
  | _ -> Alcotest.fail "isolated reached fault disappeared");
  let before = Integer_task.progress task in
  Alcotest.(check (option int64))
    "reached postfix discard survives the fault" (Some 40L)
    (Option.map (fun (w : VM.word) -> w.bits) before.runtime.final_value);
  ignore (Integer_task.execute_isolated task program);
  Alcotest.(check bool)
    "reached fault consumes the isolated image" true
    (before = Integer_task.progress task)

let preparation_ticket () =
  let session = Session.create () in
  let task =
    VM.create_task_state ~table:(Session.semantic_symbols session) ()
    |> Test_task_stream.checked
  in
  let program = Test_integer_globals.compile ~mode:Aot "I64 N=40;N+2;" in
  let entry = integer_program_entry program in
  let globals = integer_program_globals program in
  let initialization = integer_program_initialization program in
  let runtime_calls = integer_program_runtime_calls program in
  let functions = integer_program_functions program in
  let seal ticket =
    VM.finish_isolated_preparation task ticket ~runtime_calls ~globals
      ~initialization ~functions entry
  in
  let execute () =
    VM.execute_isolated_program_in_task task ~runtime_calls ~globals
      ~initialization ~functions entry
  in
  let ticket = VM.begin_isolated_preparation task in
  Alcotest.(check bool)
    "uncharged preparation cannot seal" true
    (Result.is_error (seal ticket));
  Alcotest.(check bool)
    "unsealed output cannot execute" true
    (Result.is_error (execute ()));
  VM.record_isolated_preparation task ticket ~steps:3;
  seal ticket |> Test_task_stream.checked;
  Alcotest.(check bool)
    "preparation seal rejects replay" true
    (Result.is_error (seal ticket));
  (match execute () with
  | Ok result ->
      Alcotest.(check int)
        "registered preparation is counted once" 3
        (VM.compiled_initializer_steps result)
  | Error _ -> Alcotest.fail "sealed isolated output did not execute");
  let other =
    VM.create_task_state ~table:(Session.semantic_symbols session) ()
    |> Test_task_stream.checked
  in
  let ticket = VM.begin_isolated_preparation other in
  Alcotest.(check bool)
    "foreign preparation cannot seal" true
    (Result.is_error (seal ticket));
  let ticket = VM.begin_isolated_preparation task in
  VM.record_isolated_preparation task ticket ~steps:2;
  VM.abort_isolated_preparation task ticket;
  Alcotest.(check bool)
    "aborted preparation cannot seal" true
    (Result.is_error (seal ticket));
  Alcotest.(check int)
    "aborted work remains charged" 5 (VM.task_progress task).initializer_steps

let combined_dimensions () =
  let source =
    {|I64 A[1+1]={40,2};#exe {I64 B[1+1]={40,2};StreamPrint("%d;",B[0]+B[1]);}A[0]+A[1];|}
  in
  let report = run ~max_dimension_work:3 source in
  ignore (Output.expect "" report);
  let progress = Option.get (integer_program_report_progress report) in
  Alcotest.(check int)
    "source and stream dimensions are reported" 6
    (integer_program_report_dimension_work report);
  Alcotest.(check int)
    "only stream dimensions are part of task preparation" 3
    progress.dimension_work;
  Alcotest.(check int)
    "cumulative initializer work includes stream dimensions once" 15
    progress.runtime.initializer_steps;
  ignore
    (run ~max_dimension_work:3 ~max_initializer_steps:15 source
    |> Output.expect "");
  ignore (run ~max_initializer_steps:14 source |> Output.fault "HCIRVM0007");
  let report = run ~max_dimension_work:2 source in
  ignore (Output.fault "HCIRVM0007" report);
  Alcotest.(check bool)
    "early source dimension failure does not activate providers" true
    (integer_program_report_progress report = None)

let invalid_limits () =
  List.iter
    (fun invoke ->
      let report = invoke {|#exe {Print("late");}42;|} in
      ignore (Output.fault "HCIRVM0001" report);
      Alcotest.(check bool)
        "invalid configuration does not activate providers" true
        (integer_program_report_progress report = None);
      Alcotest.(check int)
        "invalid configuration compiles no task units" 0
        (List.length (integer_program_report_task_units report)))
    [
      run ~max_steps:0;
      run ~max_initializer_steps:0;
      run ~max_dimension_work:0;
      run ~max_global_bytes:0;
      run ~max_literal_bytes:0;
      run ~max_output_bytes:0;
      run ~max_output_work:0;
    ]

let generated_limits () =
  ignore
    (run ~max_generated_bytes:3 {|#exe {StreamPrint("42;");}|}
    |> Output.expect ~value:(Some 42L) "");
  List.iter
    (fun limit ->
      let report =
        run ~max_generated_bytes:limit
          {|#exe {Print("A");StreamPrint("4");StreamPrint("2;");}|}
      in
      ignore (Output.fault ~output:"A" "HCIRVM0028" report);
      let progress = Option.get (integer_program_report_progress report) in
      Alcotest.(check int)
        "failed buffer keeps its earlier accepted generated charge"
        (if limit = 0 then 0 else 1)
        progress.runtime.generated_bytes;
      Alcotest.(check bool)
        "failed generated text cannot form an outer artifact" true
        (integer_program_report_program report = None))
    [ 0; 2 ];
  ignore
    (run ~max_generated_bytes:0 {|#exe {Print("A");}42;|}
    |> Output.expect ~value:(Some 42L) "A")

let tests =
  List.map
    (fun (name, source) -> Alcotest.test_case name `Quick (generates source))
    Test_stateful_exe.gates
  @ [
      Alcotest.test_case "joined integer token" `Quick
        (generates {|#exe {StreamPrint("4");}2;|});
      Alcotest.test_case "nested generation" `Quick
        (generates {|#exe {#exe {StreamPrint("StreamPrint(\"42;\");");}}|});
      Alcotest.test_case "generated function body" `Quick
        (generates {|I64 F(){#exe {StreamPrint("return 42;");}}F();|});
      Alcotest.test_case "generated initializer operand" `Quick
        (generates {|I64 N=#exe {StreamPrint("42");};N;|});
      Alcotest.test_case "reached effects survive later failures" `Quick
        reached_failures;
      Alcotest.test_case "early AOT task namespace separation" `Quick
        namespace_separation;
      Alcotest.test_case "ordinary and textual directives stay inactive" `Quick
        inactive_text;
      Alcotest.test_case "isolated artifact excludes task units and stays fresh"
        `Quick isolated_artifact;
      Alcotest.test_case "task and outer execution share invocation limits"
        `Quick combined_limits;
      Alcotest.test_case
        "zero remaining preparation allows work-free outer source" `Quick
        exhausted_preparation_without_more_work;
      Alcotest.test_case "outer source read errors precede directive effects"
        `Quick outer_read_timing;
      Alcotest.test_case "earlier source read precedes a bad directive opener"
        `Quick earlier_read_precedes_bad_opener;
      Alcotest.test_case "inactive outer StreamPrint keeps generated allowance"
        `Quick inactive_generation_budget;
      Alcotest.test_case "isolated preparation belongs to its invocation" `Quick
        isolated_preparation_owner;
      Alcotest.test_case
        "isolated admission preflight replay and catalog separation" `Quick
        isolated_lifecycle;
      Alcotest.test_case "isolated preparation tickets bind exact charged work"
        `Quick preparation_ticket;
      Alcotest.test_case "source and stream dimension accounting stays distinct"
        `Quick combined_dimensions;
      Alcotest.test_case "invalid invocation limits precede provider setup"
        `Quick invalid_limits;
      Alcotest.test_case "exact and exhausted generated allowances" `Quick
        generated_limits;
    ]
