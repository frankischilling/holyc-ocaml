open Holyc_lib
module Task = Integer_task
module T = Test_integer_task
module Stream = Test_task_stream
module VM = Ir_integer_interpreter

let setup ?max_steps ?max_initializer_steps ?max_global_bytes ?max_literal_bytes
    ?max_frame_bytes ?max_call_depth ?max_output_work ?max_output_bytes
    ?max_generated_bytes ?max_stream_depth () =
  let outer = Session.create () in
  let task_session = Session.fork_frontend outer in
  let task =
    Task.create ?max_steps ?max_initializer_steps ?max_global_bytes
      ?max_literal_bytes ?max_frame_bytes ?max_call_depth ?max_output_work
      ?max_output_bytes ?max_generated_bytes ?max_stream_depth task_session
    |> Stream.checked
  in
  ignore (T.run task_session task Stream.headers |> Test_integer_program.checked);
  (outer, task_session, task)

let parse ?(mode = Preprocessor.Jit) ?configure ?executor outer task source =
  let source =
    Session.add_source outer ~path:"task-executor.hc" ~contents:source
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> Stream.checked
  in
  let execute_stream span =
    (match executor with
      | None -> Task.stream_executor task span
      | Some enter -> enter span)
    |> Result.map (fun execution ->
        Option.fold ~none:execution
          ~some:(fun configure -> configure execution)
          configure)
  in
  let parsed =
    Parser.parse ~execute_stream ~sources:(Session.sources outer)
      ~definitions:(Session.definitions outer)
      ~symbols:(Session.symbols outer) ~config source
  in
  (config, parsed)

let execute_outer outer config parsed =
  let ast = Test_parser.expect_ast parsed in
  let program =
    (compile_integer_ast outer ~config ast |> Test_integer_program.checked)
      .value
  in
  VM.execute_program
    ~runtime_calls:(integer_program_runtime_calls program)
    ~globals:(integer_program_globals program)
    ~initialization:(integer_program_initialization program)
    ~functions:(integer_program_functions program)
    ~max_steps:10000 ~max_frame_bytes:1048576 ~max_call_depth:128
    (integer_program_entry program)
  |> function
  | Ok execution -> execution
  | Error errors ->
      Alcotest.fail
        (String.concat "; " (List.map (fun (e : VM.error) -> e.message) errors))

let generates source () =
  List.iter
    (fun mode ->
      let outer, _, task = setup () in
      let config, parsed = parse ~mode outer task source in
      let execution = execute_outer outer config parsed in
      Alcotest.(check (option int64))
        "generated source executes to 42" (Some 42L)
        (Option.map
           (fun (word : VM.word) -> word.bits)
           (VM.final_value execution));
      Alcotest.(check string)
        "ordinary capture is separate" "" (Task.output_bytes task))
    Test_integer_globals.modes

let rejected code (_, parsed) =
  Alcotest.(check bool)
    "failed stream produces no outer AST" true
    (Option.is_none parsed.Parser.ast);
  match parsed.diagnostics with
  | first :: _ -> Alcotest.(check string) first.message code first.code
  | [] -> Alcotest.fail "missing stream failure diagnostic"

let early_reference_reads () =
  List.iter
    (fun source ->
      let outer, _, task = setup () in
      let parsed = parse outer task source in
      Alcotest.(check string)
        "read error precedes the following directive" ""
        (Task.output_bytes task);
      rejected "HCRUN0003" parsed)
    [
      {|#exe {40+Missing #exe {Print("late");};}|};
      {|#exe {I64 N=40;#exe {1+N+#exe {Print("late");}2;}}|};
      {|I64 N=40;#exe {1+N+#exe {Print("late");}2;}|};
    ];
  let outer, _, task = setup () in
  rejected "HCPARSE0001"
    (parse outer task {|#exe {Missing #exe {Print("probe");};}|});
  Alcotest.(check string)
    "statement-start label probe still precedes error" "probe"
    (Task.output_bytes task)

let fault_cleanup () =
  let outer, session, task = setup ~max_stream_depth:1 () in
  rejected "HCIRVM0009"
    (parse outer task {|#exe {Print("A");StreamPrint("42;");I64 N=41;N++;1/0;}|});
  Alcotest.(check string)
    "reached ordinary output survives" "A" (Task.output_bytes task);
  Alcotest.(check int)
    "discarded generated text stays charged" 3
    (Task.generated_bytes task);
  T.value 42L (T.run session task "N;");
  let config, parsed = parse outer task {|#exe {StreamPrint("42;");}|} in
  Alcotest.(check (option int64))
    "later independent input still executes" (Some 42L)
    (Option.map
       (fun (w : VM.word) -> w.bits)
       (execute_outer outer config parsed |> VM.final_value));
  rejected "HCPARSE0127" (parse outer task {|#exe {Print("B");}I64 Broken=;|});
  Alcotest.(check string)
    "later outer parse fault retains stream effects" "AB"
    (Task.output_bytes task)

let accepted_finish () =
  let outer, _, task = setup () in
  let saved = ref None in
  let checked_acceptance = ref false in
  let configure (execution : Parser.stream_execution) =
    saved := Some execution;
    T.fault "HCIRVM0027" (execution.finish ());
    let observe = Option.get execution.commands.checkpoint in
    {
      execution with
      commands =
        {
          execution.commands with
          checkpoint =
            Some
              (fun event ->
                Result.bind (observe event) (fun () ->
                    (match event with
                    | Parser.Sequence_completed _ ->
                        T.fault "HCIRVM0027" (execution.finish ());
                        checked_acceptance := true
                    | _ -> ());
                    Ok ()));
        };
    }
  in
  let config, parsed =
    parse ~configure outer task {|#exe {StreamPrint("42;");}|}
  in
  ignore (execute_outer outer config parsed);
  Alcotest.(check bool)
    "finish waited for parser acceptance" true !checked_acceptance;
  T.fault "HCIRVM0027" ((Option.get !saved).finish ());
  (Option.get !saved).abort ();
  Alcotest.(check int)
    "failed/replayed finish adds no generation" 3
    (Task.generated_bytes task)

let foreign_sources_and_environment () =
  let _, _, task = setup () in
  let before = Task.progress task in
  rejected "HCRUN0004"
    (parse (Session.create ()) task {|#exe {Print("foreign");}|});
  Alcotest.(check bool)
    "foreign source has no effects" true
    (before = Task.progress task);
  rejected "HCRUN0004"
    (parse (Task.frontend task) task {|#exe {Print("unobserved");}|});
  Alcotest.(check bool)
    "unobserved shared parent has no effects" true
    (before = Task.progress task)

let pending_statement () =
  let outer, session, task = setup () in
  ignore (T.run session task "I64 N=0;42;" |> Test_integer_program.checked);
  let config, parsed =
    parse outer task {|#exe {N=1;#exe {Print("%d",N);}StreamPrint("%d;",N);}|}
  in
  Alcotest.(check string)
    "nested lookahead sees pending store's old cell" "0"
    (Task.output_bytes task);
  Alcotest.(check (option int64))
    "resumed store affects subsequent generation" (Some 1L)
    (execute_outer outer config parsed
    |> VM.final_value
    |> Option.map (fun (w : VM.word) -> w.bits));
  Alcotest.(check (option int64))
    "streams preserve the task's outer latch" (Some 42L)
    ((Task.progress task).runtime.final_value
    |> Option.map (fun (w : VM.word) -> w.bits))

let executor_replay () =
  List.iter
    (fun suspended ->
      let outer, _, task = setup () in
      let saved = ref None in
      let configure execution =
        saved := Some execution;
        execution
      in
      ignore (parse ~configure outer task {|#exe {StreamPrint("42;");}|});
      let buffer = if suspended then Some (Stream.begin_ task) else None in
      let before = Task.progress task in
      let parsed =
        parse
          ~executor:(fun _ -> Ok (Option.get !saved))
          outer task {|#exe {Print("late");StreamPrint("late");}|}
      in
      Alcotest.(check bool)
        "a consumed executor cannot execute a later context" true
        (before = Task.progress task);
      rejected "HCIRVM0027" parsed;
      Option.iter
        (fun buffer ->
          Alcotest.(check string)
            "replay cannot write into another buffer" ""
            (Stream.finish task buffer))
        buffer)
    [ false; true ]

let completed_context_replay () =
  let outer, _, task = setup () in
  let saved = ref None in
  let configure execution =
    saved := Some execution;
    { execution with Parser.finish = (fun () -> Ok "") }
  in
  ignore (parse ~configure outer task {|#exe {StreamPrint("42;");}|});
  let before = Task.progress task in
  let parsed =
    parse
      ~executor:(fun _ -> Ok (Option.get !saved))
      outer task {|#exe {Print("late");}|}
  in
  Alcotest.(check bool)
    "an accepted context cannot be replaced before finish" true
    (before = Task.progress task);
  rejected "HCIRVM0027" parsed

let exception_cleanup () =
  let outer, _, task = setup ~max_stream_depth:1 () in
  let configure (execution : Parser.stream_execution) =
    let observe = Option.get execution.commands.checkpoint in
    {
      execution with
      commands =
        {
          execution.commands with
          checkpoint =
            Some
              (fun event ->
                Result.bind (observe event) (fun () ->
                    match event with
                    | Parser.Command_resumed _ -> raise Exit
                    | _ -> Ok ()));
        };
    }
  in
  (match
     parse ~configure outer task {|#exe {Print("A");StreamPrint("discard");}|}
   with
  | _ -> Alcotest.fail "callback exception was swallowed"
  | exception Exit -> ());
  Alcotest.(check string)
    "exception retains earlier reached output" "A" (Task.output_bytes task);
  let config, parsed = parse outer task {|#exe {StreamPrint("42;");}|} in
  ignore (execute_outer outer config parsed)

let early_abort_cleanup () =
  List.iter
    (fun (at_completion, raises) ->
      let outer, _, task = setup ~max_stream_depth:1 () in
      let aborted_event = ref None in
      let saved = ref None in
      let configure (execution : Parser.stream_execution) =
        saved := Some execution;
        let observe = Option.get execution.commands.checkpoint in
        {
          execution with
          commands =
            {
              execution.commands with
              checkpoint =
                Some
                  (fun event ->
                    (match event with
                    | Parser.Sequence_aborted _ -> aborted_event := Some event
                    | _ -> ());
                    Result.bind (observe event) (fun () ->
                        let abort_now =
                          match event with
                          | Parser.Command_started _ -> not at_completion
                          | Parser.Sequence_completed _ -> at_completion
                          | _ -> false
                        in
                        if not abort_now then Ok ()
                        else (
                          execution.abort ();
                          if raises then raise Exit
                          else Result.map ignore (execution.finish ()))));
            };
        }
      in
      (match parse ~configure outer task {|#exe {StreamPrint("discard");}|} with
      | result ->
          if raises then Alcotest.fail "callback exception was swallowed"
          else rejected "HCIRVM0027" result
      | exception Exit ->
          Alcotest.(check bool) "configured exception" true raises);
      Alcotest.(check string)
        "early abort adds no ordinary output" "" (Task.output_bytes task);
      let config, parsed = parse outer task {|#exe {StreamPrint("42;");}|} in
      ignore (execute_outer outer config parsed);
      T.fault "HCIRVM0027"
        ((Option.get (Option.get !saved).commands.checkpoint)
           (Option.get !aborted_event)))
    [ (false, false); (false, true); (true, false); (true, true) ]

let resource_limits () =
  let source = {|#exe {I64 N=20+20;Print("AB");StreamPrint("%d;",N+2);}|} in
  let outer, _, task = setup () in
  let config, parsed = parse outer task source in
  ignore (execute_outer outer config parsed);
  let measured = (Task.progress task).runtime in
  Alcotest.(check (list int))
    "measured task resources including provider setup" [ 22; 5; 8; 7; 3; 12; 2 ]
    [
      measured.executed_steps;
      measured.initializer_steps;
      measured.global_bytes;
      measured.literal_bytes;
      measured.generated_bytes;
      measured.output_work;
      String.length measured.output_bytes;
    ];
  let run ?max_steps ?max_initializer_steps ?max_global_bytes ?max_literal_bytes
      ?max_generated_bytes ?max_output_work ?max_output_bytes expected =
    let outer, _, task =
      setup ?max_steps ?max_initializer_steps ?max_global_bytes
        ?max_literal_bytes ?max_generated_bytes ?max_output_work
        ?max_output_bytes ~max_stream_depth:1 ()
    in
    let result = parse outer task source in
    match expected with
    | None -> ignore (execute_outer outer (fst result) (snd result))
    | Some code ->
        rejected code result;
        let stream = Stream.begin_ task in
        Alcotest.(check string)
          "fault releases its exact buffer" ""
          (Stream.finish task stream)
  in
  run ~max_steps:measured.executed_steps
    ~max_initializer_steps:measured.initializer_steps
    ~max_global_bytes:measured.global_bytes
    ~max_literal_bytes:measured.literal_bytes
    ~max_generated_bytes:measured.generated_bytes
    ~max_output_work:measured.output_work
    ~max_output_bytes:(String.length measured.output_bytes)
    None;
  run ~max_steps:(measured.executed_steps - 1) (Some "HCIRVM0007");
  run
    ~max_initializer_steps:(measured.initializer_steps - 1)
    (Some "HCIRVM0007");
  run ~max_global_bytes:(measured.global_bytes - 1) (Some "HCIRVM0016");
  run ~max_literal_bytes:(measured.literal_bytes - 1) (Some "HCIRVM0021");
  run ~max_generated_bytes:(measured.generated_bytes - 1) (Some "HCIRVM0028");
  run ~max_output_work:(measured.output_work - 1) (Some "HCIRVM0023");
  run
    ~max_output_bytes:(String.length measured.output_bytes - 1)
    (Some "HCIRVM0022")

let nested_limit () =
  let source = {|#exe {#exe {StreamPrint("StreamPrint(\"42;\");");}}|} in
  let outer, _, task = setup ~max_stream_depth:2 () in
  let config, parsed = parse outer task source in
  ignore (execute_outer outer config parsed);
  let outer, _, task = setup ~max_stream_depth:1 () in
  rejected "HCIRVM0029" (parse outer task source);
  let stream = Stream.begin_ task in
  ignore (Stream.finish task stream)

let tests =
  List.map
    (fun (name, source) -> Alcotest.test_case name `Quick (generates source))
    Test_stateful_exe.gates
  @ [
      Alcotest.test_case "joined integer token" `Quick
        (generates {|#exe {StreamPrint("4");}2;|});
      Alcotest.test_case "nested generation" `Quick
        (generates {|#exe {#exe {StreamPrint("StreamPrint(\"42;\");");}}|});
      Alcotest.test_case "generated outer function body" `Quick
        (generates {|I64 F(){#exe {StreamPrint("return 42;");}}F();|});
      Alcotest.test_case "generated outer initializer operand" `Quick
        (generates {|I64 N=#exe {StreamPrint("42");};N;|});
      Alcotest.test_case
        "execution reads fail at their original lookup boundary" `Quick
        early_reference_reads;
      Alcotest.test_case "fault cleanup retains reached effects" `Quick
        fault_cleanup;
      Alcotest.test_case "finish requires an accepted sequence" `Quick
        accepted_finish;
      Alcotest.test_case "source and parent environment ownership" `Quick
        foreign_sources_and_environment;
      Alcotest.test_case "nested lookahead precedes pending statement execution"
        `Quick pending_statement;
      Alcotest.test_case
        "consumed executors reject later and suspended contexts" `Quick
        executor_replay;
      Alcotest.test_case "accepted executor context cannot be replaced" `Quick
        completed_context_replay;
      Alcotest.test_case "callback exceptions unwind the owning stream" `Quick
        exception_cleanup;
      Alcotest.test_case "early explicit abort releases its parser context"
        `Quick early_abort_cleanup;
      Alcotest.test_case "cumulative execution and generation limits" `Quick
        resource_limits;
      Alcotest.test_case "nested parser executor depth bound" `Quick
        nested_limit;
      Alcotest.test_case "stream blocks retain function static storage" `Quick
        (generates
           {|#exe {I64 F(){static I64 N=40;return ++N;}F();}#exe {StreamPrint("%d;",F());}|});
      Alcotest.test_case "stream blocks retain mutated literal sites" `Quick
        (generates
           {|#exe {I64 F(){U8 *p="(";++*p;return *p;}F();}#exe {StreamPrint("%d;",F());}|});
    ]
