open Holyc_lib
module D = Task_declarations
module Task = Integer_task
module VM = Ir_integer_interpreter
module C = Semantic_declaration_collection

let checked = Test_declaration_collection.checked
let expect = Test_integer_program.checked

let reject label result =
  Alcotest.(check bool) label true (Result.is_error result)

let inputs text =
  let session = Session.task_frontend (Session.create ()) in
  let source = Session.add_source session ~path:"promote.hc" ~contents:text in
  let ledger = D.create_source session ~source |> checked in
  (session, source, ledger)

let parse ?(mode = Preprocessor.Jit) ?checkpoint ?reference ?query ?declaration
    ?execute_stream session source ledger =
  let commands : Parser.command_sink =
    {
      checkpoint =
        Some (Option.value checkpoint ~default:(D.observe_command ledger));
      reference =
        Some (Option.value reference ~default:(D.observe_reference ledger));
      declaration = Some (Option.value declaration ~default:(D.observe ledger));
      query = Some (Option.value query ~default:(D.observe_query ledger));
      dimension_count = Some (D.grammar_dimension_count ledger);
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  Parser.parse ~commands ?execute_stream ~sources:(Session.sources session)
    ~definitions:(Session.definitions session)
    ~symbols:(Session.symbols session)
    ~config:(Preprocessor.Config.create ~compilation_mode:mode () |> checked)
    source

let compile_execute task ast =
  Task.compile_source_ast task ast |> expect |> Task.execute task |> expect

let preserves_order () =
  let session, source, ledger = inputs "40;2;" in
  let task = ref None in
  let first = ref None in
  let checkpoint event =
    Result.map
      (fun () ->
        match event with
        | Parser.Command_resumed command
          when command.command_start.command_ordinal = 0 ->
            first := Some command
        | Parser.Command_started start when start.command_ordinal = 1 ->
            task := Some (Task.adopt_source session ~source ~ledger |> checked)
        | Parser.Command_resumed command ->
            let task = Option.get !task in
            let later =
              Task.compile_source_ast task command.command_ast |> expect
            in
            reject "deferred predecessor still required"
              (Task.execute task later);
            ignore (compile_execute task (Option.get !first).command_ast);
            ignore (Task.execute task later |> expect);
            reject "original predecessor cannot replay"
              (Task.compile_source_ast task (Option.get !first).command_ast
              |> expect |> Task.execute task)
        | _ -> ())
      (D.observe_command ledger event)
  in
  let output = parse ~checkpoint session source ledger in
  ignore (Test_parser.expect_ast output);
  let progress = Task.progress (Option.get !task) in
  Alcotest.(check (option int64))
    "outer result follows original order" (Some 2L)
    (Option.map (fun word -> word.VM.bits) progress.runtime.final_value)

let preserves_dimension () =
  let session, source, ledger = inputs "I64 A[1+1];" in
  let task = ref None in
  let symbol = ref None in
  let dimension = ref None in
  let declaration event =
    Result.map
      (fun () ->
        match event with
        | Parser.Global_declared publication ->
            symbol := D.symbol_for ledger publication.global_entry
        | Parser.Array_dimension_completed completed ->
            dimension := Some completed;
            task :=
              Some
                (Task.adopt_source ~max_initializer_steps:3 session ~source
                   ~ledger
                |> checked)
        | _ -> ())
      (D.observe ledger event)
  in
  let checkpoint event =
    Result.map
      (fun () ->
        match event with
        | Parser.Command_resumed command ->
            let task = Option.get !task in
            let ast = command.command_ast in
            let seal = D.seal ledger ast |> expect in
            let collection =
              D.collection ~table:(Session.semantic_symbols session) ~ast seal
              |> expect
            in
            Alcotest.(check bool)
              "assigned symbol survives promotion" true
              (C.entry_symbol (List.hd (C.entries collection))
              == Option.get !symbol);
            let completed = Option.get !dimension in
            let original = completed.dimension_ast in
            Alcotest.(check bool)
              "original dimension survives promotion" true
              (D.dimension_for
                 ~table:(Session.semantic_symbols session)
                 ~ast seal original
              |> expect == completed);
            ignore (compile_execute task ast)
        | _ -> ())
      (D.observe_command ledger event)
  in
  ignore
    (parse ~checkpoint ~declaration session source ledger
    |> Test_parser.expect_ast);
  let progress = Task.progress (Option.get !task) in
  Alcotest.(check int)
    "pre-activation visits transferred once" 3 progress.dimension_work;
  Alcotest.(check int)
    "layout reuses charged dimensions" 3 progress.runtime.initializer_steps;
  Alcotest.(check int)
    "original array allocated" 16 progress.runtime.global_bytes

let rejects_foreign_and_replay () =
  let session, source, ledger = inputs "42;" in
  let checkpoint event =
    Result.map
      (fun () ->
        match event with
        | Parser.Sequence_started _ ->
            reject "a different frontend cannot adopt the source"
              (Task.adopt_source
                 (Session.task_frontend session)
                 ~source ~ledger);
            let other =
              Session.add_source session ~path:"promote.hc" ~contents:"42;"
            in
            reject "same-path input cannot adopt the source"
              (Task.adopt_source session ~source:other ~ledger);
            let task = Task.adopt_source session ~source ~ledger |> checked in
            Alcotest.(check bool)
              "exact frontend retained" true
              (Task.frontend task == session);
            reject "promotion is consumed once"
              (Task.adopt_source session ~source ~ledger)
        | _ -> ())
      (D.observe_command ledger event)
  in
  ignore (parse ~checkpoint session source ledger |> Test_parser.expect_ast)

let budget_retry () =
  let session, source, ledger = inputs "I64 A[1+1];" in
  let declaration event =
    Result.map
      (fun () ->
        match event with
        | Parser.Array_dimension_completed _ ->
            reject "one-below promotion allowance"
              (Task.adopt_source ~max_initializer_steps:2 session ~source
                 ~ledger);
            let task =
              Task.adopt_source ~max_initializer_steps:3 session ~source ~ledger
              |> checked
            in
            Alcotest.(check int)
              "failed preflight leaves source retryable" 3
              (Task.initializer_steps task)
        | _ -> ())
      (D.observe ledger event)
  in
  ignore (parse ~declaration session source ledger |> Test_parser.expect_ast)

let pending_readiness () =
  let session, source, ledger = inputs "42;" in
  let pending = ref None in
  let checkpoint event =
    Result.map
      (fun () ->
        match event with
        | Parser.Command_completed completed ->
            let task = Task.adopt_source session ~source ~ledger |> checked in
            let command =
              Task.compile_source_ast task completed.command_ast |> expect
            in
            reject "pending source command cannot execute early"
              (Task.execute task command);
            pending := Some (task, command)
        | Parser.Command_resumed _ ->
            let task, command = Option.get !pending in
            ignore (Task.execute task command |> expect);
            Alcotest.(check (option int64))
              "original resume enables execution" (Some 42L)
              (Option.map
                 (fun word -> word.VM.bits)
                 (Task.progress task).runtime.final_value)
        | _ -> ())
      (D.observe_command ledger event)
  in
  ignore (parse ~checkpoint session source ledger |> Test_parser.expect_ast)

let frozen_selection () =
  let session, source, ledger = inputs "I64 N=40;N;" in
  let prior = ref None in
  let selected = ref None in
  let task = ref None in
  let reference selection =
    Result.map
      (fun () ->
        selected := Some selection;
        let retained = Task.adopt_source session ~source ~ledger |> checked in
        task := Some retained;
        ignore (compile_execute retained (Option.get !prior).Parser.command_ast))
      (D.observe_reference ledger selection)
  in
  let checkpoint event =
    Result.map
      (fun () ->
        match event with
        | Parser.Command_resumed completed
          when completed.command_start.command_ordinal = 0 ->
            prior := Some completed
        | Parser.Command_resumed completed ->
            let ast = completed.command_ast in
            let seal = D.seal ledger ast |> expect in
            let identifier =
              Parser.selected_identifier (Option.get !selected)
            in
            let target =
              D.reference_for
                ~table:(Session.semantic_symbols session)
                ~ast seal identifier
              |> expect
            in
            (match target with
            | D.Selected_source
                { admitted = None; stage = D.Global_selection (_, Some _); _ }
              -> ()
            | _ ->
                Alcotest.fail
                  "promotion upgraded the original unadmitted selection");
            reject "later admission does not authorize an earlier frozen read"
              (Task.compile_source_ast (Option.get !task) ast)
        | _ -> ())
      (D.observe_command ledger event)
  in
  ignore
    (parse ~reference ~checkpoint session source ledger
    |> Test_parser.expect_ast)

let invalid_source_states () =
  List.iter
    (fun mode ->
      let session, source, ledger = inputs "42;" in
      let checkpoint event =
        Result.map
          (fun () ->
            match event with
            | Parser.Sequence_started _ when mode = Preprocessor.Aot ->
                reject "AOT root cannot become the task namespace"
                  (Task.adopt_source session ~source ~ledger)
            | Parser.Command_completed command when mode = Preprocessor.Jit ->
                ignore (D.seal_source ledger command.command_ast |> expect);
                reject "a source seal cannot gain runtime authority"
                  (Task.adopt_source session ~source ~ledger)
            | _ -> ())
          (D.observe_command ledger event)
      in
      ignore
        (parse ~mode ~checkpoint session source ledger |> Test_parser.expect_ast);
      reject "completed source cannot be promoted"
        (Task.adopt_source session ~source ~ledger))
    [ Preprocessor.Jit; Preprocessor.Aot ];
  let session, source, ledger = inputs "I64 Missing=" in
  let output = parse session source ledger in
  Alcotest.(check bool)
    "source actually aborted" true (Parser.has_errors output);
  reject "aborted source cannot be promoted"
    (Task.adopt_source session ~source ~ledger);
  let session, source, _ = inputs "42;" in
  let ledger = D.create session |> checked in
  let checkpoint event =
    Result.map
      (fun () ->
        match event with
        | Parser.Sequence_started _ ->
            reject "analysis ledger cannot gain source runtime authority"
              (Task.adopt_source session ~source ~ledger)
        | _ -> ())
      (D.observe_command ledger event)
  in
  ignore (parse ~checkpoint session source ledger |> Test_parser.expect_ast)

let zero_progress_runtime_states () =
  let actions =
    [
      ( "active empty stream",
        fun _ runtime _ -> ignore (VM.begin_task_stream runtime |> checked) );
      ( "aborted empty stream",
        fun _ runtime _ ->
          let stream = VM.begin_task_stream runtime |> checked in
          VM.abort_task_stream runtime stream |> checked );
      ( "uncharged isolated preparation",
        fun _ runtime _ ->
          let preparation = VM.begin_isolated_preparation runtime in
          VM.abort_isolated_preparation runtime preparation );
      ( "compiled unit without preparation",
        fun session runtime _ ->
          ignore (Test_task_declarations.compile_runtime session runtime "42;")
      );
      ( "source order without runtime work",
        fun session runtime event ->
          let other = D.create ~runtime session |> checked in
          D.observe_command other event |> expect );
      ( "failed execution preflight",
        fun session runtime _ ->
          let other =
            VM.create_task_state ~table:(Session.semantic_symbols session) ()
            |> checked
          in
          let program =
            Test_task_declarations.compile_runtime session other "42;"
          in
          reject "foreign bundle really failed preflight"
            (Test_task_declarations.execute_runtime runtime program) );
    ]
  in
  List.iter
    (fun (label, act) ->
      let session, source, ledger = inputs "42;" in
      let runtime =
        VM.create_task_state ~table:(Session.semantic_symbols session) ()
        |> checked
      in
      let checkpoint event =
        Result.map
          (fun () ->
            match event with
            | Parser.Sequence_started _ ->
                act session runtime event;
                let before = VM.task_progress runtime in
                Alcotest.(check int)
                  (label ^ " has zero runtime work")
                  0 before.executed_steps;
                Alcotest.(check int)
                  (label ^ " has zero preparation work")
                  0 before.initializer_steps;
                reject
                  (label ^ " cannot receive source authority")
                  (D.promote_source ledger ~runtime session ~source);
                Alcotest.(check bool)
                  "rejected promotion changes no progress" true
                  (before = VM.task_progress runtime);
                ignore (Task.adopt_source session ~source ~ledger |> checked)
            | _ -> ())
          (D.observe_command ledger event)
      in
      ignore (parse ~checkpoint session source ledger |> Test_parser.expect_ast))
    actions

let shares_live_stream_parser () =
  List.iter
    (fun text ->
      let session, source, ledger = inputs text in
      let retained = ref None in
      let enter span =
        let task =
          match !retained with
          | Some task -> task
          | None ->
              let task = Task.adopt_source session ~source ~ledger |> checked in
              retained := Some task;
              let detached = Session.fork_frontend session in
              let providers =
                Test_integer_task.parse detached Test_task_stream.headers
              in
              ignore
                (Task.compile_ast task providers
                |> expect |> Task.execute task |> expect);
              task
        in
        Task.stream_executor task span
      in
      let checkpoint event =
        Result.map
          (fun () ->
            match (event, !retained) with
            | Parser.Command_resumed command, Some task ->
                ignore (compile_execute task command.command_ast)
            | _ -> ())
          (D.observe_command ledger event)
      in
      let parsed =
        parse ~checkpoint ~execute_stream:enter session source ledger
      in
      ignore (Test_parser.expect_ast parsed);
      let task = Option.get !retained in
      Alcotest.(check (option int64))
        "shared stream and outer commands return 42" (Some 42L)
        (Option.map
           (fun word -> word.VM.bits)
           (Task.progress task).runtime.final_value);
      Alcotest.(check string)
        "ordinary output remains separate" "" (Task.output_bytes task))
    [
      {|#exe {StreamPrint("42;");}|};
      {|#exe {#exe {StreamPrint("StreamPrint(\"42;\");");}}|};
      {|I64 F(){#exe {StreamPrint("return 42;");}}F();|};
      {|I64 G=#exe {StreamPrint("42");};G;|};
    ]

let frozen_query () =
  let session, source, ledger = inputs "I64 A[1+1];sizeof(A);" in
  let prior = ref None in
  let query_receipt = ref None in
  let task = ref None in
  let publication = ref None in
  let declaration event =
    Result.map
      (fun () ->
        match event with
        | Parser.Global_declared entry ->
            publication :=
              Some
                ( entry.global_entry,
                  D.symbol_for ledger entry.global_entry |> Option.get )
        | _ -> ())
      (D.observe ledger event)
  in
  let query event =
    Result.map
      (fun () ->
        match event with
        | Parser.Query_completed completed ->
            query_receipt := Some completed;
            let retained =
              Task.adopt_source ~max_initializer_steps:3 session ~source ~ledger
              |> checked
            in
            task := Some retained;
            let entry, symbol = Option.get !publication in
            Alcotest.(check bool)
              "earlier publication retains its symbol" true
              (D.symbol_for ledger entry |> Option.get == symbol);
            ignore
              (compile_execute retained (Option.get !prior).Parser.command_ast)
        | _ -> ())
      (D.observe_query ledger event)
  in
  let checkpoint event =
    Result.map
      (fun () ->
        match event with
        | Parser.Command_resumed completed
          when completed.command_start.command_ordinal = 0 ->
            prior := Some completed
        | Parser.Command_resumed completed ->
            let ast = completed.command_ast in
            let seal = D.seal ledger ast |> expect in
            let receipt = Option.get !query_receipt in
            let query =
              D.query_for
                ~table:(Session.semantic_symbols session)
                ~ast seal receipt.query_expression
              |> expect
            in
            Alcotest.(check bool)
              "query keeps original completed receipt" true
              (D.query_receipt query == receipt);
            ignore (compile_execute (Option.get !task) ast)
        | _ -> ())
      (D.observe_command ledger event)
  in
  ignore
    (parse ~query ~declaration ~checkpoint session source ledger
    |> Test_parser.expect_ast);
  let progress = Task.progress (Option.get !task) in
  Alcotest.(check (option int64))
    "query reads original declared bytes" (Some 16L)
    (Option.map (fun word -> word.VM.bits) progress.runtime.final_value);
  Alcotest.(check int)
    "query and layout do not repeat dimension visits" 3
    progress.runtime.initializer_steps

let continued_dimension_budget () =
  List.iter
    (fun limit ->
      let session, source, ledger = inputs "I64 A[1+1];I64 B[1+2];" in
      let task = ref None in
      let declaration event =
        Result.map
          (fun () ->
            match (event, !task) with
            | Parser.Array_dimension_completed _, None ->
                task :=
                  Some
                    (Task.adopt_source ~max_initializer_steps:limit session
                       ~source ~ledger
                    |> checked)
            | _ -> ())
          (D.observe ledger event)
      in
      let parsed = parse ~declaration session source ledger in
      Alcotest.(check bool)
        "later dimensions use the remaining shared allowance" (limit = 5)
        (Parser.has_errors parsed);
      let progress = Task.progress (Option.get !task) in
      Alcotest.(check int)
        "every reached dimension visit remains charged" limit
        progress.runtime.initializer_steps;
      Alcotest.(check int)
        "dimension observation retains all visits" limit progress.dimension_work)
    [ 5; 6 ]

let stale_parser_lifetime () =
  List.iter
    (fun text ->
      let session, source, ledger = inputs text in
      let started = ref None in
      let checkpoint event =
        (match event with
        | Parser.Sequence_started _ -> started := Some event
        | _ -> ());
        Ok ()
      in
      let parsed = parse ~checkpoint session source ledger in
      Alcotest.(check bool)
        "actual parser success or failure" (text <> "42;")
        (Parser.has_errors parsed);
      D.observe_command ledger (Option.get !started) |> expect;
      reject "delayed start cannot revive a completed parser"
        (Task.adopt_source session ~source ~ledger))
    [ "42;"; "1+;" ]

let stale_live_checkpoint () =
  let session, source, ledger = inputs "42;" in
  let checkpoint event =
    (match event with
    | Parser.Command_started _ ->
        reject "promotion cannot skip the current live parser checkpoint"
          (Task.adopt_source session ~source ~ledger)
    | _ -> ());
    Result.map
      (fun () ->
        match event with
        | Parser.Command_started _ ->
            ignore (Task.adopt_source session ~source ~ledger |> checked)
        | _ -> ())
      (D.observe_command ledger event)
  in
  ignore (parse ~checkpoint session source ledger |> Test_parser.expect_ast)

let revoked_activation_admission () =
  let session, source, ledger = inputs "I64 A;#exe {}" in
  let publication = ref None in
  let declaration event =
    Result.map
      (fun () ->
        match event with
        | Parser.Global_declared p -> publication := Some p
        | _ -> ())
      (D.observe ledger event)
  in
  let enter span =
    let runtime =
      VM.create_task_state ~table:(Session.semantic_symbols session) ()
      |> checked
    in
    D.promote_source ledger ~runtime session ~source |> checked;
    let errors =
      [
        Diagnostic.make ~code:"TEST" ~severity:Diagnostic.Error ~primary:span
          ~message:"stop activation" ();
      ]
    in
    let result =
      D.activate_source ledger ~runtime ~span
        ~declaration:(fun _ -> Error errors)
        ~command:(fun _ -> Ok ())
    in
    reject "activation stopped before global admission" result;
    reject "revoked declaration cannot allocate storage"
      (D.admit_global ledger ~runtime (Option.get !publication));
    Alcotest.(check int)
      "revocation has no storage effect" 0
      (VM.task_progress runtime).global_bytes;
    Error errors
  in
  let output = parse ~declaration ~execute_stream:enter session source ledger in
  Alcotest.(check bool)
    "activation failure remains fatal" true (Parser.has_errors output)

let revoked_activation_command () =
  let session, source, ledger = inputs "40;2;#exe {}" in
  let first = ref None in
  let checkpoint event =
    Result.map
      (fun () ->
        match event with
        | Parser.Command_resumed command
          when command.command_start.command_ordinal = 0 ->
            first := Some command
        | _ -> ())
      (D.observe_command ledger event)
  in
  let enter span =
    let runtime =
      VM.create_task_state ~table:(Session.semantic_symbols session) ()
      |> checked
    in
    D.promote_source ledger ~runtime session ~source |> checked;
    let errors =
      [
        Diagnostic.make ~code:"TEST" ~severity:Diagnostic.Error ~primary:span
          ~message:"stop activation" ();
      ]
    in
    reject "activation stopped before command execution"
      (D.activate_source ledger ~runtime ~span
         ~declaration:(fun _ -> Ok ())
         ~command:(fun _ -> Error errors));
    let ast = (Option.get !first).Parser.command_ast in
    let declaration_command = D.seal ledger ast |> expect in
    let config =
      Preprocessor.Config.create ~compilation_mode:Jit () |> checked
    in
    let program =
      compile_integer_task_ast ~task:runtime ~declaration_command session
        ~config ast
      |> expect
    in
    reject "revoked command cannot execute"
      (Test_task_declarations.execute_runtime runtime program.value);
    Alcotest.(check int)
      "revocation has no instruction effect" 0
      (VM.task_progress runtime).executed_steps;
    Error errors
  in
  let output = parse ~checkpoint ~execute_stream:enter session source ledger in
  Alcotest.(check bool)
    "activation failure remains fatal" true (Parser.has_errors output)

let result_requires_accepted_execution () =
  List.iter
    (fun text ->
      let session, source, ledger = inputs text in
      let runtime =
        VM.create_task_state ~table:(Session.semantic_symbols session) ()
        |> checked
      in
      let sequence = ref None in
      let checkpoint event =
        Result.map
          (fun () ->
            match event with
            | Parser.Sequence_started _ ->
                D.promote_source ledger ~runtime session ~source |> checked
            | Parser.Command_resumed receipt ->
                let ast = receipt.command_ast in
                let declaration_command = D.seal ledger ast |> expect in
                let config =
                  Preprocessor.Config.create ~compilation_mode:Jit () |> checked
                in
                let program =
                  compile_integer_task_ast ~task:runtime ~declaration_command
                    session ~config ast
                  |> expect
                in
                ignore
                  (Test_task_declarations.execute_runtime runtime program.value)
            | Parser.Sequence_completed receipt ->
                sequence := Some receipt;
                reject "completion event cannot be replayed within its callback"
                  (VM.observe_task_source_event runtime event);
                reject "callback has not yet accepted source completion"
                  (VM.task_result runtime ~sequence:receipt)
            | _ -> ())
          (D.observe_command ledger event)
      in
      ignore (parse ~checkpoint session source ledger |> Test_parser.expect_ast);
      let receipt = Option.get !sequence in
      let foreign =
        VM.create_task_state ~table:(Session.semantic_symbols session) ()
        |> checked
      in
      reject "another runtime cannot claim accepted source"
        (VM.task_result foreign ~sequence:receipt);
      (match VM.task_result runtime ~sequence:receipt with
      | Ok result ->
          Alcotest.(check string)
            "only successful execution projects a result" "42;" text;
          Alcotest.(check (option int64))
            "original result" (Some 42L)
            (Option.map (fun word -> word.VM.bits) (VM.final_value result))
      | Error _ ->
          Alcotest.(check string)
            "ignored execution failure is not success" "1/0;" text);
      if text = "42;" then
        let later =
          Session.add_source session ~path:"later-root.hc" ~contents:"99;"
        in
        let checkpoint event =
          Result.map
            (fun () ->
              match event with
              | Parser.Command_started _ ->
                  reject "an older root cannot certify pending source"
                    (VM.task_result runtime ~sequence:receipt)
              | _ -> ())
            (D.observe_command ledger event)
        in
        ignore (parse ~checkpoint session later ledger |> Test_parser.expect_ast))
    [ "42;"; "1/0;" ]

let late_admission_cannot_certify_completion () =
  let session, source, ledger = inputs "42;" in
  let runtime =
    VM.create_task_state ~table:(Session.semantic_symbols session) () |> checked
  in
  let sequence = ref None in
  let checkpoint event =
    Result.map
      (fun () ->
        match event with
        | Parser.Sequence_started _ ->
            D.promote_source ledger ~runtime session ~source |> checked
        | Parser.Sequence_completed receipt -> sequence := Some receipt
        | _ -> ())
      (D.observe_command ledger event)
  in
  let ast = parse ~checkpoint session source ledger |> Test_parser.expect_ast in
  let declaration_command = D.seal ledger ast |> expect in
  let config = Preprocessor.Config.create ~compilation_mode:Jit () |> checked in
  let program =
    compile_integer_task_ast ~task:runtime ~declaration_command session ~config
      ast
    |> expect
  in
  (match Test_task_declarations.execute_runtime runtime program.value with
  | Ok _ -> ()
  | Error _ -> Alcotest.fail "late command execution failed");
  reject "post-parse admission cannot certify an earlier completion snapshot"
    (VM.task_result runtime ~sequence:(Option.get !sequence))

let tests =
  [
    Alcotest.test_case "source completion requires admission at that boundary"
      `Quick late_admission_cannot_certify_completion;
    Alcotest.test_case "source result requires accepted successful execution"
      `Quick result_requires_accepted_execution;
    Alcotest.test_case "revoked activation cannot admit a saved global" `Quick
      revoked_activation_admission;
    Alcotest.test_case "revoked activation cannot execute a saved command"
      `Quick revoked_activation_command;
    Alcotest.test_case "promotion retains original predecessor readiness" `Quick
      preserves_order;
    Alcotest.test_case "promotion retains original array preparation" `Quick
      preserves_dimension;
    Alcotest.test_case "promotion requires exact source and frontend once"
      `Quick rejects_foreign_and_replay;
    Alcotest.test_case "promotion preparation preflight permits retry" `Quick
      budget_retry;
    Alcotest.test_case "promotion preserves pending command readiness" `Quick
      pending_readiness;
    Alcotest.test_case "promotion preserves frozen unadmitted selections" `Quick
      frozen_selection;
    Alcotest.test_case "promotion rejects non-live and sealed source authority"
      `Quick invalid_source_states;
    Alcotest.test_case "zero runtime counters do not permit promotion" `Quick
      zero_progress_runtime_states;
    Alcotest.test_case "promoted source shares real stream execution" `Quick
      shares_live_stream_parser;
    Alcotest.test_case "promotion preserves selected query metadata" `Quick
      frozen_query;
    Alcotest.test_case "later dimensions consume the shared task allowance"
      `Quick continued_dimension_budget;
    Alcotest.test_case "delayed events cannot revive a finished parser" `Quick
      stale_parser_lifetime;
    Alcotest.test_case "promotion consumes the current live checkpoint" `Quick
      stale_live_checkpoint;
  ]
