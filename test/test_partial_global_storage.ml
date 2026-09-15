open Holyc_lib
module D = Task_declarations
module Task = Integer_task
module VM = Ir_integer_interpreter

let checked = Test_declaration_collection.checked
let expect = Test_integer_program.checked

let run_optional ?(max_global_bytes = 16) ?(admit = fun _ -> true)
    ?(on_admitted = fun _ _ -> ()) text =
  let session, source, ledger = Test_source_promotion.inputs text in
  let retained = ref None in
  let declarations = ref [] in
  let task () =
    match !retained with
    | Some task -> task
    | None ->
        let task =
          Task.adopt_source ~max_global_bytes session ~source ~ledger |> checked
        in
        retained := Some task;
        task
  in
  let declaration event =
    Result.bind (D.observe ledger event) (fun () ->
        match event with
        | Parser.Global_declared publication ->
            declarations := publication :: !declarations;
            if admit publication then
              Result.map
                (fun () -> on_admitted (task ()) publication)
                (Task.admit_global (task ()) publication)
            else Ok ()
        | _ -> Ok ())
  in
  let installed = ref false in
  let enter span =
    let task = task () in
    if not !installed then (
      let detached = Session.fork_frontend session in
      let providers =
        Test_integer_task.parse detached Test_task_stream.headers
      in
      ignore
        (Task.compile_ast task providers
        |> expect |> Task.execute task |> expect);
      installed := true);
    Task.stream_executor task span
  in
  let checkpoint event =
    Result.bind (D.observe_command ledger event) (fun () ->
        match (event, !retained) with
        | Parser.Command_resumed completed, Some task ->
            Result.bind (Task.compile_source_ast task completed.command_ast)
              (fun command -> Result.map ignore (Task.execute task command))
        | _ -> Ok ())
  in
  let parsed =
    Test_source_promotion.parse ~declaration ~checkpoint ~execute_stream:enter
      session source ledger
  in
  (parsed, !retained, List.rev !declarations)

let run ?max_global_bytes ?admit ?on_admitted text =
  let parsed, task, publications =
    run_optional ?max_global_bytes ?admit ?on_admitted text
  in
  (parsed, Option.get task, publications)

let initialized_array () =
  let parsed, task, _ =
    run {|I64 A[2]={#exe {A[1]=42;StreamPrint("%d",A[1]);},0};A[0]+A[1];|}
  in
  ignore (Test_parser.expect_ast parsed);
  let progress = (Task.progress task).runtime in
  Alcotest.(check (option int64))
    "nested write/read returns 42" (Some 42L)
    (Option.map (fun word -> word.VM.bits) progress.final_value);
  Alcotest.(check int) "one original allocation" 16 progress.global_bytes

let resumed_uninitialized () =
  let parsed, task, _ = run {|I64 A[2];#exe {A[1]=42;}A[1];|} in
  ignore (Test_parser.expect_ast parsed);
  let progress = (Task.progress task).runtime in
  Alcotest.(check (option int64))
    "resume preserves the earlier write" (Some 42L)
    (Option.map (fun word -> word.VM.bits) progress.final_value);
  Alcotest.(check int) "no duplicate byte charge" 16 progress.global_bytes

let scalar_and_copy () =
  List.iter
    (fun (text, bytes) ->
      let parsed, task, _ = run ~max_global_bytes:bytes text in
      ignore (Test_parser.expect_ast parsed);
      let progress = (Task.progress task).runtime in
      Alcotest.(check (option int64))
        "completed payload reaches original storage" (Some 42L)
        (Option.map (fun word -> word.VM.bits) progress.final_value);
      Alcotest.(check int)
        "one scalar or aggregate allocation" bytes progress.global_bytes)
    [
      ({|I64 N=#exe {N=1;StreamPrint("42");};N;|}, 8);
      ( {|U8 A[2][3]={#exe {A[1][2]=42;StreamPrint("\"AB\"");},"CD"};A[0][0]-23;|},
        6 );
      ({|I64 A[2][2]={#exe {A[1][1]=42;StreamPrint("42");},0,0,0};A[0][0];|}, 32);
    ]

let mixed_allocations () =
  List.iter
    (fun partial_b ->
      let admit (publication : Parser.global_publication) =
        partial_b || publication.global_name.spelling <> "B"
      in
      let parsed, task, _ =
        run ~admit ~max_global_bytes:24
          {|I64 A[2]={#exe {A[1]=2;StreamPrint("2");},0},B=40;A[0]+B;|}
      in
      ignore (Test_parser.expect_ast parsed);
      let progress = (Task.progress task).runtime in
      Alcotest.(check (option int64))
        "fresh arena does not take ownership of reused A" (Some 42L)
        (Option.map (fun word -> word.VM.bits) progress.final_value);
      Alcotest.(check int)
        "each original object is charged once" 24 progress.global_bytes)
    [ true; false ]

let unknown_read () =
  let parsed, task, _ =
    run ~max_global_bytes:8 {|I64 N=#exe {StreamPrint("%d",N);};|}
  in
  Alcotest.(check bool)
    "nested read reaches the VM unknown-cell fault" true
    (List.exists
       (fun (diagnostic : Diagnostic.t) -> diagnostic.code = "HCIRVM0012")
       parsed.diagnostics);
  let progress = (Task.progress task).runtime in
  Alcotest.(check int)
    "fault retains the admitted original object" 8 progress.global_bytes;
  Alcotest.(check int)
    "failed print generates no text" 0 progress.generated_bytes;
  Alcotest.(check bool)
    "nested instructions really ran" true
    (progress.executed_steps > 0)

let preflight_failure () =
  List.iter
    (fun (text, limit) ->
      let parsed, task, _ = run_optional ~max_global_bytes:limit text in
      Alcotest.(check bool)
        "invalid allocation is rejected before initializer effects" true
        (Parser.has_errors parsed);
      Option.iter
        (fun task ->
          let progress = (Task.progress task).runtime in
          Alcotest.(check int)
            "no failed allocation charge" 0 progress.global_bytes;
          Alcotest.(check int)
            "no command starts after failed allocation" 0
            progress.executed_steps;
          Alcotest.(check int)
            "no generation after failed allocation" 0 progress.generated_bytes)
        task)
    [
      ({|I64 A[2]={#exe {StreamPrint("42");},0};|}, 15);
      ("I64 A[0];", 16);
      ("I64 A[];", 16);
      ("U0 A;", 16);
      ("I64 *A;", 16);
      ("_I64 A;", 16);
      ("static I64 A;", 16);
      ("extern I64 A;", 16);
    ]

let replay_and_foreign () =
  let on_admitted task publication =
    let before = Task.progress task in
    Alcotest.(check bool)
      "live original publication cannot allocate twice" true
      (Result.is_error (Task.admit_global task publication));
    Alcotest.(check bool)
      "replay changes no runtime progress" true
      (before = Task.progress task);
    let other = Task.create (Task.frontend task) |> checked in
    let before = Task.progress other in
    Alcotest.(check bool)
      "same-name foreign task has no publication authority" true
      (Result.is_error (Task.admit_global other publication));
    Alcotest.(check bool)
      "foreign task remains untouched" true
      (before = Task.progress other)
  in
  let parsed, task, publications = run ~on_admitted "I64 A[2];" in
  ignore (Test_parser.expect_ast parsed);
  let before = Task.progress task in
  Alcotest.(check bool)
    "finished parser cannot rematerialize a declaration" true
    (Result.is_error (Task.admit_global task (List.hd publications)));
  Alcotest.(check bool)
    "finished replay changes no runtime progress" true
    (before = Task.progress task)

let delayed_publication () =
  let session, source, ledger = Test_source_promotion.inputs "I64 A=42;" in
  let saved = ref None in
  let declaration event =
    match event with
    | Parser.Global_declared publication ->
        saved := Some event;
        Alcotest.(check bool)
          "publication owns its synchronous callback" true
          (Parser.global_publication_is_current publication);
        Ok ()
    | Parser.Global_initializer_started _ ->
        D.observe ledger (Option.get !saved)
    | _ -> D.observe ledger event
  in
  let parsed = Test_source_promotion.parse ~declaration session source ledger in
  Alcotest.(check bool)
    "delayed publication stops the source" true (Parser.has_errors parsed);
  match Option.get !saved with
  | Parser.Global_declared publication ->
      Alcotest.(check bool)
        "callback authority was revoked" false
        (Parser.global_publication_is_current publication)
  | _ -> assert false

let raw_foreign_namespace () =
  let session, runtime, ledger = Test_task_declarations.runtime_setup () in
  let namespace =
    Semantic_declaration_collection.create_namespace
      ~table:(Session.semantic_symbols session)
      ()
    |> checked
  in
  Alcotest.(check bool)
    "task namespace cannot be replaced" true
    (Result.is_error (VM.bind_task_namespace runtime namespace));
  Alcotest.(check bool)
    "second ledger cannot attach to the same runtime" true
    (Result.is_error (D.create ~runtime session));
  let saved = ref None in
  let observe event =
    Result.bind (D.observe ledger event) (fun () ->
        match event with
        | Parser.Global_declared source ->
            let publication =
              Semantic_declaration_collection.publish_global namespace source
              |> checked
            in
            let declaration =
              Semantic_compiler_record.declare_global ~dimensions:[]
                ~predecessor:None ~previous_global:None
                ~table:(Session.semantic_symbols session)
                ~namespace publication
              |> checked
            in
            saved := Some declaration;
            let before = VM.task_progress runtime in
            Alcotest.(check bool)
              "another namespace cannot admit source storage" true
              (Result.is_error (VM.admit_declared_global runtime declaration));
            Alcotest.(check bool)
              "foreign certificate preserves runtime" true
              (before = VM.task_progress runtime);
            Ok ()
        | _ -> Ok ())
  in
  let parsed, _ =
    Test_task_declarations.parse ~observe session ledger "I64 A;"
  in
  ignore (Test_parser.expect_ast parsed);
  Alcotest.(check bool)
    "saved foreign certificate stays rejected" true
    (Result.is_error (VM.admit_declared_global runtime (Option.get !saved)))

let required_predecessors () =
  List.iter
    (fun text ->
      let admit (publication : Parser.global_publication) =
        publication.global_name.spelling <> "A"
      in
      let parsed, task, _ = run ~admit text in
      Alcotest.(check bool)
        "missing original predecessor stops admission" true
        (Parser.has_errors parsed);
      let progress = (Task.progress task).runtime in
      Alcotest.(check int)
        "no allocation before predecessors" 0 progress.global_bytes;
      Alcotest.(check int)
        "no runtime effects before predecessors" 0 progress.executed_steps)
    [ "40; I64 B;"; "I64 A,B;" ]

let deferred_storage () =
  let session, source, ledger = Test_source_promotion.inputs "I64 N=42;N;" in
  let task = ref None in
  let declaration event =
    Result.bind (D.observe ledger event) (fun () ->
        match event with
        | Parser.Global_initializer_started start ->
            Alcotest.(check bool)
              "publication callback has already ended" false
              (Parser.global_publication_is_current start.initializer_owner);
            let retained =
              Task.adopt_source ~max_global_bytes:8 session ~source ~ledger
              |> checked
            in
            task := Some retained;
            Task.admit_global retained start.initializer_owner
        | _ -> Ok ())
  in
  let checkpoint event =
    Result.bind (D.observe_command ledger event) (fun () ->
        match (event, !task) with
        | Parser.Command_resumed completed, Some task ->
            Result.bind (Task.compile_source_ast task completed.command_ast)
              (fun command -> Result.map ignore (Task.execute task command))
        | _ -> Ok ())
  in
  let parsed =
    Test_source_promotion.parse ~declaration ~checkpoint session source ledger
  in
  ignore (Test_parser.expect_ast parsed);
  let progress = (Task.progress (Option.get !task)).runtime in
  Alcotest.(check int)
    "retained boundary admits one allocation" 8 progress.global_bytes;
  Alcotest.(check (option int64))
    "completion initializes original storage" (Some 42L)
    (Option.map (fun word -> word.VM.bits) progress.final_value)

let raw_context_lifetime () =
  List.iter
    (fun admit_live ->
      let session = Session.task_frontend (Session.create ()) in
      let table = Session.semantic_symbols session in
      let runtime = VM.create_task_state ~table () |> checked in
      let ledger = D.create session |> checked in
      let namespace =
        Semantic_declaration_collection.create_namespace ~table () |> checked
      in
      let events = ref [] in
      let saved = ref None in
      let rejection label =
        let before = VM.task_progress runtime in
        match VM.admit_declared_global runtime (Option.get !saved) with
        | Error message ->
            Alcotest.(check string)
              label "declared storage source context is no longer current"
              message;
            Alcotest.(check bool)
              "context rejection changes no progress" true
              (before = VM.task_progress runtime)
        | Ok () -> Alcotest.fail label
      in
      let checkpoint event =
        events := event :: !events;
        Result.map
          (fun () ->
            match event with
            | Parser.Command_completed _ when not admit_live ->
                rejection "missed current checkpoint"
            | _ -> ())
          (D.observe_command ledger event)
      in
      let observe event =
        Result.map
          (fun () ->
            match event with
            | Parser.Global_declared source ->
                let publication =
                  Semantic_declaration_collection.publish_global namespace
                    source
                  |> checked
                in
                let declaration =
                  Semantic_compiler_record.declare_global ~dimensions:[] ~table
                    ~namespace ~predecessor:None ~previous_global:None
                    publication
                  |> checked
                in
                saved := Some declaration;
                VM.promote_task_source runtime ~namespace
                  ~events:(List.rev !events) ~dimension_steps:0
                |> checked;
                if admit_live then
                  VM.admit_declared_global runtime declaration |> checked
            | _ -> ())
          (D.observe ledger event)
      in
      let parsed, _ =
        Test_task_declarations.parse ~observe ~checkpoint session ledger
          "I64 A;"
      in
      ignore (Test_parser.expect_ast parsed);
      if not admit_live then rejection "completed parser cannot admit storage";
      Alcotest.(check int)
        "only the live original context allocates"
        (if admit_live then 8 else 0)
        (VM.task_progress runtime).global_bytes)
    [ false; true ]

let tests =
  [
    Alcotest.test_case "nested write in an open initializer" `Quick
      initialized_array;
    Alcotest.test_case "write before declaration resume" `Quick
      resumed_uninitialized;
    Alcotest.test_case "scalar and copied initializer payloads" `Quick
      scalar_and_copy;
    Alcotest.test_case "mixed retained and fresh allocations" `Quick
      mixed_allocations;
    Alcotest.test_case "unknown storage fault preserves admission" `Quick
      unknown_read;
    Alcotest.test_case "allocation preflight stops initializer effects" `Quick
      preflight_failure;
    Alcotest.test_case "replay and foreign tasks do not allocate" `Quick
      replay_and_foreign;
    Alcotest.test_case "delayed publication cannot mint a boundary" `Quick
      delayed_publication;
    Alcotest.test_case "raw admission rejects another namespace" `Quick
      raw_foreign_namespace;
    Alcotest.test_case "original command and global predecessors are required"
      `Quick required_predecessors;
    Alcotest.test_case "promotion consumes a retained publication boundary"
      `Quick deferred_storage;
    Alcotest.test_case "raw admission checks current parser lifetime" `Quick
      raw_context_lifetime;
  ]
