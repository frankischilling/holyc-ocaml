open Holyc_lib
module VM = Ir_integer_interpreter
module C = Semantic_declaration_collection
module Record = Semantic_compiler_record
module Fragment = Holyc_lib__Sema.Dimension_fragment
module Globals = Holyc_lib__Ir.Integer_globals
module Typing = Holyc_lib__Driver.Initializer_fragment_typing
module Destination = Holyc_lib__Ir.Dimension_fragment_destination
module Lowering = Holyc_lib__Driver.Dimension_fragment_lowering
module Program = Holyc_lib__Ir.Dimension_fragment_program
module Calls = Holyc_lib__Ir.Runtime_call_context

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let diagnostics = function
  | Ok value -> value
  | Error _ -> Alcotest.fail "unexpected diagnostics"

let reject label result =
  Alcotest.(check bool) label true (Result.is_error result)

let with_dimensions callback =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let owner = VM.create_task_state ~table () |> checked in
  let foreign = VM.create_task_state ~table () |> checked in
  VM.bind_task_namespace owner namespace |> checked;
  VM.bind_task_namespace foreign namespace |> checked;
  let declaration, finish = callback table namespace owner foreign in
  let commands : Parser.command_sink =
    {
      checkpoint =
        Some
          (fun event ->
            VM.observe_task_source_event owner event |> checked;
            Ok ());
      reference = None;
      declaration =
        Some
          (fun event ->
            declaration event;
            Ok ());
      query = None;
      dimension_count = None;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let source =
    Session.add_source session ~path:"dimension-authority.hc"
      ~contents:"I64 F(){I64 A[1+1][1+2];return 42;};"
  in
  let config = Preprocessor.Config.create ~compilation_mode:Jit () |> checked in
  let parsed =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config source
  in
  Alcotest.(check bool) "source parsed" false (Parser.has_errors parsed);
  finish ()

let closed_lifetime () =
  with_dimensions (fun table namespace owner foreign ->
      let pending = ref None and completed = ref [] in
      let declaration = function
        | Parser.Array_dimension_preparing preparation ->
            let prepare () =
              VM.prepare_task_closed_dimension owner ~table ~namespace
                ~preparation ~queries:[]
            in
            let result, work = prepare () in
            let proof = checked result in
            Alcotest.(check int) "original numeric visits" 3 work;
            let duplicate, duplicate_work = prepare () in
            reject "preparation is single use" duplicate;
            Alcotest.(check int) "replay does not charge" 0 duplicate_work;
            pending := Some proof
        | Parser.Array_dimension_completed receipt ->
            let proof =
              Record.complete_dimension ~receipt (Option.get !pending)
              |> checked
            in
            reject "another task cannot complete a closed preparation"
              (VM.complete_task_dimension foreign ~namespace proof);
            VM.complete_task_dimension owner ~namespace proof |> checked;
            reject "completion is single use"
              (VM.complete_task_dimension owner ~namespace proof);
            completed := proof :: !completed
        | _ -> ()
      in
      ( declaration,
        fun () ->
          Alcotest.(check int)
            "each bound charged once" 6
            (VM.task_initializer_steps owner);
          List.iter
            (fun proof ->
              reject "completion lifetime ends with callback"
                (VM.complete_task_dimension owner ~namespace proof);
              let result, work =
                VM.prepare_task_closed_dimension owner ~table ~namespace
                  ~preparation:
                    (Record.dimension_receipt proof).dimension_preparation
                  ~queries:[]
              in
              reject "preparation lifetime ends with callback" result;
              Alcotest.(check int) "expired callback does not charge" 0 work)
            !completed ))

let runtime_lifetime () =
  with_dimensions (fun table namespace owner foreign ->
      let context =
        Typing.create_context ~table ~parent:(C.namespace_scope namespace)
        |> checked
      in
      let pending = ref None and saved = ref [] in
      let declaration = function
        | Parser.Array_dimension_preparing receipt ->
            let view = VM.task_snapshot owner |> checked in
            let fragment =
              Fragment.create ~table ~namespace ~receipt
                ~environment:(Globals.task_environment view)
                ~references:[] ~queries:[]
              |> checked
            in
            let authority = Fragment.authorize fragment |> checked in
            let attempt = VM.begin_task_dimension owner authority |> checked in
            let typed = Typing.prepare_dimension context fragment |> checked in
            let destination =
              Destination.create ~task_view:view typed |> checked
            in
            let before = VM.task_initializer_steps owner in
            let execution =
              Lowering.prepare ~context ~authority ~runtime:owner destination
              |> diagnostics
            in
            let (Program.Scheduled program) = Program.code execution in
            let retyped =
              Typing.prepare_dimension context fragment |> checked
            in
            let unrelated =
              Calls.create ~records:(Typing.records context)
                ~function_sources:(Typing.function_sources context)
                ~top_level:retyped
                ~initialization:(Program.initialization program)
                ~entry:(Program.entry program) ~entry_calls:[] ~functions:[]
              |> diagnostics
            in
            reject "program retains its exact typed dimension source"
              (Program.create ~authority ~destination
                 ~entry:(Program.entry program)
                 ~initialization:(Program.initialization program)
                 ~runtime_calls:unrelated);
            reject "execution belongs to its task"
              (VM.execute_task_dimension foreign attempt execution);
            VM.execute_task_dimension owner attempt execution |> diagnostics;
            reject "execution is single use"
              (VM.execute_task_dimension owner attempt execution);
            reject "original boundary cannot start again"
              (VM.begin_task_dimension owner authority);
            let count = Option.get (VM.task_dimension_bits owner receipt) in
            let work = VM.task_initializer_steps owner - before in
            pending := Some (receipt, count, work);
            saved := fragment :: !saved
        | Parser.Array_dimension_completed receipt ->
            let preparation, count, work = Option.get !pending in
            let propose count work =
              Record.propose_runtime_dimension ~namespace ~preparation ~count
                ~work
              |> checked
              |> Record.complete_runtime_dimension ~table ~receipt ~queries:[]
              |> checked
            in
            reject "shape must match evaluated count"
              (VM.complete_task_dimension owner ~namespace
                 (propose (Int64.succ count) work));
            reject "shape must match evaluated work"
              (VM.complete_task_dimension owner ~namespace
                 (propose count (work + 1)));
            let proof = propose count work in
            reject
              "matching metadata does not establish another task's execution"
              (VM.complete_task_dimension foreign ~namespace proof);
            VM.complete_task_dimension owner ~namespace proof |> checked
        | _ -> ()
      in
      ( declaration,
        fun () ->
          Alcotest.(check bool)
            "runtime graph actually executed" true
            (VM.task_executed_steps owner > 0);
          List.iter
            (fun fragment ->
              reject "fragment authority expires" (Fragment.authorize fragment))
            !saved ))

let missing_predecessor () =
  List.iter
    (fun skip_completion ->
      with_dimensions (fun table namespace owner _ ->
          let pending = ref None in
          let declaration = function
            | Parser.Array_dimension_preparing preparation
              when preparation.dimension_index = 0 ->
                if skip_completion then
                  pending :=
                    Some
                      (fst
                         (VM.prepare_task_closed_dimension owner ~table
                            ~namespace ~preparation ~queries:[])
                      |> checked)
            | Parser.Array_dimension_preparing preparation ->
                let result, work =
                  VM.prepare_task_closed_dimension owner ~table ~namespace
                    ~preparation ~queries:[]
                in
                reject "later bound requires completed predecessor" result;
                Alcotest.(check int)
                  "unreached bound consumes no preparation" 0 work;
                let view = VM.task_snapshot owner |> checked in
                let fragment =
                  Fragment.create ~table ~namespace ~receipt:preparation
                    ~environment:(Globals.task_environment view)
                    ~references:[] ~queries:[]
                  |> checked
                in
                let authority = Fragment.authorize fragment |> checked in
                reject "runtime bound also requires completed predecessor"
                  (VM.begin_task_dimension owner authority)
            | _ -> ()
          in
          ( declaration,
            fun () ->
              Alcotest.(check int)
                "rejected bound has no runtime effects" 0
                (VM.task_executed_steps owner) )))
    [ false; true ]

let standalone_function_dependencies () =
  List.iter
    (fun body ->
      let session = Session.create () in
      let source =
        Session.add_source session ~path:"owned-frame.hc" ~contents:body
      in
      let config =
        Preprocessor.Config.create ~compilation_mode:Jit () |> checked
      in
      let report =
        run_integer_program_report ~max_steps:10000 session ~config ~source
      in
      ignore (integer_program_report_outcome report |> diagnostics);
      let definitions =
        integer_program_report_task_units report
        |> List.concat_map integer_program_functions
        |> List.filter (fun (definition : VM.function_definition) ->
            Semantic_symbol.name (Ir_function_body.symbol definition.body) = "F")
      in
      Alcotest.(check bool)
        "unused definition was compiled" true (definitions <> []);
      List.iter
        (fun (definition : VM.function_definition) ->
          let result =
            VM.execute_function ~max_steps:100 ~max_frame_bytes:1024
              ~frame:definition.frame ~arguments:[] definition.body
          in
          match result with
          | Ok _ ->
              Alcotest.fail
                "task-owned dimension escaped into standalone execution"
          | Error errors ->
              Alcotest.(check bool)
                "dependency rejected before execution" true
                (List.exists
                   (fun error ->
                     error.VM.code = "HCIRVM0026" && error.executed_steps = 0)
                   errors))
        definitions)
    [
      "I64 N=2;I64 F(){I64 A[N];return 42;};42;";
      "I64 N=2;I64 F(){static I64 A[N];return 42;};42;";
      "I64 N=2;I64 A[N];I64 F(){return sizeof A+26;};42;";
    ]

let () =
  Alcotest.run "runtime dimension authority"
    [
      ( "dimensions",
        [
          Alcotest.test_case "closed preparation and completion lifetime" `Quick
            closed_lifetime;
          Alcotest.test_case "runtime outcomes own their shape" `Quick
            runtime_lifetime;
          Alcotest.test_case "predecessor completion orders both evaluators"
            `Quick missing_predecessor;
          Alcotest.test_case "standalone functions retain extent dependencies"
            `Quick standalone_function_dependencies;
        ] );
    ]
