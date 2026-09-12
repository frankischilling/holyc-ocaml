open Holyc_lib
module VM = Ir_integer_interpreter
module C = Semantic_declaration_collection
module Record = Semantic_compiler_record
module Fragment = Holyc_lib__Sema.Offset_fragment
module Globals = Holyc_lib__Ir.Integer_globals
module Typing = Holyc_lib__Driver.Initializer_fragment_typing
module Destination = Holyc_lib__Ir.Offset_fragment_destination
module Lowering = Holyc_lib__Driver.Offset_fragment_lowering
module Program = Holyc_lib__Ir.Offset_fragment_program
module Calls = Holyc_lib__Ir.Runtime_call_context

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let diagnostics = function
  | Ok value -> value
  | Error _ -> Alcotest.fail "unexpected diagnostics"

let reject label result =
  Alcotest.(check bool) label true (Result.is_error result)

let with_offset ?max_initializer_steps callback =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let owner =
    VM.create_task_state ?max_initializer_steps ~table () |> checked
  in
  let foreign = VM.create_task_state ~table () |> checked in
  VM.bind_task_namespace owner namespace |> checked;
  VM.bind_task_namespace foreign namespace |> checked;
  let reached, finish = callback table namespace owner foreign in
  let progress = ref None in
  let declaration = function
    | Parser.Aggregate_declared source ->
        let publication = C.publish_aggregate namespace source |> checked in
        progress :=
          Some (Record.begin_aggregate ~table ~namespace publication |> checked)
    | Parser.Aggregate_advanced phase ->
        let current = Option.get !progress in
        (match phase.phase_step with
        | Parser.Aggregate_offset_reached _ -> reached current phase
        | _ -> ());
        Record.advance_aggregate ~dimensions:(fun _ -> None) current phase
        |> checked
    | _ -> ()
  in
  let commands : Parser.command_sink =
    {
      checkpoint =
        Some
          (fun event ->
            VM.observe_task_source_event owner event |> checked;
            Ok ());
      call = None;
      implicit_output = None;
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
    Session.add_source session ~path:"offset-authority.hc"
      ~contents:"class Span {$$=1+7;};"
  in
  let config = Preprocessor.Config.create ~compilation_mode:Jit () |> checked in
  let parsed =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config source
  in
  Alcotest.(check bool) "source parsed" false (Parser.has_errors parsed);
  finish ()

let fragment table namespace owner progress receipt =
  let view = VM.task_snapshot owner |> checked in
  let fragment =
    Fragment.create ~table ~namespace ~progress ~receipt
      ~environment:(Globals.task_environment view)
      ~references:[] ~queries:[]
    |> checked
  in
  let authority = Fragment.authorize fragment |> checked in
  let attempt = VM.begin_task_offset owner authority |> checked in
  let context =
    Typing.create_context ~table ~parent:(C.namespace_scope namespace)
    |> checked
  in
  let typed = Typing.prepare_offset context fragment |> checked in
  let destination = Destination.create ~task_view:view typed |> checked in
  (fragment, authority, attempt, context, destination)

let execution_lifetime () =
  with_offset (fun table namespace owner foreign ->
      let saved = ref None in
      let reached progress receipt =
        let fragment, authority, attempt, context, destination =
          fragment table namespace owner progress receipt
        in
        reject "another runtime cannot begin the source attempt"
          (VM.begin_task_offset foreign authority);
        let foreign_view = VM.task_snapshot foreign |> checked in
        reject "destination cannot borrow another snapshot"
          (Destination.create ~task_view:foreign_view
             (Destination.typed destination));
        let execution =
          Lowering.prepare ~context ~authority ~runtime:owner destination
          |> diagnostics
        in
        let (Program.Scheduled program) = Program.code execution in
        let module Lower = Holyc_lib__Ir.Integer_program_lowering in
        let unrelated_lowering =
          Lower.lower_complete
            ~globals:(Destination.globals destination)
            ~span:(Destination.span destination)
            [ Lower.Empty (Destination.span destination) ]
          |> diagnostics
        in
        reject "original graph cannot borrow another lowering's source proof"
          (Program.create ~authority ~destination ~lowered:unrelated_lowering
             ~entry:(Program.entry program)
             ~initialization:(Program.initialization program)
             ~runtime_calls:(Program.runtime_calls program));
        let retyped = Typing.prepare_offset context fragment |> checked in
        let unrelated =
          Calls.create ~records:(Typing.records context)
            ~function_sources:(Typing.function_sources context)
            ~top_level:retyped
            ~initialization:(Program.initialization program)
            ~entry:(Program.entry program) ~entry_calls:[] ~functions:[]
          |> diagnostics
        in
        reject "program retains exact typed offset source"
          (Program.create ~authority ~destination
             ~lowered:(Program.lowered program) ~entry:(Program.entry program)
             ~initialization:(Program.initialization program)
             ~runtime_calls:unrelated);
        let wrong_work =
          Program.prepare ~authority ~destination ~code:(Program.code execution)
            ~steps:(Program.steps execution + 1)
          |> checked
        in
        reject "execution rejects substituted preparation count"
          (VM.execute_task_offset owner attempt wrong_work);
        reject "execution cannot borrow a task"
          (VM.execute_task_offset foreign attempt execution);
        Alcotest.(check int)
          "foreign preflight has no runtime effects" 0
          (VM.task_executed_steps foreign);
        VM.execute_task_offset owner attempt execution |> diagnostics;
        reject "execution cannot replay"
          (VM.execute_task_offset owner attempt execution);
        reject "boundary cannot begin again"
          (VM.begin_task_offset owner authority);
        reject "semantic preparation cannot be authorized twice"
          (Fragment.authorize fragment);
        let offset = Option.get (VM.task_offset owner receipt) in
        Alcotest.(check int64)
          "actual offset value" 8L
          (Record.aggregate_offset_value offset);
        reject "runtime metadata cannot authorize isolated execution"
          (VM.charge_isolated_aggregate_offsets foreign ~table [ offset ]);
        saved := Some (fragment, attempt, execution)
      in
      ( reached,
        fun () ->
          let fragment, attempt, execution = Option.get !saved in
          reject "source authority expires" (Fragment.authorize fragment);
          reject "execution expires"
            (VM.execute_task_offset owner attempt execution) ))

let metadata_is_not_execution () =
  with_offset (fun table namespace owner foreign ->
      let reached progress receipt =
        let _, authority, attempt, context, destination =
          fragment table namespace owner progress receipt
        in
        let execution =
          Lowering.prepare ~context ~authority ~runtime:owner destination
          |> diagnostics
        in
        let metadata =
          Record.finish_runtime_aggregate_offset
            (Fragment.preparation authority)
            ~value:8L ~work:(Program.steps execution)
          |> checked
        in
        reject "metadata consumed the preparation without executing it"
          (VM.execute_task_offset owner attempt execution);
        VM.fail_task_offset owner attempt |> checked;
        Alcotest.(check bool)
          "matching metadata is not a runtime result" true
          (VM.task_offset owner receipt = None);
        reject "another runtime cannot charge matching bits as source work"
          (VM.charge_isolated_aggregate_offsets foreign ~table [ metadata ]);
        Alcotest.(check int)
          "no fabricated execution" 0
          (VM.task_executed_steps owner)
      in
      (reached, fun () -> ()))

let failed_preparation () =
  with_offset ~max_initializer_steps:2 (fun table namespace owner _ ->
      let reached progress receipt =
        let fragment, authority, attempt, context, destination =
          fragment table namespace owner progress receipt
        in
        reject "preparation cannot exceed remaining allowance"
          (Lowering.prepare ~context ~authority ~runtime:owner destination);
        VM.fail_task_offset owner attempt |> checked;
        Alcotest.(check int)
          "failed preparation keeps work" 2
          (VM.task_initializer_steps owner);
        Alcotest.(check int)
          "failed preparation never runs graph" 0
          (VM.task_executed_steps owner);
        reject "failed preparation consumes source phase"
          (Fragment.authorize fragment);
        reject "failed attempt cannot restart"
          (VM.begin_task_offset owner authority);
        Alcotest.(check bool)
          "failed attempt has no offset" true
          (VM.task_offset owner receipt = None)
      in
      (reached, fun () -> ()))

let completion_is_single_use () =
  let module Ledger = Holyc_lib__Driver.Task_declarations in
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let runtime = VM.create_task_state ~table () |> checked in
  let ledger = Ledger.create ~runtime session |> checked in
  let completed = ref false in
  let declaration = function
    | Parser.Aggregate_advanced
        ({ phase_step = Parser.Aggregate_offset_reached _; _ } as phase) ->
        let view = VM.task_snapshot runtime |> checked in
        let before = VM.task_initializer_steps runtime in
        let authority, attempt =
          Ledger.begin_runtime_offset ledger ~runtime ~task_view:view phase
          |> diagnostics
        in
        let context =
          Typing.create_context ~table ~parent:(Ledger.initializer_scope ledger)
          |> checked
        in
        let typed =
          Typing.prepare_offset context (Fragment.authorized_fragment authority)
          |> checked
        in
        let destination = Destination.create ~task_view:view typed |> checked in
        let execution =
          Lowering.prepare ~context ~authority ~runtime destination
          |> diagnostics
        in
        VM.execute_task_offset runtime attempt execution |> diagnostics;
        reject "completion cannot replace its starting counter"
          (Ledger.finish_runtime_offset ledger ~runtime ~before:(before + 1)
             ~succeeded:true phase);
        Ledger.finish_runtime_offset ledger ~runtime ~before ~succeeded:true
          phase
        |> diagnostics;
        let work = Ledger.offset_work ledger in
        reject "ledger completion cannot replay"
          (Ledger.finish_runtime_offset ledger ~runtime ~before ~succeeded:true
             phase);
        Alcotest.(check int)
          "replayed completion does not charge" work
          (Ledger.offset_work ledger);
        completed := true;
        Ok ()
    | event -> Ledger.observe ledger event
  in
  let commands : Parser.command_sink =
    {
      checkpoint = Some (Ledger.observe_command ledger);
      call = None;
      implicit_output = None;
      reference = None;
      query = None;
      declaration = Some declaration;
      dimension_count = None;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let source =
    Session.add_source session ~path:"offset-completion.hc"
      ~contents:"class Span {$$=1+7;};"
  in
  let config = Preprocessor.Config.create ~compilation_mode:Jit () |> checked in
  ignore
    (Parser.parse ~commands ~sources:(Session.sources session)
       ~definitions:(Session.definitions session)
       ~symbols:(Session.symbols session) ~config source);
  Alcotest.(check bool) "completion callback reached" true !completed

let standalone_dependencies () =
  List.iter
    (fun tail ->
      let session = Session.create () in
      let source =
        Session.add_source session ~path:"owned-offset.hc"
          ~contents:
            ("#exe {I64 N=8;class Span {$$=N;I64 x;};" ^ tail
           ^ "StreamPrint(\"42;\");}")
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
      Alcotest.(check bool) "function actually compiled" true (definitions <> []);
      List.iter
        (fun (definition : VM.function_definition) ->
          match
            VM.execute_function ~max_steps:100 ~max_frame_bytes:1024
              ~frame:definition.frame ~arguments:[] definition.body
          with
          | Ok _ ->
              Alcotest.fail
                "runtime offset escaped its task through derived layout"
          | Error errors ->
              Alcotest.(check bool)
                "dependency rejected before execution" true
                (List.exists
                   (fun error ->
                     error.VM.code = "HCIRVM0026" && error.executed_steps = 0)
                   errors))
        definitions)
    [
      "I64 F(){return sizeof(Span)+26;};";
      "I64 F(){U8 A[sizeof(Span)];return 42;};";
      "I64 F(){static U8 A[sizeof(Span)];return 42;};";
      "U8 A[sizeof(Span)];I64 F(){return sizeof(A)+26;};";
      "class B {$$=sizeof(Span);};I64 F(){return sizeof(B)+26;};";
      "class B {U8 data[sizeof(Span)];};I64 F(){return sizeof(B)+26;};";
    ]

let () =
  Alcotest.run "runtime offset authority"
    [
      ( "offsets",
        [
          Alcotest.test_case "typed execution owns original offset result"
            `Quick execution_lifetime;
          Alcotest.test_case "matching metadata is not execution" `Quick
            metadata_is_not_execution;
          Alcotest.test_case "failure consumes original bounded attempt" `Quick
            failed_preparation;
          Alcotest.test_case "derived layouts retain runtime dependencies"
            `Quick standalone_dependencies;
          Alcotest.test_case "ledger completion charges once" `Quick
            completion_is_single_use;
        ] );
    ]
