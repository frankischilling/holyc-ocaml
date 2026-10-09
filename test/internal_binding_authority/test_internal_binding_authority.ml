open Holyc_lib
module VM = Ir_integer_interpreter
module C = Semantic_declaration_collection
module N = Semantic_function_record_phase
module Prepared = Holyc_lib__Sema.Prepared_internal_binding
module Fragment = Holyc_lib__Sema.Internal_binding_fragment
module Globals = Holyc_lib__Ir.Integer_globals
module Typing = Holyc_lib__Driver.Initializer_fragment_typing
module Destination = Holyc_lib__Ir.Internal_binding_fragment_destination
module Lowering = Holyc_lib__Driver.Internal_binding_fragment_lowering
module Program = Holyc_lib__Ir.Internal_binding_fragment_program
module Calls = Holyc_lib__Ir.Runtime_call_context
module Lower = Holyc_lib__Ir.Integer_program_lowering
module Initialization = Holyc_lib__Ir.Global_initialization

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let diagnostics = function
  | Ok value -> value
  | Error _ -> Alcotest.fail "unexpected diagnostics"

let reject label result =
  Alcotest.(check bool) label true (Result.is_error result)

let original_execution ?(contents = "_intern (0x10+0xe) I64 Convert(U8 ch);")
    ?(work = 5) ?(steps = 5) () =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let foreign_namespace = C.create_namespace ~table () |> checked in
  let owner = VM.create_task_state ~table () |> checked in
  let foreign = VM.create_task_state ~table () |> checked in
  VM.bind_task_namespace owner namespace |> checked;
  VM.bind_task_namespace foreign namespace |> checked;
  let registry =
    N.create_registry ~mode:Preprocessor.Jit ~table ~namespace |> checked
  in
  let missing_registry =
    N.create_registry ~mode:Preprocessor.Jit ~table ~namespace |> checked
  in
  let copied_registry =
    N.create_registry ~mode:Preprocessor.Jit ~table ~namespace |> checked
  in
  let foreign_registry =
    N.create_registry ~mode:Preprocessor.Jit ~table ~namespace:foreign_namespace
    |> checked
  in
  let saved = ref None and original = ref None and copied = ref None in
  let foreign_target = ref None and native = ref None in
  let declaration event =
    (match event with
    | Parser.Internal_binding_preparing receipt ->
        let view = VM.task_snapshot owner |> checked in
        let fragment =
          Fragment.create ~table ~namespace ~receipt
            ~environment:(Globals.task_environment view)
            ~references:[] ~queries:[]
          |> checked
        in
        let authority = Fragment.authorize fragment |> checked in
        reject "foreign task lacks the original source boundary"
          (VM.begin_task_internal_binding foreign authority);
        let attempt =
          VM.begin_task_internal_binding owner authority |> checked
        in
        let context =
          Typing.create_context ~table ~parent:(C.namespace_scope namespace)
          |> checked
        in
        let typed =
          Typing.prepare_internal_binding context fragment |> checked
        in
        let destination = Destination.create ~task_view:view typed |> checked in
        let foreign_view = VM.task_snapshot foreign |> checked in
        reject "destination keeps its exact retained environment"
          (Destination.create ~task_view:foreign_view typed);
        let execution =
          Lowering.prepare ~context ~authority ~runtime:owner destination
          |> diagnostics
        in
        let (Program.Scheduled program) = Program.code execution in
        let retyped =
          Typing.prepare_internal_binding context fragment |> checked
        in
        let unrelated =
          Calls.create ~records:(Typing.records context)
            ~function_sources:(Typing.function_sources context)
            ~top_level:retyped
            ~initialization:(Program.initialization program)
            ~entry:(Program.entry program) ~entry_calls:[] ~functions:[]
          |> diagnostics
        in
        reject "program keeps its exact typed source"
          (Program.create ~authority ~destination
             ~lowered:(Program.lowering program) ~entry:(Program.entry program)
             ~initialization:(Program.initialization program)
             ~runtime_calls:unrelated);
        let globals = Destination.globals destination in
        let span = Destination.span destination in
        let unrelated_lowering =
          Lower.lower_complete ~globals ~records:(Typing.records context)
            ~top_calls:[] ~span [ Lower.Empty span ]
          |> diagnostics
        in
        let unrelated_entry = Lower.graph unrelated_lowering in
        let unrelated_initialization =
          Initialization.create ~span ~globals ~entry:unrelated_entry []
          |> diagnostics
        in
        let unrelated_calls =
          Calls.create ~records:(Typing.records context)
            ~function_sources:(Typing.function_sources context)
            ~top_level:typed ~initialization:unrelated_initialization
            ~entry:unrelated_entry ~entry_calls:[] ~functions:[]
          |> diagnostics
        in
        reject "matching source metadata cannot authorize unrelated IR"
          (Program.create ~authority ~destination ~lowered:unrelated_lowering
             ~entry:unrelated_entry ~initialization:unrelated_initialization
             ~runtime_calls:unrelated_calls);
        reject "original lowering cannot authorize another graph"
          (Program.create ~authority ~destination
             ~lowered:(Program.lowering program) ~entry:unrelated_entry
             ~initialization:unrelated_initialization
             ~runtime_calls:unrelated_calls);
        reject "execution belongs to its owning task"
          (VM.execute_task_internal_binding foreign attempt execution);
        VM.execute_task_internal_binding owner attempt execution |> diagnostics;
        reject "original execution cannot replay"
          (VM.execute_task_internal_binding owner attempt execution);
        reject "original boundary cannot begin again"
          (VM.begin_task_internal_binding owner authority);
        let target =
          Option.get (VM.task_internal_binding_target owner receipt)
        in
        Alcotest.(check int64) "evaluated operation" 30L (Prepared.bits target);
        Alcotest.(check int)
          "original preparation work" work (Prepared.work target);
        Alcotest.(check int)
          "actual expression instructions" steps
          (VM.task_executed_steps owner);
        let copy namespace =
          Prepared.create ~table ~namespace ~receipt
            ~bits:(Prepared.bits target) ~work:(Prepared.work target)
          |> checked
        in
        original := Some target;
        copied := Some (copy namespace);
        foreign_target := Some (copy foreign_namespace);
        saved := Some (fragment, authority, attempt, execution)
    | Parser.Function_declared source ->
        let publication = C.publish_function namespace source |> checked in
        let missing =
          N.begin_header missing_registry publication source |> checked
        in
        reject
          "literal or arithmetic source alone grants no preparation authority"
          (VM.check_function_phase_source owner ~namespace ~event
             (N.snapshot missing));
        let record =
          N.begin_header ~internal_target:(Option.get !original) registry
            publication source
          |> checked
        in
        native := Some record;
        VM.check_function_phase_source owner ~namespace ~event
          (N.snapshot record)
        |> checked;
        let reconstructed =
          N.begin_header ~internal_target:(Option.get !copied) copied_registry
            publication source
          |> checked
        in
        reject "equal source facts grant no runtime preparation authority"
          (VM.check_function_phase_source owner ~namespace ~event
             (N.snapshot reconstructed));
        reject "foreign task cannot admit another task's saved value"
          (VM.check_function_phase_source foreign ~namespace ~event
             (N.snapshot record));
        let foreign_publication =
          C.publish_function foreign_namespace source |> checked
        in
        reject "source target cannot cross namespaces"
          (N.begin_header ~internal_target:(Option.get !original)
             foreign_registry foreign_publication source);
        reject "foreign target cannot enter the original native namespace"
          (N.begin_header
             ~internal_target:(Option.get !foreign_target)
             copied_registry publication source)
    | Parser.Function_header_completed header ->
        let record = Option.get !native in
        N.observe record event |> checked;
        let snapshot = N.snapshot record in
        Alcotest.(check bool)
          "installed value is the original execution result" true
          (Option.get (N.internal_target snapshot) == Option.get !original);
        Alcotest.(check bool)
          "installed source is the original completed header" true
          (Option.get (N.internal_binding snapshot) == header);
        VM.check_function_phase_source owner ~namespace ~event snapshot
        |> checked
    | Parser.Function_position_written _ -> ()
    | _ -> Option.iter (fun record -> N.observe record event |> checked) !native);
    Ok ()
  in
  let commands : Parser.command_sink =
    {
      lexical_lookup = None;
      checkpoint =
        Some
          (fun event ->
            VM.observe_task_source_event owner event |> checked;
            Ok ());
      declaration = Some declaration;
      reference = None;
      call = None;
      implicit_output = None;
      query = None;
      dimension_count = None;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let source =
    Session.add_source session ~path:"internal-binding-authority.hc" ~contents
  in
  let config = Preprocessor.Config.create ~compilation_mode:Jit () |> checked in
  let parsed =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config source
  in
  Alcotest.(check bool)
    "original source parses" false (Parser.has_errors parsed);
  let fragment, authority, attempt, execution = Option.get !saved in
  reject "fragment authority expires with its original callback"
    (Fragment.authorize fragment);
  reject "expired source cannot create another preparation fact"
    (Prepared.create ~table ~namespace
       ~receipt:(Fragment.receipt fragment)
       ~bits:30L ~work:5);
  reject "expired source cannot begin another execution"
    (VM.begin_task_internal_binding owner authority);
  reject "execution proof remains single use after parsing"
    (VM.execute_task_internal_binding owner attempt execution)

let () =
  Alcotest.run "original internal binding authority"
    [
      ( "execution",
        [
          Alcotest.test_case "source, value and task ownership" `Quick
            (original_execution
               ~contents:"_intern (0x10+0xe) I64 Convert(U8 ch);");
          Alcotest.test_case "literal targets require original execution" `Quick
            (original_execution ~contents:"_intern 0x1e I64 Convert(U8 ch);"
               ~work:3 ~steps:3);
        ] );
    ]
