open Holyc_lib
module VM = Holyc_lib__Ir.Integer_interpreter
module C = Semantic_declaration_collection
module Record = Holyc_lib__Sema.Compiler_record
module Fragment = Holyc_lib__Sema.Member_metadata_fragment
module Globals = Holyc_lib__Ir.Integer_globals
module Typing = Holyc_lib__Driver.Initializer_fragment_typing
module Destination = Holyc_lib__Ir.Member_metadata_fragment_destination
module Lowering = Holyc_lib__Driver.Member_metadata_fragment_lowering
module Program = Holyc_lib__Ir.Member_metadata_fragment_program

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let diagnostics = function
  | Ok value -> value
  | Error _ -> Alcotest.fail "unexpected diagnostics"

let reject label result =
  Alcotest.(check bool) label true (Result.is_error result)

let ownership_and_saved_value () =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let owner = VM.create_task_state ~table () |> checked in
  let foreign = VM.create_task_state ~table () |> checked in
  VM.bind_task_namespace owner namespace |> checked;
  VM.bind_task_namespace foreign namespace |> checked;
  let progress = ref None and publication = ref None and saved = ref None in
  let completion = ref None in
  let declaration = function
    | Parser.Aggregate_declared source ->
        let published = C.publish_aggregate namespace source |> checked in
        publication := Some published;
        progress :=
          Some (Record.begin_aggregate ~table ~namespace published |> checked)
    | Parser.Aggregate_advanced receipt ->
        let current = Option.get !progress in
        (match receipt.phase_step with
        | Parser.Aggregate_member_metadata_reached _ ->
            let view = VM.task_snapshot owner |> checked in
            let fragment =
              Fragment.create ~table ~namespace ~progress:current ~receipt
                ~environment:(Globals.task_environment view)
                ~references:[] ~queries:[]
              |> checked
            in
            let authority = Fragment.authorize fragment |> checked in
            let attempt =
              VM.begin_task_member_metadata owner authority |> checked
            in
            reject "foreign runtime cannot borrow original metadata receipt"
              (VM.begin_task_member_metadata foreign authority);
            let context =
              Typing.create_context ~table ~parent:(C.namespace_scope namespace)
              |> checked
            in
            let typed =
              Typing.prepare_member_metadata context fragment |> checked
            in
            reject "foreign snapshot cannot supply metadata globals"
              (Destination.create
                 ~task_view:(VM.task_snapshot foreign |> checked)
                 typed);
            let destination =
              Destination.create ~task_view:view typed |> checked
            in
            let execution =
              Lowering.prepare ~context ~authority ~runtime:owner destination
              |> diagnostics
            in
            let (Program.Scheduled program) = Program.code execution in
            let module Lower = Holyc_lib__Ir.Integer_program_lowering in
            let unrelated =
              Lower.lower_complete
                ~globals:(Destination.globals destination)
                ~span:(Destination.span destination)
                [ Lower.Empty (Destination.span destination) ]
              |> diagnostics
            in
            reject "metadata graph cannot borrow another expression lowering"
              (Program.create ~authority ~destination ~lowered:unrelated
                 ~entry:(Program.entry program)
                 ~initialization:(Program.initialization program)
                 ~runtime_calls:(Program.runtime_calls program));
            let wrong_work =
              Program.prepare ~authority ~destination
                ~code:(Program.code execution)
                ~steps:(Program.steps execution + 1)
              |> checked
            in
            reject "changed preparation count cannot execute metadata"
              (VM.execute_task_member_metadata owner attempt wrong_work);
            reject "foreign runtime cannot execute metadata"
              (VM.execute_task_member_metadata foreign attempt execution);
            Alcotest.(check int)
              "foreign rejection has no effects" 0
              (VM.task_executed_steps foreign);
            VM.execute_task_member_metadata owner attempt execution
            |> diagnostics;
            reject "successful execution cannot replay"
              (VM.execute_task_member_metadata owner attempt execution);
            reject "successful receipt cannot begin again"
              (VM.begin_task_member_metadata owner authority);
            reject "semantic authority cannot be reacquired"
              (Fragment.authorize fragment);
            let metadata =
              VM.task_member_metadata owner receipt |> Option.get
            in
            (match Record.member_metadata_value metadata with
            | Record.Metadata_word value ->
                Alcotest.(check int64)
                  "original executed metadata word" 16L value
            | _ -> Alcotest.fail "expected a numeric metadata value");
            Alcotest.(check int)
              "saved metadata retains preparation and execution work"
              (VM.task_initializer_steps owner)
              (Record.member_metadata_work metadata);
            saved := Some (fragment, attempt, execution)
        | _ -> ());
        Record.advance_aggregate ~dimensions:(fun _ -> None) current receipt
        |> checked
    | Parser.Aggregate_completed receipt ->
        completion :=
          Some
            (Record.complete_aggregate ~table ~namespace
               ~progress:(Option.get !progress) (Option.get !publication)
               receipt
            |> checked)
    | _ -> ()
  in
  let commands : Parser.command_sink =
    {
      lexical_lookup = None;
      call = None;
      implicit_output = None;
      reference = None;
      query = None;
      dimension_count = None;
      checkpoint =
        Some
          (fun event ->
            VM.observe_task_source_event owner event |> checked;
            Ok ());
      declaration =
        Some
          (fun event ->
            declaration event;
            Ok ());
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let source =
    Session.add_source session ~path:"member-metadata-authority.hc"
      ~contents:"class Box {I64 value data $$+8;};"
  in
  let parsed =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session)
      ~config:(Preprocessor.Config.create ~compilation_mode:Jit () |> checked)
      source
  in
  Alcotest.(check bool) "source parsed" false (Parser.has_errors parsed);
  let field = Record.aggregate_fields (Option.get !completion) |> List.hd in
  let metadata =
    Record.aggregate_field_metadata_find field "data" |> Option.get
  in
  (match Record.member_metadata_value metadata with
  | Record.Metadata_word value ->
      Alcotest.(check int64) "completed member keeps captured value" 16L value
  | _ -> Alcotest.fail "expected completed numeric metadata");
  let fragment, attempt, execution = Option.get !saved in
  reject "source authority expires" (Fragment.authorize fragment);
  reject "execution expires"
    (VM.execute_task_member_metadata owner attempt execution)

let () =
  Alcotest.run "Original runtime member metadata authority"
    [
      ( "source and runtime",
        [
          Alcotest.test_case
            "saved values, foreign owners, changed work and replay" `Quick
            ownership_and_saved_value;
        ] );
    ]
