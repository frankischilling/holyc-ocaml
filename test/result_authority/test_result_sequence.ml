open Holyc_lib
module Task = Integer_task
module Declarations = Task_declarations
module VM = Ir_integer_interpreter

let checked = function
  | Ok value -> value
  | Error _ -> Alcotest.fail "source execution setup failed"

let ( let* ) = Result.bind

let accepted_root () =
  let session = Session.task_frontend (Session.create ()) in
  let source =
    Session.add_source session ~path:"original-result.hc" ~contents:"42;"
  in
  let ledger = Declarations.create_source session ~source |> checked in
  let task = ref None in
  let sequence = ref None in
  let commands : Parser.command_sink =
    {
      checkpoint =
        Some
          (fun event ->
            let* () = Declarations.observe_command ledger event in
            match event with
            | Parser.Sequence_started _ ->
                task :=
                  Some (Task.adopt_source session ~source ~ledger |> checked);
                Ok ()
            | Parser.Command_resumed receipt ->
                let task = Option.get !task in
                let* command =
                  Task.compile_source_ast task receipt.command_ast
                in
                Task.execute task command |> Result.map ignore
            | Parser.Sequence_completed receipt ->
                sequence := Some receipt;
                Ok ()
            | _ -> Ok ());
      reference = Some (Declarations.observe_reference ledger);
      declaration = Some (Declarations.observe ledger);
      query = Some (Declarations.observe_query ledger);
      dimension_count = Some (Declarations.grammar_dimension_count ledger);
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let parsed =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session)
      ~config:(Preprocessor.Config.create ~compilation_mode:Jit () |> checked)
      source
  in
  Alcotest.(check bool)
    "original source accepted" false (Parser.has_errors parsed);
  (session, Option.get !task, Option.get !sequence)

let final_bits result =
  Option.map (fun word -> word.VM.bits) (VM.final_value result)

let run_callback_free session task source =
  let parsed =
    Parser.parse ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session)
      ~config:(Preprocessor.Config.create ~compilation_mode:Jit () |> checked)
      source
  in
  Alcotest.(check bool)
    "later callback-free syntax accepted" false (Parser.has_errors parsed);
  let* command = Task.compile_ast task (Option.get parsed.ast) in
  Task.execute task command

let old_sequence_cannot_certify_later_progress run_later () =
  let session, task, sequence = accepted_root () in
  let original = Task.result task ~sequence |> checked in
  Alcotest.(check (option int64))
    "original result" (Some 42L) (final_bits original);
  let later =
    Session.add_source session ~path:"later-result.hc" ~contents:"7;"
  in
  let later_result = run_later session task later |> checked in
  Alcotest.(check (option int64))
    "later execution reached" (Some 7L) (final_bits later_result);
  match Task.result task ~sequence with
  | Error _ -> ()
  | Ok retained ->
      (* Repeated inspection may retain the original immutable result. The
         original sequence cannot certify the later source's task progress. *)
      Alcotest.(check (option int64))
        "original sequence retains its value" (final_bits original)
        (final_bits retained);
      Alcotest.(check int)
        "original sequence retains its instruction count"
        (VM.executed_steps original)
        (VM.executed_steps retained)

let input_fixture () =
  let session = Session.create () in
  let task =
    VM.create_task_state ~table:(Session.semantic_symbols session) () |> checked
  in
  (session, task)

let empty_input ?(before_completion = fun _ -> ()) session task =
  let source = Session.add_source session ~path:"empty-input.hc" ~contents:"" in
  let sequence = ref None in
  let commands : Parser.command_sink =
    {
      checkpoint =
        Some
          (fun event ->
            (match event with
            | Parser.Sequence_completed receipt ->
                before_completion receipt;
                sequence := Some receipt
            | _ -> ());
            VM.observe_task_source_event task event
            |> Result.map_error (fun _ -> []));
      reference = None;
      declaration = None;
      query = None;
      dimension_count = None;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let parsed =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session)
      ~config:(Preprocessor.Config.create ~compilation_mode:Jit () |> checked)
      source
  in
  Alcotest.(check bool) "empty input accepted" false (Parser.has_errors parsed);
  Option.get !sequence

let input_completion_ownership () =
  let session, task = input_fixture () in
  let _, foreign = input_fixture () in
  let sequence =
    empty_input session task ~before_completion:(fun sequence ->
        Alcotest.(check bool)
          "unaccepted completion has no result" true
          (Result.is_error (VM.task_input_result task ~sequence)))
  in
  let result = VM.task_input_result task ~sequence |> checked in
  Alcotest.(check (option int64)) "empty input value" None (final_bits result);
  Alcotest.(check bool)
    "foreign task cannot project completion" true
    (Result.is_error (VM.task_input_result foreign ~sequence));
  let repeated = VM.task_input_result task ~sequence |> checked in
  Alcotest.(check bool) "result is frozen" true (repeated == result);
  let next = empty_input session task in
  ignore (VM.task_input_result task ~sequence:next |> checked);
  Alcotest.(check bool)
    "previous completion cannot certify later input" true
    (Result.is_error (VM.task_input_result task ~sequence))

let delayed_input_start () =
  let session, task = input_fixture () in
  let source =
    Session.add_source session ~path:"delayed-input.hc" ~contents:""
  in
  let start = ref None in
  let commands : Parser.command_sink =
    {
      checkpoint =
        Some
          (fun event ->
            (match event with
            | Parser.Sequence_started _ -> start := Some event
            | _ -> ());
            Ok ());
      reference = None;
      declaration = None;
      query = None;
      dimension_count = None;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let parsed =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session)
      ~config:(Preprocessor.Config.create ~compilation_mode:Jit () |> checked)
      source
  in
  Alcotest.(check bool)
    "original parse accepted" false (Parser.has_errors parsed);
  Alcotest.(check bool)
    "delayed start is rejected" true
    (Result.is_error (VM.observe_task_source_event task (Option.get !start)));
  let sequence = empty_input session task in
  ignore (VM.task_input_result task ~sequence |> checked)

let input_buffer_identity () =
  let session, task = input_fixture () in
  let original = VM.begin_task_stream task |> checked in
  let first = empty_input session task in
  ignore (VM.task_input_result task ~sequence:first |> checked);
  let replacement = ref None in
  let replaced =
    empty_input session task ~before_completion:(fun _ ->
        VM.abort_task_stream task original |> checked;
        replacement := Some (VM.begin_task_stream task |> checked))
  in
  Alcotest.(check bool)
    "same depth with another buffer cannot certify input" true
    (Result.is_error (VM.task_input_result task ~sequence:replaced));
  VM.abort_task_stream task (Option.get !replacement) |> checked;
  let next = empty_input session task in
  ignore (VM.task_input_result task ~sequence:next |> checked)

let handled_isolated_failure () =
  List.iter
    (fun seal ->
      let session, task = input_fixture () in
      let isolated_session = Session.create () in
      let source =
        Session.add_source isolated_session ~path:"isolated-fault.hc"
          ~contents:"1/0;"
      in
      let config =
        Preprocessor.Config.create ~compilation_mode:Aot () |> checked
      in
      let program =
        (compile_integer_program isolated_session ~config ~source |> checked)
          .value
      in
      let entry = integer_program_entry program in
      let globals = integer_program_globals program in
      let initialization = integer_program_initialization program in
      let runtime_calls = integer_program_runtime_calls program in
      let functions = integer_program_functions program in
      let execute () =
        VM.execute_isolated_program_in_task task ~runtime_calls ~globals
          ~initialization ~functions entry
      in
      Alcotest.(check bool)
        "earlier unsealed output fails" true
        (Result.is_error (execute ()));
      if seal then (
        let ticket = VM.begin_isolated_preparation task in
        VM.record_isolated_preparation task ticket ~steps:0;
        VM.finish_isolated_preparation task ticket ~runtime_calls ~globals
          ~initialization ~functions entry
        |> checked);
      let sequence =
        empty_input session task ~before_completion:(fun _ ->
            match execute () with
            | Error (error :: _) ->
                Alcotest.(check string)
                  "expected isolated failure"
                  (if seal then "HCIRVM0009" else "HCIRVM0026")
                  error.VM.code
            | _ -> Alcotest.fail "isolated failure disappeared")
      in
      Alcotest.(check bool)
        "handled current failure prevents successful input completion" true
        (Result.is_error (VM.task_input_result task ~sequence));
      let next = empty_input session task in
      ignore (VM.task_input_result task ~sequence:next |> checked))
    [ false; true ]

let () =
  Alcotest.run "source result authority"
    [
      ( "invocation ownership",
        [
          Alcotest.test_case "old root cannot certify a later source run" `Quick
            (old_sequence_cannot_certify_later_progress (fun _ task source ->
                 Task.run task ~source));
          Alcotest.test_case "old root cannot certify callback-free execution"
            `Quick
            (old_sequence_cannot_certify_later_progress run_callback_free);
          Alcotest.test_case "input completion ownership" `Quick
            input_completion_ownership;
          Alcotest.test_case "delayed input start leaves order intact" `Quick
            delayed_input_start;
          Alcotest.test_case "input retains exact existing buffers" `Quick
            input_buffer_identity;
          Alcotest.test_case
            "handled isolated failures invalidate current input" `Quick
            handled_isolated_failure;
        ] );
    ]
