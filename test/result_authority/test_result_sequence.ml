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
        ] );
    ]
