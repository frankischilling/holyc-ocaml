open Holyc_lib
module D = Task_declarations
module VM = Ir_integer_interpreter
module Typing = Holyc_lib__Driver.Initializer_fragment_typing
module Preparation = Holyc_lib__Driver.Integer_initializers
module Fragment = Holyc_lib__Sema.Default_fragment
module Destination = Holyc_lib__Ir.Default_fragment_destination
module Program = Holyc_lib__Ir.Default_fragment_program

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let diagnostics = function
  | Ok value -> value
  | Error _ -> Alcotest.fail "unexpected diagnostics"

let reject label result =
  Alcotest.(check bool) label true (Result.is_error result)

let preparation_budget ?(publish = true) () =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let source =
    Session.add_source session ~path:"default-authority.hc"
      ~contents:"I64 F(I64 x=20+22){return x;};"
  in
  let ledger = D.create_source session ~source |> checked in
  let owner = VM.create_task_state ~table () |> checked in
  let foreign = VM.create_task_state ~table () |> checked in
  let completed = ref None in
  let declaration event =
    D.observe ledger event |> diagnostics;
    (match event with
    | Parser.Parameter_default_completed receipt ->
        let authority =
          D.begin_source_default ledger ~runtime:owner receipt |> diagnostics
        in
        let fragment = Fragment.authorized_fragment authority in
        let context =
          Typing.create_aot_context ~table ~parent:(D.initializer_scope ledger)
          |> checked
        in
        let typed = Typing.prepare_default context fragment |> checked in
        let destination = Destination.create_source typed |> checked in
        let classification, steps =
          Preparation.prepare_default ~max_steps:100 ~top_calls:[] destination
          |> diagnostics
        in
        let bits =
          match classification with
          | Preparation.Prepared_constant bits -> bits
          | Scheduled -> Alcotest.fail "constant default was scheduled"
        in
        let execution =
          Program.prepare ~authority ~destination ~code:(Program.Prepared bits)
            ~steps
          |> checked
        in
        reject "uncharged preparation cannot complete"
          (D.finish_source_default ledger execution);
        VM.record_task_preparation foreign ~before:0 ~steps;
        reject "another invocation cannot pay for preparation"
          (D.finish_source_default ledger execution);
        VM.record_task_preparation owner ~before:0 ~steps;
        D.finish_source_default ledger execution |> diagnostics;
        reject "completion cannot replay"
          (D.finish_source_default ledger execution);
        completed := Some execution
    | Parser.Function_header_completed header when publish ->
        D.complete_source_defaults ledger header |> diagnostics
    | _ -> ());
    Ok ()
  in
  let commands : Parser.command_sink =
    {
      checkpoint = Some (D.observe_command ledger);
      implicit_output = None;
      reference = Some (D.observe_reference ledger);
      declaration = Some declaration;
      query = Some (D.observe_query ledger);
      dimension_count = Some (D.grammar_dimension_count ledger);
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let config = Preprocessor.Config.create ~compilation_mode:Aot () |> checked in
  let output =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config source
  in
  Alcotest.(check bool) "source parsed" false (Parser.has_errors output);
  reject "completion cannot outlive callback"
    (D.finish_source_default ledger (Option.get !completed));
  let sealed = D.seal_source ledger (Option.get output.ast) in
  if publish then ignore (diagnostics sealed)
  else reject "unpublished default cannot enter output seal" sealed

let missing_preparation () =
  List.iter
    (fun contents ->
      let session = Session.create () in
      let source =
        Session.add_source session ~path:"missing-default.hc" ~contents
      in
      let ledger = D.create_source session ~source |> checked in
      let commands : Parser.command_sink =
        {
          checkpoint = Some (D.observe_command ledger);
          implicit_output = None;
          reference = Some (D.observe_reference ledger);
          declaration = Some (D.observe ledger);
          query = Some (D.observe_query ledger);
          dimension_count = Some (D.grammar_dimension_count ledger);
          command = (fun _ -> Ok ());
          resume = (fun () -> Ok ());
        }
      in
      let config =
        Preprocessor.Config.create ~compilation_mode:Aot () |> checked
      in
      let output =
        Parser.parse ~commands ~sources:(Session.sources session)
          ~definitions:(Session.definitions session)
          ~symbols:(Session.symbols session) ~config source
      in
      Alcotest.(check bool)
        "unprepared source parsed" false (Parser.has_errors output);
      reject "unused default requires preparation before sealing"
        (D.seal_source ledger (Option.get output.ast)))
    [ "I64 F(I64 x=20+22){return x;};"; "extern I64 F(I64 x=20+22);" ]

let () =
  Alcotest.run "AOT default authority"
    [
      ( "preparation",
        [
          Alcotest.test_case
            "owning invocation must pay for original preparation" `Quick
            (preparation_budget ~publish:true);
          Alcotest.test_case "sealing requires header publication" `Quick
            (preparation_budget ~publish:false);
          Alcotest.test_case "sealing requires even unused defaults" `Quick
            missing_preparation;
        ] );
    ]
