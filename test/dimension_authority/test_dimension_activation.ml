open Holyc_lib
module A = Holyc_lib__Sema.Source_activation
module VM = Ir_integer_interpreter
module C = Semantic_declaration_collection

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let reject label result =
  Alcotest.(check bool) label true (Result.is_error result)

let activation_lifetime () =
  List.iter
    (fun consumed ->
      let session = Session.create () in
      let table = Session.semantic_symbols session in
      let namespace = C.create_namespace ~table () |> checked in
      let source =
        Session.add_source session ~path:"authority.hc" ~contents:"42;"
      in
      let runtime = VM.create_task_state ~table () |> checked in
      let saved = ref None in
      let checkpoint event =
        (match event with
        | Parser.Sequence_started context ->
            let create () =
              A.create ~namespace ~context ~observed_events:1
                [ A.Command event ]
              |> checked
            in
            let activation = create () in
            saved := Some activation;
            if consumed then (
              A.run activation ~invalid:"invalid" (fun _ -> Ok ()) |> checked;
              reject "consumed journal cannot close promotion"
                (VM.promote_task_source_activation runtime ~namespace
                   ~activation ~dimensions:[]);
              VM.promote_task_source_activation runtime ~namespace
                ~activation:(create ()) ~dimensions:[]
              |> checked)
        | _ -> ());
        Ok ()
      in
      let commands : Parser.command_sink =
        {
          checkpoint = Some checkpoint;
          reference = None;
          declaration = None;
          query = None;
          dimension_count = None;
          command = (fun _ -> Ok ());
          resume = (fun () -> Ok ());
        }
      in
      let config =
        Preprocessor.Config.create ~compilation_mode:Jit () |> checked
      in
      let output =
        Parser.parse ~commands ~sources:(Session.sources session)
          ~definitions:(Session.definitions session)
          ~symbols:(Session.symbols session) ~config source
      in
      Alcotest.(check bool) "source parsed" false (Parser.has_errors output);
      if not consumed then
        reject "expired journal cannot close promotion"
          (VM.promote_task_source_activation runtime ~namespace
             ~activation:(Option.get !saved) ~dimensions:[]))
    [ true; false ]

let () =
  Alcotest.run "dimension activation authority"
    [
      ( "promotion",
        [
          Alcotest.test_case
            "consumed and expired journals reject before mutation" `Quick
            activation_lifetime;
        ] );
    ]
