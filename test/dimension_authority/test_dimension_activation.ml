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
          call = None;
          implicit_output = None;
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

let pending_dimension_manifest () =
  List.iter
    (fun (contents, runtime_dependent, revoke) ->
      let session = Session.create () in
      let table = Session.semantic_symbols session in
      let namespace = C.create_namespace ~table () |> checked in
      let runtime = VM.create_task_state ~table () |> checked in
      let source =
        Session.add_source session ~path:"pending-dimension.hc" ~contents
      in
      let events_rev = ref [] and command_count = ref 0 and seen = ref false in
      let calls = A.create_call_journal ~namespace () in
      let commands : Parser.command_sink =
        {
          checkpoint =
            Some
              (fun event ->
                incr command_count;
                events_rev := A.Command event :: !events_rev;
                Ok ());
          reference =
            Some
              (fun receipt ->
                events_rev := A.Reference receipt :: !events_rev;
                Ok ());
          declaration =
            Some
              (fun event ->
                events_rev := A.Declaration event :: !events_rev;
                match event with
                | Parser.Array_dimension_preparing pending ->
                    seen := true;
                    let activation =
                      A.create ~calls ~namespace
                        ~context:
                          pending.dimension_owner.dimensions_command
                            .command_context ~observed_events:!command_count
                        (List.rev !events_rev)
                      |> checked
                    in
                    reject "missing checked dimension is not silently deferred"
                      (VM.promote_task_source_activation runtime ~namespace
                         ~activation ~dimensions:[]);
                    let result =
                      VM.promote_task_source_activation
                        ~pending_runtime_dimension:pending runtime ~namespace
                        ~activation ~dimensions:[]
                    in
                    if runtime_dependent then (
                      checked result;
                      let module F = Holyc_lib__Sema.Dimension_fragment in
                      let references =
                        Holyc_lib__Sema.Initializer_source
                        .expression_identifier_nodes
                          (Option.get pending.dimension_expression)
                        |> List.map (fun (identifier : Ast.identifier) ->
                            ( identifier,
                              Holyc_lib__Sema.Reference_selection.unavailable
                                ~table ~name:identifier.Ast.spelling
                              |> checked ))
                      in
                      let environment =
                        VM.task_snapshot runtime |> checked
                        |> Holyc_lib__Ir.Integer_globals.task_environment
                      in
                      let authority =
                        F.create ~table ~namespace ~receipt:pending ~environment
                          ~references ~queries:[]
                        |> checked |> F.authorize |> checked
                      in
                      if revoke then (
                        reject "activation fails before preparation"
                          (A.run activation ~invalid:"invalid" (fun _ ->
                               Error "abort"));
                        reject
                          "failed replay cannot revive live dimension callback"
                          (VM.begin_task_dimension runtime authority))
                      else (
                        reject
                          "journaled preparation cannot begin before replay"
                          (VM.begin_task_dimension runtime authority);
                        A.run activation ~invalid:"invalid" (function
                          | A.Declaration
                              (Parser.Array_dimension_preparing original)
                            when original == pending ->
                              let attempt =
                                VM.begin_task_dimension runtime authority
                                |> checked
                              in
                              VM.fail_task_dimension runtime attempt
                          | _ ->
                              reject
                                "earlier replay event cannot begin later \
                                 dimension"
                                (VM.begin_task_dimension runtime authority);
                              Ok ())
                        |> checked))
                    else
                      reject "closed dimension cannot use runtime deferral"
                        result;
                    Alcotest.(check int)
                      "promotion performs no dimension work" 0
                      (VM.task_initializer_steps runtime);
                    Error []
                | _ -> Ok ());
          call = Some { start = (fun _ -> Ok None); emit = (fun _ -> Ok ()) };
          implicit_output = None;
          query = None;
          dimension_count = None;
          command = (fun _ -> Ok ());
          resume = (fun () -> Ok ());
        }
      in
      let config =
        Preprocessor.Config.create ~compilation_mode:Jit () |> checked
      in
      ignore
        (Parser.parse ~commands ~sources:(Session.sources session)
           ~definitions:(Session.definitions session)
           ~symbols:(Session.symbols session) ~config source);
      Alcotest.(check bool) "original preparation reached" true !seen)
    [
      ("I64 A[N];", true, false);
      ("I64 A[N];", true, true);
      ("I64 A[2];", false, false);
    ]

let () =
  Alcotest.run "dimension activation authority"
    [
      ( "promotion",
        [
          Alcotest.test_case
            "consumed and expired journals reject before mutation" `Quick
            activation_lifetime;
          Alcotest.test_case
            "only the live trailing runtime dimension can defer" `Quick
            pending_dimension_manifest;
        ] );
    ]
