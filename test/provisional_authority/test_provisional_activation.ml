open Holyc_lib
module A = Holyc_lib__Sema.Source_activation
module P = Semantic_provisional_function
module C = Semantic_declaration_collection

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let reject label value =
  Alcotest.(check bool) label true (Result.is_error value)

let activation_replays_original_phases () =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let foreign_namespace = C.create_namespace ~table () |> checked in
  let events = ref [] and count = ref 0 and publication = ref None in
  let saved = ref None and record = ref None in
  let declaration event =
    events := A.Declaration event :: !events;
    (match event with
    | Parser.Function_declared source ->
        publication := Some (C.publish_function namespace source |> checked)
    | _ -> ());
    Ok ()
  in
  let checkpoint event =
    incr count;
    events := A.Command event :: !events;
    (match event with
    | Parser.Command_completed command ->
        let activation =
          A.create ~namespace ~context:command.command_start.command_context
            ~observed_events:!count (List.rev !events)
          |> checked
        in
        saved := Some activation;
        A.run activation ~invalid:"invalid activation" (function
          | A.Declaration (Parser.Function_declared source) ->
              let publication = Option.get !publication in
              reject "inactive declaration requires activation"
                (P.create ~table ~namespace publication source);
              reject "foreign namespace cannot use activation"
                (P.create ~activation ~table ~namespace:foreign_namespace
                   publication source);
              record :=
                Some
                  (P.create ~activation ~table ~namespace publication source
                  |> checked);
              Ok ()
          | A.Declaration event ->
              let record = Option.get !record in
              if P.event_belongs record event then (
                let before = P.snapshot record in
                reject "inactive receipt has no ordinary authority"
                  (P.observe record event);
                Alcotest.(check bool)
                  "rejection preserves snapshot" true
                  (P.snapshot record == before);
                P.observe ~activation record event |> checked;
                reject "active receipt cannot repeat"
                  (P.observe ~activation record event));
              Ok ()
          | _ -> Ok ())
        |> checked;
        reject "activation can only be consumed once"
          (A.run activation ~invalid:"consumed" (fun _ -> Ok ()))
    | _ -> ());
    Ok ()
  in
  let commands : Parser.command_sink =
    {
      checkpoint = Some checkpoint;
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
    Session.add_source session ~path:"activation.hc"
      ~contents:"extern I64 F(I64 n=40,I64 m=2,...);"
  in
  let config = Preprocessor.Config.create ~compilation_mode:Jit () |> checked in
  let output =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~symbols:(Session.symbols session)
      ~definitions:(Session.definitions session)
      ~config source
  in
  Alcotest.(check bool) "source parsed" false (Parser.has_errors output);
  let state = P.snapshot (Option.get !record) in
  Alcotest.(check bool)
    "activation retained complete original transcript" true
    (List.length (P.members state) = 2
    && P.variadic_members_present state
    && Option.is_some (P.completed_header state));
  reject "expired activation cannot create source authority"
    (P.create ~activation:(Option.get !saved) ~table ~namespace
       (P.publication state) (P.source state))

let () =
  Alcotest.run "provisional source authority"
    [
      ( "activation",
        [
          Alcotest.test_case "original source phases replay once" `Quick
            activation_replays_original_phases;
        ] );
    ]
