open Holyc_lib
module P = Semantic_provisional_function
module C = Semantic_declaration_collection

let checked = Test_declaration_collection.checked

let fixture ?(session = Session.create ()) ?(inspect = fun _ _ -> ()) source =
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let records = ref [] and samples = ref [] in
  let declaration event =
    (match event with
    | Parser.Function_declared source ->
        let publication = C.publish_function namespace source |> checked in
        let record = P.create ~table ~namespace publication source |> checked in
        records := (source, record) :: !records;
        inspect record event
    | _ ->
        List.iter
          (fun (_, record) ->
            if P.event_belongs record event then (
              P.observe record event |> checked;
              inspect record event))
          !records);
    Ok ()
  in
  let _, _, parsed, _, _, _ =
    Test_stream_parser.parse ~session ~same_task:true
      ~commands:(Test_provisional_function_parser.sink declaration)
      ~configure:(fun _ execution ->
        {
          execution with
          Parser.commands = Test_provisional_function_parser.sink declaration;
        })
      ~on_enter:(fun () ->
        match !records with
        | (_, record) :: _ -> samples := P.snapshot record :: !samples
        | [] -> ())
      source
  in
  ignore (Test_parser.expect_ast parsed);
  (session, namespace, List.rev !records, List.rev !samples)

let phases_are_immutable () =
  let _, _, records, samples =
    fixture "I64 F(I64 n=#exe {}40)#exe {}{return n;}"
  in
  let source, record = List.hd records in
  let before_default, before_header =
    match samples with
    | [ first; second ] -> (first, second)
    | _ -> Alcotest.fail "expected two original nested observations"
  in
  let member snapshot = List.hd (P.members snapshot) in
  Alcotest.(check int)
    "member exists before default input" 1
    (List.length (P.members before_default));
  Alcotest.(check bool)
    "default has not been parsed" true
    (Option.is_none (P.member_default_source (member before_default)));
  Alcotest.(check bool)
    "member source is still partial" true
    (Option.is_none (P.member_completion (member before_default)));
  Alcotest.(check bool)
    "default source appears before close lookahead" true
    (Option.is_some (P.member_default_source (member before_header)));
  Alcotest.(check bool)
    "original member completion precedes header" true
    (Option.is_some (P.member_completion (member before_header))
    && Option.is_none (P.completed_header before_header));
  let final = P.snapshot record in
  Alcotest.(check bool)
    "source publication persists" true
    (P.source final == source);
  let header = Option.get (P.completed_header final) in
  let complete = Option.get (P.member_completion (member final)) in
  Alcotest.(check bool)
    "header retains original completed parameter" true
    (List.hd header.parameters == complete.parameter_ast);
  Alcotest.(check bool)
    "completion did not mutate earlier snapshot" true
    (Option.is_none (P.completed_header before_header)
    && Option.is_none (P.member_default_source (member before_default)))

let delayed_events_reject () =
  let events = ref [] in
  let _, _, records, _ =
    fixture
      ~inspect:(fun record event -> events := (record, event) :: !events)
      "I64 F(I64 n=40,...){return n;}"
  in
  List.iter
    (fun (record, event) ->
      let before = P.snapshot record in
      Alcotest.(check bool)
        "delayed observer rejects" true
        (Result.is_error (P.observe record event));
      Alcotest.(check bool)
        "rejection leaves snapshot unchanged" true
        (P.snapshot record == before))
    !events;
  let _, record = List.hd records in
  Alcotest.(check bool)
    "final ellipsis owns its synthetic members" true
    (P.variadic_members_present (P.snapshot record))

let variadic_phases () =
  let _, _, records, samples =
    fixture "I64 F(...#exe {})#exe {}{return argc;}"
  in
  match samples with
  | [ flag; members ] ->
      Alcotest.(check bool)
        "flag exists at ellipsis lookahead" true
        (Option.is_some (P.variadic_source flag));
      Alcotest.(check bool)
        "synthetic members follow lookahead" false
        (P.variadic_members_present flag);
      Alcotest.(check bool)
        "members exist at close lookahead" true
        (P.variadic_members_present members);
      Alcotest.(check bool)
        "header remains unfinished there" true
        (Option.is_none (P.completed_header members));
      let final = P.snapshot (snd (List.hd records)) in
      let source = Option.get (P.variadic_source final) in
      Alcotest.(check bool)
        "same marker reaches completed header" true
        (Option.get (Option.get (P.completed_header final)).variadic
        == source.variadic_marker)
  | _ -> Alcotest.fail "expected both ellipsis lookahead boundaries"

let live_repetition_and_foreign_events () =
  let first = ref None and rejected = ref 0 in
  let inspect record event =
    let reject target =
      let before = P.snapshot target in
      Alcotest.(check bool)
        "live replay or foreign receipt rejects" true
        (Result.is_error (P.observe target event));
      Alcotest.(check bool)
        "rejection preserves exact snapshot" true
        (P.snapshot target == before);
      incr rejected
    in
    reject record;
    match !first with
    | None -> first := Some record
    | Some previous when previous != record -> reject previous
    | _ -> ()
  in
  ignore (fixture ~inspect "I64 F(I64 n=40,...);I64 G(U8 x=2,...);");
  Alcotest.(check bool) "all live member phases exercised" true (!rejected >= 20)

let namespace_and_lifetime () =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let foreign_namespace = C.create_namespace ~table () |> checked in
  let foreign_table = Session.semantic_symbols (Session.create ()) in
  let inspect record event =
    match event with
    | Parser.Function_declared source ->
        let state = P.snapshot record in
        Alcotest.(check bool)
          "same-table foreign namespace rejects" true
          (Result.is_error
             (P.create ~table ~namespace:foreign_namespace (P.publication state)
                source));
        Alcotest.(check bool)
          "foreign table rejects" true
          (Result.is_error
             (P.create ~table:foreign_table ~namespace:foreign_namespace
                (P.publication state) source))
    | _ -> ()
  in
  let _, namespace, records, _ = fixture ~session ~inspect "I64 F(I64 n);" in
  let source, record = List.hd records in
  let state = P.snapshot record in
  Alcotest.(check bool)
    "original table and namespace retained" true
    (P.owns_table state table && P.owns_namespace state namespace);
  Alcotest.(check bool)
    "previous lookup retained without count inference" true
    (P.previous_lookup state == source.function_previous);
  Alcotest.(check bool)
    "delayed creation cannot grant source authority" true
    (Result.is_error (P.create ~table ~namespace (P.publication state) source))

let recursive_children () =
  let _, _, records, _ =
    fixture
      "extern I64 F(reg I64 *p;I64 (*callback)(U8 n=2,;;),;U8 \
       *name=lastclass,...);"
  in
  let state = P.snapshot (snd (List.hd records)) in
  let header = Option.get (P.completed_header state) in
  Alcotest.(check int)
    "only named outer members are published" 3
    (List.length (P.members state));
  List.iter2
    (fun member (ast : Ast.function_parameter) ->
      let head = P.member_source member in
      Alcotest.(check bool)
        "original recursive children retained" true
        (head.parameter_type_specifier == ast.type_specifier
        && head.parameter_function_pointer == ast.function_pointer
        && head.parameter_register_qualifiers == ast.register_qualifiers
        && head.parameter_pointer_layers == ast.pointer_layers
        && (Option.get (P.member_completion member)).parameter_ast == ast))
    (P.members state) header.parameters

let skipped_and_expired_phases () =
  List.iter
    (fun omitted ->
      let session = Session.create () in
      let table = Session.semantic_symbols session in
      let namespace = C.create_namespace ~table () |> checked in
      let saved = ref None and skipped = ref None and failed = ref false in
      let declaration event =
        (match event with
        | Parser.Function_declared source ->
            let publication = C.publish_function namespace source |> checked in
            saved :=
              Some (P.create ~table ~namespace publication source |> checked)
        | _ ->
            let record = Option.get !saved in
            if P.event_belongs record event then
              if !failed then ()
              else if
                Test_provisional_function_parser.event_name event = omitted
              then skipped := Some event
              else if Option.is_some !skipped then (
                let before = P.snapshot record in
                Alcotest.(check bool)
                  "missing predecessor rejects next phase" true
                  (Result.is_error (P.observe record event));
                Alcotest.(check bool)
                  "skipped phase rejection preserves state" true
                  (P.snapshot record == before);
                failed := true)
              else P.observe record event |> checked);
        Ok ()
      in
      ignore
        (Test_provisional_function_parser.parse declaration
           "I64 F(I64 n=40,I64 m=2,...);"
        |> Test_parser.expect_ast);
      Alcotest.(check bool) "next phase exercised" true !failed;
      let record = Option.get !saved in
      let before = P.snapshot record in
      Alcotest.(check bool)
        "record remains unfinished" true
        (Option.is_none (P.completed_header before));
      Alcotest.(check bool)
        "expired original missing receipt rejects" true
        (Result.is_error (P.observe record (Option.get !skipped)));
      Alcotest.(check bool)
        "expiration preserves partial snapshot" true
        (P.snapshot record == before))
    [ "parameter"; "default"; "completion"; "ellipsis"; "variadic" ]

let ledger_rejects_missing_member_phases () =
  List.iter
    (fun omitted ->
      let session, ledger = Test_task_declarations.setup () in
      let skipped = ref false in
      let observe event =
        if
          (not !skipped)
          && Test_provisional_function_parser.event_name event = omitted
        then (
          skipped := true;
          Ok ())
        else Task_declarations.observe ledger event
      in
      let output, _ =
        Test_task_declarations.parse ~observe session ledger
          "extern I64 F(I64 n=40,...);"
      in
      Alcotest.(check bool) "original member event was omitted" true !skipped;
      Alcotest.(check bool)
        "ledger rejects an incomplete source transcript" true
        (Parser.has_errors output))
    [ "parameter"; "completion"; "ellipsis"; "variadic" ]

let nested_source_versions () =
  let _, _, records, samples =
    fixture "I64 F(I64 n=#exe {I64 F(I64 x,I64 y){return x+y;}}40){return n;}"
  in
  match (records, samples) with
  | [ (outer_source, outer); (inner_source, inner) ], [ partial ] ->
      Alcotest.(check bool)
        "nested declarations retain separate original publications" true
        (outer_source != inner_source && P.source partial == outer_source);
      Alcotest.(check int)
        "outer source keeps one member" 1
        (List.length (P.members (P.snapshot outer)));
      Alcotest.(check int)
        "nested source keeps two members" 2
        (List.length (P.members (P.snapshot inner)));
      Alcotest.(check bool)
        "suspended outer snapshot remains partial" true
        (Option.is_none (P.completed_header partial)
        && Option.is_none
             (P.member_default_source (List.hd (P.members partial))));
      Alcotest.(check bool)
        "both original headers complete independently" true
        (Option.is_some (P.completed_header (P.snapshot outer))
        && Option.is_some (P.completed_header (P.snapshot inner)))
  | _ ->
      Alcotest.fail
        "expected two nested source versions and one suspended snapshot"

let tests =
  [
    Alcotest.test_case "source member snapshots retain native phases" `Quick
      phases_are_immutable;
    Alcotest.test_case "delayed phase events reject without mutation" `Quick
      delayed_events_reject;
    Alcotest.test_case
      "ellipsis flag and synthetic members have separate phases" `Quick
      variadic_phases;
    Alcotest.test_case "live repeated and foreign phases preserve snapshots"
      `Quick live_repetition_and_foreign_events;
    Alcotest.test_case "namespace and callback lifetime govern source creation"
      `Quick namespace_and_lifetime;
    Alcotest.test_case "recursive callback children retain original identity"
      `Quick recursive_children;
    Alcotest.test_case "skipped and expired unfinished phases reject" `Quick
      skipped_and_expired_phases;
    Alcotest.test_case "ledger rejects missing original member phases" `Quick
      ledger_rejects_missing_member_phases;
    Alcotest.test_case "nested same-name declarations retain source versions"
      `Quick nested_source_versions;
  ]
