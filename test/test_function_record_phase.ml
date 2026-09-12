open Holyc_lib
module N = Semantic_function_record_phase
module P = Semantic_provisional_function
module C = Semantic_declaration_collection
module A = Holyc_lib__Sema.Source_activation

let checked = Test_declaration_collection.checked

let fixture ?(session = Session.create ()) ?(inspect = fun _ _ -> ()) source =
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let registry =
    N.create_registry ~mode:Preprocessor.Jit ~table ~namespace |> checked
  in
  let records = ref [] and samples = ref [] in
  let declaration event =
    (match event with
    | Parser.Function_declared source ->
        let publication = C.publish_function namespace source |> checked in
        let record = N.begin_header registry publication source |> checked in
        records := (source, record) :: !records;
        inspect record event
    | _ ->
        List.iter
          (fun (_, record) ->
            if N.event_belongs record event then (
              N.observe record event |> checked;
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
        | (_, record) :: _ -> samples := N.snapshot record :: !samples
        | [] -> ())
      source
  in
  ignore (Test_parser.expect_ast parsed);
  (registry, List.rev !records, List.rev !samples)

let count label expected snapshot =
  Alcotest.(check (option int))
    label (Some expected)
    (N.argument_count snapshot)

let fixed snapshot = N.call_shape snapshot |> checked |> N.fixed_members

let fresh_and_reused () =
  List.iter
    (fun prefix ->
      let _, records, samples =
        fixture (prefix ^ "I64 F(I64 n=#exe {}40)#exe {}{return n;}")
      in
      match samples with
      | [ before_default; before_header ] -> (
          count "active count cleared before default" 0 before_default;
          count "completed source parameter does not change active count" 0
            before_header;
          Alcotest.(check (option int))
            "concrete member inserted" (Some 1)
            (N.member_count before_default);
          Alcotest.(check int)
            "zero native fixed slots despite source member" 0
            (List.length (fixed before_header));
          Alcotest.(check int)
            "unconsumed native members remain available for typing" 1
            (List.length (N.native_members before_header));
          Alcotest.(check int)
            "source member remains visible" 1
            (List.length (P.members (N.source_snapshot before_header)));
          let last = snd (List.hd (List.rev records)) in
          count "completed header activates current member count" 1
            (N.snapshot last);
          count "earlier snapshot remains zero" 0 before_header;
          match records with
          | [ (_, prior); (_, next) ] ->
              Alcotest.(check bool)
                "extern reuses native identity" true
                (N.same_identity (N.snapshot prior) (N.snapshot next));
              Alcotest.(check (option int))
                "comparison count retained separately" (Some 1)
                (N.saved_previous_argument_count (N.snapshot next))
          | _ -> ())
      | _ -> Alcotest.fail "expected original lookahead observations")
    [ ""; "extern I64 F(I64 old);" ]

let variadic_cursor () =
  List.iter
    (fun concrete ->
      let _, records, samples =
        fixture ("I64 F(" ^ concrete ^ "...#exe {})#exe {};")
      in
      match samples with
      | [ flag; synthetic ] ->
          Alcotest.(check bool)
            "flag set before synthetic members" true (N.ellipsis_flag flag);
          Alcotest.(check bool)
            "flag alone cannot grant variadic call" true
            (Option.is_none (N.variadic_tail (N.call_shape flag |> checked)));
          let tail = N.variadic_tail (N.call_shape synthetic |> checked) in
          Alcotest.(check bool)
            "tail depends on actual remaining cursor" (concrete = "")
            (Option.is_some tail);
          let final = N.snapshot (snd (List.hd records)) in
          Alcotest.(check int)
            "synthetics do not increment member count"
            (if concrete = "" then 0 else 1)
            (List.length (fixed final));
          Alcotest.(check bool)
            "completed fixed traversal reaches ellipsis" true
            (Option.is_some (N.variadic_tail (N.call_shape final |> checked)))
      | _ -> Alcotest.fail "expected ellipsis phase snapshots")
    [ ""; "I64 n," ]

let definition_allocates () =
  let _, records, _ = fixture "I64 F(I64 n){return n;}I64 F(I64 x);" in
  match records with
  | [ (_, first); (_, second) ] ->
      Alcotest.(check bool)
        "definition causes fresh native allocation" false
        (N.same_identity (N.snapshot first) (N.snapshot second))
  | _ -> Alcotest.fail "expected two publications"

let nested_shared_record () =
  let _, records, samples =
    fixture "I64 F(I64 n)#exe {I64 F(I64 x,I64 y){return x+y;}}{return n;}"
  in
  match (records, samples) with
  | [ (_, outer); (_, inner) ], [ suspended ] ->
      let outer = N.snapshot outer and inner = N.snapshot inner in
      Alcotest.(check bool)
        "nested header reuses suspended extern" true
        (N.same_identity outer inner);
      count "outer resume keeps current native count" 2 outer;
      count "nested completed count" 2 inner;
      count "suspended snapshot stays zero" 0 suspended;
      Alcotest.(check int)
        "outer source stays independent" 1
        (List.length (P.members (N.source_snapshot outer)));
      Alcotest.(check int)
        "native member view follows the nested replacement" 2
        (List.length (N.native_members outer));
      Alcotest.(check bool)
        "native member view retains exact inner member identity" true
        (List.for_all2 ( == ) (N.native_members outer)
           (P.members (N.source_snapshot inner)))
  | _ -> Alcotest.fail "expected nested original publications"

let nested_stale_member () =
  let _, records, _ =
    fixture "I64 F(I64 n=#exe {I64 F(I64 x,I64 y){return x+y;}}40){return n;}"
  in
  let outer = N.snapshot (snd (List.hd records)) in
  Alcotest.(check bool)
    "default cannot update a replaced native member" true
    (Result.is_error (N.call_shape outer))

let nested_synthetic_fixed_slot () =
  let _, records, _ = fixture "I64 F(I64 n,#exe {extern I64 F(...);}I64 m);" in
  let final = N.snapshot (snd (List.hd records)) in
  count "current native member count drives outer completion" 1 final;
  Alcotest.(check bool)
    "synthetic fixed slot cannot masquerade as source parameter" true
    (Result.is_error (N.call_shape final))

let rejections () =
  let first = ref None and saved = ref [] in
  let inspect record event =
    saved := (record, event) :: !saved;
    let reject record =
      let before = N.snapshot record in
      Alcotest.(check bool)
        "live repeat or foreign event rejected" true
        (Result.is_error (N.observe record event));
      Alcotest.(check bool)
        "rejection does not mutate snapshot" true
        (before == N.snapshot record)
    in
    reject record;
    match !first with
    | None -> first := Some record
    | Some previous when previous != record -> reject previous
    | _ -> ()
  in
  ignore (fixture ~inspect "extern I64 F(I64 n=40,...);I64 G(U8 x=2,...);");
  List.iter
    (fun (record, event) ->
      let before = N.snapshot record in
      Alcotest.(check bool)
        "expired event rejected" true
        (Result.is_error (N.observe record event));
      Alcotest.(check bool)
        "expired rejection preserves state" true
        (before == N.snapshot record))
    !saved

let mode_and_ownership () =
  let table = Session.semantic_symbols (Session.create ()) in
  let namespace = C.create_namespace ~table () |> checked in
  Alcotest.(check bool)
    "AOT has no JIT reuse authority" true
    (Result.is_error
       (N.create_registry ~mode:Preprocessor.Aot ~table ~namespace));
  let foreign = Session.semantic_symbols (Session.create ()) in
  Alcotest.(check bool)
    "foreign table rejected" true
    (Result.is_error
       (N.create_registry ~mode:Preprocessor.Jit ~table:foreign ~namespace))

let unknown_lineage () =
  let session = Session.create () in
  ignore
    (Symbol_visibility.Environment.add (Session.symbols session) ~name:"F"
       ~kind:Symbol_visibility.Function ());
  let _, records, samples = fixture ~session "I64 F(I64 n)#exe {};" in
  let partial = List.hd samples in
  Alcotest.(check bool)
    "untracked prior record is explicit unavailable evidence" true
    (Option.is_some (N.unavailable_reason partial));
  Alcotest.(check (option int))
    "untracked prior count is never guessed" None (N.argument_count partial);
  Alcotest.(check bool)
    "completion cannot fabricate missing native lineage" true
    (Result.is_error (N.call_shape (N.snapshot (snd (List.hd records)))))

let intervening_untracked_lineage () =
  let session = Session.create () in
  let inserted = ref false in
  let inspect _ event =
    match event with
    | Parser.Function_header_completed _ when not !inserted ->
        inserted := true;
        ignore
          (Symbol_visibility.Environment.add (Session.symbols session) ~name:"F"
             ~kind:Symbol_visibility.Function ())
    | _ -> ()
  in
  let _, records, _ =
    fixture ~session ~inspect "extern I64 F(I64 n);extern I64 F(I64 x);"
  in
  let final = N.snapshot (snd (List.hd (List.rev records))) in
  Alcotest.(check bool)
    "untracked replacement retains unavailable evidence" true
    (Result.is_error (N.call_shape final))

let sticky_flag_without_tail () =
  let _, records, _ = fixture "extern I64 F(...);extern I64 F(I64 n);" in
  let final = N.snapshot (snd (List.hd (List.rev records))) in
  Alcotest.(check bool)
    "extern reset preserves native function flag" true (N.ellipsis_flag final);
  let shape = N.call_shape final |> checked in
  Alcotest.(check int)
    "replacement has one native fixed member" 1
    (List.length (N.fixed_members shape));
  Alcotest.(check bool)
    "retained flag does not recreate removed synthetics" true
    (Option.is_none (N.variadic_tail shape));
  Alcotest.(check bool)
    "checked shape retains exact full snapshot" true
    (N.shape_snapshot shape == final)

let missing_events () =
  List.iter
    (fun omitted ->
      let session = Session.create () in
      let table = Session.semantic_symbols session in
      let namespace = C.create_namespace ~table () |> checked in
      let registry =
        N.create_registry ~mode:Preprocessor.Jit ~table ~namespace |> checked
      in
      let saved = ref None and skipped = ref None and failed = ref false in
      let declaration event =
        (match event with
        | Parser.Function_declared source ->
            let publication = C.publish_function namespace source |> checked in
            saved := Some (N.begin_header registry publication source |> checked)
        | _ ->
            let record = Option.get !saved in
            if N.event_belongs record event && not !failed then
              if Test_provisional_function_parser.event_name event = omitted
              then skipped := Some event
              else if Option.is_some !skipped then (
                let before = N.snapshot record in
                Alcotest.(check bool)
                  "missing predecessor rejects successor" true
                  (Result.is_error (N.observe record event));
                Alcotest.(check bool)
                  "out-of-order event preserves native state" true
                  (N.snapshot record == before);
                failed := true)
              else N.observe record event |> checked);
        Ok ()
      in
      let _, _, parsed, _, _, _ =
        Test_stream_parser.parse ~session ~same_task:true
          ~commands:(Test_provisional_function_parser.sink declaration)
          "I64 F(I64 n=40,I64 m=2,...);"
      in
      ignore (Test_parser.expect_ast parsed);
      Alcotest.(check bool) "successor was exercised" true !failed;
      let record = Option.get !saved in
      Alcotest.(check bool)
        "expired skipped event cannot repair native state" true
        (Result.is_error (N.observe record (Option.get !skipped))))
    [ "parameter"; "default"; "completion"; "ellipsis"; "variadic" ]

let unknown_body_members () =
  let _, records, _ =
    fixture
      "I64 F(I64 n)#exe {I64 F(I64 x,I64 y){I64 local;return x+y;}}{return n;}"
  in
  let outer = N.snapshot (snd (List.hd records)) in
  Alcotest.(check (option int))
    "body local prevents guessing resumed native count" None
    (N.argument_count outer);
  Alcotest.(check bool)
    "unmodeled body count cannot grant a call shape" true
    (Result.is_error (N.call_shape outer))

let activation_replay () =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let registry =
    N.create_registry ~mode:Preprocessor.Jit ~table ~namespace |> checked
  in
  let events = ref [] and observed = ref 0 and publication = ref None in
  let record = ref None and saved_activation = ref None in
  let initial = ref None in
  let declaration event =
    events := A.Declaration event :: !events;
    (match event with
    | Parser.Function_declared source ->
        publication := Some (C.publish_function namespace source |> checked)
    | _ -> ());
    Ok ()
  in
  let checkpoint event =
    incr observed;
    events := A.Command event :: !events;
    (match event with
    | Parser.Command_completed command ->
        let activation =
          A.create ~namespace ~context:command.command_start.command_context
            ~observed_events:!observed (List.rev !events)
          |> checked
        in
        saved_activation := Some activation;
        A.run activation ~invalid:"invalid activation" (function
          | A.Declaration (Parser.Function_declared source) ->
              Alcotest.(check bool)
                "delayed source needs matching activation" true
                (Result.is_error
                   (N.begin_header registry (Option.get !publication) source));
              record :=
                Some
                  (N.begin_header ~activation registry (Option.get !publication)
                     source
                  |> checked);
              initial := Some (N.snapshot (Option.get !record));
              Ok ()
          | A.Declaration event ->
              let record = Option.get !record in
              if N.event_belongs record event then (
                Alcotest.(check bool)
                  "delayed phase has no live authority" true
                  (Result.is_error (N.observe record event));
                N.observe ~activation record event |> checked;
                Alcotest.(check bool)
                  "active source phase is single use" true
                  (Result.is_error (N.observe ~activation record event)));
              Ok ()
          | _ -> Ok ())
        |> checked
    | _ -> ());
    Ok ()
  in
  let commands =
    {
      (Test_provisional_function_parser.sink declaration) with
      Parser.checkpoint = Some checkpoint;
    }
  in
  let _, _, parsed, _, _, _ =
    Test_stream_parser.parse ~session ~same_task:true ~commands
      "I64 F(I64 n=40,I64 m=2,...){return n+m;}"
  in
  ignore (Test_parser.expect_ast parsed);
  let final = N.snapshot (Option.get !record) in
  count "original activation completes native count" 2 final;
  Alcotest.(check bool)
    "original source activation creates checked forward ancestry" true
    (Result.is_ok (N.transition ~earlier:(Option.get !initial) ~later:final));
  Alcotest.(check (option bool))
    "activated body ends extern lifecycle" (Some false) (N.is_extern final);
  Alcotest.(check bool)
    "consumed activation cannot replay again" true
    (Result.is_error
       (A.run (Option.get !saved_activation) ~invalid:"consumed" (fun _ ->
            Ok ())))

let bound_lifecycle_unavailable () =
  let _, records, _ = fixture "_intern 42 I64 F(I64 n);I64 F(I64 x);" in
  let final = N.snapshot (snd (List.hd (List.rev records))) in
  Alcotest.(check bool)
    "unmodeled bound lifecycle cannot grant extern reuse" true
    (Result.is_error (N.call_shape final))

let duplicate_native_members () =
  List.iter
    (fun parameters ->
      let _, records, _ = fixture ("extern I64 F(" ^ parameters ^ ");") in
      let final = N.snapshot (snd (List.hd records)) in
      Alcotest.(check bool)
        "rejected MemberAdd cannot grant a call shape" true
        (Result.is_error (N.call_shape final));
      Alcotest.(check (option int))
        "invalid insertion grants no argument count" None
        (N.argument_count final))
    [ "I64 n,I64 n"; "I64 argc,..."; "I64 argv,..." ];
  let _, records, _ = fixture "extern I64 F(I64 pad,I64 pad);" in
  let final = N.snapshot (snd (List.hd records)) in
  count "native duplicate-name exemption is retained" 2 final;
  Alcotest.(check int)
    "exempt members retain two fixed slots" 2
    (List.length (fixed final))

let nested_native_duplicates () =
  let _, records, _ =
    fixture "I64 F(I64 n,#exe {extern I64 F(I64 m);}I64 m);"
  in
  let final = N.snapshot (snd (List.hd records)) in
  Alcotest.(check bool)
    "collision tests current native members after reuse" true
    (Result.is_error (N.call_shape final));
  let _, records, _ =
    fixture "I64 F(I64 n,#exe {extern I64 F(I64 m);}I64 n);"
  in
  let final = N.snapshot (snd (List.hd records)) in
  count "replaced source member is not a current native collision" 2 final;
  Alcotest.(check int)
    "native cursor retains inner m and resumed outer n" 2
    (List.length (fixed final))

let explicit_alias_lifecycle () =
  List.iter
    (fun (prefix, reused) ->
      let published = ref false in
      let inspect _ event =
        match event with
        | Parser.Function_header_completed header when not !published ->
            published := true;
            let environment =
              header.function_publication.function_environment
            in
            let first =
              Symbol_visibility.Environment.add_function_alias environment
                ~original_entry:header.completed_entry ()
              |> checked
            in
            ignore
              (Symbol_visibility.Environment.add_function_alias environment
                 ~original_entry:first ()
              |> checked)
        | _ -> ()
      in
      let _, records, samples =
        fixture ~inspect (prefix ^ "I64 F(I64 x)#exe {};")
      in
      match (records, samples) with
      | [ (_, first); (_, second) ], [ partial ] ->
          Alcotest.(check bool)
            "alias preserves actual extern allocation rule" reused
            (N.same_identity (N.snapshot first) (N.snapshot second));
          count "alias lineage gives checked zero active count" 0 partial;
          count "alias successor completes native member count" 1
            (N.snapshot second);
          Alcotest.(check bool)
            "alias successor has a checked call shape" true
            (Result.is_ok (N.call_shape (N.snapshot second)))
      | _ -> Alcotest.fail "expected aliased source and successor")
    [ ("extern I64 F(I64 n);", true); ("I64 F(I64 n){return n;}", false) ]

let cloned_entry_is_not_native_alias () =
  let published = ref false in
  let inspect _ event =
    match event with
    | Parser.Function_header_completed header when not !published ->
        published := true;
        let entry = header.completed_entry in
        ignore
          (Symbol_visibility.Environment.add
             ~origin:(Symbol_visibility.origin entry)
             ?function_call_shape:(Symbol_visibility.function_call_shape entry)
             header.function_publication.function_environment
             ~name:(Symbol_visibility.name entry)
             ~kind:Symbol_visibility.Function ())
    | _ -> ()
  in
  let _, records, _ = fixture ~inspect "extern I64 F(I64 n);I64 F(I64 x);" in
  let final = N.snapshot (snd (List.hd (List.rev records))) in
  Alcotest.(check bool)
    "same name origin and shape do not establish native ancestry" true
    (Result.is_error (N.call_shape final))

let nested_explicit_aliases () =
  let outer = ref None in
  let inspect _ event =
    match event with
    | Parser.Function_declared source when Option.is_none !outer ->
        outer := Some source;
        let first =
          Symbol_visibility.Environment.add_function_alias
            source.function_environment ~original_entry:source.function_entry ()
          |> checked
        in
        ignore
          (Symbol_visibility.Environment.add_function_alias
             source.function_environment ~original_entry:first ()
          |> checked)
    | _ -> ()
  in
  let _, records, samples =
    fixture ~inspect
      "I64 F(I64 n)#exe {I64 F(I64 x,I64 y){return x+y;}}{return n;}"
  in
  match (records, samples) with
  | [ (_, outer); (inner_source, inner) ], [ suspended ] -> (
      Alcotest.(check bool)
        "nested explicit alias selects shared native record" true
        (N.same_identity (N.snapshot outer) (N.snapshot inner));
      count "nested alias keeps suspended count immutable" 0 suspended;
      count "outer resume follows aliased inner native state" 2
        (N.snapshot outer);
      match inner_source.function_previous with
      | Symbol_visibility.Present entry ->
          Alcotest.(check bool)
            "parser retains original explicit alias lookup" true
            (Option.is_some (Symbol_visibility.function_alias_original entry))
      | _ -> Alcotest.fail "expected original alias selection")
  | _ -> Alcotest.fail "expected nested aliased publications"

let older_alias_rejects_without_mutation () =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let registry =
    N.create_registry ~mode:Preprocessor.Jit ~table ~namespace |> checked
  in
  let records = ref [] and headers = ref [] and rejected = ref false in
  let declaration event =
    (match event with
    | Parser.Function_declared source ->
        let publication = C.publish_function namespace source |> checked in
        if List.length !records = 2 then (
          let before = List.map N.snapshot !records in
          Alcotest.(check bool)
            "alias to known nonlatest source rejects" true
            (Result.is_error (N.begin_header registry publication source));
          Alcotest.(check bool)
            "old alias rejection preserves all native snapshots" true
            (List.for_all2 ( == ) before (List.map N.snapshot !records));
          rejected := true)
        else
          records :=
            (N.begin_header registry publication source |> checked) :: !records
    | _ -> (
        List.iter
          (fun record ->
            if N.event_belongs record event then
              N.observe record event |> checked)
          !records;
        match event with
        | Parser.Function_header_completed header ->
            headers := header :: !headers;
            if List.length !headers = 2 then
              let oldest = List.hd (List.rev !headers) in
              ignore
                (Symbol_visibility.Environment.add_function_alias
                   header.function_publication.function_environment
                   ~original_entry:oldest.completed_entry ()
                |> checked)
        | _ -> ()));
    Ok ()
  in
  let _, _, parsed, _, _, _ =
    Test_stream_parser.parse ~session ~same_task:true
      ~commands:(Test_provisional_function_parser.sink declaration)
      "extern I64 F(I64 a);extern I64 F(I64 b);extern I64 F(I64 c);"
  in
  ignore (Test_parser.expect_ast parsed);
  Alcotest.(check bool) "nonlatest alias successor was exercised" true !rejected

let forward_proof earlier later =
  let proof = N.transition ~earlier ~later |> checked in
  Alcotest.(check bool)
    "proof retains exact earlier snapshot" true
    (N.transition_earlier proof == earlier);
  Alcotest.(check bool)
    "proof retains exact later snapshot" true
    (N.transition_later proof == later)

let source_transition_order () =
  let phases = ref [] in
  let inspect record _ = phases := N.snapshot record :: !phases in
  ignore (fixture ~inspect "I64 F(I64 n=40,...){return n;}");
  let ordered = List.rev !phases in
  let rec consecutive = function
    | earlier :: (later :: _ as rest) ->
        forward_proof earlier later;
        Alcotest.(check bool)
          "backward source phase has no forward proof" true
          (Result.is_error (N.transition ~earlier:later ~later:earlier));
        consecutive rest
    | _ -> ()
  in
  consecutive ordered;
  forward_proof (List.hd ordered) (List.hd !phases);
  List.iter
    (fun phase ->
      Alcotest.(check bool)
        "same phase cannot produce a strict transition" true
        (Result.is_error (N.transition ~earlier:phase ~later:phase)))
    ordered;
  let body = List.hd !phases and header = List.nth !phases 1 in
  Alcotest.(check bool)
    "body transition is not fabricated from source transcript change" true
    (N.source_snapshot body == N.source_snapshot header);
  forward_proof header body

let rejected_events_have_no_transition () =
  let inspect record event =
    let earlier = N.snapshot record in
    Alcotest.(check bool)
      "replayed source event rejects" true
      (Result.is_error (N.observe record event));
    let later = N.snapshot record in
    Alcotest.(check bool)
      "rejected event creates no native ancestry" true
      (Result.is_error (N.transition ~earlier ~later));
    Alcotest.(check bool)
      "repeated readonly snapshots create no phase" true
      (Result.is_error (N.transition ~earlier:later ~later:(N.snapshot record)))
  in
  ignore (fixture ~inspect "I64 F(I64 n=40){return n;}")

let nested_transition_ancestry () =
  let phases = ref [] in
  let inspect record event =
    phases := (record, event, N.snapshot record) :: !phases
  in
  let _, records, samples =
    fixture ~inspect
      "I64 F(I64 n)#exe {I64 F(I64 x,I64 y){return x+y;}}{return n;}"
  in
  match (records, samples) with
  | [ (_, outer); (_, inner) ], [ suspended ] ->
      let inner_phases =
        List.filter_map
          (fun (record, _, state) ->
            if record == inner then Some state else None)
          (List.rev !phases)
      in
      let inner_start = List.hd inner_phases in
      let inner_end = List.hd (List.rev inner_phases) in
      let outer_end = N.snapshot outer in
      forward_proof suspended inner_start;
      forward_proof inner_start inner_end;
      forward_proof inner_end outer_end;
      forward_proof suspended outer_end;
      Alcotest.(check bool)
        "later outer phase cannot precede nested phase" true
        (Result.is_error (N.transition ~earlier:outer_end ~later:inner_end))
  | _ -> Alcotest.fail "expected nested source transition history"

let native_only_transition () =
  let outer = ref None and pair = ref None in
  let inspect record event =
    match (event, !outer) with
    | Parser.Function_declared _, None -> outer := Some record
    | Parser.Function_parameter_declared _, Some first
      when first != record && Option.is_none !pair ->
        pair := Some (N.snapshot first, N.snapshot record)
    | _ -> ()
  in
  let _, _, samples =
    fixture ~inspect "I64 F(I64 n)#exe {extern I64 F(I64 x);}{return n;}"
  in
  let suspended = List.hd samples in
  let outer_view, inner_view = Option.get !pair in
  Alcotest.(check bool)
    "outer source transcript remains unchanged during inner event" true
    (N.source_snapshot suspended == N.source_snapshot outer_view);
  forward_proof suspended outer_view;
  Alcotest.(check bool)
    "different source views at one native phase have no forward order" true
    (Result.is_error (N.transition ~earlier:outer_view ~later:inner_view)
    && Result.is_error (N.transition ~earlier:inner_view ~later:outer_view))

let foreign_transition_rejection () =
  let _, first, _ = fixture "extern I64 F();" in
  let _, second, _ = fixture "extern I64 F();" in
  let earlier = N.snapshot (snd (List.hd first)) in
  let later = N.snapshot (snd (List.hd second)) in
  Alcotest.(check bool)
    "same-looking foreign snapshots cannot establish ancestry" true
    (Result.is_error (N.transition ~earlier ~later));
  let _, records, _ = fixture "I64 F(){return 1;}I64 F(){return 2;}" in
  match records with
  | [ (_, first); (_, second) ] ->
      Alcotest.(check bool)
        "a fresh allocation is not a phase advance" true
        (Result.is_error
           (N.transition ~earlier:(N.snapshot first) ~later:(N.snapshot second)))
  | _ -> Alcotest.fail "expected two separate native allocations"

let reused_zero_transition () =
  let phases = ref [] in
  let inspect record event =
    match event with
    | Parser.Function_declared _ | Parser.Function_header_completed _ ->
        phases := N.snapshot record :: !phases
    | _ -> ()
  in
  ignore (fixture ~inspect "extern I64 F();extern I64 F();");
  match List.rev !phases with
  | [ _; earlier; later; _ ] ->
      count "previous native count already zero" 0 earlier;
      count "reused native count remains zero" 0 later;
      forward_proof earlier later
  | _ -> Alcotest.fail "expected completed then reused zero-count header"

let admission_phase = function
  | Parser.Function_declared _
  | Parser.Function_parameter_declared _
  | Parser.Parameter_default_completed _
  | Parser.Function_parameter_completed _
  | Parser.Function_variadic_started _
  | Parser.Function_variadic_completed _
  | Parser.Function_header_completed _ -> true
  | _ -> false

let original_event_snapshots () =
  let collect () =
    let phases = ref [] in
    let inspect record event =
      let snapshot = N.snapshot record in
      if admission_phase event then (
        Alcotest.(check bool)
          "successful event has native evidence" true
          (N.matches_event snapshot event);
        Alcotest.(check bool)
          "unchanged reads retain event evidence" true
          (N.matches_event (N.snapshot record) event);
        Alcotest.(check bool)
          "rejected replay leaves evidence intact" true
          (Result.is_error (N.observe record event));
        Alcotest.(check bool)
          "failed observation preserves original evidence" true
          (N.matches_event (N.snapshot record) event);
        phases := (event, snapshot) :: !phases)
      else
        Alcotest.(check bool)
          "body has no declaration admission authority" false
          (N.matches_event snapshot event)
    in
    ignore (fixture ~inspect "I64 F(I64 n=40,...){return n;}");
    List.rev !phases
  in
  let original = collect () and foreign = collect () in
  Alcotest.(check int)
    "all seven declaration phases observed" 7 (List.length original);
  List.iteri
    (fun index (event, snapshot) ->
      List.iteri
        (fun other (_, candidate) ->
          Alcotest.(check bool)
            "only original phase matches after parsing" (index = other)
            (N.matches_event candidate event))
        original;
      List.iter
        (fun (candidate, _) ->
          Alcotest.(check bool)
            "equal-looking foreign receipt grants no evidence" false
            (N.matches_event snapshot candidate))
        foreign)
    original

let nested_event_snapshot_authority () =
  let outer = ref None and retained = ref None and sampled = ref false in
  let inspect record event =
    match (event, !outer) with
    | Parser.Function_declared _, None -> outer := Some record
    | Parser.Function_parameter_completed _, Some first when record == first ->
        retained := Some (event, N.snapshot first)
    | Parser.Function_parameter_declared _, Some first when record != first ->
        let original_event, original = Option.get !retained in
        let changed = N.snapshot first in
        Alcotest.(check bool)
          "suspended original event remains authenticated" true
          (N.matches_event original original_event);
        Alcotest.(check bool)
          "later shared mutation cannot impersonate outer event" false
          (N.matches_event changed original_event);
        Alcotest.(check bool)
          "outer view cannot authenticate inner event" false
          (N.matches_event changed event);
        Alcotest.(check bool)
          "inner event retains its own native revision" true
          (N.matches_event (N.snapshot record) event);
        sampled := true
    | _ -> ()
  in
  ignore (fixture ~inspect "I64 F(I64 n)#exe {extern I64 F(I64 x);}{return n;}");
  Alcotest.(check bool) "nested shared-record sample exercised" true !sampled

let tests =
  [
    Alcotest.test_case "fresh and reused headers clear active count" `Quick
      fresh_and_reused;
    Alcotest.test_case "ellipsis uses actual native member cursor" `Quick
      variadic_cursor;
    Alcotest.test_case "definition ends extern reuse" `Quick
      definition_allocates;
    Alcotest.test_case "outer resume retains nested shared native state" `Quick
      nested_shared_record;
    Alcotest.test_case "reentrant stale member remains unavailable" `Quick
      nested_stale_member;
    Alcotest.test_case "native traversal rejects synthetic fixed projection"
      `Quick nested_synthetic_fixed_slot;
    Alcotest.test_case "repeated foreign and expired phases reject" `Quick
      rejections;
    Alcotest.test_case "mode and namespace enforce native ownership" `Quick
      mode_and_ownership;
    Alcotest.test_case "unknown predecessor never grants fresh counts" `Quick
      unknown_lineage;
    Alcotest.test_case "untracked replacement preserves normal source parsing"
      `Quick intervening_untracked_lineage;
    Alcotest.test_case "sticky ellipsis flag does not imply synthetic tail"
      `Quick sticky_flag_without_tail;
    Alcotest.test_case "missing native source phases reject successors" `Quick
      missing_events;
    Alcotest.test_case "body locals make resumed native count unavailable"
      `Quick unknown_body_members;
    Alcotest.test_case "original source activation updates native phases once"
      `Quick activation_replay;
    Alcotest.test_case "bound lifecycle requires separate executable evidence"
      `Quick bound_lifecycle_unavailable;
    Alcotest.test_case "duplicate native members cannot grant count authority"
      `Quick duplicate_native_members;
    Alcotest.test_case "duplicate checks follow the reused native member cursor"
      `Quick nested_native_duplicates;
    Alcotest.test_case
      "explicit aliases preserve extern versus defined identity" `Quick
      explicit_alias_lifecycle;
    Alcotest.test_case "generic clones do not create native aliases" `Quick
      cloned_entry_is_not_native_alias;
    Alcotest.test_case "nested explicit aliases retain mutable native lineage"
      `Quick nested_explicit_aliases;
    Alcotest.test_case "known nonlatest aliases reject without native mutation"
      `Quick older_alias_rejects_without_mutation;
    Alcotest.test_case "source transitions require strict forward ancestry"
      `Quick source_transition_order;
    Alcotest.test_case "rejected and readonly events create no transition"
      `Quick rejected_events_have_no_transition;
    Alcotest.test_case
      "nested and returning outer phases retain forward ancestry" `Quick
      nested_transition_ancestry;
    Alcotest.test_case
      "native-only events advance an unchanged outer transcript" `Quick
      native_only_transition;
    Alcotest.test_case "foreign and fresh native records have no transition"
      `Quick foreign_transition_rejection;
    Alcotest.test_case "reused zero counts still have a real native transition"
      `Quick reused_zero_transition;
    Alcotest.test_case
      "native snapshots authenticate only original source events" `Quick
      original_event_snapshots;
    Alcotest.test_case "nested native reads cannot impersonate earlier events"
      `Quick nested_event_snapshot_authority;
  ]
