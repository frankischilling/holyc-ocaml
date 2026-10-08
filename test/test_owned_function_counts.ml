open Holyc_lib
module N = Semantic_function_record_phase
module C = Semantic_declaration_collection

let checked = Test_declaration_collection.checked
let check = Alcotest.(check bool)

let fixture ?(lexical = true) ?(prepare = fun _ -> ())
    ?(inspect = fun _ _ -> ()) ?(read = fun _ _ _ -> ())
    ?(before_read = fun _ _ _ -> ()) text =
  let session = Session.create () in
  prepare session;
  let source =
    Session.add_source session ~path:"owned-function-counts.hc" ~contents:text
  in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let registry =
    N.create_registry ~mode:Preprocessor.Jit ~table ~namespace |> checked
  in
  let records = ref [] in
  let declaration event =
    (match event with
    | Parser.Function_declared source ->
        let publication = C.publish_function namespace source |> checked in
        let record = N.begin_header registry publication source |> checked in
        records := (source, record) :: !records;
        inspect record event
    | Parser.Function_local_allocated receipt ->
        let record = List.assq receipt.allocation_function !records in
        N.observe_local_allocation record receipt |> checked;
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
  let consume context lookup =
    if !records <> [] then (
      before_read registry context lookup;
      N.observe_lexical_lookup registry context lookup |> checked;
      read registry context lookup);
    Ok ()
  in
  let commands : Parser.command_sink =
    {
      lexical_lookup = (if lexical then Some consume else None);
      checkpoint = None;
      reference = None;
      call = None;
      implicit_output = None;
      query = None;
      declaration = Some declaration;
      dimension_count = None;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let config = Preprocessor.Config.create () |> checked in
  let output =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~symbols:(Session.symbols session)
      ~definitions:(Session.definitions session)
      ~config source
  in
  if Parser.has_errors output then
    Alcotest.fail
      (String.concat "; "
         (List.map (fun d -> d.Diagnostic.message) output.diagnostics));
  (session, registry, List.rev !records, output)

let warnings output =
  List.filter
    (fun d -> d.Diagnostic.code = "HCSEMA0075")
    output.Parser.diagnostics

let early_reuse_and_reset () =
  let counts = ref [] and warning_counts = ref [] in
  let _, _, records, output =
    fixture
      "extern I64 OwnedHashCounterSubject(); extern I64 \
       OwnedHashCounterSubject(); extern I64 OwnedHashCounterSubject();"
      ~inspect:(fun record event ->
        match event with
        | Parser.Function_declared source ->
            counts := N.use_count record :: !counts;
            let context =
              source.function_header.declaration_command.command_context
            in
            warning_counts :=
              (Parser.context_warning_count context |> checked)
              :: !warning_counts
        | _ -> ())
  in
  Alcotest.(check (list (option int64)))
    "reset before parameter input"
    [ Some 0L; Some 0L; Some 0L ]
    (List.rev !counts);
  Alcotest.(check int)
    "each unused reuse warns" 2
    (List.length (warnings output));
  Alcotest.(check (list int64))
    "warning counter at the original join" [ 0L; 1L; 2L ]
    (List.rev !warning_counts);
  let snapshots = List.map (fun (_, record) -> N.snapshot record) records in
  let first = List.hd snapshots in
  List.iter
    (fun snapshot ->
      check "extern reuse shares original object" true
        (N.same_identity first snapshot))
    snapshots

let reference_suppresses_and_cursors_stay () =
  let selected_count = ref None and retained = ref None in
  let records_seen = ref [] in
  let before_snapshot = ref None in
  let _, registry, _, output =
    fixture
      "extern I64 OwnedHashCounterSubject(); OwnedHashCounterSubject; extern \
       I64 OwnedHashCounterSubject();"
      ~inspect:(fun record event ->
        match event with
        | Parser.Function_declared _ -> records_seen := record :: !records_seen
        | _ -> ())
      ~before_read:(fun _ _ lookup ->
        if
          (Preprocessor.lexical_lookup_token lookup).Token.raw
          = "OwnedHashCounterSubject"
        then before_snapshot := Some (N.snapshot (List.hd !records_seen)))
      ~read:(fun registry context lookup ->
        if
          (Preprocessor.lexical_lookup_token lookup).Token.raw
          = "OwnedHashCounterSubject"
        then (
          let record = List.hd !records_seen in
          let before = Option.get !before_snapshot in
          selected_count := N.use_count record;
          check "duplicate cannot increment" true
            (Result.is_error (N.observe_lexical_lookup registry context lookup));
          let cross =
            Domain.spawn (fun () ->
                Result.is_error
                  (N.observe_lexical_lookup registry context lookup))
          in
          check "other domain cannot increment" true (Domain.join cross);
          let after = N.snapshot record in
          check "count does not advance executable revision" true
            (N.same_revision before after);
          check "count does not grant a transition" true
            (Result.is_error (N.transition ~earlier:before ~later:after));
          retained := Some (context, lookup)))
  in
  Alcotest.(check int)
    "one source reference reaches threshold" 0
    (List.length (warnings output));
  Alcotest.(check (option int64))
    "two lexer selections before join" (Some 2L) !selected_count;
  let context, lookup = Option.get !retained in
  check "expired original read rejects" true
    (Result.is_error (N.observe_lexical_lookup registry context lookup))

let missing_reads_are_unknown () =
  let _, _, records, output =
    fixture ~lexical:false
      "extern I64 OwnedHashCounterSubject(); OwnedHashCounterSubject;"
  in
  check "omitted source read leaves no assumed zero" true
    (Option.is_none (N.use_count (snd (List.hd records))));
  Alcotest.(check int) "no reuse warning" 0 (List.length (warnings output));
  let _, _, _, output =
    fixture ~lexical:false
      "extern I64 OwnedHashCounterSubject(); extern I64 \
       OwnedHashCounterSubject();"
  in
  Alcotest.(check int)
    "unobserved totals cannot warn" 0
    (List.length (warnings output))

let local_shadow_keeps_hash_unused () =
  let subject = ref None and local_reads = ref 0 in
  let _, _, _, output =
    fixture
      "extern I64 OwnedHashCounterSubject(); I64 OwnedHashCounterOther(I64 \
       OwnedHashCounterSubject){return OwnedHashCounterSubject;} extern I64 \
       OwnedHashCounterSubject();"
      ~inspect:(fun record event ->
        match event with
        | Parser.Function_declared source
          when source.function_name.spelling = "OwnedHashCounterSubject" ->
            subject := Some record
        | _ -> ())
      ~read:(fun _ _ lookup ->
        match Preprocessor.lexical_lookup_selection lookup with
        | Symbol_visibility.Shadowed_by_local ->
            incr local_reads;
            Alcotest.(check (option int64))
              "only the parameter name before MemberAdd used the hash" (Some 1L)
              (N.use_count (Option.get !subject))
        | _ -> ())
  in
  check "original member lookup reached" true (!local_reads > 0);
  Alcotest.(check int)
    "the pre-publication parameter name reaches the threshold" 0
    (List.length (warnings output))

let defined_record_is_filtered_after_count () =
  let _, _, records, output =
    fixture "I64 OwnedHashCounterSubject(); I64 OwnedHashCounterSubject();"
  in
  let first = snd (List.nth records 0) and second = snd (List.nth records 1) in
  Alcotest.(check (option int64))
    "the selected defined record still receives both finds" (Some 2L)
    (N.use_count first);
  check "defined predecessor creates another allocation" false
    (N.same_identity (N.snapshot first) (N.snapshot second));
  Alcotest.(check int)
    "defined predecessor does not warn" 0
    (List.length (warnings output))

let reached_failure_keeps_early_warning () =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"early-extern-error.hc"
      ~contents:
        "#exe {extern I64 OwnedHashCounterSubject(); extern I64 \
         OwnedHashCounterSubject(NoSuchType arg);} 42;"
  in
  let config = Preprocessor.Config.create () |> checked in
  match
    run_integer_program_report session ~source ~config ~max_steps:100_000
    |> integer_program_report_outcome
  with
  | Ok _ -> Alcotest.fail "invalid parameter unexpectedly compiled"
  | Error diagnostics ->
      check "warning reaches failure before parameter error" true
        (List.exists (fun d -> d.Diagnostic.code = "HCSEMA0075") diagnostics)

let native_storage_and_collection () =
  let module H = Holyc_lib__Common.Native_hash_record in
  (match Native_execution.platform () with
  | Native_execution.Unsupported -> ()
  | _ ->
      check "U32 field matches original INC including wrap" true
        (H.verify_storage ()));
  let observed = ref [] in
  let _, _, _, output =
    fixture
      "extern I64 OwnedHashCounterSubject(); OwnedHashCounterSubject; extern \
       I64 OwnedHashCounterSubject();" ~read:(fun registry context lookup ->
        Gc.full_major ();
        Gc.compact ();
        check "collection cannot revive a consumed read" true
          (Result.is_error (N.observe_lexical_lookup registry context lookup));
        observed := lookup :: !observed)
  in
  check "original counter survives actual collection" true (!observed <> []);
  Alcotest.(check int)
    "collected reference suppresses warning" 0
    (List.length (warnings output))

let alias_and_clone_ownership () =
  List.iter
    (fun alias ->
      let published = ref false in
      let inspect _ event =
        match event with
        | Parser.Function_header_completed header when not !published ->
            published := true;
            let environment =
              header.function_publication.function_environment
            in
            if alias then
              let first =
                Symbol_visibility.Environment.add_function_alias environment
                  ~original_entry:header.completed_entry ()
                |> checked
              in
              ignore
                (Symbol_visibility.Environment.add_function_alias environment
                   ~original_entry:first ()
                |> checked)
            else
              ignore
                (Symbol_visibility.Environment.add environment
                   ~origin:(Symbol_visibility.origin header.completed_entry)
                   ?function_call_shape:
                     (Symbol_visibility.function_call_shape
                        header.completed_entry)
                   ~name:"OwnedHashCounterSubject"
                   ~kind:Symbol_visibility.Function ())
        | _ -> ()
      in
      let _, _, records, output =
        fixture ~inspect
          "extern I64 OwnedHashCounterSubject(); OwnedHashCounterSubject; \
           extern I64 OwnedHashCounterSubject();"
      in
      let first = snd (List.nth records 0)
      and second = snd (List.nth records 1) in
      check "only explicit physical alias ancestry shares native storage" alias
        (N.same_identity (N.snapshot first) (N.snapshot second));
      Alcotest.(check (option int64))
        "cloned metadata cannot increment original storage" (Some 0L)
        (N.use_count first);
      Alcotest.(check (option int64))
        "clone successor has no assumed native count"
        (if alias then Some 0L else None)
        (N.use_count second);
      Alcotest.(check int)
        "one aliased source reference suppresses warning" 0
        (List.length (warnings output)))
    [ true; false ]

let untracked_predecessor () =
  let prepare session =
    ignore
      (Symbol_visibility.Environment.add (Session.symbols session)
         ~name:"OwnedHashCounterSubject" ~kind:Symbol_visibility.Function ())
  in
  let _, _, records, output =
    fixture ~prepare
      "extern I64 OwnedHashCounterSubject(); extern I64 \
       OwnedHashCounterSubject();"
  in
  List.iter
    (fun (_, record) ->
      Alcotest.(check (option int64))
        "metadata predecessor cannot establish native zero" None
        (N.use_count record))
    records;
  Alcotest.(check int)
    "unknown prior use cannot warn" 0
    (List.length (warnings output))

let missing_shared_view_read () =
  let original = ref None and visited = ref false in
  let inspect record event =
    match event with
    | Parser.Function_header_completed header when not !visited ->
        original := Some record;
        visited := true;
        let environment =
          Symbol_visibility.Environment.copy
            header.function_publication.function_environment
        in
        let session = Session.create () in
        let source =
          Session.add_source session ~path:"other-original-read.hc"
            ~contents:"OwnedHashCounterSubject"
        in
        let config = Preprocessor.Config.create () |> checked in
        let input =
          Preprocessor.create ~sources:(Session.sources session)
            ~definitions:(Session.definitions session)
            ~symbols:environment ~config source
        in
        ignore (Preprocessor.next input);
        Alcotest.(check (option int64))
          "shared original entry read makes omitted count unavailable" None
          (N.use_count record)
    | _ -> ()
  in
  let _, _, _, output =
    fixture ~inspect
      "extern I64 OwnedHashCounterSubject(); extern I64 \
       OwnedHashCounterSubject();"
  in
  check "copied original entry view exercised" true (Option.is_some !original);
  Alcotest.(check int)
    "missing other-view lookup cannot produce an unused warning" 0
    (List.length (warnings output))

let native_bucket_source_counts () =
  let module H = Holyc_lib__Common.Native_hash_record in
  Alcotest.(check int64)
    "different source names collide in the native bucket" (H.hash_string "AC")
    (H.hash_string "BA");
  let _, _, records, output =
    fixture
      "extern I64 AC(); extern I64 BA(); AC; extern I64 AC(); extern I64 BA();"
  in
  Alcotest.(check (list string))
    "only the unused colliding function warns" [ "Unused extern 'BA'" ]
    (List.map (fun d -> d.Diagnostic.message) (warnings output));
  let get name =
    List.filter
      (fun (source, _) -> source.Parser.function_name.spelling = name)
      records
  in
  List.iter
    (fun name ->
      let selected = get name in
      let first = N.snapshot (snd (List.hd selected)) in
      List.iter
        (fun (_, record) ->
          check "joined source preserves native record identity" true
            (N.same_identity first (N.snapshot record));
          Alcotest.(check (option int64))
            "original joined allocation reset" (Some 0L) (N.use_count record))
        selected)
    [ "AC"; "BA" ]

let tests =
  [
    Alcotest.test_case "original unused threshold, early warning and reset"
      `Quick early_reuse_and_reset;
    Alcotest.test_case "source reference, replay, domain and executable cursor"
      `Quick reference_suppresses_and_cursors_stay;
    Alcotest.test_case "missing original reads retain unknown counts" `Quick
      missing_reads_are_unknown;
    Alcotest.test_case "local member suppresses hash counting" `Quick
      local_shadow_keeps_hash_unused;
    Alcotest.test_case "defined function counts before the extern filter" `Quick
      defined_record_is_filtered_after_count;
    Alcotest.test_case "early warning survives a parameter failure" `Quick
      reached_failure_keeps_early_warning;
    Alcotest.test_case "native U32 INC layout, wrap and collection" `Quick
      native_storage_and_collection;
    Alcotest.test_case "explicit alias ancestry versus matching metadata" `Quick
      alias_and_clone_ownership;
    Alcotest.test_case "untracked predecessor cannot establish native zero"
      `Quick untracked_predecessor;
    Alcotest.test_case "omitted lookup in copied original entry view" `Quick
      missing_shared_view_read;
    Alcotest.test_case "native bucket collision preserves source counts" `Quick
      native_bucket_source_counts;
  ]
