open Holyc_lib
module T = Test_provisional_function_types
module H = Semantic_function_type_resolution
module R = Semantic_function_resolution
module N = Semantic_function_record_phase
module C = Semantic_declaration_collection
module F = Semantic_function_frame_layout
module S = Semantic_symbol_table

let checked = Test_declaration_collection.checked

let declaration_authority () =
  let table, namespace, snapshot = T.one "I64 F()#exe {};" in
  let function_ = T.resolve table namespace snapshot |> checked in
  List.iter
    (fun kind ->
      Alcotest.(check bool)
        ("call projection cannot become " ^ R.declaration_kind_name kind)
        true
        (Result.is_error (R.make_declaration ~function_ ~kind));
      Alcotest.(check bool)
        "compiler options do not turn call evidence into declaration authority"
        true
        (Result.is_error
           (R.make_declaration_with_options
              ~compiler_option_mask:Compiler_option.initial_mask ~function_
              ~kind)))
    [ R.Definition; R.Extern; R.Bound_extern; R.Import; R.Intern ]

let frame_fixture () =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let registry =
    N.create_registry ~mode:Preprocessor.Jit ~table ~namespace |> checked
  in
  let record = ref None and snapshot = ref None in
  let declaration event =
    (match event with
    | Parser.Function_declared source ->
        let publication = C.publish_function namespace source |> checked in
        record := Some (N.begin_header registry publication source |> checked)
    | _ ->
        Option.iter
          (fun record ->
            if N.event_belongs record event then
              N.observe record event |> checked)
          !record);
    Ok ()
  in
  let _, _, parsed, _, _, _ =
    Test_stream_parser.parse ~session ~same_task:true
      ~commands:(Test_provisional_function_parser.sink declaration)
      ~on_enter:(fun () -> snapshot := Some (N.snapshot (Option.get !record)))
      "I64 F()#exe {}{}"
  in
  let ast = Test_parser.expect_ast parsed and snapshot = Option.get !snapshot in
  let publication = N.publication snapshot in
  let symbol = C.publication_symbol publication in
  let declaration =
    C.make_declaration
      ~name:(Semantic_symbol.name symbol)
      ~declaration_kind:C.Function_definition
      ~origin:(Semantic_symbol.origin symbol)
      ~item_index:0 ()
    |> checked
  in
  let declarations =
    C.view namespace [ (publication, declaration) ] |> checked
  in
  let aggregates =
    Holyc_lib.resolve_aggregates session ~declarations ast |> checked
  in
  let functions =
    Holyc_lib.collect_functions session ~declarations ast |> checked
  in
  let function_types =
    Holyc_lib.resolve_function_types session ~declarations ~aggregates
      ~functions ast
    |> checked
  in
  let local_types =
    Holyc_lib.resolve_local_types session ~declarations ~aggregates ~functions
      ast
    |> checked
  in
  let bindings =
    Holyc_lib.index_function_bindings session ~declarations ~functions
      ~function_types ~local_types
    |> checked
  in
  let indexed_function =
    List.hd (Semantic_function_binding_index.functions bindings)
  in
  let typed_function = List.hd (H.functions function_types) in
  let local_function =
    List.hd (Semantic_local_type_resolution.functions local_types)
  in
  let input : F.function_input =
    { indexed_function; typed_function; local_function; locals = [] }
  in
  let parent = C.scope declarations in
  let aggregate_layouts =
    match Semantic_aggregate_layout.layout ~table ~parent [] with
    | Ok layouts -> layouts
    | Error error ->
        Alcotest.fail (Semantic_aggregate_layout.error_to_string error)
  in
  let layout input = F.layout ~table ~parent ~aggregate_layouts [ input ] in
  (table, namespace, snapshot, input, layout)

let frame_authority () =
  let table, namespace, snapshot, original, layout = frame_fixture () in
  Alcotest.(check bool)
    "completed ordinary source still makes a frame" true
    (Result.is_ok (layout original));
  ignore
    (R.make_declaration ~function_:original.typed_function ~kind:R.Definition
    |> checked);
  let scopes = List.length (S.all_scopes table) in
  let symbols = List.length (S.all_symbols table) in
  let typed_function =
    Holyc_lib__Driver.Function_type_resolution.resolve_provisional_call
      ~scope:(H.function_scope original.typed_function)
      ~table ~namespace
      (N.call_shape snapshot |> checked)
    |> checked
  in
  Alcotest.(check bool)
    "substitution retains exact ordinary function identity" true
    (H.function_symbol typed_function
     == H.function_symbol original.typed_function
    && H.function_scope typed_function
       == H.function_scope original.typed_function
    && H.function_item_index typed_function
       = H.function_item_index original.typed_function);
  Alcotest.(check int)
    "both headers have the same empty parameter shape" 0
    (List.length (H.signature_parameters (H.function_signature typed_function)));
  let substituted = { original with F.typed_function } in
  Test_function_frame_layout.expect_frame_error "HCSEMA0069"
    Test_function_frame_layout.invalid_input_error (layout substituted);
  Alcotest.(check int)
    "failed admission allocates no scope" scopes
    (List.length (S.all_scopes table));
  Alcotest.(check int)
    "failed admission allocates no symbols" symbols
    (List.length (S.all_symbols table));
  Alcotest.(check bool)
    "original completed frame remains usable" true
    (Result.is_ok (layout original))

let tests =
  [
    Alcotest.test_case
      "call-only projection cannot become an ordinary declaration" `Quick
      declaration_authority;
    Alcotest.test_case
      "matching call-only projection cannot authorize a body frame" `Quick
      frame_authority;
  ]

module VM = Ir_integer_interpreter
module FC = Semantic_function_record_classification
module FD = Holyc_lib__Driver.Function_type_resolution
module FCD = Holyc_lib__Driver.Function_record_classification
module Outer = Semantic_outer_environment
module Retained = Holyc_lib__Ir.Retained_function
module Activation = Holyc_lib__Sema.Source_activation

let vm_reject label result =
  Alcotest.(check bool) label true (Result.is_error result)

let vm_same_head label before after =
  Alcotest.(check bool)
    label true
    (match (before, after) with
    | None, None -> true
    | Some before, Some after -> before == after
    | _ -> false)

let vm_phase_records ~table ~namespace runtime snapshot =
  let previous =
    VM.function_record_head runtime snapshot
    |> Option.map (fun reference ->
        Retained.metadata reference |> Outer.function_classified_declaration)
  in
  let scope =
    Option.map
      (fun previous ->
        FC.classified_declaration_source previous
        |> R.resolved_declaration_site |> R.declaration_site_function
        |> H.function_scope)
      previous
  in
  let function_ =
    FD.resolve_provisional_call ?scope ~table ~namespace
      (N.call_shape snapshot |> checked)
    |> checked
  in
  let fact =
    match previous with
    | None ->
        R.make_provisional_declaration ~table ~namespace
          ~compiler_option_mask:Compiler_option.initial_mask ~function_
    | Some classified ->
        let current = FC.classified_declaration_source classified in
        let earlier =
          R.resolved_declaration_site current
          |> R.declaration_site_native_snapshot |> Option.get
        in
        let transition = N.transition ~earlier ~later:snapshot |> checked in
        R.make_provisional_advance ~table ~namespace ~pending:current ~current
          ~transition ~compiler_option_mask:Compiler_option.initial_mask
          ~function_ ()
  in
  let previous = Option.to_list previous in
  let resolution =
    R.resolve
      ~previous:(List.map FC.classified_declaration_source previous)
      ~table
      ~parent:(C.namespace_scope namespace)
      ~compilation_mode:R.Jit
      [ checked fact ]
    |> checked
  in
  let classify () =
    FCD.classify_publication ~previous ~resolution (N.source snapshot)
    |> checked
  in
  (classify (), classify ())

let vm_live_phase_authority () =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let foreign_namespace = C.create_namespace ~table () |> checked in
  let runtime = VM.create_task_state ~table () |> checked in
  let foreign = VM.create_task_state ~table () |> checked in
  VM.bind_task_namespace runtime namespace |> checked;
  VM.bind_task_namespace foreign namespace |> checked;
  let registry =
    N.create_registry ~mode:Preprocessor.Jit ~table ~namespace |> checked
  in
  let native = ref None and first = ref None and last_records = ref None in
  let admissions = ref 0 in
  let declaration event =
    (match event with
    | Parser.Function_declared publication ->
        let published = C.publish_function namespace publication |> checked in
        native := Some (N.begin_header registry published publication |> checked)
    | _ ->
        let native = Option.get !native in
        if N.event_belongs native event then N.observe native event |> checked);
    (match event with
    | Parser.Function_declared _
    | Parser.Function_parameter_declared _
    | Parser.Parameter_default_completed _
    | Parser.Function_parameter_completed _
    | Parser.Function_variadic_started _
    | Parser.Function_variadic_completed _ ->
        let snapshot = N.snapshot (Option.get !native) in
        VM.check_function_phase_source runtime ~namespace ~event snapshot
        |> checked;
        (* Classify the same resolved fact twice, without resolving a consumed
           source transition twice. Both retain the same exact predecessor. *)
        let records, competing =
          vm_phase_records ~table ~namespace runtime snapshot
        in
        let before = VM.function_record_head runtime snapshot in
        let reject_without_publication label target namespace event snapshot
            records =
          let prior = VM.function_record_head target snapshot in
          vm_reject label
            (VM.admit_function_phase target ~namespace ~event ~snapshot ~records);
          vm_same_head "rejection preserves exact catalog head" prior
            (VM.function_record_head target snapshot)
        in
        reject_without_publication "foreign task lacks original command order"
          foreign namespace event snapshot records;
        reject_without_publication "same-table foreign namespace is denied"
          runtime foreign_namespace event snapshot records;
        Option.iter
          (fun (earlier_event, earlier_snapshot) ->
            reject_without_publication
              "another source phase cannot match snapshot" runtime namespace
              earlier_event snapshot records;
            reject_without_publication
              "current callback cannot revive old snapshot" runtime namespace
              event earlier_snapshot records)
          !first;
        Option.iter
          (fun stale_records ->
            reject_without_publication
              "classified records must own exact snapshot" runtime namespace
              event snapshot stale_records)
          !last_records;
        VM.admit_function_phase runtime ~namespace ~event ~snapshot ~records
        |> checked;
        let admitted = VM.function_record_head runtime snapshot in
        Alcotest.(check bool)
          "successful phase publishes a new current head" true
          (match (before, admitted) with
          | None, Some _ -> true
          | Some before, Some after -> before != after
          | _ -> false);
        reject_without_publication "phase admission is single use" runtime
          namespace event snapshot records;
        reject_without_publication
          "competing authentic predecessor is now stale" runtime namespace event
          snapshot competing;
        vm_same_head "rejected competing branch cannot replace admitted head"
          admitted
          (VM.function_record_head runtime snapshot);
        if Option.is_none !first then first := Some (event, snapshot);
        last_records := Some records;
        incr admissions
    | _ -> ());
    Ok ()
  in
  let commands =
    {
      (Test_provisional_function_parser.sink declaration) with
      Parser.checkpoint =
        Some
          (fun event ->
            VM.observe_task_source_event runtime event |> checked;
            Ok ());
    }
  in
  let _, _, parsed, _, _, _ =
    Test_stream_parser.parse ~session ~same_task:true ~commands
      "extern I64 F(I64 n=40,...);"
  in
  ignore (Test_parser.expect_ast parsed);
  Alcotest.(check int) "six provisional phases admitted" 6 !admissions;
  let event, snapshot = Option.get !first in
  vm_reject "expired original callback cannot authorize another admission"
    (VM.check_function_phase_source runtime ~namespace ~event snapshot)

let vm_replayed_phase_authority revoke () =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let runtime = VM.create_task_state ~table () |> checked in
  let registry =
    N.create_registry ~mode:Preprocessor.Jit ~table ~namespace |> checked
  in
  let events = ref [] and observed = ref 0 and promoted = ref false in
  let exercised = ref false in
  let checkpoint event =
    incr observed;
    events := Activation.Command event :: !events;
    if !promoted then VM.observe_task_source_event runtime event |> checked;
    Ok ()
  in
  let declaration event =
    events := Activation.Declaration event :: !events;
    (match event with
    | Parser.Function_declared publication ->
        let published = C.publish_function namespace publication |> checked in
        let native = N.begin_header registry published publication |> checked in
        let snapshot = N.snapshot native in
        let records, _ = vm_phase_records ~table ~namespace runtime snapshot in
        let activation =
          Activation.create ~namespace
            ~context:
              publication.function_header.declaration_command.command_context
            ~observed_events:!observed (List.rev !events)
          |> checked
        in
        VM.promote_task_source_activation runtime ~namespace ~activation
          ~dimensions:[]
        |> checked;
        promoted := true;
        let admit () =
          VM.admit_function_phase runtime ~namespace ~event ~snapshot ~records
        in
        Alcotest.(check bool)
          "original declaration callback remains live" true
          (Parser.function_publication_is_current publication);
        vm_reject "journaled live declaration is denied before replay"
          (admit ());
        vm_same_head "pre-replay rejection publishes nothing" None
          (VM.function_record_head runtime snapshot);
        let replayed =
          Activation.run activation ~invalid:"inactive" (function
            | Activation.Declaration active when active == event ->
                admit () |> checked;
                exercised := true;
                Ok ()
            | _ ->
                vm_reject "earlier event cannot admit later live declaration"
                  (admit ());
                if revoke then Error "abort before declaration" else Ok ())
        in
        if revoke then (
          vm_reject "earlier failure revokes declaration replay" replayed;
          vm_reject "failed replay cannot fall back to still-live callback"
            (admit ());
          vm_same_head "failed replay publishes no function" None
            (VM.function_record_head runtime snapshot);
          exercised := true)
        else checked replayed;
        vm_reject "finished or failed journal cannot replay again"
          (Activation.run activation ~invalid:"consumed" (fun _ -> Ok ()))
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
      "extern I64 F();"
  in
  ignore (Test_parser.expect_ast parsed);
  Alcotest.(check bool) "replay boundary exercised" true !exercised

let vm_replayed_header_authority revoke () =
  let module CR = Semantic_compiler_record in
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let runtime = VM.create_task_state ~table () |> checked in
  let events = ref [] and observed = ref 0 and promoted = ref false in
  let publication = ref None and admissions = ref 0 in
  let checkpoint event =
    incr observed;
    events := Activation.Command event :: !events;
    if !promoted then VM.observe_task_source_event runtime event |> checked;
    Ok ()
  in
  let declaration event =
    events := Activation.Declaration event :: !events;
    (match event with
    | Parser.Function_declared source ->
        publication := Some (C.publish_function namespace source |> checked)
    | Parser.Function_header_completed header ->
        let source =
          CR.declare_function ~table ~namespace (Option.get !publication) header
          |> checked
        in
        let function_ =
          FD.resolve_completed_header ~table ~namespace source |> checked
        in
        let fact =
          R.make_pending_declaration ~table ~namespace
            ~compiler_option_mask:Compiler_option.initial_mask ~source
            ~function_
          |> checked
        in
        let resolution =
          R.resolve ~previous:[] ~table
            ~parent:(C.namespace_scope namespace)
            ~compilation_mode:R.Jit [ fact ]
          |> checked
        in
        let records =
          FCD.classify_completed_header ~previous:[] ~resolution source
          |> checked
        in
        let admit () =
          VM.admit_function_header runtime ~namespace ~source ~records
        in
        if not !promoted then (
          let activation =
            Activation.create ~namespace
              ~context:
                header.function_publication.function_header.declaration_command
                  .command_context
              ~observed_events:!observed (List.rev !events)
            |> checked
          in
          VM.promote_task_source_activation runtime ~namespace ~activation
            ~dimensions:[]
          |> checked;
          promoted := true;
          let absent () =
            Alcotest.(check bool)
              "denied header does not publish a runtime function" true
              (Option.is_none
                 (VM.admitted_publication_for_symbol runtime
                    (H.function_symbol function_)))
          in
          Alcotest.(check bool)
            "original header callback remains live" true
            (Parser.function_header_is_current header);
          vm_reject "journaled header cannot admit before replay" (admit ());
          absent ();
          let replayed =
            Activation.run activation ~invalid:"inactive" (function
              | Activation.Declaration active when active == event ->
                  admit () |> checked;
                  incr admissions;
                  Ok ()
              | _ ->
                  vm_reject "another replay event cannot admit a live header"
                    (admit ());
                  absent ();
                  if revoke then Error "abort before header" else Ok ())
          in
          if revoke then (
            vm_reject "header replay aborted" replayed;
            vm_reject "failed replay cannot revive a live header" (admit ());
            absent ())
          else checked replayed)
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
      "extern I64 F();"
  in
  ignore (Test_parser.expect_ast parsed);
  Alcotest.(check int)
    "only the exact successful header event publishes"
    (if revoke then 0 else 1)
    !admissions

let vm_phase_tests =
  [
    Alcotest.test_case
      "VM admits exact live native phases and rejects stale catalogs" `Quick
      vm_live_phase_authority;
    Alcotest.test_case "VM native phase requires its exact activation event"
      `Quick
      (vm_replayed_phase_authority false);
    Alcotest.test_case
      "VM failed native replay cannot revive a live parser callback" `Quick
      (vm_replayed_phase_authority true);
    Alcotest.test_case "VM completed header requires its exact replay event"
      `Quick
      (vm_replayed_header_authority false);
    Alcotest.test_case "VM failed replay cannot revive a completed header"
      `Quick
      (vm_replayed_header_authority true);
  ]

let tests = tests @ vm_phase_tests

(* Omitting catalog history cannot turn a reused native allocation into a new
   semantic identity. Rejection must leave the genuine successor usable. *)
let vm_reused_native_cannot_become_another_root () =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let runtime = VM.create_task_state ~table () |> checked in
  VM.bind_task_namespace runtime namespace |> checked;
  let registry =
    N.create_registry ~mode:Preprocessor.Jit ~table ~namespace |> checked
  in
  let first = ref None and exercised = ref false in
  let declaration event =
    (match event with
    | Parser.Function_declared publication -> (
        let published = C.publish_function namespace publication |> checked in
        let native = N.begin_header registry published publication |> checked in
        let snapshot = N.snapshot native in
        VM.check_function_phase_source runtime ~namespace ~event snapshot
        |> checked;
        let function_ =
          FD.resolve_provisional_call ~table ~namespace
            (N.call_shape snapshot |> checked)
          |> checked
        in
        let root =
          R.make_provisional_declaration ~table ~namespace
            ~compiler_option_mask:Compiler_option.initial_mask ~function_
          |> checked
        in
        let root_resolution =
          R.resolve ~previous:[] ~table
            ~parent:(C.namespace_scope namespace)
            ~compilation_mode:R.Jit [ root ]
          |> checked
        in
        let root_records =
          FCD.classify_publication ~previous:[] ~resolution:root_resolution
            publication
          |> checked
        in
        match !first with
        | None ->
            VM.admit_function_phase runtime ~namespace ~event ~snapshot
              ~records:root_records
            |> checked;
            first :=
              Some
                (snapshot, Option.get (VM.function_record_head runtime snapshot))
        | Some (original, retained) ->
            Alcotest.(check bool)
              "nested extern reuses the original native identity" true
              (N.same_identity original snapshot);
            Alcotest.(check bool)
              "nested declaration has a fresh source symbol" true
              (H.function_symbol function_ != Retained.symbol retained);
            vm_same_head
              "native head initially remains the admitted predecessor"
              (Some retained)
              (VM.function_record_head runtime snapshot);
            vm_reject "same native identity cannot be admitted as a second root"
              (VM.admit_function_phase runtime ~namespace ~event ~snapshot
                 ~records:root_records);
            vm_same_head
              "rejected alternate root preserves exact predecessor head"
              (Some retained)
              (VM.function_record_head runtime snapshot);
            vm_same_head "original snapshot still finds exact predecessor head"
              (Some retained)
              (VM.function_record_head runtime original);
            Alcotest.(check bool)
              "rejection publishes no alternate semantic symbol" true
              (Option.is_none
                 (VM.admitted_publication_for_symbol runtime
                    (H.function_symbol function_)));
            let previous =
              Retained.metadata retained
              |> Outer.function_classified_declaration
            in
            let current = FC.classified_declaration_source previous in
            let transition =
              N.transition ~earlier:original ~later:snapshot |> checked
            in
            let advance =
              R.make_provisional_advance ~table ~namespace ~current ~transition
                ~compiler_option_mask:Compiler_option.initial_mask ~function_ ()
              |> checked
            in
            let resolution =
              R.resolve ~previous:[ current ] ~table
                ~parent:(C.namespace_scope namespace)
                ~compilation_mode:R.Jit [ advance ]
              |> checked
            in
            let records =
              FCD.classify_publication ~previous:[ previous ] ~resolution
                publication
              |> checked
            in
            VM.admit_function_phase runtime ~namespace ~event ~snapshot ~records
            |> checked;
            Alcotest.(check bool)
              "genuine successor retains the original semantic identity" true
              (Retained.symbol
                 (Option.get (VM.function_record_head runtime snapshot))
              == Retained.symbol retained);
            exercised := true)
    | _ -> ());
    Ok ()
  in
  let commands =
    {
      (Test_provisional_function_parser.sink declaration) with
      Parser.checkpoint =
        Some
          (fun event ->
            VM.observe_task_source_event runtime event |> checked;
            Ok ());
    }
  in
  let _, _, parsed, _, _, _ =
    Test_stream_parser.parse ~session ~same_task:true ~commands
      ~configure:(fun _ execution -> { execution with Parser.commands })
      "extern I64 F(#exe {extern I64 F();});"
  in
  ignore (Test_parser.expect_ast parsed);
  Alcotest.(check bool)
    "reused native identity boundary exercised" true !exercised

let tests =
  tests
  @ [
      Alcotest.test_case
        "VM rejects another semantic root for reused native identity" `Quick
        vm_reused_native_cannot_become_another_root;
    ]

let vm_tracked_native_cannot_use_legacy_header () =
  let module CR = Holyc_lib__Sema.Compiler_record in
  let module Globals = Holyc_lib__Ir.Integer_globals in
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let runtime = VM.create_task_state ~table () |> checked in
  VM.bind_task_namespace runtime namespace |> checked;
  let registry =
    N.create_registry ~mode:Preprocessor.Jit ~table ~namespace |> checked
  in
  let publications = ref [] and exercised = ref false in
  let catalog_entries () =
    VM.task_snapshot runtime |> checked |> Globals.task_environment
    |> Outer.tables
    |> List.concat_map (fun table ->
        Outer.table_entries table
        |> List.map (fun entry ->
            ( Outer.entry_symbol entry,
              Option.map Outer.function_declaration
                (Outer.entry_function_metadata entry) )))
  in
  let declaration event =
    (match event with
    | Parser.Function_declared publication ->
        let published = C.publish_function namespace publication |> checked in
        let native = N.begin_header registry published publication |> checked in
        let snapshot = N.snapshot native in
        publications := (publication, published, snapshot) :: !publications;
        let function_ =
          FD.resolve_provisional_call ~table ~namespace
            (N.call_shape snapshot |> checked)
          |> checked
        in
        let previous =
          VM.function_record_head runtime snapshot
          |> Option.map (fun reference ->
              Retained.metadata reference
              |> Outer.function_classified_declaration)
        in
        let fact =
          match previous with
          | None ->
              R.make_provisional_declaration ~table ~namespace
                ~compiler_option_mask:Compiler_option.initial_mask ~function_
          | Some previous ->
              let current = FC.classified_declaration_source previous in
              let earlier =
                R.resolved_declaration_site current
                |> R.declaration_site_native_snapshot |> Option.get
              in
              let transition =
                N.transition ~earlier ~later:snapshot |> checked
              in
              R.make_provisional_advance ~table ~namespace ~current ~transition
                ~compiler_option_mask:Compiler_option.initial_mask ~function_ ()
        in
        let previous = Option.to_list previous in
        let resolution =
          R.resolve
            ~previous:(List.map FC.classified_declaration_source previous)
            ~table
            ~parent:(C.namespace_scope namespace)
            ~compilation_mode:R.Jit
            [ checked fact ]
          |> checked
        in
        let records =
          FCD.classify_publication ~previous ~resolution publication |> checked
        in
        VM.admit_function_phase runtime ~namespace ~event ~snapshot ~records
        |> checked
    | Parser.Function_header_completed header when not !exercised ->
        (* The first completed header belongs to the nested reused declaration. *)
        let _, published, snapshot =
          List.find
            (fun (publication, _, _) ->
              publication == header.function_publication)
            !publications
        in
        let source =
          CR.declare_function ~table ~namespace published header |> checked
        in
        let function_ =
          FD.resolve_completed_header ~table ~namespace source |> checked
        in
        let before_head = VM.function_record_head runtime snapshot in
        Alcotest.(check bool)
          "source symbol differs from joined native identity" true
          (H.function_symbol function_
          != Retained.symbol (Option.get before_head));
        let fact =
          R.make_pending_declaration ~table ~namespace
            ~compiler_option_mask:Compiler_option.initial_mask ~source
            ~function_
          |> checked
        in
        let resolution =
          R.resolve ~previous:[] ~table
            ~parent:(C.namespace_scope namespace)
            ~compilation_mode:R.Jit [ fact ]
          |> checked
        in
        let records =
          FCD.classify_completed_header ~previous:[] ~resolution source
          |> checked
        in
        let before_entries = catalog_entries () in
        VM.check_function_header_source runtime ~namespace source |> checked;
        vm_reject
          "tracked source cannot bypass native advance through legacy header"
          (VM.admit_function_header runtime ~namespace ~source ~records);
        let after_entries = catalog_entries () in
        Alcotest.(check bool)
          "rejected legacy header preserves every catalog identity" true
          (List.length before_entries = List.length after_entries
          && List.for_all2
               (fun (before_symbol, before_declaration)
                    (after_symbol, after_declaration) ->
                 before_symbol == after_symbol
                 &&
                 match (before_declaration, after_declaration) with
                 | None, None -> true
                 | Some before, Some after -> before == after
                 | _ -> false)
               before_entries after_entries);
        vm_same_head "rejected legacy header preserves native head" before_head
          (VM.function_record_head runtime snapshot);
        exercised := true
    | _ -> ());
    Ok ()
  in
  let commands =
    {
      (Test_provisional_function_parser.sink declaration) with
      Parser.checkpoint =
        Some
          (fun event ->
            VM.observe_task_source_event runtime event |> checked;
            Ok ());
    }
  in
  let _, _, parsed, _, _, _ =
    Test_stream_parser.parse ~session ~same_task:true ~commands
      ~configure:(fun _ execution -> { execution with Parser.commands })
      "extern I64 F(#exe {extern I64 F();});"
  in
  ignore (Test_parser.expect_ast parsed);
  Alcotest.(check bool)
    "legacy bypass attempted for authentic tracked source" true !exercised

let tests =
  tests
  @ [
      Alcotest.test_case
        "VM rejects legacy header for tracked joined native source" `Quick
        vm_tracked_native_cannot_use_legacy_header;
    ]

let vm_call_authority replay_failure () =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let runtime = VM.create_task_state ~table () |> checked in
  let registry =
    N.create_registry ~mode:Preprocessor.Jit ~table ~namespace |> checked
  in
  let journal = Activation.create_call_journal ~namespace () in
  let events = ref [] and observed = ref 0 and snapshots = ref [] in
  let native = ref None and selected = ref None and start_capture = ref None in
  let alternate_start = ref None and alternate_emission = ref None in
  let pending = ref None and saved_phase = ref None and exercised = ref false in
  let replay = Option.is_some replay_failure in
  if not replay then VM.bind_task_namespace runtime namespace |> checked;
  let admit event snapshot =
    let records, _ = vm_phase_records ~table ~namespace runtime snapshot in
    VM.admit_function_phase runtime ~namespace ~event ~snapshot ~records
    |> checked
  in
  let freeze selection =
    let retained =
      VM.function_record_head runtime (N.snapshot (Option.get !native))
      |> Option.get
    in
    vm_reject "copied metadata is not an admitted reference"
      (VM.observe_task_function_selection runtime ~namespace ~selection
         ~selected:(Retained.create (Retained.metadata retained)));
    VM.observe_task_function_selection runtime ~namespace ~selection
      ~selected:retained
    |> checked;
    selected := Some retained
  in
  let capture_start capture =
    let retained = Option.get !selected in
    let scope =
      Retained.metadata retained |> Outer.function_declaration
      |> R.resolved_declaration_header |> H.function_scope
    in
    let arguments =
      N.call_shape (N.call_argument_snapshot capture)
      |> checked
      |> FD.resolve_provisional_call ~scope ~table ~namespace
      |> checked
    in
    let attempt selected =
      VM.capture_task_call_start runtime ~namespace ~capture ~selected
        ~arguments
    in
    let before =
      VM.function_record_head runtime (N.call_argument_snapshot capture)
    in
    vm_reject "call cannot substitute a fresh retained identity"
      (attempt (Retained.create (Retained.metadata retained)));
    vm_same_head "forged call leaves the catalog intact" before
      (VM.function_record_head runtime (N.call_argument_snapshot capture));
    pending := Some (attempt retained |> checked);
    vm_reject "call start is single use" (attempt retained)
  in
  let capture_emission capture =
    vm_reject
      "another capture of the same receipt cannot replace original arguments"
      (VM.capture_task_call_emission runtime ~table
         ~capture:(Option.get !alternate_emission)
         (Option.get !pending));
    let phase =
      VM.capture_task_call_emission runtime ~table ~capture
        (Option.get !pending)
      |> checked
    in
    saved_phase := Some phase;
    Alcotest.(check bool)
      "captured call is available at its active event" true
      (VM.owns_call_phase runtime phase);
    vm_reject "emission is single use"
      (VM.capture_task_call_emission runtime ~table ~capture
         (Option.get !pending))
  in
  let declaration event =
    events := Activation.Declaration event :: !events;
    if not !exercised then (
      (match event with
      | Parser.Function_declared publication ->
          let publication_ =
            C.publish_function namespace publication |> checked
          in
          native :=
            Some (N.begin_header registry publication_ publication |> checked)
      | _ ->
          let record = Option.get !native in
          if N.event_belongs record event then N.observe record event |> checked);
      let snapshot = N.snapshot (Option.get !native) in
      snapshots := (event, snapshot) :: !snapshots;
      if not replay then admit event snapshot);
    Ok ()
  in
  let commands =
    {
      (Test_provisional_function_parser.sink declaration) with
      Parser.checkpoint =
        Some
          (fun event ->
            incr observed;
            events := Activation.Command event :: !events;
            if not replay then
              VM.observe_task_source_event runtime event |> checked;
            Ok ());
      reference =
        Some
          (fun selection ->
            events := Activation.Reference selection :: !events;
            if not replay then freeze selection;
            Ok ());
      call =
        Some
          {
            start =
              (fun start ->
                let capture =
                  N.capture_call_start (Option.get !native) start |> checked
                in
                start_capture := Some capture;
                alternate_start :=
                  Some
                    (N.capture_call_start (Option.get !native) start |> checked);
                if replay then
                  events :=
                    (Activation.capture_call_start journal ~events_rev:!events
                       start
                    |> checked)
                    :: !events
                else capture_start capture;
                Ok None);
            emit =
              (fun receipt ->
                let capture =
                  N.capture_call_emission (Option.get !start_capture) receipt
                  |> checked
                in
                alternate_emission :=
                  Some
                    (N.capture_call_emission
                       (Option.get !alternate_start)
                       receipt
                    |> checked);
                if replay then (
                  events :=
                    (Activation.capture_call_emission journal
                       ~events_rev:!events receipt
                    |> checked)
                    :: !events;
                  let activation =
                    Activation.create ~calls:journal ~namespace
                      ~context:
                        (Parser.selected_command
                           receipt.call_start.call_reference)
                          .command_context ~observed_events:!observed
                      (List.rev !events)
                    |> checked
                  in
                  VM.promote_task_source_activation runtime ~namespace
                    ~activation ~dimensions:[]
                  |> checked;
                  let exception Abort_replay in
                  let run () =
                    Activation.run activation ~invalid:"inactive" (function
                      | Activation.Declaration event ->
                          admit event (List.assq event !snapshots);
                          Ok ()
                      | Activation.Reference selection ->
                          freeze selection;
                          Ok ()
                      | Activation.Call_start _ ->
                          capture_start (Option.get !start_capture);
                          Ok ()
                      | Activation.Call_emission _ ->
                          capture_emission capture;
                          if Option.get replay_failure then raise Abort_replay
                          else Error "failed after capture"
                      | _ -> Ok ())
                  in
                  (try vm_reject "replay fails after call capture" (run ())
                   with Abort_replay -> ());
                  Alcotest.(check bool)
                    "failed replay revokes its uncommitted call" false
                    (VM.owns_call_phase runtime (Option.get !saved_phase));
                  vm_reject "failed replay cannot restart"
                    (Activation.run activation ~invalid:"consumed" (fun _ ->
                         Ok ()));
                  vm_reject "still-live emission cannot revive failed replay"
                    (VM.capture_task_call_emission runtime ~table ~capture
                       (Option.get !pending)))
                else capture_emission capture;
                exercised := true;
                Ok ());
          };
    }
  in
  let _, _, parsed, _, _, _ =
    Test_stream_parser.parse ~session ~same_task:true ~commands
      "I64 F(I64 n=F());"
  in
  ignore (Test_parser.expect_ast parsed);
  Alcotest.(check bool) "call boundary exercised" true !exercised;
  vm_reject "expired source start cannot manufacture another token"
    (N.capture_call_start (Option.get !native)
       (N.call_start_receipt (Option.get !start_capture)))

let tests =
  tests
  @ [
      Alcotest.test_case
        "VM call requires the original admitted identifier selection" `Quick
        (vm_call_authority None);
      Alcotest.test_case "VM failed replay revokes captured uncommitted calls"
        `Quick
        (vm_call_authority (Some false));
      Alcotest.test_case
        "VM replay exception revokes captured uncommitted calls" `Quick
        (vm_call_authority (Some true));
    ]
