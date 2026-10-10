open Holyc_lib
module C = Semantic_declaration_collection
module Record = Semantic_compiler_record
module VM = Ir_integer_interpreter

module Entries = Hashtbl.Make (struct
  type t = Symbol_visibility.entry

  let equal = ( == )
  let hash = Hashtbl.hash
end)

type state = {
  publication : C.publication;
  progress : Record.aggregate_progress;
  initial : Record.t;
  mutable record : Record.t;
}

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let reject label result =
  Alcotest.(check bool) label true (Result.is_error result)

let state_for entries source =
  Entries.find entries source.Parser.aggregate_entry

let sink declaration : Parser.command_sink =
  {
    lexical_lookup = None;
    checkpoint = None;
    reference = None;
    call = None;
    implicit_output = None;
    declaration = Some declaration;
    query = None;
    dimension_count = None;
    command = (fun _ -> Ok ());
    resume = (fun () -> Ok ());
  }

let parse session source commands =
  let output =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~symbols:(Session.symbols session)
      ~definitions:(Session.definitions session)
      ~config:(Preprocessor.Config.create ~compilation_mode:Jit () |> checked)
      source
  in
  match output.ast with
  | Some ast when not (Parser.has_errors output) -> ast
  | _ ->
      Alcotest.fail
        (List.map
           (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
           output.diagnostics
        |> String.concat "; ")

let member_selection_authority () =
  let module Types = Holyc_lib__Sema.Source_type_reference in
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let foreign = C.create_namespace ~table () |> checked in
  let foreign_table = Session.semantic_symbols (Session.create ()) in
  let entries = Entries.create 8 in
  let saved = ref [] and borrowed = ref None and completed = ref None in
  let bad_progress = ref None and mismatch_checked = ref false in
  let declaration = function
    | Parser.Aggregate_declared source ->
        let publication = C.publish_aggregate namespace source |> checked in
        let progress =
          Record.begin_aggregate ~table ~namespace publication |> checked
        in
        let initial = Record.aggregate_metadata progress |> checked in
        Entries.add entries source.aggregate_entry
          { publication; progress; initial; record = initial };
        if source.aggregate_name.spelling = "Outer" then
          bad_progress :=
            Some
              (Record.begin_aggregate ~table ~namespace publication |> checked);
        Ok ()
    | Parser.Aggregate_advanced phase ->
        let own = state_for entries phase.phase_aggregate in
        let member =
          match phase.phase_step with
          | Parser.Aggregate_member_prepared member
            when Option.is_some member.member_selection ->
              let selection = Option.get member.member_selection in
              let selected = Entries.find entries selection.entry in
              let source = Types.Aggregate_member phase in
              let proof =
                Types.select_aggregate ~table ~namespace ~source
                  selected.publication
                |> checked
              in
              let read ?(table = table) ?(namespace = namespace) record =
                Record.select_aggregate_member ~table ~namespace
                  ~selected_aggregate:proof phase record
              in
              reject "foreign table cannot supply a member class"
                (read ~table:foreign_table (Some selected.record));
              reject "foreign namespace cannot supply a member class"
                (read ~namespace:foreign (Some selected.record));
              reject "stale class snapshot cannot supply a member extent"
                (read (Some selected.initial));
              reject "containing class cannot replace the selected member class"
                (read (Some own.record));
              let other =
                Entries.fold
                  (fun _ state found ->
                    if state == selected || state == own then found
                    else Some state.record)
                  entries None
              in
              Option.iter
                (fun record ->
                  reject
                    "another equal-sized class cannot supply the member extent"
                    (read (Some record)))
                other;
              reject
                "an equal spelling cannot replace the original selected \
                 publication"
                (Types.select_aggregate ~table ~namespace ~source
                   own.publication);
              let selected_member = read (Some selected.record) |> checked in
              saved := (phase, proof, selected.record) :: !saved;
              Some selected_member
          | _ -> None
        in
        (match !bad_progress with
        | Some bad
          when phase.phase_aggregate.aggregate_name.spelling = "Outer"
               && not !mismatch_checked -> (
            Record.advance_aggregate
              ~members:(fun _ ->
                match (!borrowed, member) with
                | Some prior, _ -> Ok prior
                | None, Some current -> Ok current
                | _ -> Error "no member")
              ~dimensions:(fun _ -> None)
              bad phase
            |> checked;
            match (!borrowed, member) with
            | Some _, Some _ ->
                reject "another original placement cannot lend its class extent"
                  (Record.aggregate_metadata bad);
                mismatch_checked := true
            | _ -> ())
        | _ -> ());
        Record.advance_aggregate
          ~members:(fun requested ->
            match member with
            | Some selected when requested == phase -> Ok selected
            | _ -> Error "no original member")
          ~dimensions:(fun _ -> None)
          own.progress phase
        |> checked;
        (match member with
        | Some member when Option.is_none !borrowed -> borrowed := Some member
        | _ -> ());
        own.record <- Record.aggregate_metadata own.progress |> checked;
        Ok ()
    | Parser.Aggregate_completed receipt ->
        let own = state_for entries receipt.aggregate_publication in
        own.record <-
          Record.complete_aggregate ~progress:own.progress ~table ~namespace
            own.publication receipt
          |> checked;
        if receipt.aggregate_publication.aggregate_name.spelling = "Outer" then
          completed := Some own;
        Ok ()
    | _ -> Ok ()
  in
  let source =
    Session.add_source session ~path:"member-selection-authority.hc"
      ~contents:
        "class A{U16 word;};class B{U16 other;};class Outer{A first;B second;A \
         third;};"
  in
  ignore (parse session source (sink declaration));
  let own = Option.get !completed in
  let symbol = C.publication_aggregate_identity own.publication |> Option.get in
  let type_ =
    Semantic_type.make_aggregate ~symbol ~pointer_depth:0 |> checked
  in
  Alcotest.(check int64)
    "original selected nested extents" 6L
    (Record.return_class_size ~table ~namespace ~type_
       ~aggregate:(Some own.record)
    |> checked);
  Alcotest.(check int)
    "each original named member has a receipt" 3 (List.length !saved);
  Alcotest.(check bool)
    "borrowed placement check was reached" true !mismatch_checked;
  List.iter
    (fun (phase, proof, record) ->
      reject "expired member phase cannot mint another layout snapshot"
        (Record.select_aggregate_member ~table ~namespace
           ~selected_aggregate:proof phase (Some record)))
    !saved

let selection_authority () =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let foreign = C.create_namespace ~table () |> checked in
  let foreign_table = Session.semantic_symbols (Session.create ()) in
  let positions =
    Record.create_compiler_positions ~sources:(Session.sources session)
  in
  let entries = Entries.create 8 in
  let saved = ref [] and metadata = ref [] and first_base = ref None in
  let declaration = function
    | Parser.Aggregate_declared source ->
        let publication = C.publish_aggregate namespace source |> checked in
        let progress =
          Record.begin_aggregate ~compiler_positions:positions ~table ~namespace
            publication
          |> checked
        in
        let initial = Record.aggregate_metadata progress |> checked in
        Entries.add entries source.aggregate_entry
          { publication; progress; initial; record = initial };
        Ok ()
    | Parser.Aggregate_advanced phase ->
        let own = state_for entries phase.phase_aggregate in
        let base =
          match phase.phase_step with
          | Parser.Aggregate_base_attached selection ->
              let selected = Entries.find entries selection.base_entry in
              let read ?(table = table) ?(namespace = namespace)
                  ?(publication = selected.publication) record =
                Record.select_aggregate_base ~table ~namespace
                  ~selected_publication:publication phase record
              in
              reject "foreign namespace cannot supply the selected layout"
                (read ~namespace:foreign selected.record);
              reject "foreign table cannot supply the selected layout"
                (read ~table:foreign_table selected.record);
              reject "stale partial snapshot cannot supply the selected layout"
                (read selected.initial);
              reject "child record cannot replace its selected base"
                (read own.record);
              let forged =
                C.publish namespace
                  ~name:
                    (Semantic_symbol.name
                       (C.publication_symbol selected.publication))
                  ~kind:Semantic_symbol.Aggregate_type
                  ~origin:
                    (Semantic_symbol.origin
                       (C.publication_symbol selected.publication))
                |> checked
              in
              reject "equal spelling and origin cannot fabricate base selection"
                (read ~publication:forged selected.record);
              Option.iter
                (fun prior ->
                  reject
                    "equal size from another original class cannot substitute"
                    (read prior))
                !first_base;
              first_base := Some selected.record;
              let proof = read selected.record |> checked in
              saved :=
                (phase, selected.publication, selected.record, own.progress)
                :: !saved;
              Some proof
          | _ -> None
        in
        Record.advance_aggregate
          ~bases:(fun _ -> Ok (Option.get base))
          ~dimensions:(fun _ -> None)
          own.progress phase
        |> checked;
        own.record <- Record.aggregate_metadata own.progress |> checked;
        reject "base and body phases cannot replay"
          (Record.advance_aggregate
             ~dimensions:(fun _ -> None)
             own.progress phase);
        Ok ()
    | Parser.Aggregate_completed receipt ->
        let own = state_for entries receipt.aggregate_publication in
        own.record <-
          Record.complete_aggregate ~progress:own.progress ~table ~namespace
            own.publication receipt
          |> checked;
        (match receipt.aggregate_item with
        | Ast.Aggregate_definition definition
          when Option.is_some definition.base ->
            let proof =
              Record.retain_inherited_metadata ~table ~namespace definition
                own.record
              |> checked
            in
            reject "completed metadata cannot borrow a namespace"
              (Record.retain_inherited_metadata ~table ~namespace:foreign
                 definition own.record);
            reject "completed metadata cannot borrow a table"
              (Record.retain_inherited_metadata ~table:foreign_table ~namespace
                 definition own.record);
            Alcotest.(check bool)
              "original definition owns metadata in its original scope" true
              (Record.inherited_metadata_owns_definition ~table
                 ~scope:(C.namespace_scope namespace)
                 definition proof);
            Alcotest.(check bool)
              "foreign scope cannot hide an object definition" false
              (Record.inherited_metadata_owns_definition ~table
                 ~scope:(C.namespace_scope foreign)
                 definition proof);
            Alcotest.(check bool)
              "foreign table cannot borrow a completed storage selection" true
              (Option.is_none
                 (Record.inherited_metadata_storage_selection
                    ~table:foreign_table
                    ~scope:(C.namespace_scope namespace)
                    proof));
            Alcotest.(check bool)
              "foreign namespace cannot borrow a completed storage selection"
              true
              (Option.is_none
                 (Record.inherited_metadata_storage_selection ~table
                    ~scope:(C.namespace_scope foreign)
                    proof));
            (match
               Record.inherited_metadata_storage_selection ~table
                 ~scope:(C.namespace_scope namespace)
                 proof
             with
            | Some (original, symbol, size, base, base_symbol, base_size) ->
                Alcotest.(check bool)
                  "exact completed child definition" true
                  (original == definition);
                Alcotest.(check bool)
                  "exact child canonical identity" true
                  (symbol == C.publication_symbol own.publication);
                Alcotest.(check int64) "original child size" 9L size;
                Alcotest.(check int64) "original base size" 8L base_size;
                Alcotest.(check string)
                  "original selected base name"
                  (Option.get definition.base).base_name.spelling
                  base.name.spelling;
                Alcotest.(check string)
                  "selected canonical base name" base.name.spelling
                  (Semantic_symbol.name base_symbol)
            | None -> Alcotest.fail "completed original base selection missing");
            metadata := (definition, own.record, proof) :: !metadata
        | _ -> ());
        Ok ()
    | _ -> Ok ()
  in
  let source =
    Session.add_source session ~path:"inherited-authority.hc"
      ~contents:
        "extern class Base;class Base{I64 a;};class Child:Base{U8 b;};class \
         Other{I64 a;};class Copy:Other{U8 b;};"
  in
  let commands = sink declaration in
  ignore (parse session source commands);
  Alcotest.(check int) "two original selected base reads" 2 (List.length !saved);
  List.iter
    (fun (phase, selected_publication, record, progress) ->
      reject "base selection expires after its callback"
        (Record.select_aggregate_base ~table ~namespace ~selected_publication
           phase record);
      reject "expired phase cannot advance layout"
        (Record.advance_aggregate ~dimensions:(fun _ -> None) progress phase))
    !saved;
  let copied = parse session source (sink (fun _ -> Ok ())) in
  List.iter
    (fun ((original : Ast.aggregate_definition), record, proof) ->
      let copy =
        Ast.declaration_items copied
        |> List.find_map (function
          | _, Ast.Aggregate_definition d
            when d.name.spelling = original.Ast.name.spelling -> Some d
          | _ -> None)
        |> Option.get
      in
      Alcotest.(check bool)
        "copied source has equal location" true
        (copy.location = original.location);
      reject "equal source contents cannot rebuild completed layout authority"
        (Record.retain_inherited_metadata ~table ~namespace copy record);
      Alcotest.(check bool)
        "copied definition cannot suppress object layout" false
        (Record.inherited_metadata_owns_definition ~table
           ~scope:(C.namespace_scope namespace)
           copy proof))
    !metadata

let unexecuted_dependencies dimension =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let task = VM.create_task_state ~table () |> checked in
  VM.bind_task_namespace task namespace |> checked;
  let positions =
    Record.create_compiler_positions ~sources:(Session.sources session)
  in
  let entries = Entries.create 8 in
  let dimensions = ref []
  and query_read = ref None
  and queries = ref []
  and checks = ref 0 in
  let checked_dimensions source = List.assq_opt source !dimensions in
  let declaration = function
    | Parser.Array_dimension_completed receipt ->
        let proposal =
          Record.propose_runtime_dimension ~namespace
            ~preparation:receipt.dimension_preparation ~count:34L ~work:1
          |> checked
        in
        let proof =
          Record.complete_runtime_dimension ~table ~receipt ~queries:[] proposal
          |> checked
        in
        dimensions := (receipt.dimension_ast, proof) :: !dimensions;
        Ok ()
    | Parser.Aggregate_declared source ->
        let publication = C.publish_aggregate namespace source |> checked in
        let progress =
          Record.begin_aggregate ~compiler_positions:positions ~table ~namespace
            publication
          |> checked
        in
        let initial = Record.aggregate_metadata progress |> checked in
        Entries.add entries source.aggregate_entry
          { publication; progress; initial; record = initial };
        Ok ()
    | Parser.Aggregate_advanced phase ->
        let own = state_for entries phase.phase_aggregate in
        let base =
          match phase.phase_step with
          | Parser.Aggregate_base_attached selection ->
              let selected = Entries.find entries selection.base_entry in
              Some
                (Record.select_aggregate_base ~table ~namespace
                   ~selected_publication:selected.publication phase
                   selected.record
                |> checked)
          | _ -> None
        in
        (match phase.phase_step with
        | Parser.Aggregate_offset_reached _
          when phase.phase_aggregate.aggregate_name.spelling = "Base" ->
            let proposal =
              Record.begin_runtime_aggregate_offset ~table ~namespace
                ~queries:[] own.progress phase
              |> checked
            in
            ignore
              (Record.finish_runtime_aggregate_offset proposal ~value:34L
                 ~work:1
              |> checked)
        | Parser.Aggregate_offset_reached _ ->
            let query = List.hd !queries in
            Alcotest.(check int)
              "transitive original dimension dependencies"
              (if dimension then 1 else 0)
              (List.length (Record.query_runtime_dependencies query));
            Alcotest.(check int)
              "transitive original offset dependencies"
              (if dimension then 0 else 1)
              (List.length (Record.query_runtime_offsets query));
            let result, work =
              VM.prepare_task_aggregate_offset task ~table ~namespace
                ~queries:!queries own.progress phase
            in
            reject
              "copied base size cannot authorize an unexecuted source \
               dependency"
              result;
            Alcotest.(check int)
              "dependency rejection precedes numeric work" 0 work;
            Alcotest.(check int)
              "dependency rejection leaves task work unchanged" 0
              (VM.task_initializer_steps task);
            Alcotest.(check int)
              "dependency rejection executes no instructions" 0
              (VM.task_executed_steps task);
            let pure, _ =
              Record.prepare_aggregate_offset ~table ~namespace ~max_work:3
                ~queries:!queries own.progress phase
            in
            let pure = checked pure in
            Alcotest.(check int64)
              "metadata still has its original derived size" 42L
              (Record.aggregate_offset_value pure);
            Alcotest.(check bool)
              "copied dependencies keep metadata runtime dependent" true
              (Record.aggregate_offset_is_runtime pure);
            reject
              "derived metadata cannot charge as isolated closed preparation"
              (VM.charge_isolated_aggregate_offsets task ~table [ pure ]);
            incr checks
        | _ -> ());
        Record.advance_aggregate
          ~bases:(fun _ -> Ok (Option.get base))
          ~dimensions:checked_dimensions own.progress phase
        |> checked;
        own.record <- Record.aggregate_metadata own.progress |> checked;
        Ok ()
    | Parser.Aggregate_completed receipt ->
        let own = state_for entries receipt.aggregate_publication in
        own.record <-
          Record.complete_aggregate ~dimensions:checked_dimensions
            ~progress:own.progress ~table ~namespace own.publication receipt
          |> checked;
        Ok ()
    | _ -> Ok ()
  in
  let query = function
    | Parser.Query_root root ->
        let entry =
          match root.query_lookup with
          | Symbol_visibility.Present entry -> entry
          | _ -> Alcotest.fail "original class query absent"
        in
        query_read :=
          Some
            (Record.read_sizeof ~table ~root (Entries.find entries entry).record
            |> checked);
        Ok ()
    | Parser.Query_completed receipt ->
        queries :=
          (Record.complete_query ~sizeof_read:(Option.get !query_read) ~table
             ~receipt ()
          |> checked)
          :: !queries;
        Ok ()
    | _ -> Ok ()
  in
  let source =
    Session.add_source session ~path:"inherited-dependencies.hc"
      ~contents:
        ((if dimension then "class Base{U8 a[34];};" else "class Base{$$=34;};")
        ^ "class C:Base{I64 b;};class D:C{};class Check{$$=sizeof(D);};")
  in
  ignore (parse session source { (sink declaration) with query = Some query });
  Alcotest.(check int) "one original dependency check" 1 !checks

let () =
  Alcotest.run "Original inherited layout authority"
    [
      ( "authority",
        [
          Alcotest.test_case
            "nested member selections retain original class records" `Quick
            member_selection_authority;
          Alcotest.test_case
            "exact entry, namespace, lifetime and metadata definition" `Quick
            selection_authority;
          Alcotest.test_case
            "inherited unexecuted dimension remains unauthorized" `Quick
            (fun () -> unexecuted_dependencies true);
          Alcotest.test_case "inherited unexecuted offset remains unauthorized"
            `Quick (fun () -> unexecuted_dependencies false);
        ] );
    ]
