open Holyc_lib
module C = Semantic_declaration_collection
module Record = Semantic_compiler_record
module Native = Semantic_function_record_phase
module Table = Semantic_symbol_table
module Type = Semantic_type
module Reference = Semantic_type_reference
module Source = Holyc_lib__Sema.Source_type_reference
module D = Task_declarations

module Entries = Hashtbl.Make (struct
  type t = Symbol_visibility.entry

  let equal = ( == )
  let hash = Hashtbl.hash
end)

type aggregate = {
  publication : C.publication;
  progress : Record.aggregate_progress;
  initial : Record.t;
  mutable current : Record.t;
}

type function_ = {
  parser : Parser.function_publication;
  publication : C.publication;
  native : Native.t;
}

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let observed = function
  | Ok value -> value
  | Error diagnostics ->
      Alcotest.fail
        (diagnostics
        |> List.map (fun d -> d.Diagnostic.code ^ ": " ^ d.message)
        |> String.concat "; ")

let reject label result =
  Alcotest.(check bool) label true (Result.is_error result)

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

let parse session mode source commands =
  let output =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~symbols:(Session.symbols session)
      ~definitions:(Session.definitions session)
      ~config:(Preprocessor.Config.create ~compilation_mode:mode () |> checked)
      source
  in
  match output.ast with
  | Some ast when not (Parser.has_errors output) -> ast
  | _ ->
      Alcotest.fail
        (output.diagnostics
        |> List.map (fun d -> d.Diagnostic.code ^ ": " ^ d.message)
        |> String.concat "; ")

let original_allocation_authority () =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let foreign_namespace = C.create_namespace ~table () |> checked in
  let foreign_table = Session.semantic_symbols (Session.create ()) in
  let positions =
    Record.create_compiler_positions ~sources:(Session.sources session)
  in
  let registry =
    Native.create_registry ~mode:Preprocessor.Jit ~table ~namespace |> checked
  in
  let aggregates = Entries.create 8 in
  let functions = ref [] and saved = ref [] and excluded = ref 0 in
  let allocation function_ receipt =
    match receipt.Parser.allocation_local.local_source with
    | Parser.Local_variable local ->
        let selection = Option.get local.local_type_selection in
        let selected : aggregate = Entries.find aggregates selection.entry in
        let selected_type =
          Source.select_aggregate ~table ~namespace
            ~source:(Source.Function_local receipt) selected.publication
          |> checked
        in
        let capture ?(table = table) ?(namespace = namespace)
            ?(function_publication = function_.publication) record =
          Record.select_automatic_aggregate ~table ~namespace
            ~function_publication ~selected_aggregate:selected_type receipt
            record
        in
        let allocation =
          if
            receipt.allocation_storage = Ast.Automatic_local
            && local.local_pointer_layers = []
          then (
            reject "foreign table cannot lend a class allocation"
              (capture ~table:foreign_table selected.current);
            reject "foreign namespace cannot lend a class allocation"
              (capture ~namespace:foreign_namespace selected.current);
            let other_function =
              List.find (fun other -> other != function_) !functions
            in
            reject "another original function cannot own this local"
              (capture ~function_publication:other_function.publication
                 selected.current);
            reject
              "pre-completion metadata cannot replace the live class record"
              (capture selected.initial);
            let other_class =
              Entries.fold
                (fun _ (other : aggregate) found ->
                  if other == selected then found else Some other)
                aggregates None
              |> Option.get
            in
            reject "an equal-sized class cannot replace the selected identity"
              (capture other_class.current);
            Some (capture selected.current |> checked))
          else (
            incr excluded;
            reject "pointer or static storage is not an automatic class object"
              (capture selected.current);
            None)
        in
        let before = Native.snapshot function_.native in
        (match !saved with
        | (_, _, _, _, earlier) :: _ ->
            reject "another local cannot lend its saved class allocation"
              (Record.record_local_allocation ~automatic_aggregate:earlier
                 ~table ~namespace ~dimensions:[] positions function_.native
                 receipt);
            Alcotest.(check bool)
              "wrong-local rejection preserves native allocation state" true
              (Native.snapshot function_.native == before)
        | [] -> ());
        Record.record_local_allocation ?automatic_aggregate:allocation ~table
          ~namespace ~dimensions:[] positions function_.native receipt
        |> checked;
        reject "the original native allocation cannot be consumed twice"
          (Record.record_local_allocation ?automatic_aggregate:allocation ~table
             ~namespace ~dimensions:[] positions function_.native receipt);
        Option.iter
          (fun allocation ->
            saved :=
              (function_, receipt, selected_type, selected.current, allocation)
              :: !saved)
          allocation
    | _ -> Alcotest.fail "expected the original local variable"
  in
  let declaration event =
    (match event with
    | Parser.Aggregate_declared source ->
        let publication = C.publish_aggregate namespace source |> checked in
        let progress =
          Record.begin_aggregate ~table ~namespace publication |> checked
        in
        let initial = Record.aggregate_metadata progress |> checked in
        Entries.add aggregates source.aggregate_entry
          { publication; progress; initial; current = initial }
    | Parser.Aggregate_advanced phase ->
        let state =
          Entries.find aggregates phase.phase_aggregate.aggregate_entry
        in
        Record.advance_aggregate
          ~dimensions:(fun _ -> None)
          state.progress phase
        |> checked;
        state.current <- Record.aggregate_metadata state.progress |> checked
    | Parser.Aggregate_completed receipt ->
        let state =
          Entries.find aggregates receipt.aggregate_publication.aggregate_entry
        in
        state.current <-
          Record.complete_aggregate ~progress:state.progress ~table ~namespace
            state.publication receipt
          |> checked
    | Parser.Function_declared parser ->
        let publication = C.publish_function namespace parser |> checked in
        let native =
          Native.begin_header registry publication parser |> checked
        in
        functions := { parser; publication; native } :: !functions
    | Parser.Function_local_allocated receipt ->
        let function_ =
          List.find
            (fun value -> value.parser == receipt.allocation_function)
            !functions
        in
        allocation function_ receipt
    | Parser.Function_position_written _ -> ()
    | _ ->
        List.iter
          (fun function_ ->
            if Native.event_belongs function_.native event then
              Native.observe function_.native event |> checked)
          !functions);
    Ok ()
  in
  let source =
    Session.add_source session ~path:"original-local-class-allocation.hc"
      ~contents:
        "class A{U16 value;};class B{U16 other;};U0 G(){};U0 F(){A first;A \
         second;B third;A *pointer;static A stored;};"
  in
  ignore (parse session Preprocessor.Jit source (sink declaration));
  Alcotest.(check int) "three real object allocations" 3 (List.length !saved);
  Alcotest.(check int) "pointer and static refusals were reached" 2 !excluded;
  List.iter
    (fun (function_, receipt, selected_type, record, _) ->
      reject "expired allocation cannot mint another class receipt"
        (Record.select_automatic_aggregate ~table ~namespace
           ~function_publication:function_.publication
           ~selected_aggregate:selected_type receipt record))
    !saved

let source_allocation_and_reference_timing mode () =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let source =
    Session.add_source session ~path:"saved-local-class-timing.hc"
      ~contents:
        "extern class C;U0 Before(C *p){C early;p;};class C{U16 value;};U0 \
         After(C *p){C late;C *pointer;static C held;p;};"
  in
  let ledger = D.create_source session ~source |> checked in
  let allocations = ref [] and references = ref [] in
  let declaration event =
    D.observe ledger event |> observed;
    (match event with
    | Parser.Function_local_allocated receipt ->
        allocations := receipt :: !allocations;
        reject "live local callback cannot be replayed into the source ledger"
          (D.observe ledger event)
    | _ -> ());
    Ok ()
  in
  let commands =
    {
      (sink declaration) with
      checkpoint = Some (D.observe_command ledger);
      reference =
        Some
          (fun reference ->
            D.observe_reference ledger reference |> observed;
            references := reference :: !references;
            Ok ());
      query = Some (D.observe_query ledger);
      dimension_count = Some (D.grammar_dimension_count ledger);
    }
  in
  let ast = parse session mode source commands in
  let source_command = D.seal_source ledger ast |> observed in
  let command = D.seal ledger ast |> observed in
  let resolve =
    D.automatic_aggregate_resolver ~table ~ast command |> observed
  in
  let source_resolve =
    D.source_automatic_aggregate_resolver ~table ~ast source_command |> observed
  in
  let namespace, selected_type =
    D.selected_type_resolver ~table ~ast command |> observed
  in
  let find name =
    List.find
      (fun receipt -> receipt.Parser.allocation_local.local_spelling = name)
      !allocations
  in
  let local receipt =
    match receipt.Parser.allocation_local.local_source with
    | Parser.Local_variable local ->
        ( local.local_name,
          local.local_type_specifier,
          local.local_pointer_layers )
    | _ -> Alcotest.fail "expected an original variable type"
  in
  let saved name =
    let receipt = find name in
    let identifier, _, _ = local receipt in
    let allocation = resolve identifier |> Option.get in
    Alcotest.(check bool)
      "both command views retain the same original allocation" true
      (Option.get (source_resolve identifier) == allocation);
    allocation
  in
  let early = saved "early" and late = saved "late" in
  Alcotest.(check int64)
    "later completion cannot enlarge an earlier zero allocation" 0L
    (Record.automatic_aggregate_size early);
  Alcotest.(check int64)
    "a later genuine allocation observes the completed class" 2L
    (Record.automatic_aggregate_size late);
  List.iter
    (fun name ->
      let identifier, _, _ = local (find name) in
      Alcotest.(check bool)
        "non-object storage has no aggregate allocation authority" true
        (Option.is_none (resolve identifier)))
    [ "pointer"; "held" ];
  let late_source = find "late" in
  let late_name, late_specifier, late_pointers = local late_source in
  let clone =
    Ast.make_identifier ~spelling:late_name.spelling
      ~location:late_name.location
  in
  Alcotest.(check bool)
    "equal source metadata cannot create another local occurrence" true
    (Option.is_none (resolve clone));
  let class_proof = selected_type late_specifier |> Option.get in
  let checked_type =
    Source.selected_header_class class_proof late_specifier late_pointers
    |> checked |> Reference.resolved_type
  in
  let function_symbol =
    D.symbol_for ledger late_source.allocation_function.function_entry
    |> Option.get
  in
  let parent = C.namespace_scope namespace in
  let local_scope =
    Table.create_scope table ~parent ~kind:Table.Function ~name:"After" ()
    |> checked
  in
  let local_symbol =
    Table.add table ~scope:local_scope ~name:late_name.spelling
      ~kind:Semantic_symbol.Local_variable
      ~origin:
        (Holyc_lib__Sema.Initializer_source.origin_of_location
           late_name.location)
    |> checked
  in
  let validate local_name checked_type allocation =
    Record.validate_automatic_aggregate ~table ~parent ~function_symbol
      ~local_symbol ~local_name ~checked_type allocation
  in
  validate late_name checked_type late |> checked;
  reject "a copied local cannot borrow the original allocation"
    (validate clone checked_type late);
  reject "a pointer type cannot consume object allocation authority"
    (validate late_name (Type.pointer_to checked_type |> checked) late);
  reject "another function cannot consume the earlier object's receipt"
    (validate late_name checked_type early);
  let copied_ast =
    Ast.make_module ~source:ast.source ~span:ast.span ~items:ast.items
  in
  let foreign_table = Session.semantic_symbols (Session.create ()) in
  reject "copied command cannot retrieve allocation receipts"
    (D.automatic_aggregate_resolver ~table ~ast:copied_ast command);
  reject "foreign table cannot retrieve source allocation receipts"
    (D.source_automatic_aggregate_resolver ~table:foreign_table ~ast
       source_command);
  let visible =
    D.aggregate_reference_resolver ~table ~ast command |> observed
  in
  let source_visible =
    D.source_aggregate_reference_resolver ~table ~ast source_command |> observed
  in
  let before, after =
    match List.rev !references with
    | [ before; after ] -> (before, after)
    | _ -> Alcotest.fail "expected the two original pointer reads"
  in
  let symbol = Source.selected_base_symbol class_proof in
  let allows reference =
    let identifier = Parser.selected_identifier reference in
    let proof = visible identifier |> Option.get in
    Alcotest.(check bool)
      "source and ordinary views retain the same reference proof" true
      (Option.get (source_visible identifier) == proof);
    Record.aggregate_reference_allows ~table ~parent ~identifier ~symbol proof
  in
  Alcotest.(check bool)
    "later completion cannot authorize an earlier pointer read" false
    (allows before);
  Alcotest.(check bool)
    "a later pointer read observes actual completed class metadata" true
    (allows after);
  let identifier = Parser.selected_identifier after in
  let proof = visible identifier |> Option.get in
  let cloned_identifier =
    Ast.make_identifier ~spelling:identifier.spelling
      ~location:identifier.location
  in
  Alcotest.(check bool)
    "same source coordinates cannot copy reference visibility" false
    (Record.aggregate_reference_allows ~table ~parent
       ~identifier:cloned_identifier ~symbol proof);
  reject "expired identifier cannot mint a new completion snapshot"
    (Record.capture_aggregate_reference_visibility ~table ~namespace after []);
  reject "copied command cannot retrieve reference snapshots"
    (D.aggregate_reference_resolver ~table ~ast:copied_ast command);
  reject "foreign table cannot retrieve source reference snapshots"
    (D.source_aggregate_reference_resolver ~table:foreign_table ~ast
       source_command);
  let bodies = D.function_body_sources ~table ~ast command |> observed in
  Alcotest.(check int)
    "both original function bodies remain authenticated" 2 (List.length bodies);
  List.iter
    (fun (header, body) ->
      ignore (Parser.function_body_compiler_options header body |> checked))
    bodies

let () =
  Alcotest.run "Original local aggregate allocation"
    [
      ( "provenance",
        [
          Alcotest.test_case "live allocation, wrong owners and replay" `Quick
            original_allocation_authority;
          Alcotest.test_case
            "saved allocation and reference timing in JIT source" `Quick
            (source_allocation_and_reference_timing Preprocessor.Jit);
          Alcotest.test_case
            "saved allocation and reference timing in AOT source" `Quick
            (source_allocation_and_reference_timing Preprocessor.Aot);
        ] );
    ]
