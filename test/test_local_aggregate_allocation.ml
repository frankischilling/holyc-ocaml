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
            (receipt.allocation_storage = Ast.Automatic_local
            || receipt.allocation_storage = Ast.Static_local)
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
            reject "pointer storage is not a class object allocation"
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
  Alcotest.(check int) "four real object allocations" 4 (List.length !saved);
  Alcotest.(check int) "pointer refusal was reached" 1 !excluded;
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
    [ "pointer" ];
  let held = saved "held" in
  Alcotest.(check int64)
    "static retains its original class size" 2L
    (Record.automatic_aggregate_size held);
  (match
     (mode, Record.static_allocation (D.compiler_positions ledger) (find "held"))
   with
  | Preprocessor.Jit, Some original ->
      Alcotest.(check bool)
        "JIT static allocation carries its exact class receipt" true
        (Option.fold ~none:false ~some:(( == ) held)
           (Record.static_allocation_aggregate original))
  | Preprocessor.Aot, None -> ()
  | _ ->
      Alcotest.fail
        "static allocation has the wrong native or output source authority");
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

let completed_base_observation_authority () =
  List.iter
    (fun mode ->
      let session = Session.create () in
      let table = Session.semantic_symbols session in
      let source =
        Session.add_source session ~path:"base-member-observation.hc"
          ~contents:
            "extern class Base;class Child:Base{U8 tail;};I64 Before(){Child \
             early;return early.tail;}class Base{U8 head;};I64 After(){Child \
             late;return late.head;}"
      in
      let ledger = D.create_source session ~source |> checked in
      let allocations = ref [] and members = ref [] in
      let declaration event =
        D.observe ledger event |> observed;
        (match event with
        | Parser.Function_local_allocated receipt ->
            allocations := receipt :: !allocations
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
                if Parser.selected_member_reference reference then
                  members := reference :: !members;
                Ok ());
          query = Some (D.observe_query ledger);
          dimension_count = Some (D.grammar_dimension_count ledger);
        }
      in
      let ast = parse session mode source commands in
      let command = D.seal ledger ast |> observed in
      let namespace, _ =
        D.selected_type_resolver ~table ~ast command |> observed
      in
      let parent = C.namespace_scope namespace in
      let resolve =
        D.automatic_aggregate_resolver ~table ~ast command |> observed
      in
      let local_name receipt =
        match receipt.Parser.allocation_local.local_source with
        | Parser.Local_variable local -> local.local_name
        | _ -> Alcotest.fail "expected original object allocation"
      in
      let allocation =
        resolve (local_name (List.hd !allocations)) |> Option.get
      in
      let child =
        match Type.base (Record.automatic_aggregate_type allocation) with
        | Type.Aggregate symbol -> symbol
        | _ -> Alcotest.fail "expected Child"
      in
      let visible =
        D.aggregate_reference_resolver ~table ~ast command |> observed
      in
      let read reference =
        let identifier = Parser.selected_identifier reference in
        let proof = visible identifier |> Option.get in
        let record =
          Record.aggregate_reference_completed_record ~table ~parent ~identifier
            ~symbol:child proof
          |> Option.get
        in
        (identifier, proof, record)
      in
      let before, after =
        match List.rev !members with
        | [ before; after ] -> (before, after)
        | _ -> Alcotest.fail "expected two original member observations"
      in
      let _, _, earlier = read before in
      let identifier, proof, later = read after in
      Alcotest.(check bool)
        "future completion cannot change the earlier observation" true
        (Option.is_none (Record.aggregate_reached_field earlier "head"));
      let _, depth, field =
        Record.aggregate_reached_field later "head" |> Option.get
      in
      Alcotest.(check int) "original completed base is reached" 1 depth;
      Alcotest.(check int64)
        "base member retains zero offset" 0L
        (Record.aggregate_field_offset field);
      let copied =
        Ast.make_identifier ~spelling:identifier.spelling
          ~location:identifier.location
      in
      let unavailable table parent identifier =
        Option.is_none
          (Record.aggregate_reference_completed_record ~table ~parent
             ~identifier ~symbol:child proof)
      in
      Alcotest.(check bool)
        "copied member cannot retrieve completed metadata" true
        (unavailable table parent copied);
      let foreign_table = Session.semantic_symbols (Session.create ()) in
      Alcotest.(check bool)
        "foreign table cannot retrieve completed metadata" true
        (unavailable foreign_table parent identifier);
      let foreign_namespace = C.create_namespace ~table () |> checked in
      Alcotest.(check bool)
        "foreign namespace cannot retrieve completed metadata" true
        (unavailable table (C.namespace_scope foreign_namespace) identifier);
      let definition =
        Ast.declaration_items ast
        |> List.find_map (function
          | _, Ast.Aggregate_definition definition
            when definition.name.spelling = "Child" -> Some definition
          | _ -> None)
        |> Option.get
      in
      let header =
        Record.retain_aggregate_value_header ~table ~namespace definition
          (Record.automatic_aggregate_record allocation)
        |> checked
      in
      Alcotest.(check bool)
        "original header retains its forward base" true
        (Option.is_some
           (Record.aggregate_value_header_base ~table ~scope:parent header));
      Alcotest.(check bool)
        "foreign scope cannot retrieve original base" true
        (Option.is_none
           (Record.aggregate_value_header_base ~table
              ~scope:(C.namespace_scope foreign_namespace)
              header)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let assignment_observation_authority () =
  List.iter
    (fun mode ->
      let session = Session.create () in
      let table = Session.semantic_symbols session in
      let source =
        Session.add_source session ~path:"assignment-observation-authority.hc"
          ~contents:
            "U16 class C{U16 value;};I64 F(){C \
             object,items[2];items[0].value=1;object=42;return \
             object.value;}I64 G(){C \
             object,items[2];items[1].value=1;object=42;return object.value;}"
      in
      let ledger = D.create_source session ~source |> checked in
      let selections = ref []
      and indices = ref []
      and class_entry = ref None
      and bodies = ref [] in
      let declaration event =
        D.observe ledger event |> observed;
        (match event with
        | Parser.Aggregate_completed original ->
            class_entry := Some original.aggregate_publication.aggregate_entry
        | Parser.Index_operand_selected selection ->
            indices := selection :: !indices;
            reject "a live index observation cannot be consumed twice"
              (D.observe ledger event)
        | Parser.Assignment_operand_preparing _ ->
            reject "a live lvalue preparation cannot be consumed twice"
              (D.observe ledger event)
        | Parser.Assignment_operand_selected selection ->
            selections := selection :: !selections;
            reject "a live assignment observation cannot be consumed twice"
              (D.observe ledger event)
        | Parser.Function_body_completed (header, body) ->
            bodies := (header, body) :: !bodies
        | _ -> ());
        Ok ()
      in
      let commands =
        {
          (sink declaration) with
          checkpoint = Some (D.observe_command ledger);
          reference = Some (D.observe_reference ledger);
          query = Some (D.observe_query ledger);
          dimension_count = Some (D.grammar_dimension_count ledger);
        }
      in
      let ast = parse session mode source commands in
      let command = D.seal ledger ast |> observed in
      let resolve =
        D.aggregate_assignment_resolver ~table ~ast command |> observed
      in
      let namespace, _ =
        D.selected_type_resolver ~table ~ast command |> observed
      in
      let parent = C.namespace_scope namespace in
      let type_ =
        Type.make_aggregate
          ~symbol:(D.symbol_for ledger (Option.get !class_entry) |> Option.get)
          ~pointer_depth:0
        |> checked
      in
      let index_resolve =
        D.aggregate_index_resolver ~table ~ast command |> observed
      in
      let index_selection = List.hd !indices in
      let index_source = Parser.index_source index_selection |> Option.get in
      let index_proof = index_resolve index_source |> Option.get in
      let index_function =
        Parser.index_function index_selection |> Option.get
      in
      let index_query ~table ~parent ~function_source ~source ~element_type =
        Record.aggregate_index_element_size ~table ~parent ~function_source
          ~source ~element_type index_proof
      in
      Alcotest.(check (option int64))
        "original index observes the class size" (Some 2L)
        (index_query ~table ~parent ~function_source:index_function
           ~source:index_source ~element_type:type_);
      let copied_index =
        match index_source with
        | Ast.Index_expression value ->
            Ast.Index_expression
              (Ast.make_index_expression ~base:value.index_base
                 ~opening_bracket:value.index_opening_bracket
                 ~index:value.index_value
                 ~closing_bracket:value.index_closing_bracket
                 ~location:value.index_location)
        | _ -> Alcotest.fail "expected an original index"
      in
      let index_absent label value =
        Alcotest.(check bool) label true (Option.is_none value)
      in
      index_absent "copied index cannot resolve source metadata"
        (index_resolve copied_index);
      index_absent "copied index cannot consume its original size"
        (index_query ~table ~parent ~function_source:index_function
           ~source:copied_index ~element_type:type_);
      index_absent "another function cannot consume index metadata"
        (index_query ~table ~parent ~source:index_source ~element_type:type_
           ~function_source:
             (Parser.index_function (List.nth !indices 1) |> Option.get));
      index_absent "another table cannot consume index metadata"
        (index_query
           ~table:(Session.semantic_symbols (Session.create ()))
           ~parent ~function_source:index_function ~source:index_source
           ~element_type:type_);
      index_absent "another namespace cannot consume index metadata"
        (index_query ~table
           ~parent:(C.create_namespace ~table () |> checked |> C.namespace_scope)
           ~function_source:index_function ~source:index_source
           ~element_type:type_);
      index_absent "a pointer storage slot is not a class element"
        (index_query ~table ~parent ~function_source:index_function
           ~source:index_source
           ~element_type:(Type.pointer_to type_ |> checked));
      reject "expired index cannot mint another size observation"
        (Record.capture_aggregate_index_visibility ~table ~namespace
           index_selection []);
      let selection = List.hd !selections in
      let original = Parser.assignment_source selection |> Option.get in
      let proof = resolve original |> Option.get in
      let function_source =
        Parser.assignment_function selection |> Option.get
      in
      let query ~table ~parent ~function_source ~source ~source_type =
        Record.aggregate_assignment_value_type ~table ~parent ~function_source
          ~source ~source_type proof
      in
      let selected =
        query ~table ~parent ~function_source ~source:original
          ~source_type:type_
        |> Option.get
      in
      Alcotest.(check bool)
        "original assignment keeps its U16 storage" true
        (match Type.base selected with
        | Type.Primitive (_, Primitive_type.U16) -> true
        | _ -> false);
      let copied =
        match original with
        | Ast.Binary_expression value ->
            Ast.Binary_expression
              (Ast.make_binary_expression ~left:value.binary_left
                 ~operator:value.binary_operator
                 ~operator_spec:value.binary_operator_spec
                 ~right:value.binary_right ~location:value.binary_location)
        | _ -> Alcotest.fail "expected an original assignment"
      in
      let absent label result =
        Alcotest.(check bool) label true (Option.is_none result)
      in
      absent "same coordinates cannot copy assignment backing" (resolve copied);
      absent "copied binary cannot borrow the original observation"
        (query ~table ~parent ~function_source ~source:copied ~source_type:type_);
      absent "another function cannot consume assignment backing"
        (query ~table ~parent
           ~function_source:
             (List.find_map
                (fun selection ->
                  match Parser.assignment_function selection with
                  | Some original when original != function_source ->
                      Some original
                  | _ -> None)
                !selections
             |> Option.get)
           ~source:original ~source_type:type_);
      absent "another table cannot consume assignment backing"
        (query
           ~table:(Session.semantic_symbols (Session.create ()))
           ~parent ~function_source ~source:original ~source_type:type_);
      absent "another namespace cannot consume assignment backing"
        (query ~table
           ~parent:(C.create_namespace ~table () |> checked |> C.namespace_scope)
           ~function_source ~source:original ~source_type:type_);
      absent "a pointer type cannot consume a scalar assignment observation"
        (query ~table ~parent ~function_source ~source:original
           ~source_type:(Type.pointer_to type_ |> checked));
      reject "expired lookahead cannot mint another backing observation"
        (Record.capture_aggregate_assignment_visibility ~table ~namespace
           selection []);
      reject "expired lvalue cannot mint another endpoint preparation"
        (Record.prepare_aggregate_assignment_visibility ~table ~namespace
           selection []);
      let header, body = List.hd !bodies in
      reject "function completion cannot be replayed for another storage view"
        (Record.complete_aggregate_assignment_visibility ~table ~namespace
           ~header ~body [] proof))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let () =
  Alcotest.run "Original local aggregate allocation"
    [
      ( "provenance",
        [
          Alcotest.test_case "assignment lookahead, completion and wrong owners"
            `Quick assignment_observation_authority;
          Alcotest.test_case "live allocation, wrong owners and replay" `Quick
            original_allocation_authority;
          Alcotest.test_case
            "completed inherited member observations and original base \
             authority"
            `Quick completed_base_observation_authority;
          Alcotest.test_case
            "saved allocation and reference timing in JIT source" `Quick
            (source_allocation_and_reference_timing Preprocessor.Jit);
          Alcotest.test_case
            "saved allocation and reference timing in AOT source" `Quick
            (source_allocation_and_reference_timing Preprocessor.Aot);
        ] );
    ]
