open Holyc_lib
module C = Semantic_declaration_collection
module S = Semantic_source_type_reference
module T = Semantic_type
module TR = Semantic_type_reference
module Output = Test_integer_output
module G = Test_integer_globals
module D = Task_declarations

let checked = Test_declaration_collection.checked

let source_cases =
  [
    ( "parameter survives header lookahead shadow",
      {|class C {};I64 F(C *p)#exe {class C {};}{return 0;}42;|} );
    ( "primitive spelling selected as aggregate",
      {|class I64 {};U64 F(I64 *p)#exe {class I64 {};}{return 0;}42;|} );
    ( "unused aggregate pointer parameter",
      {|class C {};I64 F(C *p){return 7;}42;|} );
    ( "aggregate pointer local frame",
      {|class C {};I64 F(){C *p;return 42;}F();|} );
    ( "aggregate pointer local sizeof",
      {|class C {};I64 F(){C *p;return 34+sizeof(p);}F();|} );
    ( "aggregate pointer parameter sizeof admission",
      {|class C {};I64 F(C *p){return sizeof(p);}42;|} );
    ("aggregate pointer prototype return", {|class C {};extern C *Null();42;|});
    ( "aggregate pointer prototype parameter",
      {|class C {};extern I64 Use(C *p);42;|} );
    ( "forward pointer type reaches completed layout",
      {|extern class C;I64 F(C *p){return 0;}class C {I64 x;};34+sizeof(C);|} );
  ]

let source_execution () =
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source) ->
          let report = Output.run ~mode source in
          (match integer_program_report_outcome report with
          | Error (diagnostic :: _) ->
              Alcotest.failf "%s (%s): %s: %s" label
                (match mode with
                | Preprocessor.Jit -> "jit"
                | Aot -> "aot")
                diagnostic.Diagnostic.code diagnostic.message
          | _ -> ());
          ignore (Output.expect ~value:(Some 42L) "" report))
        source_cases)
    G.modes

let word_argument_does_not_become_pointer () =
  List.iter
    (fun mode ->
      let report =
        Output.run ~mode {|class C {};I64 Use(C *p){return 42;}Use(0);|}
      in
      let error =
        Test_integer_functions.first_error
          (integer_program_report_outcome report)
      in
      Alcotest.(check string)
        "raw word argument fails checked pointer admission" "HCIRVM0014"
        error.code;
      Alcotest.(check bool)
        "raw word pointer rejection names the checked parameter boundary" true
        (error.message
       = "pushed argument does not match its checked word or pointer parameter"
        ))
    G.modes

let retained_parameter_sizeof_is_pointer_sized () =
  let session, ledger = Test_task_declarations.setup () in
  let completed = ref None in
  let query event =
    Result.map
      (fun () ->
        match event with
        | Parser.Query_completed receipt -> completed := Some receipt
        | _ -> ())
      (D.observe_query ledger event)
  in
  let output, _ =
    Test_task_declarations.parse ~query session ledger
      {|class C {};I64 F(C *p){return sizeof(p);}42;|}
  in
  let ast = Test_parser.expect_ast output in
  let command = D.seal ledger ast |> Test_integer_program.checked in
  let receipt = Option.get !completed in
  let table = Session.semantic_symbols session in
  let selection =
    D.query_for ~table ~ast command receipt.Parser.query_expression
    |> Test_integer_program.checked |> D.query_selection
  in
  Alcotest.(check (option int64))
    "retained named aggregate parameter sizeof is pointer-sized" (Some 8L)
    (Semantic_query_selection.constant selection)

type aggregate_publication = {
  parser : Parser.aggregate_publication;
  semantic : C.publication;
}

type occurrence = {
  source : S.selected_source;
  semantic : C.publication;
  type_specifier : Ast.type_specifier;
  pointer_layers : Ast.pointer_layer list;
  proof : S.selected_aggregate;
}

type proof_fixture = {
  session : Session.t;
  table : Semantic_symbol_table.t;
  namespace : C.namespace;
  aggregates : aggregate_publication list;
  occurrences : occurrence list;
  rejected_publication : bool;
  rejected_namespace : bool;
  rejected_table : bool;
}

let selection_type = function
  | S.Aggregate_backing publication -> (
      match publication.Parser.aggregate_backing with
      | Some backing ->
          ( backing.backing_type_specifier,
            backing.backing_pointer_layers,
            publication.aggregate_backing_selection )
      | None -> Alcotest.fail "expected original backing type")
  | S.Aggregate_member phase -> (
      match phase.Parser.phase_step with
      | Parser.Aggregate_member_prepared member ->
          (member.member_type, member.member_pointers, member.member_selection)
      | _ -> Alcotest.fail "expected original member type")
  | S.Function_return source ->
      ( source.Parser.function_header.type_specifier,
        source.function_pointer_layers,
        source.function_return_selection )
  | S.Function_parameter source ->
      ( source.Parser.parameter_type_specifier,
        source.parameter_pointer_layers,
        source.parameter_type_selection )
  | S.Callback_return source ->
      ( source.Parser.callback_return_type_specifier,
        source.callback_return_pointer_layers,
        source.callback_return_selection )
  | S.Callback_parameter source ->
      ( source.Parser.callback_parameter_type_specifier,
        source.callback_parameter_pointer_layers,
        source.callback_parameter_type_selection )
  | S.Function_local source -> (
      match source.Parser.allocation_local.local_source with
      | Parser.Local_variable local ->
          ( local.local_type_specifier,
            local.local_pointer_layers,
            local.local_type_selection )
      | _ -> Alcotest.fail "expected original local type")
  | S.Global_type source ->
      ( source.Parser.global_header.type_specifier,
        source.global_pointer_layers,
        source.global_header.declaration_type_selection )

let selection_entry source =
  let _, _, selection = selection_type source in
  Option.map (fun selection -> selection.Parser.entry) selection

let publish_foreign_aggregate session ~namespace ~symbols ~path =
  let source = Session.add_source session ~path ~contents:"class C {};" in
  let publication = ref None in
  let commands : Parser.command_sink =
    {
      lexical_lookup = None;
      checkpoint = None;
      reference = None;
      call = None;
      implicit_output = None;
      query = None;
      dimension_count = None;
      declaration =
        Some
          (fun event ->
            (match event with
            | Parser.Aggregate_declared source ->
                publication :=
                  Some (C.publish_aggregate namespace source |> checked)
            | _ -> ());
            Ok ());
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let parsed =
    Parser.parse ~commands ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols
      ~config:(Preprocessor.Config.create () |> checked)
      source
  in
  ignore (Test_parser.expect_ast parsed);
  Option.get !publication

let parse_proofs ?(duplicate_before = false) ?(callbacks = false) source =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let other_namespace = C.create_namespace ~table () |> checked in
  let same_table_foreign =
    publish_foreign_aggregate session ~namespace:other_namespace
      ~symbols:(Symbol_visibility.Environment.create ())
      ~path:"foreign-namespace.hc"
  in
  let aggregates = ref [] in
  let occurrences = ref [] in
  let rejected_publication = ref false in
  let rejected_namespace = ref false in
  let rejected_table = ref false in
  let foreign_session = Session.create () in
  let foreign_table = Session.semantic_symbols foreign_session in
  let foreign_namespace =
    C.create_namespace ~table:foreign_table () |> checked
  in
  let foreign_table_publication =
    publish_foreign_aggregate foreign_session ~namespace:foreign_namespace
      ~symbols:(Session.symbols foreign_session)
      ~path:"foreign-table.hc"
  in
  let semantic_for_entry entry =
    !aggregates |> List.find (fun item -> item.parser.aggregate_entry == entry)
    |> fun item -> item.semantic
  in
  let mint selected_source =
    match (selection_entry selected_source, selection_type selected_source) with
    | Some entry, (type_specifier, pointer_layers, Some _) ->
        let semantic = semantic_for_entry entry in
        (if duplicate_before then
           match List.rev !aggregates with
           | wrong :: correct :: _ when correct.semantic == semantic ->
               rejected_publication :=
                 Result.is_error
                   (S.select_aggregate ~table ~namespace ~source:selected_source
                      wrong.semantic)
           | _ -> ());
        rejected_namespace :=
          Result.is_error
            (S.select_aggregate ~table ~namespace:other_namespace
               ~source:selected_source same_table_foreign);
        rejected_table :=
          Result.is_error
            (S.select_aggregate ~table:foreign_table
               ~namespace:foreign_namespace ~source:selected_source
               foreign_table_publication);
        let proof =
          S.select_aggregate ~table ~namespace ~source:selected_source semantic
          |> checked
        in
        occurrences :=
          {
            source = selected_source;
            semantic;
            type_specifier;
            pointer_layers;
            proof;
          }
          :: !occurrences
    | None, _ | _, (_, _, None) -> ()
  in
  let declaration event =
    (match event with
    | Parser.Aggregate_declared parser ->
        let semantic = C.publish_aggregate namespace parser |> checked in
        aggregates := { parser; semantic } :: !aggregates
    | Parser.Function_declared source -> mint (S.Function_return source)
    | Parser.Function_parameter_declared source ->
        mint (S.Function_parameter source)
    | Parser.Callback_signature_started source when callbacks ->
        mint (S.Callback_return source)
    | Parser.Callback_parameter_declared source when callbacks ->
        mint (S.Callback_parameter source)
    | Parser.Function_local_allocated source when callbacks ->
        mint (S.Function_local source)
    | Parser.Global_declared source when callbacks ->
        mint (S.Global_type source)
    | _ -> ());
    Ok ()
  in
  let commands : Parser.command_sink =
    {
      lexical_lookup = None;
      checkpoint = None;
      reference = None;
      call = None;
      implicit_output = None;
      query = None;
      dimension_count = None;
      declaration = Some declaration;
      command = (fun _ -> Ok ());
      resume = (fun () -> Ok ());
    }
  in
  let _, _, parsed, _, _, _ =
    Test_stream_parser.parse ~session ~same_task:true ~commands
      ~configure:(fun _ execution -> { execution with Parser.commands })
      source
  in
  ignore (Test_parser.expect_ast parsed);
  {
    session;
    table;
    namespace;
    aggregates = List.rev !aggregates;
    occurrences = List.rev !occurrences;
    rejected_publication = !rejected_publication;
    rejected_namespace = !rejected_namespace;
    rejected_table = !rejected_table;
  }

let aggregate_symbol reference =
  match TR.resolved_type reference |> T.base with
  | T.Aggregate symbol -> symbol
  | T.Primitive _ -> Alcotest.fail "selected aggregate proof resolved primitive"

let publication_aggregate_symbol (publication : aggregate_publication) =
  C.publication_aggregate_identity publication.semantic |> Option.get

let selected_identity_survives_shadow () =
  let fixture =
    parse_proofs {|class C {};I64 F(C *p)#exe {class C {};}{return 0;}42;|}
  in
  match (fixture.aggregates, fixture.occurrences) with
  | [ original; shadow ], [ selected ] ->
      let reference =
        S.selected selected.proof selected.type_specifier
          selected.pointer_layers
        |> checked
      in
      let symbol = aggregate_symbol reference in
      Alcotest.(check bool)
        "parameter keeps the class selected before #exe lookahead" true
        (symbol == publication_aggregate_symbol original);
      Alcotest.(check bool)
        "later same-name class is a distinct semantic identity" true
        (symbol != publication_aggregate_symbol shadow);
      let replayed =
        S.selected selected.proof selected.type_specifier
          selected.pointer_layers
        |> checked |> aggregate_symbol
      in
      Alcotest.(check bool)
        "retained proof is stable after callback and later #exe lookahead" true
        (replayed == symbol);
      Alcotest.(check bool)
        "expired parser owner cannot mint the proof again" true
        (Result.is_error
           (S.select_aggregate ~table:fixture.table ~namespace:fixture.namespace
              ~source:selected.source selected.semantic))
  | _ -> Alcotest.fail "expected two C publications and one selected parameter"

let selected_identity_survives_forward_completion () =
  let session, ledger = Test_task_declarations.setup () in
  let output, events =
    Test_task_declarations.parse session ledger
      {|extern class C;extern I64 Before(C *p);class C {};extern I64 After(C *p);class C {};extern I64 Shadow(C *p);|}
  in
  ignore (Test_parser.expect_ast output);
  let parameters =
    List.filter_map
      (function
        | Parser.Function_parameter_declared parameter -> Some parameter
        | _ -> None)
      events
  in
  let selected_symbol parameter =
    let proof =
      D.selected_aggregate_for ledger parameter.Parser.parameter_type_specifier
      |> Option.get
    in
    S.selected proof parameter.parameter_type_specifier
      parameter.parameter_pointer_layers
    |> checked |> aggregate_symbol
  in
  match List.map selected_symbol parameters with
  | [ before; after; shadow ] ->
      Alcotest.(check bool)
        "parameter selected through forward keeps completed aggregate identity"
        true (before == after);
      Alcotest.(check bool)
        "fresh resolved shadow is a different aggregate identity" true
        (after != shadow)
  | _ -> Alcotest.fail "expected before, completed, and shadow parameter proofs"

let primitive_spelling_keeps_selected_aggregate () =
  let fixture =
    parse_proofs
      {|class I64 {};U64 F(I64 *p)#exe {class I64 {};}{return 0;}42;|}
  in
  match (fixture.aggregates, fixture.occurrences) with
  | [ original; shadow ], [ selected ] ->
      let symbol =
        S.selected selected.proof selected.type_specifier
          selected.pointer_layers
        |> checked |> aggregate_symbol
      in
      Alcotest.(check bool)
        "class spelling I64 remains the originally selected aggregate" true
        (symbol == publication_aggregate_symbol original);
      Alcotest.(check bool)
        "later same-spelling class does not replace selected aggregate" true
        (symbol != publication_aggregate_symbol shadow)
  | _ -> Alcotest.fail "expected two I64 classes and one selected parameter"

let wrong_publication_and_namespace_reject () =
  let fixture =
    parse_proofs ~duplicate_before:true
      {|class C {};class C {};I64 F(C *p)#exe {}{return 0;}42;|}
  in
  Alcotest.(check bool)
    "same-spelling earlier publication cannot mint selected proof" true
    fixture.rejected_publication;
  Alcotest.(check bool)
    "same table but foreign namespace cannot mint selected proof" true
    fixture.rejected_namespace;
  Alcotest.(check bool)
    "foreign table cannot mint selected proof" true fixture.rejected_table

let reconstructed_type_and_pointer_children_reject () =
  let fixture = parse_proofs {|class C {};extern I64 F(C *p);|} in
  match fixture.occurrences with
  | [ selected ] ->
      let copied_type =
        match selected.type_specifier with
        | Ast.Named_type_specifier identifier ->
            Ast.Named_type_specifier
              (Ast.make_identifier ~spelling:identifier.spelling
                 ~location:identifier.location)
        | _ -> Alcotest.fail "expected selected named type"
      in
      let copied_pointers =
        List.map
          (fun (layer : Ast.pointer_layer) ->
            Ast.make_pointer_layer ~depth:layer.depth ~spelling:layer.spelling
              ~location:layer.location)
          selected.pointer_layers
      in
      Alcotest.(check bool)
        "equal-looking named AST cannot consume proof" true
        (Result.is_error
           (S.selected selected.proof copied_type selected.pointer_layers));
      Alcotest.(check bool)
        "equal-looking pointer children cannot consume proof" true
        (Result.is_error
           (S.selected selected.proof selected.type_specifier copied_pointers))
  | _ -> Alcotest.fail "expected one selected aggregate parameter"

let activation_replay_reuses_retained_proof () =
  let session, source, ledger =
    Test_source_promotion.inputs {|class C {};I64 F(C *p){return 0;}#exe {}42;|}
  in
  let parameter = ref None in
  let original_proof = ref None in
  let replayed = ref false in
  let declaration event =
    let result = D.observe ledger event in
    (match (result, event) with
    | Ok (), Parser.Function_parameter_declared source ->
        parameter := Some source;
        original_proof :=
          D.selected_aggregate_for ledger source.parameter_type_specifier
    | _ -> ());
    result
  in
  let enter span =
    let parameter = Option.get !parameter in
    let proof = Option.get !original_proof in
    let runtime =
      Ir_integer_interpreter.create_task_state
        ~table:(Session.semantic_symbols session)
        ()
      |> checked
    in
    D.promote_source ledger ~runtime session ~source |> checked;
    D.activate_source ledger ~runtime ~span
      ~declaration:(fun event ->
        (match event with
        | Parser.Function_parameter_declared candidate
          when candidate == parameter ->
            replayed := true;
            Alcotest.(check bool)
              "activation reads the retained proof instead of minting a \
               replacement"
              true
              (Option.fold ~none:false ~some:(( == ) proof)
                 (D.selected_aggregate_for ledger
                    candidate.parameter_type_specifier))
        | _ -> ());
        Ok ())
      ~command:(fun _ -> Ok ())
    |> Test_integer_program.checked;
    Error
      [
        Diagnostic.make ~code:"TEST" ~severity:Diagnostic.Error ~primary:span
          ~message:"stop after selected aggregate activation replay" ();
      ]
  in
  let parsed =
    Test_source_promotion.parse ~declaration ~execute_stream:enter session
      source ledger
  in
  Alcotest.(check bool)
    "activation probe deliberately stops the outer parse" true
    (Parser.has_errors parsed);
  Alcotest.(check bool) "selected parameter event replayed" true !replayed;
  let parameter = Option.get !parameter in
  Alcotest.(check bool)
    "activation leaves the same occurrence proof retained" true
    (match !original_proof with
    | Some proof ->
        Option.fold ~none:false ~some:(( == ) proof)
          (D.selected_aggregate_for ledger parameter.parameter_type_specifier)
    | None -> false)

let callback_selection_survives_shadow () =
  let fixture =
    parse_proofs ~callbacks:true
      {|class C {};C (*g)();I64 Run(){C (*p)(),*(*q)();I64 (*s)(C (*n)(),C *x);return 42;}class C {I64 n;};42;|}
  in
  let original, shadow =
    match fixture.aggregates with
    | [ original; shadow ] -> (original, shadow)
    | _ -> Alcotest.fail "expected original and shadow class"
  in
  Alcotest.(check bool)
    "callback receipts captured" true
    (List.length fixture.occurrences >= 8);
  List.iter
    (fun selected ->
      let callback_metadata =
        match selected.source with
        | S.Callback_return _ -> true
        | S.Callback_parameter p ->
            Option.is_some p.callback_parameter_function_pointer
        | S.Function_parameter p -> Option.is_some p.parameter_function_pointer
        | S.Function_local p -> (
            match p.allocation_local.local_source with
            | Parser.Local_variable local ->
                Option.is_some local.local_function_pointer
            | _ -> false)
        | S.Global_type p -> Option.is_some p.global_function_pointer
        | S.Function_return _ | S.Aggregate_backing _ -> false
        | S.Aggregate_member phase -> (
            match phase.phase_step with
            | Parser.Aggregate_member_prepared member ->
                Option.is_some member.member_callback
            | _ -> false)
      in
      let reference =
        (if callback_metadata then S.selected_callback_return else S.selected)
          selected.proof selected.type_specifier selected.pointer_layers
        |> checked
      in
      let symbol = aggregate_symbol reference in
      Alcotest.(check bool)
        "metadata keeps original canonical class" true
        (symbol == publication_aggregate_symbol original);
      Alcotest.(check bool)
        "shadow cannot replace callback metadata" true
        (symbol != publication_aggregate_symbol shadow);
      Alcotest.(check bool)
        "expired receipt cannot mint again" true
        (Result.is_error
           (S.select_aggregate ~table:fixture.table ~namespace:fixture.namespace
              ~source:selected.source selected.semantic));
      if callback_metadata && selected.pointer_layers = [] then
        Alcotest.(check bool)
          "return metadata grants no aggregate layout" true
          (Result.is_error
             (S.selected selected.proof selected.type_specifier [])))
    fixture.occurrences;
  Alcotest.(check bool)
    "foreign namespace rejected at mint" true fixture.rejected_namespace;
  Alcotest.(check bool)
    "foreign table rejected at mint" true fixture.rejected_table

let comma_callback_pointer_children () =
  let fixture =
    parse_proofs ~callbacks:true
      {|class C {};I64 Run(){C (*p)(),*(*q)();return 42;}|}
  in
  let pointers =
    List.filter
      (fun o ->
        match o.source with
        | S.Callback_return _ -> true
        | _ -> false)
      fixture.occurrences
  in
  let first, second =
    match pointers with
    | [ first; second ] -> (first, second)
    | _ -> Alcotest.fail "expected two original callback returns"
  in
  Alcotest.(check bool)
    "comma declarators share the original base type" true
    (first.type_specifier == second.type_specifier);
  Alcotest.(check bool)
    "one receipt cannot borrow sibling pointer children" true
    (Result.is_error
       (S.selected_callback_return first.proof second.type_specifier
          second.pointer_layers));
  let joined = S.merge_selections first.proof second.proof |> checked in
  List.iter
    (fun original ->
      ignore
        (S.selected_callback_return joined original.type_specifier
           original.pointer_layers
        |> checked))
    pointers;
  let copied =
    List.map
      (fun (layer : Ast.pointer_layer) ->
        Ast.make_pointer_layer ~depth:layer.depth ~spelling:layer.spelling
          ~location:layer.location)
      second.pointer_layers
  in
  Alcotest.(check bool)
    "equal-looking pointer children cannot replace originals" true
    (Result.is_error
       (S.selected_callback_return joined second.type_specifier copied));
  let other =
    parse_proofs ~callbacks:true {|class C {};I64 Run(){C (*p)();return 42;}|}
  in
  let other =
    List.find
      (fun o ->
        match o.source with
        | S.Callback_return _ -> true
        | _ -> false)
      other.occurrences
  in
  Alcotest.(check bool)
    "foreign source occurrence cannot join" true
    (Result.is_error (S.merge_selections joined other.proof));
  Alcotest.(check bool)
    "foreign table cannot consume retained metadata" true
    (Result.is_error
       (S.validate_selected_aggregate ~table:fixture.table
          ~namespace:fixture.namespace other.proof))

let callback_forward_and_lookahead_identity () =
  List.iter
    (fun text ->
      let fixture = parse_proofs ~callbacks:true text in
      let original, completed, shadow =
        match fixture.aggregates with
        | [ original; completed; shadow ] -> (original, completed, shadow)
        | _ -> Alcotest.fail "expected original forward, completion and shadow"
      in
      let original_symbol = publication_aggregate_symbol original in
      Alcotest.(check bool)
        "forward completion keeps its canonical class" true
        (original_symbol == publication_aggregate_symbol completed);
      List.iter
        (fun occurrence ->
          match occurrence.source with
          | S.Callback_return _ ->
              let symbol =
                S.selected_callback_return occurrence.proof
                  occurrence.type_specifier occurrence.pointer_layers
                |> checked |> aggregate_symbol
              in
              Alcotest.(check bool)
                "callback retains the token-selected class" true
                (symbol == original_symbol);
              Alcotest.(check bool)
                "later shadow cannot replace callback class" true
                (symbol != publication_aggregate_symbol shadow);
              let copied_type =
                match occurrence.type_specifier with
                | Ast.Named_type_specifier identifier ->
                    Ast.Named_type_specifier
                      (Ast.make_identifier ~spelling:identifier.spelling
                         ~location:identifier.location)
                | _ -> Alcotest.fail "expected callback class"
              in
              Alcotest.(check bool)
                "copied callback type cannot consume proof" true
                (Result.is_error
                   (S.selected_callback_return occurrence.proof copied_type
                      occurrence.pointer_layers))
          | _ -> ())
        fixture.occurrences)
    [
      {|extern class C;C (*g)();class C {};class C {U8 x;};42;|};
      {|extern class C;class C {};I64 Run(C (*p)()=42)#exe {class C {U8 x;};}{return p;}42;|};
    ]

let tests =
  [
    Alcotest.test_case "callback metadata retains original class selection"
      `Quick callback_selection_survives_shadow;
    Alcotest.test_case "comma callback returns retain original pointer children"
      `Quick comma_callback_pointer_children;
    Alcotest.test_case
      "callback forward and lookahead selections retain identity" `Quick
      callback_forward_and_lookahead_identity;
    Alcotest.test_case "public source aggregate pointer frames" `Quick
      source_execution;
    Alcotest.test_case "word argument cannot become aggregate pointer" `Quick
      word_argument_does_not_become_pointer;
    Alcotest.test_case "retained aggregate parameter sizeof is pointer-sized"
      `Quick retained_parameter_sizeof_is_pointer_sized;
    Alcotest.test_case "selected identity survives #exe shadow" `Quick
      selected_identity_survives_shadow;
    Alcotest.test_case "selected identity survives forward completion" `Quick
      selected_identity_survives_forward_completion;
    Alcotest.test_case "primitive spelling keeps aggregate selection" `Quick
      primitive_spelling_keeps_selected_aggregate;
    Alcotest.test_case "publication namespace and table substitution reject"
      `Quick wrong_publication_and_namespace_reject;
    Alcotest.test_case "reconstructed type children reject" `Quick
      reconstructed_type_and_pointer_children_reject;
    Alcotest.test_case "activation replay reuses retained proof" `Quick
      activation_replay_reuses_retained_proof;
  ]
