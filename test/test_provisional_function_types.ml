open Holyc_lib
module N = Semantic_function_record_phase
module P = Semantic_provisional_function
module C = Semantic_declaration_collection
module H = Semantic_function_type_resolution
module D = Holyc_lib__Driver.Function_type_resolution
module S = Semantic_symbol_table
module SR = Semantic_source_type_reference

let checked = Test_declaration_collection.checked

let fixture source =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let registry =
    N.create_registry ~mode:Preprocessor.Jit ~table ~namespace |> checked
  in
  let records = ref [] and snapshots = ref [] in
  let declaration event =
    (match event with
    | Parser.Function_declared source ->
        let publication = C.publish_function namespace source |> checked in
        records :=
          (N.begin_header registry publication source |> checked) :: !records
    | _ ->
        List.iter
          (fun record ->
            if N.event_belongs record event then
              N.observe record event |> checked)
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
        | record :: _ -> snapshots := N.snapshot record :: !snapshots
        | [] -> ())
      source
  in
  ignore (Test_parser.expect_ast parsed);
  (table, namespace, List.rev !snapshots)

let one source =
  match fixture source with
  | table, namespace, [ snapshot ] -> (table, namespace, snapshot)
  | _ -> Alcotest.fail "expected one source phase"

let selected_one source =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let registry =
    N.create_registry ~mode:Preprocessor.Jit ~table ~namespace |> checked
  in
  let aggregate_publications = ref [] in
  let selected_aggregates = ref [] in
  let records = ref [] and snapshots = ref [] in
  let semantic_aggregate entry =
    !aggregate_publications
    |> List.find (fun (source, _) -> source.Parser.aggregate_entry == entry)
    |> snd
  in
  let retain source type_specifier selection =
    match selection with
    | None -> ()
    | Some selection ->
        let publication = semantic_aggregate selection.Parser.entry in
        let proof =
          SR.select_aggregate ~table ~namespace ~source publication |> checked
        in
        selected_aggregates := (type_specifier, proof) :: !selected_aggregates
  in
  let declaration event =
    (match event with
    | Parser.Aggregate_declared source ->
        aggregate_publications :=
          (source, C.publish_aggregate namespace source |> checked)
          :: !aggregate_publications
    | Parser.Function_declared source ->
        retain (SR.Function_return source) source.function_header.type_specifier
          source.function_return_selection;
        let publication = C.publish_function namespace source |> checked in
        records :=
          (N.begin_header registry publication source |> checked) :: !records
    | Parser.Function_parameter_declared source ->
        retain (SR.Function_parameter source) source.parameter_type_specifier
          source.parameter_type_selection;
        List.iter
          (fun record ->
            if N.event_belongs record event then
              N.observe record event |> checked)
          !records
    | _ ->
        List.iter
          (fun record ->
            if N.event_belongs record event then
              N.observe record event |> checked)
          !records);
    Ok ()
  in
  let commands = Test_provisional_function_parser.sink declaration in
  let _, _, parsed, _, _, _ =
    Test_stream_parser.parse ~session ~same_task:true ~commands
      ~configure:(fun _ execution -> { execution with Parser.commands })
      ~on_enter:(fun () ->
        match !records with
        | record :: _ -> snapshots := N.snapshot record :: !snapshots
        | [] -> ())
      source
  in
  ignore (Test_parser.expect_ast parsed);
  let resolver type_specifier =
    List.find_map
      (fun (source, proof) ->
        if source == type_specifier then Some proof else None)
      !selected_aggregates
  in
  match List.rev !snapshots with
  | [ snapshot ] ->
      (table, namespace, resolver, List.rev !aggregate_publications, snapshot)
  | _ -> Alcotest.fail "expected one selected aggregate source phase"

let resolve table namespace snapshot =
  D.resolve_provisional_call ~table ~namespace (N.call_shape snapshot |> checked)

let zero_projection () =
  List.iter
    (fun prefix ->
      let table, namespace, snapshot =
        one (prefix ^ "I64 F(I64 n=40)#exe {};")
      in
      let symbols = List.length (S.all_symbols table) in
      let function_ = resolve table namespace snapshot |> checked in
      Alcotest.(check int)
        "source still has n" 1
        (List.length (P.members (N.source_snapshot snapshot)));
      Alcotest.(check int)
        "native active argument count is zero" 0
        (List.length (H.signature_parameters (H.function_signature function_)));
      Alcotest.(check bool)
        "explicit provisional evidence" true
        (Option.is_some (H.function_provisional_call function_));
      Alcotest.(check bool)
        "no completed header invented" true
        (Option.is_none (H.function_completed_header function_));
      Alcotest.(check int)
        "no parameter locals invented" symbols
        (List.length (S.all_symbols table));
      Alcotest.(check int)
        "no executable parameter bindings" 0
        (List.length (H.function_parameter_bindings function_)))
    [ ""; "extern I64 F(I64 old);" ]

let completed_native_members () =
  let table, namespace, snapshot = one "extern I64 F(I64 n=40);#exe {}" in
  let function_ = resolve table namespace snapshot |> checked in
  let parameter =
    match H.signature_parameters (H.function_signature function_) with
    | [ p ] -> p
    | _ -> Alcotest.fail "expected native fixed member"
  in
  let original =
    List.hd (N.fixed_members (N.call_shape snapshot |> checked))
    |> P.member_completion |> Option.get
  in
  Alcotest.(check bool)
    "exact native parameter child" true
    (H.parameter_source parameter = Some original.Parser.parameter_ast
    && Option.get (H.parameter_source parameter) == original.parameter_ast);
  Alcotest.(check bool)
    "default retained" true
    (Option.is_some (H.parameter_default parameter));
  Alcotest.(check int)
    "call projection creates no local bindings" 0
    (List.length (H.function_parameter_bindings function_))

let variadic_cursor () =
  let table, namespace, snapshots = fixture "I64 F(...#exe {})#exe {};" in
  match snapshots with
  | [ before_members; after_members ] ->
      let before = resolve table namespace before_members |> checked in
      let after = resolve table namespace after_members |> checked in
      Alcotest.(check bool)
        "ellipsis flag is not a call tail" true
        (Option.is_none (H.function_variadic_count_type before));
      Alcotest.(check bool)
        "actual native cursor provides count type" true
        (Option.is_some (H.function_variadic_count_type after));
      Alcotest.(check bool)
        "no synthetic locals" true
        (Option.is_none (H.function_variadic_bindings after))
  | _ -> Alcotest.fail "expected ellipsis boundaries"

let direct_call function_ values =
  let module R = Semantic_function_call_resolution in
  let origin = H.signature_opening_origin (H.function_signature function_) in
  let arguments =
    List.mapi
      (fun index value ->
        let kind, expression =
          match value with
          | Some value ->
              ( R.Provided,
                Some
                  (R.make_argument_expression ~kind:(R.Integer_literal value)
                     ~origin) )
          | None -> (R.Omitted, None)
        in
        R.make_argument ~index ~kind ~expression ~origin |> checked)
      values
  in
  R.make_call ~index:0 ~callee_occurrence_index:0 ~callee_name:"F"
    ~callee_origin:origin ~origin ~syntax:R.Parenthesized arguments
  |> checked

let bind_call function_ call =
  match
    Semantic_function_call_resolution.bind_direct_arguments call function_
  with
  | Ok bound -> bound
  | Error error ->
      Alcotest.fail (Semantic_function_call_resolution.error_to_string error)

let variadic_argument_cursor () =
  let module R = Semantic_function_call_resolution in
  let table, namespace, snapshots = fixture "I64 F(...#exe {})#exe {};" in
  match snapshots with
  | [ before_members; after_members ] ->
      let before = resolve table namespace before_members |> checked in
      let after = resolve table namespace after_members |> checked in
      let symbols = List.length (S.all_symbols table) in
      let call = direct_call after [ Some 40L; Some 2L ] in
      (match R.bind_direct_arguments call before with
      | Error error -> (
          match R.error_kind error with
          | R.Extra_fixed_argument { fixed_count = 0; _ } -> ()
          | _ -> Alcotest.fail "expected surplus before native tail publication"
          )
      | Ok _ -> Alcotest.fail "ellipsis flag alone admitted arguments");
      let fixed, variadic, count = bind_call after call in
      Alcotest.(check int) "no fixed members" 0 (List.length fixed);
      Alcotest.(check int64) "two source variadic values" 2L count;
      Alcotest.(check bool)
        "variadic source order and identity" true
        (List.for_all2 ( == ) variadic (R.call_arguments call));
      Alcotest.(check int)
        "argument binding creates no locals" symbols
        (List.length (S.all_symbols table));
      Alcotest.(check bool)
        "projection still has no body bindings" true
        (Option.is_none (H.function_variadic_bindings after))
  | _ -> Alcotest.fail "expected ellipsis boundaries"

let variadic_fixed_default () =
  let module R = Semantic_function_call_resolution in
  let table, namespace, snapshot = one "extern I64 F(I64 n=40,...);#exe {}" in
  let function_ = resolve table namespace snapshot |> checked in
  let call = direct_call function_ [ None; Some 2L; Some 3L ] in
  let fixed, variadic, count = bind_call function_ call in
  Alcotest.(check int64) "only supplied tail contributes to count" 2L count;
  Alcotest.(check int) "two tail values" 2 (List.length variadic);
  (match fixed with
  | [ argument ] -> (
      Alcotest.(check bool)
        "fixed parameter retains original projection member" true
        (R.fixed_parameter argument
        == List.hd (H.signature_parameters (H.function_signature function_)));
      match R.fixed_value argument with
      | R.Declared_default _ -> ()
      | _ -> Alcotest.fail "fixed omission did not retain its declared default")
  | _ -> Alcotest.fail "expected one fixed default");
  match
    R.bind_direct_arguments (direct_call function_ [ Some 40L; None ]) function_
  with
  | Error error -> (
      match R.error_kind error with
      | R.Omitted_variadic_argument _ -> ()
      | _ -> Alcotest.fail "expected an omitted variadic slot error")
  | Ok _ -> Alcotest.fail "omitted variadic slot was accepted"

let ordinary_authority_rejected () =
  let table, namespace, snapshot = one "I64 F(I64 n=40)#exe {};" in
  let function_ = resolve table namespace snapshot |> checked in
  let signature = H.function_signature function_ in
  Alcotest.(check bool)
    "ordinary constructor rejects provisional signature" true
    (Result.is_error
       (H.make_function
          ~symbol:(H.function_symbol function_)
          ~scope:(H.function_scope function_)
          ~item_index:0
          ~return_type:(H.function_return_type function_)
          ~signature ~parameter_bindings:[] ~variadic_bindings:None));
  let opening = H.signature_opening_origin signature in
  Alcotest.(check bool)
    "callback constructor rejects call-only signature" true
    (Result.is_error
       (H.make_function_pointer ~origin:opening ~opening_origin:opening
          ~indirection_origins:[ opening ] ~closing_origin:opening ~signature))

let foreign_namespace () =
  let table, namespace, snapshot = one "I64 F()#exe {};" in
  let foreign = C.create_namespace ~table () |> checked in
  let scope_count = List.length (S.all_scopes table) in
  Alcotest.(check bool)
    "foreign namespace rejected" true
    (Result.is_error (resolve table foreign snapshot));
  Alcotest.(check int)
    "rejection allocates no scope" scope_count
    (List.length (S.all_scopes table));
  let other_table = S.create () in
  Alcotest.(check bool)
    "foreign table rejected" true
    (Result.is_error (resolve other_table namespace snapshot));
  ignore (resolve table namespace snapshot |> checked)

let ledger_native_phases () =
  let session, ledger = Test_task_declarations.setup () in
  let snapshots = ref [] in
  let observe event =
    let result = Task_declarations.observe ledger event in
    (match (result, event) with
    | Ok (), Parser.Function_parameter_declared p ->
        snapshots :=
          (Task_declarations.function_record_snapshot ledger
             p.parameter_function
          |> Test_integer_program.checked)
          :: !snapshots
    | Ok (), Parser.Function_header_completed h ->
        snapshots :=
          (Task_declarations.function_record_snapshot ledger
             h.function_publication
          |> Test_integer_program.checked)
          :: !snapshots
    | _ -> ());
    result
  in
  let output, events =
    Test_task_declarations.parse ~observe session ledger
      "extern I64 F(I64 old);I64 F(I64 n=40){return n;}I64 F(I64 x);"
  in
  ignore (Test_parser.expect_ast output);
  match List.rev !snapshots with
  | [ first_head; first_complete; second_head; second_complete; third_head; _ ]
    ->
      Alcotest.(check (option int))
        "member does not complete native count" (Some 0)
        (N.argument_count first_head);
      Alcotest.(check (option int))
        "header establishes count" (Some 1)
        (N.argument_count first_complete);
      Alcotest.(check (option int))
        "replacement clears active count" (Some 0)
        (N.argument_count second_head);
      Alcotest.(check bool)
        "extern replacement shares identity" true
        (N.same_identity first_head second_complete);
      Alcotest.(check bool)
        "completed body causes fresh allocation" false
        (N.same_identity second_complete third_head);
      let _, foreign = Test_task_declarations.setup () in
      Alcotest.(check bool)
        "foreign ledger cannot read source record" true
        (Result.is_error
           (Task_declarations.function_record_snapshot foreign
              (N.source first_head)));
      List.iter
        (fun event ->
          Alcotest.(check bool)
            "expired events cannot advance record" true
            (Result.is_error (Task_declarations.observe ledger event)))
        events;
      Alcotest.(check (option int))
        "earlier phase remains immutable" (Some 0)
        (N.argument_count second_head)
  | _ ->
      Alcotest.fail "expected member and header snapshots for three functions"

let unconsumed_named_members () =
  let table, namespace, selected, _, snapshot =
    selected_one "class C {};I64 F(C n)#exe {};"
  in
  let scope_count = List.length (S.all_scopes table) in
  Alcotest.(check (option int))
    "native argument count still zero" (Some 0)
    (N.argument_count snapshot);
  Alcotest.(check bool)
    "aggregate value remains unsupported with authentic selection" true
    (Result.is_error
       (D.resolve_provisional_call ~selected_aggregate:selected ~table
          ~namespace
          (N.call_shape snapshot |> checked)));
  Alcotest.(check int)
    "aggregate value fails before scope allocation" scope_count
    (List.length (S.all_scopes table));
  let table, namespace, snapshot =
    one "class C {};I64 F(I64 (*cb)(C n))#exe {};"
  in
  let scope_count = List.length (S.all_scopes table) in
  Alcotest.(check bool)
    "nested callback named type still lacks a selection receipt" true
    (Result.is_error (resolve table namespace snapshot));
  Alcotest.(check int)
    "nested callback rejection allocates no scope" scope_count
    (List.length (S.all_scopes table))

let source_origin (location : Ast.location) =
  Semantic_symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

let semantic_named_return () =
  let table, namespace, snapshot = one "class C {};C F()#exe {};" in
  let native = N.native_source snapshot in
  let symbol =
    S.add table
      ~scope:(C.namespace_scope namespace)
      ~name:"C" ~kind:Semantic_symbol.Aggregate_type
      ~origin:
        (source_origin
           (Ast.type_specifier_location native.function_header.type_specifier))
    |> checked
  in
  let resolved_type =
    Semantic_type.make_aggregate ~symbol ~pointer_depth:0 |> checked
  in
  let return_type =
    Semantic_type_reference.make ~spelling:"C"
      ~spelling_origin:
        (source_origin
           (Ast.type_specifier_location native.function_header.type_specifier))
      ~pointer_origins:[] ~resolved_type
    |> checked
  in
  let scope =
    S.create_scope table
      ~parent:(C.namespace_scope namespace)
      ~kind:S.Function ()
    |> checked
  in
  Alcotest.(check bool)
    "same-name aggregate without selected source evidence rejected" true
    (Result.is_error
       (H.make_provisional_function ~table ~namespace
          ~shape:(N.call_shape snapshot |> checked)
          ~scope ~return_type ~parameters:[] ~variadic_register_requests:[]))

let selected_aggregate_pointers () =
  let aggregate_symbol reference =
    match Semantic_type_reference.resolved_type reference with
    | type_ -> (
        match Semantic_type.base type_ with
        | Semantic_type.Aggregate symbol -> symbol
        | Semantic_type.Primitive _ ->
            Alcotest.fail "expected aggregate parameter")
  in
  List.iter
    (fun (source, expected_count) ->
      let table, namespace, selected, aggregates, snapshot =
        selected_one source
      in
      let function_ =
        D.resolve_provisional_call ~selected_aggregate:selected ~table
          ~namespace
          (N.call_shape snapshot |> checked)
        |> checked
      in
      let parameters =
        H.signature_parameters (H.function_signature function_)
      in
      Alcotest.(check int)
        "native cursor keeps its original argument count" expected_count
        (List.length parameters);
      let source_member =
        List.hd (P.members (N.source_snapshot snapshot))
        |> P.member_completion |> Option.get
      in
      let ast = source_member.Parser.parameter_ast in
      let proof = selected ast.type_specifier |> Option.get in
      let reference =
        SR.selected proof ast.type_specifier ast.pointer_layers |> checked
      in
      let symbol = aggregate_symbol reference in
      List.iter
        (fun parameter ->
          Alcotest.(check bool)
            "active native member uses the selected class" true
            (aggregate_symbol (H.parameter_type_reference parameter) == symbol))
        parameters;
      match aggregates with
      | [ (_, original); (_, shadow) ] ->
          Alcotest.(check bool)
            "provisional parameter keeps pre-lookahead aggregate" true
            (symbol == Option.get (C.publication_aggregate_identity original));
          Alcotest.(check bool)
            "provisional parameter ignores later same-name shadow" true
            (symbol != Option.get (C.publication_aggregate_identity shadow))
      | _ -> Alcotest.fail "expected original and shadow aggregate publications")
    [
      ("class C {};I64 F(C *p)#exe {class C {};};", 0);
      ("class C {};extern I64 F(C *p);#exe {class C {};}", 1);
    ];
  let table, namespace, selected, aggregates, snapshot =
    selected_one "class C {};C *F()#exe {};"
  in
  let function_ =
    D.resolve_provisional_call ~selected_aggregate:selected ~table ~namespace
      (N.call_shape snapshot |> checked)
    |> checked
  in
  let return_type =
    H.function_return_type function_ |> Semantic_type_reference.resolved_type
  in
  match (Semantic_type.base return_type, aggregates) with
  | Semantic_type.Aggregate symbol, [ (_, original) ] ->
      Alcotest.(check bool)
        "provisional return keeps selected aggregate" true
        (symbol == Option.get (C.publication_aggregate_identity original));
      Alcotest.(check int)
        "selected aggregate return remains pointer-shaped" 1
        (Semantic_type.pointer_depth return_type)
  | _ -> Alcotest.fail "expected selected aggregate pointer return"

let foreign_selected_aggregate_consumer_rejected () =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let namespace = C.create_namespace ~table () |> checked in
  let foreign_namespace = C.create_namespace ~table () |> checked in
  let foreign_session = Session.create () in
  let foreign_table = Session.semantic_symbols foreign_session in
  let foreign_table_namespace =
    C.create_namespace ~table:foreign_table () |> checked
  in
  let registry =
    N.create_registry ~mode:Preprocessor.Jit ~table ~namespace |> checked
  in
  let own_aggregate = ref None in
  let foreign_namespace_aggregate = ref None in
  let foreign_table_aggregate = ref None in
  let foreign_namespace_proof = ref None in
  let foreign_table_proof = ref None in
  let records = ref [] and snapshots = ref [] in
  let observe_record event =
    List.iter
      (fun record ->
        if N.event_belongs record event then N.observe record event |> checked)
      !records
  in
  let declaration event =
    (match event with
    | Parser.Aggregate_declared source ->
        own_aggregate := Some (C.publish_aggregate namespace source |> checked);
        foreign_namespace_aggregate :=
          Some (C.publish_aggregate foreign_namespace source |> checked);
        foreign_table_aggregate :=
          Some (C.publish_aggregate foreign_table_namespace source |> checked)
    | Parser.Function_declared source ->
        let publication = C.publish_function namespace source |> checked in
        records :=
          (N.begin_header registry publication source |> checked) :: !records
    | Parser.Function_parameter_declared parameter ->
        let source = SR.Function_parameter parameter in
        ignore
          (SR.select_aggregate ~table ~namespace ~source
             (Option.get !own_aggregate)
          |> checked);
        foreign_namespace_proof :=
          Some
            (SR.select_aggregate ~table ~namespace:foreign_namespace ~source
               (Option.get !foreign_namespace_aggregate)
            |> checked);
        foreign_table_proof :=
          Some
            (SR.select_aggregate ~table:foreign_table
               ~namespace:foreign_table_namespace ~source
               (Option.get !foreign_table_aggregate)
            |> checked);
        observe_record event
    | _ -> observe_record event);
    Ok ()
  in
  let commands = Test_provisional_function_parser.sink declaration in
  let _, _, parsed, _, _, _ =
    Test_stream_parser.parse ~session ~same_task:true ~commands
      ~configure:(fun _ execution -> { execution with Parser.commands })
      ~on_enter:(fun () ->
        match !records with
        | record :: _ -> snapshots := N.snapshot record :: !snapshots
        | [] -> ())
      "class C {};I64 F(C *p)#exe {};"
  in
  ignore (Test_parser.expect_ast parsed);
  let snapshot =
    match List.rev !snapshots with
    | [ snapshot ] -> snapshot
    | _ -> Alcotest.fail "expected one foreign-proof provisional snapshot"
  in
  let shape = N.call_shape snapshot |> checked in
  List.iter
    (fun (label, proof) ->
      let before = List.length (S.all_scopes table) in
      let resolver _ = Some (Option.get proof) in
      Alcotest.(check bool)
        (label ^ " rejected by owning function namespace")
        true
        (Result.is_error
           (D.resolve_provisional_call ~selected_aggregate:resolver ~table
              ~namespace shape));
      Alcotest.(check int)
        (label ^ " rejects before function scope allocation")
        before
        (List.length (S.all_scopes table)))
    [
      ("same-AST foreign namespace proof", !foreign_namespace_proof);
      ("same-AST foreign table proof", !foreign_table_proof);
    ]

let substituted_default_flag () =
  let table, namespace, snapshot = one "extern I64 F(U8 *s=\"x\");#exe {}" in
  let function_ = resolve table namespace snapshot |> checked in
  let original =
    List.hd (H.signature_parameters (H.function_signature function_))
  in
  let default =
    match H.parameter_default original with
    | Some (H.Expression_default value) ->
        Some
          (H.Expression_default { value with contains_string_literal = false })
    | _ -> Alcotest.fail "expected string default"
  in
  let replacement =
    H.make_parameter
      ~source:(Option.get (H.parameter_source original))
      ~index:0
      ~origin:(H.parameter_origin original)
      ~register_requests:(H.parameter_register_requests original)
      ?name:(H.parameter_name original)
      ?name_origin:(H.parameter_name_origin original)
      ~type_reference:(H.parameter_type_reference original)
      ~declarator_kind:(H.parameter_declarator_kind original)
      ~default
      ?delimiter_origin:(H.parameter_delimiter_origin original)
      ()
  in
  let result =
    Result.bind replacement (fun parameter ->
        H.make_provisional_function ~table ~namespace
          ~shape:(Option.get (H.function_provisional_call function_))
          ~scope:(H.function_scope function_)
          ~return_type:(H.function_return_type function_)
          ~parameters:[ parameter ] ~variadic_register_requests:[])
  in
  Alcotest.(check bool)
    "original child cannot authorize substituted default flags" true
    (Result.is_error result)

let primitive_spelling_cannot_hide_aggregate () =
  let table, namespace, snapshot = one "extern I64 F(I64 n);#exe {}" in
  let function_ = resolve table namespace snapshot |> checked in
  let original =
    List.hd (H.signature_parameters (H.function_signature function_))
  in
  let symbol =
    S.add table
      ~scope:(C.namespace_scope namespace)
      ~name:"I64" ~kind:Semantic_symbol.Aggregate_type
      ~origin:(H.parameter_origin original)
    |> checked
  in
  let resolved_type =
    Semantic_type.make_aggregate ~symbol ~pointer_depth:0 |> checked
  in
  let substitute reference =
    Semantic_type_reference.make ~spelling:"I64"
      ~spelling_origin:(Semantic_type_reference.spelling_origin reference)
      ~pointer_origins:[] ~resolved_type
    |> checked
  in
  let make return_type parameters =
    H.make_provisional_function ~table ~namespace
      ~shape:(Option.get (H.function_provisional_call function_))
      ~scope:(H.function_scope function_)
      ~return_type ~parameters ~variadic_register_requests:[]
  in
  Alcotest.(check bool)
    "primitive return cannot use same-spelling aggregate" true
    (Result.is_error
       (make (substitute (H.function_return_type function_)) [ original ]));
  let replacement =
    H.make_parameter
      ~source:(Option.get (H.parameter_source original))
      ~index:0
      ~origin:(H.parameter_origin original)
      ~register_requests:(H.parameter_register_requests original)
      ?name:(H.parameter_name original)
      ?name_origin:(H.parameter_name_origin original)
      ~type_reference:(substitute (H.parameter_type_reference original))
      ~declarator_kind:(H.parameter_declarator_kind original)
      ~default:(H.parameter_default original)
      ?delimiter_origin:(H.parameter_delimiter_origin original)
      ()
    |> checked
  in
  Alcotest.(check bool)
    "primitive parameter cannot use same-spelling aggregate" true
    (Result.is_error (make (H.function_return_type function_) [ replacement ]))

let tests =
  [
    Alcotest.test_case "fresh and reused native projection" `Quick
      zero_projection;
    Alcotest.test_case "native fixed members keep original children" `Quick
      completed_native_members;
    Alcotest.test_case "variadic cursor has no fabricated locals" `Quick
      variadic_cursor;
    Alcotest.test_case "direct arguments follow the published variadic cursor"
      `Quick variadic_argument_cursor;
    Alcotest.test_case "provisional variadic binding preserves fixed defaults"
      `Quick variadic_fixed_default;
    Alcotest.test_case "call evidence cannot authorize ordinary functions"
      `Quick ordinary_authority_rejected;
    Alcotest.test_case "foreign ownership fails before allocation" `Quick
      foreign_namespace;
    Alcotest.test_case "task ledger retains native record phases" `Quick
      ledger_native_phases;
    Alcotest.test_case "unconsumed members still need type evidence" `Quick
      unconsumed_named_members;
    Alcotest.test_case "direct constructor rejects unselected aggregate" `Quick
      semantic_named_return;
    Alcotest.test_case "selected aggregate pointer phases" `Quick
      selected_aggregate_pointers;
    Alcotest.test_case "foreign selected aggregate consumer ownership" `Quick
      foreign_selected_aggregate_consumer_rejected;
    Alcotest.test_case "default flags cannot substitute original source" `Quick
      substituted_default_flag;
    Alcotest.test_case "primitive types cannot hide aggregate substitution"
      `Quick primitive_spelling_cannot_hide_aggregate;
  ]
