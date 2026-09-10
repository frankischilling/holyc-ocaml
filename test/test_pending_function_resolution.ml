open Holyc_lib
module C = Semantic_declaration_collection
module F = Semantic_function_resolution
module H = Semantic_function_type_resolution
module K = Semantic_function_record_classification

let checked = Test_declaration_collection.checked
let initial = Compiler_option.initial_mask
let single resolution = List.hd (F.declarations resolution)
let site declaration = F.resolved_declaration_site declaration

let rejected label result =
  Alcotest.(check bool) label true (Result.is_error result)

let fixture text =
  let session, namespace, headers = Test_completed_function_header.parse text in
  let _, _, source, function_ = List.hd headers in
  let table = Session.semantic_symbols session in
  let fact =
    F.make_pending_declaration ~table ~namespace ~compiler_option_mask:initial
      ~source ~function_
    |> checked
  in
  let resolution =
    F.resolve ~table
      ~parent:(C.namespace_scope namespace)
      ~compilation_mode:F.Jit [ fact ]
    |> checked
  in
  (table, namespace, source, function_, resolution)

let classify ?(previous = []) resolution =
  K.classify ~previous resolution
    [
      K.make_declaration_state ~staging_mask:0L ~compiler_option_mask:initial ();
    ]
  |> checked

let pending_definition () =
  let table, namespace, source, function_, resolution =
    fixture "I64 F(I64 n=42){return n;}"
  in
  let pending = single resolution in
  Alcotest.(check bool)
    "definition source kind retained" true
    (F.declaration_site_source_kind (site pending) = F.Definition);
  Alcotest.(check bool)
    "pending state" true
    (F.declaration_site_state (site pending) = F.Unresolved_extern);
  Alcotest.(check bool)
    "exact source proof" true
    (Option.get (F.declaration_site_pending_source (site pending)) == source);
  let prior = classify resolution |> K.declarations |> List.hd in
  let record = K.classified_declaration_record prior in
  Alcotest.(check bool)
    "pending extern access" true
    (K.is_extern record && K.call_access record = K.Jit_extern_address_slot_call);
  let earlier_completion_fact =
    F.make_completion_declaration ~table ~namespace ~pending ~function_
    |> checked
  in
  let completed =
    F.complete_pending ~table ~namespace ~pending ~function_ |> checked
  in
  let final = single completed in
  Alcotest.(check bool)
    "exact retained typed function" true
    (F.declaration_site_function (site final) == function_);
  Alcotest.(check bool)
    "completion joins exact pending declaration" true
    (F.is_joined_successor ~earlier:pending ~later:final);
  Alcotest.(check bool)
    "completion keeps original identity" true
    (F.resolved_declaration_identity_symbol final
    == F.resolved_declaration_identity_symbol pending);
  Alcotest.(check bool)
    "completed state" true
    (F.declaration_site_state (site final) = F.Resolved);
  let final_record =
    classify ~previous:[ prior ] completed
    |> K.declarations |> List.hd |> K.classified_declaration_record
  in
  Alcotest.(check bool)
    "completion enables executable access" true
    ((not (K.is_extern final_record))
    && K.call_access final_record = K.Direct_executable_call);
  rejected "repeat completion"
    (F.complete_pending ~table ~namespace ~pending ~function_);
  rejected "previously made completion fact cannot replay completion"
    (F.resolve ~previous:[ pending ] ~table
       ~parent:(C.namespace_scope namespace)
       ~compilation_mode:F.Jit
       [ earlier_completion_fact ]);
  rejected "completed declaration cannot be completed again"
    (F.complete_pending ~table ~namespace ~pending:final ~function_)

let reject_replay () =
  let table, namespace, source, function_, resolution =
    fixture "I64 F(I64 n){return n;}"
  in
  let pending = single resolution in
  let parent = C.namespace_scope namespace in
  let regular = F.make_declaration ~function_ ~kind:F.Definition |> checked in
  rejected "ordinary resolve cannot replay source"
    (F.resolve ~previous:[ pending ] ~table ~parent ~compilation_mode:F.Jit
       [ regular ]);
  let foreign = C.create_namespace ~table () |> checked in
  rejected "pending source requires original namespace"
    (F.make_pending_declaration ~table ~namespace:foreign
       ~compiler_option_mask:initial ~source ~function_);
  rejected "completion requires original namespace"
    (F.complete_pending ~table ~namespace:foreign ~pending ~function_);
  let _, _, others =
    Test_completed_function_header.parse "I64 F(I64 n){return n;}"
  in
  let _, _, _, copied = List.hd others in
  rejected "pending source rejects copied typed header"
    (F.make_pending_declaration ~table ~namespace ~compiler_option_mask:initial
       ~source ~function_:copied);
  rejected "completion rejects substituted typed header"
    (F.complete_pending ~table ~namespace ~pending ~function_:copied);
  let rebuilt =
    H.make_function
      ~symbol:(H.function_symbol function_)
      ~scope:(H.function_scope function_)
      ~item_index:(H.function_item_index function_)
      ~return_type:(H.function_return_type function_)
      ~signature:(H.function_signature function_)
      ~parameter_bindings:(H.function_parameter_bindings function_)
      ~variadic_bindings:(H.function_variadic_bindings function_)
    |> checked
    |> fun fact ->
    H.resolve ~table ~parent [ fact ] |> checked |> H.functions |> List.hd
  in
  rejected "completion rejects same-symbol reconstructed typed object"
    (F.complete_pending ~table ~namespace ~pending ~function_:rebuilt);
  let reconstruction =
    F.make_pending_declaration ~table ~namespace ~compiler_option_mask:initial
      ~source ~function_
    |> checked
  in
  let reconstructed =
    F.resolve ~table ~parent ~compilation_mode:F.Jit [ reconstruction ]
    |> checked |> single
  in
  let completion =
    F.make_completion_declaration ~table ~namespace ~pending ~function_
    |> checked
  in
  rejected "completion rejects reconstructed predecessor"
    (F.resolve ~previous:[ reconstructed ] ~table ~parent
       ~compilation_mode:F.Jit [ completion ]);
  let completed =
    F.resolve ~previous:[ pending ] ~table ~parent ~compilation_mode:F.Jit
      [ completion ]
    |> checked
  in
  let wrong_prior =
    classify
      (F.resolve ~table ~parent ~compilation_mode:F.Jit [ reconstruction ]
      |> checked)
    |> K.declarations |> List.hd
  in
  rejected "classification rejects substituted predecessor"
    (K.classify ~previous:[ wrong_prior ] completed
       [
         K.make_declaration_state ~staging_mask:0L ~compiler_option_mask:initial
           ();
       ])

let extern_completion () =
  let table, namespace, _, function_, resolution =
    fixture "extern I64 F(I64 n);"
  in
  let pending = single resolution in
  let completed =
    F.complete_pending ~table ~namespace ~pending ~function_ |> checked
  in
  let final = single completed in
  Alcotest.(check bool)
    "extern completion retains binding" true
    (F.declaration_site_kind (site final) = F.Extern
    && F.declaration_site_state (site final) = F.Unresolved_extern);
  Alcotest.(check bool)
    "extern completion is no longer pending" false
    (F.declaration_site_is_pending (site final))

let bound_completion () =
  List.iter
    (fun (text, expected) ->
      let table, namespace, _, function_, resolution = fixture text in
      let pending = single resolution in
      Alcotest.(check bool)
        "pending binding kind retained" true
        (F.declaration_site_source_kind (site pending) = expected
        && F.declaration_site_state (site pending) = F.Unresolved_extern);
      let completed =
        F.complete_pending ~table ~namespace ~pending ~function_
        |> checked |> single
      in
      Alcotest.(check bool)
        "original binding takes effect on completion" true
        (F.declaration_site_kind (site completed) = expected
        && F.declaration_site_state (site completed) = F.Resolved))
    [
      ("_extern _REMOTE I64 F(I64 n);", F.Bound_extern);
      ("_intern 42 I64 F(I64 n);", F.Intern);
    ]

let copied_parameter_source () =
  let table, namespace, source, function_, _ =
    fixture "I64 F(I64 n=42){return n;}"
  in
  let signature = H.function_signature function_ in
  let parameter = List.hd (H.signature_parameters signature) in
  let original = Option.get (H.parameter_source parameter) in
  let copied =
    Ast.make_function_parameter
      ~register_qualifiers:original.register_qualifiers
      ~type_specifier:original.type_specifier
      ~pointer_layers:original.pointer_layers ~name:original.name
      ~function_pointer:original.function_pointer ~default:original.default
      ~delimiter:original.delimiter ~location:original.location
  in
  let replacement =
    H.make_parameter ~source:copied
      ~index:(H.parameter_index parameter)
      ~origin:(H.parameter_origin parameter)
      ~register_requests:(H.parameter_register_requests parameter)
      ?name:(H.parameter_name parameter)
      ?name_origin:(H.parameter_name_origin parameter)
      ~type_reference:(H.parameter_type_reference parameter)
      ~declarator_kind:(H.parameter_declarator_kind parameter)
      ~default:(H.parameter_default parameter)
      ?delimiter_origin:(H.parameter_delimiter_origin parameter)
      ()
    |> checked
  in
  let signature =
    H.make_signature
      ~opening_origin:(H.signature_opening_origin signature)
      ~parameters:[ replacement ]
      ~closing_origin:(H.signature_closing_origin signature)
      ()
    |> checked
  in
  let replacement =
    H.make_function_with_completed_header
      (Semantic_compiler_record.declared_function_source source)
      ~symbol:(H.function_symbol function_)
      ~scope:(H.function_scope function_)
      ~item_index:(H.function_item_index function_)
      ~return_type:(H.function_return_type function_)
      ~signature
      ~parameter_bindings:(H.function_parameter_bindings function_)
      ~variadic_bindings:None
    |> checked
  in
  let replacement =
    H.resolve ~table ~parent:(C.namespace_scope namespace) [ replacement ]
    |> checked |> H.functions |> List.hd
  in
  rejected "structurally copied parameter source grants no header identity"
    (F.make_pending_declaration ~table ~namespace ~compiler_option_mask:initial
       ~source ~function_:replacement)

let converted_import () =
  let table, namespace, source, function_, _ = fixture "extern I64 F();" in
  let mask, _ =
    Compiler_option.set ~mask:initial Compiler_option.Externs_to_imports true
  in
  let fact =
    F.make_pending_declaration ~table ~namespace ~compiler_option_mask:mask
      ~source ~function_
    |> checked
  in
  let parent = C.namespace_scope namespace in
  rejected "pending effective import remains invalid in JIT"
    (F.resolve ~table ~parent ~compilation_mode:F.Jit [ fact ]);
  let resolution =
    F.resolve ~table ~parent ~compilation_mode:F.Aot [ fact ] |> checked
  in
  let pending = single resolution in
  Alcotest.(check bool)
    "pending import preserves source and effective kinds" true
    (F.declaration_site_source_kind (site pending) = F.Extern
    && F.declaration_site_kind (site pending) = F.Import
    && F.declaration_site_state (site pending) = F.Unresolved_extern);
  let state =
    K.make_declaration_state ~staging_mask:0L ~compiler_option_mask:mask
      ~import_name:"F" ()
  in
  let prior =
    K.classify resolution [ state ] |> checked |> K.declarations |> List.hd
  in
  Alcotest.(check bool)
    "pending import has not applied binding mutation" true
    (K.call_access (K.classified_declaration_record prior) = K.Aot_extern_call);
  let completed =
    F.complete_pending ~table ~namespace ~pending ~function_ |> checked
  in
  Alcotest.(check bool)
    "AOT completion applies import state" true
    (F.declaration_site_state (site (single completed)) = F.Imported);
  let record =
    K.classify ~previous:[ prior ] completed [ state ]
    |> checked |> K.declarations |> List.hd |> K.classified_declaration_record
  in
  Alcotest.(check bool)
    "AOT completion applies original loader name" true
    (K.call_access record = K.Aot_import_call && K.import_name record = Some "F");
  rejected "pending option snapshot cannot be changed during classification"
    (K.classify resolution
       [
         K.make_declaration_state ~staging_mask:0L ~compiler_option_mask:initial
           ~import_name:"F" ();
       ]);
  rejected "pending source rejects unknown option bits"
    (F.make_pending_declaration ~table ~namespace
       ~compiler_option_mask:Int64.min_int ~source ~function_)

let nested_completion_case hidden =
  let session, namespace, headers =
    Test_completed_function_header.parse
      "I64 F(I64 n=40){return n+2;}public I64 F(I64 n=99){return 7;}I64 F(I64 \
       n=77){return n;}"
  in
  let table = Session.semantic_symbols session in
  let parent = C.namespace_scope namespace in
  let pending ?(previous = []) index =
    let _, _, source, function_ = List.nth headers index in
    let fact =
      F.make_pending_declaration ~table ~namespace ~compiler_option_mask:initial
        ~source ~function_
      |> checked
    in
    ( source,
      function_,
      F.resolve ~previous ~table ~parent ~compilation_mode:F.Jit [ fact ]
      |> checked )
  in
  let classify_source previous resolution source =
    Holyc_lib__Driver__Function_record_classification.classify_completed_header
      ~previous ~resolution source
    |> checked |> K.declarations |> List.hd
  in
  let outer_source, outer, outer_resolution = pending 0 in
  let outer_pending = single outer_resolution in
  let outer_record = classify_source [] outer_resolution outer_source in
  let inner_source, inner, inner_resolution =
    pending ~previous:[ outer_pending ] 1
  in
  let inner_pending = single inner_resolution in
  let inner_record =
    classify_source [ outer_record ] inner_resolution inner_source
  in
  let inner_completed_resolution =
    F.complete_pending ~table ~namespace ~pending:inner_pending ~function_:inner
    |> checked
  in
  let inner_completed = single inner_completed_resolution in
  let inner_completed_record =
    classify_source [ inner_record ] inner_completed_resolution inner_source
  in
  let _, _, copy_resolution = pending 0 in
  rejected "a reconstructed pending node cannot act as the current record"
    (F.make_completion_declaration_against ~table ~namespace
       ~pending:outer_pending ~current:(single copy_resolution) ~function_:outer);
  rejected "current header cannot substitute for original body source"
    (F.make_completion_declaration_against ~table ~namespace
       ~pending:outer_pending ~current:inner_completed ~function_:inner);
  let previous =
    if hidden then (
      let _, shadow, shadow_resolution =
        pending ~previous:[ inner_completed ] 2
      in
      let shadow_pending = single shadow_resolution in
      let shadow_completed =
        F.complete_pending ~table ~namespace ~pending:shadow_pending
          ~function_:shadow
        |> checked |> single
      in
      rejected "same-name shadow is not a successor in the original record"
        (F.make_completion_declaration_against ~table ~namespace
           ~pending:outer_pending ~current:shadow_completed ~function_:outer);
      [ shadow_completed ])
    else [ inner_completed ]
  in
  let completion =
    F.make_completion_declaration_against ~table ~namespace
      ~pending:outer_pending ~current:inner_completed ~function_:outer
    |> checked
  in
  rejected "a different current record cannot authorize completion"
    (F.resolve
       ~previous:[ single copy_resolution ]
       ~table ~parent ~compilation_mode:F.Jit [ completion ]);
  rejected "record head pool cannot contain repeated identities"
    (F.resolve ~previous
       ~record_heads:[ inner_completed; inner_completed ]
       ~table ~parent ~compilation_mode:F.Jit [ completion ]);
  let completed_resolution =
    F.resolve ~previous ~record_heads:[ inner_completed ] ~table ~parent
      ~compilation_mode:F.Jit [ completion ]
    |> checked
  in
  let completed = single completed_resolution in
  Alcotest.(check bool)
    "outer completion retains exact original body source" true
    (F.declaration_site_function (site completed) == outer);
  Alcotest.(check bool)
    "outer completion follows the nested executable" true
    (F.is_joined_successor ~earlier:inner_completed ~later:completed);
  Alcotest.(check bool)
    "completion preserves the current inner header and defaults" true
    (F.resolved_declaration_header completed == inner);
  Alcotest.(check bool)
    "completion records its exact original pending authority" true
    (Option.get (F.resolved_declaration_completion_source completed)
    == outer_pending);
  Alcotest.(check bool)
    "original source remains discoverable through exact ancestry" true
    (Option.get (F.find_pending_source ~current:completed ~function_:outer)
    == outer_pending);
  Alcotest.(check bool)
    "body publication does not replace a header" true
    (Option.is_none (F.resolved_declaration_replaced_header completed));
  let completed_record =
    classify_source [ inner_completed_record ] completed_resolution outer_source
  in
  Alcotest.(check bool)
    "outer publication preserves inner public header flag" true
    (K.is_public (K.classified_declaration_record completed_record));
  Alcotest.(check bool)
    "body publication is resolved" true
    (K.call_access (K.classified_declaration_record completed_record)
    = K.Direct_executable_call);
  rejected "original source cannot complete a second time against its new head"
    (F.make_completion_declaration_against ~table ~namespace
       ~pending:outer_pending ~current:completed ~function_:outer);
  rejected "an already-made body publication cannot be replayed"
    (F.resolve ~previous ~record_heads:[ inner_completed ] ~table ~parent
       ~compilation_mode:F.Jit [ completion ]);
  if hidden then (
    let shadow = List.hd previous in
    Alcotest.(check bool)
      "hidden completion retains original identity" true
      (F.resolved_declaration_identity_symbol completed
      == F.resolved_declaration_identity_symbol outer_pending);
    Alcotest.(check bool)
      "visible shadow remains another record" true
      (F.resolved_declaration_identity_symbol completed
      != F.resolved_declaration_identity_symbol shadow);
    Alcotest.(check bool)
      "hidden completion does not join the visible shadow" false
      (F.is_joined_successor ~earlier:shadow ~later:completed))

let nested_completion () = nested_completion_case false
let hidden_completion () = nested_completion_case true

let tests =
  [
    Alcotest.test_case "pending definition completes exact retained header"
      `Quick pending_definition;
    Alcotest.test_case "pending completion rejects replay and substitutions"
      `Quick reject_replay;
    Alcotest.test_case "extern header completion retains extern binding" `Quick
      extern_completion;
    Alcotest.test_case "bound headers retain original source binding" `Quick
      bound_completion;
    Alcotest.test_case "pending source rejects copied parameter nodes" `Quick
      copied_parameter_source;
    Alcotest.test_case "pending AOT import completes with source options" `Quick
      converted_import;
    Alcotest.test_case "outer completion follows nested header and body" `Quick
      nested_completion;
    Alcotest.test_case
      "outer completion updates hidden record under resolved shadow" `Quick
      hidden_completion;
  ]
