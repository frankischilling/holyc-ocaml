open Holyc_lib
module R = Semantic_function_resolution
module T = Semantic_function_type_resolution
module C = Semantic_declaration_collection

let checked = Test_function_resolution.checked
let prepare = Test_function_resolution.prepare

let table prepared =
  Session.semantic_symbols prepared.Test_function_resolution.session

let prototypes prepared =
  prepared.Test_function_resolution.ast.items
  |> List.filter_map (function
    | Ast.Function_prototype prototype -> Some prototype
    | _ -> None)

let typed_functions prepared =
  T.functions prepared.Test_function_resolution.function_types

let make_source prepared prototype function_ kind =
  R.make_source_declaration_with_options ~table:(table prepared)
    ~declarations:prepared.Test_function_resolution.declarations
    ~module_:prepared.Test_function_resolution.ast ~prototype
    ~compiler_option_mask:Compiler_option.initial_mask ~function_ ~kind ()

let resolve prepared declarations =
  R.resolve ~table:(table prepared)
    ~parent:(C.scope prepared.Test_function_resolution.declarations)
    ~compilation_mode:R.Jit declarations
  |> checked

let source_binding declaration =
  declaration |> R.resolved_declaration_site
  |> R.declaration_site_source_binding

let expect_source_binding label expected declaration =
  match source_binding declaration with
  | Some actual -> Alcotest.(check bool) label true (actual == expected)
  | None -> Alcotest.fail (label ^ ": source binding was not retained")

let expect_no_source_binding label declaration =
  Alcotest.(check bool) label true (Option.is_none (source_binding declaration))

let expect_rejected label = function
  | Error _ -> ()
  | Ok _ -> Alcotest.fail (label ^ ": substituted source was accepted")

let original_same_signature_prototypes () =
  let prepared =
    prepare ~path:"internal-strlen-source-original.hc"
      "_intern 0x84 I64 A(U8 *s);extern I64 B(U8 *s);"
  in
  match (prototypes prepared, typed_functions prepared) with
  | [ a; b ], [ a_typed; b_typed ] ->
      let a_fact = make_source prepared a a_typed R.Intern |> checked in
      let b_fact = make_source prepared b b_typed R.Extern |> checked in
      let declarations =
        resolve prepared [ a_fact; b_fact ] |> R.declarations
      in
      expect_source_binding "original internal binding" a.binding
        (List.nth declarations 0);
      expect_source_binding "original extern binding" b.binding
        (List.nth declarations 1)
  | _ -> Alcotest.fail "source fixture did not produce exactly two prototypes"

let cross_function_source_substitution_rejected () =
  let prepared =
    prepare ~path:"internal-strlen-source-cross-function.hc"
      "_intern 0x84 I64 A(U8 *s);_intern 0x84 I64 B(U8 *s);"
  in
  match (prototypes prepared, typed_functions prepared) with
  | [ a; _b ], [ _a_typed; b_typed ] ->
      expect_rejected "A source prototype cannot authorize B's typed function"
        (make_source prepared a b_typed R.Intern)
  | _ -> Alcotest.fail "cross-function fixture did not produce two prototypes"

let foreign_same_text_source_rejected () =
  let contents = "_intern 0x84 I64 A(U8 *s);" in
  let original =
    prepare ~path:"internal-strlen-source-original-a.hc" contents
  in
  let foreign = prepare ~path:"internal-strlen-source-foreign-a.hc" contents in
  match (prototypes foreign, typed_functions original) with
  | [ foreign_prototype ], [ original_typed ] ->
      expect_rejected
        "same-text foreign prototype cannot authorize original type"
        (make_source original foreign_prototype original_typed R.Intern)
  | _ -> Alcotest.fail "foreign-source fixture did not produce one prototype"

let copied_prototype_rejected () =
  let prepared =
    prepare ~path:"internal-strlen-source-copy.hc" "_intern 0x84 I64 A(U8 *s);"
  in
  match (prototypes prepared, typed_functions prepared) with
  | [ original ], [ typed ] ->
      let copied =
        Ast.make_function_prototype ~modifiers:original.modifiers
          ~binding:original.binding ~return_type:original.return_type
          ~return_pointer_layers:original.return_pointer_layers
          ~name:original.name ~opening_parenthesis:original.opening_parenthesis
          ~parameters:original.parameters
          ~empty_parameter_entries:original.empty_parameter_entries
          ~variadic:original.variadic
          ~closing_parenthesis:original.closing_parenthesis
          ~semicolon:original.semicolon ~location:original.location
      in
      Alcotest.(check bool)
        "record copy is physically distinct from the module prototype" false
        (copied == original);
      expect_rejected "copied prototype cannot replace the original module item"
        (make_source prepared copied typed R.Intern)
  | _ -> Alcotest.fail "copied-source fixture did not produce one prototype"

let reconstructed_binding_target_rejected () =
  let original =
    prepare ~path:"internal-strlen-source-original-target.hc"
      "_intern 0x85 I64 A(U8 *s);"
  in
  let donor =
    prepare ~path:"internal-strlen-source-donor-target.hc"
      "_intern 0x84 I64 A(U8 *s);"
  in
  match (prototypes original, prototypes donor, typed_functions original) with
  | [ source ], [ donor_source ], [ typed ] ->
      let original_fact =
        make_source original source typed R.Intern |> checked
      in
      let original_declaration =
        resolve original [ original_fact ] |> R.declarations |> List.hd
      in
      expect_source_binding "original 0x85 binding remains authorized"
        source.binding original_declaration;
      let replacement_target =
        match donor_source.binding.target with
        | Ast.Expression_binding_target target -> target
        | Ast.No_binding_target | Ast.Symbol_binding_target _ ->
            Alcotest.fail "donor internal binding lost its expression target"
      in
      let binding =
        Ast.make_declaration_binding ~kind:source.binding.kind
          ~spelling:source.binding.spelling ~location:source.binding.location
          ~target:(Ast.Expression_binding_target replacement_target)
      in
      let reconstructed =
        Ast.make_function_prototype ~modifiers:source.modifiers ~binding
          ~return_type:source.return_type
          ~return_pointer_layers:source.return_pointer_layers ~name:source.name
          ~opening_parenthesis:source.opening_parenthesis
          ~parameters:source.parameters
          ~empty_parameter_entries:source.empty_parameter_entries
          ~variadic:source.variadic
          ~closing_parenthesis:source.closing_parenthesis
          ~semicolon:source.semicolon ~location:source.location
      in
      let reconstructed_module =
        Ast.make_module ~source:original.Test_function_resolution.ast.source
          ~span:original.Test_function_resolution.ast.span
          ~items:[ Ast.Function_prototype reconstructed ]
      in
      Alcotest.(check bool)
        "reconstructed prototype is physically distinct from original" false
        (reconstructed == source);
      expect_rejected
        "reconstructed module cannot replace the original internal target"
        (R.make_source_declaration_with_options ~table:(table original)
           ~declarations:original.Test_function_resolution.declarations
           ~module_:reconstructed_module ~prototype:reconstructed
           ~compiler_option_mask:Compiler_option.initial_mask ~function_:typed
           ~kind:R.Intern ())
  | _ -> Alcotest.fail "target-reconstruction fixture has an unexpected shape"

let collection_source_authority_cannot_be_backfilled () =
  let prepared =
    prepare ~path:"internal-strlen-source-collection.hc"
      "_intern 0x84 I64 A(U8 *s);"
  in
  match prototypes prepared with
  | [ prototype ] ->
      let original_entry =
        C.entries prepared.Test_function_resolution.declarations |> List.hd
      in
      Alcotest.(check bool)
        "batch collection retains its exact prototype source" true
        (C.entry_matches_function_source original_entry prototype);
      let original_symbol = C.entry_symbol original_entry in
      let legacy_fact =
        C.make_declaration ~name:prototype.name.spelling
          ~declaration_kind:C.Function_prototype
          ~origin:(Semantic_symbol.origin original_symbol)
          ~item_index:0 ()
        |> checked
      in
      let legacy_collection =
        C.collect ~table:(table prepared) ~module_name:"legacy-source.HC"
          [ legacy_fact ]
        |> checked
      in
      Alcotest.(check bool)
        "generic collected fact has no prototype source authority" false
        (C.entry_matches_function_source
           (C.entries legacy_collection |> List.hd)
           prototype);
      let namespace =
        C.create_namespace ~table:(table prepared) () |> checked
      in
      let generic_publication =
        C.publish namespace ~name:prototype.name.spelling
          ~kind:Semantic_symbol.Function
          ~origin:(Semantic_symbol.origin original_symbol)
        |> checked
      in
      let source_fact =
        C.make_function_prototype_declaration ~prototype ~item_index:0
        |> checked
      in
      let view =
        C.view namespace [ (generic_publication, source_fact) ] |> checked
      in
      Alcotest.(check bool)
        "generic publication view cannot backfill source from its declaration \
         fact"
        false
        (C.entry_matches_function_source (C.entries view |> List.hd) prototype)
  | _ -> Alcotest.fail "collection-source fixture did not produce one prototype"

let legacy_intern_has_no_source_authority () =
  let prepared =
    prepare ~path:"internal-strlen-source-legacy.hc"
      "_intern 0x84 I64 A(U8 *s);"
  in
  match typed_functions prepared with
  | [ typed ] ->
      let fact =
        R.make_declaration_with_options
          ~compiler_option_mask:Compiler_option.initial_mask ~function_:typed
          ~kind:R.Intern
        |> checked
      in
      let declaration =
        resolve prepared [ fact ] |> R.declarations |> List.hd
      in
      expect_no_source_binding
        "legacy semantic Intern kind does not manufacture target authority"
        declaration
  | _ -> Alcotest.fail "legacy fixture did not produce one typed function"

let later_definition_does_not_inherit_intern_binding () =
  let prepared =
    prepare ~path:"internal-strlen-source-shadow.hc"
      "_intern 0x84 I64 Same(U8 *s);I64 Same(U8 *s){return 7;}"
  in
  let resolution =
    resolve_function_identities prepared.Test_function_resolution.session
      ~declarations:prepared.declarations ~functions:prepared.function_types
      ~compilation_mode:Preprocessor.Jit prepared.ast
    |> checked
  in
  match R.declarations resolution with
  | [ internal; definition ] ->
      let original =
        match prototypes prepared with
        | [ prototype ] -> prototype.binding
        | _ -> Alcotest.fail "shadow fixture lost its internal prototype"
      in
      expect_source_binding "original internal site keeps its own binding"
        original internal;
      expect_no_source_binding
        "later ordinary same-name definition does not inherit intern target"
        definition
  | _ -> Alcotest.fail "shadow fixture did not resolve two declaration sites"

let tests =
  [
    Alcotest.test_case "original same-signature prototypes are authorized"
      `Quick original_same_signature_prototypes;
    Alcotest.test_case "cross-function source substitution is rejected" `Quick
      cross_function_source_substitution_rejected;
    Alcotest.test_case "foreign same-text prototype is rejected" `Quick
      foreign_same_text_source_rejected;
    Alcotest.test_case "copied prototype is rejected" `Quick
      copied_prototype_rejected;
    Alcotest.test_case "reconstructed binding target is rejected" `Quick
      reconstructed_binding_target_rejected;
    Alcotest.test_case "collection source authority cannot be backfilled" `Quick
      collection_source_authority_cannot_be_backfilled;
    Alcotest.test_case "legacy Intern kind has no source target" `Quick
      legacy_intern_has_no_source_authority;
    Alcotest.test_case "later definition does not inherit intern target" `Quick
      later_definition_does_not_inherit_intern_binding;
  ]
