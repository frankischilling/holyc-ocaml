open Holyc_lib

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let expect_ast = function
  | Ok ast -> ast
  | Error diagnostics ->
      Alcotest.failf "expected an AST, got %s"
        (diagnostics
        |> List.map (fun diagnostic ->
            Printf.sprintf "%s: %s" diagnostic.Diagnostic.code
              diagnostic.message)
        |> String.concat ", ")

let config mode = checked (Preprocessor.Config.create ~compilation_mode:mode ())

type prepared = {
  session : Session.t;
  ast : Ast.module_;
  declarations : Semantic_declaration_collection.t;
  aggregates : Semantic_aggregate_resolution.t;
  collected_functions : Semantic_function_collection.t;
  function_types : Semantic_function_type_resolution.t;
  local_types : Semantic_local_type_resolution.t;
  bindings : Semantic_function_binding_index.t;
  expressions : Semantic_function_expression_binding.t;
  global_types : Semantic_global_type_resolution.t;
  functions : Semantic_function_resolution.t;
  globals : Semantic_global_resolution.t;
}

let prepare ?(mode = Preprocessor.Jit) ~path contents =
  let session = Session.create () in
  let source = Session.add_source session ~path ~contents in
  let ast =
    Holyc_lib.parse_with_config session ~config:(config mode) ~source
    |> expect_ast
  in
  let declarations = checked (Holyc_lib.collect_declarations session ast) in
  let aggregates =
    checked (Holyc_lib.resolve_aggregates session ~declarations ast)
  in
  let collected_functions =
    checked (Holyc_lib.collect_functions session ~declarations ast)
  in
  let function_types =
    checked
      (Holyc_lib.resolve_function_types session ~declarations ~aggregates
         ~functions:collected_functions ast)
  in
  let local_types =
    checked
      (Holyc_lib.resolve_local_types session ~declarations ~aggregates
         ~functions:collected_functions ast)
  in
  let bindings =
    checked
      (Holyc_lib.index_function_bindings session ~declarations
         ~functions:collected_functions ~function_types ~local_types)
  in
  let expressions =
    checked
      (Holyc_lib.resolve_function_expressions session ~declarations
         ~functions:collected_functions ~local_types ~bindings ast)
  in
  let global_types =
    checked
      (Holyc_lib.resolve_global_types session ~declarations ~aggregates ast)
  in
  let functions =
    checked
      (Holyc_lib.resolve_function_identities session ~declarations
         ~functions:function_types ~compilation_mode:mode ast)
  in
  let globals =
    checked
      (Holyc_lib.resolve_global_records session ~declarations
         ~globals:global_types ~compilation_mode:mode ast)
  in
  {
    session;
    ast;
    declarations;
    aggregates;
    collected_functions;
    function_types;
    local_types;
    bindings;
    expressions;
    global_types;
    functions;
    globals;
  }

let resolve prepared =
  Holyc_lib.resolve_module_expressions prepared.session
    ~declarations:prepared.declarations ~aggregates:prepared.aggregates
    ~functions:prepared.functions ~globals:prepared.globals
    ~expressions:prepared.expressions

let function_named result name =
  Semantic_module_expression_binding.functions result
  |> List.filter (fun function_ ->
      function_ |> Semantic_module_expression_binding.function_symbol
      |> Semantic_symbol.name |> String.equal name)
  |> List.rev |> List.hd

let occurrences result name =
  function_named result name
  |> Semantic_module_expression_binding.function_occurrences

let symbol_id symbol = Semantic_symbol.id symbol |> Semantic_symbol.Id.to_int

let module_publication occurrence =
  match Semantic_module_expression_binding.occurrence_resolution occurrence with
  | Semantic_module_expression_binding.Module_binding publication -> publication
  | Semantic_module_expression_binding.Local_binding _ ->
      Alcotest.fail "expected a module binding, got a local binding"
  | Semantic_module_expression_binding.Outer_candidate ->
      Alcotest.fail "expected a module binding, got an outer candidate"

let resolution_name occurrence =
  match Semantic_module_expression_binding.occurrence_resolution occurrence with
  | Semantic_module_expression_binding.Local_binding binding ->
      "local:" ^ Semantic_symbol.name binding.symbol
  | Semantic_module_expression_binding.Module_binding publication ->
      Printf.sprintf "module:%s:%s"
        (Semantic_module_expression_binding.publication_kind publication
        |> Semantic_module_expression_binding.publication_kind_name)
        (Semantic_module_expression_binding.publication_source_symbol
           publication
        |> Semantic_symbol.name)
  | Semantic_module_expression_binding.Outer_candidate -> "outer"

let signature occurrences =
  occurrences
  |> List.map (fun occurrence ->
      ( Semantic_module_expression_binding.occurrence_name occurrence,
        resolution_name occurrence ))

let source_visibility_recursion_and_local_precedence () =
  let prepared =
    prepare ~path:"module-expression-source-order.HC"
      "I64 global;\n\
       I64 Prior(){global;return Prior();}\n\
       I64 Use(I64 global){global;Prior();return global+later;}\n\
       I64 later;"
  in
  let result = resolve prepared |> checked in
  Alcotest.(check (list (pair string string)))
    "prior function bindings"
    [
      ("global", "module:global-variable:global");
      ("Prior", "module:function:Prior");
    ]
    (signature (occurrences result "Prior"));
  Alcotest.(check (list (pair string string)))
    "current source visibility"
    [
      ("global", "local:global");
      ("Prior", "module:function:Prior");
      ("global", "local:global");
      ("later", "outer");
    ]
    (signature (occurrences result "Use"));
  let recursive = List.nth (occurrences result "Prior") 1 in
  let publication = module_publication recursive in
  Alcotest.(check int)
    "recursion sees its own item" 1
    (Semantic_module_expression_binding.publication_item_index publication)

let joined_function_identity_in_both_modes () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"module-expression-joined-function.HC"
          "extern I64 Joined();I64 Joined(){return Joined();}"
      in
      let result = resolve prepared |> checked in
      let occurrence = occurrences result "Joined" |> List.hd in
      let publication = module_publication occurrence in
      let declarations =
        Semantic_function_resolution.declarations prepared.functions
      in
      let first_identity =
        declarations |> List.hd
        |> Semantic_function_resolution.resolved_declaration_identity_symbol
      in
      let definition_site =
        List.nth declarations 1
        |> Semantic_function_resolution.resolved_declaration_site
        |> Semantic_function_resolution.declaration_site_function
        |> Semantic_function_type_resolution.function_symbol
      in
      Alcotest.(check int)
        "definition site is retained"
        (symbol_id definition_site)
        (publication
       |> Semantic_module_expression_binding.publication_source_symbol
       |> symbol_id);
      Alcotest.(check int)
        "joined identity is canonical" (symbol_id first_identity)
        (publication
       |> Semantic_module_expression_binding.publication_canonical_symbol
       |> symbol_id);
      Alcotest.(check bool)
        "site and identity remain distinct" true
        (symbol_id definition_site <> symbol_id first_identity))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let completed_aggregate_forward_is_canonical () =
  let prepared =
    prepare ~path:"module-expression-aggregate-forward.HC"
      "extern class Node;\n\
       I64 Before(){return Node.value;}\n\
       class Node {I64 value;};"
  in
  let result = resolve prepared |> checked in
  let publication =
    occurrences result "Before" |> List.hd |> module_publication
  in
  let declarations =
    Semantic_aggregate_resolution.declarations prepared.aggregates
  in
  let forward_symbol =
    declarations |> List.hd
    |> Semantic_aggregate_resolution.resolved_declaration_site
    |> Semantic_aggregate_resolution.declaration_site_symbol
  in
  let canonical =
    declarations |> List.hd
    |> Semantic_aggregate_resolution.resolved_declaration_identity_symbol
  in
  Alcotest.(check string)
    "aggregate publication kind" "aggregate"
    (publication |> Semantic_module_expression_binding.publication_kind
   |> Semantic_module_expression_binding.publication_kind_name);
  Alcotest.(check int)
    "forward site" (symbol_id forward_symbol)
    (publication |> Semantic_module_expression_binding.publication_source_symbol
   |> symbol_id);
  Alcotest.(check int)
    "completed identity" (symbol_id canonical)
    (publication
   |> Semantic_module_expression_binding.publication_canonical_symbol
   |> symbol_id);
  Alcotest.(check bool)
    "completion changes the canonical symbol" true
    (symbol_id forward_symbol <> symbol_id canonical)

let mixed_hash_chain_uses_newest_kind () =
  let prepared =
    prepare ~path:"module-expression-mixed-shadowing.HC"
      "extern class Shared;\n\
       I64 Shared;\n\
       I64 GlobalWins(){return Shared;}\n\
       I64 Run;\n\
       I64 Run(){return 1;}\n\
       I64 FunctionWins(){return Run();}\n\
       I64 TypeWins;\n\
       extern class TypeWins;\n\
       I64 AggregateWins(){return TypeWins.value;}"
  in
  let result = resolve prepared |> checked in
  let selected function_name =
    occurrences result function_name
    |> List.hd |> module_publication
    |> Semantic_module_expression_binding.publication_kind
    |> Semantic_module_expression_binding.publication_kind_name
  in
  Alcotest.(check string)
    "global shadows aggregate" "global-variable" (selected "GlobalWins");
  Alcotest.(check string)
    "function shadows global" "function" (selected "FunctionWins");
  Alcotest.(check string)
    "aggregate shadows global" "aggregate" (selected "AggregateWins")

let global_definition_and_alias_record () =
  let prepared =
    prepare ~path:"module-expression-global-alias.HC"
      "extern I64 slot;I64 slot;I64 Read(){return slot;}"
  in
  let result = resolve prepared |> checked in
  let records = Semantic_global_resolution.records prepared.globals in
  let extern_record = List.hd records in
  let definition_record = List.nth records 1 in
  let definition_symbol =
    Semantic_global_resolution.global_record_symbol definition_record
  in
  let publication =
    occurrences result "Read" |> List.hd |> module_publication
  in
  Alcotest.(check int)
    "newest global record is selected"
    (symbol_id definition_symbol)
    (publication |> Semantic_module_expression_binding.publication_source_symbol
   |> symbol_id);
  Alcotest.(check int)
    "global record is its own canonical symbol"
    (symbol_id definition_symbol)
    (publication
   |> Semantic_module_expression_binding.publication_canonical_symbol
   |> symbol_id);
  match Semantic_global_resolution.global_record_alias_target extern_record with
  | None -> Alcotest.fail "expected the extern record to retain its alias edge"
  | Some alias ->
      Alcotest.(check int)
        "extern alias points at the selected definition"
        (symbol_id definition_symbol)
        (symbol_id alias)

let source_origin = function
  | Semantic_symbol.Source_location source -> source
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected source provenance"

let generated_provenance_determinism_and_purity () =
  let prepared =
    prepare ~path:"module-expression-generated.HC"
      "#define USE target\nI64 target;U0 Generated(){USE+0;}"
  in
  let table = Session.semantic_symbols prepared.session in
  let before = Semantic_symbol_table.all_symbols table |> List.length in
  let first = resolve prepared |> checked in
  let middle = Semantic_symbol_table.all_symbols table |> List.length in
  let second = resolve prepared |> checked in
  let after = Semantic_symbol_table.all_symbols table |> List.length in
  let first_occurrences = occurrences first "Generated" in
  let second_occurrences = occurrences second "Generated" in
  Alcotest.(check (list (pair string string)))
    "repeated binding is deterministic"
    (signature first_occurrences)
    (signature second_occurrences);
  Alcotest.(check (pair int int))
    "symbol table is unchanged" (before, before) (middle, after);
  match first_occurrences with
  | [ occurrence ] ->
      let source =
        source_origin
          (Semantic_module_expression_binding.occurrence_origin occurrence)
      in
      Alcotest.(check bool)
        "macro invocation is retained" true
        (Option.is_some source.generated_from);
      Alcotest.(check bool)
        "macro definition is retained" true
        (Option.is_some source.defined_at)
  | _ -> Alcotest.fail "expected one generated identifier occurrence"

let expect_low_error_code expected = function
  | Ok _ -> Alcotest.failf "expected %s" expected
  | Error error ->
      Alcotest.(check string)
        "stable semantic error code" expected
        (Semantic_module_expression_binding.error_code error)

let validation_errors () =
  let prepared =
    prepare ~path:"module-expression-validation.HC"
      "I64 value;U0 Validate(){value;}"
  in
  let valid = resolve prepared |> checked in
  let table = Session.semantic_symbols prepared.session in
  let parent = Semantic_declaration_collection.scope prepared.declarations in
  expect_low_error_code "HCSEMA0021"
    (Semantic_module_expression_binding.resolve ~table ~parent
       ~compilation_mode:Semantic_function_resolution.Jit
       ~expressions:prepared.expressions []);
  expect_low_error_code "HCSEMA0020"
    (Semantic_module_expression_binding.resolve ~table ~parent
       ~compilation_mode:Semantic_function_resolution.Jit
       ~expressions:prepared.expressions
       (Semantic_module_expression_binding.publications valid |> List.rev));
  let foreign = Semantic_symbol_table.create () in
  expect_low_error_code "HCSEMA0020"
    (Semantic_module_expression_binding.resolve ~table
       ~parent:(Semantic_symbol_table.root foreign)
       ~compilation_mode:Semantic_function_resolution.Jit
       ~expressions:prepared.expressions
       (Semantic_module_expression_binding.publications valid));
  let aot_globals =
    checked
      (Holyc_lib.resolve_global_records prepared.session
         ~declarations:prepared.declarations ~globals:prepared.global_types
         ~compilation_mode:Preprocessor.Aot prepared.ast)
  in
  match
    Holyc_lib.resolve_module_expressions prepared.session
      ~declarations:prepared.declarations ~aggregates:prepared.aggregates
      ~functions:prepared.functions ~globals:aot_globals
      ~expressions:prepared.expressions
  with
  | Ok _ -> Alcotest.fail "expected mismatched compilation modes to fail"
  | Error message ->
      Alcotest.(check bool)
        "driver uses the stable validation family" true
        (String.starts_with ~prefix:"HCSEMA0020: " message)

let tests =
  [
    Alcotest.test_case "source visibility, recursion, and local precedence"
      `Quick source_visibility_recursion_and_local_precedence;
    Alcotest.test_case "joined function identity in both modes" `Quick
      joined_function_identity_in_both_modes;
    Alcotest.test_case "completed aggregate forward is canonical" `Quick
      completed_aggregate_forward_is_canonical;
    Alcotest.test_case "mixed hash chain uses newest kind" `Quick
      mixed_hash_chain_uses_newest_kind;
    Alcotest.test_case "global definition and alias record" `Quick
      global_definition_and_alias_record;
    Alcotest.test_case "generated provenance, determinism, and purity" `Quick
      generated_provenance_determinism_and_purity;
    Alcotest.test_case "validation errors" `Quick validation_errors;
  ]
