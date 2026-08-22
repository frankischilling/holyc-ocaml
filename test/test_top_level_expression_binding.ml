open Holyc_lib

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let checked_binding = function
  | Ok value -> value
  | Error error ->
      error |> Semantic_top_level_expression_binding.error_to_string
      |> Alcotest.fail

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
  module_expressions : Semantic_module_expression_binding.t;
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
  let module_expressions =
    checked
      (Holyc_lib.resolve_module_expressions session ~declarations ~aggregates
         ~functions ~globals ~expressions)
  in
  { session; ast; declarations; module_expressions; globals }

let resolve prepared =
  Holyc_lib.resolve_top_level_expressions prepared.session
    ~declarations:prepared.declarations
    ~module_expressions:prepared.module_expressions prepared.ast

let resolution_name occurrence =
  match
    Semantic_top_level_expression_binding.occurrence_resolution occurrence
  with
  | Semantic_top_level_expression_binding.Module_binding publication ->
      Printf.sprintf "module:%s:%s"
        (Semantic_module_expression_binding.publication_kind publication
        |> Semantic_module_expression_binding.publication_kind_name)
        (publication
       |> Semantic_module_expression_binding.publication_source_symbol
       |> Semantic_symbol.name)
  | Semantic_top_level_expression_binding.Outer_candidate -> "outer"

let signature result =
  Semantic_top_level_expression_binding.all_occurrences result
  |> List.map (fun occurrence ->
      ( Semantic_top_level_expression_binding.occurrence_name occurrence,
        resolution_name occurrence ))

let symbol_id symbol = Semantic_symbol.id symbol |> Semantic_symbol.Id.to_int

let source_order_and_outer_candidates () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"top-level-source-order.HC"
          "extern I64 value;value;I64 value;value;I64 Run(){return \
           0;}Run();(Print)(\"%d\",value);"
      in
      let result = resolve prepared |> checked in
      Alcotest.(check (list (pair string string)))
        "top-level bindings"
        [
          ("value", "module:global-variable:value");
          ("value", "module:global-variable:value");
          ("Run", "module:function:Run");
          ("Print", "outer");
          ("value", "module:global-variable:value");
        ]
        (signature result);
      let statements =
        Semantic_top_level_expression_binding.statements result
      in
      Alcotest.(check (list int))
        "source item indexes" [ 1; 3; 5; 6 ]
        (List.map Semantic_top_level_expression_binding.statement_item_index
           statements);
      let records = Semantic_global_resolution.records prepared.globals in
      let first_symbol =
        List.hd records |> Semantic_global_resolution.global_record_symbol
      in
      let second_symbol =
        List.nth records 1 |> Semantic_global_resolution.global_record_symbol
      in
      let occurrences =
        Semantic_top_level_expression_binding.all_occurrences result
      in
      let selected_symbol index =
        match
          List.nth occurrences index
          |> Semantic_top_level_expression_binding.occurrence_resolution
        with
        | Semantic_top_level_expression_binding.Module_binding publication ->
            publication
            |> Semantic_module_expression_binding.publication_source_symbol
        | Semantic_top_level_expression_binding.Outer_candidate ->
            Alcotest.fail "expected a module publication"
      in
      Alcotest.(check int)
        "first statement sees the extern" (symbol_id first_symbol)
        (selected_symbol 0 |> symbol_id);
      Alcotest.(check int)
        "later statement sees the definition" (symbol_id second_symbol)
        (selected_symbol 1 |> symbol_id))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let nested_statement_expression_order () =
  let prepared =
    prepare ~path:"top-level-nested.HC"
      "I64 \
       value;{if(value)value=value+Print(\"%d\",value);while(value)value--;switch(value){case \
       0:value;}}"
  in
  let result = resolve prepared |> checked in
  Alcotest.(check (list string))
    "recursive expression order"
    [
      "value";
      "value";
      "value";
      "Print";
      "value";
      "value";
      "value";
      "value";
      "value";
    ]
    (Semantic_top_level_expression_binding.all_occurrences result
    |> List.map Semantic_top_level_expression_binding.occurrence_name);
  Alcotest.(check (list int))
    "contiguous occurrence identities"
    [ 0; 1; 2; 3; 4; 5; 6; 7; 8 ]
    (Semantic_top_level_expression_binding.all_occurrences result
    |> List.map Semantic_top_level_expression_binding.occurrence_index)

let source_origin = function
  | Semantic_symbol.Source_location source -> source
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected source provenance"

let generated_provenance_determinism_and_purity () =
  let prepared =
    prepare ~path:"top-level-generated.HC" "#define USE value\nI64 value;USE;"
  in
  let table = Session.semantic_symbols prepared.session in
  let before = Semantic_symbol_table.all_symbols table |> List.length in
  let first = resolve prepared |> checked in
  let middle = Semantic_symbol_table.all_symbols table |> List.length in
  let second = resolve prepared |> checked in
  let after = Semantic_symbol_table.all_symbols table |> List.length in
  Alcotest.(check (list (pair string string)))
    "repeated binding is deterministic" (signature first) (signature second);
  Alcotest.(check (pair int int))
    "symbol table is unchanged" (before, before) (middle, after);
  match Semantic_top_level_expression_binding.all_occurrences first with
  | [ occurrence ] ->
      let source =
        occurrence |> Semantic_top_level_expression_binding.occurrence_origin
        |> source_origin
      in
      Alcotest.(check bool)
        "definition invocation is retained" true
        (Option.is_some source.generated_from);
      Alcotest.(check bool)
        "definition site is retained" true
        (Option.is_some source.defined_at)
  | _ -> Alcotest.fail "expected one generated top-level occurrence"

let low_level_aggregate_publication () =
  let prepared = prepare ~path:"top-level-aggregate.HC" "extern class Node;" in
  let publications =
    Semantic_module_expression_binding.publications prepared.module_expressions
  in
  let publication = List.hd publications in
  let event =
    checked
      (Semantic_top_level_expression_binding.make_identifier ~name:"Node"
         ~origin:
           (Semantic_module_expression_binding.publication_source_symbol
              publication
           |> Semantic_symbol.origin))
  in
  let input =
    checked
      (Semantic_top_level_expression_binding.make_statement ~statement_index:0
         ~item_index:1
         ~origin:
           (Semantic_module_expression_binding.publication_source_symbol
              publication
           |> Semantic_symbol.origin)
         [ event ])
  in
  let table = Session.semantic_symbols prepared.session in
  let parent = Semantic_declaration_collection.scope prepared.declarations in
  let result =
    Semantic_top_level_expression_binding.resolve ~table ~parent
      ~module_expressions:prepared.module_expressions [ input ]
    |> checked_binding
  in
  match Semantic_top_level_expression_binding.all_occurrences result with
  | [ occurrence ] ->
      Alcotest.(check string)
        "aggregate publication remains distinct" "module:aggregate:Node"
        (resolution_name occurrence)
  | _ -> Alcotest.fail "expected one aggregate occurrence"

let expect_low_error_code expected = function
  | Ok _ -> Alcotest.failf "expected %s" expected
  | Error error ->
      Alcotest.(check string)
        "stable semantic error code" expected
        (Semantic_top_level_expression_binding.error_code error)

let validation_errors () =
  let prepared = prepare ~path:"top-level-validation.HC" "I64 value;value;" in
  let valid = resolve prepared |> checked in
  let table = Session.semantic_symbols prepared.session in
  let parent = Semantic_declaration_collection.scope prepared.declarations in
  let statement =
    Semantic_top_level_expression_binding.statements valid
    |> List.hd |> Semantic_top_level_expression_binding.statement_source
  in
  expect_low_error_code "HCSEMA0052"
    (Semantic_top_level_expression_binding.resolve ~table ~parent
       ~module_expressions:prepared.module_expressions [ statement; statement ]);
  let foreign = Semantic_symbol_table.create () in
  expect_low_error_code "HCSEMA0052"
    (Semantic_top_level_expression_binding.resolve ~table
       ~parent:(Semantic_symbol_table.root foreign)
       ~module_expressions:prepared.module_expressions [ statement ]);
  let foreign_session = Session.create () in
  let foreign_table = Session.semantic_symbols foreign_session in
  expect_low_error_code "HCSEMA0052"
    (Semantic_top_level_expression_binding.resolve ~table:foreign_table
       ~parent:(Semantic_symbol_table.root foreign_table)
       ~module_expressions:prepared.module_expressions [ statement ])

let tests =
  [
    Alcotest.test_case "source order and outer candidates" `Quick
      source_order_and_outer_candidates;
    Alcotest.test_case "nested statement expression order" `Quick
      nested_statement_expression_order;
    Alcotest.test_case "generated provenance, determinism, and purity" `Quick
      generated_provenance_determinism_and_purity;
    Alcotest.test_case "low-level aggregate publication" `Quick
      low_level_aggregate_publication;
    Alcotest.test_case "validation errors" `Quick validation_errors;
  ]
