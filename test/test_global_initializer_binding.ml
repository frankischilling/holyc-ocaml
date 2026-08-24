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
  mode : Preprocessor.compilation_mode;
  session : Session.t;
  ast : Ast.module_;
  globals : Semantic_global_resolution.t;
  expressions : Semantic_module_expression_binding.t;
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
  let function_expressions =
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
  let expressions =
    checked
      (Holyc_lib.resolve_module_expressions session ~declarations ~aggregates
         ~functions ~globals ~expressions:function_expressions)
  in
  { mode; session; ast; globals; expressions }

let semantic_kind = function
  | Semantic_outer_environment.Aggregate -> Semantic_symbol.Aggregate_type
  | Semantic_outer_environment.Function -> Semantic_symbol.Function
  | Semantic_outer_environment.Global_variable ->
      Semantic_symbol.Global_variable
  | Semantic_outer_environment.Export_system_symbol ->
      Semantic_symbol.Assembler_symbol

let add_outer_symbol prepared name record_kind =
  let table = Session.semantic_symbols prepared.session in
  checked
    (Semantic_symbol_table.add table
       ~scope:(Semantic_symbol_table.root table)
       ~name
       ~kind:(semantic_kind record_kind)
       ~origin:
         (Semantic_symbol.Synthesized ("global initializer fixture " ^ name)))

let checked_environment = function
  | Ok value -> value
  | Error error ->
      Alcotest.fail (Semantic_outer_environment.error_to_string error)

let make_table prepared ~table_kind ~table_index records =
  records
  |> List.mapi (fun entry_index (name, record_kind) ->
      Semantic_outer_environment.make_entry
        ~symbol:(add_outer_symbol prepared name record_kind)
        ~record_kind ~entry_index
      |> checked_environment)
  |> Semantic_outer_environment.make_table ~table_kind ~table_index
  |> checked_environment

let environment prepared tables =
  Holyc_lib.create_outer_environment prepared.session
    ~compilation_mode:prepared.mode tables
  |> checked

let jit_environment prepared task_records assembler_records =
  environment prepared
    [
      make_table prepared ~table_kind:(Semantic_outer_environment.Jit_task 0)
        ~table_index:0 task_records;
      make_table prepared ~table_kind:Semantic_outer_environment.Assembler
        ~table_index:1 assembler_records;
    ]

let aot_environment prepared parent_records assembler_records =
  environment prepared
    [
      make_table prepared ~table_kind:(Semantic_outer_environment.Aot_parent 0)
        ~table_index:0 parent_records;
      make_table prepared ~table_kind:Semantic_outer_environment.Assembler
        ~table_index:1 assembler_records;
    ]

let resolve prepared environment =
  Holyc_lib.resolve_global_initializers prepared.session ~environment
    ~expressions:prepared.expressions ~globals:prepared.globals prepared.ast
  |> checked

let global_named result name =
  Semantic_global_initializer_binding.globals result
  |> List.find (fun global ->
      global |> Semantic_global_initializer_binding.global_symbol
      |> Semantic_symbol.name |> String.equal name)

let occurrences result name =
  global_named result name
  |> Semantic_global_initializer_binding.global_occurrences

let path_string path = path |> List.map string_of_int |> String.concat "."

let resolution_name occurrence =
  match
    Semantic_global_initializer_binding.occurrence_resolution occurrence
  with
  | Semantic_global_initializer_binding.Module_binding publication ->
      Printf.sprintf "module:%s:%s"
        (Semantic_module_expression_binding.publication_kind publication
        |> Semantic_module_expression_binding.publication_kind_name)
        (Semantic_module_expression_binding.publication_source_symbol
           publication
        |> Semantic_symbol.name)
  | Semantic_global_initializer_binding.Outer_binding binding ->
      let table = Semantic_outer_environment.binding_table binding in
      let entry = Semantic_outer_environment.binding_entry binding in
      Printf.sprintf "outer:%s:%s:%s"
        (Semantic_outer_environment.table_kind table
        |> Semantic_outer_environment.table_kind_name)
        (Semantic_outer_environment.entry_record_kind entry
        |> Semantic_outer_environment.record_kind_name)
        (Semantic_outer_environment.entry_symbol entry |> Semantic_symbol.name)

let signature occurrences =
  occurrences
  |> List.map (fun occurrence ->
      ( Semantic_global_initializer_binding.occurrence_name occurrence,
        resolution_name occurrence,
        Semantic_global_initializer_binding.occurrence_initializer_path
          occurrence
        |> path_string ))

let self_and_comma_source_order () =
  let prepared =
    prepare ~path:"global-initializer-order.HC"
      "I64 Self=Self,Earlier=Self,Before=Later,Later=1;I64 \
       BeforeItem=Future;I64 Future=1;"
  in
  let outer =
    jit_environment prepared
      [
        ("Later", Semantic_outer_environment.Global_variable);
        ("Future", Semantic_outer_environment.Global_variable);
      ]
      []
  in
  let result = resolve prepared outer in
  Alcotest.(check (list (triple string string string)))
    "self reference is visible"
    [ ("Self", "module:global-variable:Self", "") ]
    (signature (occurrences result "Self"));
  Alcotest.(check (list (triple string string string)))
    "earlier comma declarator is visible"
    [ ("Self", "module:global-variable:Self", "") ]
    (signature (occurrences result "Earlier"));
  Alcotest.(check (list (triple string string string)))
    "later comma declarator is not visible"
    [ ("Later", "outer:jit-task-0:global-variable:Later", "") ]
    (signature (occurrences result "Before"));
  Alcotest.(check (list (triple string string string)))
    "later module item is not visible"
    [ ("Future", "outer:jit-task-0:global-variable:Future", "") ]
    (signature (occurrences result "BeforeItem"));
  let indexes =
    Semantic_global_initializer_binding.globals result
    |> List.map (fun global ->
        global |> Semantic_global_initializer_binding.global_publication
        |> Semantic_module_expression_binding.publication_declaration_index)
  in
  Alcotest.(check (list int))
    "global publication indexes" [ 0; 1; 2; 3; 4; 5 ] indexes;
  let positions =
    Semantic_global_initializer_binding.globals result
    |> List.map (fun global ->
        ( Semantic_global_initializer_binding.global_item_index global,
          Semantic_global_initializer_binding.global_declarator_index global ))
  in
  Alcotest.(check (list (pair int (option int))))
    "owning AST positions"
    [
      (0, Some 0);
      (0, Some 1);
      (0, Some 2);
      (0, Some 3);
      (1, Some 0);
      (2, Some 0);
    ]
    positions

let prior_module_and_nested_paths () =
  let prepared =
    prepare ~path:"global-initializer-nested.HC"
      "I64 First=1;I64 Values[4]={First,{Outer,First},Outer};I64 \
       Unbraced[2]=First,Outer;"
  in
  let outer =
    jit_environment prepared
      [
        ("First", Semantic_outer_environment.Function);
        ("Outer", Semantic_outer_environment.Global_variable);
      ]
      []
  in
  let result = resolve prepared outer in
  Alcotest.(check (list (triple string string string)))
    "nested source order and paths"
    [
      ("First", "module:global-variable:First", "0");
      ("Outer", "outer:jit-task-0:global-variable:Outer", "1.0");
      ("First", "module:global-variable:First", "1.1");
      ("Outer", "outer:jit-task-0:global-variable:Outer", "2");
    ]
    (signature (occurrences result "Values"));
  Alcotest.(check (list (triple string string string)))
    "unbraced array paths"
    [
      ("First", "module:global-variable:First", "0");
      ("Outer", "outer:jit-task-0:global-variable:Outer", "1");
    ]
    (signature (occurrences result "Unbraced"))

let mode_specific_outer_chains () =
  let check mode expected_near =
    let prepared =
      prepare ~mode ~path:"global-initializer-mode.HC"
        "I64 Value=Near+Asm+Classish;"
    in
    let outer =
      match mode with
      | Preprocessor.Jit ->
          jit_environment prepared
            [
              ("Near", Semantic_outer_environment.Global_variable);
              ("Classish", Semantic_outer_environment.Aggregate);
            ]
            [ ("Asm", Semantic_outer_environment.Export_system_symbol) ]
      | Preprocessor.Aot ->
          aot_environment prepared
            [
              ("Near", Semantic_outer_environment.Function);
              ("Classish", Semantic_outer_environment.Aggregate);
            ]
            [ ("Asm", Semantic_outer_environment.Export_system_symbol) ]
    in
    let result = resolve prepared outer in
    Alcotest.(check (list (triple string string string)))
      "mode-specific outer bindings"
      [
        ("Near", expected_near, "");
        ("Asm", "outer:assembler:export-system-symbol:Asm", "");
        ( "Classish",
          (match mode with
          | Preprocessor.Jit -> "outer:jit-task-0:aggregate:Classish"
          | Preprocessor.Aot -> "outer:aot-parent-0:aggregate:Classish"),
          "" );
      ]
      (signature (occurrences result "Value"))
  in
  check Preprocessor.Jit "outer:jit-task-0:global-variable:Near";
  check Preprocessor.Aot "outer:aot-parent-0:function:Near"

let symbol_origin (location : Ast.location) =
  Semantic_symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

let initializer_identifier (module_ : Ast.module_) =
  match module_.items with
  | Ast.Global_declaration declaration :: _ -> (
      match declaration.declarators with
      | declarator :: _ -> (
          match declarator.global_initial_value with
          | Some initial -> (
              match initial.global_initializer_value with
              | Ast.Scalar_initializer (Ast.Identifier_expression identifier) ->
                  identifier
              | _ -> Alcotest.fail "expected a scalar identifier initializer")
          | None -> Alcotest.fail "expected a global initializer")
      | [] -> Alcotest.fail "expected a global declarator")
  | _ -> Alcotest.fail "expected a global declaration"

let source_origin = function
  | Semantic_symbol.Source_location source -> source
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected source provenance"

let generated_unresolved_identifier_keeps_provenance () =
  let prepared =
    prepare ~path:"global-initializer-generated-missing.HC"
      "#define USE Missing\nI64 Value=USE;"
  in
  let outer = jit_environment prepared [] [] in
  let identifier = initializer_identifier prepared.ast in
  let event =
    Semantic_global_initializer_binding.make_identifier
      ~name:identifier.spelling
      ~origin:(symbol_origin identifier.location)
      ~occurrence_index:0 ~initializer_path:[]
    |> checked
  in
  let record = Semantic_global_resolution.records prepared.globals |> List.hd in
  let input =
    Semantic_global_initializer_binding.make_global ~record [ event ] |> checked
  in
  let table = Session.semantic_symbols prepared.session in
  match
    Semantic_global_initializer_binding.resolve ~table ~environment:outer
      ~expressions:prepared.expressions ~globals:prepared.globals [ input ]
  with
  | Ok _ -> Alcotest.fail "expected an unresolved global initializer name"
  | Error error -> (
      Alcotest.(check string)
        "stable unresolved code" "HCSEMA0026"
        (Semantic_global_initializer_binding.error_code error);
      Alcotest.(check string)
        "specific unresolved message"
        "global initializer for \"Value\" uses ordinary identifier \
         \"Missing\", which is absent from the visible module records and the \
         complete jit outer table chain"
        (Semantic_global_initializer_binding.error_message error);
      match Semantic_global_initializer_binding.error_origin error with
      | Some origin ->
          let source = source_origin origin in
          Alcotest.(check bool)
            "macro invocation is retained" true
            (Option.is_some source.generated_from);
          Alcotest.(check bool)
            "macro definition is retained" true
            (Option.is_some source.defined_at)
      | None -> Alcotest.fail "expected an unresolved identifier origin")

let expect_low_error expected = function
  | Ok _ -> Alcotest.failf "expected %s" expected
  | Error error ->
      Alcotest.(check string)
        "stable semantic error code" expected
        (Semantic_global_initializer_binding.error_code error)

let determinism_purity_and_validation () =
  let prepared =
    prepare ~path:"global-initializer-deterministic.HC"
      "#define USE Target\nI64 Value=USE;"
  in
  let outer =
    jit_environment prepared
      [ ("Target", Semantic_outer_environment.Function) ]
      []
  in
  let table = Session.semantic_symbols prepared.session in
  let before = Semantic_symbol_table.all_symbols table |> List.length in
  let first = resolve prepared outer in
  let middle = Semantic_symbol_table.all_symbols table |> List.length in
  let second = resolve prepared outer in
  let after = Semantic_symbol_table.all_symbols table |> List.length in
  Alcotest.(check (list (triple string string string)))
    "repeated resolution is deterministic"
    (signature (occurrences first "Value"))
    (signature (occurrences second "Value"));
  Alcotest.(check (pair int int))
    "resolution does not mutate symbols" (before, before) (middle, after);
  (match occurrences first "Value" with
  | [ occurrence ] ->
      let source =
        occurrence |> Semantic_global_initializer_binding.occurrence_origin
        |> source_origin
      in
      Alcotest.(check bool)
        "successful macro invocation is retained" true
        (Option.is_some source.generated_from);
      Alcotest.(check bool)
        "successful macro definition is retained" true
        (Option.is_some source.defined_at)
  | _ -> Alcotest.fail "expected one generated identifier occurrence");
  let record = Semantic_global_resolution.records prepared.globals |> List.hd in
  let bad_event =
    Semantic_global_initializer_binding.make_identifier ~name:"Target"
      ~origin:(Semantic_symbol.Synthesized "bad global initializer index")
      ~occurrence_index:1 ~initializer_path:[]
    |> checked
  in
  let bad_input =
    Semantic_global_initializer_binding.make_global ~record [ bad_event ]
    |> checked
  in
  expect_low_error "HCSEMA0025"
    (Semantic_global_initializer_binding.resolve ~table ~environment:outer
       ~expressions:prepared.expressions ~globals:prepared.globals [ bad_input ]);
  let aot_assembler =
    make_table prepared ~table_kind:Semantic_outer_environment.Assembler
      ~table_index:0 []
  in
  let aot_outer =
    Holyc_lib.create_outer_environment prepared.session
      ~compilation_mode:Preprocessor.Aot [ aot_assembler ]
    |> checked
  in
  expect_low_error "HCSEMA0025"
    (Semantic_global_initializer_binding.resolve ~table ~environment:aot_outer
       ~expressions:prepared.expressions ~globals:prepared.globals [ bad_input ]);
  let ordered =
    prepare ~path:"global-initializer-input-order.HC" "I64 First=1,Second=2;"
  in
  let ordered_outer = jit_environment ordered [] [] in
  let ordered_table = Session.semantic_symbols ordered.session in
  let inputs =
    Semantic_global_resolution.records ordered.globals
    |> List.map (fun record ->
        Semantic_global_initializer_binding.make_global ~record [] |> checked)
  in
  expect_low_error "HCSEMA0025"
    (Semantic_global_initializer_binding.resolve ~table:ordered_table
       ~environment:ordered_outer ~expressions:ordered.expressions
       ~globals:ordered.globals (List.rev inputs));
  expect_low_error "HCSEMA0025"
    (Semantic_global_initializer_binding.resolve ~table:ordered_table
       ~environment:ordered_outer ~expressions:ordered.expressions
       ~globals:ordered.globals
       [ List.hd inputs ]);
  expect_low_error "HCSEMA0025"
    (Semantic_global_initializer_binding.resolve ~table:ordered_table
       ~environment:ordered_outer ~expressions:ordered.expressions
       ~globals:ordered.globals
       [ List.hd inputs; List.hd inputs ]);
  let other = prepare ~path:"global-initializer-other.HC" "I64 Other=1;" in
  let other_outer = jit_environment other [] [] in
  let valid_input =
    Semantic_global_initializer_binding.make_global ~record [] |> checked
  in
  expect_low_error "HCSEMA0025"
    (Semantic_global_initializer_binding.resolve ~table ~environment:other_outer
       ~expressions:prepared.expressions ~globals:prepared.globals
       [ valid_input ]);
  (match
     Semantic_global_initializer_binding.make_identifier ~name:"BadPath"
       ~origin:(Semantic_symbol.Synthesized "bad global initializer path")
       ~occurrence_index:0 ~initializer_path:[ -1 ]
   with
  | Ok _ -> Alcotest.fail "expected a negative initializer path to fail"
  | Error message ->
      Alcotest.(check string)
        "negative path validation"
        "global initializer path cannot contain a negative index" message);
  match
    Holyc_lib.resolve_global_initializers prepared.session ~environment:outer
      ~expressions:prepared.expressions ~globals:prepared.globals other.ast
  with
  | Ok _ -> Alcotest.fail "expected AST drift to fail"
  | Error message ->
      Alcotest.(check bool)
        "driver validation uses the stable family" true
        (String.starts_with ~prefix:"HCSEMA0025: " message)

let tests =
  [
    Alcotest.test_case "self and comma source order" `Quick
      self_and_comma_source_order;
    Alcotest.test_case "prior module records and nested paths" `Quick
      prior_module_and_nested_paths;
    Alcotest.test_case "JIT and AOT outer chains" `Quick
      mode_specific_outer_chains;
    Alcotest.test_case "generated unresolved name keeps provenance" `Quick
      generated_unresolved_identifier_keeps_provenance;
    Alcotest.test_case "determinism, purity, and validation" `Quick
      determinism_purity_and_validation;
  ]
