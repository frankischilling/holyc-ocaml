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
  { session; expressions }

let semantic_kind = function
  | Semantic_outer_environment.Aggregate -> Semantic_symbol.Aggregate_type
  | Semantic_outer_environment.Function -> Semantic_symbol.Function
  | Semantic_outer_environment.Global_variable ->
      Semantic_symbol.Global_variable
  | Semantic_outer_environment.Export_system_symbol ->
      Semantic_symbol.Assembler_symbol

let add_symbol prepared name record_kind =
  let table = Session.semantic_symbols prepared.session in
  checked
    (Semantic_symbol_table.add table
       ~scope:(Semantic_symbol_table.root table)
       ~name
       ~kind:(semantic_kind record_kind)
       ~origin:(Semantic_symbol.Synthesized ("outer fixture " ^ name)))

let checked_environment = function
  | Ok value -> value
  | Error error ->
      Alcotest.fail (Semantic_outer_environment.error_to_string error)

let make_entry prepared entry_index (name, record_kind) =
  Semantic_outer_environment.make_entry
    ~symbol:(add_symbol prepared name record_kind)
    ~record_kind ~entry_index
  |> checked_environment

let make_table prepared ~table_kind ~table_index records =
  records
  |> List.mapi (make_entry prepared)
  |> Semantic_outer_environment.make_table ~table_kind ~table_index
  |> checked_environment

let environment prepared mode tables =
  Holyc_lib.create_outer_environment prepared.session ~compilation_mode:mode
    tables
  |> checked

let resolve prepared environment =
  Holyc_lib.resolve_outer_expressions prepared.session ~environment
    ~expressions:prepared.expressions
  |> checked

let function_named result name =
  Semantic_outer_expression_binding.functions result
  |> List.find (fun function_ ->
      function_ |> Semantic_outer_expression_binding.function_symbol
      |> Semantic_symbol.name |> String.equal name)

let occurrences result name =
  function_named result name
  |> Semantic_outer_expression_binding.function_occurrences

let outer_binding occurrence =
  match Semantic_outer_expression_binding.occurrence_resolution occurrence with
  | Semantic_outer_expression_binding.Outer_binding binding -> binding
  | Semantic_outer_expression_binding.Local_binding _ ->
      Alcotest.fail "expected an outer binding, got a local binding"
  | Semantic_outer_expression_binding.Module_binding _ ->
      Alcotest.fail "expected an outer binding, got a module binding"

let outer_signature occurrence =
  let binding = outer_binding occurrence in
  let table = Semantic_outer_environment.binding_table binding in
  let entry = Semantic_outer_environment.binding_entry binding in
  ( Semantic_outer_expression_binding.occurrence_name occurrence,
    Semantic_outer_environment.table_kind table
    |> Semantic_outer_environment.table_kind_name,
    Semantic_outer_environment.entry_record_kind entry
    |> Semantic_outer_environment.record_kind_name )

let signature result = occurrences result "Read" |> List.map outer_signature

let jit_current_parent_and_assembler_order () =
  let prepared =
    prepare ~path:"outer-jit-chain.HC"
      "I64 Read(){return Current+Parent+Asm+Shared;}"
  in
  let tables =
    [
      make_table prepared ~table_kind:(Semantic_outer_environment.Jit_task 0)
        ~table_index:0
        [
          ("Current", Semantic_outer_environment.Global_variable);
          ("Shared", Semantic_outer_environment.Global_variable);
        ];
      make_table prepared ~table_kind:(Semantic_outer_environment.Jit_task 1)
        ~table_index:1
        [
          ("Parent", Semantic_outer_environment.Function);
          ("Shared", Semantic_outer_environment.Aggregate);
        ];
      make_table prepared ~table_kind:Semantic_outer_environment.Assembler
        ~table_index:2
        [
          ("Asm", Semantic_outer_environment.Export_system_symbol);
          ("Shared", Semantic_outer_environment.Export_system_symbol);
        ];
    ]
  in
  let result =
    environment prepared Preprocessor.Jit tables |> resolve prepared
  in
  Alcotest.(check (list (triple string string string)))
    "JIT lookup order"
    [
      ("Current", "jit-task-0", "global-variable");
      ("Parent", "jit-task-1", "function");
      ("Asm", "assembler", "export-system-symbol");
      ("Shared", "jit-task-0", "global-variable");
    ]
    (signature result)

let newest_record_wins_within_one_table () =
  let prepared =
    prepare ~path:"outer-newest-record.HC" "I64 Read(){return Shared;}"
  in
  let task =
    make_table prepared ~table_kind:(Semantic_outer_environment.Jit_task 0)
      ~table_index:0
      [
        ("Shared", Semantic_outer_environment.Aggregate);
        ("Shared", Semantic_outer_environment.Function);
        ("Shared", Semantic_outer_environment.Global_variable);
        ("Shared", Semantic_outer_environment.Export_system_symbol);
      ]
  in
  let assembler =
    make_table prepared ~table_kind:Semantic_outer_environment.Assembler
      ~table_index:1 []
  in
  let result =
    environment prepared Preprocessor.Jit [ task; assembler ]
    |> resolve prepared
  in
  let binding = occurrences result "Read" |> List.hd |> outer_binding in
  let entry = Semantic_outer_environment.binding_entry binding in
  Alcotest.(check int)
    "newest entry index" 3
    (Semantic_outer_environment.entry_index entry);
  Alcotest.(check string)
    "newest record kind" "export-system-symbol"
    (Semantic_outer_environment.entry_record_kind entry
    |> Semantic_outer_environment.record_kind_name)

let aot_parent_and_assembler_order () =
  let prepared =
    prepare ~mode:Preprocessor.Aot ~path:"outer-aot-chain.HC"
      "I64 Read(){return Near+Far+Asm+Shared;}"
  in
  let tables =
    [
      make_table prepared ~table_kind:(Semantic_outer_environment.Aot_parent 0)
        ~table_index:0
        [
          ("Near", Semantic_outer_environment.Function);
          ("Shared", Semantic_outer_environment.Global_variable);
        ];
      make_table prepared ~table_kind:(Semantic_outer_environment.Aot_parent 1)
        ~table_index:1
        [
          ("Far", Semantic_outer_environment.Aggregate);
          ("Shared", Semantic_outer_environment.Function);
        ];
      make_table prepared ~table_kind:Semantic_outer_environment.Assembler
        ~table_index:2
        [
          ("Asm", Semantic_outer_environment.Export_system_symbol);
          ("Shared", Semantic_outer_environment.Export_system_symbol);
        ];
    ]
  in
  let result =
    environment prepared Preprocessor.Aot tables |> resolve prepared
  in
  Alcotest.(check (list (triple string string string)))
    "AOT lookup order"
    [
      ("Near", "aot-parent-0", "function");
      ("Far", "aot-parent-1", "aggregate");
      ("Asm", "assembler", "export-system-symbol");
      ("Shared", "aot-parent-0", "global-variable");
    ]
    (signature result)

let top_level_aot_has_only_the_assembler_table () =
  let prepared =
    prepare ~mode:Preprocessor.Aot ~path:"outer-aot-top-level.HC"
      "I64 Read(){return Asm;}"
  in
  let assembler =
    make_table prepared ~table_kind:Semantic_outer_environment.Assembler
      ~table_index:0
      [ ("Asm", Semantic_outer_environment.Export_system_symbol) ]
  in
  let result =
    environment prepared Preprocessor.Aot [ assembler ] |> resolve prepared
  in
  Alcotest.(check (list (triple string string string)))
    "top-level AOT lookup"
    [ ("Asm", "assembler", "export-system-symbol") ]
    (signature result);
  let task =
    make_table prepared ~table_kind:(Semantic_outer_environment.Jit_task 0)
      ~table_index:0 []
  in
  let trailing_assembler =
    make_table prepared ~table_kind:Semantic_outer_environment.Assembler
      ~table_index:1 []
  in
  match
    Holyc_lib.create_outer_environment prepared.session
      ~compilation_mode:Preprocessor.Aot
      [ task; trailing_assembler ]
  with
  | Ok _ -> Alcotest.fail "expected an AOT role mismatch"
  | Error message ->
      Alcotest.(check bool)
        "stable environment validation family" true
        (String.starts_with ~prefix:"HCSEMA0022: " message)

let local_and_module_bindings_take_precedence () =
  let prepared =
    prepare ~path:"outer-inner-precedence.HC"
      "I64 ModuleValue;I64 Read(I64 LocalValue){return \
       LocalValue+ModuleValue+OuterValue;}"
  in
  let task =
    make_table prepared ~table_kind:(Semantic_outer_environment.Jit_task 0)
      ~table_index:0
      [
        ("LocalValue", Semantic_outer_environment.Global_variable);
        ("ModuleValue", Semantic_outer_environment.Global_variable);
        ("OuterValue", Semantic_outer_environment.Global_variable);
      ]
  in
  let assembler =
    make_table prepared ~table_kind:Semantic_outer_environment.Assembler
      ~table_index:1 []
  in
  let result =
    environment prepared Preprocessor.Jit [ task; assembler ]
    |> resolve prepared
  in
  let tags =
    occurrences result "Read"
    |> List.map (fun occurrence ->
        let name =
          Semantic_outer_expression_binding.occurrence_name occurrence
        in
        let tag =
          match
            Semantic_outer_expression_binding.occurrence_resolution occurrence
          with
          | Semantic_outer_expression_binding.Local_binding _ -> "local"
          | Semantic_outer_expression_binding.Module_binding _ -> "module"
          | Semantic_outer_expression_binding.Outer_binding _ -> "outer"
        in
        (name, tag))
  in
  Alcotest.(check (list (pair string string)))
    "inner lookup stages are preserved"
    [
      ("LocalValue", "local"); ("ModuleValue", "module"); ("OuterValue", "outer");
    ]
    tags

let defined_queries_use_complete_lookup_without_errors () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"outer-defined-query.HC"
          "I64 ModuleValue;I64 Read(I64 \
           LocalValue){defined(LocalValue);defined(ModuleValue);defined(OuterValue);defined(Missing);return \
           0;}"
      in
      let table_kind =
        match mode with
        | Preprocessor.Jit -> Semantic_outer_environment.Jit_task 0
        | Preprocessor.Aot -> Semantic_outer_environment.Aot_parent 0
      in
      let outer_table =
        make_table prepared ~table_kind ~table_index:0
          [
            ("LocalValue", Semantic_outer_environment.Global_variable);
            ("ModuleValue", Semantic_outer_environment.Global_variable);
            ("OuterValue", Semantic_outer_environment.Global_variable);
          ]
      in
      let assembler =
        make_table prepared ~table_kind:Semantic_outer_environment.Assembler
          ~table_index:1 []
      in
      let result =
        environment prepared mode [ outer_table; assembler ] |> resolve prepared
      in
      let queries =
        function_named result "Read"
        |> Semantic_outer_expression_binding.function_queries
      in
      let signature query =
        let name = Semantic_outer_expression_binding.query_name query in
        let resolution =
          match Semantic_outer_expression_binding.query_resolution query with
          | Semantic_outer_expression_binding.Query_undefined -> "undefined"
          | Semantic_outer_expression_binding.Query_binding
              (Semantic_outer_expression_binding.Local_binding _) -> "local"
          | Semantic_outer_expression_binding.Query_binding
              (Semantic_outer_expression_binding.Module_binding _) -> "module"
          | Semantic_outer_expression_binding.Query_binding
              (Semantic_outer_expression_binding.Outer_binding binding) ->
              let table = Semantic_outer_environment.binding_table binding in
              let entry = Semantic_outer_environment.binding_entry binding in
              Printf.sprintf "outer:%s:%s"
                (Semantic_outer_environment.table_kind table
                |> Semantic_outer_environment.table_kind_name)
                (Semantic_outer_environment.entry_record_kind entry
                |> Semantic_outer_environment.record_kind_name)
        in
        (name, resolution)
      in
      let outer_kind = Semantic_outer_environment.table_kind_name table_kind in
      Alcotest.(check (list (pair string string)))
        "defined keeps inner precedence and a proven miss"
        [
          ("LocalValue", "local");
          ("ModuleValue", "module");
          ("OuterValue", "outer:" ^ outer_kind ^ ":global-variable");
          ("Missing", "undefined");
        ]
        (List.map signature queries);
      Alcotest.(check bool)
        "outer query wraps its exact module query" true
        (List.for_all
           (fun query ->
             query |> Semantic_outer_expression_binding.query_source
             |> fun source ->
             List.exists (( == ) source)
               (prepared.expressions
              |> Semantic_module_expression_binding.functions
               |> List.find (fun function_ ->
                   function_
                   |> Semantic_module_expression_binding.function_symbol
                   |> Semantic_symbol.name |> String.equal "Read")
               |> Semantic_module_expression_binding.function_queries))
           queries))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let source_origin = function
  | Semantic_symbol.Source_location source -> source
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected source provenance"

let generated_unresolved_identifier_keeps_provenance () =
  let prepared =
    prepare ~path:"outer-generated-missing.HC"
      "#define USE Missing\nI64 Read(){return USE;}"
  in
  let task =
    make_table prepared ~table_kind:(Semantic_outer_environment.Jit_task 0)
      ~table_index:0 []
  in
  let assembler =
    make_table prepared ~table_kind:Semantic_outer_environment.Assembler
      ~table_index:1 []
  in
  let outer = environment prepared Preprocessor.Jit [ task; assembler ] in
  let table = Session.semantic_symbols prepared.session in
  match
    Semantic_outer_expression_binding.resolve ~table ~environment:outer
      ~expressions:prepared.expressions
  with
  | Ok _ -> Alcotest.fail "expected an unresolved outer identifier"
  | Error error -> (
      Alcotest.(check string)
        "stable unresolved code" "HCSEMA0024"
        (Semantic_outer_expression_binding.error_code error);
      Alcotest.(check string)
        "mode appears in the diagnostic"
        "ordinary identifier \"Missing\" is absent from the complete jit outer \
         table chain"
        (Semantic_outer_expression_binding.error_message error);
      match Semantic_outer_expression_binding.error_origin error with
      | Some origin ->
          let source = source_origin origin in
          Alcotest.(check bool)
            "macro invocation is retained" true
            (Option.is_some source.generated_from);
          Alcotest.(check bool)
            "macro definition is retained" true
            (Option.is_some source.defined_at)
      | None -> Alcotest.fail "expected an unresolved source origin")

let expect_environment_error expected = function
  | Ok _ -> Alcotest.failf "expected %s" expected
  | Error error ->
      Alcotest.(check string)
        "stable environment error code" expected
        (Semantic_outer_environment.error_code error)

let expect_binding_error expected = function
  | Ok _ -> Alcotest.failf "expected %s" expected
  | Error error ->
      Alcotest.(check string)
        "stable binding error code" expected
        (Semantic_outer_expression_binding.error_code error)

let determinism_purity_and_validation () =
  let prepared =
    prepare ~path:"outer-deterministic.HC"
      "#define USE Target\nI64 Read(){return USE;}"
  in
  let table = Session.semantic_symbols prepared.session in
  let target =
    add_symbol prepared "Target" Semantic_outer_environment.Function
  in
  let entry =
    Semantic_outer_environment.make_entry ~symbol:target
      ~record_kind:Semantic_outer_environment.Function ~entry_index:0
    |> checked_environment
  in
  let task =
    Semantic_outer_environment.make_table
      ~table_kind:(Semantic_outer_environment.Jit_task 0) ~table_index:0
      [ entry ]
    |> checked_environment
  in
  let assembler =
    Semantic_outer_environment.make_table
      ~table_kind:Semantic_outer_environment.Assembler ~table_index:1 []
    |> checked_environment
  in
  let outer = environment prepared Preprocessor.Jit [ task; assembler ] in
  let before = Semantic_symbol_table.all_symbols table |> List.length in
  let first = resolve prepared outer in
  let middle = Semantic_symbol_table.all_symbols table |> List.length in
  let second = resolve prepared outer in
  let after = Semantic_symbol_table.all_symbols table |> List.length in
  Alcotest.(check (list (triple string string string)))
    "repeated binding is deterministic" (signature first) (signature second);
  Alcotest.(check (pair int int))
    "binding does not mutate symbols" (before, before) (middle, after);
  (match occurrences first "Read" with
  | [ occurrence ] ->
      let source =
        source_origin
          (Semantic_outer_expression_binding.occurrence_origin occurrence)
      in
      Alcotest.(check bool)
        "successful macro invocation is retained" true
        (Option.is_some source.generated_from);
      Alcotest.(check bool)
        "successful macro definition is retained" true
        (Option.is_some source.defined_at)
  | _ -> Alcotest.fail "expected one generated identifier occurrence");
  let gap_symbol =
    add_symbol prepared "Gap" Semantic_outer_environment.Global_variable
  in
  let gap_entry =
    Semantic_outer_environment.make_entry ~symbol:gap_symbol
      ~record_kind:Semantic_outer_environment.Global_variable ~entry_index:2
    |> checked_environment
  in
  expect_environment_error "HCSEMA0022"
    (Semantic_outer_environment.make_table
       ~table_kind:(Semantic_outer_environment.Jit_task 0) ~table_index:0
       [ entry; gap_entry ]);
  let repeated_assembler =
    Semantic_outer_environment.make_table
      ~table_kind:Semantic_outer_environment.Assembler ~table_index:1 [ entry ]
    |> checked_environment
  in
  expect_environment_error "HCSEMA0022"
    (Semantic_outer_environment.create ~table
       ~compilation_mode:Semantic_outer_environment.Jit
       [ task; repeated_assembler ]);
  let wrong_index =
    Semantic_outer_environment.make_table
      ~table_kind:Semantic_outer_environment.Assembler ~table_index:2 []
    |> checked_environment
  in
  expect_environment_error "HCSEMA0022"
    (Semantic_outer_environment.create ~table
       ~compilation_mode:Semantic_outer_environment.Jit [ task; wrong_index ]);
  let foreign_table = Semantic_symbol_table.create () in
  let foreign_symbol =
    checked
      (Semantic_symbol_table.add foreign_table
         ~scope:(Semantic_symbol_table.root foreign_table)
         ~name:"Foreign" ~kind:Semantic_symbol.Function
         ~origin:(Semantic_symbol.Synthesized "foreign outer fixture"))
  in
  let foreign_entry =
    Semantic_outer_environment.make_entry ~symbol:foreign_symbol
      ~record_kind:Semantic_outer_environment.Function ~entry_index:0
    |> checked_environment
  in
  let foreign_task =
    Semantic_outer_environment.make_table
      ~table_kind:(Semantic_outer_environment.Jit_task 0) ~table_index:0
      [ foreign_entry ]
    |> checked_environment
  in
  expect_environment_error "HCSEMA0022"
    (Semantic_outer_environment.create ~table
       ~compilation_mode:Semantic_outer_environment.Jit
       [ foreign_task; assembler ]);
  let aot =
    Semantic_outer_environment.make_table
      ~table_kind:Semantic_outer_environment.Assembler ~table_index:0 []
    |> checked_environment
    |> fun assembler -> environment prepared Preprocessor.Aot [ assembler ]
  in
  expect_binding_error "HCSEMA0023"
    (Semantic_outer_expression_binding.resolve ~table ~environment:aot
       ~expressions:prepared.expressions);
  let foreign = prepare ~path:"outer-foreign.HC" "I64 Read(){return Target;}" in
  expect_binding_error "HCSEMA0023"
    (Semantic_outer_expression_binding.resolve ~table ~environment:outer
       ~expressions:foreign.expressions)

let tests =
  [
    Alcotest.test_case "JIT current, parent, and assembler order" `Quick
      jit_current_parent_and_assembler_order;
    Alcotest.test_case "newest record wins within one table" `Quick
      newest_record_wins_within_one_table;
    Alcotest.test_case "AOT parent and assembler order" `Quick
      aot_parent_and_assembler_order;
    Alcotest.test_case "top-level AOT uses only the assembler table" `Quick
      top_level_aot_has_only_the_assembler_table;
    Alcotest.test_case "local and module bindings take precedence" `Quick
      local_and_module_bindings_take_precedence;
    Alcotest.test_case "defined queries use the complete outer lookup" `Quick
      defined_queries_use_complete_lookup_without_errors;
    Alcotest.test_case "generated unresolved identifier keeps provenance" `Quick
      generated_unresolved_identifier_keeps_provenance;
    Alcotest.test_case "determinism, purity, and validation" `Quick
      determinism_purity_and_validation;
  ]
