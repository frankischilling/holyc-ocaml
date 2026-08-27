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

let config ?working_directory mode =
  checked
    (Preprocessor.Config.create ?working_directory ~compilation_mode:mode ())

type prepared = {
  session : Session.t;
  bindings : Semantic_top_level_expression_binding.t;
}

let finish_prepare mode session ast =
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
  let function_bindings =
    checked
      (Holyc_lib.index_function_bindings session ~declarations
         ~functions:collected_functions ~function_types ~local_types)
  in
  let function_expressions =
    checked
      (Holyc_lib.resolve_function_expressions session ~declarations
         ~functions:collected_functions ~local_types ~bindings:function_bindings
         ast)
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
         ~functions ~globals ~expressions:function_expressions)
  in
  let bindings =
    checked
      (Holyc_lib.resolve_top_level_expressions session ~declarations
         ~module_expressions ast)
  in
  { session; bindings }

let prepare ?(mode = Preprocessor.Jit) ~path contents =
  let session = Session.create () in
  let source = Session.add_source session ~path ~contents in
  let ast =
    Holyc_lib.parse_with_config session ~config:(config mode) ~source
    |> expect_ast
  in
  finish_prepare mode session ast

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
      error |> Semantic_outer_environment.error_to_string |> Alcotest.fail

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
  Holyc_lib.resolve_top_level_outer_expressions prepared.session ~environment
    ~expressions:prepared.bindings
  |> checked

let outer_signature binding =
  let table = Semantic_outer_environment.binding_table binding in
  let entry = Semantic_outer_environment.binding_entry binding in
  Printf.sprintf "outer:%s:%s:%d"
    (Semantic_outer_environment.table_kind table
    |> Semantic_outer_environment.table_kind_name)
    (Semantic_outer_environment.entry_record_kind entry
    |> Semantic_outer_environment.record_kind_name)
    (Semantic_outer_environment.entry_index entry)

let resolution_name occurrence =
  match
    Semantic_top_level_outer_expression_binding.occurrence_resolution occurrence
  with
  | Semantic_top_level_outer_expression_binding.Module_binding publication ->
      Printf.sprintf "module:%s:%s"
        (Semantic_module_expression_binding.publication_kind publication
        |> Semantic_module_expression_binding.publication_kind_name)
        (publication
       |> Semantic_module_expression_binding.publication_source_symbol
       |> Semantic_symbol.name)
  | Semantic_top_level_outer_expression_binding.Outer_binding binding ->
      outer_signature binding

let signature result =
  Semantic_top_level_outer_expression_binding.all_occurrences result
  |> List.map (fun occurrence ->
      ( Semantic_top_level_outer_expression_binding.occurrence_name occurrence,
        resolution_name occurrence ))

let query_resolution_name query =
  match
    Semantic_top_level_outer_expression_binding.query_resolution query
  with
  | Semantic_top_level_outer_expression_binding.Query_undefined -> "undefined"
  | Semantic_top_level_outer_expression_binding.Query_binding
      (Semantic_top_level_outer_expression_binding.Module_binding publication) ->
      Printf.sprintf "module:%s:%s"
        (Semantic_module_expression_binding.publication_kind publication
        |> Semantic_module_expression_binding.publication_kind_name)
        (publication
       |> Semantic_module_expression_binding.publication_source_symbol
       |> Semantic_symbol.name)
  | Semantic_top_level_outer_expression_binding.Query_binding
      (Semantic_top_level_outer_expression_binding.Outer_binding binding) ->
      outer_signature binding

let query_signature result =
  Semantic_top_level_outer_expression_binding.all_queries result
  |> List.map (fun query ->
      ( Semantic_top_level_outer_expression_binding.query_name query,
        query_resolution_name query ))

let jit_chain_module_precedence_and_newest_record () =
  let prepared =
    prepare ~path:"top-level-outer-jit.HC"
      "I64 ModuleValue;ModuleValue+Current+Parent+Asm+Shared;"
  in
  let tables =
    [
      make_table prepared ~table_kind:(Semantic_outer_environment.Jit_task 0)
        ~table_index:0
        [
          ("ModuleValue", Semantic_outer_environment.Function);
          ("Current", Semantic_outer_environment.Global_variable);
          ("Shared", Semantic_outer_environment.Function);
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
        [ ("Asm", Semantic_outer_environment.Export_system_symbol) ];
    ]
  in
  let result =
    environment prepared Preprocessor.Jit tables |> resolve prepared
  in
  Alcotest.(check (list (pair string string)))
    "module prefix and JIT outer order"
    [
      ("ModuleValue", "module:global-variable:ModuleValue");
      ("Current", "outer:jit-task-0:global-variable:1");
      ("Parent", "outer:jit-task-1:function:0");
      ("Asm", "outer:assembler:export-system-symbol:0");
      ("Shared", "outer:jit-task-0:global-variable:3");
    ]
    (signature result)

let aot_chain_preserves_statement_and_occurrence_identity () =
  let prepared =
    prepare ~mode:Preprocessor.Aot ~path:"top-level-outer-aot.HC"
      "0+Near+Far;0+Asm;"
  in
  let tables =
    [
      make_table prepared ~table_kind:(Semantic_outer_environment.Aot_parent 0)
        ~table_index:0
        [ ("Near", Semantic_outer_environment.Global_variable) ];
      make_table prepared ~table_kind:(Semantic_outer_environment.Aot_parent 1)
        ~table_index:1
        [ ("Far", Semantic_outer_environment.Function) ];
      make_table prepared ~table_kind:Semantic_outer_environment.Assembler
        ~table_index:2
        [ ("Asm", Semantic_outer_environment.Export_system_symbol) ];
    ]
  in
  let result =
    environment prepared Preprocessor.Aot tables |> resolve prepared
  in
  Alcotest.(check (list (pair string string)))
    "AOT lookup order"
    [
      ("Near", "outer:aot-parent-0:global-variable:0");
      ("Far", "outer:aot-parent-1:function:0");
      ("Asm", "outer:assembler:export-system-symbol:0");
    ]
    (signature result);
  Alcotest.(check (list int))
    "statement identities" [ 0; 1 ]
    (Semantic_top_level_outer_expression_binding.statements result
    |> List.map Semantic_top_level_outer_expression_binding.statement_index);
  Alcotest.(check (list int))
    "source item indexes" [ 0; 1 ]
    (Semantic_top_level_outer_expression_binding.statements result
    |> List.map Semantic_top_level_outer_expression_binding.statement_item_index
    );
  Alcotest.(check (list int))
    "occurrence identities" [ 0; 1; 2 ]
    (Semantic_top_level_outer_expression_binding.all_occurrences result
    |> List.map Semantic_top_level_outer_expression_binding.occurrence_index);
  let empty_assembler =
    make_table prepared ~table_kind:Semantic_outer_environment.Assembler
      ~table_index:0 []
  in
  let empty_aot = environment prepared Preprocessor.Aot [ empty_assembler ] in
  let table = Session.semantic_symbols prepared.session in
  match
    Semantic_top_level_outer_expression_binding.resolve ~table
      ~environment:empty_aot ~expressions:prepared.bindings
  with
  | Ok _ -> Alcotest.fail "expected an unresolved AOT identifier"
  | Error error ->
      Alcotest.(check string)
        "stable AOT unresolved code" "HCSEMA0054"
        (Semantic_top_level_outer_expression_binding.error_code error);
      Alcotest.(check bool)
        "AOT mode appears in the diagnostic" true
        (Semantic_top_level_outer_expression_binding.error_message error
        |> String.ends_with ~suffix:"complete aot outer table chain")

let defined_queries_use_complete_lookup_without_identifier_errors () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"top-level-outer-defined.HC"
          "I64 ModuleValue;defined(ModuleValue);defined(Current);defined(Parent);defined(Asm);defined(Missing);"
      in
      let current_kind, parent_kind =
        match mode with
        | Preprocessor.Jit ->
            ( Semantic_outer_environment.Jit_task 0,
              Semantic_outer_environment.Jit_task 1 )
        | Preprocessor.Aot ->
            ( Semantic_outer_environment.Aot_parent 0,
              Semantic_outer_environment.Aot_parent 1 )
      in
      let tables =
        [
          make_table prepared ~table_kind:current_kind ~table_index:0
            [
              ("ModuleValue", Semantic_outer_environment.Function);
              ("Current", Semantic_outer_environment.Global_variable);
            ];
          make_table prepared ~table_kind:parent_kind ~table_index:1
            [ ("Parent", Semantic_outer_environment.Function) ];
          make_table prepared ~table_kind:Semantic_outer_environment.Assembler
            ~table_index:2
            [ ("Asm", Semantic_outer_environment.Export_system_symbol) ];
        ]
      in
      let result = environment prepared mode tables |> resolve prepared in
      let current_name =
        Semantic_outer_environment.table_kind_name current_kind
      in
      let parent_name =
        Semantic_outer_environment.table_kind_name parent_kind
      in
      Alcotest.(check (list (pair string string)))
        "defined preserves module precedence and records a complete miss"
        [
          ("ModuleValue", "module:global-variable:ModuleValue");
          ("Current", "outer:" ^ current_name ^ ":global-variable:1");
          ("Parent", "outer:" ^ parent_name ^ ":function:0");
          ("Asm", "outer:assembler:export-system-symbol:0");
          ("Missing", "undefined");
        ]
        (query_signature result);
      let queries =
        Semantic_top_level_outer_expression_binding.all_queries result
      in
      Alcotest.(check (list int))
        "top-level outer query identities remain contiguous"
        [ 0; 1; 2; 3; 4 ]
        (List.map Semantic_top_level_outer_expression_binding.query_index queries);
      Alcotest.(check bool)
        "every outer query wraps the exact module-prefix query" true
        (List.for_all
           (fun query ->
             query |> Semantic_top_level_outer_expression_binding.query_source
             |> fun source ->
             List.exists (( == ) source)
               (Semantic_top_level_expression_binding.all_queries
                  prepared.bindings))
           queries))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let source_origin = function
  | Semantic_symbol.Source_location source -> source
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected source provenance"

let rec remove_tree path =
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
      Sys.readdir path |> Array.to_list |> List.sort String.compare
      |> List.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let generated_failure_and_included_success_keep_provenance () =
  let generated =
    prepare ~path:"top-level-outer-generated.HC" "#define USE Missing\n0+USE;"
  in
  let task =
    make_table generated ~table_kind:(Semantic_outer_environment.Jit_task 0)
      ~table_index:0 []
  in
  let assembler =
    make_table generated ~table_kind:Semantic_outer_environment.Assembler
      ~table_index:1 []
  in
  let outer = environment generated Preprocessor.Jit [ task; assembler ] in
  let table = Session.semantic_symbols generated.session in
  (match
     Semantic_top_level_outer_expression_binding.resolve ~table
       ~environment:outer ~expressions:generated.bindings
   with
  | Ok _ -> Alcotest.fail "expected an unresolved generated identifier"
  | Error error -> (
      Alcotest.(check string)
        "stable unresolved code" "HCSEMA0054"
        (Semantic_top_level_outer_expression_binding.error_code error);
      Alcotest.(check string)
        "mode appears in the diagnostic"
        "top-level identifier \"Missing\" is absent from the complete jit \
         outer table chain"
        (Semantic_top_level_outer_expression_binding.error_message error);
      match Semantic_top_level_outer_expression_binding.error_origin error with
      | None -> Alcotest.fail "expected an unresolved source origin"
      | Some origin ->
          let source = source_origin origin in
          Alcotest.(check bool)
            "definition invocation is retained" true
            (Option.is_some source.generated_from);
          Alcotest.(check bool)
            "definition site is retained" true
            (Option.is_some source.defined_at)));
  let directory = Filename.temp_dir "holyc-top-level-outer-" "" in
  Fun.protect
    ~finally:(fun () -> remove_tree directory)
    (fun () ->
      let root_path = Filename.concat directory "root.HC" in
      let included_path = Filename.concat directory "statements.HC" in
      write_file root_path "#include \"statements\"";
      write_file included_path "0+IncludedOuter;";
      let session = Session.create () in
      let source = checked (Session.load_source session ~path:root_path) in
      let ast =
        Holyc_lib.parse_with_config session
          ~config:(config ~working_directory:directory Preprocessor.Jit)
          ~source
        |> expect_ast
      in
      let included = finish_prepare Preprocessor.Jit session ast in
      let task =
        make_table included ~table_kind:(Semantic_outer_environment.Jit_task 0)
          ~table_index:0
          [ ("IncludedOuter", Semantic_outer_environment.Global_variable) ]
      in
      let assembler =
        make_table included ~table_kind:Semantic_outer_environment.Assembler
          ~table_index:1 []
      in
      let occurrence =
        environment included Preprocessor.Jit [ task; assembler ]
        |> resolve included
        |> Semantic_top_level_outer_expression_binding.all_occurrences
        |> List.hd
      in
      let location =
        occurrence
        |> Semantic_top_level_outer_expression_binding.occurrence_origin
        |> source_origin
      in
      let source_file =
        Source_manager.find (Session.sources session) location.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included occurrence keeps its source file" "statements.HC"
        (Source_file.path source_file |> Filename.basename))

let expect_error_code expected = function
  | Ok _ -> Alcotest.failf "expected %s" expected
  | Error error ->
      Alcotest.(check string)
        "stable semantic error code" expected
        (Semantic_top_level_outer_expression_binding.error_code error)

let determinism_purity_and_validation () =
  let prepared = prepare ~path:"top-level-outer-pure.HC" "0+Target;" in
  let task =
    make_table prepared ~table_kind:(Semantic_outer_environment.Jit_task 0)
      ~table_index:0
      [ ("Target", Semantic_outer_environment.Function) ]
  in
  let assembler =
    make_table prepared ~table_kind:Semantic_outer_environment.Assembler
      ~table_index:1 []
  in
  let outer = environment prepared Preprocessor.Jit [ task; assembler ] in
  let table = Session.semantic_symbols prepared.session in
  let before = Semantic_symbol_table.all_symbols table |> List.length in
  let first = resolve prepared outer in
  let middle = Semantic_symbol_table.all_symbols table |> List.length in
  let second = resolve prepared outer in
  let after = Semantic_symbol_table.all_symbols table |> List.length in
  Alcotest.(check (list (pair string string)))
    "repeated lookup is deterministic" (signature first) (signature second);
  Alcotest.(check (pair int int))
    "lookup does not mutate symbols" (before, before) (middle, after);
  Alcotest.(check bool)
    "source batch identity is preserved" true
    (Semantic_top_level_outer_expression_binding.source first
    == prepared.bindings);
  Alcotest.(check bool)
    "outer environment identity is preserved" true
    (Semantic_top_level_outer_expression_binding.environment first == outer);
  let aot_assembler =
    make_table prepared ~table_kind:Semantic_outer_environment.Assembler
      ~table_index:0 []
  in
  let aot = environment prepared Preprocessor.Aot [ aot_assembler ] in
  expect_error_code "HCSEMA0053"
    (Semantic_top_level_outer_expression_binding.resolve ~table ~environment:aot
       ~expressions:prepared.bindings);
  let foreign = prepare ~path:"top-level-outer-foreign.HC" "0+Target;" in
  expect_error_code "HCSEMA0053"
    (Semantic_top_level_outer_expression_binding.resolve ~table
       ~environment:outer ~expressions:foreign.bindings);
  let foreign_table = Session.semantic_symbols foreign.session in
  expect_error_code "HCSEMA0053"
    (Semantic_top_level_outer_expression_binding.resolve ~table:foreign_table
       ~environment:outer ~expressions:foreign.bindings)

let tests =
  [
    Alcotest.test_case "JIT chain, module precedence, and newest record" `Quick
      jit_chain_module_precedence_and_newest_record;
    Alcotest.test_case "AOT chain preserves source identities" `Quick
      aot_chain_preserves_statement_and_occurrence_identity;
    Alcotest.test_case "defined queries use the complete lookup chain" `Quick
      defined_queries_use_complete_lookup_without_identifier_errors;
    Alcotest.test_case "generated failure and included success provenance"
      `Quick generated_failure_and_included_success_keep_provenance;
    Alcotest.test_case "determinism, purity, and validation" `Quick
      determinism_purity_and_validation;
  ]
