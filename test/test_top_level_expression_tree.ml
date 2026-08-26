open Holyc_lib

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let expect_ast = function
  | Ok ast -> ast
  | Error diagnostics ->
      diagnostics
      |> List.map (fun diagnostic ->
          diagnostic.Diagnostic.code ^ ": " ^ diagnostic.message)
      |> String.concat ", " |> Alcotest.fail

let config mode = checked (Preprocessor.Config.create ~compilation_mode:mode ())

type prepared = {
  session : Session.t;
  ast : Ast.module_;
  declarations : Semantic_declaration_collection.t;
  bindings : Semantic_top_level_expression_binding.t;
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
  { session; ast; declarations; bindings }

let semantic_kind = function
  | Semantic_outer_environment.Aggregate -> Semantic_symbol.Aggregate_type
  | Semantic_outer_environment.Function -> Semantic_symbol.Function
  | Semantic_outer_environment.Global_variable ->
      Semantic_symbol.Global_variable
  | Semantic_outer_environment.Export_system_symbol ->
      Semantic_symbol.Assembler_symbol

let make_table prepared ~table_kind ~table_index records =
  let table = Session.semantic_symbols prepared.session in
  records
  |> List.mapi (fun entry_index (name, record_kind) ->
      let symbol =
        checked
          (Semantic_symbol_table.add table
             ~scope:(Semantic_symbol_table.root table)
             ~name
             ~kind:(semantic_kind record_kind)
             ~origin:(Semantic_symbol.Synthesized ("tree fixture " ^ name)))
      in
      Semantic_outer_environment.make_entry ~symbol ~record_kind ~entry_index
      |> function
      | Ok entry -> entry
      | Error error ->
          error |> Semantic_outer_environment.error_to_string |> Alcotest.fail)
  |> Semantic_outer_environment.make_table ~table_kind ~table_index
  |> function
  | Ok table -> table
  | Error error ->
      error |> Semantic_outer_environment.error_to_string |> Alcotest.fail

let outer prepared mode records =
  let tables =
    match mode with
    | Preprocessor.Jit ->
        [
          make_table prepared
            ~table_kind:(Semantic_outer_environment.Jit_task 0) ~table_index:0
            records;
          make_table prepared ~table_kind:Semantic_outer_environment.Assembler
            ~table_index:1 [];
        ]
    | Preprocessor.Aot ->
        [
          make_table prepared ~table_kind:Semantic_outer_environment.Assembler
            ~table_index:0 records;
        ]
  in
  checked
    (Holyc_lib.create_outer_environment prepared.session ~compilation_mode:mode
       tables)

let build prepared mode records =
  let environment = outer prepared mode records in
  let expressions =
    checked
      (Holyc_lib.resolve_top_level_outer_expressions prepared.session
         ~environment ~expressions:prepared.bindings)
  in
  checked
    (Holyc_lib.build_top_level_expression_trees prepared.session
       ~declarations:prepared.declarations ~compilation_mode:mode ~expressions
       prepared.ast)

let indexes accessor values = List.map accessor values

let rec expression_kinds expression =
  let open Semantic_function_call_resolution in
  let current =
    expression |> argument_expression_kind |> argument_expression_kind_name
  in
  let children =
    match argument_expression_kind expression with
    | Parenthesized_expression grouped -> expression_kinds grouped
    | Prefix_expression prefix -> expression_kinds (prefix_operand prefix)
    | Postfix_expression postfix -> expression_kinds (postfix_operand postfix)
    | Postfix_cast_expression (operand, _) -> expression_kinds operand
    | Binary_expression binary ->
        expression_kinds (binary_left binary)
        @ expression_kinds (binary_right binary)
    | Index_expression index ->
        expression_kinds (index_base index)
        @ expression_kinds (index_value index)
    | Member_access_expression member -> expression_kinds (member_base member)
    | Integer_literal _
    | Float_literal _
    | Character_literal _
    | String_literal _
    | Bound_identifier_expression _
    | Top_level_bound_identifier_expression _
    | Unresolved_expression _ -> []
  in
  current :: children

let complete_shapes_roles_calls_and_identities () =
  let source =
    "I64 F(I64 a=1,I64 b=2);I64 value;I64 values[3];\n\
     class Box{I64 member;};Box box;\n\
     ((-value+1)*2);2.5;'AB';F(,value);\n\
     if(value) \"v=%d\",F(value,);while(value)value--;\n\
     switch[value]{case 1...2:values[1];dft:box.member;}\n\
     F(value,2);(*F)(value,2);F;value(I64);box(Box *);$$;sizeof(I64);\n\
     offset(Box.member);\n\
     defined(value);"
  in
  List.iter
    (fun mode ->
      let prepared = prepare ~mode ~path:"top-level-tree.HC" source in
      let result = build prepared mode [] in
      let roots = Semantic_top_level_expression_tree.all_roots result in
      let calls = Semantic_top_level_expression_tree.all_calls result in
      let nodes =
        Semantic_top_level_expression_tree.all_expression_nodes result
      in
      Alcotest.(check (list int))
        "root identities"
        (List.init (List.length roots) Fun.id)
        (indexes Semantic_top_level_expression_tree.root_index roots);
      Alcotest.(check (list int))
        "call identities"
        (List.init (List.length calls) Fun.id)
        (calls
        |> List.map (fun call ->
            call |> Semantic_top_level_expression_tree.call_source
            |> Semantic_function_call_resolution.call_index));
      Alcotest.(check (list int))
        "node identities"
        (List.init (List.length nodes) Fun.id)
        (indexes Semantic_top_level_expression_tree.expression_node_index nodes);
      let role_names =
        List.map
          (fun root ->
            root |> Semantic_top_level_expression_tree.root_role
            |> Semantic_top_level_expression_tree.root_role_name)
          roots
      in
      List.iter
        (fun prefix ->
          Alcotest.(check bool)
            ("role " ^ prefix) true
            (List.exists (String.starts_with ~prefix) role_names))
        [
          "expression-statement:";
          "implicit-output:";
          "condition:";
          "switch-selector:";
          "switch-case:";
        ];
      let kinds =
        nodes
        |> List.concat_map (fun node ->
            node |> Semantic_top_level_expression_tree.expression_node_source
            |> expression_kinds)
        |> List.sort_uniq String.compare
      in
      List.iter
        (fun kind ->
          Alcotest.(check bool)
            ("expression kind " ^ kind)
            true (List.mem kind kinds))
        [
          "integer-literal";
          "float-literal";
          "character-literal";
          "string-literal";
          "parenthesized";
          "prefix";
          "postfix";
          "postfix-cast";
          "binary";
          "index";
          "member";
          "call";
          "top-level-bound-identifier";
          "current-position";
          "sizeof";
          "offset";
          "defined";
        ];
      Alcotest.(check int) "five calls" 5 (List.length calls);
      Alcotest.(check (list string))
        "both call syntaxes remain explicit"
        [ "parenthesis-free"; "parenthesized" ]
        (calls
        |> List.map (fun call ->
            call |> Semantic_top_level_expression_tree.call_source
            |> Semantic_function_call_resolution.call_syntax
            |> Semantic_function_call_resolution.call_syntax_name)
        |> List.sort_uniq String.compare);
      let first_call =
        calls |> List.hd |> Semantic_top_level_expression_tree.call_source
      in
      List.iter
        (fun call ->
          Alcotest.(check string)
            "call result remains linked to its source tree" "call"
            (call |> Semantic_top_level_expression_tree.call_result_expression
           |> Semantic_function_call_resolution.argument_expression_kind
           |> Semantic_function_call_resolution.argument_expression_kind_name))
        calls;
      Alcotest.(check (list string))
        "omitted call hole remains distinct" [ "omitted"; "provided" ]
        (first_call |> Semantic_function_call_resolution.call_arguments
        |> List.map (fun argument ->
            argument |> Semantic_function_call_resolution.argument_kind
            |> Semantic_function_call_resolution.argument_kind_name));
      List.iter
        (fun call ->
          let occurrence =
            Semantic_top_level_expression_tree.call_callee call
          in
          Alcotest.(check string)
            "call retains the exact function binding" "F"
            (Semantic_top_level_outer_expression_binding.occurrence_name
               occurrence);
          match
            Semantic_top_level_outer_expression_binding.occurrence_resolution
              occurrence
          with
          | Semantic_top_level_outer_expression_binding.Module_binding
              publication ->
              Alcotest.(check string)
                "callee publication kind" "function"
                (publication
               |> Semantic_module_expression_binding.publication_kind
               |> Semantic_module_expression_binding.publication_kind_name)
          | Semantic_top_level_outer_expression_binding.Outer_binding _ ->
              Alcotest.fail "expected a module function binding")
        calls)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let generated_outer_binding_keeps_provenance () =
  let prepared =
    prepare ~path:"top-level-tree-generated.HC" "#define USE Outer\n0+USE;"
  in
  let result =
    build prepared Preprocessor.Jit
      [ ("Outer", Semantic_outer_environment.Global_variable) ]
  in
  let node =
    result |> Semantic_top_level_expression_tree.all_expression_nodes
    |> List.find (fun node ->
        match
          node |> Semantic_top_level_expression_tree.expression_node_source
          |> Semantic_function_call_resolution.argument_expression_kind
        with
        | Semantic_function_call_resolution
          .Top_level_bound_identifier_expression
            _ -> true
        | _ -> false)
  in
  match
    node |> Semantic_top_level_expression_tree.expression_node_source
    |> Semantic_function_call_resolution.argument_expression_kind
  with
  | Semantic_function_call_resolution.Top_level_bound_identifier_expression
      identifier -> (
      let occurrence =
        Semantic_function_call_resolution.top_level_bound_identifier_occurrence
          identifier
      in
      (match
         Semantic_top_level_outer_expression_binding.occurrence_resolution
           occurrence
       with
      | Semantic_top_level_outer_expression_binding.Outer_binding binding ->
          Alcotest.(check string)
            "outer table survives in the tree" "jit-task-0"
            (binding |> Semantic_outer_environment.binding_table
           |> Semantic_outer_environment.table_kind
           |> Semantic_outer_environment.table_kind_name)
      | Semantic_top_level_outer_expression_binding.Module_binding _ ->
          Alcotest.fail "expected an outer binding");
      match
        Semantic_top_level_outer_expression_binding.occurrence_origin occurrence
      with
      | Semantic_symbol.Source_location location ->
          Alcotest.(check bool)
            "definition provenance survives" true
            (Option.is_some location.generated_from)
      | _ -> Alcotest.fail "expected generated source provenance")
  | _ -> Alcotest.fail "expected a top-level bound identifier"

let deterministic_pure_and_checked_inputs () =
  let prepared = prepare ~path:"top-level-tree-pure.HC" "I64 value;value+1;" in
  let environment = outer prepared Preprocessor.Jit [] in
  let expressions =
    checked
      (Holyc_lib.resolve_top_level_outer_expressions prepared.session
         ~environment ~expressions:prepared.bindings)
  in
  let table = Session.semantic_symbols prepared.session in
  let before = Semantic_symbol_table.all_symbols table |> List.length in
  let first =
    checked
      (Holyc_lib.build_top_level_expression_trees prepared.session
         ~declarations:prepared.declarations ~compilation_mode:Preprocessor.Jit
         ~expressions prepared.ast)
  in
  let middle = Semantic_symbol_table.all_symbols table |> List.length in
  let second =
    checked
      (Holyc_lib.build_top_level_expression_trees prepared.session
         ~declarations:prepared.declarations ~compilation_mode:Preprocessor.Jit
         ~expressions prepared.ast)
  in
  let after = Semantic_symbol_table.all_symbols table |> List.length in
  Alcotest.(check (pair int int))
    "tree building does not mutate symbols" (before, before) (middle, after);
  Alcotest.(check (list string))
    "repeated tree building is deterministic"
    (first |> Semantic_top_level_expression_tree.all_roots
    |> List.map (fun root ->
        root |> Semantic_top_level_expression_tree.root_role
        |> Semantic_top_level_expression_tree.root_role_name))
    (second |> Semantic_top_level_expression_tree.all_roots
    |> List.map (fun root ->
        root |> Semantic_top_level_expression_tree.root_role
        |> Semantic_top_level_expression_tree.root_role_name));
  (match
     Holyc_lib.build_top_level_expression_trees prepared.session
       ~declarations:prepared.declarations ~compilation_mode:Preprocessor.Aot
       ~expressions prepared.ast
   with
  | Ok _ -> Alcotest.fail "expected a mode mismatch"
  | Error message ->
      Alcotest.(check bool)
        "mode mismatch has a stable code" true
        (String.starts_with ~prefix:"HCSEMA0055:" message));
  let foreign =
    prepare ~path:"top-level-tree-foreign.HC" "I64 value;value+1;"
  in
  match
    Holyc_lib.build_top_level_expression_trees foreign.session
      ~declarations:foreign.declarations ~compilation_mode:Preprocessor.Jit
      ~expressions foreign.ast
  with
  | Ok _ -> Alcotest.fail "expected foreign semantic inputs to fail"
  | Error message ->
      Alcotest.(check bool)
        "foreign input has a stable code" true
        (String.starts_with ~prefix:"HCSEMA0055:" message)

let tests =
  [
    Alcotest.test_case "complete shapes, roles, calls, and identities" `Quick
      complete_shapes_roles_calls_and_identities;
    Alcotest.test_case "generated outer binding provenance" `Quick
      generated_outer_binding_keeps_provenance;
    Alcotest.test_case "determinism, purity, and checked inputs" `Quick
      deterministic_pure_and_checked_inputs;
  ]
