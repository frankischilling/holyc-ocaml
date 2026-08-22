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
  mode : Preprocessor.compilation_mode;
  session : Session.t;
  ast : Ast.module_;
  declarations : Semantic_declaration_collection.t;
  global_types : Semantic_global_type_resolution.t;
  function_types : Semantic_function_type_resolution.t;
  functions : Semantic_function_resolution.t;
  module_expressions : Semantic_module_expression_binding.t;
}

let prepare ?(mode = Preprocessor.Jit) ~path source_text =
  let session = Session.create () in
  let source = Session.add_source session ~path ~contents:source_text in
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
  let global_records =
    checked
      (Holyc_lib.resolve_global_records session ~declarations
         ~globals:global_types ~compilation_mode:mode ast)
  in
  let module_expressions =
    checked
      (Holyc_lib.resolve_module_expressions session ~declarations ~aggregates
         ~functions ~globals:global_records ~expressions:function_expressions)
  in
  {
    mode;
    session;
    ast;
    declarations;
    global_types;
    function_types;
    functions;
    module_expressions;
  }

let semantic_kind = function
  | Semantic_outer_environment.Aggregate -> Semantic_symbol.Aggregate_type
  | Semantic_outer_environment.Function -> Semantic_symbol.Function
  | Semantic_outer_environment.Global_variable ->
      Semantic_symbol.Global_variable
  | Semantic_outer_environment.Export_system_symbol ->
      Semantic_symbol.Assembler_symbol

let environment prepared records =
  let table = Session.semantic_symbols prepared.session in
  let entries =
    records
    |> List.mapi (fun entry_index (name, record_kind) ->
        let symbol =
          checked
            (Semantic_symbol_table.add table
               ~scope:(Semantic_symbol_table.root table)
               ~name
               ~kind:(semantic_kind record_kind)
               ~origin:(Semantic_symbol.Synthesized ("outer " ^ name)))
        in
        Semantic_outer_environment.make_entry ~symbol ~record_kind ~entry_index
        |> function
        | Ok entry -> entry
        | Error error ->
            error |> Semantic_outer_environment.error_to_string |> Alcotest.fail)
  in
  let table_kind =
    match prepared.mode with
    | Preprocessor.Jit -> Semantic_outer_environment.Jit_task 0
    | Preprocessor.Aot -> Semantic_outer_environment.Assembler
  in
  let tables =
    match prepared.mode with
    | Preprocessor.Jit ->
        let task =
          Semantic_outer_environment.make_table ~table_kind ~table_index:0
            entries
          |> function
          | Ok table -> table
          | Error error ->
              error |> Semantic_outer_environment.error_to_string
              |> Alcotest.fail
        in
        let assembler =
          Semantic_outer_environment.make_table
            ~table_kind:Semantic_outer_environment.Assembler ~table_index:1 []
          |> function
          | Ok table -> table
          | Error error ->
              error |> Semantic_outer_environment.error_to_string
              |> Alcotest.fail
        in
        [ task; assembler ]
    | Preprocessor.Aot ->
        [
          ( Semantic_outer_environment.make_table ~table_kind ~table_index:0
              entries
          |> function
            | Ok table -> table
            | Error error ->
                error |> Semantic_outer_environment.error_to_string
                |> Alcotest.fail );
        ]
  in
  checked
    (Holyc_lib.create_outer_environment prepared.session
       ~compilation_mode:prepared.mode tables)

let tree prepared records =
  let environment = environment prepared records in
  let module_bound =
    checked
      (Holyc_lib.resolve_top_level_expressions prepared.session
         ~declarations:prepared.declarations
         ~module_expressions:prepared.module_expressions prepared.ast)
  in
  let outer_bound =
    checked
      (Holyc_lib.resolve_top_level_outer_expressions prepared.session
         ~environment ~expressions:module_bound)
  in
  checked
    (Holyc_lib.build_top_level_expression_trees prepared.session
       ~declarations:prepared.declarations ~compilation_mode:prepared.mode
       ~expressions:outer_bound prepared.ast)

let classify prepared expressions =
  Holyc_lib.classify_top_level_identifiers prepared.session
    ~globals:prepared.global_types ~functions:prepared.functions ~expressions

let leaf_name leaf =
  leaf |> Semantic_top_level_identifier_resolution.leaf_occurrence
  |> Semantic_top_level_outer_expression_binding.occurrence_name

let leaf_named result name =
  result |> Semantic_top_level_identifier_resolution.leaves
  |> List.find (fun leaf -> String.equal (leaf_name leaf) name)

let module_value leaf =
  match Semantic_top_level_identifier_resolution.leaf_resolution leaf with
  | Semantic_top_level_identifier_resolution.Module_value value -> value
  | Semantic_top_level_identifier_resolution.Outer_type_required _ ->
      Alcotest.fail "expected a source-typed module value"

let module_types_and_value_shapes () =
  let source =
    "I64 F(I64 value=1);I64 scalar;scalar;I64 scalar;I64 array[3];\n\
     I64 (*callback)(I64 value);I64 (*callbacks)()[2];\n\
     class Box{I64 member;};\n\
     scalar;array;callback;callbacks;F;0+Box.member;(*F)(scalar);F;"
  in
  List.iter
    (fun mode ->
      let prepared = prepare ~mode ~path:"top-level-identifiers.HC" source in
      let expressions = tree prepared [] in
      let result = classify prepared expressions |> checked in
      let scalar = leaf_named result "scalar" |> module_value in
      let array = leaf_named result "array" |> module_value in
      let callback = leaf_named result "callback" |> module_value in
      let callbacks = leaf_named result "callbacks" |> module_value in
      let function_ = leaf_named result "F" |> module_value in
      let aggregate = leaf_named result "Box" |> module_value in
      let shape value =
        value |> Semantic_top_level_identifier_resolution.module_value_shape
        |> Option.map
             Semantic_function_call_resolution.identifier_value_shape_name
      in
      Alcotest.(check (option string))
        "ordinary global" (Some "object") (shape scalar);
      (match
         Semantic_top_level_identifier_resolution.module_value_type scalar
       with
      | Some type_ -> (
          Alcotest.(check int)
            "ordinary global pointer depth" 0
            (Semantic_type.pointer_depth type_);
          match Semantic_type.base type_ with
          | Semantic_type.Primitive
              (Semantic_type.Public_spelling, Primitive_type.I64) -> ()
          | Semantic_type.Primitive _ | Semantic_type.Aggregate _ ->
              Alcotest.fail "expected the checked public I64 type")
      | None -> Alcotest.fail "ordinary global has no checked type");
      Alcotest.(check (option string))
        "array global" (Some "array") (shape array);
      Alcotest.(check (option int))
        "array rank" (Some 1)
        (Semantic_top_level_identifier_resolution.module_value_array_rank array);
      Alcotest.(check (option string))
        "callback global" (Some "function-pointer") (shape callback);
      Alcotest.(check (option string))
        "callback array" (Some "array") (shape callbacks);
      Alcotest.(check (option int))
        "callback array rank" (Some 1)
        (Semantic_top_level_identifier_resolution.module_value_array_rank
           callbacks);
      Alcotest.(check (option string))
        "direct function" (Some "direct-function") (shape function_);
      (match function_ with
      | Semantic_top_level_identifier_resolution.Direct_function_value
          { declaration; _ } ->
          Alcotest.(check string)
            "direct function identity" "F"
            (declaration
           |> Semantic_function_resolution.resolved_declaration_identity_symbol
           |> Semantic_symbol.name)
      | Semantic_top_level_identifier_resolution.Global_value _
      | Semantic_top_level_identifier_resolution.Aggregate_offset_base _ ->
          Alcotest.fail "expected a direct function value");
      Alcotest.(check (option string))
        "aggregate is not a runtime value" None (shape aggregate);
      let scalar_symbols =
        result |> Semantic_top_level_identifier_resolution.leaves
        |> List.filter (fun leaf -> String.equal (leaf_name leaf) "scalar")
        |> List.map (fun leaf ->
            match module_value leaf with
            | Semantic_top_level_identifier_resolution.Global_value
                { global; _ } ->
                global |> Semantic_global_type_resolution.global_symbol
                |> Semantic_symbol.id |> Semantic_symbol.Id.to_int
            | Semantic_top_level_identifier_resolution.Direct_function_value _
            | Semantic_top_level_identifier_resolution.Aggregate_offset_base _
              -> Alcotest.fail "expected a scalar global")
      in
      Alcotest.(check int)
        "redeclaration selects two source records" 2
        (scalar_symbols |> List.sort_uniq Int.compare |> List.length);
      Alcotest.(check (list string))
        "all identifier leaves are classified"
        [
          "scalar";
          "scalar";
          "array";
          "callback";
          "callbacks";
          "F";
          "Box";
          "F";
          "F";
        ]
        (result |> Semantic_top_level_identifier_resolution.leaves
       |> List.map leaf_name))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let outer_records_require_typed_metadata () =
  let records =
    [
      ("OuterClass", Semantic_outer_environment.Aggregate);
      ("OuterFun", Semantic_outer_environment.Function);
      ("OuterGlobal", Semantic_outer_environment.Global_variable);
      ("OuterExport", Semantic_outer_environment.Export_system_symbol);
    ]
  in
  let prepared =
    prepare ~path:"top-level-outer-identifiers.HC"
      "0+OuterClass;0+OuterFun;0+OuterGlobal;0+OuterExport;"
  in
  let result = tree prepared records |> classify prepared |> checked in
  let leaves = Semantic_top_level_identifier_resolution.leaves result in
  Alcotest.(check int) "four outer leaves" 4 (List.length leaves);
  List.iter
    (fun leaf ->
      match Semantic_top_level_identifier_resolution.leaf_resolution leaf with
      | Semantic_top_level_identifier_resolution.Outer_type_required binding ->
          Alcotest.(check string)
            "outer binding identity survives" (leaf_name leaf)
            (binding |> Semantic_outer_environment.binding_entry
           |> Semantic_outer_environment.entry_symbol |> Semantic_symbol.name)
      | Semantic_top_level_identifier_resolution.Module_value _ ->
          Alcotest.fail "outer records must not receive guessed module types")
    leaves

let generated_provenance_and_checked_inputs () =
  let prepared =
    prepare ~path:"top-level-generated-identifier.HC"
      "#define NAME value\nI64 NAME;NAME;"
  in
  let expressions = tree prepared [] in
  let table = Session.semantic_symbols prepared.session in
  let before = Semantic_symbol_table.all_symbols table |> List.length in
  let first = classify prepared expressions |> checked in
  let second = classify prepared expressions |> checked in
  let after = Semantic_symbol_table.all_symbols table |> List.length in
  Alcotest.(check (pair int int))
    "classification is read-only" (before, before) (before, after);
  Alcotest.(check int)
    "deterministic leaf count"
    (first |> Semantic_top_level_identifier_resolution.leaves |> List.length)
    (second |> Semantic_top_level_identifier_resolution.leaves |> List.length);
  let occurrence =
    first |> Semantic_top_level_identifier_resolution.leaves |> List.hd
    |> Semantic_top_level_identifier_resolution.leaf_occurrence
  in
  (match
     Semantic_top_level_outer_expression_binding.occurrence_origin occurrence
   with
  | Semantic_symbol.Source_location location ->
      Alcotest.(check bool)
        "definition provenance survives" true
        (Option.is_some location.defined_at)
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected source provenance");
  let alternate_functions =
    checked
      (Holyc_lib.resolve_function_identities prepared.session
         ~declarations:prepared.declarations ~functions:prepared.function_types
         ~compilation_mode:Preprocessor.Aot prepared.ast)
  in
  (match
     Holyc_lib.classify_top_level_identifiers prepared.session
       ~globals:prepared.global_types ~functions:alternate_functions
       ~expressions
   with
  | Ok _ -> Alcotest.fail "expected a compilation-mode mismatch"
  | Error message ->
      Alcotest.(check bool)
        "mode mismatch has a stable code" true
        (String.starts_with ~prefix:"HCSEMA0056:" message));
  let foreign =
    prepare ~path:"top-level-foreign-identifier.HC" "I64 other;other;"
  in
  let foreign_expressions = tree foreign [] in
  (match
     Holyc_lib.classify_top_level_identifiers prepared.session
       ~globals:foreign.global_types ~functions:foreign.functions
       ~expressions:foreign_expressions
   with
  | Ok _ -> Alcotest.fail "expected foreign semantic ownership to fail"
  | Error message ->
      Alcotest.(check bool)
        "foreign ownership has a stable code" true
        (String.starts_with ~prefix:"HCSEMA0056:" message));
  let empty_globals =
    checked
      (Semantic_global_type_resolution.resolve ~table
         ~parent:(Semantic_declaration_collection.scope prepared.declarations)
         [])
  in
  match
    Holyc_lib.classify_top_level_identifiers prepared.session
      ~globals:empty_globals ~functions:prepared.functions ~expressions
  with
  | Ok _ -> Alcotest.fail "expected a stale global-type batch"
  | Error message ->
      Alcotest.(check bool)
        "stale batch has a stable code" true
        (String.starts_with ~prefix:"HCSEMA0056:" message)

let tests =
  [
    Alcotest.test_case "module types and value shapes" `Quick
      module_types_and_value_shapes;
    Alcotest.test_case "outer records require typed metadata" `Quick
      outer_records_require_typed_metadata;
    Alcotest.test_case "generated provenance and checked inputs" `Quick
      generated_provenance_and_checked_inputs;
  ]
