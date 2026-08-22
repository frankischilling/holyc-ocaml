open Holyc_lib

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let checked_result = function
  | Ok value -> value
  | Error error ->
      error |> Semantic_function_call_expression_result.error_to_string
      |> Alcotest.fail

let prepared = Test_function_call_conversion_policy.prepare

let empty_environment (source : Test_function_call_conversion_policy.prepared) =
  let make_table table_kind table_index =
    Semantic_outer_environment.make_table ~table_kind ~table_index []
    |> function
    | Ok table -> table
    | Error error ->
        error |> Semantic_outer_environment.error_to_string |> Alcotest.fail
  in
  let tables =
    match source.mode with
    | Preprocessor.Jit ->
        [
          make_table (Semantic_outer_environment.Jit_task 0) 0;
          make_table Semantic_outer_environment.Assembler 1;
        ]
    | Preprocessor.Aot -> [ make_table Semantic_outer_environment.Assembler 0 ]
  in
  checked
    (Holyc_lib.create_outer_environment source.session
       ~compilation_mode:source.mode tables)

let analyze ?environment
    (source : Test_function_call_conversion_policy.prepared) =
  let environment =
    Option.value environment ~default:(empty_environment source)
  in
  let module_bound =
    checked
      (Holyc_lib.resolve_top_level_expressions source.session
         ~declarations:source.declarations
         ~module_expressions:source.module_expressions source.ast)
  in
  let outer_bound =
    checked
      (Holyc_lib.resolve_top_level_outer_expressions source.session ~environment
         ~expressions:module_bound)
  in
  let expressions =
    checked
      (Holyc_lib.build_top_level_expression_trees source.session
         ~declarations:source.declarations ~compilation_mode:source.mode
         ~expressions:outer_bound source.ast)
  in
  let identifiers =
    checked
      (Holyc_lib.classify_top_level_identifiers source.session
         ~globals:source.global_types ~functions:source.functions ~expressions)
  in
  let policies =
    source |> Test_function_call_conversion_policy.analyze
    |> Test_function_call_conversion_policy.checked_policy
  in
  let result =
    Holyc_lib.type_top_level_expressions source.session ~members:source.members
      ~policies ~identifiers expressions
    |> checked_result
  in
  (expressions, identifiers, policies, result)

let roots result =
  result |> Semantic_function_call_expression_result.top_level_statements
  |> List.concat_map
       Semantic_function_call_expression_result.top_level_statement_roots

let root_values result =
  roots result
  |> List.map Semantic_function_call_expression_result.top_level_root_value

let type_name result =
  match Semantic_function_call_expression_result.result_type result with
  | None -> "unavailable"
  | Some type_ ->
      let base =
        match Semantic_type.base type_ with
        | Semantic_type.Primitive (_, primitive) ->
            Primitive_type.to_string primitive
        | Semantic_type.Aggregate symbol -> Semantic_symbol.name symbol
      in
      base ^ String.make (Semantic_type.pointer_depth type_) '*'

let descriptor result =
  Printf.sprintf "%s:%s:%s:rank-%d" (type_name result)
    (result |> Semantic_function_call_expression_result.result_category
   |> Semantic_function_call_expression_result.value_category_name)
    (result |> Semantic_function_call_expression_result.result_class
   |> Semantic_function_call_expression_result.result_class_name)
    (Semantic_function_call_expression_result.result_array_rank result)

let literals_and_module_values () =
  List.iter
    (fun mode ->
      let source =
        prepared ~mode ~path:"top-level-scalar-results.HC"
          "I64 scalar;F64 floating;I64 array[2];\n\
           I64 (*callback)(I64 value);I64 F();\n\
           1;1.0;'A';\"text\";scalar;floating;array;callback;&F;\n\
           (scalar+1);floating+1;"
      in
      let _, identifiers, policies, result = analyze source in
      Alcotest.(check bool)
        "identifier batch retained" true
        (Semantic_function_call_expression_result.top_level_owns_identifiers
           result identifiers);
      Alcotest.(check bool)
        "conversion policy retained" true
        (Semantic_function_call_expression_result.top_level_owns_policies result
           policies);
      Alcotest.(check (list string))
        "scalar roots use the function expression lattice"
        [
          "I64:object-value:integer-result:rank-0";
          "F64:object-value:f64-result:rank-0";
          "I64:object-value:integer-result:rank-0";
          "U8*:address-value:integer-result:rank-0";
          "I64:object-value:integer-result:rank-0";
          "F64:object-value:f64-result:rank-0";
          "I64:array-value:integer-result:rank-1";
          "I64:callback-value:integer-result:rank-0";
          "I64:address-value:integer-result:rank-0";
          "I64:object-value:integer-result:rank-0";
          "F64:object-value:f64-result:rank-0";
        ]
        (root_values result |> List.map descriptor);
      roots result
      |> List.iter (fun root ->
          let expected =
            match
              root
              |> Semantic_function_call_expression_result.top_level_root_source
              |> Semantic_top_level_expression_tree.root_role
            with
            | Semantic_top_level_expression_tree.Expression_statement _ ->
                Some "ICF_RES_NOT_USED"
            | Semantic_top_level_expression_tree.Implicit_output_fixed _
            | Semantic_top_level_expression_tree.Implicit_output_argument _
            | Semantic_top_level_expression_tree.Condition _
            | Semantic_top_level_expression_tree.Switch_selector _
            | Semantic_top_level_expression_tree.Switch_case_value _
            | Semantic_top_level_expression_tree.Local_array_dimension _
            | Semantic_top_level_expression_tree.Local_initializer _
            | Semantic_top_level_expression_tree.Return_value _ -> None
          in
          Alcotest.(check (option string))
            "result use follows the root role" expected
            (root
           |> Semantic_function_call_expression_result.top_level_root_result_use
            |> Option.map
                 Semantic_function_call_expression_result.result_use_name)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let updates_assignments_and_invalid_lvalues () =
  let source =
    prepared ~path:"top-level-lvalues.HC"
      "I64 scalar;scalar=2;++scalar;scalar++;scalar+=3;"
  in
  let _, _, _, result = analyze source in
  Alcotest.(check (list string))
    "updates and assignments return object values"
    [ "object-value"; "object-value"; "object-value"; "object-value" ]
    (root_values result
    |> List.map (fun value ->
        value |> Semantic_function_call_expression_result.result_category
        |> Semantic_function_call_expression_result.value_category_name));
  let invalid = prepared ~path:"top-level-invalid-lvalue.HC" "1=2;" in
  let environment = empty_environment invalid in
  let module_bound =
    checked
      (Holyc_lib.resolve_top_level_expressions invalid.session
         ~declarations:invalid.declarations
         ~module_expressions:invalid.module_expressions invalid.ast)
  in
  let outer_bound =
    checked
      (Holyc_lib.resolve_top_level_outer_expressions invalid.session
         ~environment ~expressions:module_bound)
  in
  let expressions =
    checked
      (Holyc_lib.build_top_level_expression_trees invalid.session
         ~declarations:invalid.declarations ~compilation_mode:invalid.mode
         ~expressions:outer_bound invalid.ast)
  in
  let identifiers =
    checked
      (Holyc_lib.classify_top_level_identifiers invalid.session
         ~globals:invalid.global_types ~functions:invalid.functions ~expressions)
  in
  let policies =
    invalid |> Test_function_call_conversion_policy.analyze
    |> Test_function_call_conversion_policy.checked_policy
  in
  match
    Holyc_lib.type_top_level_expressions invalid.session
      ~members:invalid.members ~policies ~identifiers expressions
  with
  | Ok _ -> Alcotest.fail "expected a non-lvalue assignment diagnostic"
  | Error error ->
      Alcotest.(check string)
        "shared lvalue diagnostic code" "HCSEMA0046"
        (Semantic_function_call_expression_result.error_code error);
      Alcotest.(check bool)
        "diagnostic explains the rejected destination" true
        (String.ends_with ~suffix:"assignment destination is not an lvalue"
           (Semantic_function_call_expression_result.error_message error))

let indexes_casts_and_conversions () =
  let source =
    prepared ~path:"top-level-conversions.HC"
      "I64 scalar;F64 floating;I64 array[2];\n\
       array[1];scalar(I16);scalar<2;scalar&&1;\n\
       floating=scalar;scalar=floating;"
  in
  let _, _, _, result = analyze source in
  Alcotest.(check (list string))
    "indexes, casts, comparisons, and assignments keep checked result types"
    [
      "I64:object-value:integer-result:rank-0";
      "I16:object-value:integer-result:rank-0";
      "I64:object-value:integer-result:rank-0";
      "I64:object-value:integer-result:rank-0";
      "F64:object-value:f64-result:rank-0";
      "I64:object-value:integer-result:rank-0";
    ]
    (root_values result |> List.map descriptor);
  let all =
    Semantic_function_call_expression_result.top_level_all_results result
  in
  let result_for_source source =
    List.find
      (fun result ->
        Semantic_function_call_expression_result.result_source result == source)
      all
  in
  let assignment_right root =
    let source =
      root |> Semantic_function_call_expression_result.top_level_root_value
      |> Semantic_function_call_expression_result.result_source
    in
    match Semantic_function_call_resolution.argument_expression_kind source with
    | Semantic_function_call_resolution.Binary_expression binary ->
        binary |> Semantic_function_call_resolution.binary_right
        |> result_for_source
    | _ -> Alcotest.fail "expected an assignment expression"
  in
  let assigned = roots result |> List.rev in
  Alcotest.(check (list string))
    "assignment operands retain their target conversions"
    [ "ICF_RES_TO_F64"; "ICF_RES_TO_INT" ]
    ([ List.nth assigned 1; List.nth assigned 0 ]
    |> List.map (fun root ->
        root |> assignment_right
        |> Semantic_function_call_expression_result.result_intrinsic_conversion
        |> Semantic_function_call_expression_result.intrinsic_conversion_name))

let unavailable_boundaries_and_checked_ownership () =
  let source =
    prepared ~path:"top-level-boundaries.HC"
      "class Box{I64 member;};Box box;I64 F(I64 value);box.member;F(1);"
  in
  let expressions, identifiers, policies, result = analyze source in
  Alcotest.(check (list string))
    "member and call roots remain explicit boundaries"
    [ "unavailable"; "unavailable" ]
    (root_values result
    |> List.map (fun value ->
        value |> Semantic_function_call_expression_result.result_category
        |> Semantic_function_call_expression_result.value_category_name));
  let first_ids =
    result |> Semantic_function_call_expression_result.top_level_all_results
    |> List.map (fun value ->
        value |> Semantic_function_call_expression_result.result_id
        |> Semantic_function_call_expression_result.Id.to_int)
  in
  let second =
    Holyc_lib.type_top_level_expressions source.session ~members:source.members
      ~policies ~identifiers expressions
    |> checked_result
  in
  Alcotest.(check (list int))
    "repeated analysis is deterministic" first_ids
    (second |> Semantic_function_call_expression_result.top_level_all_results
    |> List.map (fun value ->
        value |> Semantic_function_call_expression_result.result_id
        |> Semantic_function_call_expression_result.Id.to_int));
  let foreign = Session.create () in
  match
    Holyc_lib.type_top_level_expressions foreign ~members:source.members
      ~policies ~identifiers expressions
  with
  | Ok _ -> Alcotest.fail "expected foreign session ownership to fail"
  | Error error ->
      Alcotest.(check string)
        "top-level ownership diagnostic" "HCSEMA0057"
        (Semantic_function_call_expression_result.error_code error)

let generated_provenance_and_purity () =
  let source =
    prepared ~path:"top-level-generated-results.HC"
      "#define VALUE scalar\nI64 scalar;VALUE+1;"
  in
  let table = Session.semantic_symbols source.session in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  let _, _, _, first = analyze source in
  let _, _, _, second = analyze source in
  Alcotest.(check int)
    "typing does not mutate the symbol table" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length);
  Alcotest.(check (list string))
    "generated results replay deterministically"
    (first |> Semantic_function_call_expression_result.top_level_all_results
   |> List.map descriptor)
    (second |> Semantic_function_call_expression_result.top_level_all_results
   |> List.map descriptor);
  let generated_identifier =
    first |> Semantic_function_call_expression_result.top_level_all_results
    |> List.find (fun result ->
        match
          result |> Semantic_function_call_expression_result.result_source
          |> Semantic_function_call_resolution.argument_expression_kind
        with
        | Semantic_function_call_resolution
          .Top_level_bound_identifier_expression
            _ -> true
        | _ -> false)
  in
  match
    Semantic_function_call_expression_result.result_origin generated_identifier
  with
  | Semantic_symbol.Source_location location ->
      Alcotest.(check bool)
        "definition origin survives result typing" true
        (Option.is_some location.defined_at)
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected a definition-generated source location"

let tests =
  [
    Alcotest.test_case "literals and module values" `Quick
      literals_and_module_values;
    Alcotest.test_case "updates, assignments, and invalid lvalues" `Quick
      updates_assignments_and_invalid_lvalues;
    Alcotest.test_case "indexes, casts, and conversions" `Quick
      indexes_casts_and_conversions;
    Alcotest.test_case "unavailable boundaries and checked ownership" `Quick
      unavailable_boundaries_and_checked_ownership;
    Alcotest.test_case "generated provenance and purity" `Quick
      generated_provenance_and_purity;
  ]
