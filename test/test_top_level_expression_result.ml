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

let build_inputs ?environment
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
  (expressions, identifiers)

let analyze ?environment
    (source : Test_function_call_conversion_policy.prepared) =
  let expressions, identifiers = build_inputs ?environment source in
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

let policies_for_mode (source : Test_function_call_conversion_policy.prepared)
    mode =
  let functions =
    checked
      (Holyc_lib.resolve_function_identities source.session
         ~declarations:source.declarations ~functions:source.function_types
         ~compilation_mode:mode source.ast)
  in
  let globals =
    checked
      (Holyc_lib.resolve_global_records source.session
         ~declarations:source.declarations ~globals:source.global_types
         ~compilation_mode:mode source.ast)
  in
  let module_expressions =
    checked
      (Holyc_lib.resolve_module_expressions source.session
         ~declarations:source.declarations ~aggregates:source.aggregates
         ~functions ~globals ~expressions:source.expressions)
  in
  let calls =
    checked
      (Holyc_lib.resolve_function_calls source.session
         ~declarations:source.declarations ~function_types:source.function_types
         ~members:source.members ~local_types:source.local_types
         ~global_types:source.global_types ~functions
         ~expressions:module_expressions source.ast)
  in
  Holyc_lib.analyze_function_call_conversions source.session
    ~declarations:source.declarations ~headers:source.headers ~calls
  |> Test_function_call_conversion_policy.checked_policy

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

let lookup_description result =
  match
    Semantic_function_call_expression_result.result_member_lookup result
  with
  | None -> Alcotest.fail "expected a resolved aggregate member lookup"
  | Some lookup ->
      let member = Semantic_aggregate_member_index.lookup_member lookup in
      let layout = Semantic_aggregate_member_index.member_layout member in
      Printf.sprintf "%s:%s:depth-%d:offset-%Ld"
        (member |> Semantic_aggregate_member_index.member_symbol
       |> Semantic_symbol.name)
        (lookup |> Semantic_aggregate_member_index.lookup_declaring_aggregate
       |> Semantic_symbol.name)
        (Semantic_aggregate_member_index.lookup_inheritance_depth lookup)
        layout.offset

let offset_description result =
  match
    Semantic_function_call_expression_result.result_aggregate_offset_path result
  with
  | None -> Alcotest.fail "expected an aggregate offset path"
  | Some path ->
      let base =
        path |> Semantic_function_call_expression_result.aggregate_offset_base
        |> Semantic_module_expression_binding.publication_canonical_symbol
        |> Semantic_symbol.name
      in
      let segments =
        path
        |> Semantic_function_call_expression_result.aggregate_offset_segments
        |> List.map (fun segment ->
            let lookup =
              Semantic_function_call_expression_result
              .aggregate_offset_segment_lookup segment
            in
            let name =
              lookup |> Semantic_aggregate_member_index.lookup_member
              |> Semantic_aggregate_member_index.member_symbol
              |> Semantic_symbol.name
            in
            Printf.sprintf "%s:%Ld" name
              (Semantic_function_call_expression_result
               .aggregate_offset_segment_cumulative_offset segment))
      in
      Printf.sprintf "%s[%s]=%Ld" base
        (String.concat "/" segments)
        (Semantic_function_call_expression_result.aggregate_offset_value path)

let completed_offsets result =
  result |> Semantic_function_call_expression_result.top_level_all_results
  |> List.filter (fun value ->
      match
        Semantic_function_call_expression_result.result_aggregate_offset_path
          value
      with
      | Some path ->
          path
          |> Semantic_function_call_expression_result.aggregate_offset_segments
          <> []
      | None -> false)

let top_level_call_name call =
  call |> Semantic_function_call_expression_result.top_level_direct_source
  |> Semantic_top_level_expression_tree.call_source
  |> Semantic_function_call_resolution.call_callee_name

let top_level_global_callback_name call =
  call
  |> Semantic_function_call_expression_result.top_level_global_callback_source
  |> Semantic_top_level_expression_tree.call_source
  |> Semantic_function_call_resolution.call_callee_name

let top_level_indexed_global_callback_name call =
  call
  |> Semantic_function_call_expression_result
     .top_level_indexed_global_callback_source
  |> Semantic_top_level_expression_tree.call_source
  |> Semantic_function_call_resolution.call_callee_name

let top_level_member_callback_name call =
  call
  |> Semantic_function_call_expression_result.top_level_member_callback_source
  |> Semantic_top_level_expression_tree.call_source
  |> Semantic_function_call_resolution.call_callee_name

let top_level_fixed_description fixed =
  let target =
    fixed
    |> Semantic_function_call_expression_result.top_level_fixed_target_class
    |> Semantic_function_call_expression_result.result_class_name
  in
  let path =
    match
      Semantic_function_call_expression_result.top_level_fixed_path fixed
    with
    | Semantic_function_call_expression_result.Provided_result value ->
        "provided:" ^ descriptor value
    | Semantic_function_call_expression_result.Declared_default_result value ->
        Printf.sprintf "default:%s:%s:%s"
          ( value
            |> Semantic_function_call_expression_result.declared_default_type
            |> Semantic_type.base
          |> function
            | Semantic_type.Primitive (_, primitive) ->
                Primitive_type.to_string primitive
            | Semantic_type.Aggregate symbol -> Semantic_symbol.name symbol )
          (value
         |> Semantic_function_call_expression_result.declared_default_class
         |> Semantic_function_call_expression_result.result_class_name)
          (value
         |> Semantic_function_call_expression_result
            .declared_default_materialization
         |> Semantic_function_call_expression_result
            .declared_default_materialization_name)
  in
  target ^ ":" ^ path

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

let aggregate_member_paths () =
  List.iter
    (fun mode ->
      let source =
        prepared ~mode ~path:"top-level-member-paths.HC"
          "F64 class FloatBox {};\n\
          \           class Base {I8 inherited;};\n\
          \           class Box : Base {I64 matrix[2][3];F64 \
           (*callback)(I64);FloatBox floating;I64 value;};\n\
          \           Box box;Box *pointer;\n\
          \           \
           box.inherited;box.matrix;box.matrix[0];box.callback;box.floating;\n\
          \           pointer->value;++box.value;box.value=3;"
      in
      let _, _, _, result = analyze source in
      let values = root_values result in
      Alcotest.(check (list string))
        "member paths retain scalar, array, callback, and backing classes"
        [
          "I8:object-value:integer-result:rank-0";
          "I64:array-value:integer-result:rank-2";
          "I64:array-value:integer-result:rank-1";
          "F64:callback-value:integer-result:rank-0";
          "FloatBox:object-value:f64-result:rank-0";
          "I64:object-value:integer-result:rank-0";
          "I64:object-value:integer-result:rank-0";
          "I64:object-value:integer-result:rank-0";
        ]
        (List.map descriptor values);
      Alcotest.(check string)
        "inherited lookup retains its declaring aggregate and depth"
        "inherited:Base:depth-1:offset-0"
        (List.hd values |> lookup_description);
      Alcotest.(check string)
        "pointer lookup retains the direct member layout"
        "value:Box:depth-0:offset-57"
        (List.nth values 5 |> lookup_description))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let invalid_aggregate_member_paths () =
  [
    ( "scalar",
      "I64 value;value.missing;",
      "member access base is not an aggregate" );
    ( "direct pointer",
      "class Box {I64 value;};Box *box;box.value;",
      "direct member access requires an aggregate object, not a pointer" );
    ( "pointer object",
      "class Box {I64 value;};Box box;box->value;",
      "pointer member access requires a pointer to an aggregate" );
    ( "missing member",
      "class Box {I64 value;};Box box;box.missing;",
      "aggregate `Box` has no member `missing`" );
    ( "incomplete aggregate",
      "extern class Later;Later *later;later->value;class Later {I64 value;};",
      "aggregate `Later` is not complete before this member access" );
  ]
  |> List.iter (fun (label, contents, expected_message) ->
      let source =
        prepared ~path:("top-level-invalid-member-" ^ label ^ ".HC") contents
      in
      let expressions, identifiers = build_inputs source in
      let policies =
        source |> Test_function_call_conversion_policy.analyze
        |> Test_function_call_conversion_policy.checked_policy
      in
      match
        Holyc_lib.type_top_level_expressions source.session
          ~members:source.members ~policies ~identifiers expressions
      with
      | Ok _ -> Alcotest.failf "expected %s member path to fail" label
      | Error error -> (
          Alcotest.(check string)
            (label ^ " code") "HCSEMA0046"
            (Semantic_function_call_expression_result.error_code error);
          Alcotest.(check string)
            (label ^ " message") expected_message
            (Semantic_function_call_expression_result.error_message error);
          match Semantic_function_call_expression_result.error_origin error with
          | Some (Semantic_symbol.Source_location _) -> ()
          | Some
              (Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _)
          | None -> Alcotest.fail "expected a member source location"))

let aggregate_offset_paths () =
  List.iter
    (fun mode ->
      let source =
        prepared ~mode ~path:"top-level-offset-paths.HC"
          "F64 class FloatBox {};\n\
          \           class Inner {I8 head;I64 value;};\n\
          \           class Base {I8 inherited;};\n\
          \           class Box : Base {I16 prefix;Inner inner;FloatBox \
           floating;};\n\
          \           \
           0+Box.prefix;0+Box.inner.value;0+Box.inherited;0+Box.floating;\n\
          \           0+Box.inner.value;"
      in
      let _, _, _, result = analyze source in
      let values = root_values result in
      Alcotest.(check (list string))
        "offset paths yield integers before ordinary expression consumers"
        [
          "I64:object-value:integer-result:rank-0";
          "I64:object-value:integer-result:rank-0";
          "I64:object-value:integer-result:rank-0";
          "I64:object-value:integer-result:rank-0";
          "I64:object-value:integer-result:rank-0";
        ]
        (List.map descriptor values);
      let offsets = completed_offsets result in
      Alcotest.(check (list string))
        "offset paths retain ordered segments and cumulative values"
        [
          "Box[prefix:1]=1";
          "Box[inner:3/value:4]=4";
          "Box[inner:3]=3";
          "Box[inherited:0]=0";
          "Box[floating:12]=12";
          "Box[inner:3/value:4]=4";
          "Box[inner:3]=3";
        ]
        (List.map offset_description offsets);
      let float_path =
        List.nth offsets 4
        |> Semantic_function_call_expression_result.result_aggregate_offset_path
        |> Option.get
      in
      Alcotest.(check string)
        "a backed member keeps its path type without changing the I64 result"
        "FloatBox"
        ( float_path
          |> Semantic_function_call_expression_result
             .aggregate_offset_current_type |> Semantic_type.base
        |> function
          | Semantic_type.Aggregate symbol -> Semantic_symbol.name symbol
          | Semantic_type.Primitive _ -> "primitive" ))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let invalid_aggregate_offset_paths () =
  [
    ( "bare aggregate",
      "class Box {I64 value;};0+Box;",
      "aggregate offset base requires a member path" );
    ( "pointer syntax",
      "class Box {I64 value;};0+Box->value;",
      "aggregate offset paths require direct member access" );
    ( "missing member",
      "class Box {I64 value;};0+Box.missing;",
      "aggregate `Box` has no member `missing`" );
    ( "incomplete aggregate",
      "extern class Later;0+Later.value;class Later {I64 value;};",
      "aggregate `Later` is not complete before this member access" );
    ( "nonaggregate continuation",
      "class Box {I64 value;};0+Box.value.missing;",
      "member access base is not an aggregate" );
  ]
  |> List.iter (fun (label, contents, expected_message) ->
      let source =
        prepared ~path:("top-level-invalid-offset-" ^ label ^ ".HC") contents
      in
      let expressions, identifiers = build_inputs source in
      let policies =
        source |> Test_function_call_conversion_policy.analyze
        |> Test_function_call_conversion_policy.checked_policy
      in
      match
        Holyc_lib.type_top_level_expressions source.session
          ~members:source.members ~policies ~identifiers expressions
      with
      | Ok _ -> Alcotest.failf "expected %s offset path to fail" label
      | Error error -> (
          Alcotest.(check string)
            (label ^ " code") "HCSEMA0046"
            (Semantic_function_call_expression_result.error_code error);
          Alcotest.(check string)
            (label ^ " message") expected_message
            (Semantic_function_call_expression_result.error_message error);
          match Semantic_function_call_expression_result.error_origin error with
          | Some (Semantic_symbol.Source_location _) -> ()
          | Some
              (Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _)
          | None -> Alcotest.fail "expected an offset-path source location"))

let top_level_direct_calls () =
  List.iter
    (fun mode ->
      let source =
        prepared ~mode ~path:"top-level-direct-calls.HC"
          "F64 class FloatBox {};\n\
          \           I64 Int(I64 left,I64 middle=2,I64 right);\n\
          \           F64 Float();I64 *Pointer();FloatBox Make();\n\
          \           I64 Variadic(I64 first,...);\n\
          \           Int(1,,3);Float;Pointer();Make();Variadic(1,2,3);\n\
          \           Int(Float(),,3);"
      in
      let _, _, _, result = analyze source in
      Alcotest.(check (list string))
        "direct call roots keep their source-visible return types"
        [
          "I64:object-value:integer-result:rank-0";
          "F64:object-value:f64-result:rank-0";
          "I64*:address-value:integer-result:rank-0";
          "FloatBox:object-value:f64-result:rank-0";
          "I64:object-value:integer-result:rank-0";
          "I64:object-value:integer-result:rank-0";
        ]
        (root_values result |> List.map descriptor);
      let calls =
        Semantic_function_call_expression_result.top_level_direct_calls result
      in
      Alcotest.(check (list string))
        "direct and nested calls retain source order"
        [ "Int"; "Float"; "Pointer"; "Make"; "Variadic"; "Int"; "Float" ]
        (List.map top_level_call_name calls);
      let first = List.hd calls in
      Alcotest.(check (list string))
        "non-trailing defaults stay distinct from provided arguments"
        [
          "integer-result:provided:I64:object-value:integer-result:rank-0";
          "integer-result:default:I64:integer-result:immediate";
          "integer-result:provided:I64:object-value:integer-result:rank-0";
        ]
        (first
       |> Semantic_function_call_expression_result
          .top_level_direct_fixed_results
        |> List.map top_level_fixed_description);
      let variadic = List.nth calls 4 in
      Alcotest.(check int64)
        "variadic count uses target I64" 2L
        (Semantic_function_call_expression_result
         .top_level_direct_variadic_count variadic);
      Alcotest.(check (list string))
        "variadic expressions retain source order and actual classes"
        [
          "I64:object-value:integer-result:rank-0";
          "I64:object-value:integer-result:rank-0";
        ]
        (variadic
       |> Semantic_function_call_expression_result
          .top_level_direct_variadic_results |> List.map descriptor);
      let outer = List.nth calls 5 in
      let nested_first =
        outer
        |> Semantic_function_call_expression_result
           .top_level_direct_fixed_results |> List.hd
      in
      Alcotest.(check string)
        "nested F64 result remains distinct from its integer target"
        "integer-result:provided:F64:object-value:f64-result:rank-0"
        (top_level_fixed_description nested_first);
      let all =
        Semantic_function_call_expression_result.top_level_all_results result
      in
      List.iter
        (fun call ->
          let id =
            Semantic_function_call_expression_result.top_level_direct_result_id
              call
          in
          Alcotest.(check int)
            "each direct call links to one expression result" 1
            (List.length
               (List.filter
                  (fun value ->
                    Semantic_function_call_expression_result.Id.equal id
                      (Semantic_function_call_expression_result.result_id value))
                  all)))
        calls)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let top_level_direct_call_replacement_headers () =
  List.iter
    (fun mode ->
      let source =
        prepared ~mode ~path:"top-level-call-replacements.HC"
          "I64 F(I64 value=1);F();F64 F(F64 value=1.0);F();"
      in
      let _, _, _, result = analyze source in
      Alcotest.(check (list string))
        "each call uses the header visible at its statement"
        [
          "I64:object-value:integer-result:rank-0";
          "F64:object-value:f64-result:rank-0";
        ]
        (root_values result |> List.map descriptor);
      Alcotest.(check (list int))
        "replacement headers retain distinct source item indexes" [ 0; 2 ]
        (result
       |> Semantic_function_call_expression_result.top_level_direct_calls
        |> List.map (fun call ->
            call
            |> Semantic_function_call_expression_result.top_level_direct_header
            |> Semantic_function_type_resolution.function_item_index)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let included_top_level_calls () =
  let include_path =
    if Sys.file_exists "test/fixtures/top-level-direct-call-header.HH" then
      "test/fixtures/top-level-direct-call-header.HH"
    else "fixtures/top-level-direct-call-header.HH"
  in
  let source =
    prepared ~path:"top-level-direct-call-use.HC"
      (Printf.sprintf
         "#include \"%s\"\n\
          IncludedCall(1);IncludedCallback(2);IncludedCallbacks[0](3);IncludedObject.Member(4);"
         include_path)
  in
  let _, _, _, result = analyze source in
  let calls =
    Semantic_function_call_expression_result.top_level_direct_calls result
  in
  Alcotest.(check (list string))
    "an included header supplies the exact visible call declaration"
    [ "IncludedCall" ]
    (List.map top_level_call_name calls);
  let call = List.hd calls in
  let header =
    Semantic_function_call_expression_result.top_level_direct_header call
  in
  (match Semantic_function_type_resolution.function_return_type header with
  | type_reference -> (
      match Semantic_type_reference.spelling_origin type_reference with
      | Semantic_symbol.Source_location location ->
          Alcotest.(check bool)
            "included call header keeps an include backtrace" true
            (location.source_segments <> [])
      | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
          Alcotest.fail "expected an included call header source location"));
  let callback_calls =
    Semantic_function_call_expression_result.top_level_global_callback_calls
      result
  in
  Alcotest.(check (list string))
    "an included callback keeps its source-visible global"
    [ "IncludedCallback" ]
    (List.map top_level_global_callback_name callback_calls);
  let callback_global =
    callback_calls |> List.hd
    |> Semantic_function_call_expression_result.top_level_global_callback_global
  in
  (match
     Semantic_global_type_resolution.global_declarator_origin callback_global
   with
  | Semantic_symbol.Source_location location ->
      Alcotest.(check bool)
        "included callback keeps an include backtrace" true
        (location.source_segments <> [])
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected an included callback source location");
  let indexed_calls =
    Semantic_function_call_expression_result
    .top_level_indexed_global_callback_calls result
  in
  Alcotest.(check (list string))
    "an included callback array keeps its source-visible global"
    [ "IncludedCallbacks" ]
    (List.map top_level_indexed_global_callback_name indexed_calls);
  let indexed_global =
    indexed_calls |> List.hd
    |> Semantic_function_call_expression_result
       .top_level_indexed_global_callback_global
  in
  (match
     Semantic_global_type_resolution.global_declarator_origin indexed_global
   with
  | Semantic_symbol.Source_location location ->
      Alcotest.(check bool)
        "included callback array keeps an include backtrace" true
        (location.source_segments <> [])
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected an included callback-array source location");
  let member_calls =
    Semantic_function_call_expression_result.top_level_member_callback_calls
      result
  in
  Alcotest.(check (list string))
    "an included callback member keeps its source-visible global"
    [ "IncludedObject" ]
    (List.map top_level_member_callback_name member_calls);
  let member =
    member_calls |> List.hd
    |> Semantic_function_call_expression_result.top_level_member_callback_lookup
    |> Semantic_aggregate_member_index.lookup_member
    |> Semantic_aggregate_member_index.member_symbol
  in
  match Semantic_symbol.origin member with
  | Semantic_symbol.Source_location location ->
      Alcotest.(check bool)
        "included callback member keeps an include backtrace" true
        (location.source_segments <> [])
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected an included callback-member source location"

let invalid_top_level_direct_calls () =
  [
    ( "missing",
      "I64 F(I64 value);F();",
      "call to \"F\" is missing required argument 1 (value)" );
    ( "extra",
      "I64 F(I64 value);F(1,2);",
      "call to \"F\" provides argument 2, but its active header has 1 fixed \
       parameter" );
    ( "omitted variadic",
      "I64 F(I64 value,...);F(1,,2);",
      "call to \"F\" omits variadic argument 2; variadic positions require an \
       expression" );
  ]
  |> List.iter (fun (label, contents, expected_message) ->
      let source =
        prepared ~path:("top-level-invalid-call-" ^ label ^ ".HC") contents
      in
      let expressions, identifiers = build_inputs source in
      let policies =
        source |> Test_function_call_conversion_policy.analyze
        |> Test_function_call_conversion_policy.checked_policy
      in
      match
        Holyc_lib.type_top_level_expressions source.session
          ~members:source.members ~policies ~identifiers expressions
      with
      | Ok _ -> Alcotest.failf "expected %s direct call to fail" label
      | Error error ->
          Alcotest.(check string)
            (label ^ " code") "HCSEMA0057"
            (Semantic_function_call_expression_result.error_code error);
          Alcotest.(check string)
            (label ^ " message") expected_message
            (Semantic_function_call_expression_result.error_message error))

let top_level_global_callback_calls () =
  List.iter
    (fun mode ->
      let source =
        prepared ~mode ~path:"top-level-global-callbacks.HC"
          "F64 class CallbackBacked {};\n\
           I64 (*CbInt)(I64 first,I64 second=2,I64 third);\n\
           F64 (*CbFloat)();I64 *(*CbPointer)();\n\
           CallbackBacked (*CbMake)();I64 (*CbVariadic)(I64 first,...);\n\
           CbInt(1,,3);CbFloat();CbPointer();CbMake();\n\
           CbVariadic(1,2,3);CbInt(CbFloat(),,3);"
      in
      let _, _, _, result = analyze source in
      Alcotest.(check (list string))
        "global callback returns use their stored signatures"
        [
          "I64:object-value:integer-result:rank-0";
          "F64:object-value:f64-result:rank-0";
          "I64*:address-value:integer-result:rank-0";
          "CallbackBacked:object-value:f64-result:rank-0";
          "I64:object-value:integer-result:rank-0";
          "I64:object-value:integer-result:rank-0";
        ]
        (root_values result |> List.map descriptor);
      let calls =
        Semantic_function_call_expression_result.top_level_global_callback_calls
          result
      in
      Alcotest.(check (list string))
        "callback calls retain source order, including nested calls"
        [
          "CbInt";
          "CbFloat";
          "CbPointer";
          "CbMake";
          "CbVariadic";
          "CbInt";
          "CbFloat";
        ]
        (List.map top_level_global_callback_name calls);
      let first = List.hd calls in
      Alcotest.(check string)
        "callback call keeps its scalar global value shape" "function-pointer"
        (first
       |> Semantic_function_call_expression_result
          .top_level_global_callback_value
       |> Semantic_function_call_resolution.identifier_value_shape
       |> Semantic_function_call_resolution.identifier_value_shape_name);
      Alcotest.(check int)
        "callback call keeps its stored fixed signature" 3
        (first
       |> Semantic_function_call_expression_result
          .top_level_global_callback_callable
       |> Semantic_function_call_resolution.callable_signature
       |> Semantic_function_type_resolution.signature_parameters |> List.length
        );
      Alcotest.(check (list string))
        "callback defaults stay separate from provided arguments"
        [
          "integer-result:provided:I64:object-value:integer-result:rank-0";
          "integer-result:default:I64:integer-result:immediate";
          "integer-result:provided:I64:object-value:integer-result:rank-0";
        ]
        (first
       |> Semantic_function_call_expression_result
          .top_level_global_callback_fixed_results
        |> List.map top_level_fixed_description);
      let variadic = List.nth calls 4 in
      Alcotest.(check int64)
        "callback variadic count uses target I64" 2L
        (Semantic_function_call_expression_result
         .top_level_global_callback_variadic_count variadic);
      Alcotest.(check string)
        "callback publication and result keep one global identity" "CbInt"
        (first
       |> Semantic_function_call_expression_result
          .top_level_global_callback_global
       |> Semantic_global_type_resolution.global_symbol |> Semantic_symbol.name
        );
      let all =
        Semantic_function_call_expression_result.top_level_all_results result
      in
      List.iter
        (fun call ->
          let id =
            Semantic_function_call_expression_result
            .top_level_global_callback_result_id call
          in
          Alcotest.(check int)
            "each callback call links to one expression result" 1
            (List.length
               (List.filter
                  (fun value ->
                    Semantic_function_call_expression_result.Id.equal id
                      (Semantic_function_call_expression_result.result_id value))
                  all)))
        calls)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let invalid_top_level_global_callback_calls () =
  [
    ( "missing",
      "I64 (*Callback)(I64 value);Callback();",
      "call to \"Callback\" is missing required argument 1 (value)" );
    ( "extra",
      "I64 (*Callback)(I64 value);Callback(1,2);",
      "call to \"Callback\" provides argument 2, but its active header has 1 \
       fixed parameter" );
    ( "omitted variadic",
      "I64 (*Callback)(I64 value,...);Callback(1,,2);",
      "call to \"Callback\" omits variadic argument 2; variadic positions \
       require an expression" );
  ]
  |> List.iter (fun (label, contents, expected_message) ->
      let source =
        prepared ~path:("top-level-invalid-callback-" ^ label ^ ".HC") contents
      in
      let expressions, identifiers = build_inputs source in
      let policies =
        source |> Test_function_call_conversion_policy.analyze
        |> Test_function_call_conversion_policy.checked_policy
      in
      match
        Holyc_lib.type_top_level_expressions source.session
          ~members:source.members ~policies ~identifiers expressions
      with
      | Ok _ -> Alcotest.failf "expected %s callback call to fail" label
      | Error error ->
          Alcotest.(check string)
            (label ^ " code") "HCSEMA0057"
            (Semantic_function_call_expression_result.error_code error);
          Alcotest.(check string)
            (label ^ " message") expected_message
            (Semantic_function_call_expression_result.error_message error))

let top_level_indexed_global_callback_calls () =
  List.iter
    (fun mode ->
      let source =
        prepared ~mode ~path:"top-level-indexed-global-callbacks.HC"
          "F64 class CallbackBacked {};\n\
           I64 (*CbInt)(I64 first,I64 second=2,I64 third)[2];\n\
           F64 (*CbFloat)()[2][3];I64 *(*CbPointer)()[1];\n\
           CallbackBacked (*CbMake)()[1];\n\
           I64 (*CbVariadic)(I64 first,...)[2];\n\
           CbInt[0](1,,3);CbFloat[1][2]();CbPointer[0]();CbMake[0]();\n\
           CbVariadic[1](1,2,3);CbInt[0](CbFloat[0][0](),,3);"
      in
      let _, _, _, result = analyze source in
      Alcotest.(check (list string))
        "indexed callback returns use their stored signatures"
        [
          "I64:object-value:integer-result:rank-0";
          "F64:object-value:f64-result:rank-0";
          "I64*:address-value:integer-result:rank-0";
          "CallbackBacked:object-value:f64-result:rank-0";
          "I64:object-value:integer-result:rank-0";
          "I64:object-value:integer-result:rank-0";
        ]
        (root_values result |> List.map descriptor);
      let calls =
        Semantic_function_call_expression_result
        .top_level_indexed_global_callback_calls result
      in
      Alcotest.(check (list string))
        "indexed callback calls retain source order, including nested calls"
        [
          "CbInt";
          "CbFloat";
          "CbPointer";
          "CbMake";
          "CbVariadic";
          "CbInt";
          "CbFloat";
        ]
        (List.map top_level_indexed_global_callback_name calls);
      let first = List.hd calls in
      Alcotest.(check string)
        "indexed callback keeps its array publication" "array"
        (first
       |> Semantic_function_call_expression_result
          .top_level_indexed_global_callback_value
       |> Semantic_function_call_resolution.identifier_value_shape
       |> Semantic_function_call_resolution.identifier_value_shape_name);
      Alcotest.(check string)
        "indexed callback keeps its completed callee result"
        "I64:object-value:integer-result:rank-0"
        (first
       |> Semantic_function_call_expression_result
          .top_level_indexed_global_callback_callee_result |> descriptor);
      Alcotest.(check (list string))
        "indexed callback defaults stay separate from provided arguments"
        [
          "integer-result:provided:I64:object-value:integer-result:rank-0";
          "integer-result:default:I64:integer-result:immediate";
          "integer-result:provided:I64:object-value:integer-result:rank-0";
        ]
        (first
       |> Semantic_function_call_expression_result
          .top_level_indexed_global_callback_fixed_results
        |> List.map top_level_fixed_description);
      let variadic = List.nth calls 4 in
      Alcotest.(check int64)
        "indexed callback variadic count uses target I64" 2L
        (Semantic_function_call_expression_result
         .top_level_indexed_global_callback_variadic_count variadic);
      let index_conversions =
        result |> Semantic_function_call_expression_result.top_level_all_results
        |> List.filter (fun value ->
            Semantic_function_call_expression_result.result_intrinsic_conversion
              value
            = Semantic_function_call_expression_result.Result_to_int)
      in
      Alcotest.(check int)
        "every retained callback-array index converts to target integer" 9
        (List.length index_conversions);
      let all =
        Semantic_function_call_expression_result.top_level_all_results result
      in
      List.iter
        (fun call ->
          let id =
            Semantic_function_call_expression_result
            .top_level_indexed_global_callback_result_id call
          in
          Alcotest.(check int)
            "each indexed callback links to one expression result" 1
            (List.length
               (List.filter
                  (fun value ->
                    Semantic_function_call_expression_result.Id.equal id
                      (Semantic_function_call_expression_result.result_id value))
                  all)))
        calls)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let invalid_top_level_indexed_global_callback_calls () =
  [
    ( "partial rank",
      "I64 (*Callback)(I64 value)[2][3];Callback[0](1);",
      "indexed callback callee uses 1 bracket, but global `Callback` has 2 \
       dimensions" );
    ( "excess rank",
      "I64 (*Callback)(I64 value)[2];Callback[0][1](1);",
      "indexed callback callee uses 2 brackets, but global `Callback` has 1 \
       dimension" );
    ( "missing",
      "I64 (*Callback)(I64 value)[2];Callback[0]();",
      "call to \"Callback\" is missing required argument 1 (value)" );
  ]
  |> List.iter (fun (label, contents, expected_message) ->
      let source =
        prepared
          ~path:("top-level-invalid-indexed-callback-" ^ label ^ ".HC")
          contents
      in
      let expressions, identifiers = build_inputs source in
      let policies =
        source |> Test_function_call_conversion_policy.analyze
        |> Test_function_call_conversion_policy.checked_policy
      in
      match
        Holyc_lib.type_top_level_expressions source.session
          ~members:source.members ~policies ~identifiers expressions
      with
      | Ok _ -> Alcotest.failf "expected %s indexed callback to fail" label
      | Error error ->
          Alcotest.(check string)
            (label ^ " code") "HCSEMA0057"
            (Semantic_function_call_expression_result.error_code error);
          Alcotest.(check string)
            (label ^ " message") expected_message
            (Semantic_function_call_expression_result.error_message error))

let top_level_member_callback_calls () =
  List.iter
    (fun mode ->
      let source =
        prepared ~mode ~path:"top-level-member-callbacks.HC"
          "F64 class Product {};\n\
          \           class Base {F64 (*Invoke)(I64 first=1,I64 required,F64 \
           last=3,...);};\n\
          \           class Box:Base {I64 (*Integer)();I64 *(*Pointer)();\n\
          \           Product (*Make)();I64 (*Apply)(I64 value);I64 ordinary;};\n\
          \           Box box;Box *pointer;\n\
          \           box.Integer();pointer->Invoke(,2,,4.0,5);\n\
          \           (box.Pointer)();box.Make();box.Apply(box.Integer());"
      in
      let _, _, _, result = analyze source in
      Alcotest.(check (list string))
        "member callback returns use their stored signatures"
        [
          "I64:object-value:integer-result:rank-0";
          "F64:object-value:f64-result:rank-0";
          "I64*:address-value:integer-result:rank-0";
          "Product:object-value:f64-result:rank-0";
          "I64:object-value:integer-result:rank-0";
        ]
        (root_values result |> List.map descriptor);
      let calls =
        Semantic_function_call_expression_result.top_level_member_callback_calls
          result
      in
      Alcotest.(check (list string))
        "member callback calls retain source order, including nested calls"
        [ "box"; "pointer"; "box"; "box"; "box"; "box" ]
        (List.map top_level_member_callback_name calls);
      let member_names =
        List.map
          (fun call ->
            call
            |> Semantic_function_call_expression_result
               .top_level_member_callback_lookup
            |> Semantic_aggregate_member_index.lookup_member
            |> Semantic_aggregate_member_index.member_symbol
            |> Semantic_symbol.name)
          calls
      in
      Alcotest.(check (list string))
        "each call keeps its exact direct or inherited member"
        [ "Integer"; "Invoke"; "Pointer"; "Make"; "Apply"; "Integer" ]
        member_names;
      let inherited = List.nth calls 1 in
      let inherited_lookup =
        Semantic_function_call_expression_result
        .top_level_member_callback_lookup inherited
      in
      Alcotest.(check (triple string string int))
        "pointer access keeps the inherited callback lookup" ("Box", "Base", 1)
        ( inherited_lookup
          |> Semantic_aggregate_member_index.lookup_queried_aggregate
          |> Semantic_symbol.name,
          inherited_lookup
          |> Semantic_aggregate_member_index.lookup_declaring_aggregate
          |> Semantic_symbol.name,
          Semantic_aggregate_member_index.lookup_inheritance_depth
            inherited_lookup );
      Alcotest.(check (list string))
        "member defaults stay separate from provided arguments"
        [
          "integer-result:default:I64:integer-result:immediate";
          "integer-result:provided:I64:object-value:integer-result:rank-0";
          "f64-result:default:F64:f64-result:immediate";
        ]
        (inherited
       |> Semantic_function_call_expression_result
          .top_level_member_callback_fixed_results
        |> List.map top_level_fixed_description);
      Alcotest.(check int64)
        "member variadic count uses target I64" 2L
        (Semantic_function_call_expression_result
         .top_level_member_callback_variadic_count inherited);
      Alcotest.(check string)
        "parenthesized member callee keeps its callback lookup"
        "Pointer:Box:depth-0:offset-16"
        (List.nth calls 2
       |> Semantic_function_call_expression_result
          .top_level_member_callback_callee_result |> lookup_description);
      let all =
        Semantic_function_call_expression_result.top_level_all_results result
      in
      List.iter
        (fun call ->
          let id =
            Semantic_function_call_expression_result
            .top_level_member_callback_result_id call
          in
          Alcotest.(check int)
            "each member callback links to one expression result" 1
            (List.length
               (List.filter
                  (fun value ->
                    Semantic_function_call_expression_result.Id.equal id
                      (Semantic_function_call_expression_result.result_id value))
                  all)))
        calls)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let invalid_top_level_member_callback_calls () =
  [
    ( "ordinary member",
      "class Box {I64 value;};Box box;box.value();",
      "member `value` is not callable" );
    ( "callback array",
      "class Box {I64 (*callbacks)(I64 value)[2];};Box box;box.callbacks(1);",
      "callback member `callbacks` retains 1 array dimension" );
    ( "wrong access",
      "class Box {I64 (*callback)();};Box *box;box.callback();",
      "direct member access requires an aggregate object, not a pointer" );
    ( "missing argument",
      "class Box {I64 (*callback)(I64 value);};Box box;box.callback();",
      "call to \"box\" is missing required argument 1 (value)" );
  ]
  |> List.iter (fun (label, contents, expected_message) ->
      let source =
        prepared
          ~path:("top-level-invalid-member-callback-" ^ label ^ ".HC")
          contents
      in
      let expressions, identifiers = build_inputs source in
      let policies =
        source |> Test_function_call_conversion_policy.analyze
        |> Test_function_call_conversion_policy.checked_policy
      in
      match
        Holyc_lib.type_top_level_expressions source.session
          ~members:source.members ~policies ~identifiers expressions
      with
      | Ok _ -> Alcotest.failf "expected %s member callback to fail" label
      | Error error ->
          Alcotest.(check string)
            (label ^ " code") "HCSEMA0057"
            (Semantic_function_call_expression_result.error_code error);
          Alcotest.(check string)
            (label ^ " message") expected_message
            (Semantic_function_call_expression_result.error_message error))

let unavailable_boundaries_and_checked_ownership () =
  let source =
    prepared ~path:"top-level-boundaries.HC"
      "class Box{I64 member;};Box box;I64 (*callbacks)(I64 value)[2];\n\
       box.member;callbacks(1);"
  in
  let expressions, identifiers, policies, result = analyze source in
  Alcotest.(check (list string))
    "member roots type while callback arrays remain an explicit boundary"
    [ "object-value"; "unavailable" ]
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

let stale_batches_and_mode_mismatch () =
  let session = Session.create () in
  let prepare path contents =
    let source = Session.add_source session ~path ~contents in
    let ast =
      Holyc_lib.parse_with_config session
        ~config:(Test_function_call_conversion_policy.config Preprocessor.Jit)
        ~source
      |> Test_function_call_conversion_policy.expect_ast
    in
    Test_function_call_conversion_policy.finish_prepare Preprocessor.Jit session
      ast
  in
  let first = prepare "top-level-batch-first.HC" "I64 first;first+1;" in
  let second = prepare "top-level-batch-second.HC" "I64 second;second+2;" in
  let expressions, identifiers = build_inputs first in
  let first_policies =
    first |> Test_function_call_conversion_policy.analyze
    |> Test_function_call_conversion_policy.checked_policy
  in
  let second_policies =
    second |> Test_function_call_conversion_policy.analyze
    |> Test_function_call_conversion_policy.checked_policy
  in
  let expect_input_error label expected_message result =
    match result with
    | Ok _ -> Alcotest.failf "expected %s to fail" label
    | Error error ->
        Alcotest.(check string)
          (label ^ " code") "HCSEMA0057"
          (Semantic_function_call_expression_result.error_code error);
        Alcotest.(check string)
          (label ^ " message") expected_message
          (Semantic_function_call_expression_result.error_message error)
  in
  Holyc_lib.type_top_level_expressions session ~members:first.members
    ~policies:second_policies ~identifiers expressions
  |> expect_input_error "stale policy batch"
       "call conversion policies describe another module";
  Holyc_lib.type_top_level_expressions session ~members:second.members
    ~policies:first_policies ~identifiers expressions
  |> expect_input_error "stale member batch"
       "aggregate member index describes another module";
  let aot_policies = policies_for_mode first Preprocessor.Aot in
  Holyc_lib.type_top_level_expressions session ~members:first.members
    ~policies:aot_policies ~identifiers expressions
  |> expect_input_error "mode mismatch"
       "top-level expressions and conversion policies use different \
        compilation modes"

let generated_provenance_and_purity () =
  let source =
    prepared ~path:"top-level-generated-results.HC"
      "#define VALUE box.value\n\
       #define OFFSET Box.value\n\
       #define CALL F(1)\n\
       #define CALLBACK Callback(2)\n\
       #define INDEXED Indexed[0](3)\n\
       #define MEMBER box.Member(4)\n\
       class Box {I64 value;I64 (*Member)(I64 value);};I64 F(I64 value);\n\
       I64 (*Callback)(I64 value);I64 (*Indexed)(I64 value)[1];Box box;\n\
       VALUE+1;0+OFFSET;CALL;CALLBACK;INDEXED;MEMBER;"
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
  Alcotest.(check (list string))
    "generated offset paths replay deterministically"
    (first |> completed_offsets |> List.map offset_description)
    (second |> completed_offsets |> List.map offset_description);
  let direct_calls =
    Semantic_function_call_expression_result.top_level_direct_calls first
  in
  Alcotest.(check (list string))
    "generated direct calls retain deterministic identity" [ "F" ]
    (List.map top_level_call_name direct_calls);
  let generated_call = List.hd direct_calls in
  let generated_call_result =
    let id =
      Semantic_function_call_expression_result.top_level_direct_result_id
        generated_call
    in
    first |> Semantic_function_call_expression_result.top_level_all_results
    |> List.find (fun result ->
        Semantic_function_call_expression_result.Id.equal id
          (Semantic_function_call_expression_result.result_id result))
  in
  (match
     Semantic_function_call_expression_result.result_origin
       generated_call_result
   with
  | Semantic_symbol.Source_location location ->
      Alcotest.(check bool)
        "generated call keeps its definition origin" true
        (Option.is_some location.defined_at)
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected a generated call source location");
  let generated_callbacks =
    Semantic_function_call_expression_result.top_level_global_callback_calls
      first
  in
  Alcotest.(check (list string))
    "generated callback calls retain deterministic identity" [ "Callback" ]
    (List.map top_level_global_callback_name generated_callbacks);
  let generated_callback_result =
    let id =
      generated_callbacks |> List.hd
      |> Semantic_function_call_expression_result
         .top_level_global_callback_result_id
    in
    first |> Semantic_function_call_expression_result.top_level_all_results
    |> List.find (fun result ->
        Semantic_function_call_expression_result.Id.equal id
          (Semantic_function_call_expression_result.result_id result))
  in
  (match
     Semantic_function_call_expression_result.result_origin
       generated_callback_result
   with
  | Semantic_symbol.Source_location location ->
      Alcotest.(check bool)
        "generated callback call keeps its definition origin" true
        (Option.is_some location.defined_at)
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected a generated callback source location");
  let generated_indexed_callbacks =
    Semantic_function_call_expression_result
    .top_level_indexed_global_callback_calls first
  in
  Alcotest.(check (list string))
    "generated indexed callback calls retain deterministic identity"
    [ "Indexed" ]
    (List.map top_level_indexed_global_callback_name generated_indexed_callbacks);
  let generated_indexed_callback_result =
    let id =
      generated_indexed_callbacks |> List.hd
      |> Semantic_function_call_expression_result
         .top_level_indexed_global_callback_result_id
    in
    first |> Semantic_function_call_expression_result.top_level_all_results
    |> List.find (fun result ->
        Semantic_function_call_expression_result.Id.equal id
          (Semantic_function_call_expression_result.result_id result))
  in
  (match
     Semantic_function_call_expression_result.result_origin
       generated_indexed_callback_result
   with
  | Semantic_symbol.Source_location location ->
      Alcotest.(check bool)
        "generated indexed callback call keeps its definition origin" true
        (Option.is_some location.defined_at)
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected a generated indexed callback source location");
  let generated_member_callbacks =
    Semantic_function_call_expression_result.top_level_member_callback_calls
      first
  in
  Alcotest.(check (list string))
    "generated member callback calls retain deterministic identity" [ "box" ]
    (List.map top_level_member_callback_name generated_member_callbacks);
  let generated_member_callback_result =
    let id =
      generated_member_callbacks |> List.hd
      |> Semantic_function_call_expression_result
         .top_level_member_callback_result_id
    in
    first |> Semantic_function_call_expression_result.top_level_all_results
    |> List.find (fun result ->
        Semantic_function_call_expression_result.Id.equal id
          (Semantic_function_call_expression_result.result_id result))
  in
  (match
     Semantic_function_call_expression_result.result_origin
       generated_member_callback_result
   with
  | Semantic_symbol.Source_location location ->
      Alcotest.(check bool)
        "generated member callback keeps its definition origin" true
        (Option.is_some location.defined_at)
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected a generated member callback source location");
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
  let generated_member =
    first |> Semantic_function_call_expression_result.top_level_all_results
    |> List.find (fun result ->
        match
          result |> Semantic_function_call_expression_result.result_source
          |> Semantic_function_call_resolution.argument_expression_kind
        with
        | Semantic_function_call_resolution.Member_access_expression _ -> true
        | _ -> false)
  in
  Alcotest.(check bool)
    "generated member retains its checked lookup" true
    (Option.is_some
       (Semantic_function_call_expression_result.result_member_lookup
          generated_member));
  let generated_offset =
    first |> Semantic_function_call_expression_result.top_level_all_results
    |> List.find (fun result ->
        result |> Semantic_function_call_expression_result.result_category
        = Semantic_function_call_expression_result.Offset_value)
  in
  Alcotest.(check string)
    "generated offset retains its cumulative path" "Box[value:0]=0"
    (offset_description generated_offset);
  (match
     Semantic_function_call_expression_result.result_origin generated_offset
   with
  | Semantic_symbol.Source_location location ->
      Alcotest.(check bool)
        "generated offset keeps its definition origin" true
        (Option.is_some location.defined_at)
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected a generated offset source location");
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
    Alcotest.test_case "aggregate member paths" `Quick aggregate_member_paths;
    Alcotest.test_case "invalid aggregate member paths" `Quick
      invalid_aggregate_member_paths;
    Alcotest.test_case "aggregate offset paths" `Quick aggregate_offset_paths;
    Alcotest.test_case "invalid aggregate offset paths" `Quick
      invalid_aggregate_offset_paths;
    Alcotest.test_case "top-level direct calls" `Quick top_level_direct_calls;
    Alcotest.test_case "top-level direct call replacement headers" `Quick
      top_level_direct_call_replacement_headers;
    Alcotest.test_case "included top-level calls" `Quick
      included_top_level_calls;
    Alcotest.test_case "invalid top-level direct calls" `Quick
      invalid_top_level_direct_calls;
    Alcotest.test_case "top-level global callback calls" `Quick
      top_level_global_callback_calls;
    Alcotest.test_case "invalid top-level global callback calls" `Quick
      invalid_top_level_global_callback_calls;
    Alcotest.test_case "top-level indexed global callback calls" `Quick
      top_level_indexed_global_callback_calls;
    Alcotest.test_case "invalid top-level indexed global callback calls" `Quick
      invalid_top_level_indexed_global_callback_calls;
    Alcotest.test_case "top-level member callback calls" `Quick
      top_level_member_callback_calls;
    Alcotest.test_case "invalid top-level member callback calls" `Quick
      invalid_top_level_member_callback_calls;
    Alcotest.test_case "unavailable boundaries and checked ownership" `Quick
      unavailable_boundaries_and_checked_ownership;
    Alcotest.test_case "stale batches and mode mismatch" `Quick
      stale_batches_and_mode_mismatch;
    Alcotest.test_case "generated provenance and purity" `Quick
      generated_provenance_and_purity;
  ]
