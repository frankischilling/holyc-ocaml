open Holyc_lib

let checked_results = function
  | Ok value -> value
  | Error error ->
      error |> Semantic_function_call_expression_result.error_to_string
      |> Alcotest.fail

let checked_decision = function
  | Ok value -> value
  | Error error ->
      error |> Semantic_function_call_conversion_decision.error_to_string
      |> Alcotest.fail

let checked_type = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let prepare = Test_function_call_conversion_policy.prepare

let with_included_source contents apply =
  let directory = Filename.temp_dir "holyc-index-result-" "" in
  let rec remove_tree path =
    if Sys.file_exists path then
      if Sys.is_directory path then (
        Sys.readdir path
        |> Array.iter (fun entry -> remove_tree (Filename.concat path entry));
        Unix.rmdir path)
      else Sys.remove path
  in
  let write_file path contents =
    let channel = open_out_bin path in
    Fun.protect
      ~finally:(fun () -> close_out channel)
      (fun () -> output_string channel contents)
  in
  Fun.protect
    ~finally:(fun () -> remove_tree directory)
    (fun () ->
      let root_path = Filename.concat directory "root.HC" in
      write_file root_path "#include \"calls\"";
      write_file (Filename.concat directory "calls.HC") contents;
      let session = Session.create () in
      let source =
        Test_function_call_conversion_policy.checked
          (Session.load_source session ~path:root_path)
      in
      let ast =
        Holyc_lib.parse_with_config session
          ~config:
            (Test_function_call_conversion_policy.config
               ~working_directory:directory Preprocessor.Jit)
          ~source
        |> Test_function_call_conversion_policy.expect_ast
      in
      let prepared =
        Test_function_call_conversion_policy.finish_prepare Preprocessor.Jit
          session ast
      in
      apply prepared)

let analyze prepared =
  let policies =
    Test_function_call_conversion_policy.analyze prepared
    |> Test_function_call_conversion_policy.checked_policy
  in
  let results =
    Holyc_lib.type_function_call_expressions prepared.session ~policies
    |> checked_results
  in
  (policies, results)

let function_named results name =
  Semantic_function_call_expression_result.functions results
  |> List.find (fun function_ ->
      function_ |> Semantic_function_call_expression_result.function_symbol
      |> Semantic_symbol.name |> String.equal name)

let direct = function
  | Semantic_function_call_expression_result.Direct_call_result call -> call
  | Semantic_function_call_expression_result.Deferred_call_result _ ->
      Alcotest.fail "expected a direct typed call"

let only_direct results name =
  match
    function_named results name
    |> Semantic_function_call_expression_result.function_calls
    |> List.map direct
  with
  | [ call ] -> call
  | calls ->
      Alcotest.failf "expected one direct typed call in %s, got %d" name
        (List.length calls)

let root_results results name =
  only_direct results name
  |> Semantic_function_call_expression_result.direct_fixed_results
  |> List.filter_map (fun fixed ->
      match Semantic_function_call_expression_result.fixed_path fixed with
      | Semantic_function_call_expression_result.Provided_result result ->
          Some result
      | Semantic_function_call_expression_result.Declared_default_result -> None)

let category_names results =
  List.map
    (fun result ->
      result |> Semantic_function_call_expression_result.result_category
      |> Semantic_function_call_expression_result.value_category_name)
    results

let class_names results =
  List.map
    (fun result ->
      result |> Semantic_function_call_expression_result.result_class
      |> Semantic_function_call_expression_result.result_class_name)
    results

let array_ranks results =
  List.map Semantic_function_call_expression_result.result_array_rank results

let intrinsic_conversion_names results =
  List.map
    (fun result ->
      result
      |> Semantic_function_call_expression_result.result_intrinsic_conversion
      |> Semantic_function_call_expression_result.intrinsic_conversion_name)
    results

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

let pointer_transitions_are_checked () =
  let scalar =
    Semantic_type.make_primitive ~form:Semantic_type.Public_spelling
      ~primitive:Primitive_type.I64 ~pointer_depth:0
    |> checked_type
  in
  let pointer = Semantic_type.pointer_to scalar |> checked_type in
  Alcotest.(check int)
    "addressing adds one pointer layer" 1
    (Semantic_type.pointer_depth pointer);
  let restored = Semantic_type.dereference pointer |> checked_type in
  Alcotest.(check int)
    "dereferencing removes one pointer layer" 0
    (Semantic_type.pointer_depth restored);
  (match (Semantic_type.base scalar, Semantic_type.base restored) with
  | ( Semantic_type.Primitive (left_form, left_primitive),
      Semantic_type.Primitive (right_form, right_primitive) ) ->
      Alcotest.(check bool)
        "pointer transitions retain the primitive form" true
        (left_form = right_form);
      Alcotest.(check bool)
        "pointer transitions retain the primitive identity" true
        (Primitive_type.equal left_primitive right_primitive)
  | _ -> Alcotest.fail "expected a primitive type after pointer transitions");
  Alcotest.(check (result reject string))
    "a scalar cannot be dereferenced"
    (Error "cannot dereference a type with no pointer layer")
    (Semantic_type.dereference scalar);
  let deepest =
    Semantic_type.make_primitive ~form:Semantic_type.Public_spelling
      ~primitive:Primitive_type.I64
      ~pointer_depth:Semantic_type.max_pointer_depth
    |> checked_type
  in
  Alcotest.(check (result reject string))
    "addressing cannot exceed the checked limit"
    (Error "semantic pointer depth 5 exceeds HolyC's limit of 4")
    (Semantic_type.pointer_to deepest);
  List.iter
    (fun form ->
      List.iter
        (fun pointer_depth ->
          let source_type =
            Semantic_type.make_primitive ~form ~primitive:Primitive_type.I64
              ~pointer_depth
            |> checked_type
          in
          let pointee = Semantic_type.dereference source_type |> checked_type in
          Alcotest.(check int)
            "each checked pointer depth loses one layer" (pointer_depth - 1)
            (Semantic_type.pointer_depth pointee);
          match Semantic_type.base pointee with
          | Semantic_type.Primitive (actual_form, primitive) ->
              Alcotest.(check bool)
                "dereference retains public or intrinsic form" true
                (actual_form = form);
              Alcotest.(check bool)
                "dereference retains the primitive identity" true
                (Primitive_type.equal primitive Primitive_type.I64)
          | Semantic_type.Aggregate _ ->
              Alcotest.fail "expected a primitive pointee")
        (List.init Semantic_type.max_pointer_depth (fun index -> index + 1)))
    [ Semantic_type.Public_spelling; Semantic_type.Internal_storage ]

let roots_retain_types_and_categories () =
  let prepared =
    prepare ~path:"call-expression-results.HC"
      "I64 (*GlobalCallback)(I64);I64 GlobalArray[1];\n\
       extern I64 Target(I64 a,I64 b,I64 c,I64 d,I64 e,I64 f,I64 g,I64 h,I64 \
       i,I64 j);\n\
       I64 Caller(I64 value,F64 floating,I64 (*callback)(I64)){I64 \
       array[1];return \
       Target(value,floating,array,callback,&value,1,2.5,\"x\",~2.5,sizeof(I64));}"
  in
  let _, results = analyze prepared in
  let roots = root_results results "Caller" in
  Alcotest.(check (list string))
    "provided roots keep their source categories"
    [
      "object-value";
      "object-value";
      "array-value";
      "callback-value";
      "address-value";
      "object-value";
      "object-value";
      "address-value";
      "object-value";
      "object-value";
    ]
    (category_names roots);
  Alcotest.(check (list string))
    "provided roots keep known source types"
    [ "I64"; "F64"; "I64"; "I64"; "I64*"; "I64"; "F64"; "U8*"; "I64"; "I64" ]
    (List.map type_name roots);
  Alcotest.(check (list string))
    "provided roots keep forwarded result classes"
    [
      "integer-result";
      "f64-result";
      "integer-result";
      "integer-result";
      "integer-result";
      "integer-result";
      "f64-result";
      "integer-result";
      "integer-result";
      "integer-result";
    ]
    (class_names roots)

let nested_results_have_stable_value_contexts () =
  let prepared =
    prepare ~path:"call-expression-contexts.HC"
      "extern I64 Target(I64 a,I64 b,I64 c,I64 d);\n\
       I64 Caller(I64 value){return Target(&value,value++,(value),value+1);}"
  in
  let _, results = analyze prepared in
  let all = Semantic_function_call_expression_result.all_results results in
  Alcotest.(check (list int))
    "expression identities are contiguous and source ordered"
    (List.init 9 Fun.id)
    (List.map
       (fun result ->
         result |> Semantic_function_call_expression_result.result_id
         |> Semantic_function_call_expression_result.Id.to_int)
       all);
  Alcotest.(check (list string))
    "address and update operands remain lvalues"
    [
      "address-value";
      "lvalue";
      "object-value";
      "lvalue";
      "object-value";
      "object-value";
      "object-value";
      "object-value";
      "object-value";
    ]
    (category_names all)

let dereferences_retain_source_types_and_shapes () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-expression-dereference.HC"
          "F64 class FloatBox {};\n\
           extern I64 Target(I64 public1,I64 public2,I64 public3,I64 \
           public4,I64 internal1,I64 internal2,I64 internal3,I64 internal4,F64 \
           floating,F64 array_value,I64 function_value,F64 aggregate_value,F64 \
           unknown_value);\n\
           I64 Caller(I64 *public1,I64 **public2,I64 ***public3,I64 \
           ****public4,I64i *internal1,I64i **internal2,I64i ***internal3,I64i \
           ****internal4,F64 **floating,I64 (*callback)(I64),FloatBox \
           *box){F64 values[1];return \
           Target(*public1,**public2,***public3,****public4,*internal1,**internal2,***internal3,****internal4,**floating,*values,*callback,*box,*Outer);}"
      in
      let _, results = analyze prepared in
      let roots = root_results results "Caller" in
      Alcotest.(check (list string))
        "dereferences retain the pointee or source-shaped type"
        [
          "I64";
          "I64";
          "I64";
          "I64";
          "I64";
          "I64";
          "I64";
          "I64";
          "F64";
          "F64";
          "I64";
          "FloatBox";
          "unavailable";
        ]
        (List.map type_name roots);
      Alcotest.(check (list string))
        "array and callback dereferences keep distinct result categories"
        [
          "object-value";
          "object-value";
          "object-value";
          "object-value";
          "object-value";
          "object-value";
          "object-value";
          "object-value";
          "object-value";
          "object-value";
          "function-value";
          "object-value";
          "unavailable";
        ]
        (category_names roots);
      Alcotest.(check (list string))
        "dereferenced classes follow the resulting type"
        [
          "integer-result";
          "integer-result";
          "integer-result";
          "integer-result";
          "integer-result";
          "integer-result";
          "integer-result";
          "integer-result";
          "f64-result";
          "f64-result";
          "integer-result";
          "f64-result";
          "unresolved";
        ]
        (class_names roots))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let aggregate_dereference_uses_source_visible_backing () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-expression-dereference-backing.HC"
          "extern class Later;\n\
           extern I64 Target(F64 value);\n\
           I64 Before(Later *value){return Target(*value);}\n\
           F64 class Later {};\n\
           I64 After(Later *value){return Target(*value);}"
      in
      let policies, results = analyze prepared in
      Alcotest.(check (list string))
        "aggregate dereference retains the canonical identity"
        [ "Later"; "Later" ]
        [
          root_results results "Before" |> List.hd |> type_name;
          root_results results "After" |> List.hd |> type_name;
        ];
      Alcotest.(check (list string))
        "only a source-visible backing changes the forwarded class"
        [ "integer-result"; "f64-result" ]
        [
          root_results results "Before"
          |> List.hd |> Semantic_function_call_expression_result.result_class
          |> Semantic_function_call_expression_result.result_class_name;
          root_results results "After"
          |> List.hd |> Semantic_function_call_expression_result.result_class
          |> Semantic_function_call_expression_result.result_class_name;
        ];
      let decision =
        Holyc_lib.decide_function_call_conversions prepared.session ~policies
          ~expressions:results
        |> checked_decision
      in
      let paths owner =
        decision |> Semantic_function_call_conversion_decision.functions
        |> List.find (fun function_ ->
            function_
            |> Semantic_function_call_conversion_decision.function_symbol
            |> Semantic_symbol.name |> String.equal owner)
        |> Semantic_function_call_conversion_decision.function_calls |> List.hd
        |> function
        | Semantic_function_call_conversion_decision.Direct_call_decision call
          ->
            call
            |> Semantic_function_call_conversion_decision.direct_fixed_decisions
            |> List.map (fun fixed ->
                fixed |> Semantic_function_call_conversion_decision.fixed_path
                |> Semantic_function_call_conversion_decision.fixed_path_name)
        | Semantic_function_call_conversion_decision.Deferred_call_decision _ ->
            Alcotest.fail "expected a direct call conversion"
      in
      Alcotest.(check (list string))
        "the conversion pass consumes each source-visible dereference class"
        [ "provided:integer-result:ICF_RES_TO_F64" ]
        (paths "Before");
      Alcotest.(check (list string))
        "the completed backing needs no conversion"
        [ "provided:f64-result:none" ]
        (paths "After"))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let dereference_lvalues_follow_the_parent_context () =
  let prepared =
    prepare ~path:"call-expression-dereference-context.HC"
      "extern I64 Target(I64 *address,I64 updated,I64 loaded);\n\
       I64 Caller(I64 *pointer,I64 value){return \
       Target(&*pointer,(*pointer)++,*(&value));}"
  in
  let _, results = analyze prepared in
  Alcotest.(check (list string))
    "only lvalue consumers retain a dereference lvalue"
    [
      "address-value";
      "lvalue";
      "object-value";
      "object-value";
      "lvalue";
      "lvalue";
      "object-value";
      "object-value";
      "address-value";
      "address-value";
      "lvalue";
    ]
    (results |> Semantic_function_call_expression_result.all_results
   |> category_names);
  Alcotest.(check (list string))
    "dereference roots keep their value-context types" [ "I64*"; "I64"; "I64" ]
    (root_results results "Caller" |> List.map type_name)

let scalar_dereference_retains_its_class () =
  let prepared =
    prepare ~path:"call-expression-scalar-dereference.HC"
      "extern I64 Target(I64 integer,U0 nothing);\n\
       I64 Caller(I64 address,U0 nothing){return Target(*address,*nothing);}"
  in
  let _, results = analyze prepared in
  let roots = root_results results "Caller" in
  Alcotest.(check (list string))
    "zero-depth dereferences retain their source type" [ "I64"; "U0" ]
    (List.map type_name roots);
  Alcotest.(check (list string))
    "zero-depth dereferences retain their forwarded class"
    [ "integer-result"; "integer-result" ]
    (class_names roots)

let indexes_retain_element_types_ranks_and_integer_intent () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-expression-index.HC"
          "F64 Global[2];\n\
           extern I64 Target(F64 element,I64 row,I64 pointer_value,I64 \
           *pointer_pointer,F64 second_element,F64 global_element);\n\
           I64 Caller(I64 *pointer,I64 **pointer_pointer,F64 floating){F64 \
           matrix[2][3];return \
           Target(matrix[0][floating],matrix[1],pointer[0],pointer_pointer[0],matrix[floating][0],Global[1]);}"
      in
      let _, results = analyze prepared in
      let roots = root_results results "Caller" in
      Alcotest.(check (list string))
        "each index consumes one array dimension or pointer layer"
        [ "F64"; "F64"; "I64"; "I64*"; "F64"; "F64" ]
        (List.map type_name roots);
      Alcotest.(check (list string))
        "partial array indexing remains an array value"
        [
          "object-value";
          "array-value";
          "object-value";
          "object-value";
          "object-value";
          "object-value";
        ]
        (category_names roots);
      Alcotest.(check (list int))
        "only the partial multidimensional result retains a dimension"
        [ 0; 1; 0; 0; 0; 0 ] (array_ranks roots);
      let index_values =
        results |> Semantic_function_call_expression_result.all_results
        |> List.filter (fun result ->
            result
            |> Semantic_function_call_expression_result
               .result_intrinsic_conversion
            = Semantic_function_call_expression_result.Result_to_int)
      in
      Alcotest.(check (list string))
        "every subscript records TempleOS integer-result conversion intent"
        [
          "ICF_RES_TO_INT";
          "ICF_RES_TO_INT";
          "ICF_RES_TO_INT";
          "ICF_RES_TO_INT";
          "ICF_RES_TO_INT";
          "ICF_RES_TO_INT";
          "ICF_RES_TO_INT";
          "ICF_RES_TO_INT";
        ]
        (intrinsic_conversion_names index_values);
      Alcotest.(check (list string))
        "integer intent does not erase the subscript source types"
        [ "I64"; "F64"; "I64"; "I64"; "I64"; "F64"; "I64"; "I64" ]
        (List.map type_name index_values))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let indexes_retain_pointer_depths_and_primitive_forms () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-expression-index-pointers.HC"
          "extern I64 Target(I64 p1,I64 *p2,I64 **p3,I64 ***p4,I64 i1,I64i \
           *i2,I64i **i3,I64i ***i4);\n\
           I64 Caller(I64 *p1,I64 **p2,I64 ***p3,I64 ****p4,I64i *i1,I64i \
           **i2,I64i ***i3,I64i ****i4){return \
           Target((p1)[0],p2[0],p3[0],p4[0],i1[0],i2[0],i3[0],i4[0]);}"
      in
      let _, results = analyze prepared in
      let roots = root_results results "Caller" in
      Alcotest.(check (list string))
        "indexing removes exactly one of one through four pointer layers"
        [ "I64"; "I64*"; "I64**"; "I64***"; "I64"; "I64*"; "I64**"; "I64***" ]
        (List.map type_name roots);
      let forms =
        roots
        |> List.map (fun result ->
            match
              Semantic_function_call_expression_result.result_type result
            with
            | Some type_ -> (
                match Semantic_type.base type_ with
                | Semantic_type.Primitive (Semantic_type.Public_spelling, _) ->
                    "public"
                | Semantic_type.Primitive (Semantic_type.Internal_storage, _) ->
                    "internal"
                | Semantic_type.Aggregate _ -> "aggregate")
            | None -> "unavailable")
      in
      Alcotest.(check (list string))
        "indexing preserves public and intrinsic primitive forms"
        [
          "public";
          "public";
          "public";
          "public";
          "internal";
          "internal";
          "internal";
          "internal";
        ]
        forms)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let aggregate_indexes_use_source_visible_backing () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-expression-index-backing.HC"
          "extern class Later;\n\
           extern I64 Target(F64 value);\n\
           I64 Before(Later *value){return Target(value[0]);}\n\
           F64 class Later {};\n\
           I64 After(Later *value){return Target(value[0]);}"
      in
      let policies, results = analyze prepared in
      Alcotest.(check (list string))
        "aggregate indexing retains canonical type identity"
        [ "Later"; "Later" ]
        [
          root_results results "Before" |> List.hd |> type_name;
          root_results results "After" |> List.hd |> type_name;
        ];
      Alcotest.(check (list string))
        "only a source-visible aggregate backing changes the index class"
        [ "integer-result"; "f64-result" ]
        [
          root_results results "Before"
          |> List.hd |> Semantic_function_call_expression_result.result_class
          |> Semantic_function_call_expression_result.result_class_name;
          root_results results "After"
          |> List.hd |> Semantic_function_call_expression_result.result_class
          |> Semantic_function_call_expression_result.result_class_name;
        ];
      let decision =
        Holyc_lib.decide_function_call_conversions prepared.session ~policies
          ~expressions:results
        |> checked_decision
      in
      let path owner =
        decision |> Semantic_function_call_conversion_decision.functions
        |> List.find (fun function_ ->
            function_
            |> Semantic_function_call_conversion_decision.function_symbol
            |> Semantic_symbol.name |> String.equal owner)
        |> Semantic_function_call_conversion_decision.function_calls |> List.hd
        |> function
        | Semantic_function_call_conversion_decision.Direct_call_decision call
          ->
            call
            |> Semantic_function_call_conversion_decision.direct_fixed_decisions
            |> List.hd |> Semantic_function_call_conversion_decision.fixed_path
            |> Semantic_function_call_conversion_decision.fixed_path_name
        | Semantic_function_call_conversion_decision.Deferred_call_decision _ ->
            Alcotest.fail "expected a direct aggregate-index conversion"
      in
      Alcotest.(check string)
        "the incomplete backing converts through the integer class"
        "provided:integer-result:ICF_RES_TO_F64" (path "Before");
      Alcotest.(check string)
        "the completed F64 backing needs no conversion"
        "provided:f64-result:none" (path "After"))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let indexed_lvalues_follow_the_parent_context () =
  let prepared =
    prepare ~path:"call-expression-index-context.HC"
      "extern I64 Target(F64 *address,F64 updated,F64 loaded);\n\
       I64 Caller(){F64 matrix[2][3];return \
       Target(&matrix[0][0],matrix[0][0]++,matrix[0][0]);}"
  in
  let _, results = analyze prepared in
  Alcotest.(check (list string))
    "index roots follow address, update, and value contexts"
    [ "address-value"; "object-value"; "object-value" ]
    (root_results results "Caller" |> category_names);
  let indexes =
    results |> Semantic_function_call_expression_result.all_results
    |> List.filter (fun result ->
        match
          result |> Semantic_function_call_expression_result.result_source
          |> Semantic_function_call_resolution.argument_expression_kind
        with
        | Semantic_function_call_resolution.Index_expression _ -> true
        | _ -> false)
  in
  Alcotest.(check (list string))
    "only final indexed elements become lvalues"
    [
      "lvalue";
      "array-value";
      "lvalue";
      "array-value";
      "object-value";
      "array-value";
    ]
    (category_names indexes)

let invalid_index_base_reports_the_opening_bracket () =
  [
    ( "scalar",
      "extern I64 Target(I64 value);I64 Caller(I64 value){return \
       Target(value[0]);}",
      String.index );
    ( "exhausted array",
      "extern I64 Target(I64 value);I64 Caller(){I64 array[1];return \
       Target(array[0][0]);}",
      String.rindex );
    ( "outer base before subscript",
      "extern I64 Target(I64 value);I64 Caller(I64 value,I64 other){return \
       Target(value[other[0]]);}",
      String.index );
  ]
  |> List.iter (fun (label, source, bracket_position) ->
      let prepared =
        prepare ~path:("call-expression-invalid-index-" ^ label ^ ".HC") source
      in
      let policies =
        Test_function_call_conversion_policy.analyze prepared
        |> Test_function_call_conversion_policy.checked_policy
      in
      match
        Holyc_lib.type_function_call_expressions prepared.session ~policies
      with
      | Ok _ -> Alcotest.failf "expected %s indexing to fail" label
      | Error error ->
          Alcotest.(check string)
            (label ^ " indexing has a stable semantic code")
            "HCSEMA0046"
            (Semantic_function_call_expression_result.error_code error);
          Alcotest.(check string)
            (label ^ " indexing names the rejected base shape")
            "index base is neither an array nor a pointer"
            (Semantic_function_call_expression_result.error_message error);
          let location =
            match
              Semantic_function_call_expression_result.error_origin error
            with
            | Some (Semantic_symbol.Source_location location) -> location
            | Some
                (Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _)
            | None -> Alcotest.fail "expected an opening-bracket source origin"
          in
          Alcotest.(check int)
            (label ^ " primary origin starts at the rejected opening bracket")
            (bracket_position source '[')
            location.span.start;
          Alcotest.(check int)
            (label ^ " opening-bracket origin is one byte")
            1
            (Span.length location.span))

let unresolved_index_bases_remain_unavailable () =
  let prepared =
    prepare ~path:"call-expression-unresolved-index.HC"
      "extern I64 Target(I64 value);I64 Caller(){return Target(Outer[1.5]);}"
  in
  let _, results = analyze prepared in
  let roots = root_results results "Caller" in
  Alcotest.(check (list string))
    "an unresolved base does not acquire a guessed element type"
    [ "unavailable" ] (List.map type_name roots);
  Alcotest.(check (list string))
    "an unresolved base keeps its unresolved result class" [ "unresolved" ]
    (class_names roots);
  let converted =
    results |> Semantic_function_call_expression_result.all_results
    |> List.filter (fun result ->
        result
        |> Semantic_function_call_expression_result.result_intrinsic_conversion
        = Semantic_function_call_expression_result.Result_to_int)
  in
  Alcotest.(check (list string))
    "the known subscript still carries integer conversion intent" [ "F64" ]
    (List.map type_name converted)

let included_indexes_keep_their_bracket_origins () =
  with_included_source
    "extern I64 Target(I64 value);I64 Caller(){I64 array[1];return \
     Target(array[0]);}" (fun prepared ->
      let _, results = analyze prepared in
      let result = root_results results "Caller" |> List.hd in
      let index =
        match
          result |> Semantic_function_call_expression_result.result_source
          |> Semantic_function_call_resolution.argument_expression_kind
        with
        | Semantic_function_call_resolution.Index_expression index -> index
        | _ -> Alcotest.fail "expected a retained included index"
      in
      [
        Semantic_function_call_expression_result.result_origin result;
        Semantic_function_call_resolution.index_opening_origin index;
        Semantic_function_call_resolution.index_closing_origin index;
      ]
      |> List.iter (function
        | Semantic_symbol.Source_location location ->
            let source =
              Source_manager.find
                (Session.sources prepared.session)
                location.span.source
              |> Option.get
            in
            Alcotest.(check string)
              "included index location keeps its source file" "calls.HC"
              (Source_file.path source |> Filename.basename)
        | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
            Alcotest.fail "expected included index provenance"))

let conversion_uses_the_exact_typed_roots () =
  let prepared =
    prepare ~path:"call-expression-conversion-identity.HC"
      "extern I64 Target(F64 a,I64 b,F64 c);\n\
       I64 Caller(){return Target(1,2.5,3);}"
  in
  let policies, expressions = analyze prepared in
  let expected_ids =
    root_results expressions "Caller"
    |> List.map Semantic_function_call_expression_result.result_id
  in
  let decision =
    Holyc_lib.decide_function_call_conversions prepared.session ~policies
      ~expressions
    |> checked_decision
  in
  let function_ =
    Semantic_function_call_conversion_decision.functions decision
    |> List.find (fun function_ ->
        function_ |> Semantic_function_call_conversion_decision.function_symbol
        |> Semantic_symbol.name |> String.equal "Caller")
  in
  let actual_ids, paths =
    match
      Semantic_function_call_conversion_decision.function_calls function_
    with
    | [ Semantic_function_call_conversion_decision.Direct_call_decision call ]
      ->
        call
        |> Semantic_function_call_conversion_decision.direct_fixed_decisions
        |> List.map (fun fixed ->
            match
              Semantic_function_call_conversion_decision.fixed_path fixed
            with
            | Semantic_function_call_conversion_decision.Provided_path provided
              ->
                ( provided
                  |> Semantic_function_call_conversion_decision
                     .provided_actual_result
                  |> Semantic_function_call_expression_result.result_id,
                  fixed |> Semantic_function_call_conversion_decision.fixed_path
                  |> Semantic_function_call_conversion_decision.fixed_path_name
                )
            | Semantic_function_call_conversion_decision.Declared_default_path
              -> Alcotest.fail "expected a provided conversion path")
        |> List.split
    | _ -> Alcotest.fail "expected one direct conversion decision"
  in
  Alcotest.(check (list int))
    "conversion decisions retain the exact typed result identity"
    (List.map Semantic_function_call_expression_result.Id.to_int expected_ids)
    (List.map Semantic_function_call_expression_result.Id.to_int actual_ids);
  Alcotest.(check (list string))
    "typed extraction preserves the pinned conversions"
    [
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:f64-result:ICF_RES_TO_INT";
      "provided:integer-result:ICF_RES_TO_F64";
    ]
    paths

let deterministic_generated_results_do_not_mutate_symbols () =
  let prepared =
    prepare ~path:"call-expression-generated.HC"
      "#define ARG array[0]\n\
       extern I64 Target(I64 value);\n\
       I64 Caller(){I64 array[1];return Target(ARG);}"
  in
  let policies =
    Test_function_call_conversion_policy.analyze prepared
    |> Test_function_call_conversion_policy.checked_policy
  in
  let table = Session.semantic_symbols prepared.session in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  let first =
    Holyc_lib.type_function_call_expressions prepared.session ~policies
    |> checked_results
  in
  let second =
    Holyc_lib.type_function_call_expressions prepared.session ~policies
    |> checked_results
  in
  let describe results =
    Semantic_function_call_expression_result.all_results results
    |> List.map (fun result ->
        ( result |> Semantic_function_call_expression_result.result_id
          |> Semantic_function_call_expression_result.Id.to_int,
          result |> Semantic_function_call_expression_result.result_category
          |> Semantic_function_call_expression_result.value_category_name,
          result |> Semantic_function_call_expression_result.result_class
          |> Semantic_function_call_expression_result.result_class_name ))
  in
  Alcotest.(check (list (triple int string string)))
    "replaying expression typing is deterministic" (describe first)
    (describe second);
  Alcotest.(check int)
    "expression typing leaves the symbol table unchanged" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length);
  match root_results first "Caller" with
  | [ result ] ->
      (match Semantic_function_call_expression_result.result_origin result with
      | Semantic_symbol.Source_location location ->
          Alcotest.(check bool)
            "generated result keeps its invocation" true
            (Option.is_some location.generated_from);
          Alcotest.(check bool)
            "generated result keeps its definition" true
            (Option.is_some location.defined_at)
      | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
          Alcotest.fail "expected generated expression provenance");
      let index =
        match
          result |> Semantic_function_call_expression_result.result_source
          |> Semantic_function_call_resolution.argument_expression_kind
        with
        | Semantic_function_call_resolution.Index_expression index -> index
        | _ -> Alcotest.fail "expected a retained generated index"
      in
      [
        Semantic_function_call_resolution.index_opening_origin index;
        Semantic_function_call_resolution.index_closing_origin index;
      ]
      |> List.iter (function
        | Semantic_symbol.Source_location location ->
            Alcotest.(check bool)
              "generated bracket keeps its invocation" true
              (Option.is_some location.generated_from);
            Alcotest.(check bool)
              "generated bracket keeps its definition" true
              (Option.is_some location.defined_at)
        | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
            Alcotest.fail "expected generated bracket provenance")
  | _ -> Alcotest.fail "expected one generated expression result"

let foreign_session_and_traversal_are_rejected () =
  let prepared =
    prepare ~path:"call-expression-ownership.HC"
      "extern I64 Target(I64 value);I64 Caller(){I64 array[1];return \
       Target(array[0]);}"
  in
  let first_policies, first_results = analyze prepared in
  let second_policies =
    Test_function_call_conversion_policy.analyze prepared
    |> Test_function_call_conversion_policy.checked_policy
  in
  let foreign = Session.create () in
  (match
     Holyc_lib.type_function_call_expressions foreign ~policies:first_policies
   with
  | Ok _ -> Alcotest.fail "expected foreign expression typing to fail"
  | Error error ->
      Alcotest.(check string)
        "foreign session has a stable expression diagnostic" "HCSEMA0046"
        (Semantic_function_call_expression_result.error_code error));
  match
    Holyc_lib.decide_function_call_conversions prepared.session
      ~policies:second_policies ~expressions:first_results
  with
  | Ok _ -> Alcotest.fail "expected a mismatched traversal to fail"
  | Error error ->
      Alcotest.(check string)
        "mismatched traversal has a stable conversion diagnostic" "HCSEMA0045"
        (Semantic_function_call_conversion_decision.error_code error)

let tests =
  [
    Alcotest.test_case "checked pointer transitions" `Quick
      pointer_transitions_are_checked;
    Alcotest.test_case "root types and categories" `Quick
      roots_retain_types_and_categories;
    Alcotest.test_case "nested value contexts" `Quick
      nested_results_have_stable_value_contexts;
    Alcotest.test_case "dereference types and shapes" `Quick
      dereferences_retain_source_types_and_shapes;
    Alcotest.test_case "dereference backing source order" `Quick
      aggregate_dereference_uses_source_visible_backing;
    Alcotest.test_case "dereference lvalue contexts" `Quick
      dereference_lvalues_follow_the_parent_context;
    Alcotest.test_case "scalar dereference class" `Quick
      scalar_dereference_retains_its_class;
    Alcotest.test_case "indexed types, ranks, and conversion intent" `Quick
      indexes_retain_element_types_ranks_and_integer_intent;
    Alcotest.test_case "indexed pointer depths and primitive forms" `Quick
      indexes_retain_pointer_depths_and_primitive_forms;
    Alcotest.test_case "indexed aggregate backing source order" `Quick
      aggregate_indexes_use_source_visible_backing;
    Alcotest.test_case "indexed lvalue contexts" `Quick
      indexed_lvalues_follow_the_parent_context;
    Alcotest.test_case "invalid index base origin" `Quick
      invalid_index_base_reports_the_opening_bracket;
    Alcotest.test_case "unresolved index base" `Quick
      unresolved_index_bases_remain_unavailable;
    Alcotest.test_case "included index origins" `Quick
      included_indexes_keep_their_bracket_origins;
    Alcotest.test_case "conversion identity" `Quick
      conversion_uses_the_exact_typed_roots;
    Alcotest.test_case "determinism and generated provenance" `Quick
      deterministic_generated_results_do_not_mutate_symbols;
    Alcotest.test_case "ownership validation" `Quick
      foreign_session_and_traversal_are_rejected;
  ]
