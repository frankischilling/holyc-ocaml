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

let substring_start source needle =
  let source_length = String.length source in
  let needle_length = String.length needle in
  let rec search index =
    if index + needle_length > source_length then
      Alcotest.failf "expected %S in test source" needle
    else if String.sub source index needle_length = needle then index
    else search (index + 1)
  in
  search 0

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
    Holyc_lib.type_function_call_expressions prepared.session
      ~members:prepared.members ~policies
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
  | Semantic_function_call_expression_result.Indirect_call_result _ ->
      Alcotest.fail "expected a direct call, got an indirect call"
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

let direct_named results owner callee =
  function_named results owner
  |> Semantic_function_call_expression_result.function_calls
  |> List.filter_map (function
    | Semantic_function_call_expression_result.Direct_call_result call ->
        let name =
          call |> Semantic_function_call_expression_result.direct_source
          |> Semantic_function_call_conversion_policy.direct_source
          |> Semantic_function_call_resolution.direct_source
          |> Semantic_function_call_resolution.call_callee_name
        in
        if String.equal name callee then Some call else None
    | Semantic_function_call_expression_result.Indirect_call_result _ -> None
    | Semantic_function_call_expression_result.Deferred_call_result _ -> None)
  |> function
  | [ call ] -> call
  | calls ->
      Alcotest.failf "expected one direct call to %s in %s, got %d" callee owner
        (List.length calls)

let indirect_named results owner callee =
  function_named results owner
  |> Semantic_function_call_expression_result.function_calls
  |> List.filter_map (function
    | Semantic_function_call_expression_result.Indirect_call_result call ->
        let name =
          call |> Semantic_function_call_expression_result.indirect_source
          |> Semantic_function_call_conversion_policy.indirect_source
          |> Semantic_function_call_resolution.indirect_source
          |> Semantic_function_call_resolution.call_callee_name
        in
        if String.equal name callee then Some call else None
    | Semantic_function_call_expression_result.Direct_call_result _
    | Semantic_function_call_expression_result.Deferred_call_result _ -> None)
  |> function
  | [ call ] -> call
  | calls ->
      Alcotest.failf "expected one indirect call to %s in %s, got %d" callee
        owner (List.length calls)

let decision_direct_named result owner callee =
  result |> Semantic_function_call_conversion_decision.functions
  |> List.find (fun function_ ->
      function_ |> Semantic_function_call_conversion_decision.function_symbol
      |> Semantic_symbol.name |> String.equal owner)
  |> Semantic_function_call_conversion_decision.function_calls
  |> List.filter_map (function
    | Semantic_function_call_conversion_decision.Direct_call_decision call ->
        let name =
          call |> Semantic_function_call_conversion_decision.direct_source
          |> Semantic_function_call_conversion_policy.direct_source
          |> Semantic_function_call_resolution.direct_source
          |> Semantic_function_call_resolution.call_callee_name
        in
        if String.equal name callee then Some call else None
    | Semantic_function_call_conversion_decision.Indirect_call_decision _ ->
        None
    | Semantic_function_call_conversion_decision.Deferred_call_decision _ ->
        None)
  |> function
  | [ call ] -> call
  | calls ->
      Alcotest.failf "expected one direct decision for %s in %s, got %d" callee
        owner (List.length calls)

let provided_results call =
  call |> Semantic_function_call_expression_result.direct_fixed_results
  |> List.filter_map (fun fixed ->
      match Semantic_function_call_expression_result.fixed_path fixed with
      | Semantic_function_call_expression_result.Provided_result result ->
          Some result
      | Semantic_function_call_expression_result.Declared_default_result _ ->
          None)

let declared_defaults fixed_results =
  List.filter_map
    (fun fixed ->
      match Semantic_function_call_expression_result.fixed_path fixed with
      | Semantic_function_call_expression_result.Declared_default_result result
        -> Some (fixed, result)
      | Semantic_function_call_expression_result.Provided_result _ -> None)
    fixed_results

let lastclass_substitutions fixed_results =
  List.filter_map
    Semantic_function_call_expression_result.fixed_lastclass_substitution
    fixed_results

let lastclass_names fixed_results =
  fixed_results |> lastclass_substitutions
  |> List.map Semantic_function_call_expression_result.lastclass_class_name

let root_results results name = only_direct results name |> provided_results

let variadic_results results name =
  only_direct results name
  |> Semantic_function_call_expression_result.direct_variadic_results

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

let function_address_path_name result =
  match
    Semantic_function_call_expression_result.result_function_address_path result
  with
  | Some path ->
      Semantic_function_call_resolution.direct_function_address_path_name path
  | None -> "none"

let semantic_type_name type_ =
  let base =
    match Semantic_type.base type_ with
    | Semantic_type.Primitive (_, primitive) ->
        Primitive_type.to_string primitive
    | Semantic_type.Aggregate symbol -> Semantic_symbol.name symbol
  in
  base ^ String.make (Semantic_type.pointer_depth type_) '*'

let member_lookup result =
  match
    Semantic_function_call_expression_result.result_member_lookup result
  with
  | Some lookup -> lookup
  | None -> Alcotest.fail "expected a resolved aggregate member lookup"

let member_source result =
  match
    result |> Semantic_function_call_expression_result.result_source
    |> Semantic_function_call_resolution.argument_expression_kind
  with
  | Semantic_function_call_resolution.Member_access_expression member -> member
  | _ -> Alcotest.fail "expected a retained member expression"

let binary_source result =
  match
    result |> Semantic_function_call_expression_result.result_source
    |> Semantic_function_call_resolution.argument_expression_kind
  with
  | Semantic_function_call_resolution.Binary_expression binary -> binary
  | _ -> Alcotest.fail "expected a retained binary expression"

let address_operand result =
  match
    result |> Semantic_function_call_expression_result.result_source
    |> Semantic_function_call_resolution.argument_expression_kind
  with
  | Semantic_function_call_resolution.Prefix_expression prefix
    when Semantic_function_call_resolution.prefix_operator prefix
         = Semantic_function_call_resolution.Address_of ->
      Semantic_function_call_resolution.prefix_operand prefix
  | _ -> Alcotest.fail "expected a retained address-of expression"

let bound_identifier expression =
  match
    Semantic_function_call_resolution.argument_expression_kind expression
  with
  | Semantic_function_call_resolution.Bound_identifier_expression identifier ->
      identifier
  | _ -> Alcotest.fail "expected a retained bound identifier"

let bound_publication expression =
  let occurrence =
    expression |> bound_identifier
    |> Semantic_function_call_resolution.bound_identifier_occurrence
  in
  match Semantic_module_expression_binding.occurrence_resolution occurrence with
  | Semantic_module_expression_binding.Module_binding publication -> publication
  | Semantic_module_expression_binding.Local_binding _ ->
      Alcotest.fail "expected a module publication"
  | Semantic_module_expression_binding.Outer_candidate ->
      Alcotest.fail "expected a resolved module publication"

let update_operator_name result =
  match
    result |> Semantic_function_call_expression_result.result_source
    |> Semantic_function_call_resolution.argument_expression_kind
  with
  | Semantic_function_call_resolution.Prefix_expression prefix ->
      prefix |> Semantic_function_call_resolution.prefix_operator
      |> Semantic_function_call_resolution.prefix_operator_name
  | Semantic_function_call_resolution.Postfix_expression postfix ->
      postfix |> Semantic_function_call_resolution.postfix_operator
      |> Semantic_function_call_resolution.postfix_operator_name
  | _ -> Alcotest.fail "expected a retained update expression"

let update_operand result =
  match
    result |> Semantic_function_call_expression_result.result_source
    |> Semantic_function_call_resolution.argument_expression_kind
  with
  | Semantic_function_call_resolution.Prefix_expression prefix ->
      Semantic_function_call_resolution.prefix_operand prefix
  | Semantic_function_call_resolution.Postfix_expression postfix ->
      Semantic_function_call_resolution.postfix_operand postfix
  | _ -> Alcotest.fail "expected a retained update expression"

let result_for_source results source =
  results |> Semantic_function_call_expression_result.all_results
  |> List.find (fun result ->
      Semantic_function_call_expression_result.result_source result == source)

let execution_class_name result =
  match
    Semantic_function_call_expression_result.result_execution_class result
  with
  | None -> "none"
  | Some class_ ->
      Semantic_function_call_expression_result.result_class_name class_

let lookup_description result =
  let lookup = member_lookup result in
  let member = Semantic_aggregate_member_index.lookup_member lookup in
  let layout = Semantic_aggregate_member_index.member_layout member in
  ( member |> Semantic_aggregate_member_index.member_symbol
    |> Semantic_symbol.name,
    lookup |> Semantic_aggregate_member_index.lookup_declaring_aggregate
    |> Semantic_symbol.name,
    Semantic_aggregate_member_index.lookup_inheritance_depth lookup,
    layout.offset )

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

let direct_function_addresses_keep_publication_identity () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-expression-function-address.HC"
          "extern I64 Handler();\n\
           extern I64 Target(I64 fixed=0,...);\n\
           I64 Handler(){return 1;}\n\
           I64 Caller(){return Target(&Handler,&Caller);}"
      in
      let table = Session.semantic_symbols prepared.session in
      let symbol_count =
        Semantic_symbol_table.all_symbols table |> List.length
      in
      let _, first = analyze prepared in
      let _, second = analyze prepared in
      let call = direct_named first "Caller" "Target" in
      let roots =
        provided_results call
        @ Semantic_function_call_expression_result.direct_variadic_results call
      in
      Alcotest.(check (list string))
        "direct function addresses have address results"
        [ "address-value"; "address-value" ]
        (category_names roots);
      Alcotest.(check (list string))
        "direct function addresses retain RT_PTR's internal I64 type"
        [ "I64"; "I64" ] (List.map type_name roots);
      Alcotest.(check (list string))
        "direct function addresses use integer result registers"
        [ "integer-result"; "integer-result" ]
        (class_names roots);
      Alcotest.(check (list string))
        "direct function addresses retain their compilation path"
        (match mode with
        | Preprocessor.Jit -> [ "jit-immediate"; "jit-immediate" ]
        | Preprocessor.Aot -> [ "aot-absolute"; "aot-absolute" ])
        (List.map function_address_path_name roots);
      let operands = List.map address_operand roots in
      let operand_results = List.map (result_for_source first) operands in
      Alcotest.(check (list string))
        "direct function operands remain function values"
        [ "function-value"; "function-value" ]
        (category_names operand_results);
      Alcotest.(check (list string))
        "direct function operands retain the distinct binding shape"
        [ "direct-function"; "direct-function" ]
        (List.map
           (fun operand ->
             operand |> bound_identifier
             |> Semantic_function_call_resolution.bound_identifier_shape
             |> Semantic_function_call_resolution.identifier_value_shape_name)
           operands);
      let handler_identifier = List.hd operands |> bound_identifier in
      let handler_occurrence =
        Semantic_function_call_resolution.bound_identifier_occurrence
          handler_identifier
      in
      let handler_type =
        Semantic_function_call_resolution.bound_identifier_type
          handler_identifier
      in
      Alcotest.(check (result reject string))
        "a function publication cannot be relabeled as object storage"
        (Error "bound call argument occurrence is not a typed value binding")
        (Semantic_function_call_resolution
         .make_bound_identifier_argument_expression
           ~occurrence:handler_occurrence ~resolved_type:handler_type
           ~shape:Semantic_function_call_resolution.Object_value ~array_rank:0
           ());
      List.iter
        (fun result ->
          match Semantic_function_call_expression_result.result_type result with
          | Some type_ -> (
              Alcotest.(check int)
                "RT_PTR is not an object pointer layer" 0
                (Semantic_type.pointer_depth type_);
              match Semantic_type.base type_ with
              | Semantic_type.Primitive (form, primitive) ->
                  Alcotest.(check bool)
                    "RT_PTR uses the intrinsic primitive form" true
                    (form = Semantic_type.Internal_storage);
                  Alcotest.(check bool)
                    "RT_PTR aliases RT_I64" true
                    (Primitive_type.equal primitive Primitive_type.I64)
              | Semantic_type.Aggregate _ ->
                  Alcotest.fail "expected RT_PTR's primitive type")
          | None -> Alcotest.fail "expected a direct function address type")
        roots;
      let selected_handler = List.hd operands |> bound_publication in
      Alcotest.(check int)
        "the address uses the source-visible replacement header" 2
        (Semantic_module_expression_binding.publication_item_index
           selected_handler);
      let source_symbol =
        Semantic_module_expression_binding.publication_source_symbol
          selected_handler
      in
      let canonical_symbol =
        Semantic_module_expression_binding.publication_canonical_symbol
          selected_handler
      in
      Alcotest.(check (pair string string))
        "the replacement keeps its source and canonical function names"
        ("Handler", "Handler")
        ( Semantic_symbol.name source_symbol,
          Semantic_symbol.name canonical_symbol );
      Alcotest.(check bool)
        "the replacement source and joined identity remain distinct" false
        (Semantic_symbol.Id.equal
           (Semantic_symbol.id source_symbol)
           (Semantic_symbol.id canonical_symbol));
      let selected_declaration =
        roots |> List.hd
        |> Semantic_function_call_expression_result.result_function_declaration
        |> Option.get
      in
      let selected_site =
        Semantic_function_resolution.resolved_declaration_site
          selected_declaration
      in
      Alcotest.(check bool)
        "the address keeps the declaration for its exact source publication"
        true
        (Semantic_symbol.Id.equal
           (selected_site
          |> Semantic_function_resolution.declaration_site_function
          |> Semantic_function_type_resolution.function_symbol
          |> Semantic_symbol.id)
           (Semantic_symbol.id source_symbol));
      Alcotest.(check bool)
        "the address declaration keeps the joined canonical identity" true
        (Semantic_symbol.Id.equal
           (selected_declaration
          |> Semantic_function_resolution.resolved_declaration_identity_symbol
          |> Semantic_symbol.id)
           (Semantic_symbol.id canonical_symbol));
      let recursive = List.nth operands 1 |> bound_publication in
      Alcotest.(check string)
        "the recursive address retains Caller publication identity" "Caller"
        (recursive
       |> Semantic_module_expression_binding.publication_source_symbol
       |> Semantic_symbol.name);
      Alcotest.(check (list string))
        "replaying direct function addresses is deterministic"
        (Semantic_function_call_expression_result.all_results first
        |> List.map type_name)
        (Semantic_function_call_expression_result.all_results second
        |> List.map type_name);
      Alcotest.(check int)
        "typing function addresses leaves symbols unchanged" symbol_count
        (Semantic_symbol_table.all_symbols table |> List.length))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let function_address_paths_follow_extern_state () =
  let jit =
    prepare ~mode:Preprocessor.Jit ~path:"call-expression-jit-extern-address.HC"
      "extern I64 External();extern I64 Target(I64 address);\n\
       I64 Caller(){return Target(&External);}"
  in
  let _, jit_results = analyze jit in
  let jit_root = root_results jit_results "Caller" |> List.hd in
  Alcotest.(check string)
    "a JIT extern address uses its executable-address slot" "jit-extern-slot"
    (function_address_path_name jit_root);
  let aot_bound =
    prepare ~mode:Preprocessor.Aot ~path:"call-expression-aot-bound-address.HC"
      "_extern REMOTE_BOUND I64 Bound();extern I64 Target(I64 address);\n\
       I64 Caller(){return Target(&Bound);}"
  in
  let _, aot_results = analyze aot_bound in
  let aot_root = root_results aot_results "Caller" |> List.hd in
  Alcotest.(check string)
    "a bound AOT function uses the absolute-address path" "aot-absolute"
    (function_address_path_name aot_root);
  let declaration =
    Semantic_function_call_expression_result.result_function_declaration
      aot_root
    |> Option.get
  in
  let site =
    Semantic_function_resolution.resolved_declaration_site declaration
  in
  Alcotest.(check bool)
    "the AOT path follows the checked bound-extern declaration" true
    (Semantic_function_resolution.declaration_site_source_kind site
    = Semantic_function_resolution.Bound_extern)

let rejected_function_address_path expected_message source =
  let prepared =
    prepare ~mode:Preprocessor.Aot ~path:"call-expression-aot-address-error.HC"
      source
  in
  let policies =
    Test_function_call_conversion_policy.analyze prepared
    |> Test_function_call_conversion_policy.checked_policy
  in
  match
    Holyc_lib.type_function_call_expressions prepared.session
      ~members:prepared.members ~policies
  with
  | Ok _ -> Alcotest.fail "expected the AOT function address to be rejected"
  | Error error -> (
      Alcotest.(check string)
        "function address rejection has a stable diagnostic" "HCSEMA0046"
        (Semantic_function_call_expression_result.error_code error);
      Alcotest.(check string)
        "function address rejection explains the unavailable path"
        expected_message
        (Semantic_function_call_expression_result.error_message error);
      match Semantic_function_call_expression_result.error_origin error with
      | Some (Semantic_symbol.Source_location location) ->
          Alcotest.(check int)
            "function address rejection points to address-of"
            (substring_start source "&")
            location.span.start
      | Some (Semantic_symbol.Pinned_source _)
      | Some (Semantic_symbol.Synthesized _)
      | None -> Alcotest.fail "expected a source-positioned address diagnostic")

let aot_and_internal_function_addresses_fail_explicitly () =
  rejected_function_address_path
    "cannot take the address of unresolved AOT function \"External\" outside \
     assembly"
    "extern I64 External();extern I64 Target(I64 address);\n\
     I64 Caller(){return Target(&External);}";
  rejected_function_address_path
    "cannot take the address of imported AOT function \"Imported\" outside \
     assembly"
    "import I64 Imported();extern I64 Target(I64 address);\n\
     I64 Caller(){return Target(&Imported);}";
  rejected_function_address_path
    "cannot use internal compiler function \"Internal\" as a direct function \
     address"
    "_intern IC_BSF I64 Internal(I64 value);\n\
     extern I64 Target(I64 address);\n\
     I64 Caller(){return Target(&Internal);}"

let function_address_provenance_and_storage_shadowing () =
  with_included_source
    "#define FN Handler\n\
     extern I64 Target(I64 value);I64 Handler(){return 1;}\n\
     I64 Caller(){return Target(&FN);}" (fun prepared ->
      let _, results = analyze prepared in
      let root = root_results results "Caller" |> List.hd in
      let occurrence =
        root |> address_operand |> bound_identifier
        |> Semantic_function_call_resolution.bound_identifier_occurrence
      in
      match Semantic_module_expression_binding.occurrence_origin occurrence with
      | Semantic_symbol.Source_location location ->
          Alcotest.(check bool)
            "generated function address keeps its invocation" true
            (Option.is_some location.generated_from);
          Alcotest.(check bool)
            "generated function address keeps its definition" true
            (Option.is_some location.defined_at)
      | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
          Alcotest.fail "expected generated function-address provenance");
  let prepared =
    prepare ~path:"call-expression-function-address-shadow.HC"
      "extern I64 Target(I64 object_address,I64 callback_address);\n\
       I64 Handler(){return 1;}\n\
       I64 Caller(I64 (*callback)()){I64 Handler;return \
       Target(&Handler,&callback);}"
  in
  let _, results = analyze prepared in
  let roots = root_results results "Caller" in
  Alcotest.(check (list string))
    "storage shadowing keeps ordinary address-of types" [ "I64*"; "I64*" ]
    (List.map type_name roots);
  let shapes =
    roots
    |> List.map (fun result ->
        result |> address_operand |> bound_identifier
        |> Semantic_function_call_resolution.bound_identifier_shape
        |> Semantic_function_call_resolution.identifier_value_shape_name)
  in
  Alcotest.(check (list string))
    "local storage and callbacks are not direct functions"
    [ "object"; "function-pointer" ]
    shapes;
  let local_identifier =
    roots |> List.hd |> address_operand |> bound_identifier
  in
  Alcotest.(check (result reject string))
    "local storage cannot be relabeled as a direct function"
    (Error "bound call argument occurrence is not a typed value binding")
    (Semantic_function_call_resolution.make_bound_identifier_argument_expression
       ~occurrence:
         (Semantic_function_call_resolution.bound_identifier_occurrence
            local_identifier)
       ~resolved_type:
         (Semantic_function_call_resolution.bound_identifier_type
            local_identifier)
       ~shape:Semantic_function_call_resolution.Direct_function_value
       ~array_rank:0 ())

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
        | Semantic_function_call_conversion_decision.Indirect_call_decision _ ->
            Alcotest.fail "expected a direct call conversion"
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
        | Semantic_function_call_conversion_decision.Indirect_call_decision _ ->
            Alcotest.fail "expected a direct aggregate-index conversion"
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
        Holyc_lib.type_function_call_expressions prepared.session
          ~members:prepared.members ~policies
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

let members_retain_lookup_identity_and_access_kind () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-expression-member-lookup.HC"
          "class Base {I8 inherited;};class Child : Base {I64 own;};\n\
           extern I64 Target(I64 a,I64 b,I64 c);\n\
           I64 Caller(Child object,Child *pointer){return \
           Target(object.own,pointer->own,object.inherited);}"
      in
      let _, results = analyze prepared in
      let roots = root_results results "Caller" in
      Alcotest.(check (list string))
        "direct and pointer member roots keep their declared types"
        [ "I64"; "I64"; "I8" ] (List.map type_name roots);
      Alcotest.(check (list string))
        "member roots are values in direct-call arguments"
        [ "object-value"; "object-value"; "object-value" ]
        (category_names roots);
      Alcotest.(check (list string))
        "lookup retains member identity, declaration owner, depth, and offset"
        [ "own:Child:0:1"; "own:Child:0:1"; "inherited:Base:1:0" ]
        (List.map
           (fun result ->
             let member, owner, depth, offset = lookup_description result in
             Printf.sprintf "%s:%s:%d:%Ld" member owner depth offset)
           roots);
      let access_kinds =
        roots
        |> List.map (fun result ->
            match
              result |> Semantic_function_call_expression_result.result_source
              |> Semantic_function_call_resolution.argument_expression_kind
            with
            | Semantic_function_call_resolution.Member_access_expression member
              ->
                member |> Semantic_function_call_resolution.member_access_kind
                |> Semantic_function_call_resolution.member_access_kind_name
            | _ -> Alcotest.fail "expected a retained member expression")
      in
      Alcotest.(check (list string))
        "direct and pointer spellings remain distinct"
        [ "direct"; "pointer"; "direct" ]
        access_kinds)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let member_constructor_validates_names_and_origins () =
  let base =
    Semantic_function_call_resolution.make_argument_expression
      ~kind:Semantic_function_call_resolution.Integer_literal
      ~origin:(Semantic_symbol.Synthesized "member base")
  in
  let make ?(operator_origin = Semantic_symbol.Synthesized "member operator")
      ?(member_name = "value")
      ?(member_origin = Semantic_symbol.Synthesized "member name") () =
    Semantic_function_call_resolution.make_member_argument_expression ~base
      ~access_kind:Semantic_function_call_resolution.Direct_member
      ~operator_origin ~member_name ~member_origin
  in
  [
    ( make ~operator_origin:(Semantic_symbol.Synthesized "") (),
      "call argument member has an invalid operator origin" );
    (make ~member_name:"" (), "call argument member name cannot be empty");
    ( make ~member_origin:(Semantic_symbol.Synthesized "") (),
      "call argument member has an invalid name origin" );
  ]
  |> List.iter (fun (result, expected) ->
      match result with
      | Ok _ -> Alcotest.fail "expected invalid member construction to fail"
      | Error message ->
          Alcotest.(check string)
            "member constructor diagnostic" expected message);
  match make () with
  | Error message -> Alcotest.fail message
  | Ok (Semantic_function_call_resolution.Member_access_expression member) ->
      Alcotest.(check string)
        "valid member construction retains its spelling" "value"
        (Semantic_function_call_resolution.member_name member)
  | Ok _ -> Alcotest.fail "expected a member expression constructor"

let member_arrays_callbacks_and_lvalues_keep_their_shapes () =
  let prepared =
    prepare ~path:"call-expression-member-shapes.HC"
      "class Box {I64 matrix[2][3];F64 (*callback)(I64);F64 value;};\n\
       extern I64 Target(I64 a,I64 b,I64 c,F64 d,F64 *e,F64 f);\n\
       I64 Caller(Box box){return \
       Target(box.matrix,box.matrix[0],box.callback,box.value,&box.value,box.value++);}"
  in
  let _, results = analyze prepared in
  let roots = root_results results "Caller" in
  Alcotest.(check (list string))
    "member arrays, callbacks, values, addresses, and updates stay distinct"
    [
      "array-value";
      "array-value";
      "callback-value";
      "object-value";
      "address-value";
      "object-value";
    ]
    (category_names roots);
  Alcotest.(check (list int))
    "a partial member index consumes one retained dimension"
    [ 2; 1; 0; 0; 0; 0 ] (array_ranks roots);
  Alcotest.(check (list string))
    "member types survive array, callback, address, and update contexts"
    [ "I64"; "I64"; "F64"; "F64"; "F64*"; "F64" ]
    (List.map type_name roots);
  let lvalues =
    results |> Semantic_function_call_expression_result.all_results
    |> List.filter (fun result ->
        result |> Semantic_function_call_expression_result.result_category
        = Semantic_function_call_expression_result.Lvalue)
    |> List.filter_map (fun result ->
        match
          Semantic_function_call_expression_result.result_member_lookup result
        with
        | None -> None
        | Some lookup ->
            lookup |> Semantic_aggregate_member_index.lookup_member
            |> Semantic_aggregate_member_index.member_symbol
            |> Semantic_symbol.name |> Option.some)
  in
  Alcotest.(check (list string))
    "address and update parents retain member lvalues" [ "value"; "value" ]
    lvalues

let member_result_uses_source_visible_backing () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-expression-member-backing.HC"
          "F64 class FloatBox {};class Holder {FloatBox value;};\n\
           extern I64 Target(F64 value);\n\
           I64 Caller(Holder holder){return Target(holder.value);}"
      in
      let policies, results = analyze prepared in
      let root = root_results results "Caller" |> List.hd in
      Alcotest.(check string)
        "aggregate member retains its canonical type" "FloatBox"
        (type_name root);
      Alcotest.(check string)
        "the visible F64 backing supplies the member result class" "f64-result"
        (root |> Semantic_function_call_expression_result.result_class
       |> Semantic_function_call_expression_result.result_class_name);
      let decision =
        Holyc_lib.decide_function_call_conversions prepared.session ~policies
          ~expressions:results
        |> checked_decision
      in
      let path =
        decision |> Semantic_function_call_conversion_decision.functions
        |> List.find (fun function_ ->
            function_
            |> Semantic_function_call_conversion_decision.function_symbol
            |> Semantic_symbol.name |> String.equal "Caller")
        |> Semantic_function_call_conversion_decision.function_calls |> List.hd
        |> function
        | Semantic_function_call_conversion_decision.Direct_call_decision call
          ->
            call
            |> Semantic_function_call_conversion_decision.direct_fixed_decisions
            |> List.hd |> Semantic_function_call_conversion_decision.fixed_path
            |> Semantic_function_call_conversion_decision.fixed_path_name
        | Semantic_function_call_conversion_decision.Indirect_call_decision _ ->
            Alcotest.fail "expected a direct member conversion"
        | Semantic_function_call_conversion_decision.Deferred_call_decision _ ->
            Alcotest.fail "expected a direct member conversion"
      in
      Alcotest.(check string)
        "direct-call conversion consumes the typed member root"
        "provided:f64-result:none" path)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let invalid_member_access_reports_the_operator_or_name () =
  [
    ( "scalar",
      "extern I64 Target(I64 value);I64 Caller(I64 value){return \
       Target(value.missing);}",
      ".missing",
      0,
      "member access base is not an aggregate" );
    ( "direct pointer",
      "class Box {I64 value;};extern I64 Target(I64 value);I64 Caller(Box \
       *box){return Target(box.value);}",
      ".value",
      0,
      "direct member access requires an aggregate object, not a pointer" );
    ( "pointer object",
      "class Box {I64 value;};extern I64 Target(I64 value);I64 Caller(Box \
       box){return Target(box->value);}",
      "->value",
      0,
      "pointer member access requires a pointer to an aggregate" );
    ( "missing member",
      "class Box {I64 value;};extern I64 Target(I64 value);I64 Caller(Box \
       box){return Target(box.missing);}",
      "box.missing",
      4,
      "aggregate `Box` has no member `missing`" );
    ( "incomplete aggregate",
      "extern class Later;extern I64 Target(I64 value);I64 Caller(Later \
       *later){return Target(later->value);}class Later {I64 value;};",
      "later->value",
      7,
      "aggregate `Later` is not complete before this member access" );
  ]
  |> List.iter (fun (label, source, marker, marker_offset, expected_message) ->
      let prepared =
        prepare ~path:("call-expression-invalid-member-" ^ label ^ ".HC") source
      in
      let policies =
        Test_function_call_conversion_policy.analyze prepared
        |> Test_function_call_conversion_policy.checked_policy
      in
      match
        Holyc_lib.type_function_call_expressions prepared.session
          ~members:prepared.members ~policies
      with
      | Ok _ -> Alcotest.failf "expected %s member access to fail" label
      | Error error -> (
          Alcotest.(check string)
            (label ^ " member access has a stable semantic code")
            "HCSEMA0046"
            (Semantic_function_call_expression_result.error_code error);
          Alcotest.(check string)
            (label ^ " member access explains the rejected shape")
            expected_message
            (Semantic_function_call_expression_result.error_message error);
          match Semantic_function_call_expression_result.error_origin error with
          | Some (Semantic_symbol.Source_location location) ->
              Alcotest.(check int)
                (label ^ " member diagnostic uses the operator or name")
                (substring_start source marker + marker_offset)
                location.span.start
          | Some
              (Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _)
          | None -> Alcotest.fail "expected a member source origin"))

let unresolved_member_bases_remain_unavailable () =
  let prepared =
    prepare ~path:"call-expression-unresolved-member.HC"
      "extern I64 Target(I64 value);I64 Caller(){return Target(outer.value);}"
  in
  let _, results = analyze prepared in
  match root_results results "Caller" with
  | [ result ] ->
      Alcotest.(check string)
        "an outer member base does not acquire a guessed type" "unavailable"
        (type_name result);
      Alcotest.(check string)
        "an outer member base remains unresolved" "unresolved"
        (result |> Semantic_function_call_expression_result.result_class
       |> Semantic_function_call_expression_result.result_class_name);
      Alcotest.(check bool)
        "an unavailable member has no fabricated lookup" true
        (Option.is_none
           (Semantic_function_call_expression_result.result_member_lookup result))
  | _ -> Alcotest.fail "expected one unresolved member result"

let included_and_generated_members_keep_their_origins () =
  with_included_source
    "class Box {I64 value;};extern I64 Target(I64 value);I64 Caller(Box \
     box){return Target(box.value);}" (fun prepared ->
      let _, results = analyze prepared in
      let member = root_results results "Caller" |> List.hd |> member_source in
      [
        Semantic_function_call_resolution.member_operator_origin member;
        Semantic_function_call_resolution.member_origin member;
      ]
      |> List.iter (function
        | Semantic_symbol.Source_location location ->
            let source_file =
              Source_manager.find
                (Session.sources prepared.session)
                location.span.source
              |> Option.get
            in
            Alcotest.(check string)
              "included member origin keeps its source file" "calls.HC"
              (Source_file.path source_file |> Filename.basename)
        | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
            Alcotest.fail "expected an included member source origin"));
  let prepared =
    prepare ~path:"call-expression-generated-member.HC"
      "#define ACCESS box.value\n\
       class Box {I64 value;};extern I64 Target(I64 value);\n\
       I64 Caller(Box box){return Target(ACCESS);}"
  in
  let _, results = analyze prepared in
  let member = root_results results "Caller" |> List.hd |> member_source in
  [
    Semantic_function_call_resolution.member_operator_origin member;
    Semantic_function_call_resolution.member_origin member;
  ]
  |> List.iter (function
    | Semantic_symbol.Source_location location ->
        Alcotest.(check bool)
          "generated member origin keeps its invocation" true
          (Option.is_some location.generated_from);
        Alcotest.(check bool)
          "generated member origin keeps its definition" true
          (Option.is_some location.defined_at)
    | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
        Alcotest.fail "expected a generated member source origin")

let assignment_operators_keep_destination_results () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-expression-assignment-operators.HC"
          "extern I64 Target(I64 a,I64 b,I64 c,I64 d,I64 e,I64 f,I64 g,I64 \
           h,I64 i,I64 j,I64 k);\n\
           I64 Caller(I64 value){return \
           Target(value=1,value<<=1,value>>=1,value*=1,value/=1,value%=1,value&=1,value|=1,value^=1,value+=1,value-=1);}"
      in
      let _, results = analyze prepared in
      let roots = root_results results "Caller" in
      Alcotest.(check int)
        "the checked assignment inventory has eleven source operators" 11
        (List.length roots);
      Alcotest.(check (list string))
        "every assignment keeps the destination type"
        (List.init 11 (fun _ -> "I64"))
        (List.map type_name roots);
      Alcotest.(check (list string))
        "every assignment keeps the destination result class"
        (List.init 11 (fun _ -> "integer-result"))
        (class_names roots);
      Alcotest.(check (list string))
        "integer assignment execution stays on the integer path"
        (List.init 11 (fun _ -> "integer-result"))
        (List.map execution_class_name roots);
      Alcotest.(check (list string))
        "the semantic roots preserve the generated assignment identities"
        [
          "IC_ASSIGN";
          "IC_SHL_EQU";
          "IC_SHR_EQU";
          "IC_MUL_EQU";
          "IC_DIV_EQU";
          "IC_MOD_EQU";
          "IC_AND_EQU";
          "IC_OR_EQU";
          "IC_XOR_EQU";
          "IC_ADD_EQU";
          "IC_SUB_EQU";
        ]
        (List.map
           (fun root ->
             root |> binary_source
             |> Semantic_function_call_resolution.binary_operator
             |> Semantic_function_call_resolution.binary_operator_name)
           roots))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let assignment_conversions_separate_storage_and_execution () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-expression-assignment-conversions.HC"
          "extern I64 Target(F64 a,I64 b,F64 c,I64 d,I64 e,I64 f);\n\
           I64 Caller(I64 integer,F64 floating){return \
           Target(floating=integer,integer=floating,floating+=integer,integer+=floating,integer<<=floating,integer&=floating);}"
      in
      let policies, results = analyze prepared in
      let roots = root_results results "Caller" in
      Alcotest.(check (list string))
        "assignment roots follow destination storage classes"
        [
          "f64-result";
          "integer-result";
          "f64-result";
          "integer-result";
          "integer-result";
          "integer-result";
        ]
        (class_names roots);
      Alcotest.(check (list string))
        "compound execution retains its separate F64 or integer path"
        [
          "f64-result";
          "integer-result";
          "f64-result";
          "f64-result";
          "integer-result";
          "integer-result";
        ]
        (List.map execution_class_name roots);
      let right_results =
        roots
        |> List.map (fun root ->
            root |> binary_source
            |> Semantic_function_call_resolution.binary_right
            |> result_for_source results)
      in
      Alcotest.(check (list string))
        "right operands retain the pinned assignment conversions"
        [
          "ICF_RES_TO_F64";
          "ICF_RES_TO_INT";
          "ICF_RES_TO_F64";
          "none";
          "ICF_RES_TO_INT";
          "ICF_RES_TO_INT";
        ]
        (intrinsic_conversion_names right_results);
      let decision =
        Holyc_lib.decide_function_call_conversions prepared.session ~policies
          ~expressions:results
        |> checked_decision
      in
      let paths =
        decision |> Semantic_function_call_conversion_decision.functions
        |> List.find (fun function_ ->
            function_
            |> Semantic_function_call_conversion_decision.function_symbol
            |> Semantic_symbol.name |> String.equal "Caller")
        |> Semantic_function_call_conversion_decision.function_calls |> List.hd
        |> function
        | Semantic_function_call_conversion_decision.Direct_call_decision call
          ->
            call
            |> Semantic_function_call_conversion_decision.direct_fixed_decisions
            |> List.map (fun fixed ->
                fixed |> Semantic_function_call_conversion_decision.fixed_path
                |> Semantic_function_call_conversion_decision.fixed_path_name)
        | Semantic_function_call_conversion_decision.Indirect_call_decision _ ->
            Alcotest.fail "expected direct assignment call conversions"
        | Semantic_function_call_conversion_decision.Deferred_call_decision _ ->
            Alcotest.fail "expected direct assignment call conversions"
      in
      Alcotest.(check (list string))
        "outer call conversion consumes each assignment destination class"
        [
          "provided:f64-result:none";
          "provided:integer-result:none";
          "provided:f64-result:none";
          "provided:integer-result:none";
          "provided:integer-result:none";
          "provided:integer-result:none";
        ]
        paths)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let assignment_destinations_cover_identifiers_pointers_indexes_and_members () =
  let prepared =
    prepare ~path:"call-expression-assignment-destinations.HC"
      "class Box {I64 value;};extern I64 Target(I64 a,I64 b,I64 c,I64 d,I64 \
       *p,I64 q);\n\
       I64 Caller(I64 scalar,I64 *pointer,Box box){I64 array[1];return \
       Target(scalar=1,*pointer=2,array[0]=3,box.value=4,pointer=pointer,pointer+=1);}"
  in
  let _, results = analyze prepared in
  let roots = root_results results "Caller" in
  Alcotest.(check (list string))
    "valid storage destinations retain scalar and pointer types"
    [ "I64"; "I64"; "I64"; "I64"; "I64*"; "I64*" ]
    (List.map type_name roots);
  let left_results =
    roots
    |> List.map (fun root ->
        root |> binary_source |> Semantic_function_call_resolution.binary_left
        |> result_for_source results)
  in
  Alcotest.(check (list string))
    "each destination is retained as an lvalue beneath the assignment"
    (List.init 6 (fun _ -> "lvalue"))
    (category_names left_results)

let invalid_assignment_destinations_report_the_operator () =
  [
    ( "literal",
      "extern I64 Target(I64 value);I64 Caller(){return Target(1=2);}",
      "assignment destination is not an lvalue" );
    ( "address",
      "extern I64 Target(I64 value);I64 Caller(I64 value){return \
       Target((&value)=2);}",
      "assignment destination is not an lvalue" );
    ( "array",
      "extern I64 Target(I64 value);I64 Caller(){I64 array[1];return \
       Target(array=0);}",
      "assignment destination is not an lvalue" );
    ( "callback",
      "extern I64 Target(I64 value);I64 Caller(I64 (*callback)(I64)){return \
       Target(callback=0);}",
      "assignment destination is not an lvalue" );
    ( "aggregate",
      "class Box {I64 value;};extern I64 Target(I64 value);I64 Caller(Box \
       left,Box right){return Target(left=right);}",
      "assignment destination is not a pointer or internal storage value" );
    ( "outer",
      "extern I64 Target(I64 value);I64 Caller(){return Target(outer=1);}",
      "assignment destination is unavailable" );
  ]
  |> List.iter (fun (label, source, expected_message) ->
      let prepared =
        prepare
          ~path:("call-expression-invalid-assignment-" ^ label ^ ".HC")
          source
      in
      let policies =
        Test_function_call_conversion_policy.analyze prepared
        |> Test_function_call_conversion_policy.checked_policy
      in
      match
        Holyc_lib.type_function_call_expressions prepared.session
          ~members:prepared.members ~policies
      with
      | Ok _ -> Alcotest.failf "expected %s assignment to fail" label
      | Error error -> (
          Alcotest.(check string)
            (label ^ " assignment uses the expression semantic code")
            "HCSEMA0046"
            (Semantic_function_call_expression_result.error_code error);
          Alcotest.(check string)
            (label ^ " assignment explains the invalid destination")
            expected_message
            (Semantic_function_call_expression_result.error_message error);
          match Semantic_function_call_expression_result.error_origin error with
          | Some (Semantic_symbol.Source_location location) ->
              Alcotest.(check int)
                (label ^ " assignment points at the operator")
                (String.index source '=') location.span.start
          | Some
              (Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _)
          | None -> Alcotest.fail "expected an assignment operator origin"))

let nested_and_generated_assignments_are_deterministic () =
  with_included_source
    "extern I64 Target(I64 value);I64 Caller(I64 value){return \
     Target(value=1);}" (fun prepared ->
      let _, results = analyze prepared in
      let binary = root_results results "Caller" |> List.hd |> binary_source in
      match Semantic_function_call_resolution.binary_operator_origin binary with
      | Semantic_symbol.Source_location location ->
          let source_file =
            Source_manager.find
              (Session.sources prepared.session)
              location.span.source
            |> Option.get
          in
          Alcotest.(check string)
            "included assignment operator keeps its source file" "calls.HC"
            (Source_file.path source_file |> Filename.basename)
      | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
          Alcotest.fail "expected an included assignment operator origin");
  let prepared =
    prepare ~path:"call-expression-generated-assignment.HC"
      "#define ASSIGN left=right=1\n\
       extern I64 Target(I64 value);I64 Caller(I64 left,I64 right){return \
       Target(ASSIGN);}"
  in
  let policies =
    Test_function_call_conversion_policy.analyze prepared
    |> Test_function_call_conversion_policy.checked_policy
  in
  let table = Session.semantic_symbols prepared.session in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  let analyze_once () =
    Holyc_lib.type_function_call_expressions prepared.session
      ~members:prepared.members ~policies
    |> checked_results
  in
  let first = analyze_once () in
  let second = analyze_once () in
  let describe results =
    results |> Semantic_function_call_expression_result.all_results
    |> List.map (fun result ->
        ( result |> Semantic_function_call_expression_result.result_id
          |> Semantic_function_call_expression_result.Id.to_int,
          ( result |> type_name,
            result |> Semantic_function_call_expression_result.result_class
            |> Semantic_function_call_expression_result.result_class_name,
            execution_class_name result ) ))
  in
  Alcotest.(check (list (pair int (triple string string string))))
    "nested assignment typing replays identically" (describe first)
    (describe second);
  Alcotest.(check int)
    "assignment typing leaves the symbol table unchanged" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length);
  let roots = root_results first "Caller" in
  Alcotest.(check (list string))
    "right-associative assignment keeps the outer destination result" [ "I64" ]
    (List.map type_name roots);
  let assignment_results =
    first |> Semantic_function_call_expression_result.all_results
    |> List.filter (fun result ->
        match
          result |> Semantic_function_call_expression_result.result_source
          |> Semantic_function_call_resolution.argument_expression_kind
        with
        | Semantic_function_call_resolution.Binary_expression binary ->
            binary |> Semantic_function_call_resolution.binary_operator
            |> Semantic_function_call_resolution.binary_operator_name
            |> String.equal "IC_ASSIGN"
        | _ -> false)
  in
  Alcotest.(check int)
    "the generated expression retains both assignment nodes" 2
    (List.length assignment_results);
  assignment_results
  |> List.iter (fun result ->
      let binary = binary_source result in
      match Semantic_function_call_resolution.binary_operator_origin binary with
      | Semantic_symbol.Source_location location ->
          Alcotest.(check bool)
            "generated assignment operator keeps its invocation" true
            (Option.is_some location.generated_from);
          Alcotest.(check bool)
            "generated assignment operator keeps its definition" true
            (Option.is_some location.defined_at)
      | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
          Alcotest.fail "expected a generated assignment operator origin")

let update_operators_keep_storage_results () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-expression-update-operators.HC"
          "extern I64 Target(I64 a,I64 b,I64 c,I64 d,F64 e,F64 f,F64 g,F64 \
           h,I64 *i,I64 *j,I64 *k,I64 *l);I64 Caller(I64 integer,F64 \
           floating,I64 *pointer){return \
           Target(++integer,--integer,integer++,integer--,++floating,--floating,floating++,floating--,++pointer,--pointer,pointer++,pointer--);}"
      in
      let _, results = analyze prepared in
      let roots = root_results results "Caller" in
      Alcotest.(check (list string))
        "the four update identities remain distinct for each storage class"
        [
          "pre-increment";
          "pre-decrement";
          "post-increment";
          "post-decrement";
          "pre-increment";
          "pre-decrement";
          "post-increment";
          "post-decrement";
          "pre-increment";
          "pre-decrement";
          "post-increment";
          "post-decrement";
        ]
        (List.map update_operator_name roots);
      Alcotest.(check (list string))
        "updates preserve integer, floating, and pointer storage types"
        ([ "I64"; "I64"; "I64"; "I64" ]
        @ [ "F64"; "F64"; "F64"; "F64" ]
        @ [ "I64*"; "I64*"; "I64*"; "I64*" ])
        (List.map type_name roots);
      Alcotest.(check (list string))
        "updates preserve each forwarded result class"
        (List.init 4 (fun _ -> "integer-result")
        @ List.init 4 (fun _ -> "f64-result")
        @ List.init 4 (fun _ -> "integer-result"))
        (class_names roots);
      Alcotest.(check (list string))
        "update roots are values"
        (List.init 12 (fun _ -> "object-value"))
        (category_names roots);
      let operands =
        List.map
          (fun root -> update_operand root |> result_for_source results)
          roots
      in
      Alcotest.(check (list string))
        "every update retains a writable lvalue operand"
        (List.init 12 (fun _ -> "lvalue"))
        (category_names operands))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let update_destinations_cover_identifiers_dereferences_indexes_and_members () =
  let prepared =
    prepare ~path:"call-expression-update-destinations.HC"
      "class Box {I64 value;};extern I64 Target(I64 a,I64 b,I64 c,I64 d,I64 \
       *e);I64 Caller(I64 scalar,I64 *pointer,Box box){I64 array[1];return \
       Target(++scalar,(*pointer)--,++array[0],box.value--,pointer++);}"
  in
  let _, results = analyze prepared in
  let roots = root_results results "Caller" in
  Alcotest.(check (list string))
    "identifier, dereference, index, member, and pointer updates keep types"
    [ "I64"; "I64"; "I64"; "I64"; "I64*" ]
    (List.map type_name roots);
  Alcotest.(check (list string))
    "every supported update destination remains an lvalue below its root"
    (List.init 5 (fun _ -> "lvalue"))
    (roots
    |> List.map (fun root -> update_operand root |> result_for_source results)
    |> category_names)

let invalid_update_operands_report_the_operator () =
  [
    ( "literal",
      "extern I64 Target(I64 value);I64 Caller(){return Target(++1);}",
      "++",
      "pre-increment operand is not an lvalue" );
    ( "address",
      "extern I64 Target(I64 value);I64 Caller(I64 value){return \
       Target((&value)++);}",
      "++",
      "post-increment operand is not an lvalue" );
    ( "array",
      "extern I64 Target(I64 value);I64 Caller(){I64 array[1];return \
       Target(array--);}",
      "--",
      "post-decrement operand is not an lvalue" );
    ( "callback",
      "extern I64 Target(I64 value);I64 Caller(I64 (*callback)(I64)){return \
       Target(callback++);}",
      "++",
      "post-increment operand is not an lvalue" );
    ( "function",
      "extern I64 Target(I64 value);I64 Caller(I64 (*callback)(I64)){return \
       Target((*callback)--);}",
      "--",
      "post-decrement operand is not an lvalue" );
    ( "aggregate",
      "class Box {I64 value;};extern I64 Target(I64 value);I64 Caller(Box \
       box){return Target(++box);}",
      "++",
      "pre-increment operand is not a pointer or internal storage value" );
    ( "outer",
      "extern I64 Target(I64 value);I64 Caller(){return Target(--outer);}",
      "--",
      "pre-decrement operand is unavailable" );
  ]
  |> List.iter (fun (label, source, spelling, expected_message) ->
      let prepared =
        prepare ~path:("call-expression-invalid-update-" ^ label ^ ".HC") source
      in
      let policies =
        Test_function_call_conversion_policy.analyze prepared
        |> Test_function_call_conversion_policy.checked_policy
      in
      match
        Holyc_lib.type_function_call_expressions prepared.session
          ~members:prepared.members ~policies
      with
      | Ok _ -> Alcotest.failf "expected %s update to fail" label
      | Error error -> (
          Alcotest.(check string)
            (label ^ " update uses the expression semantic code")
            "HCSEMA0046"
            (Semantic_function_call_expression_result.error_code error);
          Alcotest.(check string)
            (label ^ " update explains the invalid operand")
            expected_message
            (Semantic_function_call_expression_result.error_message error);
          match Semantic_function_call_expression_result.error_origin error with
          | Some (Semantic_symbol.Source_location location) ->
              Alcotest.(check int)
                (label ^ " update points at the operator")
                (substring_start source spelling)
                location.span.start
          | Some
              (Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _)
          | None -> Alcotest.fail "expected an update operator origin"))

let included_and_generated_updates_are_deterministic () =
  with_included_source
    "extern I64 Target(I64 value);I64 Caller(I64 value){return \
     Target(value++);}" (fun prepared ->
      let _, results = analyze prepared in
      let root = root_results results "Caller" |> List.hd in
      match
        root |> Semantic_function_call_expression_result.result_source
        |> Semantic_function_call_resolution.argument_expression_kind
      with
      | Semantic_function_call_resolution.Postfix_expression postfix -> (
          match
            Semantic_function_call_resolution.postfix_operator_origin postfix
          with
          | Semantic_symbol.Source_location location ->
              let source_file =
                Source_manager.find
                  (Session.sources prepared.session)
                  location.span.source
                |> Option.get
              in
              Alcotest.(check string)
                "included update operator keeps its source file" "calls.HC"
                (Source_file.path source_file |> Filename.basename)
          | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
              Alcotest.fail "expected an included update operator origin")
      | _ -> Alcotest.fail "expected an included postfix update");
  let prepared =
    prepare ~path:"call-expression-generated-update.HC"
      "#define UPDATE ++value\n\
       extern I64 Target(I64 value);I64 Caller(I64 value){return \
       Target(UPDATE);}"
  in
  let policies =
    Test_function_call_conversion_policy.analyze prepared
    |> Test_function_call_conversion_policy.checked_policy
  in
  let table = Session.semantic_symbols prepared.session in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  let analyze_once () =
    Holyc_lib.type_function_call_expressions prepared.session
      ~members:prepared.members ~policies
    |> checked_results
  in
  let first = analyze_once () in
  let second = analyze_once () in
  let describe results =
    results |> Semantic_function_call_expression_result.all_results
    |> List.map (fun result ->
        ( result |> Semantic_function_call_expression_result.result_id
          |> Semantic_function_call_expression_result.Id.to_int,
          result |> type_name,
          result |> Semantic_function_call_expression_result.result_category
          |> Semantic_function_call_expression_result.value_category_name ))
  in
  Alcotest.(check (list (triple int string string)))
    "generated update typing replays identically" (describe first)
    (describe second);
  Alcotest.(check int)
    "update typing leaves the symbol table unchanged" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length);
  let root = root_results first "Caller" |> List.hd in
  match
    root |> Semantic_function_call_expression_result.result_source
    |> Semantic_function_call_resolution.argument_expression_kind
  with
  | Semantic_function_call_resolution.Prefix_expression prefix -> (
      match Semantic_function_call_resolution.prefix_operator_origin prefix with
      | Semantic_symbol.Source_location location ->
          Alcotest.(check bool)
            "generated update operator keeps its invocation" true
            (Option.is_some location.generated_from);
          Alcotest.(check bool)
            "generated update operator keeps its definition" true
            (Option.is_some location.defined_at)
      | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
          Alcotest.fail "expected a generated update operator origin")
  | _ -> Alcotest.fail "expected a generated prefix update"

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
    Holyc_lib.type_function_call_expressions prepared.session
      ~members:prepared.members ~policies
    |> checked_results
  in
  let second =
    Holyc_lib.type_function_call_expressions prepared.session
      ~members:prepared.members ~policies
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

let variadic_expressions_receive_typed_results () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-expression-variadic.HC"
          "class Box {F64 value;};\n\
           extern I64 Mix(F64 fixed=1,...);\n\
           I64 Caller(I64 value,F64 floating,I64 *pointer,Box box){I64 \
           array[1];return \
           Mix(,1,2.5,'A',\"x\",value,floating,&value,*pointer,array[0],box.value,value+1,value(F64),++value,value++);}"
      in
      let _, results = analyze prepared in
      let variadic = variadic_results results "Caller" in
      Alcotest.(check int)
        "every provided variadic expression receives a result" 14
        (List.length variadic);
      Alcotest.(check (list string))
        "variadic roots retain source types"
        [
          "I64";
          "F64";
          "I64";
          "U8*";
          "I64";
          "F64";
          "I64*";
          "I64";
          "I64";
          "F64";
          "I64";
          "F64";
          "I64";
          "I64";
        ]
        (List.map type_name variadic);
      Alcotest.(check (list string))
        "variadic roots retain actual result classes"
        [
          "integer-result";
          "f64-result";
          "integer-result";
          "integer-result";
          "integer-result";
          "f64-result";
          "integer-result";
          "integer-result";
          "integer-result";
          "f64-result";
          "integer-result";
          "f64-result";
          "integer-result";
          "integer-result";
        ]
        (class_names variadic);
      Alcotest.(check (list string))
        "variadic roots retain value categories"
        [
          "object-value";
          "object-value";
          "object-value";
          "address-value";
          "object-value";
          "object-value";
          "address-value";
          "object-value";
          "object-value";
          "object-value";
          "object-value";
          "object-value";
          "object-value";
          "object-value";
        ]
        (category_names variadic);
      Alcotest.(check (list int))
        "variadic roots retain their completed array ranks"
        (List.init 14 (Fun.const 0))
        (array_ranks variadic);
      let ids =
        variadic
        |> List.map (fun result ->
            result |> Semantic_function_call_expression_result.result_id
            |> Semantic_function_call_expression_result.Id.to_int)
      in
      Alcotest.(check (list int))
        "variadic root identities remain in source order" ids
        (List.sort Int.compare ids);
      match
        only_direct results "Caller"
        |> Semantic_function_call_expression_result.direct_fixed_results
      with
      | [ fixed ] -> (
          match Semantic_function_call_expression_result.fixed_path fixed with
          | Semantic_function_call_expression_result.Declared_default_result _
            -> ()
          | Semantic_function_call_expression_result.Provided_result _ ->
              Alcotest.fail "expected the fixed default to remain separate")
      | fixed ->
          Alcotest.failf "expected one fixed default, got %d"
            (List.length fixed))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let variadic_origins_and_replay_are_deterministic () =
  with_included_source
    "extern I64 Mix(I64 fixed=0,...);I64 Caller(){return Mix(,2.5);}"
    (fun prepared ->
      let _, results = analyze prepared in
      match variadic_results results "Caller" with
      | [ result ] -> (
          match
            Semantic_function_call_expression_result.result_origin result
          with
          | Semantic_symbol.Source_location location ->
              let source =
                Source_manager.find
                  (Session.sources prepared.session)
                  location.span.source
                |> Option.get
              in
              Alcotest.(check string)
                "included variadic result keeps its source file" "calls.HC"
                (Source_file.path source |> Filename.basename)
          | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
              Alcotest.fail "expected an included variadic result origin")
      | results ->
          Alcotest.failf "expected one included variadic result, got %d"
            (List.length results));
  let prepared =
    prepare ~path:"call-expression-generated-variadic.HC"
      "#define ARG 2.5\n\
       extern I64 Mix(I64 fixed=0,...);I64 Caller(){return Mix(,ARG);}"
  in
  let policies =
    Test_function_call_conversion_policy.analyze prepared
    |> Test_function_call_conversion_policy.checked_policy
  in
  let table = Session.semantic_symbols prepared.session in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  let run () =
    Holyc_lib.type_function_call_expressions prepared.session
      ~members:prepared.members ~policies
    |> checked_results
  in
  let first = run () in
  let second = run () in
  let describe results =
    variadic_results results "Caller"
    |> List.map (fun result ->
        ( result |> Semantic_function_call_expression_result.result_id
          |> Semantic_function_call_expression_result.Id.to_int,
          result |> Semantic_function_call_expression_result.result_class
          |> Semantic_function_call_expression_result.result_class_name ))
  in
  Alcotest.(check (list (pair int string)))
    "variadic typing replays identically" (describe first) (describe second);
  Alcotest.(check int)
    "variadic typing leaves the symbol table unchanged" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length);
  match variadic_results first "Caller" with
  | [ result ] -> (
      match Semantic_function_call_expression_result.result_origin result with
      | Semantic_symbol.Source_location location ->
          Alcotest.(check bool)
            "generated variadic result keeps its invocation" true
            (Option.is_some location.generated_from);
          Alcotest.(check bool)
            "generated variadic result keeps its definition" true
            (Option.is_some location.defined_at)
      | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
          Alcotest.fail "expected generated variadic result provenance")
  | results ->
      Alcotest.failf "expected one generated variadic result, got %d"
        (List.length results)

let nested_direct_calls_retain_return_results () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-expression-nested-results.HC"
          "F64 class FloatBox {};\n\
           extern I64 MakeInt();extern F64 MakeFloat();extern I64 *MakePtr();\n\
           extern FloatBox MakeBox();\n\
           extern I64 Target(F64 integer,I64 floating,I64 pointer,F64 box);\n\
           I64 Caller(){return \
           Target(MakeInt(),MakeFloat(),MakePtr(),MakeBox());}"
      in
      let policies, results = analyze prepared in
      let roots = direct_named results "Caller" "Target" |> provided_results in
      Alcotest.(check (list string))
        "nested calls retain source-visible return types"
        [ "I64"; "F64"; "I64*"; "FloatBox" ]
        (List.map type_name roots);
      Alcotest.(check (list string))
        "pointer and object call results remain distinct"
        [ "object-value"; "object-value"; "address-value"; "object-value" ]
        (category_names roots);
      Alcotest.(check (list string))
        "aggregate returns follow their visible backing"
        [ "integer-result"; "f64-result"; "integer-result"; "f64-result" ]
        (class_names roots);
      let nested_names_and_indexes =
        roots
        |> List.map (fun result ->
            match
              Semantic_function_call_expression_result.result_call_resolution
                result
            with
            | Some (Semantic_function_call_resolution.Direct_call direct) ->
                let call =
                  Semantic_function_call_resolution.direct_source direct
                in
                ( Semantic_function_call_resolution.call_callee_name call,
                  Semantic_function_call_resolution.call_index call )
            | Some (Semantic_function_call_resolution.Deferred_call _) ->
                Alcotest.fail "expected a retained direct nested call"
            | Some (Semantic_function_call_resolution.Indirect_call _) ->
                Alcotest.fail "expected a retained direct nested call"
            | None -> Alcotest.fail "expected a retained nested call result")
      in
      Alcotest.(check (list (pair string int)))
        "nested call identities follow source order"
        [ ("MakeInt", 1); ("MakeFloat", 2); ("MakePtr", 3); ("MakeBox", 4) ]
        nested_names_and_indexes;
      let decisions =
        Holyc_lib.decide_function_call_conversions prepared.session ~policies
          ~expressions:results
        |> checked_decision
      in
      let paths =
        decision_direct_named decisions "Caller" "Target"
        |> Semantic_function_call_conversion_decision.direct_fixed_decisions
        |> List.map (fun fixed ->
            fixed |> Semantic_function_call_conversion_decision.fixed_path
            |> Semantic_function_call_conversion_decision.fixed_path_name)
      in
      Alcotest.(check (list string))
        "outer fixed calls consume nested return classes"
        [
          "provided:integer-result:ICF_RES_TO_F64";
          "provided:f64-result:ICF_RES_TO_INT";
          "provided:integer-result:none";
          "provided:f64-result:none";
        ]
        paths)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let nested_calls_use_source_visible_headers_and_variadic_paths () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-expression-nested-visible.HC"
          "extern I64 Target(F64 value);extern I64 Sink(I64 fixed=0,...);\n\
           extern I64 Choice();I64 Before(){return Target(Choice());}\n\
           extern F64 Choice();I64 After(){return Target(Choice());}\n\
           I64 Variadic(){return Sink(,Choice());}"
      in
      let policies, results = analyze prepared in
      let before =
        direct_named results "Before" "Target" |> provided_results |> List.hd
      in
      let after =
        direct_named results "After" "Target" |> provided_results |> List.hd
      in
      Alcotest.(check (list string))
        "replacement headers do not leak backward" [ "I64"; "F64" ]
        [ type_name before; type_name after ];
      let variadic =
        direct_named results "Variadic" "Sink"
        |> Semantic_function_call_expression_result.direct_variadic_results
        |> List.hd
      in
      Alcotest.(check string)
        "a nested variadic call retains its actual class" "f64-result"
        (variadic |> Semantic_function_call_expression_result.result_class
       |> Semantic_function_call_expression_result.result_class_name);
      let decisions =
        Holyc_lib.decide_function_call_conversions prepared.session ~policies
          ~expressions:results
        |> checked_decision
      in
      let variadic_decision =
        decision_direct_named decisions "Variadic" "Sink"
        |> Semantic_function_call_conversion_decision.direct_variadic_decisions
        |> List.hd
      in
      Alcotest.(check string)
        "the variadic decision has no fixed target conversion" "f64-result"
        (variadic_decision
       |> Semantic_function_call_conversion_decision.variadic_actual
       |> Semantic_function_call_conversion_decision.actual_class_name);
      Alcotest.(check bool)
        "the variadic decision keeps the exact nested result" true
        (Semantic_function_call_conversion_decision.variadic_actual_result
           variadic_decision
        == variadic))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let nested_indirect_calls_use_callback_return_headers () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-expression-nested-indirect.HC"
          "F64 class FloatBox {};\n\
           extern I64 Target(F64 integer,I64 floating,I64 pointer,F64 box);\n\
           extern I64 Sink(I64 fixed=0,...);\n\
           I64 Caller(I64 (*MakeInt)(),F64 (*MakeFloat)(),\n\
           I64 *(*MakePtr)(),FloatBox (*MakeBox)()){\n\
           Target((*MakeInt)(),MakeFloat(),MakePtr(),MakeBox());\n\
           return Sink(,MakeFloat());}"
      in
      let policies, results = analyze prepared in
      let roots = direct_named results "Caller" "Target" |> provided_results in
      Alcotest.(check (list string))
        "callback calls keep their declared return types"
        [ "I64"; "F64"; "I64*"; "FloatBox" ]
        (List.map type_name roots);
      Alcotest.(check (list string))
        "callback pointer and object returns remain distinct"
        [ "object-value"; "object-value"; "address-value"; "object-value" ]
        (category_names roots);
      Alcotest.(check (list string))
        "callback aggregate returns follow their backing class"
        [ "integer-result"; "f64-result"; "integer-result"; "f64-result" ]
        (class_names roots);
      roots
      |> List.iter (fun result ->
          match
            Semantic_function_call_expression_result.result_call_resolution
              result
          with
          | Some (Semantic_function_call_resolution.Indirect_call _) -> ()
          | Some (Semantic_function_call_resolution.Direct_call _)
          | Some (Semantic_function_call_resolution.Deferred_call _)
          | None -> Alcotest.fail "expected a retained indirect call identity");
      let decisions =
        Holyc_lib.decide_function_call_conversions prepared.session ~policies
          ~expressions:results
        |> checked_decision
      in
      let paths =
        decision_direct_named decisions "Caller" "Target"
        |> Semantic_function_call_conversion_decision.direct_fixed_decisions
        |> List.map (fun fixed ->
            fixed |> Semantic_function_call_conversion_decision.fixed_path
            |> Semantic_function_call_conversion_decision.fixed_path_name)
      in
      Alcotest.(check (list string))
        "outer fixed calls consume callback return classes"
        [
          "provided:integer-result:ICF_RES_TO_F64";
          "provided:f64-result:ICF_RES_TO_INT";
          "provided:integer-result:none";
          "provided:f64-result:none";
        ]
        paths;
      let sink_variadic =
        direct_named results "Caller" "Sink"
        |> Semantic_function_call_expression_result.direct_variadic_results
        |> List.hd
      in
      Alcotest.(check string)
        "a variadic callback result keeps its actual class" "f64-result"
        (sink_variadic |> Semantic_function_call_expression_result.result_class
       |> Semantic_function_call_expression_result.result_class_name))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let indirect_included_and_generated_nested_calls_keep_identity () =
  let prepared =
    prepare ~path:"call-expression-deferred-call.HC"
      "extern I64 Target(F64 value);I64 Caller(I64 (*callback)()){return \
       Target(callback());}"
  in
  let _, results = analyze prepared in
  let indirect_result =
    direct_named results "Caller" "Target" |> provided_results |> List.hd
  in
  Alcotest.(check (pair string string))
    "a callback call uses its header return type" ("I64", "object-value")
    ( type_name indirect_result,
      indirect_result
      |> Semantic_function_call_expression_result.result_category
      |> Semantic_function_call_expression_result.value_category_name );
  (match
     Semantic_function_call_expression_result.result_call_resolution
       indirect_result
   with
  | Some (Semantic_function_call_resolution.Indirect_call indirect) ->
      let call = Semantic_function_call_resolution.indirect_source indirect in
      Alcotest.(check string)
        "the callback call keeps its callee form" "identifier"
        (call |> Semantic_function_call_resolution.call_callee_form
       |> Semantic_function_call_resolution.callee_form_name)
  | Some (Semantic_function_call_resolution.Direct_call _) ->
      Alcotest.fail "expected a retained indirect callback call"
  | Some (Semantic_function_call_resolution.Deferred_call _) ->
      Alcotest.fail "expected the typed callback call to resolve"
  | None -> Alcotest.fail "expected the indirect call identity to be retained");
  with_included_source
    "extern I64 Target(I64 value);\n\
     I64 Caller(F64 (*callback)()){return Target(callback());}" (fun included ->
      let _, results = analyze included in
      let result =
        direct_named results "Caller" "Target" |> provided_results |> List.hd
      in
      match
        Semantic_function_call_expression_result.result_call_resolution result
      with
      | Some (Semantic_function_call_resolution.Indirect_call indirect) ->
          let call =
            Semantic_function_call_resolution.indirect_source indirect
          in
          let source =
            Source_manager.find
              (Session.sources included.session)
              ( Semantic_function_call_expression_result.result_origin result
              |> function
                | Semantic_symbol.Source_location location ->
                    location.span.source
                | Semantic_symbol.Pinned_source _
                | Semantic_symbol.Synthesized _ ->
                    Alcotest.fail "expected an included nested call origin" )
            |> Option.get
          in
          Alcotest.(check string)
            "included nested call keeps its source file" "calls.HC"
            (Source_file.path source |> Filename.basename);
          Alcotest.(check string)
            "included nested call keeps its indirect identity" "callback"
            (Semantic_function_call_resolution.call_callee_name call)
      | Some (Semantic_function_call_resolution.Direct_call _)
      | Some (Semantic_function_call_resolution.Deferred_call _) ->
          Alcotest.fail "expected an included indirect nested call"
      | None -> Alcotest.fail "expected an included nested call identity");
  let generated =
    prepare ~path:"call-expression-generated-call.HC"
      "#define CALL callback()\n\
       extern I64 Target(I64 value);\n\
       I64 Caller(F64 (*callback)()){return Target(CALL);}"
  in
  let policies =
    Test_function_call_conversion_policy.analyze generated
    |> Test_function_call_conversion_policy.checked_policy
  in
  let table = Session.semantic_symbols generated.session in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  let run () =
    Holyc_lib.type_function_call_expressions generated.session
      ~members:generated.members ~policies
    |> checked_results
  in
  let first = run () in
  let second = run () in
  let describe results =
    direct_named results "Caller" "Target" |> provided_results |> List.hd
    |> fun result ->
    ( result |> Semantic_function_call_expression_result.result_id
      |> Semantic_function_call_expression_result.Id.to_int,
      type_name result,
      result |> Semantic_function_call_expression_result.result_class
      |> Semantic_function_call_expression_result.result_class_name )
  in
  Alcotest.(check (triple int string string))
    "generated nested call typing replays identically" (describe first)
    (describe second);
  Alcotest.(check int)
    "nested call typing leaves the symbol table unchanged" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length);
  let result =
    direct_named first "Caller" "Target" |> provided_results |> List.hd
  in
  match Semantic_function_call_expression_result.result_origin result with
  | Semantic_symbol.Source_location location ->
      Alcotest.(check bool)
        "generated nested call keeps its invocation" true
        (Option.is_some location.generated_from);
      Alcotest.(check bool)
        "generated nested call keeps its definition" true
        (Option.is_some location.defined_at)
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected generated nested call provenance"

let selected_defaults_retain_semantic_results () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-expression-default-results.HC"
          "I64i union ScalarBack {};\n\
           U0 Target(I64 integer=1,F64 floating=2.0,U8 \
           *text=\"fallback\",ScalarBack backed=0(ScalarBack),U8 \
           *name=lastclass);\n\
           U0 AllDefault(I64 value=1);\n\
           U0 Caller(U0 (*callback)(I64 integer=1,F64 floating=2.0,U8 \
           *text=\"fallback\",U8 *name=lastclass)){\n\
           Target(,,,,);AllDefault;callback(,,,);return;\n\
           }"
      in
      let _, results = analyze prepared in
      let direct_defaults callee =
        direct_named results "Caller" callee
        |> Semantic_function_call_expression_result.direct_fixed_results
        |> declared_defaults
      in
      let indirect_defaults =
        indirect_named results "Caller" "callback"
        |> Semantic_function_call_expression_result.indirect_fixed_results
        |> declared_defaults
      in
      let describe defaults =
        List.map
          (fun (_, result) ->
            Printf.sprintf "%s|%s|%s|%s"
              (result
             |> Semantic_function_call_expression_result.declared_default_type
             |> semantic_type_name)
              (result
             |> Semantic_function_call_expression_result.declared_default_class
             |> Semantic_function_call_expression_result.result_class_name)
              (result
             |> Semantic_function_call_expression_result.declared_default_kind
             |> Semantic_function_call_expression_result
                .declared_default_kind_name)
              (result
             |> Semantic_function_call_expression_result
                .declared_default_materialization
             |> Semantic_function_call_expression_result
                .declared_default_materialization_name))
          defaults
      in
      let materialization string_path =
        match mode with
        | Preprocessor.Jit -> "immediate"
        | Preprocessor.Aot -> string_path
      in
      Alcotest.(check (list string))
        "direct defaults retain their type, class, kind, and materialization"
        [
          "I64|integer-result|expression|immediate";
          "F64|f64-result|expression|immediate";
          Printf.sprintf "U8*|integer-result|expression|%s"
            (materialization "aot-string-constant");
          "ScalarBack|integer-result|expression|immediate";
          Printf.sprintf "U8*|integer-result|lastclass|%s"
            (materialization "aot-string-constant");
        ]
        (describe (direct_defaults "Target"));
      Alcotest.(check (list string))
        "typed indirect defaults use the same semantic result"
        [
          "I64|integer-result|expression|immediate";
          "F64|f64-result|expression|immediate";
          Printf.sprintf "U8*|integer-result|expression|%s"
            (materialization "aot-string-constant");
          Printf.sprintf "U8*|integer-result|lastclass|%s"
            (materialization "aot-string-constant");
        ]
        (describe indirect_defaults);
      Alcotest.(check int)
        "parenthesis-free calls select one typed default" 1
        (List.length (direct_defaults "AllDefault"));
      List.iter
        (fun (fixed, result) ->
          let source =
            fixed |> Semantic_function_call_expression_result.fixed_source
            |> Semantic_function_call_conversion_policy.fixed_source
          in
          let expected_parameter =
            Semantic_function_call_resolution.fixed_parameter source
          in
          let expected_default =
            match Semantic_function_call_resolution.fixed_value source with
            | Semantic_function_call_resolution.Declared_default default ->
                default
            | Semantic_function_call_resolution.Provided_argument _ ->
                Alcotest.fail "expected a selected declared default"
          in
          Alcotest.(check bool)
            "default result keeps the exact selected parameter" true
            (expected_parameter
            == Semantic_function_call_expression_result
               .declared_default_parameter result);
          Alcotest.(check bool)
            "default result keeps the exact selected source" true
            (expected_default
            == Semantic_function_call_expression_result.declared_default_source
                 result))
        (direct_defaults "Target" @ indirect_defaults);
      Alcotest.(check int)
        "selected defaults do not manufacture expression identities" 0
        (results |> Semantic_function_call_expression_result.all_results
       |> List.length))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let selected_default_provenance_and_replay_are_deterministic () =
  with_included_source "U0 Target(U8 *text=\"fallback\");U0 Caller(){Target();}"
    (fun included ->
      let _, results = analyze included in
      let default =
        direct_named results "Caller" "Target"
        |> Semantic_function_call_expression_result.direct_fixed_results
        |> declared_defaults |> List.hd |> snd
      in
      match
        default
        |> Semantic_function_call_expression_result.declared_default_source
        |> Semantic_function_call_resolution.default_parameter_default
      with
      | Semantic_function_type_resolution.Expression_default { origin; _ } -> (
          match origin with
          | Semantic_symbol.Source_location location ->
              let source =
                Source_manager.find
                  (Session.sources included.session)
                  location.span.source
                |> Option.get
              in
              Alcotest.(check string)
                "included default keeps its source file" "calls.HC"
                (Source_file.path source |> Filename.basename)
          | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
              Alcotest.fail "expected included default provenance")
      | Semantic_function_type_resolution.Lastclass_default _ ->
          Alcotest.fail "expected an expression default");
  let prepared =
    prepare ~mode:Preprocessor.Aot
      ~path:"call-expression-generated-default-result.HC"
      "#define FALLBACK \"fallback\"\n\
       U0 Target(U8 *text=FALLBACK);U0 Caller(){Target();}"
  in
  let policies =
    Test_function_call_conversion_policy.analyze prepared
    |> Test_function_call_conversion_policy.checked_policy
  in
  let table = Session.semantic_symbols prepared.session in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  let run () =
    Holyc_lib.type_function_call_expressions prepared.session
      ~members:prepared.members ~policies
    |> checked_results
  in
  let describe results =
    let default =
      direct_named results "Caller" "Target"
      |> Semantic_function_call_expression_result.direct_fixed_results
      |> declared_defaults |> List.hd |> snd
    in
    Printf.sprintf "%s|%s|%s|%s"
      (default |> Semantic_function_call_expression_result.declared_default_type
     |> semantic_type_name)
      (default
     |> Semantic_function_call_expression_result.declared_default_class
     |> Semantic_function_call_expression_result.result_class_name)
      (default |> Semantic_function_call_expression_result.declared_default_kind
     |> Semantic_function_call_expression_result.declared_default_kind_name)
      (default
     |> Semantic_function_call_expression_result
        .declared_default_materialization
     |> Semantic_function_call_expression_result
        .declared_default_materialization_name)
  in
  let first = run () in
  let second = run () in
  Alcotest.(check string)
    "generated default typing replays identically" (describe first)
    (describe second);
  Alcotest.(check int)
    "default typing leaves the symbol table unchanged" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length);
  let default =
    direct_named first "Caller" "Target"
    |> Semantic_function_call_expression_result.direct_fixed_results
    |> declared_defaults |> List.hd |> snd
  in
  match
    default |> Semantic_function_call_expression_result.declared_default_source
    |> Semantic_function_call_resolution.default_parameter_default
  with
  | Semantic_function_type_resolution.Expression_default
      { expression_origin; _ } -> (
      match expression_origin with
      | Semantic_symbol.Source_location location ->
          Alcotest.(check bool)
            "definition-backed default keeps its definition" true
            (Option.is_some location.defined_at)
      | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
          Alcotest.fail "expected generated default provenance")
  | Semantic_function_type_resolution.Lastclass_default _ ->
      Alcotest.fail "expected a generated expression default"

let lastclass_defaults_follow_previous_provided_results () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-expression-lastclass.HC"
          "class Student {};\n\
           I64i union ScalarBack {};\n\
           Student *Make();\n\
           U0 StudentDefaults(Student *value,U8 *name=lastclass,I64 \
           ordinary=1,U8 *again=lastclass);\n\
           U0 NumberDefaults(I64 *value,U8 *name=lastclass);\n\
           U0 ScalarDefaults(I64 value,U8 *name=lastclass);\n\
           U0 FloatDefaults(F64 value,U8 *name=lastclass);\n\
           U0 NestedDefaults(Student *value,U8 *name=lastclass);\n\
           U0 BackedDefaults(ScalarBack value,U8 *name=lastclass);\n\
           U0 Empty(U8 *name=lastclass);\n\
           U0 Caller(Student *student,I64 *number,I64 scalar,F64 float,U0 \
           (*callback)(Student *value,U8 *name=lastclass)){\n\
           StudentDefaults(student,,,);NumberDefaults(number,);\n\
           ScalarDefaults(scalar,);FloatDefaults(float,);\n\
           NestedDefaults(Make(),);BackedDefaults(0(ScalarBack),);\n\
           Empty();callback(student,);return;\n\
           }"
      in
      let _, results = analyze prepared in
      let direct_fixed callee =
        direct_named results "Caller" callee
        |> Semantic_function_call_expression_result.direct_fixed_results
      in
      Alcotest.(check (list (option string)))
        "ordinary defaults do not replace the previous provided class"
        [ Some "Student"; Some "Student" ]
        (lastclass_names (direct_fixed "StudentDefaults"));
      let substitutions =
        direct_fixed "StudentDefaults" |> lastclass_substitutions
      in
      let previous_ids =
        List.map
          (fun substitution ->
            substitution
            |> Semantic_function_call_expression_result
               .lastclass_previous_result
            |> Option.map (fun result ->
                result |> Semantic_function_call_expression_result.result_id
                |> Semantic_function_call_expression_result.Id.to_int))
          substitutions
      in
      Alcotest.(check (list (option int)))
        "both lastclass defaults retain the same provided expression"
        [ Some 0; Some 0 ] previous_ids;
      Alcotest.(check (list (option string)))
        "public primitive pointers keep their public base spelling"
        [ Some "I64" ]
        (lastclass_names (direct_fixed "NumberDefaults"));
      Alcotest.(check (list (option string)))
        "public union scalars forward to their storage class" [ Some "I64i" ]
        (lastclass_names (direct_fixed "ScalarDefaults"));
      Alcotest.(check (list (option string)))
        "direct public scalar classes keep their spelling" [ Some "F64" ]
        (lastclass_names (direct_fixed "FloatDefaults"));
      Alcotest.(check (list (option string)))
        "nested call return types supply lastclass" [ Some "Student" ]
        (lastclass_names (direct_fixed "NestedDefaults"));
      Alcotest.(check (list (option string)))
        "by-value aggregate backings supply the forwarded class name"
        [ Some "I64i" ]
        (lastclass_names (direct_fixed "BackedDefaults"));
      Alcotest.(check (list (option string)))
        "a missing previous expression stays explicit" [ None ]
        (lastclass_names (direct_fixed "Empty"));
      let indirect_fixed =
        indirect_named results "Caller" "callback"
        |> Semantic_function_call_expression_result.indirect_fixed_results
      in
      Alcotest.(check (list (option string)))
        "typed indirect calls use the same lastclass state" [ Some "Student" ]
        (lastclass_names indirect_fixed))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let lastclass_provenance_and_replay_are_deterministic () =
  with_included_source
    "class Student {};U0 Target(Student *value,U8 *name=lastclass);U0 \
     Caller(Student *student){Target(student,);}" (fun included ->
      let _, results = analyze included in
      let substitution =
        direct_named results "Caller" "Target"
        |> Semantic_function_call_expression_result.direct_fixed_results
        |> lastclass_substitutions |> List.hd
      in
      match
        Semantic_function_call_expression_result.lastclass_previous_result
          substitution
      with
      | None -> Alcotest.fail "expected an included previous call result"
      | Some result -> (
          match
            Semantic_function_call_expression_result.result_origin result
          with
          | Semantic_symbol.Source_location location ->
              let source =
                Source_manager.find
                  (Session.sources included.session)
                  location.span.source
                |> Option.get
              in
              Alcotest.(check string)
                "included lastclass input keeps its source file" "calls.HC"
                (Source_file.path source |> Filename.basename)
          | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
              Alcotest.fail "expected included lastclass provenance"));
  let generated =
    prepare ~path:"call-expression-generated-lastclass.HC"
      "class Student {};\n\
       U0 Target(Student *value,U8 *name=lastclass);\n\
       #define ARG student\n\
       U0 Caller(Student *student){Target(ARG,);}"
  in
  let policies =
    Test_function_call_conversion_policy.analyze generated
    |> Test_function_call_conversion_policy.checked_policy
  in
  let table = Session.semantic_symbols generated.session in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  let run () =
    Holyc_lib.type_function_call_expressions generated.session
      ~members:generated.members ~policies
    |> checked_results
  in
  let first = run () in
  let second = run () in
  let describe results =
    direct_named results "Caller" "Target"
    |> Semantic_function_call_expression_result.direct_fixed_results
    |> lastclass_substitutions
    |> List.map (fun substitution ->
        ( Semantic_function_call_expression_result.lastclass_previous_result
            substitution
          |> Option.map (fun result ->
              result |> Semantic_function_call_expression_result.result_id
              |> Semantic_function_call_expression_result.Id.to_int),
          Semantic_function_call_expression_result.lastclass_class_name
            substitution ))
  in
  Alcotest.(check (list (pair (option int) (option string))))
    "generated lastclass typing replays identically" (describe first)
    (describe second);
  Alcotest.(check int)
    "lastclass typing leaves the symbol table unchanged" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length);
  let substitution =
    direct_named first "Caller" "Target"
    |> Semantic_function_call_expression_result.direct_fixed_results
    |> lastclass_substitutions |> List.hd
  in
  match
    Semantic_function_call_expression_result.lastclass_previous_result
      substitution
  with
  | None -> Alcotest.fail "expected a generated previous call result"
  | Some result -> (
      match Semantic_function_call_expression_result.result_origin result with
      | Semantic_symbol.Source_location location ->
          Alcotest.(check bool)
            "generated lastclass input keeps its invocation" true
            (Option.is_some location.generated_from);
          Alcotest.(check bool)
            "generated lastclass input keeps its definition" true
            (Option.is_some location.defined_at)
      | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
          Alcotest.fail "expected generated lastclass provenance")

let unsupported_member_callback_calls_remain_unresolved () =
  let prepared =
    prepare ~path:"call-expression-member-callee.HC"
      "class Box {F64 (*callback)();};extern I64 Target(I64 value);\n\
       I64 Caller(Box box){return Target(box.callback());}"
  in
  let _, results = analyze prepared in
  let result =
    direct_named results "Caller" "Target" |> provided_results |> List.hd
  in
  Alcotest.(check (triple string string bool))
    "member callback calls are not assigned a guessed type"
    ("unavailable", "unresolved", false)
    ( result |> Semantic_function_call_expression_result.result_category
      |> Semantic_function_call_expression_result.value_category_name,
      result |> Semantic_function_call_expression_result.result_class
      |> Semantic_function_call_expression_result.result_class_name,
      result |> Semantic_function_call_expression_result.result_call_resolution
      |> Option.is_some )

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
     Holyc_lib.type_function_call_expressions foreign ~members:prepared.members
       ~policies:first_policies
   with
  | Ok _ -> Alcotest.fail "expected foreign expression typing to fail"
  | Error error ->
      Alcotest.(check string)
        "foreign session has a stable expression diagnostic" "HCSEMA0046"
        (Semantic_function_call_expression_result.error_code error));
  let other =
    prepare ~path:"call-expression-foreign-member-index.HC"
      "class Other {I64 value;};extern I64 Target(I64 value);I64 Caller(Other \
       other){return Target(other.value);}"
  in
  (match
     Holyc_lib.type_function_call_expressions prepared.session
       ~members:other.members ~policies:first_policies
   with
  | Ok _ -> Alcotest.fail "expected a foreign member index to fail"
  | Error error ->
      Alcotest.(check string)
        "foreign member index has a stable expression diagnostic" "HCSEMA0046"
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
    Alcotest.test_case "direct function address identity" `Quick
      direct_function_addresses_keep_publication_identity;
    Alcotest.test_case "function address extern paths" `Quick
      function_address_paths_follow_extern_state;
    Alcotest.test_case "rejected AOT function addresses" `Quick
      aot_and_internal_function_addresses_fail_explicitly;
    Alcotest.test_case "function address provenance and shadowing" `Quick
      function_address_provenance_and_storage_shadowing;
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
    Alcotest.test_case "member lookup identity and access kind" `Quick
      members_retain_lookup_identity_and_access_kind;
    Alcotest.test_case "member constructor validation" `Quick
      member_constructor_validates_names_and_origins;
    Alcotest.test_case "member arrays, callbacks, and lvalues" `Quick
      member_arrays_callbacks_and_lvalues_keep_their_shapes;
    Alcotest.test_case "member aggregate backing" `Quick
      member_result_uses_source_visible_backing;
    Alcotest.test_case "invalid member access origins" `Quick
      invalid_member_access_reports_the_operator_or_name;
    Alcotest.test_case "unresolved member base" `Quick
      unresolved_member_bases_remain_unavailable;
    Alcotest.test_case "included and generated member origins" `Quick
      included_and_generated_members_keep_their_origins;
    Alcotest.test_case "assignment operator inventory" `Quick
      assignment_operators_keep_destination_results;
    Alcotest.test_case "assignment storage and execution conversions" `Quick
      assignment_conversions_separate_storage_and_execution;
    Alcotest.test_case "assignment destination shapes" `Quick
      assignment_destinations_cover_identifiers_pointers_indexes_and_members;
    Alcotest.test_case "invalid assignment destination origins" `Quick
      invalid_assignment_destinations_report_the_operator;
    Alcotest.test_case "nested generated assignment determinism" `Quick
      nested_and_generated_assignments_are_deterministic;
    Alcotest.test_case "update operator storage results" `Quick
      update_operators_keep_storage_results;
    Alcotest.test_case "update destination shapes" `Quick
      update_destinations_cover_identifiers_dereferences_indexes_and_members;
    Alcotest.test_case "invalid update operand origins" `Quick
      invalid_update_operands_report_the_operator;
    Alcotest.test_case "included generated update determinism" `Quick
      included_and_generated_updates_are_deterministic;
    Alcotest.test_case "determinism and generated provenance" `Quick
      deterministic_generated_results_do_not_mutate_symbols;
    Alcotest.test_case "typed variadic expression results" `Quick
      variadic_expressions_receive_typed_results;
    Alcotest.test_case "variadic provenance and determinism" `Quick
      variadic_origins_and_replay_are_deterministic;
    Alcotest.test_case "nested direct-call return results" `Quick
      nested_direct_calls_retain_return_results;
    Alcotest.test_case "nested call headers and variadic paths" `Quick
      nested_calls_use_source_visible_headers_and_variadic_paths;
    Alcotest.test_case "nested indirect-call return results" `Quick
      nested_indirect_calls_use_callback_return_headers;
    Alcotest.test_case "indirect included generated nested calls" `Quick
      indirect_included_and_generated_nested_calls_keep_identity;
    Alcotest.test_case "selected default semantic results" `Quick
      selected_defaults_retain_semantic_results;
    Alcotest.test_case "selected default provenance and replay" `Quick
      selected_default_provenance_and_replay_are_deterministic;
    Alcotest.test_case "lastclass default substitution" `Quick
      lastclass_defaults_follow_previous_provided_results;
    Alcotest.test_case "lastclass provenance and replay" `Quick
      lastclass_provenance_and_replay_are_deterministic;
    Alcotest.test_case "unsupported member callback call" `Quick
      unsupported_member_callback_calls_remain_unresolved;
    Alcotest.test_case "ownership validation" `Quick
      foreign_session_and_traversal_are_rejected;
  ]
