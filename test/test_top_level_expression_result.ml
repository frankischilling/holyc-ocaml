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

type prepared_source = Test_function_call_conversion_policy.prepared

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

let checked_outer = function
  | Ok value -> value
  | Error error ->
      error |> Semantic_outer_environment.error_to_string |> Alcotest.fail

let typed_global_named (source : prepared_source) name =
  source.global_types |> Semantic_global_type_resolution.globals
  |> List.find (fun global ->
      global |> Semantic_global_type_resolution.global_symbol
      |> Semantic_symbol.name |> String.equal name)

let outer_global_metadata source template_name =
  let global = typed_global_named source template_name in
  let declarator_kind =
    match Semantic_global_type_resolution.global_declarator_kind global with
    | Semantic_global_type_resolution.Object ->
        Semantic_outer_environment.Object_global
    | Semantic_global_type_resolution.Function_pointer pointer ->
        Semantic_outer_environment.Function_pointer_global pointer
  in
  Semantic_outer_environment.make_global_metadata
    ~type_reference:
      (Semantic_global_type_resolution.global_type_reference global)
    ~declarator_kind
    ~array_rank:
      (global |> Semantic_global_type_resolution.global_array_dimensions
     |> List.length)
  |> checked_outer

let make_outer_entry (source : prepared_source) ~entry_index ~name ?metadata ()
    =
  let table = Session.semantic_symbols source.session in
  let symbol =
    Semantic_symbol_table.add table
      ~scope:(Semantic_symbol_table.root table)
      ~name ~kind:Semantic_symbol.Global_variable
      ~origin:(Semantic_symbol.Synthesized ("top-level outer " ^ name))
    |> checked
  in
  let entry =
    match metadata with
    | None ->
        Semantic_outer_environment.make_entry ~symbol
          ~record_kind:Semantic_outer_environment.Global_variable ~entry_index
    | Some global_metadata ->
        Semantic_outer_environment.make_global_entry ~symbol ~entry_index
          ~global_metadata
  in
  checked_outer entry

let outer_environment (source : prepared_source) entries =
  let make_table table_kind table_index entries =
    Semantic_outer_environment.make_table ~table_kind ~table_index entries
    |> checked_outer
  in
  let data_kind =
    match source.mode with
    | Preprocessor.Jit -> Semantic_outer_environment.Jit_task 0
    | Preprocessor.Aot -> Semantic_outer_environment.Aot_parent 0
  in
  let data = make_table data_kind 0 entries in
  let assembler = make_table Semantic_outer_environment.Assembler 1 [] in
  checked
    (Holyc_lib.create_outer_environment source.session
       ~compilation_mode:source.mode [ data; assembler ])

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

let binary_source result =
  match
    result |> Semantic_function_call_expression_result.result_source
    |> Semantic_function_call_resolution.argument_expression_kind
  with
  | Semantic_function_call_resolution.Binary_expression binary -> binary
  | _ -> Alcotest.fail "expected a retained top-level binary expression"

let binary_operands result =
  match
    Semantic_function_call_expression_result.result_binary_operands result
  with
  | Some operands -> operands
  | None -> Alcotest.fail "expected exact checked top-level binary operands"

let top_level_result_for_source results source =
  results |> Semantic_function_call_expression_result.top_level_all_results
  |> List.find (fun result ->
      Semantic_function_call_expression_result.result_source result == source)

let checked_top_level_source_operands results root =
  let left, right = binary_operands root in
  let binary = binary_source root in
  let expected_left =
    binary |> Semantic_function_call_resolution.binary_left
    |> top_level_result_for_source results
  in
  let expected_right =
    binary |> Semantic_function_call_resolution.binary_right
    |> top_level_result_for_source results
  in
  Alcotest.(check bool)
    "the top-level left child is the checked source result" true
    (left == expected_left);
  Alcotest.(check bool)
    "the top-level right child is the checked source result" true
    (right == expected_right);
  (left, right)

let rec literal_payload expression =
  match
    Semantic_function_call_resolution.argument_expression_kind expression
  with
  | Semantic_function_call_resolution.Integer_literal value ->
      Printf.sprintf "integer:%Ld" value
  | Semantic_function_call_resolution.Float_literal bits ->
      Printf.sprintf "f64:%016Lx" bits
  | Semantic_function_call_resolution.Character_literal value ->
      Printf.sprintf "character:%016Lx" value
  | Semantic_function_call_resolution.String_literal bytes ->
      Printf.sprintf "string:%S" bytes
  | Semantic_function_call_resolution.Parenthesized_expression grouped ->
      literal_payload grouped
  | Semantic_function_call_resolution.Prefix_expression prefix ->
      prefix |> Semantic_function_call_resolution.prefix_operand
      |> literal_payload
  | kind ->
      kind |> Semantic_function_call_resolution.argument_expression_kind_name
      |> Printf.sprintf "nonliteral:%s"

let rec literal_shape expression =
  match
    Semantic_function_call_resolution.argument_expression_kind expression
  with
  | Semantic_function_call_resolution.Parenthesized_expression grouped ->
      Printf.sprintf "parenthesized(%s)" (literal_shape grouped)
  | Semantic_function_call_resolution.Prefix_expression prefix ->
      Printf.sprintf "%s(%s)"
        (prefix |> Semantic_function_call_resolution.prefix_operator
       |> Semantic_function_call_resolution.prefix_operator_name)
        (prefix |> Semantic_function_call_resolution.prefix_operand
       |> literal_shape)
  | kind -> Semantic_function_call_resolution.argument_expression_kind_name kind

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

let top_level_outer_callback_name call =
  let occurrence =
    Semantic_function_call_expression_result.top_level_outer_callback_occurrence
      call
  in
  Semantic_top_level_outer_expression_binding.occurrence_name occurrence

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

let literal_payloads_reach_typed_top_level_results () =
  List.iter
    (fun mode ->
      let source =
        prepared ~mode ~path:"top-level-literal-payloads.HC"
          "0xFFFFFFFFFFFFFFFF;0x8000000000000000;-7;'ABC';0.1;\"a\\n\\x42\\d\";+((42));"
      in
      let _, _, _, result = analyze source in
      let values = root_values result in
      Alcotest.(check (list string))
        "typed top-level roots retain exact literal payloads"
        [
          "integer:-1";
          "integer:-9223372036854775808";
          "integer:7";
          "character:0000000000434241";
          Printf.sprintf "f64:%016Lx" (Int64.bits_of_float 0.1);
          "string:\"a\\nB$\"";
          "integer:42";
        ]
        (values
        |> List.map (fun value ->
            value |> Semantic_function_call_expression_result.result_source
            |> literal_payload));
      Alcotest.(check (list string))
        "top-level prefixes and grouping remain explicit"
        [
          "integer-literal";
          "integer-literal";
          "unary-minus(integer-literal)";
          "character-literal";
          "float-literal";
          "string-literal";
          "unary-plus(parenthesized(parenthesized(integer-literal)))";
        ]
        (values
        |> List.map (fun value ->
            value |> Semantic_function_call_expression_result.result_source
            |> literal_shape));
      Alcotest.(check (list string))
        "top-level literal payloads keep existing result metadata"
        [
          "I64:object-value:integer-result:rank-0";
          "I64:object-value:integer-result:rank-0";
          "I64:object-value:integer-result:rank-0";
          "I64:object-value:integer-result:rank-0";
          "F64:object-value:f64-result:rank-0";
          "U8*:address-value:integer-result:rank-0";
          "I64:object-value:integer-result:rank-0";
        ]
        (List.map descriptor values))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let top_level_current_positions_are_rip_addresses () =
  List.iter
    (fun mode ->
      let source =
        prepared ~mode ~path:"top-level-current-position-results.HC"
          "$$;($$);$$+1;"
      in
      let table = Session.semantic_symbols source.session in
      let symbol_count =
        Semantic_symbol_table.all_symbols table |> List.length
      in
      let _, _, _, first = analyze source in
      let _, _, _, second = analyze source in
      Alcotest.(check (list string))
        "top-level current positions keep RIP address results"
        [
          "I64:address-value:integer-result:rank-0";
          "I64:address-value:integer-result:rank-0";
          "I64:object-value:integer-result:rank-0";
        ]
        (root_values first |> List.map descriptor);
      let current_positions result =
        result |> Semantic_function_call_expression_result.top_level_all_results
        |> List.filter (fun expression ->
            match
              expression
              |> Semantic_function_call_expression_result.result_source
              |> Semantic_function_call_resolution.argument_expression_kind
            with
            | Semantic_function_call_resolution.Unresolved_expression
                Semantic_function_call_resolution.Current_position_expression ->
                true
            | _ -> false)
      in
      let positions = current_positions first in
      Alcotest.(check int)
        "every top-level current-position occurrence is typed" 3
        (List.length positions);
      List.iter
        (fun result ->
          Alcotest.(check string)
            "each current-position leaf is an address"
            "I64:address-value:integer-result:rank-0" (descriptor result);
          match Semantic_function_call_expression_result.result_type result with
          | Some type_ -> (
              Alcotest.(check int)
                "top-level RT_PTR has no source pointer layer" 0
                (Semantic_type.pointer_depth type_);
              match Semantic_type.base type_ with
              | Semantic_type.Primitive (form, primitive) ->
                  Alcotest.(check bool)
                    "top-level RT_PTR keeps the intrinsic storage form" true
                    (form = Semantic_type.Internal_storage);
                  Alcotest.(check bool)
                    "top-level RT_PTR uses the pinned RT_I64 slot" true
                    (Primitive_type.equal primitive Primitive_type.I64)
              | Semantic_type.Aggregate _ ->
                  Alcotest.fail "expected RT_PTR's intrinsic primitive type")
          | None -> Alcotest.fail "expected a top-level current-position type")
        positions;
      Alcotest.(check (list string))
        "top-level current-position typing replays identically"
        (root_values first |> List.map descriptor)
        (root_values second |> List.map descriptor);
      Alcotest.(check int)
        "top-level current-position typing leaves symbols unchanged"
        symbol_count
        (Semantic_symbol_table.all_symbols table |> List.length))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let top_level_outer_globals_retain_checked_shapes () =
  List.iter
    (fun mode ->
      let source =
        prepared ~mode ~path:"top-level-typed-outer-results.HC"
          "class Box {F64 value;};F64 TemplateF;I64 *TemplateP;I64 \
           TemplateArray[2];I64 (*TemplateCallback)(I64);Box TemplateBox;\n\
           (OuterF);(OuterP);(OuterArray);(OuterCallback);(OuterBox);"
      in
      let specifications =
        [
          ("OuterF", "TemplateF");
          ("OuterP", "TemplateP");
          ("OuterArray", "TemplateArray");
          ("OuterCallback", "TemplateCallback");
          ("OuterBox", "TemplateBox");
        ]
      in
      let entries =
        specifications
        |> List.mapi (fun entry_index (name, template_name) ->
            make_outer_entry source ~entry_index ~name
              ~metadata:(outer_global_metadata source template_name)
              ())
      in
      let environment = outer_environment source entries in
      let _, identifiers, _, result = analyze ~environment source in
      Alcotest.(check (list string))
        "outer globals use the ordinary top-level type lattice"
        [
          "F64:object-value:f64-result:rank-0";
          "I64*:object-value:integer-result:rank-0";
          "I64:array-value:integer-result:rank-1";
          "I64:callback-value:integer-result:rank-0";
          "Box:object-value:integer-result:rank-0";
        ]
        (root_values result |> List.map descriptor);
      let names =
        root_values result
        |> List.map (fun value ->
            let occurrence =
              let open Semantic_function_call_expression_result in
              value |> result_top_level_outer_occurrence |> Option.get
            in
            let binding =
              value
              |> Semantic_function_call_expression_result.result_outer_binding
              |> Option.get
            in
            Alcotest.(check bool)
              "function-body outer occurrence stays absent" true
              (Option.is_none
                 (Semantic_function_call_expression_result
                  .result_outer_occurrence value));
            let name =
              Semantic_top_level_outer_expression_binding.occurrence_name
                occurrence
            in
            let selected_name =
              binding |> Semantic_outer_environment.binding_entry
              |> Semantic_outer_environment.entry_symbol |> Semantic_symbol.name
            in
            Alcotest.(check string)
              "the result retains the selected outer record" name selected_name;
            let leaf =
              Semantic_top_level_identifier_resolution.find_leaf identifiers
                occurrence
              |> Option.get
            in
            (match
               Semantic_top_level_identifier_resolution.leaf_resolution leaf
             with
            | Semantic_top_level_identifier_resolution.Outer_value selected ->
                Alcotest.(check bool)
                  "the result and classifier share one binding" true
                  (selected == binding)
            | Semantic_top_level_identifier_resolution.Module_value _
            | Semantic_top_level_identifier_resolution.Outer_type_required _ ->
                Alcotest.fail "expected a typed outer classification");
            name)
      in
      Alcotest.(check (list string))
        "top-level outer provenance follows source order"
        [ "OuterF"; "OuterP"; "OuterArray"; "OuterCallback"; "OuterBox" ]
        names)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let metadata_free_top_level_outer_stays_unavailable () =
  let source = prepared ~path:"top-level-untyped-outer-result.HC" "(Outer);" in
  let entry = make_outer_entry source ~entry_index:0 ~name:"Outer" () in
  let environment = outer_environment source [ entry ] in
  let _, _, _, result = analyze ~environment source in
  match root_values result with
  | [ value ] ->
      Alcotest.(check string)
        "missing metadata does not invent a target type"
        "unavailable:unavailable:unresolved:rank-0" (descriptor value);
      Alcotest.(check bool)
        "untyped result retains its top-level occurrence" true
        (Option.is_some
           (Semantic_function_call_expression_result
            .result_top_level_outer_occurrence value));
      Alcotest.(check bool)
        "untyped result retains its outer binding" true
        (Option.is_some
           (Semantic_function_call_expression_result.result_outer_binding value))
  | values ->
      Alcotest.failf "expected one metadata-free result, got %d"
        (List.length values)

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

let top_level_binary_operands_are_shared_in_both_modes () =
  List.iter
    (fun mode ->
      let source =
        prepared ~mode ~path:"top-level-binary-operands.HC"
          "I64 left,right;F64 \
           floating;left=right=1;floating+=left;left+floating;left<right;left&&right;left;"
      in
      let _, _, _, results = analyze source in
      let values = root_values results in
      Alcotest.(check int)
        "five binary roots and one identifier are executable" 6
        (List.length values);
      let binary_roots = List.init 5 (List.nth values) in
      Alcotest.(check (list string))
        "top-level binary roots keep the requested operations"
        [ "IC_ASSIGN"; "IC_ADD_EQU"; "IC_ADD"; "IC_LESS"; "IC_AND_AND" ]
        (List.map
           (fun root ->
             root |> binary_source
             |> Semantic_function_call_resolution.binary_operator
             |> Semantic_function_call_resolution.binary_operator_name)
           binary_roots);
      let operands =
        List.map (checked_top_level_source_operands results) binary_roots
      in
      Alcotest.(check (list (pair string string)))
        "top-level binary operands stay in left-to-right source order"
        [
          ("I64", "I64");
          ("F64", "I64");
          ("I64", "F64");
          ("I64", "I64");
          ("I64", "I64");
        ]
        (List.map
           (fun (left, right) -> (type_name left, type_name right))
           operands);
      Alcotest.(check (list string))
        "top-level assignment destinations keep their lvalue category"
        [ "lvalue"; "lvalue"; "object-value"; "object-value"; "object-value" ]
        (operands
        |> List.map (fun (left, _) ->
            left |> Semantic_function_call_expression_result.result_category
            |> Semantic_function_call_expression_result.value_category_name));
      let compound_right = List.nth operands 1 |> snd in
      Alcotest.(check string)
        "the top-level compound right operand keeps its F64 conversion"
        "ICF_RES_TO_F64"
        (compound_right
       |> Semantic_function_call_expression_result.result_intrinsic_conversion
       |> Semantic_function_call_expression_result.intrinsic_conversion_name);
      Alcotest.(check bool)
        "a top-level identifier has no binary operand pair" true
        (List.nth values 5
       |> Semantic_function_call_expression_result.result_binary_operands
       |> Option.is_none))
    [ Preprocessor.Jit; Preprocessor.Aot ]

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
        |> Semantic_function_call_expression_result.intrinsic_conversion_name));
  List.iter
    (fun mode ->
      let source =
        prepared ~mode ~path:"top-level-postfix-cast-operands.HC"
          "I64 scalar;scalar(F64);"
      in
      let _, _, _, result = analyze source in
      let cast = root_values result |> List.hd in
      let source_operand =
        match
          cast |> Semantic_function_call_expression_result.result_source
          |> Semantic_function_call_resolution.argument_expression_kind
        with
        | Semantic_function_call_resolution.Postfix_cast_expression (operand, _)
          -> operand
        | _ -> Alcotest.fail "expected a top-level postfix cast"
      in
      let expected =
        result |> Semantic_function_call_expression_result.top_level_all_results
        |> List.find (fun candidate ->
            Semantic_function_call_expression_result.result_source candidate
            == source_operand)
      in
      let cast_operand cast =
        match Semantic_function_call_expression_result.result_operand cast with
        | Some operand -> operand
        | None ->
            Alcotest.fail "top-level postfix cast lost its checked operand"
      in
      let operand = cast_operand cast in
      Alcotest.(check bool)
        "top-level cast points to the exact checked source result" true
        (operand == expected);
      Alcotest.(check string)
        "top-level cast keeps its F64 target" "F64" (type_name cast);
      Alcotest.(check string)
        "top-level cast keeps its I64 operand" "I64" (type_name operand);
      (match Semantic_function_call_expression_result.result_origin operand with
      | Semantic_symbol.Source_location location ->
          let operand_source =
            Source_manager.find
              (Session.sources source.session)
              location.span.source
            |> Option.get
          in
          Alcotest.(check string)
            "top-level cast operand keeps its source file"
            "top-level-postfix-cast-operands.HC"
            (Source_file.path operand_source |> Filename.basename)
      | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
          Alcotest.fail "expected source-backed top-level cast operand");
      let edge result =
        let root = root_values result |> List.hd in
        let operand = cast_operand root in
        ( root |> Semantic_function_call_expression_result.result_id
          |> Semantic_function_call_expression_result.Id.to_int,
          operand |> Semantic_function_call_expression_result.result_id
          |> Semantic_function_call_expression_result.Id.to_int )
      in
      let expected_edge = edge result in
      let _, _, _, repeated = analyze source in
      Alcotest.(check (pair int int))
        "top-level cast edge replays deterministically" expected_edge
        (edge repeated))
    [ Preprocessor.Jit; Preprocessor.Aot ]

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

let standalone_top_level_offset_paths () =
  List.iter
    (fun mode ->
      let source =
        prepared ~mode ~path:"top-level-standalone-offset-paths.HC"
          "class Inner {I8 head;I64 value;};class Base {I8 inherited;};class \
           Box : Base {I16 prefix;Inner \
           inner;};offset(Box.prefix);((offset(((Box.inner.value)))));offset(Box.inherited);1+offset(Box.prefix);"
      in
      let _, _, _, result = analyze source in
      let offsets =
        completed_offsets result
        |> List.filter (fun value ->
            match
              value |> Semantic_function_call_expression_result.result_source
              |> Semantic_function_call_resolution.argument_expression_kind
            with
            | Semantic_function_call_resolution.Standalone_offset_expression _
              -> true
            | _ -> false)
      in
      Alcotest.(check (list string))
        "standalone top-level offsets retain ordered cumulative paths"
        [
          "Box[prefix:1]=1";
          "Box[inner:3/value:4]=4";
          "Box[inherited:0]=0";
          "Box[prefix:1]=1";
        ]
        (List.map offset_description offsets);
      let sources =
        offsets
        |> List.map (fun value ->
            value |> Semantic_function_call_expression_result.result_source
            |> Semantic_function_call_resolution.argument_expression_kind)
      in
      List.iter
        (function
          | Semantic_function_call_resolution.Standalone_offset_expression
              offset ->
              Alcotest.(check string)
                "top-level offset retains its root spelling" "Box"
                (Semantic_function_call_resolution.offset_target_spelling offset)
          | _ -> Alcotest.fail "expected a standalone top-level offset source")
        sources;
      let _, _, _, replayed = analyze source in
      let replayed_descriptions =
        completed_offsets replayed
        |> List.filter_map (fun value ->
            match
              value |> Semantic_function_call_expression_result.result_source
              |> Semantic_function_call_resolution.argument_expression_kind
            with
            | Semantic_function_call_resolution.Standalone_offset_expression _
              -> Some (offset_description value)
            | _ -> None)
      in
      Alcotest.(check (list string))
        "standalone top-level offsets replay deterministically"
        (List.map offset_description offsets)
        replayed_descriptions)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let invalid_standalone_top_level_offsets () =
  [
    ("unknown root", "offset(Missing.value);");
    ( "incomplete root",
      "extern class Later;offset(Later.value);class Later {I64 value;};" );
    ("missing member", "class Box {I64 value;};offset(Box.missing);");
    ( "pointer continuation",
      "class Box {I64 *pointer;};offset(Box.pointer.value);" );
  ]
  |> List.iter (fun (label, contents) ->
      let source =
        prepared
          ~path:("top-level-invalid-standalone-offset-" ^ label ^ ".HC")
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
      | Error error ->
          Alcotest.(check string)
            (label ^ " code") "HCSEMA0046"
            (Semantic_function_call_expression_result.error_code error)
      | Ok _ -> Alcotest.failf "expected standalone %s offset to fail" label)

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
          let actual_message =
            Semantic_function_call_expression_result.error_message error
          in
          Alcotest.(check string)
            (label ^ " message") expected_message actual_message)

let top_level_outer_callback_calls () =
  List.iter
    (fun mode ->
      let source =
        prepared ~mode ~path:"top-level-outer-callbacks.HC"
          "F64 class CallbackBacked {};\n\
           I64 (*TemplateInt)(I64 first,I64 second=2,I64 third);\n\
           F64 (*TemplateFloat)();I64 *(*TemplatePointer)();\n\
           CallbackBacked (*TemplateMake)();\n\
           I64 (*TemplateVariadic)(I64 first,...);\n\
           I64 DefaultValue(I64 value=9);\n\
           (OuterInt)(1,,3);(OuterFloat)();(OuterPointer)();(OuterMake)();\n\
           (OuterVariadic)(1,2,3);(OuterInt)((OuterFloat)(),,3);\n\
           (OuterInt)(DefaultValue,,3);"
      in
      let specifications =
        [
          ("OuterInt", "TemplateInt");
          ("OuterFloat", "TemplateFloat");
          ("OuterPointer", "TemplatePointer");
          ("OuterMake", "TemplateMake");
          ("OuterVariadic", "TemplateVariadic");
        ]
      in
      let entries =
        specifications
        |> List.mapi (fun entry_index (name, template_name) ->
            make_outer_entry source ~entry_index ~name
              ~metadata:(outer_global_metadata source template_name)
              ())
      in
      let environment = outer_environment source entries in
      let table = Session.semantic_symbols source.session in
      let symbols_before = Semantic_symbol_table.all_symbols table in
      let symbol_count = List.length symbols_before in
      let expressions, identifiers, policies, result =
        analyze ~environment source
      in
      Alcotest.(check (list string))
        "outer callback returns use their checked signatures"
        [
          "I64:object-value:integer-result:rank-0";
          "F64:object-value:f64-result:rank-0";
          "I64*:address-value:integer-result:rank-0";
          "CallbackBacked:object-value:f64-result:rank-0";
          "I64:object-value:integer-result:rank-0";
          "I64:object-value:integer-result:rank-0";
          "I64:object-value:integer-result:rank-0";
        ]
        (root_values result |> List.map descriptor);
      let calls =
        Semantic_function_call_expression_result.top_level_outer_callback_calls
          result
      in
      Alcotest.(check (list string))
        "outer callback calls retain source order, including nested calls"
        [
          "OuterInt";
          "OuterFloat";
          "OuterPointer";
          "OuterMake";
          "OuterVariadic";
          "OuterInt";
          "OuterFloat";
          "OuterInt";
        ]
        (List.map top_level_outer_callback_name calls);
      let first = List.hd calls in
      let first_callable =
        Semantic_function_call_expression_result
        .top_level_outer_callback_callable first
      in
      Alcotest.(check bool)
        "scalar outer callback has no computed callee result" true
        (Option.is_none
           (Semantic_function_call_expression_result
            .top_level_outer_callback_callee_result first));
      let first_signature =
        Semantic_function_call_resolution.callable_signature first_callable
      in
      let fixed_parameter_count =
        first_signature
        |> Semantic_function_type_resolution.signature_parameters |> List.length
      in
      Alcotest.(check int)
        "outer callback keeps its stored fixed signature" 3
        fixed_parameter_count;
      let fixed_results =
        Semantic_function_call_expression_result
        .top_level_outer_callback_fixed_results first
      in
      let fixed_descriptions =
        List.map top_level_fixed_description fixed_results
      in
      Alcotest.(check (list string))
        "outer callback defaults stay separate from provided arguments"
        [
          "integer-result:provided:I64:object-value:integer-result:rank-0";
          "integer-result:default:I64:integer-result:immediate";
          "integer-result:provided:I64:object-value:integer-result:rank-0";
        ]
        fixed_descriptions;
      let variadic = List.nth calls 4 in
      let variadic_count =
        Semantic_function_call_expression_result
        .top_level_outer_callback_variadic_count variadic
      in
      Alcotest.(check int64)
        "outer callback variadic count uses target I64" 2L variadic_count;
      List.iter
        (fun call ->
          let occurrence =
            call
            |> Semantic_function_call_expression_result
               .top_level_outer_callback_occurrence
          in
          let binding =
            call
            |> Semantic_function_call_expression_result
               .top_level_outer_callback_binding
          in
          let name =
            Semantic_top_level_outer_expression_binding.occurrence_name
              occurrence
          in
          let entry = Semantic_outer_environment.binding_entry binding in
          let symbol = Semantic_outer_environment.entry_symbol entry in
          let selected_name = Semantic_symbol.name symbol in
          Alcotest.(check string)
            "outer callback retains the selected table entry" name selected_name;
          let id =
            Semantic_function_call_expression_result
            .top_level_outer_callback_result_id call
          in
          let value =
            result
            |> Semantic_function_call_expression_result.top_level_all_results
            |> List.find (fun value ->
                Semantic_function_call_expression_result.Id.equal id
                  (Semantic_function_call_expression_result.result_id value))
          in
          let selected_occurrence =
            Semantic_function_call_expression_result
            .result_top_level_outer_occurrence value
          in
          let has_exact_occurrence =
            match selected_occurrence with
            | None -> false
            | Some selected -> selected == occurrence
          in
          Alcotest.(check bool)
            "outer callback result retains its exact occurrence" true
            has_exact_occurrence;
          let selected_binding =
            Semantic_function_call_expression_result.result_outer_binding value
          in
          let has_exact_binding =
            match selected_binding with
            | None -> false
            | Some selected -> selected == binding
          in
          Alcotest.(check bool)
            "outer callback result retains its exact binding" true
            has_exact_binding)
        calls;
      let replay =
        Holyc_lib.type_top_level_expressions source.session
          ~members:source.members ~policies ~identifiers expressions
        |> checked_result
      in
      let call_ids result =
        result
        |> Semantic_function_call_expression_result
           .top_level_outer_callback_calls
        |> List.map (fun call ->
            let id =
              Semantic_function_call_expression_result
              .top_level_outer_callback_result_id call
            in
            Semantic_function_call_expression_result.Id.to_int id)
      in
      Alcotest.(check (list int))
        "outer callback replay is deterministic" (call_ids result)
        (call_ids replay);
      Alcotest.(check int)
        "outer callback typing does not mutate the symbol table" symbol_count
        (Semantic_symbol_table.all_symbols table |> List.length))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let top_level_indexed_outer_callback_arrays () =
  List.iter
    (fun mode ->
      let source =
        prepared ~mode ~path:"top-level-indexed-outer-callbacks.HC"
          "F64 class CallbackBacked {};\n\
           I64 (*TemplateInt)(I64 first,I64 second=2,I64 third)[2];\n\
           F64 (*TemplateFloat)()[2][3];I64 *(*TemplatePointer)()[1];\n\
           CallbackBacked (*TemplateMake)()[1];\n\
           I64 (*TemplateVariadic)(I64 first,...)[2];\n\
           (OuterInt[0])(1,,3);(OuterFloat[1][2])();\n\
           (OuterPointer[0])();(OuterMake[0])();\n\
           (OuterVariadic[1])(1,2,3);\n\
           (OuterInt[1])((OuterFloat[0][0])(),,3);"
      in
      let specifications =
        [
          ("OuterInt", "TemplateInt");
          ("OuterFloat", "TemplateFloat");
          ("OuterPointer", "TemplatePointer");
          ("OuterMake", "TemplateMake");
          ("OuterVariadic", "TemplateVariadic");
        ]
      in
      let entries =
        specifications
        |> List.mapi (fun entry_index (name, template_name) ->
            make_outer_entry source ~entry_index ~name
              ~metadata:(outer_global_metadata source template_name)
              ())
      in
      let environment = outer_environment source entries in
      let table = Session.semantic_symbols source.session in
      let symbol_count =
        Semantic_symbol_table.all_symbols table |> List.length
      in
      let expressions, identifiers, policies, result =
        analyze ~environment source
      in
      Alcotest.(check (list string))
        "indexed outer callback returns use their stored signatures"
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
        Semantic_function_call_expression_result.top_level_outer_callback_calls
          result
      in
      Alcotest.(check (list string))
        "indexed outer callbacks retain source order, including nested calls"
        [
          "OuterInt";
          "OuterFloat";
          "OuterPointer";
          "OuterMake";
          "OuterVariadic";
          "OuterInt";
          "OuterFloat";
        ]
        (List.map top_level_outer_callback_name calls);
      let first = List.hd calls in
      let first_callee =
        match
          first
          |> Semantic_function_call_expression_result
             .top_level_outer_callback_callee_result
        with
        | Some callee -> callee
        | None -> Alcotest.fail "expected an indexed callee"
      in
      Alcotest.(check string)
        "indexed outer callback keeps its completed callee result"
        "I64:object-value:integer-result:rank-0" (descriptor first_callee);
      Alcotest.(check (list string))
        "indexed outer callback defaults stay separate from provided arguments"
        [
          "integer-result:provided:I64:object-value:integer-result:rank-0";
          "integer-result:default:I64:integer-result:immediate";
          "integer-result:provided:I64:object-value:integer-result:rank-0";
        ]
        (first
       |> Semantic_function_call_expression_result
          .top_level_outer_callback_fixed_results
        |> List.map top_level_fixed_description);
      let variadic = List.nth calls 4 in
      Alcotest.(check int64)
        "indexed outer callback variadic count uses target I64" 2L
        (Semantic_function_call_expression_result
         .top_level_outer_callback_variadic_count variadic);
      let index_conversions =
        result |> Semantic_function_call_expression_result.top_level_all_results
        |> List.filter (fun value ->
            Semantic_function_call_expression_result.result_intrinsic_conversion
              value
            = Semantic_function_call_expression_result.Result_to_int)
      in
      Alcotest.(check int)
        "nine outer callback-array indexes and the fixed root convert" 10
        (List.length index_conversions);
      let all =
        Semantic_function_call_expression_result.top_level_all_results result
      in
      List.iter
        (fun call ->
          let occurrence =
            Semantic_function_call_expression_result
            .top_level_outer_callback_occurrence call
          in
          let binding =
            Semantic_function_call_expression_result
            .top_level_outer_callback_binding call
          in
          let entry = Semantic_outer_environment.binding_entry binding in
          Alcotest.(check string)
            "indexed outer callback retains its selected record"
            (Semantic_top_level_outer_expression_binding.occurrence_name
               occurrence)
            (entry |> Semantic_outer_environment.entry_symbol
           |> Semantic_symbol.name);
          Alcotest.(check bool)
            "indexed outer callback retains a computed callee" true
            (Option.is_some
               (Semantic_function_call_expression_result
                .top_level_outer_callback_callee_result call));
          let id =
            Semantic_function_call_expression_result
            .top_level_outer_callback_result_id call
          in
          let value =
            List.find
              (fun value ->
                Semantic_function_call_expression_result.Id.equal id
                  (Semantic_function_call_expression_result.result_id value))
              all
          in
          Alcotest.(check bool)
            "indexed outer result keeps the exact occurrence" true
            (match
               Semantic_function_call_expression_result
               .result_top_level_outer_occurrence value
             with
            | Some selected -> selected == occurrence
            | None -> false);
          Alcotest.(check bool)
            "indexed outer result keeps the exact binding" true
            (match
               Semantic_function_call_expression_result.result_outer_binding
                 value
             with
            | Some selected -> selected == binding
            | None -> false))
        calls;
      let replay =
        Holyc_lib.type_top_level_expressions source.session
          ~members:source.members ~policies ~identifiers expressions
        |> checked_result
      in
      let call_ids result =
        result
        |> Semantic_function_call_expression_result
           .top_level_outer_callback_calls
        |> List.map (fun call ->
            call
            |> Semantic_function_call_expression_result
               .top_level_outer_callback_result_id
            |> Semantic_function_call_expression_result.Id.to_int)
      in
      Alcotest.(check (list int))
        "indexed outer callback replay is deterministic" (call_ids result)
        (call_ids replay);
      Alcotest.(check int)
        "indexed outer callback typing leaves symbols unchanged" symbol_count
        (Semantic_symbol_table.all_symbols table |> List.length))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let invalid_top_level_indexed_outer_callback_arrays () =
  [
    ( "unindexed",
      "I64 (*Template)(I64 value)[2];(Outer)(1);",
      "indexed top-level outer callback callee uses 0 brackets, but `Outer` \
       has 1 dimension" );
    ( "partial rank",
      "I64 (*Template)(I64 value)[2][3];(Outer[0])(1);",
      "indexed top-level outer callback callee uses 1 bracket, but `Outer` has \
       2 dimensions" );
    ( "excess rank",
      "I64 (*Template)(I64 value)[2];(Outer[0][1])(1);",
      "indexed top-level outer callback callee uses 2 brackets, but `Outer` \
       has 1 dimension" );
    ( "missing argument",
      "I64 (*Template)(I64 value)[2];(Outer[0])();",
      "call to \"Outer\" is missing required argument 1 (value)" );
  ]
  |> List.iter (fun (label, contents, expected_message) ->
      List.iter
        (fun mode ->
          let source =
            prepared ~mode
              ~path:("top-level-invalid-indexed-outer-" ^ label ^ ".HC")
              contents
          in
          let entry =
            make_outer_entry source ~entry_index:0 ~name:"Outer"
              ~metadata:(outer_global_metadata source "Template")
              ()
          in
          let environment = outer_environment source [ entry ] in
          let expressions, identifiers = build_inputs ~environment source in
          let policies =
            source |> Test_function_call_conversion_policy.analyze
            |> Test_function_call_conversion_policy.checked_policy
          in
          match
            Holyc_lib.type_top_level_expressions source.session
              ~members:source.members ~policies ~identifiers expressions
          with
          | Ok _ ->
              Alcotest.failf
                "expected %s indexed outer callback to fail in %s mode" label
                (Preprocessor.compilation_mode_name mode)
          | Error error -> (
              Alcotest.(check string)
                (label ^ " code") "HCSEMA0057"
                (Semantic_function_call_expression_result.error_code error);
              Alcotest.(check string)
                (label ^ " message") expected_message
                (Semantic_function_call_expression_result.error_message error);
              match
                Semantic_function_call_expression_result.error_origin error
              with
              | Some (Semantic_symbol.Source_location _) -> ()
              | Some
                  ( Semantic_symbol.Pinned_source _
                  | Semantic_symbol.Synthesized _ )
              | None ->
                  Alcotest.fail
                    "expected an indexed outer callback source location"))
        [ Preprocessor.Jit; Preprocessor.Aot ])

let unavailable_top_level_outer_call_shapes () =
  let source =
    prepared ~path:"top-level-unavailable-outer-calls.HC"
      "I64 TemplateObject;(OuterUntyped)(1);(OuterObject)(2);"
  in
  let entries =
    [
      make_outer_entry source ~entry_index:0 ~name:"OuterUntyped" ();
      make_outer_entry source ~entry_index:1 ~name:"OuterObject"
        ~metadata:(outer_global_metadata source "TemplateObject")
        ();
    ]
  in
  let environment = outer_environment source entries in
  let _, _, _, result = analyze ~environment source in
  Alcotest.(check (list string))
    "unsupported outer call shapes remain unavailable"
    [
      "unavailable:unavailable:unresolved:rank-0";
      "unavailable:unavailable:unresolved:rank-0";
    ]
    (root_values result |> List.map descriptor);
  let callback_count =
    result
    |> Semantic_function_call_expression_result.top_level_outer_callback_calls
    |> List.length
  in
  Alcotest.(check int)
    "unsupported outer calls do not create callback records" 0 callback_count;
  root_values result
  |> List.iter (fun value ->
      Alcotest.(check bool)
        "unavailable outer call retains its source occurrence" true
        (Option.is_some
           (Semantic_function_call_expression_result
            .result_top_level_outer_occurrence value));
      Alcotest.(check bool)
        "unavailable outer call retains its selected binding" true
        (Option.is_some
           (Semantic_function_call_expression_result.result_outer_binding value)))

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
        "nine retained callback-array indexes and the fixed root convert" 10
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

let top_level_indexed_member_callback_arrays () =
  List.iter
    (fun mode ->
      let source =
        prepared ~mode ~path:"top-level-member-callback-arrays.HC"
          "F64 class Product {};\n\
          \           class Base {F64 (*Invoke)(I64 first=1,I64 required,F64 \
           last=3,...)[2][3];};\n\
          \           class Box:Base {I64 *(*Pointer)(I64 value)[2];Product \
           (*Make)()[1];};\n\
          \           Box box;Box *pointer;\n\
          \           \
           pointer->Invoke[1][2](,4,,5.0);(box.Pointer[0])(7);box.Make[0]();"
      in
      let _, _, _, result = analyze source in
      Alcotest.(check (list string))
        "indexed member callbacks use their stored return types"
        [
          "F64:object-value:f64-result:rank-0";
          "I64*:address-value:integer-result:rank-0";
          "Product:object-value:f64-result:rank-0";
        ]
        (root_values result |> List.map descriptor);
      let calls =
        Semantic_function_call_expression_result.top_level_member_callback_calls
          result
      in
      Alcotest.(check (list string))
        "indexed member callbacks retain their global roots"
        [ "pointer"; "box"; "box" ]
        (List.map top_level_member_callback_name calls);
      let inherited = List.hd calls in
      let lookup =
        Semantic_function_call_expression_result
        .top_level_member_callback_lookup inherited
      in
      Alcotest.(check (triple string string int))
        "the top-level array call keeps its inherited lookup"
        ("Invoke", "Base", 1)
        ( lookup |> Semantic_aggregate_member_index.lookup_member
          |> Semantic_aggregate_member_index.member_symbol
          |> Semantic_symbol.name,
          lookup |> Semantic_aggregate_member_index.lookup_declaring_aggregate
          |> Semantic_symbol.name,
          Semantic_aggregate_member_index.lookup_inheritance_depth lookup );
      Alcotest.(check string)
        "the fully indexed callee keeps its member identity"
        "Invoke:Base:depth-1:offset-0"
        (inherited
       |> Semantic_function_call_expression_result
          .top_level_member_callback_callee_result |> lookup_description);
      Alcotest.(check (list string))
        "the array callback keeps defaults distinct from provided values"
        [
          "integer-result:default:I64:integer-result:immediate";
          "integer-result:provided:I64:object-value:integer-result:rank-0";
          "f64-result:default:F64:f64-result:immediate";
        ]
        (inherited
       |> Semantic_function_call_expression_result
          .top_level_member_callback_fixed_results
        |> List.map top_level_fixed_description))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let invalid_top_level_indexed_member_callback_arrays () =
  [
    ( "partial",
      "class Box {I64 (*callbacks)(I64 value)[2][3];};Box \
       box;box.callbacks[0](1);",
      "callback member `callbacks` retains 1 array dimension" );
    ( "excessive",
      "class Box {I64 (*callbacks)(I64 value)[2];};Box \
       box;box.callbacks[0][1](1);",
      "callback member `callbacks` has 1 array dimension, but the callee uses \
       2 subscripts" );
  ]
  |> List.iter (fun (label, contents, expected_message) ->
      let source =
        prepared
          ~path:("top-level-invalid-member-callback-array-" ^ label ^ ".HC")
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
      | Ok _ -> Alcotest.failf "expected %s callback array to fail" label
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
       #define MEMBER box.Member[0](4)\n\
       class Box {I64 value;I64 (*Member)(I64 value)[1];};I64 F(I64 value);\n\
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
    Alcotest.test_case "literal payloads in typed top-level results" `Quick
      literal_payloads_reach_typed_top_level_results;
    Alcotest.test_case "top-level current-position RIP addresses" `Quick
      top_level_current_positions_are_rip_addresses;
    Alcotest.test_case "typed top-level outer globals" `Quick
      top_level_outer_globals_retain_checked_shapes;
    Alcotest.test_case "metadata-free top-level outer global" `Quick
      metadata_free_top_level_outer_stays_unavailable;
    Alcotest.test_case "updates, assignments, and invalid lvalues" `Quick
      updates_assignments_and_invalid_lvalues;
    Alcotest.test_case "top-level binary operand identity" `Quick
      top_level_binary_operands_are_shared_in_both_modes;
    Alcotest.test_case "indexes, casts, and conversions" `Quick
      indexes_casts_and_conversions;
    Alcotest.test_case "aggregate member paths" `Quick aggregate_member_paths;
    Alcotest.test_case "invalid aggregate member paths" `Quick
      invalid_aggregate_member_paths;
    Alcotest.test_case "aggregate offset paths" `Quick aggregate_offset_paths;
    Alcotest.test_case "standalone top-level offset paths" `Quick
      standalone_top_level_offset_paths;
    Alcotest.test_case "invalid standalone top-level offsets" `Quick
      invalid_standalone_top_level_offsets;
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
    Alcotest.test_case "top-level outer callback calls" `Quick
      top_level_outer_callback_calls;
    Alcotest.test_case "top-level indexed outer callback arrays" `Quick
      top_level_indexed_outer_callback_arrays;
    Alcotest.test_case "invalid top-level indexed outer callback arrays" `Quick
      invalid_top_level_indexed_outer_callback_arrays;
    Alcotest.test_case "unavailable top-level outer call shapes" `Quick
      unavailable_top_level_outer_call_shapes;
    Alcotest.test_case "top-level indexed global callback calls" `Quick
      top_level_indexed_global_callback_calls;
    Alcotest.test_case "invalid top-level indexed global callback calls" `Quick
      invalid_top_level_indexed_global_callback_calls;
    Alcotest.test_case "top-level member callback calls" `Quick
      top_level_member_callback_calls;
    Alcotest.test_case "invalid top-level member callback calls" `Quick
      invalid_top_level_member_callback_calls;
    Alcotest.test_case "top-level indexed member callback arrays" `Quick
      top_level_indexed_member_callback_arrays;
    Alcotest.test_case "invalid indexed member callback arrays" `Quick
      invalid_top_level_indexed_member_callback_arrays;
    Alcotest.test_case "unavailable boundaries and checked ownership" `Quick
      unavailable_boundaries_and_checked_ownership;
    Alcotest.test_case "stale batches and mode mismatch" `Quick
      stale_batches_and_mode_mismatch;
    Alcotest.test_case "generated provenance and purity" `Quick
      generated_provenance_and_purity;
  ]
