open Holyc_lib
module Condition = Semantic_top_level_condition_result

let checked = Test_function_call_conversion_policy.checked
let prepare = Test_function_call_conversion_policy.prepare

type prepared = Test_function_call_conversion_policy.prepared

let checked_tree = function
  | Ok value -> value
  | Error error ->
      error |> Semantic_top_level_expression_tree.error_to_string
      |> Alcotest.fail

let checked_result = function
  | Ok value -> value
  | Error error -> error |> Condition.error_to_string |> Alcotest.fail

let typed_results ?environment source =
  let _, _, _, results =
    Test_top_level_expression_result.analyze ?environment source
  in
  results

let collect (source : prepared) results =
  Holyc_lib.collect_top_level_conditions source.session results

let condition_descriptions result =
  result |> Condition.conditions
  |> List.map (fun condition ->
      Printf.sprintf "%d:%s:%s:%s"
        (Condition.condition_index condition)
        (condition |> Condition.condition_role
       |> Semantic_function_call_expression_result.condition_role_name)
        (condition |> Condition.condition_branch_test
       |> Condition.branch_test_name)
        (condition |> Condition.condition_value
       |> Test_top_level_expression_result.descriptor))

let conditions_keep_roles_types_and_branch_tests () =
  List.iter
    (fun mode ->
      let source =
        prepare ~mode ~path:"top-level-condition-results.HC"
          "class Box {I64 value;I64 (*callback)();};\n\
           I64 Direct(){return 1;}\n\
           I64 flag;F64 ratio;U8 *pointer;I64 values[2];Box box;\n\
           if(flag);while(ratio);do ;while(pointer);\n\
           for(flag=0;box.value;flag++);if(values[1]);\n\
           if(Direct());if(box.callback());"
      in
      let table = Session.semantic_symbols source.session in
      let results = typed_results source in
      let collected = collect source results |> checked_result in
      Alcotest.(check (list string))
        "top-level conditions keep their typed value and source branch"
        [
          "0:if:zero:I64:object-value:integer-result:rank-0";
          "1:while:zero:F64:object-value:f64-result:rank-0";
          "2:do-while:nonzero:U8*:object-value:integer-result:rank-0";
          "3:for:zero:I64:object-value:integer-result:rank-0";
          "4:if:zero:I64:object-value:integer-result:rank-0";
          "5:if:zero:I64:object-value:integer-result:rank-0";
          "6:if:zero:I64:object-value:integer-result:rank-0";
        ]
        (condition_descriptions collected);
      Alcotest.(check bool)
        "condition view owns the semantic table" true
        (Condition.owns_table collected table);
      Alcotest.(check bool)
        "condition view retains the exact typed batch" true
        (Condition.source collected == results);
      collected |> Condition.conditions
      |> List.iter (fun condition ->
          let root = Condition.condition_root condition in
          Alcotest.(check bool)
            "condition reuses the root's expression result" true
            (Condition.condition_value condition
            == Semantic_function_call_expression_result.top_level_root_value
                 root);
          Alcotest.(check (option string))
            "condition roots do not invent a discarded-result flag" None
            (root
           |> Semantic_function_call_expression_result.top_level_root_result_use
            |> Option.map
                 Semantic_function_call_expression_result.result_use_name);
          Alcotest.(check string)
            "condition roots do not invent a Boolean conversion" "none"
            (condition |> Condition.condition_value
           |> Semantic_function_call_expression_result
              .result_intrinsic_conversion
           |> Semantic_function_call_expression_result.intrinsic_conversion_name
            ));
      let direct_id =
        results
        |> Semantic_function_call_expression_result.top_level_direct_calls
        |> List.hd
        |> Semantic_function_call_expression_result.top_level_direct_result_id
      in
      let member_id =
        results
        |> Semantic_function_call_expression_result
           .top_level_member_callback_calls |> List.hd
        |> Semantic_function_call_expression_result
           .top_level_member_callback_result_id
      in
      let condition_ids =
        collected |> Condition.conditions
        |> List.map (fun condition ->
            condition |> Condition.condition_value
            |> Semantic_function_call_expression_result.result_id)
      in
      Alcotest.(check bool)
        "direct and callback conditions keep their call result identities" true
        (Semantic_function_call_expression_result.Id.equal direct_id
           (List.nth condition_ids 5)
        && Semantic_function_call_expression_result.Id.equal member_id
             (List.nth condition_ids 6)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let with_included_source contents apply =
  let directory = Filename.temp_dir "holyc-top-level-condition-" "" in
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
      write_file root_path "#include \"conditions\"";
      write_file (Filename.concat directory "conditions.HC") contents;
      let session = Session.create () in
      let source = checked (Session.load_source session ~path:root_path) in
      let ast =
        Holyc_lib.parse_with_config session
          ~config:
            (Test_function_call_conversion_policy.config
               ~working_directory:directory Preprocessor.Jit)
          ~source
        |> Test_function_call_conversion_policy.expect_ast
      in
      Test_function_call_conversion_policy.finish_prepare Preprocessor.Jit
        session ast
      |> apply)

let included_conditions_replay_without_mutation () =
  with_included_source
    "#define CHECK ratio\nF64 ratio;if(CHECK);do ;while(CHECK);" (fun source ->
      let table = Session.semantic_symbols source.session in
      let before = Semantic_symbol_table.all_symbols table |> List.length in
      let results = typed_results source in
      let first = collect source results |> checked_result in
      let second = collect source results |> checked_result in
      Alcotest.(check (list string))
        "included condition collection replays identically"
        (condition_descriptions first)
        (condition_descriptions second);
      Alcotest.(check int)
        "condition collection leaves semantic symbols unchanged" before
        (Semantic_symbol_table.all_symbols table |> List.length);
      first |> Condition.conditions
      |> List.iter (fun condition ->
          match Condition.condition_keyword_origin condition with
          | Semantic_symbol.Source_location location ->
              let source_file =
                Source_manager.find
                  (Session.sources source.session)
                  location.span.source
                |> Option.get
              in
              Alcotest.(check string)
                "condition keywords keep their include source" "conditions.HC"
                (Source_file.path source_file |> Filename.basename)
          | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
              Alcotest.fail "expected included condition keyword provenance");
      first |> Condition.conditions
      |> List.iter (fun condition ->
          match
            condition |> Condition.condition_value
            |> Semantic_function_call_expression_result.result_origin
          with
          | Semantic_symbol.Source_location location ->
              Alcotest.(check bool)
                "definition-backed conditions keep their definition origin" true
                (Option.is_some location.defined_at)
          | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
              Alcotest.fail "expected generated condition value provenance"))

let retag_conditions (source : prepared) expressions update_index =
  let statements =
    expressions |> Semantic_top_level_expression_tree.statements
    |> List.map (fun statement ->
        let roots =
          statement |> Semantic_top_level_expression_tree.statement_roots
          |> List.map (fun root ->
              let role =
                match Semantic_top_level_expression_tree.root_role root with
                | Semantic_top_level_expression_tree.Condition
                    { condition_index; role; keyword_origin } ->
                    Semantic_top_level_expression_tree.Condition
                      {
                        condition_index = update_index condition_index;
                        role;
                        keyword_origin;
                      }
                | role -> role
              in
              Semantic_top_level_expression_tree.make_root
                ~index:(Semantic_top_level_expression_tree.root_index root)
                ~role
                ~expression:
                  (Semantic_top_level_expression_tree.root_expression root)
                ~origin:(Semantic_top_level_expression_tree.root_origin root)
              |> checked_tree)
        in
        Semantic_top_level_expression_tree.make_statement
          ~source:
            (Semantic_top_level_expression_tree.statement_source statement)
          ~roots
          ~calls:(Semantic_top_level_expression_tree.statement_calls statement)
          ~switch_cases:
            (Semantic_top_level_expression_tree.statement_switch_cases statement)
        |> checked_tree)
  in
  Semantic_top_level_expression_tree.create
    ~table:(Session.semantic_symbols source.session)
    ~source:(Semantic_top_level_expression_tree.source expressions)
    statements
  |> checked_tree

let malformed_and_foreign_inputs_are_rejected () =
  let source =
    prepare ~path:"top-level-condition-validation.HC" "if(1);while(2);"
  in
  let expressions, _ = Test_top_level_expression_result.build_inputs source in
  let malformed = retag_conditions source expressions (fun _ -> 0) in
  let identifiers =
    Holyc_lib.classify_top_level_identifiers source.session
      ~globals:source.global_types ~functions:source.functions
      ~expressions:malformed
    |> checked
  in
  let policies =
    source |> Test_function_call_conversion_policy.analyze
    |> Test_function_call_conversion_policy.checked_policy
  in
  let typed =
    Holyc_lib.type_top_level_expressions source.session ~members:source.members
      ~policies ~identifiers malformed
    |> function
    | Ok value -> value
    | Error error ->
        error |> Semantic_function_call_expression_result.error_to_string
        |> Alcotest.fail
  in
  (match collect source typed with
  | Ok _ -> Alcotest.fail "expected duplicate condition indexes to fail"
  | Error error ->
      Alcotest.(check string)
        "duplicate-index code" "HCSEMA0063"
        (Condition.error_code error);
      Alcotest.(check string)
        "duplicate-index message"
        "top-level condition index 0 appears where index 1 was expected"
        (Condition.error_message error));
  let valid = typed_results source in
  let foreign = Session.create () in
  match Holyc_lib.collect_top_level_conditions foreign valid with
  | Ok _ -> Alcotest.fail "expected a foreign condition batch to fail"
  | Error error ->
      Alcotest.(check string)
        "foreign-session code" "HCSEMA0063"
        (Condition.error_code error)

let tests =
  [
    Alcotest.test_case "roles, types, and branch tests" `Quick
      conditions_keep_roles_types_and_branch_tests;
    Alcotest.test_case "included condition replay" `Quick
      included_conditions_replay_without_mutation;
    Alcotest.test_case "malformed and foreign inputs" `Quick
      malformed_and_foreign_inputs_are_rejected;
  ]
