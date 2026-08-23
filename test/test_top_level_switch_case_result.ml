open Holyc_lib
module Case = Semantic_top_level_switch_case_result

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
  | Error error -> error |> Case.error_to_string |> Alcotest.fail

let typed_results source =
  let _, _, _, results = Test_top_level_expression_result.analyze source in
  results

let collect (source : prepared) results =
  Holyc_lib.collect_top_level_switch_cases source.session results

let case_values case_ =
  match Case.case_pattern case_ with
  | Case.Implicit_case_result -> []
  | Case.Single_case_result value -> [ value ]
  | Case.Ranged_case_result { start_value; end_value; _ } ->
      [ start_value; end_value ]

let case_descriptions result =
  result |> Case.cases
  |> List.map (fun case_ ->
      Printf.sprintf "%d:%s" (Case.case_index case_)
        (case_ |> Case.case_pattern |> Case.case_pattern_name))

let cases_keep_patterns_types_conversions_and_calls () =
  List.iter
    (fun mode ->
      let source =
        prepare ~mode ~path:"top-level-switch-case-results.HC"
          "class Box {I64 value;I64 (*callback)();};\n\
           I64 Direct(){return 1;}\n\
           I64 integer;F64 floating;U8 *pointer;Box box;\n\
           switch(integer){\n\
           case:\n\
           case integer:\n\
           case floating...pointer:\n\
           case box.value:\n\
           case Direct():\n\
           case box.callback():break;\n\
           }"
      in
      let table = Session.semantic_symbols source.session in
      let results = typed_results source in
      let collected = collect source results |> checked_result in
      Alcotest.(check (list string))
        "top-level cases keep their source identities and patterns"
        [
          "0:implicit";
          "1:single";
          "2:ranged";
          "3:single";
          "4:single";
          "5:single";
        ]
        (case_descriptions collected);
      Alcotest.(check bool)
        "case view owns the semantic table" true
        (Case.owns_table collected table);
      Alcotest.(check bool)
        "case view retains the exact typed batch" true
        (Case.source collected == results);
      let cases = Case.cases collected in
      let source_cases =
        results |> Semantic_function_call_expression_result.top_level_source
        |> Semantic_top_level_expression_tree.all_switch_cases
      in
      Alcotest.(check int)
        "semantic tree retains every case label" 6 (List.length source_cases);
      List.iter2
        (fun case_ source_case ->
          Alcotest.(check bool)
            "case result reuses its source metadata" true
            (Case.case_source case_ == source_case))
        cases source_cases;
      (match Case.case_pattern (List.hd cases) with
      | Case.Implicit_case_result -> ()
      | Case.Single_case_result _ | Case.Ranged_case_result _ ->
          Alcotest.fail "implicit case gained a fabricated expression");
      let values = List.concat_map case_values cases in
      Alcotest.(check (list string))
        "explicit case values retain source-visible types"
        [ "I64"; "F64"; "U8*"; "I64"; "I64"; "I64" ]
        (List.map
           (fun value ->
             value |> Case.case_value_result
             |> Test_top_level_expression_result.type_name)
           values);
      Alcotest.(check (list string))
        "only F64 case values carry the integer conversion"
        [ "none"; "ICF_RES_TO_INT"; "none"; "none"; "none"; "none" ]
        (List.map
           (fun value ->
             value |> Case.case_value_conversion
             |> Semantic_function_call_expression_result
                .intrinsic_conversion_name)
           values);
      List.iter
        (fun case_ ->
          let statement = Case.case_statement case_ in
          case_values case_
          |> List.iter (fun value ->
              let root = Case.case_value_root value in
              Alcotest.(check bool)
                "case root belongs to its retained statement" true
                (statement
               |> Semantic_function_call_expression_result
                  .top_level_statement_roots
                |> List.exists (fun candidate -> candidate == root));
              Alcotest.(check bool)
                "case value reuses the root's typed expression" true
                (Case.case_value_result value
                == Semantic_function_call_expression_result.top_level_root_value
                     root);
              Alcotest.(check (option string))
                "case roots do not carry discarded-result intent" None
                (root
               |> Semantic_function_call_expression_result
                  .top_level_root_result_use
                |> Option.map
                     Semantic_function_call_expression_result.result_use_name)))
        cases;
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
      let value_ids =
        List.map
          (fun value ->
            value |> Case.case_value_result
            |> Semantic_function_call_expression_result.result_id)
          values
      in
      Alcotest.(check bool)
        "case calls keep their result identities" true
        (Semantic_function_call_expression_result.Id.equal direct_id
           (List.nth value_ids 4)
        && Semantic_function_call_expression_result.Id.equal member_id
             (List.nth value_ids 5)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let with_included_source contents apply =
  let directory = Filename.temp_dir "holyc-top-level-case-" "" in
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
      write_file root_path "#include \"cases\"";
      write_file (Filename.concat directory "cases.HC") contents;
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

let included_nested_cases_replay_without_mutation () =
  with_included_source
    "#define LOW value\n\
     F64 value;switch(0){case:switch[1]{case LOW:;}case 2...LOW:;}"
    (fun source ->
      let table = Session.semantic_symbols source.session in
      let before = Semantic_symbol_table.all_symbols table |> List.length in
      let results = typed_results source in
      let first = collect source results |> checked_result in
      let second = collect source results |> checked_result in
      Alcotest.(check (list string))
        "included case collection replays identically" (case_descriptions first)
        (case_descriptions second);
      Alcotest.(check int)
        "case collection leaves semantic symbols unchanged" before
        (Semantic_symbol_table.all_symbols table |> List.length);
      Alcotest.(check (list string))
        "nested cases remain in recursive source order"
        [ "0:implicit"; "1:single"; "2:ranged" ]
        (case_descriptions first);
      first |> Case.cases
      |> List.iter (fun case_ ->
          match Case.case_keyword_origin case_ with
          | Semantic_symbol.Source_location location ->
              let source_file =
                Source_manager.find
                  (Session.sources source.session)
                  location.span.source
                |> Option.get
              in
              Alcotest.(check string)
                "case keywords keep their include source" "cases.HC"
                (Source_file.path source_file |> Filename.basename)
          | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
              Alcotest.fail "expected included case keyword provenance");
      (match List.nth_opt (Case.cases first) 2 with
      | Some case_ -> (
          match Case.case_pattern case_ with
          | Case.Ranged_case_result { ellipsis_origin; _ } -> (
              match ellipsis_origin with
              | Semantic_symbol.Source_location location ->
                  let source_file =
                    Source_manager.find
                      (Session.sources source.session)
                      location.span.source
                    |> Option.get
                  in
                  Alcotest.(check string)
                    "range ellipsis keeps its include source" "cases.HC"
                    (Source_file.path source_file |> Filename.basename)
              | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _
                -> Alcotest.fail "expected included range ellipsis provenance")
          | Case.Implicit_case_result | Case.Single_case_result _ ->
              Alcotest.fail "expected the third source case to stay ranged")
      | None -> Alcotest.fail "expected the included ranged case");
      first |> Case.cases
      |> List.concat_map case_values
      |> List.filter (fun value ->
          value |> Case.case_value_result
          |> Test_top_level_expression_result.type_name |> String.equal "F64")
      |> List.iter (fun value ->
          match
            value |> Case.case_value_result
            |> Semantic_function_call_expression_result.result_origin
          with
          | Semantic_symbol.Source_location location ->
              Alcotest.(check bool)
                "definition-backed case values keep their definition origin"
                true
                (Option.is_some location.defined_at)
          | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
              Alcotest.fail "expected generated case value provenance"))

let rebuild_tree (source : prepared) expressions ~map_root ~map_case =
  let statements =
    expressions |> Semantic_top_level_expression_tree.statements
    |> List.map (fun statement ->
        let roots =
          statement |> Semantic_top_level_expression_tree.statement_roots
          |> List.map (fun root ->
              Semantic_top_level_expression_tree.make_root
                ~index:(Semantic_top_level_expression_tree.root_index root)
                ~role:(map_root root)
                ~expression:
                  (Semantic_top_level_expression_tree.root_expression root)
                ~origin:(Semantic_top_level_expression_tree.root_origin root)
              |> checked_tree)
        in
        let switch_cases =
          statement |> Semantic_top_level_expression_tree.statement_switch_cases
          |> List.map (fun case_ ->
              let index, pattern = map_case case_ in
              Semantic_top_level_expression_tree.make_switch_case ~index
                ~keyword_origin:
                  (Semantic_top_level_expression_tree.switch_case_keyword_origin
                     case_)
                ~pattern
                ~origin:
                  (Semantic_top_level_expression_tree.switch_case_origin case_)
              |> checked_tree)
        in
        Semantic_top_level_expression_tree.make_statement
          ~source:
            (Semantic_top_level_expression_tree.statement_source statement)
          ~roots
          ~calls:(Semantic_top_level_expression_tree.statement_calls statement)
          ~switch_cases
        |> checked_tree)
  in
  Semantic_top_level_expression_tree.create
    ~table:(Session.semantic_symbols source.session)
    ~source:(Semantic_top_level_expression_tree.source expressions)
    statements
  |> checked_tree

let type_tree (source : prepared) expressions =
  let identifiers =
    Holyc_lib.classify_top_level_identifiers source.session
      ~globals:source.global_types ~functions:source.functions ~expressions
    |> checked
  in
  let policies =
    source |> Test_function_call_conversion_policy.analyze
    |> Test_function_call_conversion_policy.checked_policy
  in
  Holyc_lib.type_top_level_expressions source.session ~members:source.members
    ~policies ~identifiers expressions
  |> function
  | Ok value -> value
  | Error error ->
      error |> Semantic_function_call_expression_result.error_to_string
      |> Alcotest.fail

let malformed_and_foreign_inputs_are_rejected () =
  let source =
    prepare ~path:"top-level-case-validation.HC" "switch(1){case:;case 2:;}"
  in
  let expressions, _ = Test_top_level_expression_result.build_inputs source in
  let unchanged_root root = Semantic_top_level_expression_tree.root_role root in
  let discontinuous =
    rebuild_tree source expressions ~map_root:unchanged_root
      ~map_case:(fun case_ ->
        ( Semantic_top_level_expression_tree.switch_case_index case_ * 2,
          Semantic_top_level_expression_tree.switch_case_pattern case_ ))
    |> type_tree source
  in
  (match collect source discontinuous with
  | Ok _ -> Alcotest.fail "expected discontinuous case indexes to fail"
  | Error error ->
      Alcotest.(check string)
        "discontinuous-index code" "HCSEMA0065" (Case.error_code error);
      Alcotest.(check string)
        "discontinuous-index message"
        "top-level switch case index 2 appears where index 1 was expected"
        (Case.error_message error));
  let missing =
    rebuild_tree source expressions
      ~map_root:(fun root ->
        match Semantic_top_level_expression_tree.root_role root with
        | Semantic_top_level_expression_tree.Switch_case_value
            { case_index = 1; _ } ->
            Semantic_top_level_expression_tree.Expression_statement
              { statement_index = 99 }
        | role -> role)
      ~map_case:(fun case_ ->
        ( Semantic_top_level_expression_tree.switch_case_index case_,
          Semantic_top_level_expression_tree.switch_case_pattern case_ ))
    |> type_tree source
  in
  (match collect source missing with
  | Ok _ -> Alcotest.fail "expected a missing case root to fail"
  | Error error ->
      Alcotest.(check string)
        "missing-root code" "HCSEMA0065" (Case.error_code error);
      Alcotest.(check string)
        "missing-root message"
        "top-level switch case 1 is missing its single value root"
        (Case.error_message error));
  let excess =
    rebuild_tree source expressions
      ~map_root:(fun root ->
        match Semantic_top_level_expression_tree.root_role root with
        | Semantic_top_level_expression_tree.Switch_selector _ ->
            Semantic_top_level_expression_tree.Switch_case_value
              {
                case_index = 0;
                position = Semantic_top_level_expression_tree.Single_case;
              }
        | role -> role)
      ~map_case:(fun case_ ->
        ( Semantic_top_level_expression_tree.switch_case_index case_,
          Semantic_top_level_expression_tree.switch_case_pattern case_ ))
    |> type_tree source
  in
  (match collect source excess with
  | Ok _ -> Alcotest.fail "expected an implicit case value root to fail"
  | Error error ->
      Alcotest.(check string)
        "implicit-root code" "HCSEMA0065" (Case.error_code error);
      Alcotest.(check string)
        "implicit-root message"
        "implicit top-level switch case unexpectedly has a value root"
        (Case.error_message error));
  let valid = typed_results source in
  let foreign = Session.create () in
  match Holyc_lib.collect_top_level_switch_cases foreign valid with
  | Ok _ -> Alcotest.fail "expected a foreign case batch to fail"
  | Error error ->
      Alcotest.(check string)
        "foreign-session code" "HCSEMA0065" (Case.error_code error)

let tests =
  [
    Alcotest.test_case "patterns, types, conversions, and calls" `Quick
      cases_keep_patterns_types_conversions_and_calls;
    Alcotest.test_case "included nested case replay" `Quick
      included_nested_cases_replay_without_mutation;
    Alcotest.test_case "malformed and foreign inputs" `Quick
      malformed_and_foreign_inputs_are_rejected;
  ]
