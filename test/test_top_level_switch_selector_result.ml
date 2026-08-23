open Holyc_lib
module Selector = Semantic_top_level_switch_selector_result

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
  | Error error -> error |> Selector.error_to_string |> Alcotest.fail

let typed_results ?environment source =
  let _, _, _, results =
    Test_top_level_expression_result.analyze ?environment source
  in
  results

let collect (source : prepared) results =
  Holyc_lib.collect_top_level_switch_selectors source.session results

let selector_descriptions result =
  result |> Selector.selectors
  |> List.map (fun selector ->
      Printf.sprintf "%d:%s:%s"
        (Selector.selector_index selector)
        (selector |> Selector.selector_mode |> Selector.selector_mode_name)
        (selector |> Selector.selector_value
       |> Test_top_level_expression_result.descriptor))

let selectors_keep_modes_types_and_calls () =
  List.iter
    (fun mode ->
      let source =
        prepare ~mode ~path:"top-level-switch-selector-results.HC"
          "class Box {I64 value;I64 (*callback)();};\n\
           I64 Direct(){return 1;}\n\
           I64 flag;F64 ratio;U8 *pointer;I64 values[2];Box box;\n\
           switch(flag){}switch[ratio]{}switch(pointer){}\n\
           switch(box.value){}switch(values[1]){}\n\
           switch(Direct()){}switch[box.callback()]{}"
      in
      let table = Session.semantic_symbols source.session in
      let results = typed_results source in
      let collected = collect source results |> checked_result in
      Alcotest.(check (list string))
        "top-level selectors keep their typed value and source mode"
        [
          "0:bounded:I64:object-value:integer-result:rank-0";
          "1:no-bound:F64:object-value:f64-result:rank-0";
          "2:bounded:U8*:object-value:integer-result:rank-0";
          "3:bounded:I64:object-value:integer-result:rank-0";
          "4:bounded:I64:object-value:integer-result:rank-0";
          "5:bounded:I64:object-value:integer-result:rank-0";
          "6:no-bound:I64:object-value:integer-result:rank-0";
        ]
        (selector_descriptions collected);
      Alcotest.(check bool)
        "selector view owns the semantic table" true
        (Selector.owns_table collected table);
      Alcotest.(check bool)
        "selector view retains the exact typed batch" true
        (Selector.source collected == results);
      collected |> Selector.selectors
      |> List.iter (fun selector ->
          let statement = Selector.selector_statement selector in
          let root = Selector.selector_root selector in
          Alcotest.(check bool)
            "selector root belongs to its retained statement" true
            (statement
           |> Semantic_function_call_expression_result.top_level_statement_roots
            |> List.exists (fun candidate -> candidate == root));
          Alcotest.(check bool)
            "selector reuses the root's expression result" true
            (Selector.selector_value selector
            == Semantic_function_call_expression_result.top_level_root_value
                 root);
          Alcotest.(check (option string))
            "selector roots do not carry a discarded-result flag" None
            (root
           |> Semantic_function_call_expression_result.top_level_root_result_use
            |> Option.map
                 Semantic_function_call_expression_result.result_use_name));
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
      let selector_ids =
        collected |> Selector.selectors
        |> List.map (fun selector ->
            selector |> Selector.selector_value
            |> Semantic_function_call_expression_result.result_id)
      in
      Alcotest.(check bool)
        "direct and callback selectors keep their call result identities" true
        (Semantic_function_call_expression_result.Id.equal direct_id
           (List.nth selector_ids 5)
        && Semantic_function_call_expression_result.Id.equal member_id
             (List.nth selector_ids 6)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let with_included_source contents apply =
  let directory = Filename.temp_dir "holyc-top-level-selector-" "" in
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
      write_file root_path "#include \"selectors\"";
      write_file (Filename.concat directory "selectors.HC") contents;
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

let included_selectors_replay_without_mutation () =
  with_included_source
    "#define SELECT ratio\nF64 ratio;switch(SELECT){switch[SELECT]{}}"
    (fun source ->
      let table = Session.semantic_symbols source.session in
      let before = Semantic_symbol_table.all_symbols table |> List.length in
      let results = typed_results source in
      let first = collect source results |> checked_result in
      let second = collect source results |> checked_result in
      Alcotest.(check (list string))
        "included selector collection replays identically"
        (selector_descriptions first)
        (selector_descriptions second);
      Alcotest.(check int)
        "selector collection leaves semantic symbols unchanged" before
        (Semantic_symbol_table.all_symbols table |> List.length);
      Alcotest.(check (list string))
        "nested selectors remain in source order"
        [
          "0:bounded:F64:object-value:f64-result:rank-0";
          "1:no-bound:F64:object-value:f64-result:rank-0";
        ]
        (selector_descriptions first);
      first |> Selector.selectors
      |> List.iter (fun selector ->
          match Selector.selector_keyword_origin selector with
          | Semantic_symbol.Source_location location ->
              let source_file =
                Source_manager.find
                  (Session.sources source.session)
                  location.span.source
                |> Option.get
              in
              Alcotest.(check string)
                "selector keywords keep their include source" "selectors.HC"
                (Source_file.path source_file |> Filename.basename)
          | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
              Alcotest.fail "expected included selector keyword provenance");
      first |> Selector.selectors
      |> List.iter (fun selector ->
          match
            selector |> Selector.selector_value
            |> Semantic_function_call_expression_result.result_origin
          with
          | Semantic_symbol.Source_location location ->
              Alcotest.(check bool)
                "definition-backed selectors keep their definition origin" true
                (Option.is_some location.defined_at)
          | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
              Alcotest.fail "expected generated selector value provenance"))

let retag_selectors (source : prepared) expressions update_index =
  let statements =
    expressions |> Semantic_top_level_expression_tree.statements
    |> List.map (fun statement ->
        let roots =
          statement |> Semantic_top_level_expression_tree.statement_roots
          |> List.map (fun root ->
              let role =
                match Semantic_top_level_expression_tree.root_role root with
                | Semantic_top_level_expression_tree.Switch_selector
                    { selector_index; mode; keyword_origin } ->
                    Semantic_top_level_expression_tree.Switch_selector
                      {
                        selector_index = update_index selector_index;
                        mode;
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
        |> checked_tree)
  in
  Semantic_top_level_expression_tree.create
    ~table:(Session.semantic_symbols source.session)
    ~source:(Semantic_top_level_expression_tree.source expressions)
    statements
  |> checked_tree

let malformed_and_foreign_inputs_are_rejected () =
  let source =
    prepare ~path:"top-level-selector-validation.HC" "switch(1){}switch[2]{}"
  in
  let expressions, _ = Test_top_level_expression_result.build_inputs source in
  let malformed = retag_selectors source expressions (fun _ -> 0) in
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
  | Ok _ -> Alcotest.fail "expected duplicate selector indexes to fail"
  | Error error ->
      Alcotest.(check string)
        "duplicate-index code" "HCSEMA0064"
        (Selector.error_code error);
      Alcotest.(check string)
        "duplicate-index message"
        "top-level switch selector index 0 appears where index 1 was expected"
        (Selector.error_message error));
  let valid = typed_results source in
  let foreign = Session.create () in
  match Holyc_lib.collect_top_level_switch_selectors foreign valid with
  | Ok _ -> Alcotest.fail "expected a foreign selector batch to fail"
  | Error error ->
      Alcotest.(check string)
        "foreign-session code" "HCSEMA0064"
        (Selector.error_code error)

let tests =
  [
    Alcotest.test_case "modes, types, and calls" `Quick
      selectors_keep_modes_types_and_calls;
    Alcotest.test_case "included selector replay" `Quick
      included_selectors_replay_without_mutation;
    Alcotest.test_case "malformed and foreign inputs" `Quick
      malformed_and_foreign_inputs_are_rejected;
  ]
