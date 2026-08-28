open Holyc_lib
module Target = Semantic_top_level_function_call_target_classification

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let prepare = Test_function_call_conversion_policy.prepare

let analyze prepared =
  let _, _, _, results = Test_top_level_expression_result.analyze prepared in
  let records =
    Holyc_lib.classify_function_records prepared.session
      ~resolution:prepared.functions prepared.ast
    |> checked
  in
  (results, records)

let direct_calls results =
  Semantic_function_call_expression_result.top_level_direct_calls results

let call_name call =
  call |> Semantic_function_call_expression_result.top_level_direct_source
  |> Semantic_top_level_expression_tree.call_source
  |> Semantic_function_call_resolution.call_callee_name

let target_access target =
  target |> Target.call_access
  |> Semantic_function_record_classification.call_access_name

let classify records call =
  Target.classify ~records call |> function
  | Ok target -> target
  | Error error -> error |> Target.error_to_string |> Alcotest.fail

let access_matrix () =
  let run mode source expected =
    let prepared =
      prepare ~mode ~path:"top-level-call-target-matrix.HC" source
    in
    let results, records = analyze prepared in
    let calls = direct_calls results in
    let targets = List.map (classify records) calls in
    Alcotest.(check (list string))
      "call source order" (List.map fst expected) (List.map call_name calls);
    Alcotest.(check (list string))
      "all declaration-order access paths" (List.map snd expected)
      (List.map target_access targets);
    List.iter2
      (fun call target ->
        let declaration =
          Semantic_function_call_expression_result.top_level_direct_declaration
            call
        in
        let classified = Target.classified_declaration target in
        let record = Target.record target in
        Alcotest.(check bool)
          "the target keeps the exact typed call" true
          (Target.source target == call);
        Alcotest.(check bool)
          "the target keeps the exact retained declaration" true
          (Target.declaration target == declaration);
        Alcotest.(check bool)
          "the classified declaration owns the exact source" true
          (Semantic_function_record_classification.classified_declaration_source
             classified
          == declaration);
        Alcotest.(check bool)
          "the target record is the classified declaration record" true
          (record
          == Semantic_function_record_classification
             .classified_declaration_record classified))
      calls targets
  in
  run Preprocessor.Jit
    "extern U0 External();_extern _BOUND U0 Bound();\n\
     _intern 42 U0 Internal();U0 Defined(){}\n\
     External();Bound();Internal();Defined();"
    [
      ("External", "jit-extern-address-slot-call");
      ("Bound", "direct-executable-call");
      ("Internal", "internal-operation");
      ("Defined", "direct-executable-call");
    ];
  run Preprocessor.Aot
    "extern U0 External();_extern _BOUND U0 Bound();\n\
     import U0 Imported();_intern 42 U0 Internal();U0 Defined(){}\n\
     External();Bound();Imported();Internal();Defined();"
    [
      ("External", "aot-extern-call");
      ("Bound", "direct-executable-call");
      ("Imported", "aot-import-call");
      ("Internal", "internal-operation");
      ("Defined", "direct-executable-call");
    ]

let replacement_headers_select_source_snapshots () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"top-level-call-target-replacement.HC"
          "extern U0 Shift();Shift();_extern _SHIFT U0 Shift();Shift();"
      in
      let results, records = analyze prepared in
      let calls = direct_calls results in
      let before = List.hd calls in
      let after = List.nth calls 1 in
      let expected_before =
        match mode with
        | Preprocessor.Jit -> "jit-extern-address-slot-call"
        | Preprocessor.Aot -> "aot-extern-call"
      in
      Alcotest.(check (pair string string))
        "calls use their declaration-order snapshots"
        (expected_before, "direct-executable-call")
        ( target_access (classify records before),
          target_access (classify records after) );
      let before_declaration = Target.declaration (classify records before) in
      let after_declaration = Target.declaration (classify records after) in
      Alcotest.(check bool)
        "replacement calls keep distinct declaration objects" true
        (before_declaration != after_declaration))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let ownership_rejection_is_stable_and_pure () =
  let source = "extern U0 Target();Target();" in
  let prepared =
    prepare ~mode:Preprocessor.Jit ~path:"top-level-call-target-owner.HC" source
  in
  let results, records = analyze prepared in
  let call = direct_calls results |> List.hd in
  let table = Session.semantic_symbols prepared.session in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  let replay =
    prepare ~mode:Preprocessor.Jit ~path:"top-level-call-target-owner.HC" source
  in
  let _, foreign_records = analyze replay in
  (match Target.classify ~records:foreign_records call with
  | Ok _ -> Alcotest.fail "expected foreign call target evidence to fail"
  | Error error -> (
      Alcotest.(check string)
        "ownership code" "HCSEMA0068" (Target.error_code error);
      Alcotest.(check string)
        "ownership message"
        "top-level direct-call declaration is not owned by the function-record \
         classification"
        (Target.error_message error);
      match Target.error_origin error with
      | Some (Semantic_symbol.Source_location location) ->
          Alcotest.(check int)
            "ownership error points to the call"
            (String.rindex_from source (String.length source - 1) 'T')
            location.span.start
      | Some (Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _)
      | None -> Alcotest.fail "expected a source-positioned ownership error"));
  let other_mode =
    prepare ~mode:Preprocessor.Aot ~path:"top-level-call-target-owner.HC" source
  in
  let _, other_mode_records = analyze other_mode in
  Alcotest.(check bool)
    "a classification from another compilation mode is rejected" true
    (Target.classify ~records:other_mode_records call |> Result.is_error);
  let second_records =
    Holyc_lib.classify_function_records prepared.session
      ~resolution:prepared.functions prepared.ast
    |> checked
  in
  Alcotest.(check string)
    "reclassification of the same resolution is deterministic"
    (classify records call |> target_access)
    (classify second_records call |> target_access);
  Alcotest.(check int)
    "classification leaves the symbol table unchanged" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length)

let tests =
  [
    Alcotest.test_case "top-level direct-call access matrix" `Quick
      access_matrix;
    Alcotest.test_case "top-level replacement declaration snapshots" `Quick
      replacement_headers_select_source_snapshots;
    Alcotest.test_case "top-level ownership, replay, and purity" `Quick
      ownership_rejection_is_stable_and_pure;
  ]
