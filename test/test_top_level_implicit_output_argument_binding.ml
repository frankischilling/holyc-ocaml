open Holyc_lib
module Binding = Semantic_top_level_implicit_output_argument_binding
module Target = Semantic_top_level_implicit_output_target_resolution

let checked = Test_function_call_conversion_policy.checked
let prepare = Test_function_call_conversion_policy.prepare

type prepared = Test_function_call_conversion_policy.prepared

let checked_target = function
  | Ok value -> value
  | Error error -> error |> Target.error_to_string |> Alcotest.fail

let checked_binding = function
  | Ok value -> value
  | Error error -> error |> Binding.error_to_string |> Alcotest.fail

type inputs = {
  source : prepared;
  policies : Semantic_function_call_conversion_policy.t;
  expressions : Semantic_function_call_expression_result.top_level_t;
  targets : Target.t;
}

let analyze ?environment source =
  let _, _, policies, expressions =
    Test_top_level_expression_result.analyze ?environment source
  in
  let targets =
    Holyc_lib.resolve_top_level_implicit_output_targets source.session
      ~function_types:source.function_types ~functions:source.functions
      expressions
    |> checked_target
  in
  { source; policies; expressions; targets }

let bind ?outer_headers inputs =
  Holyc_lib.bind_top_level_implicit_output_arguments inputs.source.session
    ~policies:inputs.policies ?outer_headers inputs.targets

let bound = function
  | Binding.Bound_output output -> output
  | Binding.Deferred_outer_output _ ->
      Alcotest.fail "expected a bound top-level output"

let deferred = function
  | Binding.Deferred_outer_output output -> output
  | Binding.Bound_output _ ->
      Alcotest.fail "expected a deferred top-level output"

let fixed_path_names output =
  output |> Binding.bound_fixed_slots
  |> List.map (fun slot ->
      slot |> Binding.fixed_path |> Binding.fixed_path_name)

let header_named (source : prepared) name =
  source.function_types |> Semantic_function_type_resolution.functions
  |> List.find (fun header ->
      header |> Semantic_function_type_resolution.function_symbol
      |> Semantic_symbol.name |> String.equal name)

let standard_bindings_follow_selected_headers () =
  List.iter
    (fun mode ->
      let source =
        prepare ~mode ~path:"top-level-output-standard-bindings.HC"
          "F64 class FloatBox {};\n\
           extern U0 Print(FloatBox first,I64 second,F64 third,...);\n\
           \"\" 1,2.5,3,4;\n\
           extern U0 PutChars(U64 ch);U64 ch;'' ch;"
      in
      let inputs = analyze source in
      let result = bind inputs |> checked_binding in
      let outputs = Binding.outputs result in
      match outputs with
      | [ print; put_chars ] ->
          let print = bound print in
          let put_chars_result = put_chars in
          let put_chars = bound put_chars in
          Alcotest.(check (list string))
            "fixed conversions follow the selected Print header"
            [
              "provided:integer-result:ICF_RES_TO_F64";
              "provided:f64-result:ICF_RES_TO_INT";
              "provided:integer-result:ICF_RES_TO_F64";
            ]
            (fixed_path_names print);
          Alcotest.(check int)
            "only the fourth Print value is variadic" 1
            (print |> Binding.bound_variadic_roots |> List.length);
          Alcotest.(check (list string))
            "PutChars following expression fills its fixed slot"
            [ "provided:integer-result:none" ]
            (fixed_path_names put_chars);
          let provided =
            put_chars |> Binding.bound_fixed_slots |> List.hd
            |> Binding.fixed_path
          in
          (match provided with
          | Binding.Defaulted_path _ ->
              Alcotest.fail "expected a provided PutChars value"
          | Binding.Provided_path provided -> (
              match Binding.provided_source provided with
              | Binding.Trailing_value _ ->
                  Alcotest.fail "expected the fixed PutChars value"
              | Binding.Fixed_value root ->
                  Alcotest.(check bool)
                    "fixed root identity is retained" true
                    (root
                    == (put_chars_result |> Binding.output_source
                      |> Target.output_fixed_value))));
          Alcotest.(check string)
            "empty marker form is retained by target resolution"
            "following-expression"
            (put_chars_result |> Binding.output_source
           |> Target.output_fixed_source
           |> Semantic_function_call_resolution
              .implicit_output_fixed_source_name);
          Alcotest.(check bool)
            "binding keeps its target batch" true
            (Binding.targets result == inputs.targets)
      | outputs ->
          Alcotest.failf "expected two top-level outputs, got %d"
            (List.length outputs))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let defaults_keep_mode_and_generated_provenance () =
  List.iter
    (fun mode ->
      let source =
        prepare ~mode ~path:"top-level-output-default-bindings.HC"
          "#define GENERATED \"value\"\n\
           extern U0 Print(U8 *fmt,U8 *suffix=\"done\",I64 count=3);\n\
           if(1) GENERATED;"
      in
      let output =
        analyze source |> bind |> checked_binding |> Binding.outputs |> List.hd
        |> bound
      in
      let defaults =
        output |> Binding.bound_fixed_slots
        |> List.filter_map (fun slot ->
            match Binding.fixed_path slot with
            | Binding.Provided_path _ -> None
            | Binding.Defaulted_path binding -> Some binding)
      in
      Alcotest.(check (list int))
        "default omissions keep fixed positions" [ 1; 2 ]
        (List.map
           (fun binding ->
             binding |> Binding.default_omission |> Binding.omission_position)
           defaults);
      Alcotest.(check (list string))
        "default materialization follows mode"
        (match mode with
        | Preprocessor.Jit -> [ "immediate"; "immediate" ]
        | Preprocessor.Aot -> [ "aot-string-constant"; "immediate" ])
        (List.map
           (fun binding ->
             binding |> Binding.default_materialization
             |> Binding.default_materialization_name)
           defaults);
      let provided =
        output |> Binding.bound_fixed_slots |> List.hd |> Binding.fixed_path
      in
      match provided with
      | Binding.Defaulted_path _ ->
          Alcotest.fail "expected the generated format value"
      | Binding.Provided_path provided -> (
          match
            provided |> Binding.provided_result
            |> Semantic_function_call_expression_result.result_origin
          with
          | Semantic_symbol.Source_location { defined_at = Some _; _ } -> ()
          | _ -> Alcotest.fail "expected generated format provenance"))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let missing_and_extra_values_have_stable_diagnostics () =
  let missing =
    prepare ~path:"top-level-output-missing-fixed.HC"
      "extern U0 Print(U8 *fmt,I64 value);\"value\";"
    |> analyze
  in
  (match bind missing with
  | Ok _ -> Alcotest.fail "expected a missing fixed parameter"
  | Error error ->
      Alcotest.(check string)
        "missing code" "HCSEMA0061" (Binding.error_code error);
      Alcotest.(check string)
        "missing message"
        "top-level implicit output call to \"Print\" is missing required \
         argument 2 (value)"
        (Binding.error_message error);
      Alcotest.(check bool)
        "missing error points at the output marker" true
        (Binding.error_origin error
        = Some
            (missing.targets |> Target.outputs |> List.hd
           |> Target.output_marker_origin)));
  let extra =
    prepare ~path:"top-level-output-extra-fixed.HC"
      "extern U0 Print(U8 *fmt);\"value\",1;"
    |> analyze
  in
  match bind extra with
  | Ok _ -> Alcotest.fail "expected a nonvariadic extra value"
  | Error error ->
      Alcotest.(check string)
        "extra code" "HCSEMA0062" (Binding.error_code error);
      Alcotest.(check string)
        "extra message"
        "top-level implicit output call to \"Print\" provides argument 2, but \
         its selected header has 1 fixed parameter"
        (Binding.error_message error)

let outer_headers_are_exact_or_deferred () =
  List.iter
    (fun mode ->
      let source =
        prepare ~mode ~path:"top-level-output-outer-bindings.HC"
          "\"\" 1;extern U0 Print(F64 fmt);extern U0 PutChars(U64 ch);"
      in
      let print = header_named source "Print" in
      let put_chars = header_named source "PutChars" in
      let print_symbol =
        Semantic_function_type_resolution.function_symbol print
      in
      let entry =
        Semantic_outer_environment.make_entry ~symbol:print_symbol
          ~record_kind:Semantic_outer_environment.Function ~entry_index:0
        |> Test_top_level_implicit_output_target_resolution.checked_outer
      in
      let table_kind =
        match mode with
        | Preprocessor.Jit -> Semantic_outer_environment.Jit_task 0
        | Preprocessor.Aot -> Semantic_outer_environment.Aot_parent 0
      in
      let table =
        Semantic_outer_environment.make_table ~table_kind ~table_index:0
          [ entry ]
        |> Test_top_level_implicit_output_target_resolution.checked_outer
      in
      let assembler =
        Semantic_outer_environment.make_table
          ~table_kind:Semantic_outer_environment.Assembler ~table_index:1 []
        |> Test_top_level_implicit_output_target_resolution.checked_outer
      in
      let environment =
        Holyc_lib.create_outer_environment source.session ~compilation_mode:mode
          [ table; assembler ]
        |> checked
      in
      let inputs = analyze ~environment source in
      let no_header =
        bind inputs |> checked_binding |> Binding.outputs |> List.hd
      in
      Alcotest.(check string)
        "missing outer signature is explicit" "deferred-outer-header"
        (Binding.output_result_name no_header);
      let wrong_header =
        bind ~outer_headers:[ put_chars ] inputs
        |> checked_binding |> Binding.outputs |> List.hd
      in
      ignore (deferred wrong_header);
      let output =
        bind ~outer_headers:[ print ] inputs
        |> checked_binding |> Binding.outputs |> List.hd |> bound
      in
      Alcotest.(check (list string))
        "exact outer signature enables conversion"
        [ "provided:integer-result:ICF_RES_TO_F64" ]
        (fixed_path_names output))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let replay_is_pure_and_validates_inputs () =
  let source =
    prepare ~path:"top-level-output-binding-replay.HC"
      "extern U0 Print(U8 *fmt,...);\"%d\",1;"
  in
  let inputs = analyze source in
  let table = Session.semantic_symbols source.session in
  let before = Semantic_symbol_table.all_symbols table |> List.length in
  let first = bind inputs |> checked_binding in
  let second = bind inputs |> checked_binding in
  Alcotest.(check bool)
    "binding owns the semantic table" true
    (Binding.owns_table first table);
  Alcotest.(check bool)
    "binding keeps the conversion policy" true
    (Binding.policies first == inputs.policies);
  Alcotest.(check string)
    "binding keeps compilation mode" "jit"
    (first |> Binding.compilation_mode
   |> Semantic_function_resolution.compilation_mode_name);
  Alcotest.(check (list string))
    "binding replay is deterministic"
    (first |> Binding.outputs |> List.map Binding.output_result_name)
    (second |> Binding.outputs |> List.map Binding.output_result_name);
  Alcotest.(check int)
    "binding leaves semantic symbols unchanged" before
    (Semantic_symbol_table.all_symbols table |> List.length);
  let foreign = Session.create () in
  (match
     Holyc_lib.bind_top_level_implicit_output_arguments foreign
       ~policies:inputs.policies inputs.targets
   with
  | Ok _ -> Alcotest.fail "expected a foreign-session binding failure"
  | Error error ->
      Alcotest.(check string)
        "foreign-session code" "HCSEMA0060" (Binding.error_code error));
  let aot_policies =
    Test_top_level_expression_result.policies_for_mode source Preprocessor.Aot
  in
  (match
     Holyc_lib.bind_top_level_implicit_output_arguments source.session
       ~policies:aot_policies inputs.targets
   with
  | Ok _ -> Alcotest.fail "expected a policy mode mismatch"
  | Error error ->
      Alcotest.(check string)
        "mode mismatch code" "HCSEMA0060" (Binding.error_code error));
  let print = header_named source "Print" in
  match bind ~outer_headers:[ print; print ] inputs with
  | Ok _ -> Alcotest.fail "expected duplicate outer headers to fail"
  | Error error ->
      Alcotest.(check string)
        "duplicate outer-header code" "HCSEMA0060" (Binding.error_code error)

let shared_rules_preserve_function_binding () =
  let source =
    prepare ~path:"top-level-output-shared-rules.HC"
      "extern U0 Print(U8 *fmt,...);I64 Caller(){\"%d\",1;return 0;}"
  in
  let environment =
    Test_implicit_output_argument_binding.environment source Preprocessor.Jit []
  in
  let inputs =
    Test_implicit_output_argument_binding.semantic_inputs source environment
  in
  let output =
    Test_implicit_output_argument_binding.bind inputs
    |> Test_implicit_output_argument_binding.checked_bindings
    |> fun result ->
    Test_implicit_output_argument_binding.outputs_named result "Caller"
    |> List.hd |> Test_implicit_output_argument_binding.bound
  in
  Alcotest.(check (list string))
    "function-body adapter still uses the same plan"
    [ "provided:integer-result:none" ]
    (Test_implicit_output_argument_binding.fixed_path_names output)

let tests =
  [
    Alcotest.test_case "selected headers and variadic tail" `Quick
      standard_bindings_follow_selected_headers;
    Alcotest.test_case "defaults and generated provenance" `Quick
      defaults_keep_mode_and_generated_provenance;
    Alcotest.test_case "missing and extra values" `Quick
      missing_and_extra_values_have_stable_diagnostics;
    Alcotest.test_case "outer header matching" `Quick
      outer_headers_are_exact_or_deferred;
    Alcotest.test_case "replay, purity, and validation" `Quick
      replay_is_pure_and_validates_inputs;
    Alcotest.test_case "shared function-body rules" `Quick
      shared_rules_preserve_function_binding;
  ]
