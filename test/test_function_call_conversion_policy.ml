open Holyc_lib

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let checked_policy = function
  | Ok value -> value
  | Error error ->
      error |> Semantic_function_call_conversion_policy.error_to_string
      |> Alcotest.fail

let expect_ast = function
  | Ok ast -> ast
  | Error diagnostics ->
      Alcotest.failf "expected an AST, got %s"
        (diagnostics
        |> List.map (fun diagnostic ->
            Printf.sprintf "%s: %s" diagnostic.Diagnostic.code
              diagnostic.message)
        |> String.concat ", ")

let config ?working_directory mode =
  checked
    (Preprocessor.Config.create ?working_directory ~compilation_mode:mode ())

type prepared = {
  session : Session.t;
  declarations : Semantic_declaration_collection.t;
  headers : Semantic_aggregate_header_resolution.t;
  members : Semantic_aggregate_member_index.t;
  calls : Semantic_function_call_resolution.t;
}

let finish_prepare mode session ast =
  let declarations = checked (Holyc_lib.collect_declarations session ast) in
  let aggregates =
    checked (Holyc_lib.resolve_aggregates session ~declarations ast)
  in
  let headers =
    checked
      (Holyc_lib.resolve_aggregate_headers session ~declarations ~aggregates ast)
  in
  let collected_members =
    checked (Holyc_lib.collect_members session ~declarations ast)
  in
  let typed_members =
    checked
      (Holyc_lib.resolve_member_types session ~declarations ~aggregates ~headers
         ~members:collected_members ast)
  in
  let layouts =
    checked
      (Holyc_lib.layout_aggregates session ~declarations ~aggregates ~headers
         ~members:typed_members ast)
  in
  let members =
    checked
      (Holyc_lib.index_aggregate_members session ~declarations ~headers
         ~members:typed_members ~layouts)
  in
  let collected_functions =
    checked (Holyc_lib.collect_functions session ~declarations ast)
  in
  let function_types =
    checked
      (Holyc_lib.resolve_function_types session ~declarations ~aggregates
         ~functions:collected_functions ast)
  in
  let local_types =
    checked
      (Holyc_lib.resolve_local_types session ~declarations ~aggregates
         ~functions:collected_functions ast)
  in
  let bindings =
    checked
      (Holyc_lib.index_function_bindings session ~declarations
         ~functions:collected_functions ~function_types ~local_types)
  in
  let expressions =
    checked
      (Holyc_lib.resolve_function_expressions session ~declarations
         ~functions:collected_functions ~local_types ~bindings ast)
  in
  let global_types =
    checked
      (Holyc_lib.resolve_global_types session ~declarations ~aggregates ast)
  in
  let functions =
    checked
      (Holyc_lib.resolve_function_identities session ~declarations
         ~functions:function_types ~compilation_mode:mode ast)
  in
  let globals =
    checked
      (Holyc_lib.resolve_global_records session ~declarations
         ~globals:global_types ~compilation_mode:mode ast)
  in
  let module_expressions =
    checked
      (Holyc_lib.resolve_module_expressions session ~declarations ~aggregates
         ~functions ~globals ~expressions)
  in
  let calls =
    checked
      (Holyc_lib.resolve_function_calls session ~declarations ~function_types
         ~local_types ~global_types ~functions ~expressions:module_expressions
         ast)
  in
  { session; declarations; headers; members; calls }

let prepare ?(mode = Preprocessor.Jit) ~path contents =
  let session = Session.create () in
  let source = Session.add_source session ~path ~contents in
  let ast =
    Holyc_lib.parse_with_config session ~config:(config mode) ~source
    |> expect_ast
  in
  finish_prepare mode session ast

let analyze prepared =
  Holyc_lib.analyze_function_call_conversions prepared.session
    ~declarations:prepared.declarations ~headers:prepared.headers
    ~calls:prepared.calls

let function_named result name =
  Semantic_function_call_conversion_policy.functions result
  |> List.filter (fun function_ ->
      function_ |> Semantic_function_call_conversion_policy.function_symbol
      |> Semantic_symbol.name |> String.equal name)
  |> List.rev |> List.hd

let direct = function
  | Semantic_function_call_conversion_policy.Direct_call_policy call -> call
  | Semantic_function_call_conversion_policy.Indirect_call_policy _ ->
      Alcotest.fail "expected a direct call, got an indirect call"
  | Semantic_function_call_conversion_policy.Deferred_call_policy _ ->
      Alcotest.fail "expected a direct call conversion policy"

let direct_calls result name =
  function_named result name
  |> Semantic_function_call_conversion_policy.function_calls |> List.map direct

let only_direct result name =
  match direct_calls result name with
  | [ call ] -> call
  | calls ->
      Alcotest.failf "expected one direct call in %s, got %d" name
        (List.length calls)

let path_names call =
  call |> Semantic_function_call_conversion_policy.direct_fixed_policies
  |> List.map (fun fixed ->
      fixed |> Semantic_function_call_conversion_policy.fixed_path
      |> Semantic_function_call_conversion_policy.fixed_path_name)

let public_and_intrinsic_primitives () =
  let prepared =
    prepare ~path:"call-conversion-primitives.HC"
      "extern I64 Target(I0 i0,I8 i8,I16 i16,I32 i32,I64 i64,U0 u0,U8 u8,U16 \
       u16,U32 u32,U64 u64,F64 f64,Bool b,I64i storage_i64,F64i storage_f64);\n\
       I64 Caller(){return Target(0,0,0,0,0,0,0,0,0,0,0,0,0,0);}"
  in
  let paths =
    analyze prepared |> checked_policy |> fun result ->
    only_direct result "Caller" |> path_names
  in
  Alcotest.(check (list string))
    "only public and intrinsic F64 use the floating target"
    [
      "provided-expression:integer-result";
      "provided-expression:integer-result";
      "provided-expression:integer-result";
      "provided-expression:integer-result";
      "provided-expression:integer-result";
      "provided-expression:integer-result";
      "provided-expression:integer-result";
      "provided-expression:integer-result";
      "provided-expression:integer-result";
      "provided-expression:integer-result";
      "provided-expression:f64-result";
      "provided-expression:integer-result";
      "provided-expression:integer-result";
      "provided-expression:f64-result";
    ]
    paths

let pointers_and_callbacks_use_integer_results () =
  let prepared =
    prepare ~path:"call-conversion-pointers.HC"
      "extern I64 Target(F64 scalar,F64 *pointer,F64i **storage_pointer,F64 \
       (*callback)());\n\
       I64 Caller(){return Target(0,0,0,0);}"
  in
  let paths =
    analyze prepared |> checked_policy |> fun result ->
    only_direct result "Caller" |> path_names
  in
  Alcotest.(check (list string))
    "pointer shape wins over pointee and callback return classes"
    [
      "provided-expression:f64-result";
      "provided-expression:integer-result";
      "provided-expression:integer-result";
      "provided-expression:integer-result";
    ]
    paths

let aggregate_backing_chains_are_forwarded () =
  let prepared =
    prepare ~path:"call-conversion-backing.HC"
      "extern class Forward;\n\
       F64 class FloatBox {};\n\
       FloatBox class FloatChain {};\n\
       F64i class StorageFloat {};\n\
       F64 * class PointerBox {};\n\
       class Plain {};\n\
       extern I64 Target(FloatBox a,FloatChain b,StorageFloat c,PointerBox \
       d,Plain e,Forward f);\n\
       I64 Caller(){return Target(0,0,0,0,0,0);}"
  in
  let paths =
    analyze prepared |> checked_policy |> fun result ->
    only_direct result "Caller" |> path_names
  in
  Alcotest.(check (list string))
    "by-value backing chains reach their forwarded raw class"
    [
      "provided-expression:f64-result";
      "provided-expression:f64-result";
      "provided-expression:f64-result";
      "provided-expression:integer-result";
      "provided-expression:integer-result";
      "provided-expression:integer-result";
    ]
    paths

let defaults_and_variadics_keep_separate_paths () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-conversion-defaults.HC"
          "extern I64 Mix(F64 floating=1,I64 integer=2,...);\n\
           I64 Caller(){Mix;return Mix(,2,3,4);}"
      in
      let calls =
        analyze prepared |> checked_policy |> fun result ->
        direct_calls result "Caller"
      in
      Alcotest.(check int) "two calls" 2 (List.length calls);
      Alcotest.(check (list string))
        "parenthesis-free defaults are direct values"
        [ "declared-default"; "declared-default" ]
        (List.nth calls 0 |> path_names);
      Alcotest.(check (list string))
        "provided fixed value keeps its integer target"
        [ "declared-default"; "provided-expression:integer-result" ]
        (List.nth calls 1 |> path_names);
      Alcotest.(check int)
        "variadic expressions stay outside fixed conversion policy" 2
        (List.nth calls 1
       |> Semantic_function_call_conversion_policy.direct_variadic_arguments
       |> List.length))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let source_visible_headers_choose_the_policy () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-conversion-visible.HC"
          "extern I64 Pick(I64 value);\n\
           I64 Before(){return Pick(1);}\n\
           extern I64 Pick(F64 value);\n\
           I64 After(){return Pick(1);}"
      in
      let result = analyze prepared |> checked_policy in
      Alcotest.(check (list string))
        "earlier caller keeps the first header"
        [ "provided-expression:integer-result" ]
        (only_direct result "Before" |> path_names);
      Alcotest.(check (list string))
        "later caller uses the replacement header"
        [ "provided-expression:f64-result" ]
        (only_direct result "After" |> path_names);
      let before =
        only_direct result "Before"
        |> Semantic_function_call_conversion_policy.direct_source
        |> Semantic_function_call_resolution.direct_target_symbol
      in
      let after =
        only_direct result "After"
        |> Semantic_function_call_conversion_policy.direct_source
        |> Semantic_function_call_resolution.direct_target_symbol
      in
      Alcotest.(check int)
        "both headers retain one joined target"
        (Semantic_symbol.id before |> Semantic_symbol.Id.to_int)
        (Semantic_symbol.id after |> Semantic_symbol.Id.to_int))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let cycles_and_foreign_inputs_are_rejected () =
  let cyclic =
    prepare ~path:"call-conversion-cycle.HC"
      "extern class A; A class B {}; B class A {};\n\
       extern I64 Target(A value);I64 Caller(){return Target(0);}"
  in
  (match analyze cyclic with
  | Ok _ -> Alcotest.fail "expected an aggregate backing cycle"
  | Error error ->
      Alcotest.(check string)
        "cycle diagnostic" "HCSEMA0044"
        (Semantic_function_call_conversion_policy.error_code error));
  let prepared =
    prepare ~path:"call-conversion-valid.HC"
      "extern I64 Target(F64 value);I64 Caller(){return Target(0);}"
  in
  let table = Session.semantic_symbols prepared.session in
  let before = Semantic_symbol_table.all_symbols table |> List.length in
  let first =
    analyze prepared |> checked_policy |> fun result ->
    only_direct result "Caller" |> path_names
  in
  let second =
    analyze prepared |> checked_policy |> fun result ->
    only_direct result "Caller" |> path_names
  in
  Alcotest.(check (list string))
    "repeated policy analysis is deterministic" first second;
  Alcotest.(check int)
    "policy analysis does not mutate symbols" before
    (Semantic_symbol_table.all_symbols table |> List.length);
  let foreign = Session.create () in
  match
    Holyc_lib.analyze_function_call_conversions foreign
      ~declarations:prepared.declarations ~headers:prepared.headers
      ~calls:prepared.calls
  with
  | Ok _ -> Alcotest.fail "expected a foreign-session failure"
  | Error error -> (
      Alcotest.(check string)
        "foreign session diagnostic" "HCSEMA0043"
        (Semantic_function_call_conversion_policy.error_code error);
      let session = Session.create () in
      let prepare_same_session path contents =
        let source = Session.add_source session ~path ~contents in
        let ast =
          Holyc_lib.parse_with_config session ~config:(config Preprocessor.Jit)
            ~source
          |> expect_ast
        in
        finish_prepare Preprocessor.Jit session ast
      in
      let first =
        prepare_same_session "call-conversion-first-module.HC"
          "extern I64 First(F64 value);I64 Caller(){return First(0);}"
      in
      let second =
        prepare_same_session "call-conversion-second-module.HC"
          "F64 class Other {};extern I64 Second(Other value);"
      in
      match
        Holyc_lib.analyze_function_call_conversions session
          ~declarations:first.declarations ~headers:second.headers
          ~calls:first.calls
      with
      | Ok _ -> Alcotest.fail "expected mismatched module inputs to fail"
      | Error error ->
          Alcotest.(check string)
            "mismatched module diagnostic" "HCSEMA0043"
            (Semantic_function_call_conversion_policy.error_code error))

let generated_and_included_provenance_survives () =
  let generated =
    prepare ~path:"call-conversion-generated.HC"
      "#define TARGET Callee\n\
       extern I64 Callee(F64 value);\n\
       I64 Caller(){return TARGET(0);}"
  in
  let generated_call =
    analyze generated |> checked_policy |> fun result ->
    only_direct result "Caller"
    |> Semantic_function_call_conversion_policy.direct_source
    |> Semantic_function_call_resolution.direct_source
  in
  let generated_origin =
    generated_call |> Semantic_function_call_resolution.call_callee_origin
  in
  (match generated_origin with
  | Semantic_symbol.Source_location location ->
      Alcotest.(check bool)
        "generated call keeps its invocation" true
        (Option.is_some location.generated_from);
      Alcotest.(check bool)
        "generated call keeps its definition" true
        (Option.is_some location.defined_at)
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected generated call provenance");
  let directory = Filename.temp_dir "holyc-call-policy-" "" in
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
      let include_path = Filename.concat directory "calls.HC" in
      write_file root_path "#include \"calls\"";
      write_file include_path
        "extern I64 Included(F64 value);I64 Caller(){return Included(0);}";
      let session = Session.create () in
      let source = checked (Session.load_source session ~path:root_path) in
      let ast =
        Holyc_lib.parse_with_config session
          ~config:(config ~working_directory:directory Preprocessor.Jit)
          ~source
        |> expect_ast
      in
      let prepared = finish_prepare Preprocessor.Jit session ast in
      let call_origin =
        analyze prepared |> checked_policy |> fun result ->
        only_direct result "Caller"
        |> Semantic_function_call_conversion_policy.direct_source
        |> Semantic_function_call_resolution.direct_source
        |> Semantic_function_call_resolution.call_origin
      in
      match call_origin with
      | Semantic_symbol.Source_location location ->
          let call_source =
            Source_manager.find (Session.sources session) location.span.source
            |> Option.get
          in
          Alcotest.(check string)
            "included call keeps its source file" "calls.HC"
            (Source_file.path call_source |> Filename.basename)
      | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
          Alcotest.fail "expected included call provenance")

let tests =
  [
    Alcotest.test_case "public and intrinsic primitive targets" `Quick
      public_and_intrinsic_primitives;
    Alcotest.test_case "pointer and callback targets" `Quick
      pointers_and_callbacks_use_integer_results;
    Alcotest.test_case "aggregate backing chains" `Quick
      aggregate_backing_chains_are_forwarded;
    Alcotest.test_case "defaults and variadic paths" `Quick
      defaults_and_variadics_keep_separate_paths;
    Alcotest.test_case "source-visible replacement headers" `Quick
      source_visible_headers_choose_the_policy;
    Alcotest.test_case "cycles, foreign inputs, and purity" `Quick
      cycles_and_foreign_inputs_are_rejected;
    Alcotest.test_case "generated and included provenance" `Quick
      generated_and_included_provenance_survives;
  ]
