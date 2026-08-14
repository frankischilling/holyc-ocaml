open Holyc_lib

let checked_decision = function
  | Ok value -> value
  | Error error ->
      error |> Semantic_function_call_conversion_decision.error_to_string
      |> Alcotest.fail

let prepare = Test_function_call_conversion_policy.prepare

let decide prepared =
  let policies =
    Test_function_call_conversion_policy.analyze prepared
    |> Test_function_call_conversion_policy.checked_policy
  in
  Holyc_lib.decide_function_call_conversions prepared.session ~policies

let function_named result name =
  Semantic_function_call_conversion_decision.functions result
  |> List.filter (fun function_ ->
      function_ |> Semantic_function_call_conversion_decision.function_symbol
      |> Semantic_symbol.name |> String.equal name)
  |> List.rev |> List.hd

let direct = function
  | Semantic_function_call_conversion_decision.Direct_call_decision call -> call
  | Semantic_function_call_conversion_decision.Deferred_call_decision _ ->
      Alcotest.fail "expected a direct call conversion decision"

let direct_calls result name =
  function_named result name
  |> Semantic_function_call_conversion_decision.function_calls
  |> List.map direct

let only_direct result name =
  match direct_calls result name with
  | [ call ] -> call
  | calls ->
      Alcotest.failf "expected one direct call in %s, got %d" name
        (List.length calls)

let direct_named result owner callee =
  function_named result owner
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
    | Semantic_function_call_conversion_decision.Deferred_call_decision _ ->
        None)
  |> function
  | [ call ] -> call
  | calls ->
      Alcotest.failf "expected one direct call to %s in %s, got %d" callee owner
        (List.length calls)

let path_names call =
  call |> Semantic_function_call_conversion_decision.direct_fixed_decisions
  |> List.map (fun fixed ->
      fixed |> Semantic_function_call_conversion_decision.fixed_path
      |> Semantic_function_call_conversion_decision.fixed_path_name)

let provided_argument fixed =
  match
    fixed |> Semantic_function_call_conversion_decision.fixed_source
    |> Semantic_function_call_conversion_policy.fixed_source
    |> Semantic_function_call_resolution.fixed_value
  with
  | Semantic_function_call_resolution.Provided_argument argument -> argument
  | Semantic_function_call_resolution.Declared_default _ ->
      Alcotest.fail "expected a provided fixed argument"

let provided_expression fixed =
  fixed |> provided_argument
  |> Semantic_function_call_resolution.argument_expression |> Option.get

let literal_directions_and_expression_retention () =
  let prepared =
    prepare ~path:"call-decision-literals.HC"
      "extern I64 Target(F64 a,I64 b,F64 c,I64 d,F64 e,I64 f,F64 g,I64 h);\n\
       I64 Caller(){return Target(1,2.5,(3),(4.5),'A',\"x\",6.0,7);}"
  in
  let fixed =
    decide prepared |> checked_decision |> fun result ->
    only_direct result "Caller"
    |> Semantic_function_call_conversion_decision.direct_fixed_decisions
  in
  Alcotest.(check (list string))
    "literal classes select the source conversion branch"
    [
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:f64-result:ICF_RES_TO_INT";
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:f64-result:ICF_RES_TO_INT";
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:integer-result:none";
      "provided:f64-result:none";
      "provided:integer-result:none";
    ]
    (fixed
    |> List.map (fun fixed ->
        fixed |> Semantic_function_call_conversion_decision.fixed_path
        |> Semantic_function_call_conversion_decision.fixed_path_name));
  Alcotest.(check (list string))
    "provided arguments retain their semantic source kinds"
    [
      "integer-literal";
      "float-literal";
      "parenthesized";
      "parenthesized";
      "character-literal";
      "string-literal";
      "float-literal";
      "integer-literal";
    ]
    (fixed
    |> List.map (fun fixed ->
        fixed |> provided_expression
        |> Semantic_function_call_resolution.argument_expression_kind
        |> Semantic_function_call_resolution.argument_expression_kind_name))

let pointer_callback_and_backed_targets () =
  let prepared =
    prepare ~path:"call-decision-targets.HC"
      "F64 class FloatBox {};F64 * class PointerBox {};class Plain {};\n\
       extern I64 Target(F64 *pointer,F64 (*callback)(),FloatBox \
       box,PointerBox backed,Plain plain);\n\
       I64 Caller(){return Target(1.0,1.0,1,1.0,1.0);}"
  in
  let paths =
    decide prepared |> checked_decision |> fun result ->
    only_direct result "Caller" |> path_names
  in
  Alcotest.(check (list string))
    "target policy and literal class combine without collapsing pointers"
    [
      "provided:f64-result:ICF_RES_TO_INT";
      "provided:f64-result:ICF_RES_TO_INT";
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:f64-result:ICF_RES_TO_INT";
      "provided:f64-result:ICF_RES_TO_INT";
    ]
    paths

let unsupported_expression_classes_stay_unresolved () =
  let prepared =
    prepare ~path:"call-decision-unresolved.HC"
      "class Box {I64 member;};\n\
       extern I64 Nested(I64 value);\n\
       extern I64 Target(F64 a,F64 b,F64 c,F64 d,F64 e,F64 f,F64 g,F64 h,F64 \
       i,F64 j,F64 k,F64 l);\n\
       I64 Caller(I64 value){I64 array[1];Box box;return \
       Target(value,$$,sizeof(I64),offset(Box.member),defined(value),-4,value++,3(F64),1+2,Nested(1),array[0],box.member);}"
  in
  let fixed =
    decide prepared |> checked_decision |> fun result ->
    direct_named result "Caller" "Target"
    |> Semantic_function_call_conversion_decision.direct_fixed_decisions
  in
  Alcotest.(check (list string))
    "unsupported actual classes remain explicit"
    [
      "provided:unresolved:unresolved";
      "provided:unresolved:unresolved";
      "provided:unresolved:unresolved";
      "provided:unresolved:unresolved";
      "provided:unresolved:unresolved";
      "provided:unresolved:unresolved";
      "provided:unresolved:unresolved";
      "provided:unresolved:unresolved";
      "provided:unresolved:unresolved";
      "provided:unresolved:unresolved";
      "provided:unresolved:unresolved";
      "provided:unresolved:unresolved";
    ]
    (fixed
    |> List.map (fun fixed ->
        fixed |> Semantic_function_call_conversion_decision.fixed_path
        |> Semantic_function_call_conversion_decision.fixed_path_name));
  Alcotest.(check (list string))
    "every unresolved top-level source kind remains named"
    [
      "identifier";
      "current-position";
      "sizeof";
      "offset";
      "defined";
      "prefix";
      "postfix";
      "postfix-cast";
      "binary";
      "call";
      "index";
      "member";
    ]
    (fixed
    |> List.map (fun fixed ->
        fixed |> provided_expression
        |> Semantic_function_call_resolution.argument_expression_kind
        |> Semantic_function_call_resolution.argument_expression_kind_name))

let defaults_and_variadics_remain_separate () =
  let prepared =
    prepare ~path:"call-decision-defaults.HC"
      "extern I64 Mix(F64 fixed=1,...);\nI64 Caller(){Mix;return Mix(,2.5);}"
  in
  let calls =
    decide prepared |> checked_decision |> fun result ->
    direct_calls result "Caller"
  in
  Alcotest.(check int) "two direct calls" 2 (List.length calls);
  List.iter
    (fun call ->
      Alcotest.(check (list string))
        "default never enters the literal conversion branch"
        [ "declared-default" ] (path_names call))
    calls;
  Alcotest.(check int)
    "the second call retains one variadic expression" 1
    (List.nth calls 1
   |> Semantic_function_call_conversion_decision.direct_variadic_arguments
   |> List.length);
  let variadic =
    List.nth calls 1
    |> Semantic_function_call_conversion_decision.direct_variadic_arguments
    |> List.hd |> Semantic_function_call_resolution.argument_expression
    |> Option.get
  in
  Alcotest.(check string)
    "variadic float source kind" "float-literal"
    (variadic |> Semantic_function_call_resolution.argument_expression_kind
   |> Semantic_function_call_resolution.argument_expression_kind_name)

let source_visible_headers_choose_literal_flags () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-decision-visible.HC"
          "extern I64 Pick(I64 value);\n\
           I64 Before(){return Pick(1.5);}\n\
           extern I64 Pick(F64 value);\n\
           I64 After(){return Pick(1);}"
      in
      let result = decide prepared |> checked_decision in
      Alcotest.(check (list string))
        "earlier header converts a float result to integer"
        [ "provided:f64-result:ICF_RES_TO_INT" ]
        (only_direct result "Before" |> path_names);
      Alcotest.(check (list string))
        "replacement header converts an integer result to F64"
        [ "provided:integer-result:ICF_RES_TO_F64" ]
        (only_direct result "After" |> path_names))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let deferred_provenance_foreign_and_purity () =
  let deferred =
    prepare ~path:"call-decision-deferred.HC"
      "I64 Caller(I64 (*callback)(F64)){return callback(1);}"
  in
  let deferred_calls =
    decide deferred |> checked_decision |> fun result ->
    function_named result "Caller"
    |> Semantic_function_call_conversion_decision.function_calls
  in
  (match deferred_calls with
  | [ Semantic_function_call_conversion_decision.Deferred_call_decision _ ] ->
      ()
  | _ -> Alcotest.fail "expected one deferred callback call");
  let generated =
    prepare ~path:"call-decision-generated.HC"
      "#define VALUE 1.0\n\
       extern I64 Target(I64 value);\n\
       I64 Caller(){return Target(VALUE);}"
  in
  let generated_expression =
    decide generated |> checked_decision |> fun result ->
    only_direct result "Caller"
    |> Semantic_function_call_conversion_decision.direct_fixed_decisions
    |> List.hd |> provided_expression
  in
  let generated_origin =
    Semantic_function_call_resolution.argument_expression_origin
      generated_expression
  in
  let generated_location =
    match generated_origin with
    | Semantic_symbol.Source_location location -> location
    | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
        Alcotest.fail "expected generated expression provenance"
  in
  (match
     Semantic_function_call_resolution.make_argument ~index:0
       ~kind:Semantic_function_call_resolution.Provided ~expression:None
       ~origin:generated_origin
   with
  | Ok _ -> Alcotest.fail "expected a missing provided expression to fail"
  | Error message ->
      Alcotest.(check string)
        "missing provided expression" "provided call argument has no expression"
        message);
  (match
     Semantic_function_call_resolution.make_argument ~index:0
       ~kind:Semantic_function_call_resolution.Omitted
       ~expression:(Some generated_expression) ~origin:generated_origin
   with
  | Ok _ -> Alcotest.fail "expected an expression-bearing omission to fail"
  | Error message ->
      Alcotest.(check string)
        "expression-bearing omission" "omitted call argument has an expression"
        message);
  Alcotest.(check bool)
    "generated literal keeps its invocation" true
    (Option.is_some generated_location.generated_from);
  Alcotest.(check bool)
    "generated literal keeps its definition" true
    (Option.is_some generated_location.defined_at);
  let table = Session.semantic_symbols generated.session in
  let before = Semantic_symbol_table.all_symbols table |> List.length in
  let first = decide generated |> checked_decision in
  let second = decide generated |> checked_decision in
  Alcotest.(check (list string))
    "repeated decisions are deterministic"
    (only_direct first "Caller" |> path_names)
    (only_direct second "Caller" |> path_names);
  Alcotest.(check int)
    "decision analysis does not mutate symbols" before
    (Semantic_symbol_table.all_symbols table |> List.length);
  let policies =
    Test_function_call_conversion_policy.analyze generated
    |> Test_function_call_conversion_policy.checked_policy
  in
  let foreign = Session.create () in
  (match Holyc_lib.decide_function_call_conversions foreign ~policies with
  | Ok _ -> Alcotest.fail "expected a foreign-session failure"
  | Error error ->
      Alcotest.(check string)
        "foreign session diagnostic" "HCSEMA0045"
        (Semantic_function_call_conversion_decision.error_code error));
  let directory = Filename.temp_dir "holyc-call-decision-" "" in
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
        "extern I64 Included(F64 value);I64 Caller(){return Included(1);}";
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
      let expression =
        decide prepared |> checked_decision |> fun result ->
        only_direct result "Caller"
        |> Semantic_function_call_conversion_decision.direct_fixed_decisions
        |> List.hd |> provided_expression
      in
      let location =
        match
          Semantic_function_call_resolution.argument_expression_origin
            expression
        with
        | Semantic_symbol.Source_location location -> location
        | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
            Alcotest.fail "expected included expression provenance"
      in
      let source_file =
        Source_manager.find (Session.sources session) location.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included literal keeps its source file" "calls.HC"
        (Source_file.path source_file |> Filename.basename))

let tests =
  [
    Alcotest.test_case "literal directions and expression retention" `Quick
      literal_directions_and_expression_retention;
    Alcotest.test_case "pointer, callback, and backed targets" `Quick
      pointer_callback_and_backed_targets;
    Alcotest.test_case "unsupported actual classes" `Quick
      unsupported_expression_classes_stay_unresolved;
    Alcotest.test_case "default and variadic paths" `Quick
      defaults_and_variadics_remain_separate;
    Alcotest.test_case "source-visible replacement headers" `Quick
      source_visible_headers_choose_literal_flags;
    Alcotest.test_case "deferred, provenance, foreign, and purity" `Quick
      deferred_provenance_foreign_and_purity;
  ]
