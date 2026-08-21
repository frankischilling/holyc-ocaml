open Holyc_lib

let checked_decision = function
  | Ok value -> value
  | Error error ->
      error |> Semantic_function_call_conversion_decision.error_to_string
      |> Alcotest.fail

let checked_expression_results = function
  | Ok value -> value
  | Error error ->
      error |> Semantic_function_call_expression_result.error_to_string
      |> Alcotest.fail

let prepare = Test_function_call_conversion_policy.prepare

let with_included_source contents apply =
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
      write_file include_path contents;
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

let decide prepared =
  let policies =
    Test_function_call_conversion_policy.analyze prepared
    |> Test_function_call_conversion_policy.checked_policy
  in
  let expressions =
    Holyc_lib.type_function_call_expressions prepared.session ~policies
    |> checked_expression_results
  in
  Holyc_lib.decide_function_call_conversions prepared.session ~policies
    ~expressions

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

let postfix_cast_parts expression =
  match
    Semantic_function_call_resolution.argument_expression_kind expression
  with
  | Semantic_function_call_resolution.Postfix_cast_expression (operand, target)
    -> (operand, target)
  | _ -> Alcotest.fail "expected a retained postfix cast target"

let prefix_parts expression =
  match
    Semantic_function_call_resolution.argument_expression_kind expression
  with
  | Semantic_function_call_resolution.Prefix_expression prefix -> prefix
  | _ -> Alcotest.fail "expected a retained prefix expression"

let postfix_parts expression =
  match
    Semantic_function_call_resolution.argument_expression_kind expression
  with
  | Semantic_function_call_resolution.Postfix_expression postfix -> postfix
  | _ -> Alcotest.fail "expected a retained postfix expression"

let binary_parts expression =
  match
    Semantic_function_call_resolution.argument_expression_kind expression
  with
  | Semantic_function_call_resolution.Binary_expression binary -> binary
  | _ -> Alcotest.fail "expected a retained binary expression"

let bound_identifier expression =
  match
    Semantic_function_call_resolution.argument_expression_kind expression
  with
  | Semantic_function_call_resolution.Bound_identifier_expression identifier ->
      identifier
  | _ -> Alcotest.fail "expected a retained bound identifier"

let bound_resolution_name identifier =
  let occurrence =
    Semantic_function_call_resolution.bound_identifier_occurrence identifier
  in
  match Semantic_module_expression_binding.occurrence_resolution occurrence with
  | Semantic_module_expression_binding.Local_binding binding ->
      "local:"
      ^ (binding |> Semantic_function_binding_index.binding_kind
       |> Semantic_function_binding_index.binding_kind_name)
  | Semantic_module_expression_binding.Module_binding publication ->
      "module:"
      ^ (publication |> Semantic_module_expression_binding.publication_kind
       |> Semantic_module_expression_binding.publication_kind_name)
  | Semantic_module_expression_binding.Outer_candidate -> "outer"

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

let source_expression_classes_stay_explicit () =
  let prepared =
    prepare ~path:"call-decision-unresolved.HC"
      "class Box {I64 member;};\n\
       extern I64 Nested(I64 value);\n\
       extern I64 Target(F64 a,F64 b,F64 c,F64 d,F64 e,F64 f,F64 g,F64 h,F64 \
       i,F64 j,F64 k,F64 l);\n\
       I64 Caller(I64 value){I64 array[1];Box box;return \
       Target(value,$$,sizeof(I64),offset(Box.member),defined(value),-4,value++,3(Box),1+2,Nested(1),array[0],box.member);}"
  in
  let fixed =
    decide prepared |> checked_decision |> fun result ->
    direct_named result "Caller" "Target"
    |> Semantic_function_call_conversion_decision.direct_fixed_decisions
  in
  Alcotest.(check (list string))
    "known primaries and unresolved actual classes remain explicit"
    [
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:integer-result:ICF_RES_TO_F64";
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
      "bound-identifier";
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

let prefix_directions_and_retention () =
  let prepared =
    prepare ~path:"call-decision-prefixes.HC"
      "F64 class FloatBox {};\n\
       extern I64 Target(F64 plus_int,I64 minus_float,F64 not_int,I64 \
       not_float,F64 address,I64 backed_minus,F64 complement,F64 \
       dereference,F64 increment,F64 decrement);\n\
       I64 Caller(I64 value){return \
       Target(+1,-2.5,!3,!4.5,&value,-value(FloatBox),~1,*(&value),++value,--value);}"
  in
  let fixed =
    decide prepared |> checked_decision |> fun result ->
    only_direct result "Caller"
    |> Semantic_function_call_conversion_decision.direct_fixed_decisions
  in
  Alcotest.(check (list string))
    "audited prefix classes select the source conversion branch"
    [
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:f64-result:ICF_RES_TO_INT";
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:f64-result:ICF_RES_TO_INT";
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:f64-result:ICF_RES_TO_INT";
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:integer-result:ICF_RES_TO_F64";
    ]
    (fixed
    |> List.map (fun fixed ->
        fixed |> Semantic_function_call_conversion_decision.fixed_path
        |> Semantic_function_call_conversion_decision.fixed_path_name));
  let prefixes =
    List.map (fun fixed -> fixed |> provided_expression |> prefix_parts) fixed
  in
  Alcotest.(check (list string))
    "prefix operators stay explicit"
    [
      "unary-plus";
      "unary-minus";
      "logical-not";
      "logical-not";
      "address-of";
      "unary-minus";
      "bitwise-not";
      "dereference";
      "pre-increment";
      "pre-decrement";
    ]
    (prefixes
    |> List.map (fun prefix ->
        prefix |> Semantic_function_call_resolution.prefix_operator
        |> Semantic_function_call_resolution.prefix_operator_name));
  Alcotest.(check string)
    "unary minus keeps the nested aggregate cast" "postfix-cast"
    (List.nth prefixes 5 |> Semantic_function_call_resolution.prefix_operand
   |> Semantic_function_call_resolution.argument_expression_kind
   |> Semantic_function_call_resolution.argument_expression_kind_name)

let bitwise_complement_is_an_integer_result () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-decision-complement.HC"
          "F64 class FloatBox {};\n\
           extern I64 Target(F64 integer,I64 float,F64 backed,F64 binary,F64 \
           outer,I64 nested);\n\
           I64 Caller(I64 value){return \
           Target(~1,~2.5,~value(FloatBox),~(1+2.5),~OuterValue,~(~2.5));}"
      in
      Alcotest.(check (list string))
        "complement produces an integer result for every retained operand shape"
        [
          "provided:integer-result:ICF_RES_TO_F64";
          "provided:integer-result:none";
          "provided:integer-result:ICF_RES_TO_F64";
          "provided:integer-result:ICF_RES_TO_F64";
          "provided:integer-result:ICF_RES_TO_F64";
          "provided:integer-result:none";
        ]
        ( decide prepared |> checked_decision |> fun result ->
          only_direct result "Caller" |> path_names ))
    [ Preprocessor.Jit; Preprocessor.Aot ];
  let generated =
    prepare ~path:"call-decision-complement-generated.HC"
      "#define COM ~\n\
       extern I64 Target(F64 value);\n\
       I64 Caller(){return Target(COM 2.5);}"
  in
  let table = Session.semantic_symbols generated.session in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  let first = decide generated |> checked_decision in
  let second = decide generated |> checked_decision in
  Alcotest.(check (list string))
    "replaying complement classification is stable"
    (only_direct first "Caller" |> path_names)
    (only_direct second "Caller" |> path_names);
  Alcotest.(check int)
    "replaying complement classification leaves symbols unchanged" symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length);
  let prefix =
    only_direct first "Caller"
    |> Semantic_function_call_conversion_decision.direct_fixed_decisions
    |> List.hd |> provided_expression |> prefix_parts
  in
  (match Semantic_function_call_resolution.prefix_operator_origin prefix with
  | Semantic_symbol.Source_location location ->
      Alcotest.(check bool)
        "generated complement keeps its invocation" true
        (Option.is_some location.generated_from);
      Alcotest.(check bool)
        "generated complement keeps its definition" true
        (Option.is_some location.defined_at)
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected generated complement provenance");
  with_included_source
    "extern I64 Target(I64 value);I64 Caller(){return Target(~2.5);}"
    (fun included ->
      let fixed =
        decide included |> checked_decision |> fun result ->
        only_direct result "Caller"
        |> Semantic_function_call_conversion_decision.direct_fixed_decisions
        |> List.hd
      in
      Alcotest.(check string)
        "included complement keeps its integer result"
        "provided:integer-result:none"
        (fixed |> Semantic_function_call_conversion_decision.fixed_path
       |> Semantic_function_call_conversion_decision.fixed_path_name);
      let location =
        match
          fixed |> provided_expression |> prefix_parts
          |> Semantic_function_call_resolution.prefix_operator_origin
        with
        | Semantic_symbol.Source_location location -> location
        | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
            Alcotest.fail "expected included complement provenance"
      in
      let source_file =
        Source_manager.find
          (Session.sources included.session)
          location.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included complement keeps its source file" "calls.HC"
        (Source_file.path source_file |> Filename.basename))

let prefix_source_order_modes_and_provenance () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-decision-prefix-source-order.HC"
          "extern I64 Target(I64 value);\n\
           extern class Later;\n\
           I64 Before(I64 value){return Target(-(value(Later)));}\n\
           F64 class Later {};\n\
           I64 After(I64 value){return Target(!(-(value(Later))));}"
      in
      let result = decide prepared |> checked_decision in
      Alcotest.(check (list string))
        "an earlier prefixed cast does not see a later backing"
        [ "provided:integer-result:none" ]
        (only_direct result "Before" |> path_names);
      Alcotest.(check (list string))
        "nested prefixes keep the later visible F64 class"
        [ "provided:f64-result:ICF_RES_TO_INT" ]
        (only_direct result "After" |> path_names))
    [ Preprocessor.Jit; Preprocessor.Aot ];
  let generated =
    prepare ~path:"call-decision-prefix-generated.HC"
      "#define NEG -\n\
       extern I64 Target(I64 value);\n\
       I64 Caller(){return Target(NEG 1.5);}"
  in
  let prefix =
    decide generated |> checked_decision |> fun result ->
    only_direct result "Caller"
    |> Semantic_function_call_conversion_decision.direct_fixed_decisions
    |> List.hd |> provided_expression |> prefix_parts
  in
  (match Semantic_function_call_resolution.prefix_operator_origin prefix with
  | Semantic_symbol.Source_location location ->
      Alcotest.(check bool)
        "generated prefix keeps its invocation" true
        (Option.is_some location.generated_from);
      Alcotest.(check bool)
        "generated prefix keeps its definition" true
        (Option.is_some location.defined_at)
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected generated prefix provenance");
  with_included_source
    "extern I64 Target(F64 value);I64 Caller(){return Target(-1);}"
    (fun included ->
      let prefix =
        decide included |> checked_decision |> fun result ->
        only_direct result "Caller"
        |> Semantic_function_call_conversion_decision.direct_fixed_decisions
        |> List.hd |> provided_expression |> prefix_parts
      in
      let location =
        match
          Semantic_function_call_resolution.prefix_operator_origin prefix
        with
        | Semantic_symbol.Source_location location -> location
        | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
            Alcotest.fail "expected included prefix provenance"
      in
      let source_file =
        Source_manager.find
          (Session.sources included.session)
          location.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included prefix keeps its source file" "calls.HC"
        (Source_file.path source_file |> Filename.basename))

let prefix_constructor_validation () =
  let operand =
    Semantic_function_call_resolution.make_argument_expression
      ~kind:Semantic_function_call_resolution.Integer_literal
      ~origin:(Semantic_symbol.Synthesized "prefix operand")
  in
  match
    Semantic_function_call_resolution.make_prefix_argument_expression
      ~operator:Semantic_function_call_resolution.Unary_minus
      ~operator_origin:(Semantic_symbol.Synthesized "") ~operand
  with
  | Ok _ -> Alcotest.fail "expected an invalid prefix origin to fail"
  | Error message ->
      Alcotest.(check string)
        "invalid prefix origin"
        "call argument prefix operator has an invalid source origin" message

let prefix_update_directions_and_provenance () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-decision-prefix-updates.HC"
          "extern I64 Target(F64 int_inc,I64 float_dec,F64 float_inc,I64 \
           unresolved_dec);\n\
           I64 Caller(I64 value){return Target(++1,--2.5,++3.5,--value);}"
      in
      Alcotest.(check (list string))
        "prefix updates forward audited operand classes"
        [
          "provided:integer-result:ICF_RES_TO_F64";
          "provided:f64-result:ICF_RES_TO_INT";
          "provided:f64-result:none";
          "provided:integer-result:none";
        ]
        ( decide prepared |> checked_decision |> fun result ->
          only_direct result "Caller" |> path_names ))
    [ Preprocessor.Jit; Preprocessor.Aot ];
  let generated =
    prepare ~path:"call-decision-prefix-update-generated.HC"
      "#define STEP ++\n\
       extern I64 Target(F64 value);\n\
       I64 Caller(){return Target(STEP 1);}"
  in
  let prefix =
    decide generated |> checked_decision |> fun result ->
    only_direct result "Caller"
    |> Semantic_function_call_conversion_decision.direct_fixed_decisions
    |> List.hd |> provided_expression |> prefix_parts
  in
  (match Semantic_function_call_resolution.prefix_operator_origin prefix with
  | Semantic_symbol.Source_location location ->
      Alcotest.(check bool)
        "generated prefix update keeps its invocation" true
        (Option.is_some location.generated_from);
      Alcotest.(check bool)
        "generated prefix update keeps its definition" true
        (Option.is_some location.defined_at)
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected generated prefix update provenance");
  with_included_source
    "extern I64 Target(I64 value);I64 Caller(){return Target(--2.5);}"
    (fun included ->
      let prefix =
        decide included |> checked_decision |> fun result ->
        only_direct result "Caller"
        |> Semantic_function_call_conversion_decision.direct_fixed_decisions
        |> List.hd |> provided_expression |> prefix_parts
      in
      let location =
        match
          Semantic_function_call_resolution.prefix_operator_origin prefix
        with
        | Semantic_symbol.Source_location location -> location
        | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
            Alcotest.fail "expected included prefix update provenance"
      in
      let source_file =
        Source_manager.find
          (Session.sources included.session)
          location.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included prefix update keeps its source file" "calls.HC"
        (Source_file.path source_file |> Filename.basename))

let postfix_directions_and_retention () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-decision-postfixes.HC"
          "F64 class FloatBox {};\n\
           extern I64 Target(F64 int_inc,I64 float_dec,I64 backed_inc,F64 \
           binary_inc,F64 unresolved_dec);\n\
           I64 Caller(I64 value){return \
           Target((1)++,(2.5)--,(3(FloatBox))++,((1+2))++,value--);}"
      in
      let fixed =
        decide prepared |> checked_decision |> fun result ->
        only_direct result "Caller"
        |> Semantic_function_call_conversion_decision.direct_fixed_decisions
      in
      Alcotest.(check (list string))
        "postfix expressions forward their audited operand classes"
        [
          "provided:integer-result:ICF_RES_TO_F64";
          "provided:f64-result:ICF_RES_TO_INT";
          "provided:f64-result:ICF_RES_TO_INT";
          "provided:integer-result:ICF_RES_TO_F64";
          "provided:integer-result:ICF_RES_TO_F64";
        ]
        (fixed
        |> List.map (fun decision ->
            decision |> Semantic_function_call_conversion_decision.fixed_path
            |> Semantic_function_call_conversion_decision.fixed_path_name));
      let postfixes =
        fixed
        |> List.map (fun decision ->
            decision |> provided_expression |> postfix_parts)
      in
      Alcotest.(check (list string))
        "postfix operators stay explicit"
        [
          "post-increment";
          "post-decrement";
          "post-increment";
          "post-increment";
          "post-decrement";
        ]
        (postfixes
        |> List.map (fun postfix ->
            postfix |> Semantic_function_call_resolution.postfix_operator
            |> Semantic_function_call_resolution.postfix_operator_name));
      Alcotest.(check string)
        "the backed postfix operand keeps its grouping" "parenthesized"
        (List.nth postfixes 2
       |> Semantic_function_call_resolution.postfix_operand
       |> Semantic_function_call_resolution.argument_expression_kind
       |> Semantic_function_call_resolution.argument_expression_kind_name))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let postfix_provenance () =
  let generated =
    prepare ~path:"call-decision-postfix-generated.HC"
      "#define STEP ++\n\
       extern I64 Target(F64 value);\n\
       I64 Caller(){return Target((1)STEP);}"
  in
  let postfix =
    decide generated |> checked_decision |> fun result ->
    only_direct result "Caller"
    |> Semantic_function_call_conversion_decision.direct_fixed_decisions
    |> List.hd |> provided_expression |> postfix_parts
  in
  (match Semantic_function_call_resolution.postfix_operator_origin postfix with
  | Semantic_symbol.Source_location location ->
      Alcotest.(check bool)
        "generated postfix keeps its invocation" true
        (Option.is_some location.generated_from);
      Alcotest.(check bool)
        "generated postfix keeps its definition" true
        (Option.is_some location.defined_at)
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected generated postfix provenance");
  with_included_source
    "extern I64 Target(F64 value);I64 Caller(){return Target((1)--);}"
    (fun included ->
      let postfix =
        decide included |> checked_decision |> fun result ->
        only_direct result "Caller"
        |> Semantic_function_call_conversion_decision.direct_fixed_decisions
        |> List.hd |> provided_expression |> postfix_parts
      in
      let location =
        match
          Semantic_function_call_resolution.postfix_operator_origin postfix
        with
        | Semantic_symbol.Source_location location -> location
        | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
            Alcotest.fail "expected included postfix provenance"
      in
      let source_file =
        Source_manager.find
          (Session.sources included.session)
          location.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included postfix keeps its source file" "calls.HC"
        (Source_file.path source_file |> Filename.basename))

let postfix_constructor_validation () =
  let operand =
    Semantic_function_call_resolution.make_argument_expression
      ~kind:Semantic_function_call_resolution.Integer_literal
      ~origin:(Semantic_symbol.Synthesized "postfix operand")
  in
  match
    Semantic_function_call_resolution.make_postfix_argument_expression
      ~operator:Semantic_function_call_resolution.Post_increment
      ~operator_origin:(Semantic_symbol.Synthesized "") ~operand
  with
  | Ok _ -> Alcotest.fail "expected an invalid postfix origin to fail"
  | Error message ->
      Alcotest.(check string)
        "invalid postfix origin"
        "call argument postfix operator has an invalid source origin" message

let binary_operator_inventory_and_assignments () =
  let cases =
    [
      ("1`2", "IC_POWER", "provided:f64-result:none");
      ("1<<2", "IC_SHL", "provided:integer-result:ICF_RES_TO_F64");
      ("1>>2", "IC_SHR", "provided:integer-result:ICF_RES_TO_F64");
      ("1*2", "IC_MUL", "provided:integer-result:ICF_RES_TO_F64");
      ("1/2", "IC_DIV", "provided:integer-result:ICF_RES_TO_F64");
      ("1%2", "IC_MOD", "provided:integer-result:ICF_RES_TO_F64");
      ("1&2", "IC_AND", "provided:integer-result:ICF_RES_TO_F64");
      ("1^2", "IC_XOR", "provided:integer-result:ICF_RES_TO_F64");
      ("1|2", "IC_OR", "provided:integer-result:ICF_RES_TO_F64");
      ("1+2", "IC_ADD", "provided:integer-result:ICF_RES_TO_F64");
      ("1-2", "IC_SUB", "provided:integer-result:ICF_RES_TO_F64");
      ("1<2", "IC_LESS", "provided:integer-result:ICF_RES_TO_F64");
      ("1>2", "IC_GREATER", "provided:integer-result:ICF_RES_TO_F64");
      ("1<=2", "IC_LESS_EQU", "provided:integer-result:ICF_RES_TO_F64");
      ("1>=2", "IC_GREATER_EQU", "provided:integer-result:ICF_RES_TO_F64");
      ("1==2", "IC_EQU_EQU", "provided:integer-result:ICF_RES_TO_F64");
      ("1!=2", "IC_NOT_EQU", "provided:integer-result:ICF_RES_TO_F64");
      ("1&&2", "IC_AND_AND", "provided:integer-result:ICF_RES_TO_F64");
      ("1^^2", "IC_XOR_XOR", "provided:integer-result:ICF_RES_TO_F64");
      ("1||2", "IC_OR_OR", "provided:integer-result:ICF_RES_TO_F64");
      ("value=1", "IC_ASSIGN", "provided:unresolved:unresolved");
      ("value<<=1", "IC_SHL_EQU", "provided:unresolved:unresolved");
      ("value>>=1", "IC_SHR_EQU", "provided:unresolved:unresolved");
      ("value*=1", "IC_MUL_EQU", "provided:unresolved:unresolved");
      ("value/=1", "IC_DIV_EQU", "provided:unresolved:unresolved");
      ("value%=1", "IC_MOD_EQU", "provided:unresolved:unresolved");
      ("value&=1", "IC_AND_EQU", "provided:unresolved:unresolved");
      ("value|=1", "IC_OR_EQU", "provided:unresolved:unresolved");
      ("value^=1", "IC_XOR_EQU", "provided:unresolved:unresolved");
      ("value+=1", "IC_ADD_EQU", "provided:unresolved:unresolved");
      ("value-=1", "IC_SUB_EQU", "provided:unresolved:unresolved");
    ]
  in
  let parameters =
    List.mapi (fun index _ -> Printf.sprintf "F64 p%d" index) cases
    |> String.concat ","
  in
  let arguments =
    cases |> List.map (fun (source, _, _) -> source) |> String.concat ","
  in
  let prepared =
    prepare ~path:"call-decision-binary-inventory.HC"
      (Printf.sprintf
         "extern I64 Target(%s);I64 Caller(I64 value){return Target(%s);}"
         parameters arguments)
  in
  let fixed =
    decide prepared |> checked_decision |> fun result ->
    only_direct result "Caller"
    |> Semantic_function_call_conversion_decision.direct_fixed_decisions
  in
  let checked_names =
    Operator.binary_operators
    |> List.map (fun (operator : Operator.binary_operator) -> operator.ic_name)
  in
  Alcotest.(check int)
    "the fixture covers the complete binary table"
    (List.length checked_names)
    (List.length cases);
  Alcotest.(check (list string))
    "binary operators retain the generated IC identity" checked_names
    (fixed
    |> List.map (fun fixed ->
        fixed |> provided_expression |> binary_parts
        |> Semantic_function_call_resolution.binary_operator
        |> Semantic_function_call_resolution.binary_operator_name));
  let first_binary = List.hd fixed |> provided_expression |> binary_parts in
  Alcotest.(check (list string))
    "binary operands remain independently recursive"
    [ "integer-literal"; "integer-literal" ]
    ([
       Semantic_function_call_resolution.binary_left first_binary;
       Semantic_function_call_resolution.binary_right first_binary;
     ]
    |> List.map (fun expression ->
        expression |> Semantic_function_call_resolution.argument_expression_kind
        |> Semantic_function_call_resolution.argument_expression_kind_name));
  Alcotest.(check (list string))
    "binary families select or defer their source result class"
    (cases |> List.map (fun (_, _, path) -> path))
    (fixed
    |> List.map (fun fixed ->
        fixed |> Semantic_function_call_conversion_decision.fixed_path
        |> Semantic_function_call_conversion_decision.fixed_path_name))

let binary_result_joins_and_nesting () =
  let prepared =
    prepare ~path:"call-decision-binary-joins.HC"
      "extern I64 Target(I64 a,I64 b,I64 c,I64 d,I64 e,I64 f,I64 g,I64 h,I64 \
       i,I64 j);\n\
       I64 Caller(I64 value,I64 other){return \
       Target(1+2.5,2.5+1,2.5+3.5,1+2,value+2.5,value+1,value`other,value<other,value&&other,-(1+2.5));}"
  in
  let fixed =
    decide prepared |> checked_decision |> fun result ->
    only_direct result "Caller"
    |> Semantic_function_call_conversion_decision.direct_fixed_decisions
  in
  Alcotest.(check (list string))
    "binary result classes follow the pinned operator families"
    [
      "provided:f64-result:ICF_RES_TO_INT";
      "provided:f64-result:ICF_RES_TO_INT";
      "provided:f64-result:ICF_RES_TO_INT";
      "provided:integer-result:none";
      "provided:f64-result:ICF_RES_TO_INT";
      "provided:integer-result:none";
      "provided:f64-result:ICF_RES_TO_INT";
      "provided:integer-result:none";
      "provided:integer-result:none";
      "provided:f64-result:ICF_RES_TO_INT";
    ]
    (fixed
    |> List.map (fun fixed ->
        fixed |> Semantic_function_call_conversion_decision.fixed_path
        |> Semantic_function_call_conversion_decision.fixed_path_name));
  let nested = List.nth fixed 9 |> provided_expression |> prefix_parts in
  let nested_kind =
    nested |> Semantic_function_call_resolution.prefix_operand
    |> Semantic_function_call_resolution.argument_expression_kind
  in
  let nested_binary =
    match nested_kind with
    | Semantic_function_call_resolution.Parenthesized_expression grouped ->
        grouped
    | _ -> Alcotest.fail "expected parentheses inside the prefix expression"
  in
  Alcotest.(check string)
    "a prefix retains its nested binary expression" "binary"
    (nested_binary |> Semantic_function_call_resolution.argument_expression_kind
   |> Semantic_function_call_resolution.argument_expression_kind_name)

let binary_source_order_in_both_modes () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-decision-binary-source-order.HC"
          "extern I64 Target(I64 value);\n\
           extern class Later;\n\
           I64 Before(I64 value){return Target(value(Later)+1);}\n\
           F64 class Later {};\n\
           I64 After(I64 value){return Target(value(Later)+1);}"
      in
      let result = decide prepared |> checked_decision in
      Alcotest.(check (list string))
        "an earlier binary cast uses the unbacked class"
        [ "provided:integer-result:none" ]
        (only_direct result "Before" |> path_names);
      Alcotest.(check (list string))
        "a later binary cast sees the published F64 backing"
        [ "provided:f64-result:ICF_RES_TO_INT" ]
        (only_direct result "After" |> path_names))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let binary_provenance_and_constructor_validation () =
  let generated =
    prepare ~path:"call-decision-binary-generated.HC"
      "#define PLUS +\n\
       extern I64 Target(F64 value);\n\
       I64 Caller(){return Target(1 PLUS 2);}"
  in
  let table = Session.semantic_symbols generated.session in
  let before = Semantic_symbol_table.all_symbols table |> List.length in
  let first = decide generated |> checked_decision in
  let second = decide generated |> checked_decision in
  Alcotest.(check (list string))
    "repeated binary decisions are deterministic"
    (only_direct first "Caller" |> path_names)
    (only_direct second "Caller" |> path_names);
  Alcotest.(check int)
    "binary decisions do not mutate symbols" before
    (Semantic_symbol_table.all_symbols table |> List.length);
  let binary =
    only_direct first "Caller"
    |> Semantic_function_call_conversion_decision.direct_fixed_decisions
    |> List.hd |> provided_expression |> binary_parts
  in
  (match Semantic_function_call_resolution.binary_operator_origin binary with
  | Semantic_symbol.Source_location location ->
      Alcotest.(check bool)
        "generated binary operator keeps its invocation" true
        (Option.is_some location.generated_from);
      Alcotest.(check bool)
        "generated binary operator keeps its definition" true
        (Option.is_some location.defined_at)
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected generated binary operator provenance");
  with_included_source
    "extern I64 Target(F64 value);I64 Caller(){return Target(1+2);}"
    (fun included ->
      let binary =
        decide included |> checked_decision |> fun result ->
        only_direct result "Caller"
        |> Semantic_function_call_conversion_decision.direct_fixed_decisions
        |> List.hd |> provided_expression |> binary_parts
      in
      let location =
        match
          Semantic_function_call_resolution.binary_operator_origin binary
        with
        | Semantic_symbol.Source_location location -> location
        | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
            Alcotest.fail "expected included binary operator provenance"
      in
      let source_file =
        Source_manager.find
          (Session.sources included.session)
          location.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included binary operator keeps its source file" "calls.HC"
        (Source_file.path source_file |> Filename.basename));
  let operand =
    Semantic_function_call_resolution.make_argument_expression
      ~kind:Semantic_function_call_resolution.Integer_literal
      ~origin:(Semantic_symbol.Synthesized "binary operand")
  in
  let make operator operator_origin =
    Semantic_function_call_resolution.make_binary_argument_expression ~operator
      ~operator_origin ~left:operand ~right:operand
  in
  (match
     make Ir_opcode.Ic_addr (Semantic_symbol.Synthesized "binary operator")
   with
  | Ok _ -> Alcotest.fail "expected a nonbinary IC to fail"
  | Error message ->
      Alcotest.(check string)
        "nonbinary IC rejection" "IC_ADDR is not a checked binary operator"
        message);
  match make Ir_opcode.Ic_add (Semantic_symbol.Synthesized "") with
  | Ok _ -> Alcotest.fail "expected an invalid binary origin to fail"
  | Error message ->
      Alcotest.(check string)
        "invalid binary origin"
        "call argument binary operator has an invalid source origin" message

let primitive_postfix_cast_directions_and_retention () =
  let prepared =
    prepare ~path:"call-decision-postfix-casts.HC"
      "extern I64 Target(I64 scalar_f64,F64 scalar_int,F64 int_pointer,I64 \
       f64_pointer,F64 intrinsic_f64);\n\
       I64 Caller(I64 value){return Target(value(F64),value(I64),value(I64 \
       *),value(F64 *),value(F64i));}"
  in
  let fixed =
    decide prepared |> checked_decision |> fun result ->
    only_direct result "Caller"
    |> Semantic_function_call_conversion_decision.direct_fixed_decisions
  in
  Alcotest.(check (list string))
    "postfix cast targets select the call conversion"
    [
      "provided:f64-result:ICF_RES_TO_INT";
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:integer-result:none";
      "provided:f64-result:none";
    ]
    (fixed
    |> List.map (fun fixed ->
        fixed |> Semantic_function_call_conversion_decision.fixed_path
        |> Semantic_function_call_conversion_decision.fixed_path_name));
  let targets =
    List.map
      (fun fixed -> fixed |> provided_expression |> postfix_cast_parts |> snd)
      fixed
  in
  Alcotest.(check (list string))
    "cast source spellings stay explicit"
    [ "F64"; "I64"; "I64"; "F64"; "F64i" ]
    (List.map Semantic_type_reference.spelling targets);
  Alcotest.(check (list int))
    "cast pointer depths stay explicit" [ 0; 0; 1; 1; 0 ]
    (targets
    |> List.map (fun target ->
        target |> Semantic_type_reference.resolved_type
        |> Semantic_type.pointer_depth));
  Alcotest.(check (list int))
    "cast pointer origins match their depths" [ 0; 0; 1; 1; 0 ]
    (List.map
       (fun target ->
         target |> Semantic_type_reference.pointer_origins |> List.length)
       targets);
  let all_spellings =
    [
      "I0";
      "I8";
      "I16";
      "I32";
      "I64";
      "U0";
      "U8";
      "U16";
      "U32";
      "U64";
      "F64";
      "Bool";
      "I0i";
      "U0i";
      "I8i";
      "U8i";
      "I16i";
      "U16i";
      "I32i";
      "U32i";
      "I64i";
      "U64i";
      "F64i";
    ]
  in
  let parameters =
    all_spellings
    |> List.mapi (fun index _ -> Printf.sprintf "F64 p%d" index)
    |> String.concat ","
  in
  let arguments =
    all_spellings |> List.map (Printf.sprintf "value(%s)") |> String.concat ","
  in
  let every =
    prepare ~path:"call-decision-all-primitive-casts.HC"
      (Printf.sprintf
         "extern I64 Every(%s);I64 Caller(I64 value){return Every(%s);}"
         parameters arguments)
  in
  Alcotest.(check (list string))
    "every public and intrinsic scalar cast follows its raw result class"
    (List.map
       (fun spelling ->
         if String.equal spelling "F64" || String.equal spelling "F64i" then
           "provided:f64-result:none"
         else "provided:integer-result:ICF_RES_TO_F64")
       all_spellings)
    ( decide every |> checked_decision |> fun result ->
      only_direct result "Caller" |> path_names )

let outer_postfix_cast_wins_in_both_modes () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-decision-outer-postfix-cast.HC"
          "extern I64 Nested();\n\
           extern I64 Target(F64 from_binary,I64 from_value,F64 from_call,I64 \
           from_nested);\n\
           I64 Caller(I64 value){return \
           Target(((value+1)(I64)),value(F64),Nested()(I64),value(F64)(I64));}"
      in
      Alcotest.(check (list string))
        "the outer cast replaces the operand class"
        [
          "provided:integer-result:ICF_RES_TO_F64";
          "provided:f64-result:ICF_RES_TO_INT";
          "provided:integer-result:ICF_RES_TO_F64";
          "provided:integer-result:none";
        ]
        ( decide prepared |> checked_decision |> fun result ->
          direct_named result "Caller" "Target" |> path_names ))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let postfix_cast_provenance_and_purity () =
  let prepared =
    prepare ~path:"call-decision-postfix-cast-generated.HC"
      "#define CONVERT value(F64i)\n\
       extern I64 Target(I64 value);\n\
       I64 Caller(I64 value){return Target(CONVERT);}"
  in
  let result = decide prepared |> checked_decision in
  let fixed =
    only_direct result "Caller"
    |> Semantic_function_call_conversion_decision.direct_fixed_decisions
    |> List.hd
  in
  Alcotest.(check string)
    "generated intrinsic cast converts to integer"
    "provided:f64-result:ICF_RES_TO_INT"
    (fixed |> Semantic_function_call_conversion_decision.fixed_path
   |> Semantic_function_call_conversion_decision.fixed_path_name);
  let _, target = fixed |> provided_expression |> postfix_cast_parts in
  (match Semantic_type_reference.spelling_origin target with
  | Semantic_symbol.Source_location location ->
      Alcotest.(check bool)
        "generated cast target keeps its invocation" true
        (Option.is_some location.generated_from);
      Alcotest.(check bool)
        "generated cast target keeps its definition" true
        (Option.is_some location.defined_at)
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected generated cast target provenance");
  let table = Session.semantic_symbols prepared.session in
  let before = Semantic_symbol_table.all_symbols table |> List.length in
  Alcotest.(check (list string))
    "postfix cast decisions are deterministic"
    (only_direct result "Caller" |> path_names)
    ( decide prepared |> checked_decision |> fun next ->
      only_direct next "Caller" |> path_names );
  Alcotest.(check int)
    "postfix cast decisions do not mutate symbols" before
    (Semantic_symbol_table.all_symbols table |> List.length);
  with_included_source
    "extern I64 Included(F64 value);I64 Caller(I64 value){return \
     Included(value(I64 *));}" (fun included ->
      let fixed =
        decide included |> checked_decision |> fun included_result ->
        only_direct included_result "Caller"
        |> Semantic_function_call_conversion_decision.direct_fixed_decisions
        |> List.hd
      in
      Alcotest.(check string)
        "included pointer cast follows the integer conversion path"
        "provided:integer-result:ICF_RES_TO_F64"
        (fixed |> Semantic_function_call_conversion_decision.fixed_path
       |> Semantic_function_call_conversion_decision.fixed_path_name);
      let _, target = fixed |> provided_expression |> postfix_cast_parts in
      let location =
        match Semantic_type_reference.spelling_origin target with
        | Semantic_symbol.Source_location location -> location
        | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
            Alcotest.fail "expected included cast target provenance"
      in
      let source_file =
        Source_manager.find
          (Session.sources included.session)
          location.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included cast target keeps its source file" "calls.HC"
        (Source_file.path source_file |> Filename.basename))

let postfix_cast_target_validation () =
  let origin = Semantic_symbol.Synthesized "postfix cast validation" in
  let pointer_type =
    Semantic_type.make_primitive ~form:Semantic_type.Public_spelling
      ~primitive:Primitive_type.F64 ~pointer_depth:1
    |> function
    | Ok value -> value
    | Error message -> Alcotest.fail message
  in
  (match
     Semantic_type_reference.make ~spelling:"F64" ~spelling_origin:origin
       ~pointer_origins:[] ~resolved_type:pointer_type
   with
  | Ok _ -> Alcotest.fail "expected missing pointer provenance to fail"
  | Error message ->
      Alcotest.(check string)
        "pointer provenance diagnostic"
        "semantic type-reference pointer provenance does not match its type"
        message);
  let scalar_type =
    Semantic_type.make_primitive ~form:Semantic_type.Internal_storage
      ~primitive:Primitive_type.F64 ~pointer_depth:0
    |> function
    | Ok value -> value
    | Error message -> Alcotest.fail message
  in
  match
    Semantic_type_reference.make ~spelling:"F64" ~spelling_origin:origin
      ~pointer_origins:[] ~resolved_type:scalar_type
  with
  | Ok _ -> Alcotest.fail "expected an intrinsic spelling mismatch to fail"
  | Error message ->
      Alcotest.(check string)
        "intrinsic spelling diagnostic"
        "semantic type-reference spelling \"F64\" does not match \"F64i\""
        message

let named_postfix_cast_directions_and_backings () =
  let prepared =
    prepare ~path:"call-decision-named-postfix-casts.HC"
      "F64 class FloatBox {};\n\
       F64i class StorageFloat {};\n\
       I64 class IntBox {};\n\
       FloatBox class FloatChain {};\n\
       F64 * class PointerBox {};\n\
       class Plain {};\n\
       union PlainUnion {};\n\
       extern I64 Target(I64 float_box,F64 int_box,F64 chain,F64 \
       pointer_box,I64 plain,F64 union_pointer,F64 float_pointer,F64 \
       storage_float);\n\
       I64 Caller(I64 value){return \
       Target(value(FloatBox),value(IntBox),value(FloatChain),value(PointerBox),value(Plain),value(PlainUnion \
       *),value(FloatBox *),value(StorageFloat));}"
  in
  let fixed =
    decide prepared |> checked_decision |> fun result ->
    only_direct result "Caller"
    |> Semantic_function_call_conversion_decision.direct_fixed_decisions
  in
  Alcotest.(check (list string))
    "named cast backings select the actual conversion path"
    [
      "provided:f64-result:ICF_RES_TO_INT";
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:f64-result:none";
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:integer-result:none";
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:f64-result:none";
    ]
    (fixed
    |> List.map (fun fixed ->
        fixed |> Semantic_function_call_conversion_decision.fixed_path
        |> Semantic_function_call_conversion_decision.fixed_path_name));
  let targets =
    fixed
    |> List.map (fun fixed ->
        fixed |> provided_expression |> postfix_cast_parts |> snd)
  in
  Alcotest.(check (list string))
    "named cast spellings stay explicit"
    [
      "FloatBox";
      "IntBox";
      "FloatChain";
      "PointerBox";
      "Plain";
      "PlainUnion";
      "FloatBox";
      "StorageFloat";
    ]
    (List.map Semantic_type_reference.spelling targets);
  Alcotest.(check (list int))
    "named cast pointer depths stay explicit" [ 0; 0; 0; 0; 0; 1; 1; 0 ]
    (targets
    |> List.map (fun target ->
        target |> Semantic_type_reference.resolved_type
        |> Semantic_type.pointer_depth));
  Alcotest.(check (list string))
    "named casts retain canonical aggregate identities"
    [
      "FloatBox";
      "IntBox";
      "FloatChain";
      "PointerBox";
      "Plain";
      "PlainUnion";
      "FloatBox";
      "StorageFloat";
    ]
    (targets
    |> List.map (fun target ->
        match
          target |> Semantic_type_reference.resolved_type |> Semantic_type.base
        with
        | Semantic_type.Aggregate symbol -> Semantic_symbol.name symbol
        | Semantic_type.Primitive _ ->
            Alcotest.fail "expected a named aggregate cast target"))

let named_postfix_cast_source_order () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-decision-named-cast-source-order.HC"
          "extern I64 Target(I64 value);\n\
           F64 class Box {};\n\
           I64 BeforeShadow(I64 value){return Target(value(Box));}\n\
           I64 class Box {};\n\
           I64 AfterShadow(I64 value){return Target(value(Box));}\n\
           extern class Later;\n\
           I64 BeforeCompletion(I64 value){return Target(value(Later));}\n\
           F64 class Later {};\n\
           I64 AfterCompletion(I64 value){return Target(value(Later));}"
      in
      let result = decide prepared |> checked_decision in
      Alcotest.(check (list string))
        "an earlier function keeps the earlier same-name identity"
        [ "provided:f64-result:ICF_RES_TO_INT" ]
        (only_direct result "BeforeShadow" |> path_names);
      Alcotest.(check (list string))
        "a later function sees the shadowing identity"
        [ "provided:integer-result:none" ]
        (only_direct result "AfterShadow" |> path_names);
      Alcotest.(check (list string))
        "a later completion does not change an earlier forward cast"
        [ "provided:integer-result:none" ]
        (only_direct result "BeforeCompletion" |> path_names);
      Alcotest.(check (list string))
        "a function after completion sees the aggregate backing"
        [ "provided:f64-result:ICF_RES_TO_INT" ]
        (only_direct result "AfterCompletion" |> path_names))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let named_postfix_cast_provenance_and_outer_class () =
  let prepared =
    prepare ~path:"call-decision-named-cast-generated.HC"
      "#define CAST Box\n\
       F64 class Box {};\n\
       extern I64 Target(I64 value);\n\
       I64 Caller(I64 value){return Target((value+1)(CAST));}"
  in
  let fixed =
    decide prepared |> checked_decision |> fun result ->
    only_direct result "Caller"
    |> Semantic_function_call_conversion_decision.direct_fixed_decisions
    |> List.hd
  in
  Alcotest.(check string)
    "the named outer cast replaces its binary operand class"
    "provided:f64-result:ICF_RES_TO_INT"
    (fixed |> Semantic_function_call_conversion_decision.fixed_path
   |> Semantic_function_call_conversion_decision.fixed_path_name);
  let _, target = fixed |> provided_expression |> postfix_cast_parts in
  (match Semantic_type_reference.spelling_origin target with
  | Semantic_symbol.Source_location location ->
      Alcotest.(check bool)
        "generated named cast keeps its invocation" true
        (Option.is_some location.generated_from);
      Alcotest.(check bool)
        "generated named cast keeps its definition" true
        (Option.is_some location.defined_at)
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected generated named cast provenance");
  with_included_source
    "F64 class Box {};extern I64 Included(F64 value);I64 Caller(I64 \
     value){return Included(value(Box *));}" (fun included ->
      let fixed =
        decide included |> checked_decision |> fun result ->
        only_direct result "Caller"
        |> Semantic_function_call_conversion_decision.direct_fixed_decisions
        |> List.hd
      in
      Alcotest.(check string)
        "included aggregate pointer cast stays on the integer path"
        "provided:integer-result:ICF_RES_TO_F64"
        (fixed |> Semantic_function_call_conversion_decision.fixed_path
       |> Semantic_function_call_conversion_decision.fixed_path_name);
      let _, target = fixed |> provided_expression |> postfix_cast_parts in
      let location =
        match Semantic_type_reference.spelling_origin target with
        | Semantic_symbol.Source_location location -> location
        | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
            Alcotest.fail "expected included named cast provenance"
      in
      let source_file =
        Source_manager.find
          (Session.sources included.session)
          location.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included named cast keeps its source file" "calls.HC"
        (Source_file.path source_file |> Filename.basename))

let integer_primaries_convert_to_f64 () =
  let prepared =
    prepare ~path:"call-decision-integer-primaries.HC"
      "class Box {I64 member;};\n\
       extern I64 Target(F64 position,F64 size,F64 member_offset,F64 condition);\n\
       I64 Caller(){return \
       Target($$,sizeof(I64),offset(Box.member),defined(Box));}"
  in
  Alcotest.(check (list string))
    "integer-result primaries convert to a scalar F64 target"
    [
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:integer-result:ICF_RES_TO_F64";
    ]
    ( decide prepared |> checked_decision |> fun result ->
      only_direct result "Caller" |> path_names )

let integer_primaries_need_no_integer_conversion () =
  let prepared =
    prepare ~path:"call-decision-integer-primary-identity.HC"
      "class Box {I64 member;};\n\
       extern I64 Target(I64 position,I64 size,I64 member_offset,I64 condition);\n\
       I64 Caller(){return \
       Target($$,sizeof(I64),offset(Box.member),defined(Box));}"
  in
  Alcotest.(check (list string))
    "integer-result primaries already match an integer target"
    [
      "provided:integer-result:none";
      "provided:integer-result:none";
      "provided:integer-result:none";
      "provided:integer-result:none";
    ]
    ( decide prepared |> checked_decision |> fun result ->
      only_direct result "Caller" |> path_names )

let integer_primary_parentheses_and_modes () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-decision-integer-primary-modes.HC"
          "class Box {I64 member;};\n\
           extern I64 Target(F64 position,F64 size,F64 member_offset,F64 \
           condition);\n\
           I64 Caller(){return \
           Target((($$)),((sizeof(I64))),((offset(Box.member))),((defined(Box))));}"
      in
      Alcotest.(check (list string))
        "parentheses and compilation mode preserve primary result classes"
        [
          "provided:integer-result:ICF_RES_TO_F64";
          "provided:integer-result:ICF_RES_TO_F64";
          "provided:integer-result:ICF_RES_TO_F64";
          "provided:integer-result:ICF_RES_TO_F64";
        ]
        ( decide prepared |> checked_decision |> fun result ->
          only_direct result "Caller" |> path_names ))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let integer_primary_provenance_and_purity () =
  let prepared =
    prepare ~path:"call-decision-integer-primary-generated.HC"
      "#define QUERY defined(Target)\n\
       extern I64 Target(F64 value);\n\
       I64 Caller(){return Target(QUERY);}"
  in
  let result = decide prepared |> checked_decision in
  let fixed =
    only_direct result "Caller"
    |> Semantic_function_call_conversion_decision.direct_fixed_decisions
    |> List.hd
  in
  Alcotest.(check (list string))
    "generated defined expression keeps the integer result"
    [ "provided:integer-result:ICF_RES_TO_F64" ]
    (only_direct result "Caller" |> path_names);
  let origin =
    fixed |> provided_expression
    |> Semantic_function_call_resolution.argument_expression_origin
  in
  (match origin with
  | Semantic_symbol.Source_location location ->
      Alcotest.(check bool)
        "generated primary keeps its invocation" true
        (Option.is_some location.generated_from);
      Alcotest.(check bool)
        "generated primary keeps its definition" true
        (Option.is_some location.defined_at)
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected generated primary provenance");
  let table = Session.semantic_symbols prepared.session in
  let before = Semantic_symbol_table.all_symbols table |> List.length in
  Alcotest.(check (list string))
    "repeated primary decisions are deterministic"
    (only_direct result "Caller" |> path_names)
    ( decide prepared |> checked_decision |> fun next ->
      only_direct next "Caller" |> path_names );
  Alcotest.(check int)
    "primary decision does not mutate symbols" before
    (Semantic_symbol_table.all_symbols table |> List.length);
  with_included_source
    "extern I64 Included(F64 value);I64 Caller(){return Included(sizeof(I64));}"
    (fun included ->
      let fixed =
        decide included |> checked_decision |> fun included_result ->
        only_direct included_result "Caller"
        |> Semantic_function_call_conversion_decision.direct_fixed_decisions
        |> List.hd
      in
      Alcotest.(check string)
        "included sizeof follows the integer conversion path"
        "provided:integer-result:ICF_RES_TO_F64"
        (fixed |> Semantic_function_call_conversion_decision.fixed_path
       |> Semantic_function_call_conversion_decision.fixed_path_name);
      let location =
        match
          fixed |> provided_expression
          |> Semantic_function_call_resolution.argument_expression_origin
        with
        | Semantic_symbol.Source_location location -> location
        | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
            Alcotest.fail "expected included primary provenance"
      in
      let source_file =
        Source_manager.find
          (Session.sources included.session)
          location.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included primary keeps its source file" "calls.HC"
        (Source_file.path source_file |> Filename.basename))

let bound_scalar_identifiers_use_resolved_types () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-decision-bound-scalars.HC"
          "F64 class FloatBox {};class Plain {};\n\
           I64 global_i;F64 global_f;FloatBox global_box;Plain global_plain;\n\
           extern I64 Target(I64 a,I64 b,I64 c,I64 d,I64 e,I64 f,I64 g,I64 \
           h,I64 i,I64 j,I64 k,I64 l);\n\
           I64 Caller(I64 param_i,F64 param_f){I64 local_i;F64 local_f;static \
           I64 static_i;static F64 static_f;FloatBox box;Plain plain;return \
           Target(param_i,param_f,local_i,local_f,static_i,static_f,global_i,global_f,box,plain,global_box,global_plain);}"
      in
      let fixed =
        decide prepared |> checked_decision |> fun result ->
        only_direct result "Caller"
        |> Semantic_function_call_conversion_decision.direct_fixed_decisions
      in
      Alcotest.(check (list string))
        "parameters, locals, statics, globals, and aggregates use checked types"
        [
          "provided:integer-result:none";
          "provided:f64-result:ICF_RES_TO_INT";
          "provided:integer-result:none";
          "provided:f64-result:ICF_RES_TO_INT";
          "provided:integer-result:none";
          "provided:f64-result:ICF_RES_TO_INT";
          "provided:integer-result:none";
          "provided:f64-result:ICF_RES_TO_INT";
          "provided:f64-result:ICF_RES_TO_INT";
          "provided:integer-result:none";
          "provided:f64-result:ICF_RES_TO_INT";
          "provided:integer-result:none";
        ]
        (fixed
        |> List.map (fun decision ->
            decision |> Semantic_function_call_conversion_decision.fixed_path
            |> Semantic_function_call_conversion_decision.fixed_path_name));
      Alcotest.(check (list string))
        "scalar identifier nodes keep object value shapes"
        (List.init 12 (fun _ -> "object"))
        (fixed
        |> List.map (fun decision ->
            decision |> provided_expression |> bound_identifier
            |> Semantic_function_call_resolution.bound_identifier_shape
            |> Semantic_function_call_resolution.identifier_value_shape_name));
      Alcotest.(check (list string))
        "identifier nodes retain their stable binding categories"
        [
          "local:named-parameter";
          "local:named-parameter";
          "local:automatic-local";
          "local:automatic-local";
          "local:static-local";
          "local:static-local";
          "module:global-variable";
          "module:global-variable";
          "local:automatic-local";
          "local:automatic-local";
          "module:global-variable";
          "module:global-variable";
        ]
        (fixed
        |> List.map (fun decision ->
            decision |> provided_expression |> bound_identifier
            |> bound_resolution_name)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let bound_pointer_shapes_use_integer_results () =
  let prepared =
    prepare ~path:"call-decision-bound-shapes.HC"
      "I64 (*GlobalCallback)(I64);I64 GlobalArray[1];\n\
       extern I64 Target(F64 a,F64 b,F64 c,F64 d,F64 e,F64 f);\n\
       I64 Caller(I64 (*callback)(I64),...){I64 local_array[1];return \
       Target(callback,local_array,argc,argv,GlobalCallback,GlobalArray);}"
  in
  let fixed =
    decide prepared |> checked_decision |> fun result ->
    only_direct result "Caller"
    |> Semantic_function_call_conversion_decision.direct_fixed_decisions
  in
  Alcotest.(check (list string))
    "arrays, callbacks, argc, and argv take the integer result path"
    (List.init 6 (fun _ -> "provided:integer-result:ICF_RES_TO_F64"))
    (fixed
    |> List.map (fun decision ->
        decision |> Semantic_function_call_conversion_decision.fixed_path
        |> Semantic_function_call_conversion_decision.fixed_path_name));
  Alcotest.(check (list string))
    "declarator and synthetic shapes remain explicit"
    [
      "function-pointer";
      "array";
      "object";
      "array";
      "function-pointer";
      "array";
    ]
    (fixed
    |> List.map (fun decision ->
        decision |> provided_expression |> bound_identifier
        |> Semantic_function_call_resolution.bound_identifier_shape
        |> Semantic_function_call_resolution.identifier_value_shape_name));
  Alcotest.(check (list string))
    "synthetic and declared bindings retain their exact categories"
    [
      "local:named-parameter";
      "local:automatic-local";
      "local:variadic-argc";
      "local:variadic-argv";
      "module:global-variable";
      "module:global-variable";
    ]
    (fixed
    |> List.map (fun decision ->
        decision |> provided_expression |> bound_identifier
        |> bound_resolution_name))

let bound_identifier_provenance_and_outer_boundary () =
  let generated =
    prepare ~path:"call-decision-bound-generated.HC"
      "#define ARG value\n\
       extern I64 Target(I64 value);\n\
       I64 Caller(F64 value){return Target(ARG);}"
  in
  let table = Session.semantic_symbols generated.session in
  let symbol_count = Semantic_symbol_table.all_symbols table |> List.length in
  let first = decide generated |> checked_decision in
  let second = decide generated |> checked_decision in
  Alcotest.(check (list string))
    "replaying a bound identifier decision is stable"
    (only_direct first "Caller" |> path_names)
    (only_direct second "Caller" |> path_names);
  Alcotest.(check int)
    "replaying a bound identifier decision leaves symbols unchanged"
    symbol_count
    (Semantic_symbol_table.all_symbols table |> List.length);
  let identifier =
    only_direct first "Caller"
    |> Semantic_function_call_conversion_decision.direct_fixed_decisions
    |> List.hd |> provided_expression |> bound_identifier
  in
  let occurrence =
    Semantic_function_call_resolution.bound_identifier_occurrence identifier
  in
  (match Semantic_module_expression_binding.occurrence_origin occurrence with
  | Semantic_symbol.Source_location location ->
      Alcotest.(check bool)
        "generated identifier keeps its invocation" true
        (Option.is_some location.generated_from);
      Alcotest.(check bool)
        "generated identifier keeps its definition" true
        (Option.is_some location.defined_at)
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected generated identifier provenance");
  with_included_source
    "extern I64 Target(I64 value);I64 Caller(F64 value){return Target(value);}"
    (fun included ->
      let fixed =
        decide included |> checked_decision |> fun result ->
        only_direct result "Caller"
        |> Semantic_function_call_conversion_decision.direct_fixed_decisions
        |> List.hd
      in
      Alcotest.(check string)
        "included bound value keeps its conversion path"
        "provided:f64-result:ICF_RES_TO_INT"
        (fixed |> Semantic_function_call_conversion_decision.fixed_path
       |> Semantic_function_call_conversion_decision.fixed_path_name);
      let location =
        fixed |> provided_expression |> bound_identifier
        |> Semantic_function_call_resolution.bound_identifier_occurrence
        |> Semantic_module_expression_binding.occurrence_origin
        |> function
        | Semantic_symbol.Source_location location -> location
        | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
            Alcotest.fail "expected included identifier provenance"
      in
      let source_file =
        Source_manager.find
          (Session.sources included.session)
          location.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included bound value keeps its source file" "calls.HC"
        (Source_file.path source_file |> Filename.basename));
  let outer =
    prepare ~path:"call-decision-bound-outer.HC"
      "extern I64 Target(F64 value);I64 Caller(){return Target(OuterValue);}"
  in
  Alcotest.(check (list string))
    "an outer candidate remains unresolved"
    [ "provided:unresolved:unresolved" ]
    ( decide outer |> checked_decision |> fun result ->
      only_direct result "Caller" |> path_names )

let bound_identifier_source_order () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"call-decision-bound-source-order.HC"
          "extern I64 Target(I64 value);\n\
           extern F64 value;\n\
           I64 Before(){return Target(value);}\n\
           extern I64 first,second;\n\
           I64 Grouped(){return Target(second);}\n\
           I64 Shadowed(){F64 second;return Target(second);}\n\
           extern I64 value;\n\
           I64 After(){return Target(value);}"
      in
      let result = decide prepared |> checked_decision in
      Alcotest.(check (list string))
        "the earlier global type remains visible to the earlier caller"
        [ "provided:f64-result:ICF_RES_TO_INT" ]
        (only_direct result "Before" |> path_names);
      Alcotest.(check (list string))
        "a comma-published global keeps its checked integer type"
        [ "provided:integer-result:none" ]
        (only_direct result "Grouped" |> path_names);
      let shadowed = only_direct result "Shadowed" in
      Alcotest.(check (list string))
        "a local shadows the same-name global"
        [ "provided:f64-result:ICF_RES_TO_INT" ]
        (path_names shadowed);
      Alcotest.(check string)
        "the shadowed value keeps its local identity" "local:automatic-local"
        (shadowed
       |> Semantic_function_call_conversion_decision.direct_fixed_decisions
       |> List.hd |> provided_expression |> bound_identifier
       |> bound_resolution_name);
      Alcotest.(check (list string))
        "the later global declaration replaces the visible type"
        [ "provided:integer-result:none" ]
        (only_direct result "After" |> path_names))
    [ Preprocessor.Jit; Preprocessor.Aot ]

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
  (match Holyc_lib.type_function_call_expressions foreign ~policies with
  | Ok _ -> Alcotest.fail "expected foreign expression typing to fail"
  | Error error ->
      Alcotest.(check string)
        "foreign expression typing diagnostic" "HCSEMA0046"
        (Semantic_function_call_expression_result.error_code error));
  let expressions =
    Holyc_lib.type_function_call_expressions generated.session ~policies
    |> checked_expression_results
  in
  (match
     Holyc_lib.decide_function_call_conversions foreign ~policies ~expressions
   with
  | Ok _ -> Alcotest.fail "expected a foreign-session failure"
  | Error error ->
      Alcotest.(check string)
        "foreign session diagnostic" "HCSEMA0045"
        (Semantic_function_call_conversion_decision.error_code error));
  with_included_source
    "extern I64 Included(F64 value);I64 Caller(){return Included(1);}"
    (fun prepared ->
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
        Source_manager.find
          (Session.sources prepared.session)
          location.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included literal keeps its source file" "calls.HC"
        (Source_file.path source_file |> Filename.basename))

let included_dereference_keeps_its_source_origin () =
  with_included_source
    "extern I64 Target(F64 value);I64 Caller(I64 *pointer){return \
     Target(*pointer);}" (fun prepared ->
      let fixed =
        decide prepared |> checked_decision |> fun result ->
        only_direct result "Caller"
        |> Semantic_function_call_conversion_decision.direct_fixed_decisions
        |> List.hd
      in
      Alcotest.(check string)
        "included dereference drives the checked conversion"
        "provided:integer-result:ICF_RES_TO_F64"
        (fixed |> Semantic_function_call_conversion_decision.fixed_path
       |> Semantic_function_call_conversion_decision.fixed_path_name);
      let actual =
        match Semantic_function_call_conversion_decision.fixed_path fixed with
        | Semantic_function_call_conversion_decision.Provided_path provided ->
            Semantic_function_call_conversion_decision.provided_actual_result
              provided
        | Semantic_function_call_conversion_decision.Declared_default_path ->
            Alcotest.fail "expected a provided dereference"
      in
      let location =
        match Semantic_function_call_expression_result.result_origin actual with
        | Semantic_symbol.Source_location location -> location
        | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
            Alcotest.fail "expected included dereference provenance"
      in
      let source_file =
        Source_manager.find
          (Session.sources prepared.session)
          location.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included dereference keeps its source file" "calls.HC"
        (Source_file.path source_file |> Filename.basename))

let tests =
  [
    Alcotest.test_case "literal directions and expression retention" `Quick
      literal_directions_and_expression_retention;
    Alcotest.test_case "pointer, callback, and backed targets" `Quick
      pointer_callback_and_backed_targets;
    Alcotest.test_case "source expression classes" `Quick
      source_expression_classes_stay_explicit;
    Alcotest.test_case "prefix directions and retention" `Quick
      prefix_directions_and_retention;
    Alcotest.test_case "bitwise complement integer result" `Quick
      bitwise_complement_is_an_integer_result;
    Alcotest.test_case "prefix source order and provenance" `Quick
      prefix_source_order_modes_and_provenance;
    Alcotest.test_case "prefix constructor validation" `Quick
      prefix_constructor_validation;
    Alcotest.test_case "prefix update directions and provenance" `Quick
      prefix_update_directions_and_provenance;
    Alcotest.test_case "postfix directions and retention" `Quick
      postfix_directions_and_retention;
    Alcotest.test_case "postfix provenance" `Quick postfix_provenance;
    Alcotest.test_case "postfix constructor validation" `Quick
      postfix_constructor_validation;
    Alcotest.test_case "binary operator inventory and assignments" `Quick
      binary_operator_inventory_and_assignments;
    Alcotest.test_case "binary result joins and nesting" `Quick
      binary_result_joins_and_nesting;
    Alcotest.test_case "binary source order in both modes" `Quick
      binary_source_order_in_both_modes;
    Alcotest.test_case "binary provenance and constructor validation" `Quick
      binary_provenance_and_constructor_validation;
    Alcotest.test_case "primitive postfix cast directions" `Quick
      primitive_postfix_cast_directions_and_retention;
    Alcotest.test_case "outer postfix cast and modes" `Quick
      outer_postfix_cast_wins_in_both_modes;
    Alcotest.test_case "postfix cast provenance and purity" `Quick
      postfix_cast_provenance_and_purity;
    Alcotest.test_case "postfix cast target validation" `Quick
      postfix_cast_target_validation;
    Alcotest.test_case "named postfix cast directions" `Quick
      named_postfix_cast_directions_and_backings;
    Alcotest.test_case "named postfix cast source order" `Quick
      named_postfix_cast_source_order;
    Alcotest.test_case "named postfix cast provenance" `Quick
      named_postfix_cast_provenance_and_outer_class;
    Alcotest.test_case "integer primaries to F64" `Quick
      integer_primaries_convert_to_f64;
    Alcotest.test_case "integer primaries to integer" `Quick
      integer_primaries_need_no_integer_conversion;
    Alcotest.test_case "integer primary parentheses and modes" `Quick
      integer_primary_parentheses_and_modes;
    Alcotest.test_case "integer primary provenance and purity" `Quick
      integer_primary_provenance_and_purity;
    Alcotest.test_case "bound scalar identifier types" `Quick
      bound_scalar_identifiers_use_resolved_types;
    Alcotest.test_case "bound identifier pointer shapes" `Quick
      bound_pointer_shapes_use_integer_results;
    Alcotest.test_case "bound identifier provenance and outer boundary" `Quick
      bound_identifier_provenance_and_outer_boundary;
    Alcotest.test_case "bound identifier source order" `Quick
      bound_identifier_source_order;
    Alcotest.test_case "default and variadic paths" `Quick
      defaults_and_variadics_remain_separate;
    Alcotest.test_case "source-visible replacement headers" `Quick
      source_visible_headers_choose_literal_flags;
    Alcotest.test_case "deferred, provenance, foreign, and purity" `Quick
      deferred_provenance_foreign_and_purity;
    Alcotest.test_case "included dereference provenance" `Quick
      included_dereference_keeps_its_source_origin;
  ]
