open Holyc_lib

let checked_decision = function
  | Ok value -> value
  | Error error ->
      error |> Semantic_function_call_conversion_decision.error_to_string
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

let postfix_cast_parts expression =
  match
    Semantic_function_call_resolution.argument_expression_kind expression
  with
  | Semantic_function_call_resolution.Postfix_cast_expression (operand, target)
    -> (operand, target)
  | _ -> Alcotest.fail "expected a retained postfix cast target"

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
      "provided:unresolved:unresolved";
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:unresolved:unresolved";
      "provided:unresolved:unresolved";
      "provided:integer-result:ICF_RES_TO_F64";
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

let tests =
  [
    Alcotest.test_case "literal directions and expression retention" `Quick
      literal_directions_and_expression_retention;
    Alcotest.test_case "pointer, callback, and backed targets" `Quick
      pointer_callback_and_backed_targets;
    Alcotest.test_case "source expression classes" `Quick
      source_expression_classes_stay_explicit;
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
    Alcotest.test_case "default and variadic paths" `Quick
      defaults_and_variadics_remain_separate;
    Alcotest.test_case "source-visible replacement headers" `Quick
      source_visible_headers_choose_literal_flags;
    Alcotest.test_case "deferred, provenance, foreign, and purity" `Quick
      deferred_provenance_foreign_and_purity;
  ]
