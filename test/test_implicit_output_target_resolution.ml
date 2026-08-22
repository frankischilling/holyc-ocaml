open Holyc_lib

let checked = Test_function_call_conversion_policy.checked
let prepare = Test_function_call_conversion_policy.prepare

type prepared = Test_function_call_conversion_policy.prepared

let checked_target = function
  | Ok value -> value
  | Error error ->
      error |> Semantic_implicit_output_target_resolution.error_to_string
      |> Alcotest.fail

let semantic_kind = function
  | Semantic_outer_environment.Aggregate -> Semantic_symbol.Aggregate_type
  | Semantic_outer_environment.Function -> Semantic_symbol.Function
  | Semantic_outer_environment.Global_variable ->
      Semantic_symbol.Global_variable
  | Semantic_outer_environment.Export_system_symbol ->
      Semantic_symbol.Assembler_symbol

let checked_environment = function
  | Ok value -> value
  | Error error ->
      error |> Semantic_outer_environment.error_to_string |> Alcotest.fail

let add_outer_symbol (prepared : prepared) name record_kind =
  let table = Session.semantic_symbols prepared.session in
  checked
    (Semantic_symbol_table.add table
       ~scope:(Semantic_symbol_table.root table)
       ~name
       ~kind:(semantic_kind record_kind)
       ~origin:(Semantic_symbol.Synthesized ("outer output fixture " ^ name)))

let make_outer_table (prepared : prepared) ~table_kind ~table_index records =
  let entries =
    records
    |> List.mapi (fun entry_index (name, record_kind) ->
        Semantic_outer_environment.make_entry
          ~symbol:(add_outer_symbol prepared name record_kind)
          ~record_kind ~entry_index
        |> checked_environment)
  in
  Semantic_outer_environment.make_table ~table_kind ~table_index entries
  |> checked_environment

let environment (prepared : prepared) mode records =
  let tables =
    match mode with
    | Preprocessor.Jit ->
        [
          make_outer_table prepared
            ~table_kind:(Semantic_outer_environment.Jit_task 0) ~table_index:0
            records;
          make_outer_table prepared
            ~table_kind:Semantic_outer_environment.Assembler ~table_index:1 [];
        ]
    | Preprocessor.Aot ->
        [
          make_outer_table prepared
            ~table_kind:(Semantic_outer_environment.Aot_parent 0) ~table_index:0
            records;
          make_outer_table prepared
            ~table_kind:Semantic_outer_environment.Assembler ~table_index:1 [];
        ]
  in
  Holyc_lib.create_outer_environment prepared.session ~compilation_mode:mode
    tables
  |> checked

let expression_results (prepared : prepared) =
  let policies =
    Test_function_call_conversion_policy.analyze prepared
    |> Test_function_call_conversion_policy.checked_policy
  in
  Holyc_lib.type_function_call_expressions prepared.session
    ~members:prepared.members ~policies
  |> function
  | Ok value -> value
  | Error error ->
      error |> Semantic_function_call_expression_result.error_to_string
      |> Alcotest.fail

let resolve (prepared : prepared) environment =
  Holyc_lib.resolve_implicit_output_targets prepared.session ~environment
    ~module_expressions:prepared.module_expressions
    ~function_types:prepared.function_types ~functions:prepared.functions
    ~expressions:(expression_results prepared)

let function_named result name =
  result |> Semantic_implicit_output_target_resolution.functions
  |> List.find (fun function_ ->
      function_ |> Semantic_implicit_output_target_resolution.function_source
      |> Semantic_function_call_expression_result.function_symbol
      |> Semantic_symbol.name |> String.equal name)

let outputs_named result name =
  function_named result name
  |> Semantic_implicit_output_target_resolution.function_outputs

let module_target = function
  | Semantic_implicit_output_target_resolution.Module_function target -> target
  | Semantic_implicit_output_target_resolution.Outer_function _ ->
      Alcotest.fail "expected a module output target"

let outer_target = function
  | Semantic_implicit_output_target_resolution.Outer_function target -> target
  | Semantic_implicit_output_target_resolution.Module_function _ ->
      Alcotest.fail "expected an outer output target"

let module_headers_follow_source_order () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"implicit-output-module-targets.HC"
          "extern U0 Print(I64 fmt,...);\n\
           I64 Before(){\"%d\",1;return 0;}\n\
           extern U0 Print(F64 fmt,...);\n\
           I64 Print;\n\
           extern U0 PutChars(U64 ch);\n\
           I64 After(){\"%d\",2;'A';return 0;}"
      in
      let environment = environment prepared mode [] in
      let result = resolve prepared environment |> checked_target in
      let before = outputs_named result "Before" in
      let after = outputs_named result "After" in
      Alcotest.(check (list string))
        "before target" [ "Print" ]
        (List.map Semantic_implicit_output_target_resolution.output_target_name
           before);
      Alcotest.(check (list string))
        "after targets" [ "Print"; "PutChars" ]
        (List.map Semantic_implicit_output_target_resolution.output_target_name
           after);
      let first_header =
        List.hd before
        |> Semantic_implicit_output_target_resolution.output_binding
        |> module_target
        |> Semantic_implicit_output_target_resolution.module_header
      in
      let second_header =
        List.hd after
        |> Semantic_implicit_output_target_resolution.output_binding
        |> module_target
        |> Semantic_implicit_output_target_resolution.module_header
      in
      let parameter_type header =
        header |> Semantic_function_type_resolution.function_signature
        |> Semantic_function_type_resolution.signature_parameters |> List.hd
        |> Semantic_function_type_resolution.parameter_type_reference
        |> Semantic_type_reference.resolved_type
      in
      let primitive_name type_ =
        match Semantic_type.base type_ with
        | Semantic_type.Primitive (_, primitive) ->
            Primitive_type.to_string primitive
        | Semantic_type.Aggregate symbol -> Semantic_symbol.name symbol
      in
      Alcotest.(check string)
        "earlier header type" "I64"
        (parameter_type first_header |> primitive_name);
      Alcotest.(check string)
        "later header type" "F64"
        (parameter_type second_header |> primitive_name);
      let first_target =
        List.hd before
        |> Semantic_implicit_output_target_resolution.output_binding
        |> module_target
        |> Semantic_implicit_output_target_resolution.module_target_symbol
      in
      let second_target =
        List.hd after
        |> Semantic_implicit_output_target_resolution.output_binding
        |> module_target
        |> Semantic_implicit_output_target_resolution.module_target_symbol
      in
      Alcotest.(check int)
        "joined Print identity"
        (Semantic_symbol.id first_target |> Semantic_symbol.Id.to_int)
        (Semantic_symbol.id second_target |> Semantic_symbol.Id.to_int))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let outer_lookup_filters_record_kinds () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"implicit-output-outer-targets.HC"
          "I64 Caller(){\"value=%d\",1;'Z';return 0;}"
      in
      let environment =
        environment prepared mode
          [
            ("Print", Semantic_outer_environment.Function);
            ("Print", Semantic_outer_environment.Global_variable);
            ("PutChars", Semantic_outer_environment.Function);
          ]
      in
      let result = resolve prepared environment |> checked_target in
      let outputs = outputs_named result "Caller" in
      Alcotest.(check (list string))
        "outer binding kinds"
        [
          (match mode with
          | Preprocessor.Jit -> "jit-task-0"
          | Preprocessor.Aot -> "aot-parent-0");
          (match mode with
          | Preprocessor.Jit -> "jit-task-0"
          | Preprocessor.Aot -> "aot-parent-0");
        ]
        (List.map
           (fun output ->
             output |> Semantic_implicit_output_target_resolution.output_binding
             |> outer_target |> Semantic_outer_environment.binding_table
             |> Semantic_outer_environment.table_kind
             |> Semantic_outer_environment.table_kind_name)
           outputs);
      let print_symbol =
        List.hd outputs
        |> Semantic_implicit_output_target_resolution.output_binding
        |> outer_target |> Semantic_outer_environment.binding_entry
        |> Semantic_outer_environment.entry_symbol
      in
      Alcotest.(check string)
        "type-filtered Print symbol" "function"
        (Semantic_symbol.kind print_symbol |> Semantic_symbol.kind_name))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let missing_headers_report_marker_origin () =
  let prepared =
    prepare ~path:"implicit-output-missing-target.HC"
      "I64 Caller(){\"missing\";return 0;}"
  in
  let environment =
    environment prepared Preprocessor.Jit
      [ ("Print", Semantic_outer_environment.Global_variable) ]
  in
  match resolve prepared environment with
  | Ok _ -> Alcotest.fail "expected a missing Print header"
  | Error error ->
      Alcotest.(check string)
        "missing-header code" "HCSEMA0048"
        (Semantic_implicit_output_target_resolution.error_code error);
      Alcotest.(check string)
        "missing-header message"
        "implicit output requires a visible Print function header"
        (Semantic_implicit_output_target_resolution.error_message error);
      Alcotest.(check bool)
        "marker origin retained" true
        (Option.is_some
           (Semantic_implicit_output_target_resolution.error_origin error))

let replay_and_ownership_are_checked () =
  let prepared =
    prepare ~path:"implicit-output-target-replay.HC"
      "I64 Caller(){\"value=%d\",1;return 0;}"
  in
  let environment =
    environment prepared Preprocessor.Jit
      [ ("Print", Semantic_outer_environment.Function) ]
  in
  let table = Session.semantic_symbols prepared.session in
  let before = Semantic_symbol_table.all_symbols table |> List.length in
  let first = resolve prepared environment |> checked_target in
  let second = resolve prepared environment |> checked_target in
  Alcotest.(check (list string))
    "deterministic target replay"
    (outputs_named first "Caller"
    |> List.map Semantic_implicit_output_target_resolution.output_target_name)
    (outputs_named second "Caller"
    |> List.map Semantic_implicit_output_target_resolution.output_target_name);
  Alcotest.(check int)
    "target resolution is immutable" before
    (Semantic_symbol_table.all_symbols table |> List.length);
  let foreign = Session.create () in
  match
    Holyc_lib.resolve_implicit_output_targets foreign ~environment
      ~module_expressions:prepared.module_expressions
      ~function_types:prepared.function_types ~functions:prepared.functions
      ~expressions:(expression_results prepared)
  with
  | Ok _ -> Alcotest.fail "expected foreign-session target resolution to fail"
  | Error error ->
      Alcotest.(check string)
        "ownership diagnostic" "HCSEMA0047"
        (Semantic_implicit_output_target_resolution.error_code error)

let tests =
  [
    Alcotest.test_case "module headers follow source order" `Quick
      module_headers_follow_source_order;
    Alcotest.test_case "outer lookup filters record kinds" `Quick
      outer_lookup_filters_record_kinds;
    Alcotest.test_case "missing headers retain marker origins" `Quick
      missing_headers_report_marker_origin;
    Alcotest.test_case "target replay and ownership" `Quick
      replay_and_ownership_are_checked;
  ]
