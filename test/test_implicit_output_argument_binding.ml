open Holyc_lib

let checked = Test_function_call_conversion_policy.checked
let prepare = Test_function_call_conversion_policy.prepare

type prepared = Test_function_call_conversion_policy.prepared

let checked_policy = Test_function_call_conversion_policy.checked_policy

let checked_expression_results = function
  | Ok value -> value
  | Error error ->
      error |> Semantic_function_call_expression_result.error_to_string
      |> Alcotest.fail

let checked_targets = function
  | Ok value -> value
  | Error error ->
      error |> Semantic_implicit_output_target_resolution.error_to_string
      |> Alcotest.fail

let checked_bindings = function
  | Ok value -> value
  | Error error ->
      error |> Semantic_implicit_output_argument_binding.error_to_string
      |> Alcotest.fail

let checked_environment = function
  | Ok value -> value
  | Error error ->
      error |> Semantic_outer_environment.error_to_string |> Alcotest.fail

let semantic_kind = function
  | Semantic_outer_environment.Aggregate -> Semantic_symbol.Aggregate_type
  | Semantic_outer_environment.Function -> Semantic_symbol.Function
  | Semantic_outer_environment.Global_variable ->
      Semantic_symbol.Global_variable
  | Semantic_outer_environment.Export_system_symbol ->
      Semantic_symbol.Assembler_symbol

let add_outer_symbol (prepared : prepared) name record_kind =
  let table = Session.semantic_symbols prepared.session in
  checked
    (Semantic_symbol_table.add table
       ~scope:(Semantic_symbol_table.root table)
       ~name
       ~kind:(semantic_kind record_kind)
       ~origin:(Semantic_symbol.Synthesized ("outer output fixture " ^ name)))

let make_table ~table_kind ~table_index entries =
  Semantic_outer_environment.make_table ~table_kind ~table_index entries
  |> checked_environment

let environment_from_entries (prepared : prepared) mode entries =
  let first_kind =
    match mode with
    | Preprocessor.Jit -> Semantic_outer_environment.Jit_task 0
    | Preprocessor.Aot -> Semantic_outer_environment.Aot_parent 0
  in
  let tables =
    [
      make_table ~table_kind:first_kind ~table_index:0 entries;
      make_table ~table_kind:Semantic_outer_environment.Assembler ~table_index:1
        [];
    ]
  in
  Holyc_lib.create_outer_environment prepared.session ~compilation_mode:mode
    tables
  |> checked

let environment (prepared : prepared) mode records =
  let entries =
    records
    |> List.mapi (fun entry_index (name, record_kind) ->
        Semantic_outer_environment.make_entry
          ~symbol:(add_outer_symbol prepared name record_kind)
          ~record_kind ~entry_index
        |> checked_environment)
  in
  environment_from_entries prepared mode entries

type prepared_binding = {
  prepared : prepared;
  policies : Semantic_function_call_conversion_policy.t;
  expressions : Semantic_function_call_expression_result.t;
  targets : Semantic_implicit_output_target_resolution.t;
}

let semantic_inputs prepared environment =
  let policies =
    Test_function_call_conversion_policy.analyze prepared |> checked_policy
  in
  let expressions =
    Holyc_lib.type_function_call_expressions prepared.session
      ~members:prepared.members ~policies
    |> checked_expression_results
  in
  let targets =
    Holyc_lib.resolve_implicit_output_targets prepared.session ~environment
      ~module_expressions:prepared.module_expressions
      ~function_types:prepared.function_types ~functions:prepared.functions
      ~expressions
    |> checked_targets
  in
  { prepared; policies; expressions; targets }

let bind ?outer_headers inputs =
  Holyc_lib.bind_implicit_output_arguments inputs.prepared.session
    ~policies:inputs.policies ?outer_headers inputs.targets

let function_named result name =
  result |> Semantic_implicit_output_argument_binding.functions
  |> List.find (fun function_ ->
      function_ |> Semantic_implicit_output_argument_binding.function_source
      |> Semantic_implicit_output_target_resolution.function_source
      |> Semantic_function_call_expression_result.function_symbol
      |> Semantic_symbol.name |> String.equal name)

let outputs_named result name =
  function_named result name
  |> Semantic_implicit_output_argument_binding.function_outputs

let bound = function
  | Semantic_implicit_output_argument_binding.Bound_output output -> output
  | Semantic_implicit_output_argument_binding.Deferred_outer_output _ ->
      Alcotest.fail "expected a bound implicit output"

let deferred = function
  | Semantic_implicit_output_argument_binding.Deferred_outer_output output ->
      output
  | Semantic_implicit_output_argument_binding.Bound_output _ ->
      Alcotest.fail "expected a deferred outer implicit output"

let fixed_path_names output =
  output |> Semantic_implicit_output_argument_binding.bound_fixed_slots
  |> List.map (fun slot ->
      slot |> Semantic_implicit_output_argument_binding.fixed_path
      |> Semantic_implicit_output_argument_binding.fixed_path_name)

let ordinary_print_and_putchars () =
  let prepared =
    prepare ~path:"implicit-output-standard-bindings.HC"
      "extern U0 Print(U8 *fmt,...);\n\
       extern U0 PutChars(U64 ch);\n\
       I64 Caller(){\"value=%d\",1;'A';return 0;}"
  in
  let inputs =
    semantic_inputs prepared (environment prepared Preprocessor.Jit [])
  in
  let outputs =
    bind inputs |> checked_bindings |> fun result ->
    outputs_named result "Caller"
  in
  match outputs with
  | [ print; put_chars ] ->
      let print = bound print in
      let put_chars = bound put_chars in
      Alcotest.(check (list string))
        "Print fixed slot"
        [ "provided:integer-result:none" ]
        (fixed_path_names print);
      Alcotest.(check int)
        "Print variadic tail" 1
        (print
       |> Semantic_implicit_output_argument_binding.bound_variadic_values
       |> List.length);
      Alcotest.(check (list string))
        "PutChars fixed slot"
        [ "provided:integer-result:none" ]
        (fixed_path_names put_chars);
      Alcotest.(check int)
        "PutChars has no tail" 0
        (put_chars
       |> Semantic_implicit_output_argument_binding.bound_variadic_values
       |> List.length)
  | outputs ->
      Alcotest.failf "expected Print and PutChars outputs, got %d"
        (List.length outputs)

let selected_header_drives_conversions () =
  let prepared =
    prepare ~path:"implicit-output-selected-header.HC"
      "#define FIRST 1\n\
       F64 class FloatBox {};\n\
       extern U0 Print(FloatBox first,I64 second,F64 third,...);\n\
       I64 Caller(){\"\" FIRST,2.5,3,4;return 0;}\n\
       extern U0 Print(U8 *ptr);\n\
       I64 PointerCaller(U8 *ptr){\"\" ptr;return 0;}\n\
       extern U0 Print(I64 (*callback)(I64));\n\
       I64 CallbackCaller(I64 (*callback)(I64)){\"\" callback;return 0;}"
  in
  let inputs =
    semantic_inputs prepared (environment prepared Preprocessor.Jit [])
  in
  let output =
    bind inputs |> checked_bindings |> fun result ->
    outputs_named result "Caller" |> List.hd |> bound
  in
  Alcotest.(check (list string))
    "fixed conversions follow the replacement header"
    [
      "provided:integer-result:ICF_RES_TO_F64";
      "provided:f64-result:ICF_RES_TO_INT";
      "provided:integer-result:ICF_RES_TO_F64";
    ]
    (fixed_path_names output);
  let tail =
    Semantic_implicit_output_argument_binding.bound_variadic_values output
  in
  Alcotest.(check int) "only the fourth value is variadic" 1 (List.length tail);
  Alcotest.(check string)
    "tail class" "integer-result"
    (tail |> List.hd |> Semantic_function_call_expression_result.result_class
   |> Semantic_function_call_expression_result.result_class_name);
  let first_slot =
    output |> Semantic_implicit_output_argument_binding.bound_fixed_slots
    |> List.hd
  in
  (match Semantic_implicit_output_argument_binding.fixed_path first_slot with
  | Semantic_implicit_output_argument_binding.Defaulted_path _ ->
      Alcotest.fail "expected a definition-backed provided value"
  | Semantic_implicit_output_argument_binding.Provided_path provided -> (
      match
        provided |> Semantic_implicit_output_argument_binding.provided_result
        |> Semantic_function_call_expression_result.result_origin
      with
      | Semantic_symbol.Source_location { defined_at = Some _; _ } -> ()
      | _ -> Alcotest.fail "expected definition provenance on FIRST"));
  List.iter
    (fun name ->
      let output =
        bind inputs |> checked_bindings |> fun result ->
        outputs_named result name |> List.hd |> bound
      in
      Alcotest.(check (list string))
        (name ^ " integer callback or pointer path")
        [ "provided:integer-result:none" ]
        (fixed_path_names output))
    [ "PointerCaller"; "CallbackCaller" ]

let defaults_keep_omissions_and_mode_materialization () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"implicit-output-default-bindings.HC"
          "extern U0 Print(U8 *fmt,U8 *suffix=\"done\",I64 count=3);\n\
           I64 Caller(){\"value\";return 0;}"
      in
      let inputs = semantic_inputs prepared (environment prepared mode []) in
      let output =
        bind inputs |> checked_bindings |> fun result ->
        outputs_named result "Caller" |> List.hd |> bound
      in
      let slots =
        Semantic_implicit_output_argument_binding.bound_fixed_slots output
      in
      let defaults =
        slots
        |> List.filter_map (fun slot ->
            match Semantic_implicit_output_argument_binding.fixed_path slot with
            | Semantic_implicit_output_argument_binding.Provided_path _ -> None
            | Semantic_implicit_output_argument_binding.Defaulted_path binding
              -> Some binding)
      in
      Alcotest.(check (list int))
        "default omissions retain their fixed positions" [ 1; 2 ]
        (List.map
           (fun binding ->
             binding
             |> Semantic_implicit_output_argument_binding.default_omission
             |> Semantic_implicit_output_argument_binding.omission_position)
           defaults);
      Alcotest.(check (list string))
        "default materialization follows compilation mode"
        (match mode with
        | Preprocessor.Jit -> [ "immediate"; "immediate" ]
        | Preprocessor.Aot -> [ "aot-string-constant"; "immediate" ])
        (List.map
           (fun binding ->
             binding
             |> Semantic_implicit_output_argument_binding
                .default_materialization
             |> Semantic_implicit_output_argument_binding
                .default_materialization_name)
           defaults))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let missing_required_parameter () =
  let prepared =
    prepare ~path:"implicit-output-missing-fixed.HC"
      "extern U0 Print(U8 *fmt,I64 value);\nI64 Caller(){\"value\";return 0;}"
  in
  let inputs =
    semantic_inputs prepared (environment prepared Preprocessor.Jit [])
  in
  match bind inputs with
  | Ok _ -> Alcotest.fail "expected a missing fixed parameter"
  | Error error ->
      Alcotest.(check string)
        "missing code" "HCSEMA0050"
        (Semantic_implicit_output_argument_binding.error_code error);
      Alcotest.(check string)
        "missing message"
        "implicit output call to \"Print\" is missing required argument 2 \
         (value)"
        (Semantic_implicit_output_argument_binding.error_message error);
      Alcotest.(check bool)
        "missing origin" true
        (Option.is_some
           (Semantic_implicit_output_argument_binding.error_origin error))

let nonvariadic_extra_argument () =
  let prepared =
    prepare ~path:"implicit-output-extra-fixed.HC"
      "extern U0 Print(U8 *fmt);\nI64 Caller(){\"value\",1;return 0;}"
  in
  let inputs =
    semantic_inputs prepared (environment prepared Preprocessor.Jit [])
  in
  match bind inputs with
  | Ok _ -> Alcotest.fail "expected a nonvariadic extra argument"
  | Error error ->
      Alcotest.(check string)
        "extra code" "HCSEMA0051"
        (Semantic_implicit_output_argument_binding.error_code error);
      Alcotest.(check string)
        "extra message"
        "implicit output call to \"Print\" provides argument 2, but its \
         selected header has 1 fixed parameter"
        (Semantic_implicit_output_argument_binding.error_message error)

let untyped_outer_target_is_deferred () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"implicit-output-untyped-outer.HC"
          "I64 Caller(){\"value\";return 0;}"
      in
      let inputs =
        semantic_inputs prepared
          (environment prepared mode
             [ ("Print", Semantic_outer_environment.Function) ])
      in
      let output =
        bind inputs |> checked_bindings |> fun result ->
        outputs_named result "Caller" |> List.hd
      in
      let deferred = deferred output in
      Alcotest.(check string)
        "deferred result is explicit" "deferred-outer-header"
        (Semantic_implicit_output_argument_binding.output_result_name output);
      Alcotest.(check string)
        "exact outer symbol retained" "Print"
        (deferred
       |> Semantic_implicit_output_argument_binding.deferred_outer_binding
       |> Semantic_outer_environment.binding_entry
       |> Semantic_outer_environment.entry_symbol |> Semantic_symbol.name))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let checked_outer_header_enables_binding () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"implicit-output-typed-outer.HC"
          "I64 Caller(){\"\" 1;return 0;}\nextern U0 Print(F64 fmt);"
      in
      let header =
        prepared.function_types |> Semantic_function_type_resolution.functions
        |> List.find (fun header ->
            header |> Semantic_function_type_resolution.function_symbol
            |> Semantic_symbol.name |> String.equal "Print")
      in
      let entry =
        Semantic_outer_environment.make_entry
          ~symbol:(Semantic_function_type_resolution.function_symbol header)
          ~record_kind:Semantic_outer_environment.Function ~entry_index:0
        |> checked_environment
      in
      let inputs =
        semantic_inputs prepared
          (environment_from_entries prepared mode [ entry ])
      in
      let output =
        bind ~outer_headers:[ header ] inputs |> checked_bindings
        |> fun result -> outputs_named result "Caller" |> List.hd |> bound
      in
      Alcotest.(check (list string))
        "checked outer signature selects F64 conversion"
        [ "provided:integer-result:ICF_RES_TO_F64" ]
        (fixed_path_names output))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let replay_purity_and_foreign_batches () =
  let prepared =
    prepare ~path:"implicit-output-binding-replay.HC"
      "extern U0 Print(U8 *fmt,...);\nI64 Caller(){\"%d\",1;return 0;}"
  in
  let inputs =
    semantic_inputs prepared (environment prepared Preprocessor.Jit [])
  in
  let table = Session.semantic_symbols prepared.session in
  let before = Semantic_symbol_table.all_symbols table |> List.length in
  let first = bind inputs |> checked_bindings in
  let second = bind inputs |> checked_bindings in
  let names result =
    outputs_named result "Caller"
    |> List.map Semantic_implicit_output_argument_binding.output_result_name
  in
  Alcotest.(check (list string))
    "deterministic replay" (names first) (names second);
  Alcotest.(check int)
    "binding is immutable" before
    (Semantic_symbol_table.all_symbols table |> List.length);
  let foreign = Session.create () in
  match
    Holyc_lib.bind_implicit_output_arguments foreign ~policies:inputs.policies
      inputs.targets
  with
  | Ok _ -> Alcotest.fail "expected a foreign-session binding failure"
  | Error error ->
      Alcotest.(check string)
        "foreign batch code" "HCSEMA0049"
        (Semantic_implicit_output_argument_binding.error_code error)

let included_provenance_survives () =
  Test_function_call_expression_result.with_included_source
    "extern U0 Print(F64 fmt);I64 Caller(){\"\" 1;return 0;}" (fun prepared ->
      let inputs =
        semantic_inputs prepared (environment prepared Preprocessor.Jit [])
      in
      let output =
        bind inputs |> checked_bindings |> fun result ->
        outputs_named result "Caller" |> List.hd |> bound
      in
      let slot =
        output |> Semantic_implicit_output_argument_binding.bound_fixed_slots
        |> List.hd
      in
      match Semantic_implicit_output_argument_binding.fixed_path slot with
      | Semantic_implicit_output_argument_binding.Defaulted_path _ ->
          Alcotest.fail "expected a provided included value"
      | Semantic_implicit_output_argument_binding.Provided_path provided -> (
          match
            provided
            |> Semantic_implicit_output_argument_binding.provided_result
            |> Semantic_function_call_expression_result.result_origin
          with
          | Semantic_symbol.Source_location location ->
              let source =
                Source_manager.find
                  (Session.sources prepared.session)
                  location.span.source
                |> Option.get
              in
              Alcotest.(check string)
                "included output value keeps its source file" "calls.HC"
                (Source_file.path source |> Filename.basename)
          | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
              Alcotest.fail "expected included output provenance"))

let tests =
  [
    Alcotest.test_case "ordinary Print and PutChars" `Quick
      ordinary_print_and_putchars;
    Alcotest.test_case "selected header conversions" `Quick
      selected_header_drives_conversions;
    Alcotest.test_case "default omissions and modes" `Quick
      defaults_keep_omissions_and_mode_materialization;
    Alcotest.test_case "missing required parameter" `Quick
      missing_required_parameter;
    Alcotest.test_case "nonvariadic extra argument" `Quick
      nonvariadic_extra_argument;
    Alcotest.test_case "untyped outer target deferral" `Quick
      untyped_outer_target_is_deferred;
    Alcotest.test_case "checked outer header binding" `Quick
      checked_outer_header_enables_binding;
    Alcotest.test_case "replay, purity, and ownership" `Quick
      replay_purity_and_foreign_batches;
    Alcotest.test_case "included provenance" `Quick included_provenance_survives;
  ]
