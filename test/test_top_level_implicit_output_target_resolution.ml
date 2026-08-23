open Holyc_lib
module Target = Semantic_top_level_implicit_output_target_resolution

let checked = Test_function_call_conversion_policy.checked
let prepare = Test_function_call_conversion_policy.prepare

type prepared = Test_function_call_conversion_policy.prepared

let checked_outer = function
  | Ok value -> value
  | Error error ->
      error |> Semantic_outer_environment.error_to_string |> Alcotest.fail

let checked_target = function
  | Ok value -> value
  | Error error -> error |> Target.error_to_string |> Alcotest.fail

let semantic_kind = function
  | Semantic_outer_environment.Aggregate -> Semantic_symbol.Aggregate_type
  | Semantic_outer_environment.Function -> Semantic_symbol.Function
  | Semantic_outer_environment.Global_variable ->
      Semantic_symbol.Global_variable
  | Semantic_outer_environment.Export_system_symbol ->
      Semantic_symbol.Assembler_symbol

let make_entries (source : prepared) description records =
  let table = Session.semantic_symbols source.session in
  records
  |> List.mapi (fun entry_index (name, record_kind) ->
      let symbol =
        Semantic_symbol_table.add table
          ~scope:(Semantic_symbol_table.root table)
          ~name
          ~kind:(semantic_kind record_kind)
          ~origin:(Semantic_symbol.Synthesized (description ^ " " ^ name))
        |> checked
      in
      Semantic_outer_environment.make_entry ~symbol ~record_kind ~entry_index
      |> checked_outer)

let environment (source : prepared) ~outer ~assembler =
  let data_kind =
    match source.mode with
    | Preprocessor.Jit -> Semantic_outer_environment.Jit_task 0
    | Preprocessor.Aot -> Semantic_outer_environment.Aot_parent 0
  in
  let data =
    Semantic_outer_environment.make_table ~table_kind:data_kind ~table_index:0
      (make_entries source "outer output" outer)
    |> checked_outer
  in
  let assembler =
    Semantic_outer_environment.make_table
      ~table_kind:Semantic_outer_environment.Assembler ~table_index:1
      (make_entries source "assembler output" assembler)
    |> checked_outer
  in
  Holyc_lib.create_outer_environment source.session
    ~compilation_mode:source.mode [ data; assembler ]
  |> checked

let expression_results ?environment (source : prepared) =
  let _, _, _, result =
    Test_top_level_expression_result.analyze ?environment source
  in
  result

let resolve (source : prepared) expressions =
  Holyc_lib.resolve_top_level_implicit_output_targets source.session
    ~function_types:source.function_types ~functions:source.functions
    expressions

let module_target = function
  | Target.Module_function target -> target
  | Target.Outer_function _ -> Alcotest.fail "expected a module output target"

let outer_target = function
  | Target.Outer_function target -> target
  | Target.Module_function _ -> Alcotest.fail "expected an outer output target"

let primitive_name type_ =
  match Semantic_type.base type_ with
  | Semantic_type.Primitive (_, primitive) -> Primitive_type.to_string primitive
  | Semantic_type.Aggregate symbol -> Semantic_symbol.name symbol

let header_parameter_name header =
  header |> Semantic_function_type_resolution.function_signature
  |> Semantic_function_type_resolution.signature_parameters |> List.hd
  |> Semantic_function_type_resolution.parameter_type_reference
  |> Semantic_type_reference.resolved_type |> primitive_name

let output_item_index output =
  output |> Target.output_statement
  |> Semantic_function_call_expression_result.top_level_statement_source
  |> Semantic_top_level_expression_tree.statement_source
  |> Semantic_top_level_outer_expression_binding.statement_item_index

let module_headers_follow_top_level_source_order () =
  List.iter
    (fun mode ->
      let source =
        prepare ~mode ~path:"top-level-output-module-targets.HC"
          "extern U0 Print(I64 fmt,...);\n\
           \"first=%d\",1;I64 Print;\"second=%d\",2;\n\
           extern U0 Print(F64 fmt,...);\"third=%d\",3;\n\
           extern U0 PutChars(U64 ch);'A';"
      in
      let expressions = expression_results source in
      let result = resolve source expressions |> checked_target in
      let outputs = Target.outputs result in
      Alcotest.(check (list string))
        "top-level targets retain source order"
        [ "Print"; "Print"; "Print"; "PutChars" ]
        (List.map Target.output_target_name outputs);
      Alcotest.(check (list int))
        "top-level targets use their containing item positions" [ 1; 3; 5; 7 ]
        (List.map output_item_index outputs);
      let headers =
        outputs
        |> List.map (fun output ->
            output |> Target.output_binding |> module_target
            |> Target.module_header |> header_parameter_name)
      in
      Alcotest.(check (list string))
        "same-name objects do not hide function headers"
        [ "I64"; "I64"; "F64"; "U64" ]
        headers;
      let targets =
        outputs
        |> List.map (fun output ->
            output |> Target.output_binding |> module_target
            |> Target.module_target_symbol)
      in
      Alcotest.(check bool)
        "replacement Print headers retain one canonical identity" true
        (let first = List.nth targets 0 in
         let second = List.nth targets 1 in
         let third = List.nth targets 2 in
         Semantic_symbol.Id.equal (Semantic_symbol.id first)
           (Semantic_symbol.id second)
         && Semantic_symbol.Id.equal (Semantic_symbol.id first)
              (Semantic_symbol.id third));
      Alcotest.(check bool)
        "target result owns its typed expression batch" true
        (Target.source result == expressions))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let outer_and_assembler_lookup_filter_record_kinds () =
  List.iter
    (fun mode ->
      let source =
        prepare ~mode ~path:"top-level-output-outer-targets.HC" "\"text\";'Z';"
      in
      let environment =
        environment source
          ~outer:
            [
              ("Print", Semantic_outer_environment.Function);
              ("Print", Semantic_outer_environment.Global_variable);
            ]
          ~assembler:[ ("PutChars", Semantic_outer_environment.Function) ]
      in
      let expressions = expression_results ~environment source in
      let result = resolve source expressions |> checked_target in
      let outputs = Target.outputs result in
      Alcotest.(check (list string))
        "outer and assembler records follow the active chain"
        [
          (match mode with
          | Preprocessor.Jit -> "jit-task-0"
          | Preprocessor.Aot -> "aot-parent-0");
          "assembler";
        ]
        (outputs
        |> List.map (fun output ->
            output |> Target.output_binding |> outer_target
            |> Semantic_outer_environment.binding_table
            |> Semantic_outer_environment.table_kind
            |> Semantic_outer_environment.table_kind_name));
      outputs
      |> List.iter (fun output ->
          let entry =
            output |> Target.output_binding |> outer_target
            |> Semantic_outer_environment.binding_entry
          in
          Alcotest.(check bool)
            "kind-filtered output target is a function" true
            (Semantic_outer_environment.entry_record_kind entry
            = Semantic_outer_environment.Function)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let recursive_outputs_retain_markers_and_typed_values () =
  List.iter
    (fun mode ->
      let source =
        prepare ~mode ~path:"top-level-output-shapes.HC"
          "extern U0 Print(U8 *fmt,...);extern U0 PutChars(U64 ch);\n\
           U8 *fmt;U64 ch;\n\
           if(1){\"nested=%d\",1;while(1)'A';}\n\
           \"\" fmt;'' ch;\n\
           #define GENERATED \"generated\"\n\
           GENERATED;"
      in
      let table = Session.semantic_symbols source.session in
      let before = Semantic_symbol_table.all_symbols table |> List.length in
      let expressions = expression_results source in
      let first = resolve source expressions |> checked_target in
      let second = resolve source expressions |> checked_target in
      let outputs = Target.outputs first in
      Alcotest.(check (list int))
        "recursive output identities are contiguous" [ 0; 1; 2; 3; 4 ]
        (List.map Target.output_index outputs);
      Alcotest.(check (list string))
        "recursive outputs keep their targets"
        [ "Print"; "PutChars"; "Print"; "PutChars"; "Print" ]
        (List.map Target.output_target_name outputs);
      Alcotest.(check (list string))
        "marker and following-expression forms stay distinct"
        [
          "marker";
          "marker";
          "following-expression";
          "following-expression";
          "marker";
        ]
        (outputs
        |> List.map (fun output ->
            output |> Target.output_fixed_source
            |> Semantic_function_call_resolution
               .implicit_output_fixed_source_name));
      Alcotest.(check (list int))
        "only Print's first recursive form has a trailing value"
        [ 1; 0; 0; 0; 0 ]
        (List.map
           (fun output -> Target.output_arguments output |> List.length)
           outputs);
      Alcotest.(check (list string))
        "fixed roots retain their checked value classes"
        [
          "U8*:address-value:integer-result:rank-0";
          "I64:object-value:integer-result:rank-0";
          "U8*:object-value:integer-result:rank-0";
          "U64:object-value:integer-result:rank-0";
          "U8*:address-value:integer-result:rank-0";
        ]
        (outputs
        |> List.map (fun output ->
            output |> Target.output_fixed_value
            |> Semantic_function_call_expression_result.top_level_root_value
            |> Test_top_level_expression_result.descriptor));
      let following = List.nth outputs 2 in
      Alcotest.(check bool)
        "empty marker origin stays separate from its following value" false
        (Target.output_marker_origin following
        = (following |> Target.output_fixed_value
         |> Semantic_function_call_expression_result.top_level_root_value
         |> Semantic_function_call_expression_result.result_origin));
      let generated = List.nth outputs 4 in
      (match Target.output_marker_origin generated with
      | Semantic_symbol.Source_location location ->
          Alcotest.(check bool)
            "generated marker retains its definition origin" true
            (Option.is_some location.defined_at)
      | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
          Alcotest.fail "expected a generated marker source location");
      Alcotest.(check (list int))
        "target replay is deterministic"
        (List.map Target.output_index outputs)
        (second |> Target.outputs |> List.map Target.output_index);
      Alcotest.(check int)
        "target resolution leaves semantic symbols unchanged" before
        (Semantic_symbol_table.all_symbols table |> List.length))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let missing_header_reports_the_literal_marker () =
  let source =
    prepare ~path:"top-level-output-missing-target.HC" "\"missing\";"
  in
  let expressions = expression_results source in
  let marker_origin =
    expressions |> Semantic_function_call_expression_result.top_level_statements
    |> List.hd
    |> Semantic_function_call_expression_result.top_level_statement_roots
    |> List.hd |> Semantic_function_call_expression_result.top_level_root_source
    |> Semantic_top_level_expression_tree.root_role
    |> function
    | Semantic_top_level_expression_tree.Implicit_output_fixed
        { marker_origin; _ } -> marker_origin
    | _ -> Alcotest.fail "expected an implicit output fixed root"
  in
  match resolve source expressions with
  | Ok _ -> Alcotest.fail "expected a missing top-level Print header"
  | Error error ->
      Alcotest.(check string)
        "missing-header code" "HCSEMA0059" (Target.error_code error);
      Alcotest.(check string)
        "missing-header message"
        "top-level implicit output requires a visible Print function header"
        (Target.error_message error);
      Alcotest.(check bool)
        "missing header points at the literal marker" true
        (Target.error_origin error = Some marker_origin)

let ownership_and_mode_are_checked () =
  let source =
    prepare ~path:"top-level-output-validation.HC"
      "extern U0 Print(U8 *fmt,...);\"value\";"
  in
  let expressions = expression_results source in
  let foreign = Session.create () in
  (match
     Holyc_lib.resolve_top_level_implicit_output_targets foreign
       ~function_types:source.function_types ~functions:source.functions
       expressions
   with
  | Ok _ -> Alcotest.fail "expected foreign-session output resolution to fail"
  | Error error ->
      Alcotest.(check string)
        "foreign-session code" "HCSEMA0058" (Target.error_code error));
  let aot_functions =
    Holyc_lib.resolve_function_identities source.session
      ~declarations:source.declarations ~functions:source.function_types
      ~compilation_mode:Preprocessor.Aot source.ast
    |> checked
  in
  match
    Holyc_lib.resolve_top_level_implicit_output_targets source.session
      ~function_types:source.function_types ~functions:aot_functions expressions
  with
  | Ok _ -> Alcotest.fail "expected output mode mismatch to fail"
  | Error error ->
      Alcotest.(check string)
        "mode mismatch code" "HCSEMA0058" (Target.error_code error)

let tests =
  [
    Alcotest.test_case "module headers follow top-level source order" `Quick
      module_headers_follow_top_level_source_order;
    Alcotest.test_case "outer lookup filters record kinds" `Quick
      outer_and_assembler_lookup_filter_record_kinds;
    Alcotest.test_case "recursive outputs retain markers and values" `Quick
      recursive_outputs_retain_markers_and_typed_values;
    Alcotest.test_case "missing header retains the literal marker" `Quick
      missing_header_reports_the_literal_marker;
    Alcotest.test_case "ownership and compilation mode" `Quick
      ownership_and_mode_are_checked;
  ]
