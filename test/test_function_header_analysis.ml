open Holyc_lib

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let checked_header = function
  | Ok value -> value
  | Error error ->
      Alcotest.fail (Semantic_function_header_analysis.error_to_string error)

let expect_ast = function
  | Ok ast -> ast
  | Error diagnostics ->
      Alcotest.failf "expected an AST, got %s"
        (diagnostics
        |> List.map (fun diagnostic ->
            Printf.sprintf "%s: %s" diagnostic.Diagnostic.code
              diagnostic.message)
        |> String.concat ", ")

let config mode = checked (Preprocessor.Config.create ~compilation_mode:mode ())

type prepared = {
  mode : Preprocessor.compilation_mode;
  session : Session.t;
  ast : Ast.module_;
  declarations : Semantic_declaration_collection.t;
  function_types : Semantic_function_type_resolution.t;
  functions : Semantic_function_resolution.t;
}

let finish_prepare ?compiler_option_mask mode session ast =
  let declarations = checked (Holyc_lib.collect_declarations session ast) in
  let aggregates =
    checked (Holyc_lib.resolve_aggregates session ~declarations ast)
  in
  let collected =
    checked (Holyc_lib.collect_functions session ~declarations ast)
  in
  let function_types =
    checked
      (Holyc_lib.resolve_function_types session ~declarations ~aggregates
         ~functions:collected ast)
  in
  let functions =
    checked
      (Holyc_lib.resolve_function_identities ?compiler_option_mask session
         ~declarations ~functions:function_types ~compilation_mode:mode ast)
  in
  { mode; session; ast; declarations; function_types; functions }

let prepare ?(mode = Preprocessor.Jit) ?compiler_option_mask ~path contents =
  let session = Session.create () in
  let source = Session.add_source session ~path ~contents in
  let ast =
    Holyc_lib.parse_with_config session ~config:(config mode) ~source
    |> expect_ast
  in
  finish_prepare ?compiler_option_mask mode session ast

let semantic_mode = function
  | Preprocessor.Jit -> Semantic_function_resolution.Jit
  | Preprocessor.Aot -> Semantic_function_resolution.Aot

let resolve_with_options prepared specifications =
  let functions =
    Semantic_function_type_resolution.functions prepared.function_types
  in
  if List.length functions <> List.length specifications then
    Alcotest.fail "option snapshots do not match the function batch";
  let declarations =
    List.map2
      (fun function_ (compiler_option_mask, kind) ->
        Semantic_function_resolution.make_declaration_with_options
          ~compiler_option_mask ~function_ ~kind
        |> checked)
      functions specifications
  in
  Semantic_function_resolution.resolve
    ~table:(Session.semantic_symbols prepared.session)
    ~parent:(Semantic_declaration_collection.scope prepared.declarations)
    ~compilation_mode:(semantic_mode prepared.mode)
    declarations
  |> checked

let declaration_parameters declaration =
  declaration |> Semantic_function_resolution.resolved_declaration_site
  |> Semantic_function_resolution.declaration_site_function
  |> Semantic_function_type_resolution.function_signature
  |> Semantic_function_type_resolution.signature_parameters

let make_input declaration defaults =
  let parameters = declaration_parameters declaration in
  if List.length parameters <> List.length defaults then
    Alcotest.fail "test defaults do not match the fixed parameter count";
  let inputs =
    List.map2
      (fun parameter evaluated_default ->
        Semantic_function_header_analysis.make_parameter_input ~parameter
          ~evaluated_default
        |> checked_header)
      parameters defaults
  in
  Semantic_function_header_analysis.make_function_input ~declaration inputs
  |> checked_header

let make_inputs functions defaults =
  let declarations = Semantic_function_resolution.declarations functions in
  if List.length declarations <> List.length defaults then
    Alcotest.fail "test defaults do not match the declaration count";
  List.map2 make_input declarations defaults

let analyze prepared defaults =
  Holyc_lib.analyze_function_headers prepared.session
    ~functions:prepared.functions
    (make_inputs prepared.functions defaults)
  |> checked_header

let only_comparison result =
  match Semantic_function_header_analysis.comparisons result with
  | [ comparison ] -> comparison
  | comparisons ->
      Alcotest.failf "expected one joined-header comparison, got %d"
        (List.length comparisons)

let warning_codes result =
  result |> Semantic_function_header_analysis.warnings
  |> List.map Semantic_function_header_analysis.warning_code

let warning_messages result =
  result |> Semantic_function_header_analysis.warnings
  |> List.map Semantic_function_header_analysis.warning_message

let site_item_index site =
  site |> Semantic_function_resolution.declaration_site_function
  |> Semantic_function_type_resolution.function_item_index

let bits value = Some (Semantic_function_header_analysis.Bits value)

let string_bytes value =
  Some (Semantic_function_header_analysis.String_bytes value)

let independent_return_and_argument_warnings () =
  let prepared =
    prepare ~path:"function-header-both.HC"
      "extern I64 Both(I64 first=1);\nU64 Both(U64 second=2){}"
  in
  let result = analyze prepared [ [ bits 1L ]; [ bits 2L ] ] in
  let comparison = only_comparison result in
  Alcotest.(check (option bool))
    "return mismatch" (Some false)
    (Semantic_function_header_analysis.comparison_return_types_match comparison);
  Alcotest.(check (option bool))
    "argument mismatch" (Some false)
    (Semantic_function_header_analysis.comparison_arguments_match comparison);
  Alcotest.(check (list string))
    "source warning order"
    [ "HCSEMA0037"; "HCSEMA0038" ]
    (warning_codes result);
  Alcotest.(check (list string))
    "specific warning messages"
    [
      "function \"Both\" return type does not match the replaced header";
      "function \"Both\" argument list does not match the replaced header";
    ]
    (warning_messages result);
  List.iter
    (fun warning ->
      Alcotest.(check int)
        "warning keeps the current declaration" 1
        (warning |> Semantic_function_header_analysis.warning_declaration
       |> Semantic_function_resolution.resolved_declaration_site
       |> site_item_index);
      Alcotest.(check int)
        "warning keeps the replaced declaration" 0
        (warning |> Semantic_function_header_analysis.warning_replaced_header
       |> site_item_index))
    (Semantic_function_header_analysis.warnings result)

let evaluated_defaults_match_source_records () =
  let same =
    prepare ~path:"function-header-defaults.HC"
      "extern U0 Values(I64 kind=lastclass,F64 ratio=1.5,U8 *text=\"alpha\");\n\
       U0 Values(I64 kind=0,F64 ratio=1.5,U8 *text=\"alpha\"){}"
  in
  let float_bits = Int64.bits_of_float 1.5 in
  let result =
    analyze same
      [
        [ None; bits float_bits; string_bytes "alpha\000old-storage" ];
        [ bits 0L; bits float_bits; string_bytes "alpha\000new-storage" ];
      ]
  in
  let comparison = only_comparison result in
  Alcotest.(check (option bool))
    "lastclass uses the zero record payload" (Some true)
    (Semantic_function_header_analysis.comparison_arguments_match comparison);
  Alcotest.(check (list string))
    "matching defaults do not warn" [] (warning_codes result);
  let changed =
    prepare ~path:"function-header-default-changes.HC"
      "extern U0 Values(F64 ratio=1.5,U8 *text=\"alpha\");\n\
       U0 Values(F64 ratio=2.0,U8 *text=\"beta\"){}"
  in
  let changed_result =
    analyze changed
      [
        [ bits (Int64.bits_of_float 1.5); string_bytes "alpha" ];
        [ bits (Int64.bits_of_float 2.0); string_bytes "beta" ];
      ]
  in
  Alcotest.(check (list string))
    "changed payloads warn once" [ "HCSEMA0038" ]
    (warning_codes changed_result)

let member_class_projection () =
  let names =
    prepare ~path:"function-header-names.HC"
      "extern U0 Names(I64 left);\nU0 Names(I64 right){}"
  in
  Alcotest.(check (list string))
    "parameter spellings are compared" [ "HCSEMA0038" ]
    (analyze names [ [ None ]; [ None ] ] |> warning_codes);
  let callback =
    prepare ~path:"function-header-callback.HC"
      "extern U0 Callback(I64 (*cb)(I64 nested=1));\n\
       U0 Callback(U8 (*cb)(F64 changed=2)){}"
  in
  let callback_result = analyze callback [ [ None ]; [ None ] ] in
  Alcotest.(check (option bool))
    "callback return and nested signature are ignored" (Some true)
    (only_comparison callback_result
    |> Semantic_function_header_analysis.comparison_arguments_match);
  let depth =
    prepare ~path:"function-header-callback-depth.HC"
      "extern U0 Callback(I64 (*cb)());\nU0 Callback(I64 (**cb)()){}"
  in
  Alcotest.(check (list string))
    "callback pointer depth is compared" [ "HCSEMA0038" ]
    (analyze depth [ [ None ]; [ None ] ] |> warning_codes);
  let intrinsic =
    prepare ~path:"function-header-intrinsic.HC"
      "extern U0 Raw(I64 value);\nU0 Raw(I64i value){}"
  in
  Alcotest.(check (list string))
    "public and intrinsic classes stay distinct" [ "HCSEMA0038" ]
    (analyze intrinsic [ [ None ]; [ None ] ] |> warning_codes);
  let aggregate =
    prepare ~path:"function-header-aggregate.HC"
      "class A {}; class B {};\nextern U0 Use(A value);\nU0 Use(B value){}"
  in
  Alcotest.(check (list string))
    "aggregate identity is compared" [ "HCSEMA0038" ]
    (analyze aggregate [ [ None ]; [ None ] ] |> warning_codes)

let old_header_count_is_asymmetric () =
  let old_variadic =
    prepare ~path:"function-header-old-variadic.HC"
      "extern U0 Prefix(I64 first,...);\nU0 Prefix(I64 first,I64 extra){}"
  in
  let result = analyze old_variadic [ [ None ]; [ None; None ] ] in
  Alcotest.(check (option bool))
    "old variadic tail caps the comparison" (Some true)
    (only_comparison result
   |> Semantic_function_header_analysis.comparison_arguments_match);
  let old_fixed =
    prepare ~path:"function-header-old-fixed.HC"
      "extern U0 Reverse(I64 first,I64 extra);\nU0 Reverse(I64 first,...){}"
  in
  Alcotest.(check (list string))
    "reversing the declarations exposes the mismatch" [ "HCSEMA0038" ]
    (analyze old_fixed [ [ None; None ]; [ None ] ] |> warning_codes);
  let zero_variadic =
    prepare ~path:"function-header-zero-variadic.HC"
      "extern U0 Zero(...);\nU0 Zero(U64 anything){}"
  in
  Alcotest.(check (option bool))
    "two nonempty lists match at an old count of zero" (Some true)
    (analyze zero_variadic [ []; [ None ] ]
    |> only_comparison
    |> Semantic_function_header_analysis.comparison_arguments_match);
  let zero_empty =
    prepare ~path:"function-header-zero-empty.HC"
      "extern U0 Empty();\nU0 Empty(...){}"
  in
  Alcotest.(check (list string))
    "an empty old list and nonempty current list differ" [ "HCSEMA0038" ]
    (analyze zero_empty [ []; [] ] |> warning_codes)

let register_flags_and_disabled_option_do_not_warn () =
  let ignored =
    prepare ~path:"function-header-ignored-fields.HC"
      "extern U0 Same(reg R15 I64 value);\ninterrupt U0 Same(noreg I64 value){}"
  in
  Alcotest.(check (list string))
    "register requests and function flags are omitted" []
    (analyze ignored [ [ None ]; [ None ] ] |> warning_codes);
  let disabled_mask, _ =
    Compiler_option.set ~mask:Compiler_option.initial_mask
      Compiler_option.Warn_header_mismatch false
  in
  let disabled =
    prepare ~compiler_option_mask:disabled_mask
      ~path:"function-header-warning-disabled.HC"
      "extern I64 Quiet(I64 before);\nU64 Quiet(U64 after){}"
  in
  let result = analyze disabled [ [ None ]; [ None ] ] in
  let comparison = only_comparison result in
  Alcotest.(check bool)
    "option snapshot is disabled" false
    (Semantic_function_header_analysis.comparison_option_enabled comparison);
  Alcotest.(check (option bool))
    "disabled comparison has no synthetic result" None
    (Semantic_function_header_analysis.comparison_return_types_match comparison);
  Alcotest.(check (option bool))
    "disabled arguments have no synthetic result" None
    (Semantic_function_header_analysis.comparison_arguments_match comparison);
  Alcotest.(check (list string))
    "disabled option emits no warning" [] (warning_codes result);
  let per_site =
    prepare ~path:"function-header-per-site-options.HC"
      "extern I64 PerSite(I64 before);\nU64 PerSite(U64 after){}"
  in
  let declarations =
    resolve_with_options per_site
      [
        (Compiler_option.initial_mask, Semantic_function_resolution.Extern);
        (disabled_mask, Semantic_function_resolution.Definition);
      ]
  in
  let disabled_result =
    Holyc_lib.analyze_function_headers per_site.session ~functions:declarations
      (make_inputs declarations [ [ None ]; [ None ] ])
    |> checked_header
  in
  Alcotest.(check (list string))
    "the current disabled snapshot controls the join" []
    (warning_codes disabled_result);
  let declarations =
    resolve_with_options per_site
      [
        (disabled_mask, Semantic_function_resolution.Extern);
        (Compiler_option.initial_mask, Semantic_function_resolution.Definition);
      ]
  in
  let enabled_result =
    Holyc_lib.analyze_function_headers per_site.session ~functions:declarations
      (make_inputs declarations [ [ None ]; [ None ] ])
    |> checked_header
  in
  Alcotest.(check (list string))
    "the current enabled snapshot emits both warnings"
    [ "HCSEMA0037"; "HCSEMA0038" ]
    (warning_codes enabled_result)

let source_origin = function
  | Semantic_symbol.Source_location source -> source
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected source provenance"

let warning_source result =
  result |> Semantic_function_header_analysis.warnings |> List.hd
  |> Semantic_function_header_analysis.warning_origin |> source_origin

let remove_tree path =
  let rec remove path =
    match (Unix.lstat path).st_kind with
    | Unix.S_DIR ->
        Sys.readdir path |> Array.to_list |> List.sort String.compare
        |> List.iter (fun name -> remove (Filename.concat path name));
        Unix.rmdir path
    | _ -> Unix.unlink path
  in
  if Sys.file_exists path then remove path

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let generated_and_included_warning_provenance () =
  let generated =
    prepare ~path:"function-header-generated.HC"
      "#define FN Join\nextern I64 FN(I64 value);\nU64 FN(U64 value){}"
  in
  let generated_source =
    analyze generated [ [ None ]; [ None ] ] |> warning_source
  in
  Alcotest.(check bool)
    "generated name keeps its invocation" true
    (Option.is_some generated_source.generated_from);
  Alcotest.(check bool)
    "generated name keeps its definition" true
    (Option.is_some generated_source.defined_at);
  let directory = Filename.temp_dir "holyc-header-analysis-" "" in
  Fun.protect
    ~finally:(fun () -> remove_tree directory)
    (fun () ->
      let root_path = Filename.concat directory "root.HC" in
      let include_path = Filename.concat directory "headers.HC" in
      write_file root_path "#include \"headers\"";
      write_file include_path
        "extern I64 Included(I64 value); U64 Included(U64 value){}";
      let session = Session.create () in
      let source = checked (Session.load_source session ~path:root_path) in
      let config =
        checked (Preprocessor.Config.create ~working_directory:directory ())
      in
      let ast =
        Holyc_lib.parse_with_config session ~config ~source |> expect_ast
      in
      let prepared = finish_prepare Preprocessor.Jit session ast in
      let site = analyze prepared [ [ None ]; [ None ] ] |> warning_source in
      let source =
        Source_manager.find (Session.sources session) site.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "warning points into the included header" "headers.HC"
        (Source_file.path source |> Filename.basename))

let invalid_inputs_are_stable_and_pure () =
  let prepared =
    prepare ~path:"function-header-invalid.HC"
      "extern U0 Check(I64 scalar=1,U8 *text=\"x\",I64 kind=lastclass);\n\
       U0 Check(I64 scalar=1,U8 *text=\"x\",I64 kind=lastclass){}"
  in
  let declarations =
    Semantic_function_resolution.declarations prepared.functions
  in
  let first = List.hd declarations in
  let parameters = declaration_parameters first in
  let expect_error expected = function
    | Ok _ -> Alcotest.fail "expected invalid header input"
    | Error error ->
        Alcotest.(check string)
          "stable invalid-input code" "HCSEMA0036"
          (Semantic_function_header_analysis.error_code error);
        Alcotest.(check string)
          "specific invalid-input message" expected
          (Semantic_function_header_analysis.error_message error)
  in
  expect_error
    "an ordinary function default requires a compile-time evaluated value"
    (Semantic_function_header_analysis.make_parameter_input
       ~parameter:(List.nth parameters 0) ~evaluated_default:None);
  expect_error
    "a string-backed function default requires evaluated string bytes"
    (Semantic_function_header_analysis.make_parameter_input
       ~parameter:(List.nth parameters 1) ~evaluated_default:(bits 1L));
  expect_error
    "a lastclass default uses its zero-initialized record payload and cannot \
     accept an evaluated value"
    (Semantic_function_header_analysis.make_parameter_input
       ~parameter:(List.nth parameters 2) ~evaluated_default:(bits 0L));
  let inputs =
    make_inputs prepared.functions
      [
        [ bits 1L; string_bytes "x"; None ]; [ bits 1L; string_bytes "x"; None ];
      ]
  in
  let table = Session.semantic_symbols prepared.session in
  let before_symbols = Semantic_symbol_table.all_symbols table |> List.length in
  let first_result =
    Holyc_lib.analyze_function_headers prepared.session
      ~functions:prepared.functions inputs
    |> checked_header
  in
  let second_result =
    Holyc_lib.analyze_function_headers prepared.session
      ~functions:prepared.functions inputs
    |> checked_header
  in
  Alcotest.(check (list string))
    "repeated analysis is deterministic"
    (warning_codes first_result)
    (warning_codes second_result);
  Alcotest.(check int)
    "analysis does not mutate symbols" before_symbols
    (Semantic_symbol_table.all_symbols table |> List.length);
  expect_error
    "function header input count does not match function identity resolution"
    (Holyc_lib.analyze_function_headers prepared.session
       ~functions:prepared.functions
       [ List.hd inputs ]);
  let foreign = Session.create () in
  expect_error "function header analysis belongs to a different symbol table"
    (Holyc_lib.analyze_function_headers foreign ~functions:prepared.functions
       inputs)

let tests =
  [
    Alcotest.test_case "independent return and argument warnings" `Quick
      independent_return_and_argument_warnings;
    Alcotest.test_case "evaluated default payloads" `Quick
      evaluated_defaults_match_source_records;
    Alcotest.test_case "member class projection" `Quick member_class_projection;
    Alcotest.test_case "old header count asymmetry" `Quick
      old_header_count_is_asymmetric;
    Alcotest.test_case "ignored fields and disabled option" `Quick
      register_flags_and_disabled_option_do_not_warn;
    Alcotest.test_case "generated and included warning provenance" `Quick
      generated_and_included_warning_provenance;
    Alcotest.test_case "invalid inputs, determinism, and purity" `Quick
      invalid_inputs_are_stable_and_pure;
  ]
