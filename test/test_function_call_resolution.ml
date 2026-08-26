open Holyc_lib

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

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
  mode : Preprocessor.compilation_mode;
  session : Session.t;
  ast : Ast.module_;
  declarations : Semantic_declaration_collection.t;
  function_types : Semantic_function_type_resolution.t;
  local_types : Semantic_local_type_resolution.t;
  global_types : Semantic_global_type_resolution.t;
  functions : Semantic_function_resolution.t;
  module_expressions : Semantic_module_expression_binding.t;
}

let finish_prepare mode session ast =
  let declarations = checked (Holyc_lib.collect_declarations session ast) in
  let aggregates =
    checked (Holyc_lib.resolve_aggregates session ~declarations ast)
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
  {
    mode;
    session;
    ast;
    declarations;
    function_types;
    local_types;
    global_types;
    functions;
    module_expressions;
  }

let prepare ?(mode = Preprocessor.Jit) ~path contents =
  let session = Session.create () in
  let source = Session.add_source session ~path ~contents in
  let ast =
    Holyc_lib.parse_with_config session ~config:(config mode) ~source
    |> expect_ast
  in
  finish_prepare mode session ast

let resolve prepared =
  Holyc_lib.resolve_function_calls prepared.session
    ~declarations:prepared.declarations ~function_types:prepared.function_types
    ~local_types:prepared.local_types ~global_types:prepared.global_types
    ~functions:prepared.functions ~expressions:prepared.module_expressions
    prepared.ast

let function_named result name =
  Semantic_function_call_resolution.functions result
  |> List.filter (fun function_ ->
      function_ |> Semantic_function_call_resolution.function_symbol
      |> Semantic_symbol.name |> String.equal name)
  |> List.rev |> List.hd

let calls_named result name =
  function_named result name |> Semantic_function_call_resolution.function_calls

let direct = function
  | Semantic_function_call_resolution.Direct_call call -> call
  | Semantic_function_call_resolution.Indirect_call _ ->
      Alcotest.fail "expected a direct call, got an indirect call"
  | Semantic_function_call_resolution.Deferred_call _ ->
      Alcotest.fail "expected a resolved direct function call"

let only_direct result name =
  match calls_named result name with
  | [ call ] -> direct call
  | calls ->
      Alcotest.failf "expected one call in %s, got %d" name (List.length calls)

let indirect = function
  | Semantic_function_call_resolution.Indirect_call call -> call
  | Semantic_function_call_resolution.Direct_call _ ->
      Alcotest.fail "expected an indirect call, got a direct call"
  | Semantic_function_call_resolution.Deferred_call _ ->
      Alcotest.fail "expected a resolved indirect call"

let symbol_id symbol = Semantic_symbol.id symbol |> Semantic_symbol.Id.to_int

let defaulted = function
  | Semantic_function_call_resolution.Declared_default use -> use
  | Semantic_function_call_resolution.Provided_argument _ ->
      Alcotest.fail "expected a declared default"

let provided = function
  | Semantic_function_call_resolution.Provided_argument argument -> argument
  | Semantic_function_call_resolution.Declared_default _ ->
      Alcotest.fail "expected a provided call argument"

let fixed_values call =
  call |> Semantic_function_call_resolution.direct_fixed_arguments
  |> List.map Semantic_function_call_resolution.fixed_value

let source_location = function
  | Semantic_symbol.Source_location location -> location
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected a source-positioned call"

let fixed_defaults_and_sparse_slots () =
  let prepared =
    prepare ~path:"function-call-fixed.HC"
      "extern I64 Mix(I64 first=1,I64 required,I64 last=3);\n\
       I64 Caller(){return Mix(,7);}"
  in
  let result = resolve prepared |> checked in
  let call = only_direct result "Caller" in
  Alcotest.(check string)
    "call syntax" "parenthesized"
    (call |> Semantic_function_call_resolution.direct_source
   |> Semantic_function_call_resolution.call_syntax
   |> Semantic_function_call_resolution.call_syntax_name);
  let values = fixed_values call in
  Alcotest.(check int) "three fixed bindings" 3 (List.length values);
  let first = List.nth values 0 |> defaulted in
  Alcotest.(check bool)
    "explicit first omission is retained" true
    (first |> Semantic_function_call_resolution.default_omission
   |> Option.is_some);
  let second = List.nth values 1 |> provided in
  Alcotest.(check int)
    "required argument keeps its source slot" 1
    (Semantic_function_call_resolution.argument_index second);
  let last = List.nth values 2 |> defaulted in
  Alcotest.(check bool)
    "absent trailing slot is distinct" true
    (last |> Semantic_function_call_resolution.default_omission
   |> Option.is_none);
  Alcotest.(check int64)
    "fixed call has no variadic extras" 0L
    (Semantic_function_call_resolution.direct_variadic_count call)

let active_header_and_canonical_identity () =
  let joined =
    prepare ~path:"function-call-joined.HC"
      "extern I64 Joined(I64 value=1);\n\
       I64 Joined(I64 value=2){return Joined();}"
  in
  let result = resolve joined |> checked in
  let call = only_direct result "Joined" in
  let header_symbol =
    call |> Semantic_function_call_resolution.direct_active_header
    |> Semantic_function_type_resolution.function_symbol
  in
  let target = Semantic_function_call_resolution.direct_target_symbol call in
  Alcotest.(check bool)
    "definition header and joined identity stay distinct" true
    (symbol_id header_symbol <> symbol_id target);
  Alcotest.(check int)
    "the definition supplies the active header" 1
    (call |> Semantic_function_call_resolution.direct_active_header
   |> Semantic_function_type_resolution.function_item_index);
  let visible =
    prepare ~path:"function-call-visible-header.HC"
      "extern I64 Pick(I64 value=1);\n\
       I64 Before(){return Pick();}\n\
       extern I64 Pick(I64 value=2);\n\
       I64 After(){return Pick();}"
  in
  let calls = resolve visible |> checked in
  Alcotest.(check int)
    "earlier caller sees the first header" 0
    (only_direct calls "Before"
   |> Semantic_function_call_resolution.direct_active_header
   |> Semantic_function_type_resolution.function_item_index);
  Alcotest.(check int)
    "later caller sees the replacement header" 2
    (only_direct calls "After"
   |> Semantic_function_call_resolution.direct_active_header
   |> Semantic_function_type_resolution.function_item_index)

let parenthesis_free_and_oracle_evidence () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"function-call-parenthesis-free.HC"
          "extern I64 Mixed(I64 first=1,I64 required,I64 last=3);\n\
           extern I64 Zero();\n\
           extern I64 Var(...);\n\
           I64 Caller(){Mixed 7;Zero;return Var;}"
      in
      let result = resolve prepared |> checked in
      let calls = calls_named result "Caller" in
      Alcotest.(check int) "three parenthesis-free calls" 3 (List.length calls);
      let mixed = List.nth calls 0 |> direct in
      Alcotest.(check string)
        "mixed syntax" "parenthesis-free"
        (mixed |> Semantic_function_call_resolution.direct_source
       |> Semantic_function_call_resolution.call_syntax
       |> Semantic_function_call_resolution.call_syntax_name);
      let omissions =
        fixed_values mixed
        |> List.filter_map (fun value ->
            match value with
            | Semantic_function_call_resolution.Provided_argument _ -> None
            | Semantic_function_call_resolution.Declared_default use ->
                Semantic_function_call_resolution.default_omission use)
      in
      Alcotest.(check int)
        "parenthesis-free defaults retain two omitted slots" 2
        (List.length omissions);
      let var = List.nth calls 2 |> direct in
      Alcotest.(check int64)
        "parenthesis-free variadic tail is empty" 0L
        (Semantic_function_call_resolution.direct_variadic_count var))
    [ Preprocessor.Jit; Preprocessor.Aot ];
  let fixture =
    [
      "oracle/parenthesis-free-calls.json";
      "test/oracle/parenthesis-free-calls.json";
      "../test/oracle/parenthesis-free-calls.json";
    ]
    |> List.find_opt Sys.file_exists
    |> Option.get |> Yojson.Safe.from_file
  in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "oracle reference commit" "c26482bb6ad3f80106d28504ec5db3c6a360732c"
    (fixture |> member "reference" |> member "commit" |> to_string);
  Alcotest.(check (list int))
    "native oracle values"
    [ 10; 46; 173; 23; 0; 12; 1 ]
    (fixture |> member "observed_values" |> to_list |> List.map to_int)

let variadic_slots_and_shape_errors () =
  let prepared =
    prepare ~path:"function-call-variadic.HC"
      "extern I64 Var(I64 fixed=1,...);\nI64 Caller(){Var();return Var(2,3,4);}"
  in
  let result = resolve prepared |> checked in
  let calls = calls_named result "Caller" |> List.map direct in
  Alcotest.(check (list int64))
    "variadic counts" [ 0L; 2L ]
    (List.map Semantic_function_call_resolution.direct_variadic_count calls);
  Alcotest.(check (list int))
    "variadic source slots" [ 1; 2 ]
    (List.nth calls 1
   |> Semantic_function_call_resolution.direct_variadic_arguments
    |> List.map Semantic_function_call_resolution.argument_index);
  let expect_error code text source =
    let prepared = prepare ~path:"function-call-error.HC" source in
    match resolve prepared with
    | Ok _ -> Alcotest.failf "expected %s" code
    | Error message ->
        Alcotest.(check bool)
          "stable call diagnostic" true
          (String.starts_with ~prefix:(code ^ ": ") message);
        Alcotest.(check bool)
          "specific call diagnostic" true
          (String.ends_with ~suffix:text message)
  in
  expect_error "HCSEMA0040"
    "call to \"Fixed\" is missing required argument 1 (value)"
    "extern I64 Fixed(I64 value);I64 Caller(){return Fixed();}";
  expect_error "HCSEMA0041"
    "call to \"Fixed\" provides argument 2, but its active header has 1 fixed \
     parameter"
    "extern I64 Fixed(I64 value);I64 Caller(){return Fixed(1,2);}";
  expect_error "HCSEMA0042"
    "call to \"Var\" omits variadic argument 2; variadic positions require an \
     expression"
    "extern I64 Var(I64 fixed,...);I64 Caller(){return Var(1,,3);}";
  expect_error "HCSEMA0040"
    "call to \"callback\" is missing required argument 1 (value)"
    "I64 Caller(I64 (*callback)(I64 value)){return callback();}";
  expect_error "HCSEMA0041"
    "call to \"callback\" provides argument 2, but its active header has 1 \
     fixed parameter"
    "I64 Caller(I64 (*callback)(I64 value)){return (*callback)(1,2);}";
  expect_error "HCSEMA0042"
    "call to \"callback\" omits variadic argument 2; variadic positions \
     require an expression"
    "I64 Caller(I64 (*callback)(I64 fixed,...)){return callback(1,,3);}"

let typed_function_pointer_calls_resolve_indirectly () =
  let prepared =
    prepare ~path:"function-call-deferred.HC"
      "I64 (*GlobalCallback)();I64 (*CallbackArray)()[2];\n\
       I64 Caller(I64 (*local_callback)()){\n\
       local_callback();GlobalCallback();CallbackArray();return OuterCallback();\n\
       }"
  in
  let result = resolve prepared |> checked in
  let reasons =
    calls_named result "Caller"
    |> List.map (function
      | Semantic_function_call_resolution.Direct_call _ -> "direct"
      | Semantic_function_call_resolution.Indirect_call _ -> "indirect"
      | Semantic_function_call_resolution.Deferred_call { reason; _ } ->
          Semantic_function_call_resolution.deferred_reason_name reason)
  in
  Alcotest.(check (list string))
    "function pointers are not mislabeled as direct calls"
    [ "indirect"; "indirect"; "global-callee"; "outer-callee" ]
    reasons

let indirect_headers_bind_defaults_holes_and_varargs () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"function-call-indirect-headers.HC"
          "F64 (*Global)(I64 first=1,I64 required,F64 last=3,...);\n\
           I64 Caller(F64 (*parameter)(I64 first=1,I64 required,F64 \
           last=3,...),F64 (**deep)(I64 first=1,I64 required,F64 last=3,...)){\n\
           F64 (*automatic)(I64 first=1,I64 required,F64 last=3,...);\n\
           static F64 (*stored)(I64 first=1,I64 required,F64 last=3,...);\n\
           parameter(,2,,4);(**deep)(,2,,4);(*automatic)(,2,,4);stored(,2,,4);\n\
           return Global(,2,,4);}"
      in
      let calls =
        resolve prepared |> checked |> fun result ->
        calls_named result "Caller" |> List.map indirect
      in
      Alcotest.(check (list string))
        "all checked callback storage kinds resolve indirectly"
        [ "parameter"; "deep"; "automatic"; "stored"; "Global" ]
        (List.map
           (fun call ->
             call |> Semantic_function_call_resolution.indirect_source
             |> Semantic_function_call_resolution.call_callee_name)
           calls);
      Alcotest.(check (list string))
        "the explicit QSort-shaped dereference remains visible"
        [
          "identifier";
          "dereferenced-identifier:2";
          "dereferenced-identifier:1";
          "identifier";
          "identifier";
        ]
        (List.map
           (fun call ->
             call |> Semantic_function_call_resolution.indirect_source
             |> Semantic_function_call_resolution.call_callee_form
             |> Semantic_function_call_resolution.callee_form_name)
           calls);
      Alcotest.(check (list string))
        "every callback keeps its F64 return header"
        [ "F64"; "F64"; "F64"; "F64"; "F64" ]
        (List.map
           (fun call ->
             call |> Semantic_function_call_resolution.indirect_callable
             |> Semantic_function_call_resolution.callable_return_type
             |> Semantic_type_reference.spelling)
           calls);
      let path call =
        call |> Semantic_function_call_resolution.indirect_fixed_arguments
        |> List.map (fun fixed ->
            match Semantic_function_call_resolution.fixed_value fixed with
            | Semantic_function_call_resolution.Declared_default _ -> "default"
            | Semantic_function_call_resolution.Provided_argument _ ->
                "provided")
      in
      List.iter
        (fun call ->
          Alcotest.(check (list string))
            "callback holes select their declared defaults"
            [ "default"; "provided"; "default" ]
            (path call);
          Alcotest.(check (pair int64 (list int)))
            "callback varargs retain count and source slot" (1L, [ 3 ])
            ( Semantic_function_call_resolution.indirect_variadic_count call,
              call
              |> Semantic_function_call_resolution.indirect_variadic_arguments
              |> List.map Semantic_function_call_resolution.argument_index ))
        calls)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let indexed_callback_arrays_resolve_indirectly () =
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"function-call-indexed-callback-arrays.HC"
          "F64 (*Global)(I64 first=1,I64 required,F64 last=3,...)[2][3];\n\
           I64 Caller(){\n\
           F64 (*automatic)(I64 first=1,I64 required,F64 last=3,...)[2];\n\
           static F64 (*stored)(I64 first=1,I64 required,F64 last=3,...)[2][2];\n\
           automatic[1](,2,,4);stored[0][1](,2,,4);\n\
           return Global[1][2](,2,,4);}"
      in
      let calls =
        resolve prepared |> checked |> fun result ->
        calls_named result "Caller" |> List.map indirect
      in
      Alcotest.(check (list string))
        "local and global callback arrays keep source order"
        [ "automatic"; "stored"; "Global" ]
        (List.map
           (fun call ->
             call |> Semantic_function_call_resolution.indirect_source
             |> Semantic_function_call_resolution.call_callee_name)
           calls);
      Alcotest.(check (list string))
        "indexed callees retain their computed bracket trees"
        [ "index"; "index"; "index" ]
        (List.map
           (fun call ->
             call |> Semantic_function_call_resolution.indirect_source
             |> Semantic_function_call_resolution.call_computed_callee
             |> Option.map (fun expression ->
                 expression
                 |> Semantic_function_call_resolution.argument_expression_kind
                 |> Semantic_function_call_resolution
                    .argument_expression_kind_name)
             |> Option.value ~default:"missing")
           calls);
      List.iter
        (fun call ->
          Alcotest.(check string)
            "the array keeps its recursive callback return type" "F64"
            (call |> Semantic_function_call_resolution.indirect_callable
           |> Semantic_function_call_resolution.callable_return_type
           |> Semantic_type_reference.spelling);
          let paths =
            call |> Semantic_function_call_resolution.indirect_fixed_arguments
            |> List.map (fun fixed ->
                match Semantic_function_call_resolution.fixed_value fixed with
                | Semantic_function_call_resolution.Declared_default _ ->
                    "default"
                | Semantic_function_call_resolution.Provided_argument _ ->
                    "provided")
          in
          Alcotest.(check (list string))
            "middle holes use the retained callback header"
            [ "default"; "provided"; "default" ]
            paths;
          Alcotest.(check int64)
            "the variadic tail keeps its target count" 1L
            (Semantic_function_call_resolution.indirect_variadic_count call))
        calls)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let invalid_indexed_callback_arrays_report_rank_and_shape () =
  [
    ( "partial rank",
      "I64 (*Callback)(I64 value)[2][3];I64 Caller(){return Callback[0](1);}",
      "HCSEMA0039: indexed callback callee uses 1 bracket, but `Callback` has \
       2 dimensions" );
    ( "excess rank",
      "I64 (*Callback)(I64 value)[2];I64 Caller(){return Callback[0][1](1);}",
      "HCSEMA0039: indexed callback callee uses 2 brackets, but `Callback` has \
       1 dimension" );
    ( "ordinary array",
      "I64 Values[2];I64 Caller(){return Values[0]();}",
      "HCSEMA0039: indexed value `Values` has no function-pointer signature" );
  ]
  |> List.iter (fun (label, source, expected) ->
      let prepared =
        prepare ~path:("function-call-indexed-" ^ label ^ ".HC") source
      in
      Alcotest.(check (result reject string))
        label (Error expected) (resolve prepared))

let generated_and_included_call_provenance () =
  let generated =
    prepare ~path:"function-call-generated.HC"
      "#define TARGET Callee\n\
       extern I64 Callee(I64 value=1);\n\
       I64 Caller(){return TARGET();}"
  in
  let call =
    resolve generated |> checked |> fun result -> only_direct result "Caller"
  in
  let callee =
    call |> Semantic_function_call_resolution.direct_source
    |> Semantic_function_call_resolution.call_callee_origin |> source_location
  in
  Alcotest.(check bool)
    "generated callee keeps its invocation" true
    (Option.is_some callee.generated_from);
  Alcotest.(check bool)
    "generated callee keeps its definition" true
    (Option.is_some callee.defined_at);
  let directory = Filename.temp_dir "holyc-call-resolution-" "" in
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
        "extern I64 Included(I64 value=1);I64 Caller(){return Included();}";
      let session = Session.create () in
      let source = checked (Session.load_source session ~path:root_path) in
      let ast =
        Holyc_lib.parse_with_config session
          ~config:(config ~working_directory:directory Preprocessor.Jit)
          ~source
        |> expect_ast
      in
      let prepared = finish_prepare Preprocessor.Jit session ast in
      let location =
        resolve prepared |> checked |> fun result ->
        only_direct result "Caller"
        |> Semantic_function_call_resolution.direct_source
        |> Semantic_function_call_resolution.call_origin |> source_location
      in
      let call_source =
        Source_manager.find (Session.sources session) location.span.source
        |> Option.get
      in
      Alcotest.(check string)
        "included call keeps its source file" "calls.HC"
        (Source_file.path call_source |> Filename.basename))

let invalid_inputs_are_stable_and_pure () =
  let prepared =
    prepare ~path:"function-call-invalid.HC"
      "extern I64 Target(I64 value=1);I64 Caller(){return Target();}"
  in
  let table = Session.semantic_symbols prepared.session in
  let before = Semantic_symbol_table.all_symbols table |> List.length in
  let first = resolve prepared |> checked in
  let second = resolve prepared |> checked in
  let signature result =
    only_direct result "Caller"
    |> Semantic_function_call_resolution.direct_fixed_arguments
    |> List.map (fun fixed ->
        match Semantic_function_call_resolution.fixed_value fixed with
        | Semantic_function_call_resolution.Provided_argument argument ->
            "provided:"
            ^ string_of_int
                (Semantic_function_call_resolution.argument_index argument)
        | Semantic_function_call_resolution.Declared_default use ->
            "default:"
            ^ string_of_bool
                (use |> Semantic_function_call_resolution.default_omission
               |> Option.is_some))
  in
  Alcotest.(check (list string))
    "repeated call resolution is deterministic" (signature first)
    (signature second);
  Alcotest.(check int)
    "call resolution does not mutate symbols" before
    (Semantic_symbol_table.all_symbols table |> List.length);
  let inputs =
    prepared.module_expressions |> Semantic_module_expression_binding.functions
    |> List.map (fun function_ ->
        let symbol =
          Semantic_module_expression_binding.function_symbol function_
        in
        let calls =
          if String.equal (Semantic_symbol.name symbol) "Caller" then
            let occurrence =
              function_
              |> Semantic_module_expression_binding.function_occurrences
              |> List.hd
            in
            [
              checked
                (Semantic_function_call_resolution.make_call ~index:0
                   ~callee_occurrence_index:
                     (Semantic_module_expression_binding.occurrence_index
                        occurrence
                     + 1)
                   ~callee_name:
                     (Semantic_module_expression_binding.occurrence_name
                        occurrence)
                   ~callee_origin:
                     (Semantic_module_expression_binding.occurrence_origin
                        occurrence)
                   ~origin:
                     (Semantic_module_expression_binding.occurrence_origin
                        occurrence)
                   ~syntax:Semantic_function_call_resolution.Parenthesized []);
            ]
          else []
        in
        checked
          (Semantic_function_call_resolution.make_function ~symbol
             ~scope:
               (Semantic_module_expression_binding.function_scope function_)
             ~item_index:
               (Semantic_module_expression_binding.function_item_index function_)
             calls))
  in
  let expect_invalid label inputs =
    match
      Semantic_function_call_resolution.resolve ~table
        ~parent:(Semantic_declaration_collection.scope prepared.declarations)
        ~function_types:prepared.function_types ~functions:prepared.functions
        ~expressions:prepared.module_expressions inputs
    with
    | Ok _ -> Alcotest.failf "expected %s to fail" label
    | Error error ->
        Alcotest.(check string)
          (label ^ " diagnostic") "HCSEMA0039"
          (Semantic_function_call_resolution.error_code error)
  in
  expect_invalid "reordered batch" (List.rev inputs);
  expect_invalid "callee occurrence drift" inputs;
  let other_source =
    Session.add_source prepared.session ~path:"function-call-other-module.HC"
      ~contents:
        "extern I64 Other(I64 value);I64 Foreign(I64 value){return \
         Other(value);}"
  in
  let other_ast =
    Holyc_lib.parse_with_config prepared.session ~config:(config prepared.mode)
      ~source:other_source
    |> expect_ast
  in
  let other_same_session =
    finish_prepare prepared.mode prepared.session other_ast
  in
  let foreign_occurrence =
    other_same_session.module_expressions
    |> Semantic_module_expression_binding.functions
    |> List.find (fun function_ ->
        function_ |> Semantic_module_expression_binding.function_symbol
        |> Semantic_symbol.name |> String.equal "Foreign")
    |> Semantic_module_expression_binding.function_occurrences
    |> List.find (fun occurrence ->
        String.equal
          (Semantic_module_expression_binding.occurrence_name occurrence)
          "value")
  in
  let integer_type =
    checked
      (Semantic_type.make_primitive ~form:Semantic_type.Public_spelling
         ~primitive:Primitive_type.I64 ~pointer_depth:0)
  in
  let foreign_expression =
    checked
      (Semantic_function_call_resolution
       .make_bound_identifier_argument_expression ~occurrence:foreign_occurrence
         ~resolved_type:integer_type
         ~shape:Semantic_function_call_resolution.Object_value ~array_rank:0 ())
    |> fun kind ->
    Semantic_function_call_resolution.make_argument_expression ~kind
      ~origin:
        (Semantic_module_expression_binding.occurrence_origin foreign_occurrence)
  in
  let foreign_argument =
    checked
      (Semantic_function_call_resolution.make_argument ~index:0
         ~kind:Semantic_function_call_resolution.Provided
         ~expression:(Some foreign_expression)
         ~origin:
           (Semantic_module_expression_binding.occurrence_origin
              foreign_occurrence))
  in
  let caller_call =
    only_direct first "Caller"
    |> Semantic_function_call_resolution.direct_source
  in
  let foreign_callable_prepared =
    prepare ~path:"function-call-foreign-callable.HC"
      "class ForeignBox {};I64 Foreign(ForeignBox (*callback)()){return 0;}"
  in
  let callback_parameter =
    foreign_callable_prepared.function_types
    |> Semantic_function_type_resolution.functions
    |> List.find (fun function_ ->
        function_ |> Semantic_function_type_resolution.function_symbol
        |> Semantic_symbol.name |> String.equal "Foreign")
    |> Semantic_function_type_resolution.function_signature
    |> Semantic_function_type_resolution.signature_parameters |> List.hd
  in
  let callback_pointer =
    match
      Semantic_function_type_resolution.parameter_declarator_kind
        callback_parameter
    with
    | Semantic_function_type_resolution.Function_pointer pointer -> pointer
    | Semantic_function_type_resolution.Object ->
        Alcotest.fail "expected a foreign callback parameter"
  in
  let foreign_callable =
    Semantic_function_call_resolution.make_callable
      ~return_type:
        (Semantic_function_type_resolution.parameter_type_reference
           callback_parameter)
      ~function_pointer:callback_pointer
  in
  let call_with_foreign_callable =
    checked
      (Semantic_function_call_resolution.make_call
         ~index:(Semantic_function_call_resolution.call_index caller_call)
         ~callee_occurrence_index:
           (Semantic_function_call_resolution.call_callee_occurrence_index
              caller_call)
         ~callee_name:
           (Semantic_function_call_resolution.call_callee_name caller_call)
         ~callee_origin:
           (Semantic_function_call_resolution.call_callee_origin caller_call)
         ~callable:foreign_callable
         ~origin:(Semantic_function_call_resolution.call_origin caller_call)
         ~syntax:(Semantic_function_call_resolution.call_syntax caller_call)
         [])
  in
  let foreign_callable_inputs =
    prepared.module_expressions |> Semantic_module_expression_binding.functions
    |> List.map (fun function_ ->
        let symbol =
          Semantic_module_expression_binding.function_symbol function_
        in
        checked
          (Semantic_function_call_resolution.make_function ~symbol
             ~scope:
               (Semantic_module_expression_binding.function_scope function_)
             ~item_index:
               (Semantic_module_expression_binding.function_item_index function_)
             (if String.equal (Semantic_symbol.name symbol) "Caller" then
                [ call_with_foreign_callable ]
              else [])))
  in
  expect_invalid "foreign callback return type" foreign_callable_inputs;
  let foreign_call =
    checked
      (Semantic_function_call_resolution.make_call
         ~index:(Semantic_function_call_resolution.call_index caller_call)
         ~callee_occurrence_index:
           (Semantic_function_call_resolution.call_callee_occurrence_index
              caller_call)
         ~callee_name:
           (Semantic_function_call_resolution.call_callee_name caller_call)
         ~callee_origin:
           (Semantic_function_call_resolution.call_callee_origin caller_call)
         ~origin:(Semantic_function_call_resolution.call_origin caller_call)
         ~syntax:(Semantic_function_call_resolution.call_syntax caller_call)
         [ foreign_argument ])
  in
  let foreign_occurrence_inputs =
    prepared.module_expressions |> Semantic_module_expression_binding.functions
    |> List.map (fun function_ ->
        let symbol =
          Semantic_module_expression_binding.function_symbol function_
        in
        checked
          (Semantic_function_call_resolution.make_function ~symbol
             ~scope:
               (Semantic_module_expression_binding.function_scope function_)
             ~item_index:
               (Semantic_module_expression_binding.function_item_index function_)
             (if String.equal (Semantic_symbol.name symbol) "Caller" then
                [ foreign_call ]
              else [])))
  in
  expect_invalid "foreign bound occurrence" foreign_occurrence_inputs;
  let address_prepared =
    prepare ~path:"function-call-address-metadata.HC"
      "extern I64 Target(I64 address);I64 Handler(){return 1;}\n\
       I64 Caller(){return Target(&Handler);}"
  in
  let address_call =
    resolve address_prepared |> checked |> fun result ->
    only_direct result "Caller"
    |> Semantic_function_call_resolution.direct_source
  in
  let handler_occurrence =
    address_prepared.module_expressions
    |> Semantic_module_expression_binding.functions
    |> List.find (fun function_ ->
        function_ |> Semantic_module_expression_binding.function_symbol
        |> Semantic_symbol.name |> String.equal "Caller")
    |> Semantic_module_expression_binding.function_occurrences
    |> List.find (fun occurrence ->
        occurrence |> Semantic_module_expression_binding.occurrence_name
        |> String.equal "Handler")
  in
  let declaration_named name =
    address_prepared.functions |> Semantic_function_resolution.declarations
    |> List.find (fun declaration ->
        declaration |> Semantic_function_resolution.resolved_declaration_site
        |> Semantic_function_resolution.declaration_site_function
        |> Semantic_function_type_resolution.function_symbol
        |> Semantic_symbol.name |> String.equal name)
  in
  let handler_declaration = declaration_named "Handler" in
  let target_declaration = declaration_named "Target" in
  let address_type =
    checked
      (Semantic_type.make_primitive ~form:Semantic_type.Internal_storage
         ~primitive:Primitive_type.I64 ~pointer_depth:0)
  in
  Alcotest.(check (result reject string))
    "a declaration for another publication is rejected at construction"
    (Error "bound direct function declaration does not match its publication")
    (Semantic_function_call_resolution.make_bound_identifier_argument_expression
       ~occurrence:handler_occurrence ~resolved_type:address_type
       ~shape:Semantic_function_call_resolution.Direct_function_value
       ~array_rank:0 ~function_declaration:target_declaration
       ~function_address_path:Semantic_function_call_resolution.Jit_extern_slot
       ());
  let wrong_path_expression =
    checked
      (Semantic_function_call_resolution
       .make_bound_identifier_argument_expression ~occurrence:handler_occurrence
         ~resolved_type:address_type
         ~shape:Semantic_function_call_resolution.Direct_function_value
         ~array_rank:0 ~function_declaration:handler_declaration
         ~function_address_path:Semantic_function_call_resolution.Aot_absolute
         ())
    |> fun kind ->
    Semantic_function_call_resolution.make_argument_expression ~kind
      ~origin:
        (Semantic_module_expression_binding.occurrence_origin handler_occurrence)
  in
  let wrong_path_argument =
    checked
      (Semantic_function_call_resolution.make_argument ~index:0
         ~kind:Semantic_function_call_resolution.Provided
         ~expression:(Some wrong_path_expression)
         ~origin:
           (Semantic_module_expression_binding.occurrence_origin
              handler_occurrence))
  in
  let wrong_path_call =
    checked
      (Semantic_function_call_resolution.make_call
         ~index:(Semantic_function_call_resolution.call_index address_call)
         ~callee_occurrence_index:
           (Semantic_function_call_resolution.call_callee_occurrence_index
              address_call)
         ~callee_name:
           (Semantic_function_call_resolution.call_callee_name address_call)
         ~callee_origin:
           (Semantic_function_call_resolution.call_callee_origin address_call)
         ~origin:(Semantic_function_call_resolution.call_origin address_call)
         ~syntax:(Semantic_function_call_resolution.call_syntax address_call)
         [ wrong_path_argument ])
  in
  let wrong_path_inputs =
    address_prepared.module_expressions
    |> Semantic_module_expression_binding.functions
    |> List.map (fun function_ ->
        let symbol =
          Semantic_module_expression_binding.function_symbol function_
        in
        checked
          (Semantic_function_call_resolution.make_function ~symbol
             ~scope:
               (Semantic_module_expression_binding.function_scope function_)
             ~item_index:
               (Semantic_module_expression_binding.function_item_index function_)
             (if String.equal (Semantic_symbol.name symbol) "Caller" then
                [ wrong_path_call ]
              else [])))
  in
  (match
     Semantic_function_call_resolution.resolve
       ~table:(Session.semantic_symbols address_prepared.session)
       ~parent:
         (Semantic_declaration_collection.scope address_prepared.declarations)
       ~function_types:address_prepared.function_types
       ~functions:address_prepared.functions
       ~expressions:address_prepared.module_expressions wrong_path_inputs
   with
  | Ok _ -> Alcotest.fail "expected a forged function address path to fail"
  | Error error ->
      Alcotest.(check string)
        "a forged function address path has a stable diagnostic"
        "bound direct function has the wrong address path"
        (Semantic_function_call_resolution.error_message error));
  let foreign = Session.create () in
  (match
     Holyc_lib.resolve_function_calls foreign
       ~declarations:prepared.declarations
       ~function_types:prepared.function_types ~functions:prepared.functions
       ~local_types:prepared.local_types ~global_types:prepared.global_types
       ~expressions:prepared.module_expressions prepared.ast
   with
  | Ok _ -> Alcotest.fail "expected a foreign-session failure"
  | Error message ->
      Alcotest.(check string)
        "foreign session diagnostic"
        "HCSEMA0039: function call declarations belong to another symbol table"
        message);
  let other =
    prepare ~path:"function-call-foreign-types.HC"
      "I64 Global;extern I64 Other(I64 value);I64 Foreign(){I64 local;return \
       Other(local+Global);}"
  in
  let expect_driver_invalid label ~local_types ~global_types =
    match
      Holyc_lib.resolve_function_calls prepared.session
        ~declarations:prepared.declarations
        ~function_types:prepared.function_types ~functions:prepared.functions
        ~local_types ~global_types ~expressions:prepared.module_expressions
        prepared.ast
    with
    | Ok _ -> Alcotest.failf "expected %s to fail" label
    | Error message ->
        Alcotest.(check bool)
          (label ^ " diagnostic family")
          true
          (String.starts_with ~prefix:"HCSEMA0039:" message)
  in
  expect_driver_invalid "foreign local types" ~local_types:other.local_types
    ~global_types:prepared.global_types;
  expect_driver_invalid "foreign global types" ~local_types:prepared.local_types
    ~global_types:other.global_types

let named_cast_targets_validate_source_identity () =
  let prepared =
    prepare ~path:"function-call-named-cast-identity.HC"
      "F64 class Box {};\n\
       extern I64 Target(F64 value);\n\
       I64 Caller(I64 value){return Target(value(Box));}\n\
       I64 class Box {};"
  in
  let resolved = resolve prepared |> checked in
  let box_publications =
    prepared.module_expressions
    |> Semantic_module_expression_binding.publications
    |> List.filter_map (fun publication ->
        let symbol =
          Semantic_module_expression_binding.publication_canonical_symbol
            publication
        in
        if
          Semantic_module_expression_binding.publication_kind publication
          = Semantic_module_expression_binding.Aggregate
          && String.equal (Semantic_symbol.name symbol) "Box"
        then Some symbol
        else None)
  in
  let first_box, second_box =
    match box_publications with
    | [ first; second ] -> (first, second)
    | values ->
        Alcotest.failf "expected two Box publications, got %d"
          (List.length values)
  in
  let caller =
    resolved |> Semantic_function_call_resolution.functions
    |> List.find (fun function_ ->
        function_ |> Semantic_function_call_resolution.function_symbol
        |> Semantic_symbol.name |> String.equal "Caller")
  in
  let source_call =
    match Semantic_function_call_resolution.function_calls caller with
    | [ Semantic_function_call_resolution.Direct_call direct ] ->
        Semantic_function_call_resolution.direct_source direct
    | _ -> Alcotest.fail "expected one direct Caller call"
  in
  let source_argument =
    source_call |> Semantic_function_call_resolution.call_arguments |> List.hd
  in
  let source_expression =
    source_argument |> Semantic_function_call_resolution.argument_expression
    |> Option.get
  in
  let operand, retained_target =
    match
      Semantic_function_call_resolution.argument_expression_kind
        source_expression
    with
    | Semantic_function_call_resolution.Postfix_cast_expression (operand, target)
      -> (operand, target)
    | _ -> Alcotest.fail "expected a retained named cast"
  in
  let retained_symbol =
    match
      retained_target |> Semantic_type_reference.resolved_type
      |> Semantic_type.base
    with
    | Semantic_type.Aggregate symbol -> symbol
    | Semantic_type.Primitive _ ->
        Alcotest.fail "expected an aggregate cast target"
  in
  Alcotest.(check int)
    "the driver selects the source-visible aggregate identity"
    (Semantic_symbol.id first_box |> Semantic_symbol.Id.to_int)
    (Semantic_symbol.id retained_symbol |> Semantic_symbol.Id.to_int);
  let inputs target_symbol =
    let target_type =
      Semantic_type.make_aggregate ~symbol:target_symbol ~pointer_depth:0
      |> checked
    in
    let target =
      Semantic_type_reference.make ~spelling:"Box"
        ~spelling_origin:
          (Semantic_type_reference.spelling_origin retained_target)
        ~pointer_origins:[] ~resolved_type:target_type
      |> checked
    in
    let replacement_expression =
      Semantic_function_call_resolution.make_argument_expression
        ~kind:
          (Semantic_function_call_resolution.Postfix_cast_expression
             (operand, target))
        ~origin:
          (Semantic_function_call_resolution.argument_expression_origin
             source_expression)
    in
    let replacement_argument =
      Semantic_function_call_resolution.make_argument
        ~index:
          (Semantic_function_call_resolution.argument_index source_argument)
        ~kind:Semantic_function_call_resolution.Provided
        ~expression:(Some replacement_expression)
        ~origin:
          (Semantic_function_call_resolution.argument_origin source_argument)
      |> checked
    in
    let replacement_call =
      Semantic_function_call_resolution.make_call
        ~index:(Semantic_function_call_resolution.call_index source_call)
        ~callee_occurrence_index:
          (Semantic_function_call_resolution.call_callee_occurrence_index
             source_call)
        ~callee_name:
          (Semantic_function_call_resolution.call_callee_name source_call)
        ~callee_origin:
          (Semantic_function_call_resolution.call_callee_origin source_call)
        ~origin:(Semantic_function_call_resolution.call_origin source_call)
        ~syntax:(Semantic_function_call_resolution.call_syntax source_call)
        [ replacement_argument ]
      |> checked
    in
    resolved |> Semantic_function_call_resolution.functions
    |> List.map (fun function_ ->
        let symbol =
          Semantic_function_call_resolution.function_symbol function_
        in
        let calls =
          if String.equal (Semantic_symbol.name symbol) "Caller" then
            [ replacement_call ]
          else
            function_ |> Semantic_function_call_resolution.function_calls
            |> List.map (function
              | Semantic_function_call_resolution.Direct_call direct ->
                  Semantic_function_call_resolution.direct_source direct
              | Semantic_function_call_resolution.Indirect_call indirect ->
                  Semantic_function_call_resolution.indirect_source indirect
              | Semantic_function_call_resolution.Deferred_call { call; _ } ->
                  call)
        in
        Semantic_function_call_resolution.make_function ~symbol
          ~scope:(Semantic_function_call_resolution.function_scope function_)
          ~item_index:
            (Semantic_function_call_resolution.function_item_index function_)
          calls
        |> checked)
  in
  let table = Session.semantic_symbols prepared.session in
  let expect_invalid label expected target =
    let before = Semantic_symbol_table.all_symbols table |> List.length in
    (match
       Semantic_function_call_resolution.resolve ~table
         ~parent:(Semantic_declaration_collection.scope prepared.declarations)
         ~function_types:prepared.function_types ~functions:prepared.functions
         ~expressions:prepared.module_expressions (inputs target)
     with
    | Ok _ -> Alcotest.failf "expected %s to fail" label
    | Error error ->
        Alcotest.(check string)
          (label ^ " code") "HCSEMA0039"
          (Semantic_function_call_resolution.error_code error);
        Alcotest.(check string)
          (label ^ " message") expected
          (Semantic_function_call_resolution.error_message error));
    Alcotest.(check int)
      (label ^ " leaves symbols unchanged")
      before
      (Semantic_symbol_table.all_symbols table |> List.length)
  in
  expect_invalid "stale identity"
    "function call cast target does not match the source-visible aggregate \
     identity"
    second_box;
  let other_scope =
    Semantic_symbol_table.create_scope table
      ~parent:(Semantic_symbol_table.root table)
      ~kind:Semantic_symbol_table.Module ~name:"other module" ()
    |> checked
  in
  let wrong_scope =
    Semantic_symbol_table.add table ~scope:other_scope ~name:"Box"
      ~kind:Semantic_symbol.Aggregate_type
      ~origin:(Semantic_type_reference.spelling_origin retained_target)
    |> checked
  in
  expect_invalid "wrong module" "function call cast target has the wrong scope"
    wrong_scope;
  let foreign_table = Semantic_symbol_table.create () in
  let foreign_scope =
    Semantic_symbol_table.create_scope foreign_table
      ~parent:(Semantic_symbol_table.root foreign_table)
      ~kind:Semantic_symbol_table.Module ~name:"foreign module" ()
    |> checked
  in
  let foreign =
    Semantic_symbol_table.add foreign_table ~scope:foreign_scope ~name:"Box"
      ~kind:Semantic_symbol.Aggregate_type
      ~origin:(Semantic_type_reference.spelling_origin retained_target)
    |> checked
  in
  expect_invalid "foreign identity"
    "function call cast target belongs to another symbol table" foreign

let index_expression_constructors_validate_bracket_origins () =
  let base_origin = Semantic_symbol.Synthesized "index base" in
  let index_origin = Semantic_symbol.Synthesized "index value" in
  let opening_origin = Semantic_symbol.Synthesized "opening bracket" in
  let closing_origin = Semantic_symbol.Synthesized "closing bracket" in
  let base =
    Semantic_function_call_resolution.make_argument_expression
      ~kind:(Semantic_function_call_resolution.Integer_literal 0L)
      ~origin:base_origin
  in
  let index =
    Semantic_function_call_resolution.make_argument_expression
      ~kind:(Semantic_function_call_resolution.Float_literal 0L)
      ~origin:index_origin
  in
  let retained =
    checked
      (Semantic_function_call_resolution.make_index_argument_expression ~base
         ~opening_origin ~index ~closing_origin)
  in
  (match retained with
  | Semantic_function_call_resolution.Index_expression retained ->
      Alcotest.(check bool)
        "index constructor retains its base" true
        (Semantic_function_call_resolution.index_base retained == base);
      Alcotest.(check bool)
        "index constructor retains its value" true
        (Semantic_function_call_resolution.index_value retained == index);
      Alcotest.(check bool)
        "index constructor retains its opening bracket" true
        (Semantic_function_call_resolution.index_opening_origin retained
        = opening_origin);
      Alcotest.(check bool)
        "index constructor retains its closing bracket" true
        (Semantic_function_call_resolution.index_closing_origin retained
        = closing_origin)
  | _ -> Alcotest.fail "expected a checked index expression");
  Alcotest.(check (result reject string))
    "an empty opening-bracket origin is rejected"
    (Error "call argument index has an invalid opening-bracket origin")
    (Semantic_function_call_resolution.make_index_argument_expression ~base
       ~opening_origin:(Semantic_symbol.Synthesized "") ~index ~closing_origin);
  Alcotest.(check (result reject string))
    "an empty closing-bracket origin is rejected"
    (Error "call argument index has an invalid closing-bracket origin")
    (Semantic_function_call_resolution.make_index_argument_expression ~base
       ~opening_origin ~index ~closing_origin:(Semantic_symbol.Synthesized ""))

let expression_statement_constructors_validate_identity_and_origin () =
  let expression_origin = Semantic_symbol.Synthesized "statement expression" in
  let statement_origin = Semantic_symbol.Synthesized "expression statement" in
  let expression =
    Semantic_function_call_resolution.make_argument_expression
      ~kind:(Semantic_function_call_resolution.Integer_literal 0L)
      ~origin:expression_origin
  in
  let make ?(index = 0) ?(origin = statement_origin) () =
    Semantic_function_call_resolution.make_expression_statement ~index
      ~expression ~origin
  in
  let retained = checked (make ()) in
  Alcotest.(check int)
    "expression statement constructor retains its identity" 0
    (Semantic_function_call_resolution.expression_statement_index retained);
  Alcotest.(check bool)
    "expression statement constructor retains its expression" true
    (Semantic_function_call_resolution.expression_statement_expression retained
    == expression);
  Alcotest.(check bool)
    "expression statement constructor retains its origin" true
    (Semantic_function_call_resolution.expression_statement_origin retained
    = statement_origin);
  Alcotest.(check (result reject string))
    "a negative expression statement identity is rejected"
    (Error "function expression statement index cannot be negative")
    (make ~index:(-1) ());
  Alcotest.(check (result reject string))
    "an empty expression statement origin is rejected"
    (Error "function expression statement has an invalid source origin")
    (make ~origin:(Semantic_symbol.Synthesized "") ());
  let prepared =
    prepare ~path:"function-expression-statement-constructor.HC"
      "I64 Caller(){0;return 0;}"
  in
  let owner =
    prepared.module_expressions |> Semantic_module_expression_binding.functions
    |> List.hd
  in
  let noncontiguous = checked (make ~index:1 ()) in
  Alcotest.(check (result reject string))
    "a function rejects noncontiguous expression statement identities"
    (Error "function expression statement indexes are not contiguous")
    (Semantic_function_call_resolution.make_function
       ~symbol:(Semantic_module_expression_binding.function_symbol owner)
       ~scope:(Semantic_module_expression_binding.function_scope owner)
       ~item_index:
         (Semantic_module_expression_binding.function_item_index owner)
       ~expression_statements:[ noncontiguous ] [])

let implicit_output_constructors_validate_source_shape () =
  let marker_origin = Semantic_symbol.Synthesized "output marker" in
  let fixed_origin = Semantic_symbol.Synthesized "fixed output expression" in
  let comma_origin = Semantic_symbol.Synthesized "output argument comma" in
  let argument_origin = Semantic_symbol.Synthesized "output argument" in
  let statement_origin = Semantic_symbol.Synthesized "output statement" in
  let fixed_expression =
    Semantic_function_call_resolution.make_argument_expression
      ~kind:(Semantic_function_call_resolution.String_literal "")
      ~origin:fixed_origin
  in
  let argument_expression =
    Semantic_function_call_resolution.make_argument_expression
      ~kind:(Semantic_function_call_resolution.Integer_literal 0L)
      ~origin:argument_origin
  in
  let make_argument ?(index = 0) ?(leading_comma_origin = comma_origin)
      ?(origin = argument_origin) () =
    Semantic_function_call_resolution.make_implicit_output_argument ~index
      ~leading_comma_origin ~expression:argument_expression ~origin
  in
  let argument = checked (make_argument ()) in
  Alcotest.(check int)
    "implicit output argument retains its identity" 0
    (Semantic_function_call_resolution.implicit_output_argument_index argument);
  Alcotest.(check bool)
    "implicit output argument retains its comma" true
    (Semantic_function_call_resolution
     .implicit_output_argument_leading_comma_origin argument
    = comma_origin);
  let make ?(index = 0)
      ?(target = Semantic_function_call_resolution.Print_output)
      ?(marker_origin = marker_origin)
      ?(fixed_source = Semantic_function_call_resolution.Marker_fixed_output)
      ?(arguments = [ argument ]) ?(origin = statement_origin) () =
    Semantic_function_call_resolution.make_implicit_output ~index ~target
      ~marker_origin ~fixed_source ~fixed_expression ~arguments ~origin
  in
  let retained = checked (make ()) in
  Alcotest.(check string)
    "implicit output retains its target" "Print"
    (retained |> Semantic_function_call_resolution.implicit_output_target
   |> Semantic_function_call_resolution.implicit_output_target_name);
  Alcotest.(check string)
    "implicit output retains its fixed source" "marker"
    (retained |> Semantic_function_call_resolution.implicit_output_fixed_source
   |> Semantic_function_call_resolution.implicit_output_fixed_source_name);
  Alcotest.(check (result reject string))
    "a negative output argument identity is rejected"
    (Error "implicit output argument index cannot be negative")
    (make_argument ~index:(-1) ());
  Alcotest.(check (result reject string))
    "an empty output argument comma origin is rejected"
    (Error "implicit output argument comma has an invalid source origin")
    (make_argument ~leading_comma_origin:(Semantic_symbol.Synthesized "") ());
  Alcotest.(check (result reject string))
    "an empty output argument origin is rejected"
    (Error "implicit output argument has an invalid source origin")
    (make_argument ~origin:(Semantic_symbol.Synthesized "") ());
  Alcotest.(check (result reject string))
    "a negative implicit output identity is rejected"
    (Error "function implicit output index cannot be negative")
    (make ~index:(-1) ());
  Alcotest.(check (result reject string))
    "an empty output marker origin is rejected"
    (Error "function implicit output marker has an invalid source origin")
    (make ~marker_origin:(Semantic_symbol.Synthesized "") ());
  Alcotest.(check (result reject string))
    "an empty output statement origin is rejected"
    (Error "function implicit output statement has an invalid source origin")
    (make ~origin:(Semantic_symbol.Synthesized "") ());
  Alcotest.(check (result reject string))
    "PutChars rejects a variadic output argument"
    (Error "implicit PutChars output cannot have variadic arguments")
    (make ~target:Semantic_function_call_resolution.Put_chars_output ());
  let skipped_argument = checked (make_argument ~index:1 ()) in
  Alcotest.(check (result reject string))
    "implicit output rejects noncontiguous argument identities"
    (Error "implicit output argument indexes are not contiguous")
    (make ~arguments:[ skipped_argument ] ());
  let prepared =
    prepare ~path:"function-implicit-output-constructor.HC"
      "I64 Caller(){\"value\";return 0;}"
  in
  let owner =
    prepared.module_expressions |> Semantic_module_expression_binding.functions
    |> List.hd
  in
  let noncontiguous = checked (make ~index:1 ()) in
  Alcotest.(check (result reject string))
    "a function rejects noncontiguous implicit output identities"
    (Error "function implicit output indexes are not contiguous")
    (Semantic_function_call_resolution.make_function
       ~symbol:(Semantic_module_expression_binding.function_symbol owner)
       ~scope:(Semantic_module_expression_binding.function_scope owner)
       ~item_index:
         (Semantic_module_expression_binding.function_item_index owner)
       ~implicit_outputs:[ noncontiguous ] [])

let switch_selector_constructors_validate_identity_and_origins () =
  let keyword_origin = Semantic_symbol.Synthesized "switch keyword" in
  let expression_origin = Semantic_symbol.Synthesized "switch expression" in
  let statement_origin = Semantic_symbol.Synthesized "switch statement" in
  let expression =
    Semantic_function_call_resolution.make_argument_expression
      ~kind:(Semantic_function_call_resolution.Integer_literal 0L)
      ~origin:expression_origin
  in
  let make ?(index = 0)
      ?(mode = Semantic_function_call_resolution.Bounded_switch)
      ?(keyword_origin = keyword_origin) ?(origin = statement_origin) () =
    Semantic_function_call_resolution.make_selector ~index ~mode ~keyword_origin
      ~expression ~origin
  in
  let retained = checked (make ()) in
  Alcotest.(check int)
    "selector constructor retains its identity" 0
    (Semantic_function_call_resolution.selector_index retained);
  Alcotest.(check bool)
    "selector constructor retains its mode" true
    (Semantic_function_call_resolution.selector_mode retained
    = Semantic_function_call_resolution.Bounded_switch);
  Alcotest.(check bool)
    "selector constructor retains its expression" true
    (Semantic_function_call_resolution.selector_expression retained
    == expression);
  Alcotest.(check (result reject string))
    "a negative selector identity is rejected"
    (Error "function switch selector index cannot be negative")
    (make ~index:(-1) ());
  Alcotest.(check (result reject string))
    "an empty switch keyword origin is rejected"
    (Error "function switch keyword has an invalid source origin")
    (make ~keyword_origin:(Semantic_symbol.Synthesized "") ());
  Alcotest.(check (result reject string))
    "an empty switch statement origin is rejected"
    (Error "function switch statement has an invalid source origin")
    (make ~origin:(Semantic_symbol.Synthesized "") ());
  let prepared =
    prepare ~path:"function-switch-selector-constructor.HC"
      "I64 Caller(){switch(0){case 0:break;}return 0;}"
  in
  let owner =
    prepared.module_expressions |> Semantic_module_expression_binding.functions
    |> List.hd
  in
  let noncontiguous = checked (make ~index:1 ()) in
  Alcotest.(check (result reject string))
    "a function rejects noncontiguous selector identities"
    (Error "function switch selector indexes are not contiguous")
    (Semantic_function_call_resolution.make_function
       ~symbol:(Semantic_module_expression_binding.function_symbol owner)
       ~scope:(Semantic_module_expression_binding.function_scope owner)
       ~item_index:
         (Semantic_module_expression_binding.function_item_index owner)
       ~selectors:[ noncontiguous ] [])

let switch_case_constructors_validate_patterns_and_origins () =
  let keyword_origin = Semantic_symbol.Synthesized "case keyword" in
  let expression_origin = Semantic_symbol.Synthesized "case expression" in
  let ellipsis_origin = Semantic_symbol.Synthesized "case ellipsis" in
  let label_origin = Semantic_symbol.Synthesized "case label" in
  let expression =
    Semantic_function_call_resolution.make_argument_expression
      ~kind:(Semantic_function_call_resolution.Integer_literal 0L)
      ~origin:expression_origin
  in
  let range =
    Semantic_function_call_resolution.make_ranged_case_pattern
      ~start_expression:expression ~ellipsis_origin ~end_expression:expression
    |> checked
  in
  let make ?(index = 0) ?(keyword_origin = keyword_origin) ?(pattern = range)
      ?(origin = label_origin) () =
    Semantic_function_call_resolution.make_switch_case ~index ~keyword_origin
      ~pattern ~origin
  in
  let retained = checked (make ()) in
  Alcotest.(check int)
    "case constructor retains its identity" 0
    (Semantic_function_call_resolution.switch_case_index retained);
  (match Semantic_function_call_resolution.switch_case_pattern retained with
  | Semantic_function_call_resolution.Ranged_case retained ->
      Alcotest.(check bool)
        "ranged case retains its start" true
        (retained.start_expression == expression);
      Alcotest.(check bool)
        "ranged case retains its end" true
        (retained.end_expression == expression);
      Alcotest.(check bool)
        "ranged case retains its ellipsis" true
        (retained.ellipsis_origin = ellipsis_origin)
  | Semantic_function_call_resolution.Implicit_case
  | Semantic_function_call_resolution.Single_case _ ->
      Alcotest.fail "expected a retained ranged case");
  Alcotest.(check (result reject string))
    "an empty case ellipsis origin is rejected"
    (Error "function switch case ellipsis has an invalid source origin")
    (Semantic_function_call_resolution.make_ranged_case_pattern
       ~start_expression:expression
       ~ellipsis_origin:(Semantic_symbol.Synthesized "")
       ~end_expression:expression);
  Alcotest.(check (result reject string))
    "the case constructor rejects an unvalidated ellipsis origin"
    (Error "function switch case ellipsis has an invalid source origin")
    (make
       ~pattern:
         (Semantic_function_call_resolution.Ranged_case
            {
              start_expression = expression;
              ellipsis_origin = Semantic_symbol.Synthesized "";
              end_expression = expression;
            })
       ());
  Alcotest.(check (result reject string))
    "a negative case identity is rejected"
    (Error "function switch case index cannot be negative")
    (make ~index:(-1) ());
  Alcotest.(check (result reject string))
    "an empty case keyword origin is rejected"
    (Error "function switch case keyword has an invalid source origin")
    (make ~keyword_origin:(Semantic_symbol.Synthesized "") ());
  Alcotest.(check (result reject string))
    "an empty case label origin is rejected"
    (Error "function switch case label has an invalid source origin")
    (make ~origin:(Semantic_symbol.Synthesized "") ());
  let prepared =
    prepare ~path:"function-switch-case-constructor.HC"
      "I64 Caller(){switch(0){case 0:break;}return 0;}"
  in
  let owner =
    prepared.module_expressions |> Semantic_module_expression_binding.functions
    |> List.hd
  in
  let noncontiguous = checked (make ~index:1 ()) in
  Alcotest.(check (result reject string))
    "a function rejects noncontiguous case identities"
    (Error "function switch case indexes are not contiguous")
    (Semantic_function_call_resolution.make_function
       ~symbol:(Semantic_module_expression_binding.function_symbol owner)
       ~scope:(Semantic_module_expression_binding.function_scope owner)
       ~item_index:
         (Semantic_module_expression_binding.function_item_index owner)
       ~switch_cases:[ noncontiguous ] [])

let literal_constructors_retain_typed_payloads () =
  let origin = Semantic_symbol.Synthesized "literal constructor test" in
  let expressions =
    [
      Semantic_function_call_resolution.make_argument_expression
        ~kind:(Semantic_function_call_resolution.Integer_literal (-1L)) ~origin;
      Semantic_function_call_resolution.make_argument_expression
        ~kind:
          (Semantic_function_call_resolution.Float_literal 0x7ff8000000000042L)
        ~origin;
      Semantic_function_call_resolution.make_argument_expression
        ~kind:(Semantic_function_call_resolution.Character_literal 0x434241L)
        ~origin;
      Semantic_function_call_resolution.make_argument_expression
        ~kind:(Semantic_function_call_resolution.String_literal "a\nB$") ~origin;
    ]
  in
  let description expression =
    match Semantic_function_call_resolution.argument_expression_kind expression with
    | Semantic_function_call_resolution.Integer_literal value ->
        Printf.sprintf "integer:%Ld" value
    | Semantic_function_call_resolution.Float_literal bits ->
        Printf.sprintf "f64:%016Lx" bits
    | Semantic_function_call_resolution.Character_literal value ->
        Printf.sprintf "character:%016Lx" value
    | Semantic_function_call_resolution.String_literal bytes ->
        Printf.sprintf "string:%S:length-%d" bytes (String.length bytes)
    | _ -> Alcotest.fail "expected a typed literal constructor"
  in
  Alcotest.(check (list string))
    "literal constructors retain only their declared payload representation"
    [
      "integer:-1";
      "f64:7ff8000000000042";
      "character:0000000000434241";
      "string:\"a\\nB$\":length-4";
    ]
    (List.map description expressions);
  Alcotest.(check bool)
    "literal constructors retain their source origin" true
    (List.for_all
       (fun expression ->
         Semantic_function_call_resolution.argument_expression_origin expression
         = origin)
       expressions)

let tests =
  [
    Alcotest.test_case "fixed defaults and sparse slots" `Quick
      fixed_defaults_and_sparse_slots;
    Alcotest.test_case "active header and canonical identity" `Quick
      active_header_and_canonical_identity;
    Alcotest.test_case "parenthesis-free oracle evidence" `Quick
      parenthesis_free_and_oracle_evidence;
    Alcotest.test_case "variadic slots and shape errors" `Quick
      variadic_slots_and_shape_errors;
    Alcotest.test_case "typed function-pointer calls" `Quick
      typed_function_pointer_calls_resolve_indirectly;
    Alcotest.test_case "indirect defaults, holes, and varargs" `Quick
      indirect_headers_bind_defaults_holes_and_varargs;
    Alcotest.test_case "indexed callback arrays" `Quick
      indexed_callback_arrays_resolve_indirectly;
    Alcotest.test_case "invalid indexed callback arrays" `Quick
      invalid_indexed_callback_arrays_report_rank_and_shape;
    Alcotest.test_case "generated and included provenance" `Quick
      generated_and_included_call_provenance;
    Alcotest.test_case "invalid inputs, determinism, and purity" `Quick
      invalid_inputs_are_stable_and_pure;
    Alcotest.test_case "named cast target identity validation" `Quick
      named_cast_targets_validate_source_identity;
    Alcotest.test_case "index expression constructor validation" `Quick
      index_expression_constructors_validate_bracket_origins;
    Alcotest.test_case "expression statement constructor validation" `Quick
      expression_statement_constructors_validate_identity_and_origin;
    Alcotest.test_case "implicit output constructor validation" `Quick
      implicit_output_constructors_validate_source_shape;
    Alcotest.test_case "switch selector constructor validation" `Quick
      switch_selector_constructors_validate_identity_and_origins;
    Alcotest.test_case "switch case constructor validation" `Quick
      switch_case_constructors_validate_patterns_and_origins;
    Alcotest.test_case "typed literal constructor payloads" `Quick
      literal_constructors_retain_typed_payloads;
  ]
