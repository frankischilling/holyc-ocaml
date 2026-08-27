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

let resolve ?outer prepared =
  Holyc_lib.resolve_function_calls prepared.session
    ~declarations:prepared.declarations ~function_types:prepared.function_types
    ~local_types:prepared.local_types ~global_types:prepared.global_types
    ~functions:prepared.functions ~expressions:prepared.module_expressions
    ?outer prepared.ast

let outer_environment prepared records =
  let table = Session.semantic_symbols prepared.session in
  let semantic_kind = function
    | Semantic_outer_environment.Aggregate -> Semantic_symbol.Aggregate_type
    | Semantic_outer_environment.Function -> Semantic_symbol.Function
    | Semantic_outer_environment.Global_variable ->
        Semantic_symbol.Global_variable
    | Semantic_outer_environment.Export_system_symbol ->
        Semantic_symbol.Assembler_symbol
  in
  let entries =
    records
    |> List.mapi (fun entry_index (name, record_kind) ->
        let symbol =
          Semantic_symbol_table.add table
            ~scope:(Semantic_symbol_table.root table)
            ~name
            ~kind:(semantic_kind record_kind)
            ~origin:(Semantic_symbol.Synthesized ("outer fixture " ^ name))
          |> checked
        in
        Semantic_outer_environment.make_entry ~symbol ~record_kind ~entry_index
        |> function
        | Ok entry -> entry
        | Error error ->
            Alcotest.fail (Semantic_outer_environment.error_to_string error))
  in
  let table_kind =
    match prepared.mode with
    | Preprocessor.Jit -> Semantic_outer_environment.Jit_task 0
    | Preprocessor.Aot -> Semantic_outer_environment.Aot_parent 0
  in
  let outer_table =
    Semantic_outer_environment.make_table ~table_kind ~table_index:0 entries
    |> function
    | Ok table -> table
    | Error error ->
        Alcotest.fail (Semantic_outer_environment.error_to_string error)
  in
  let assembler =
    Semantic_outer_environment.make_table
      ~table_kind:Semantic_outer_environment.Assembler ~table_index:1 []
    |> function
    | Ok table -> table
    | Error error ->
        Alcotest.fail (Semantic_outer_environment.error_to_string error)
  in
  Holyc_lib.create_outer_environment prepared.session
    ~compilation_mode:prepared.mode [ outer_table; assembler ]
  |> checked

let outer_expressions prepared records =
  let environment = outer_environment prepared records in
  Holyc_lib.resolve_outer_expressions prepared.session ~environment
    ~expressions:prepared.module_expressions
  |> checked

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

let rec defined_expressions expression =
  let open Semantic_function_call_resolution in
  match argument_expression_kind expression with
  | Defined_expression defined -> [ (expression, defined) ]
  | Parenthesized_expression grouped -> defined_expressions grouped
  | Prefix_expression prefix -> defined_expressions (prefix_operand prefix)
  | Postfix_expression postfix -> defined_expressions (postfix_operand postfix)
  | Postfix_cast_expression (operand, _) -> defined_expressions operand
  | Binary_expression binary ->
      defined_expressions (binary_left binary)
      @ defined_expressions (binary_right binary)
  | Index_expression index ->
      defined_expressions (index_base index)
      @ defined_expressions (index_value index)
  | Member_access_expression member -> defined_expressions (member_base member)
  | Integer_literal _
  | Float_literal _
  | Character_literal _
  | String_literal _
  | Bound_identifier_expression _
  | Top_level_bound_identifier_expression _
  | Sizeof_expression _
  | Unresolved_expression _ -> []

let rec sizeof_expressions expression =
  let open Semantic_function_call_resolution in
  match argument_expression_kind expression with
  | Sizeof_expression sizeof -> [ (expression, sizeof) ]
  | Parenthesized_expression grouped -> sizeof_expressions grouped
  | Prefix_expression prefix -> sizeof_expressions (prefix_operand prefix)
  | Postfix_expression postfix -> sizeof_expressions (postfix_operand postfix)
  | Postfix_cast_expression (operand, _) -> sizeof_expressions operand
  | Binary_expression binary ->
      sizeof_expressions (binary_left binary)
      @ sizeof_expressions (binary_right binary)
  | Index_expression index ->
      sizeof_expressions (index_base index)
      @ sizeof_expressions (index_value index)
  | Member_access_expression member -> sizeof_expressions (member_base member)
  | Integer_literal _
  | Float_literal _
  | Character_literal _
  | String_literal _
  | Bound_identifier_expression _
  | Top_level_bound_identifier_expression _
  | Defined_expression _
  | Unresolved_expression _ -> []

let sizeof_resolution_fact (_, sizeof) =
  let open Semantic_function_call_resolution in
  match sizeof_root_resolution sizeof with
  | Sizeof_top_level_query _ -> "top-level-query"
  | Sizeof_function_query (Module_query query) -> (
      match Semantic_module_expression_binding.query_resolution query with
      | Semantic_module_expression_binding.Local_binding binding ->
          "local:"
          ^ Semantic_symbol.name
              (binding : Semantic_function_binding_index.binding).symbol
      | Semantic_module_expression_binding.Module_binding publication ->
          "module:"
          ^ (publication |> Semantic_module_expression_binding.publication_kind
           |> Semantic_module_expression_binding.publication_kind_name)
          ^ ":"
          ^ (publication
           |> Semantic_module_expression_binding.publication_source_symbol
           |> Semantic_symbol.name)
      | Semantic_module_expression_binding.Outer_candidate -> "outer-candidate")
  | Sizeof_function_query (Outer_query query) -> (
      match Semantic_outer_expression_binding.query_resolution query with
      | Semantic_outer_expression_binding.Query_undefined -> "outer:undefined"
      | Semantic_outer_expression_binding.Query_binding
          (Semantic_outer_expression_binding.Local_binding binding) ->
          "local:"
          ^ Semantic_symbol.name
              (binding : Semantic_function_binding_index.binding).symbol
      | Semantic_outer_expression_binding.Query_binding
          (Semantic_outer_expression_binding.Module_binding publication) ->
          "module:"
          ^ (publication |> Semantic_module_expression_binding.publication_kind
           |> Semantic_module_expression_binding.publication_kind_name)
          ^ ":"
          ^ (publication
           |> Semantic_module_expression_binding.publication_source_symbol
           |> Semantic_symbol.name)
      | Semantic_outer_expression_binding.Query_binding
          (Semantic_outer_expression_binding.Outer_binding binding) ->
          "outer:"
          ^ (binding |> Semantic_outer_environment.binding_entry
           |> Semantic_outer_environment.entry_symbol |> Semantic_symbol.name))

let defined_fact (_, defined) =
  let open Semantic_function_call_resolution in
  let kind =
    match defined_operand_kind defined with
    | Defined_name -> "name"
    | Defined_non_name -> "non-name"
  in
  kind ^ ":" ^ defined_operand_spelling defined

let defined_resolution_fact (_, defined) =
  let open Semantic_function_call_resolution in
  let spelling = defined_operand_spelling defined in
  let resolution =
    match defined_operand_resolution defined with
    | Defined_non_name_false -> "non-name-false"
    | Defined_top_level_name -> "top-level-deferred"
    | Defined_top_level_query _ -> "top-level-query"
    | Defined_function_query (Module_query query) -> (
        match Semantic_module_expression_binding.query_resolution query with
        | Semantic_module_expression_binding.Outer_candidate ->
            "nonlocal-deferred"
        | Semantic_module_expression_binding.Local_binding binding ->
            "function:"
            ^ Semantic_symbol.name
                (binding : Semantic_function_binding_index.binding).symbol
        | Semantic_module_expression_binding.Module_binding publication ->
            "module:"
            ^ (publication
             |> Semantic_module_expression_binding.publication_kind
             |> Semantic_module_expression_binding.publication_kind_name)
            ^ ":"
            ^ (publication
             |> Semantic_module_expression_binding.publication_source_symbol
             |> Semantic_symbol.name))
    | Defined_function_query (Outer_query query) -> (
        match Semantic_outer_expression_binding.query_resolution query with
        | Semantic_outer_expression_binding.Query_undefined -> "outer:undefined"
        | Semantic_outer_expression_binding.Query_binding
            (Semantic_outer_expression_binding.Local_binding binding) ->
            "function:"
            ^ Semantic_symbol.name
                (binding : Semantic_function_binding_index.binding).symbol
        | Semantic_outer_expression_binding.Query_binding
            (Semantic_outer_expression_binding.Module_binding publication) ->
            "module:"
            ^ (publication
             |> Semantic_module_expression_binding.publication_kind
             |> Semantic_module_expression_binding.publication_kind_name)
            ^ ":"
            ^ (publication
             |> Semantic_module_expression_binding.publication_source_symbol
             |> Semantic_symbol.name)
        | Semantic_outer_expression_binding.Query_binding
            (Semantic_outer_expression_binding.Outer_binding binding) ->
            "outer:"
            ^ (binding |> Semantic_outer_environment.binding_entry
             |> Semantic_outer_environment.entry_symbol |> Semantic_symbol.name
              ))
  in
  let known =
    match defined_known_value defined with
    | None -> "deferred"
    | Some true -> "true"
    | Some false -> "false"
  in
  Printf.sprintf "%s:%s:%s" spelling resolution known

let expression_slice source expression =
  let location =
    expression |> Semantic_function_call_resolution.argument_expression_origin
    |> source_location
  in
  let span = location.span in
  String.sub source span.start (span.stop - span.start)

let origin_slice source origin =
  let span = (source_location origin).span in
  String.sub source span.start (span.stop - span.start)

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

let defined_operands_survive_function_expression_shapes () =
  let source =
    "extern I64 Use(I64 value);\n\
     I64 Caller(I64 local){\n\
     ((defined(((local)))));\n\
     -defined(+);\n\
     defined(left)+defined(right);\n\
     defined(cast)(I64);\n\
     Use(defined(argument));\n\
     if(defined(condition)) return defined(result);\n\
     return 0;\n\
     }"
  in
  List.iter
    (fun mode ->
      let prepared =
        prepare ~mode ~path:"function-defined-retention.HC" source
      in
      let result = resolve prepared |> checked in
      let caller = function_named result "Caller" in
      let statement_terms =
        caller
        |> Semantic_function_call_resolution.function_expression_statements
        |> List.concat_map (fun statement ->
            statement
            |> Semantic_function_call_resolution.expression_statement_expression
            |> defined_expressions)
      in
      Alcotest.(check (list string))
        "statement expression shapes retain defined operands"
        [ "name:local"; "non-name:+"; "name:left"; "name:right"; "name:cast" ]
        (List.map defined_fact statement_terms);
      let call_terms =
        caller |> Semantic_function_call_resolution.function_calls
        |> List.concat_map (fun resolution ->
            let call =
              match resolution with
              | Semantic_function_call_resolution.Direct_call direct ->
                  Semantic_function_call_resolution.direct_source direct
              | Semantic_function_call_resolution.Indirect_call indirect ->
                  Semantic_function_call_resolution.indirect_source indirect
              | Semantic_function_call_resolution.Deferred_call { call; _ } ->
                  call
            in
            call |> Semantic_function_call_resolution.call_arguments
            |> List.concat_map (fun argument ->
                argument
                |> Semantic_function_call_resolution.argument_expression
                |> Option.to_list
                |> List.concat_map defined_expressions))
      in
      Alcotest.(check (list string))
        "call argument retains its defined operand" [ "name:argument" ]
        (List.map defined_fact call_terms);
      let condition_terms =
        caller |> Semantic_function_call_resolution.function_conditions
        |> List.concat_map (fun condition ->
            condition |> Semantic_function_call_resolution.condition_expression
            |> defined_expressions)
      in
      Alcotest.(check (list string))
        "condition retains its defined operand" [ "name:condition" ]
        (List.map defined_fact condition_terms);
      let return_terms =
        caller |> Semantic_function_call_resolution.function_returns
        |> List.concat_map (fun return_ ->
            return_ |> Semantic_function_call_resolution.return_expression
            |> Option.to_list
            |> List.concat_map defined_expressions)
      in
      Alcotest.(check (list string))
        "return retains its defined operand" [ "name:result" ]
        (List.map defined_fact return_terms);
      let wrapped_expression, wrapped = List.hd statement_terms in
      Alcotest.(check string)
        "multiply wrapped defined expression keeps its complete origin"
        "defined(((local)))"
        (expression_slice source wrapped_expression);
      Alcotest.(check string)
        "multiply wrapped defined expression keeps its exact operand" "local"
        (let span =
           wrapped |> Semantic_function_call_resolution.defined_operand_origin
           |> source_location
           |> fun location -> location.span
         in
         String.sub source span.start (span.stop - span.start)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let sizeof_inputs_survive_function_expression_contexts () =
  let source =
    "I64 Root;extern I64 Use(I64 value);\n\
     I64 Caller(I64 local){\n\
     sizeof(((local)));\n\
     -sizeof(Root.leaf.value);\n\
     Use(sizeof(Root**));\n\
     if(sizeof(local)) return sizeof(Root.leaf);\n\
     switch(sizeof(local)){case sizeof(Root):return 0;}\n\
     return 0;\n\
     }"
  in
  List.iter
    (fun mode ->
      let prepared = prepare ~mode ~path:"function-sizeof-inputs.HC" source in
      let table = Session.semantic_symbols prepared.session in
      let before = Semantic_symbol_table.all_symbols table |> List.length in
      let inspect () =
        let caller =
          resolve prepared |> checked |> fun result ->
          function_named result "Caller"
        in
        let statement_terms =
          caller
          |> Semantic_function_call_resolution.function_expression_statements
          |> List.concat_map (fun statement ->
              statement
              |> Semantic_function_call_resolution
                 .expression_statement_expression |> sizeof_expressions)
        in
        let call_terms =
          caller |> Semantic_function_call_resolution.function_calls
          |> List.concat_map (fun resolution ->
              let call =
                match resolution with
                | Semantic_function_call_resolution.Direct_call direct ->
                    Semantic_function_call_resolution.direct_source direct
                | Semantic_function_call_resolution.Indirect_call indirect ->
                    Semantic_function_call_resolution.indirect_source indirect
                | Semantic_function_call_resolution.Deferred_call { call; _ } ->
                    call
              in
              call |> Semantic_function_call_resolution.call_arguments
              |> List.concat_map (fun argument ->
                  argument
                  |> Semantic_function_call_resolution.argument_expression
                  |> Option.to_list
                  |> List.concat_map sizeof_expressions))
        in
        let condition_terms =
          caller |> Semantic_function_call_resolution.function_conditions
          |> List.concat_map (fun condition ->
              condition
              |> Semantic_function_call_resolution.condition_expression
              |> sizeof_expressions)
        in
        let selector_terms =
          caller |> Semantic_function_call_resolution.function_selectors
          |> List.concat_map (fun selector ->
              selector |> Semantic_function_call_resolution.selector_expression
              |> sizeof_expressions)
        in
        let case_terms =
          caller |> Semantic_function_call_resolution.function_switch_cases
          |> List.concat_map (fun case ->
              match
                Semantic_function_call_resolution.switch_case_pattern case
              with
              | Semantic_function_call_resolution.Implicit_case -> []
              | Semantic_function_call_resolution.Single_case expression ->
                  sizeof_expressions expression
              | Semantic_function_call_resolution.Ranged_case
                  { start_expression; end_expression; _ } ->
                  sizeof_expressions start_expression
                  @ sizeof_expressions end_expression)
        in
        let return_terms =
          caller |> Semantic_function_call_resolution.function_returns
          |> List.concat_map (fun return_ ->
              return_ |> Semantic_function_call_resolution.return_expression
              |> Option.to_list
              |> List.concat_map sizeof_expressions)
        in
        let terms =
          statement_terms @ call_terms @ condition_terms @ selector_terms
          @ case_terms @ return_terms
        in
        Alcotest.(check (list string))
          "every function expression context retains the sizeof target"
          [ "local"; "Root"; "Root"; "local"; "local"; "Root"; "Root" ]
          (terms
          |> List.map (fun (_, sizeof) ->
              Semantic_function_call_resolution.sizeof_target_spelling sizeof));
        Alcotest.(check (list (list string)))
          "repeated sizeof member traversal is source ordered"
          [ []; [ "leaf"; "value" ]; []; []; []; []; [ "leaf" ] ]
          (terms
          |> List.map (fun (_, sizeof) ->
              sizeof |> Semantic_function_call_resolution.sizeof_members
              |> List.map Semantic_function_call_resolution.sizeof_member_name)
          );
        Alcotest.(check (list (list int)))
          "pointer layers keep contiguous source depths"
          [ []; []; [ 1; 2 ]; []; []; []; [] ]
          (terms
          |> List.map (fun (_, sizeof) ->
              sizeof |> Semantic_function_call_resolution.sizeof_pointer_layers
              |> List.map Semantic_function_call_resolution.sizeof_pointer_depth)
          );
        Alcotest.(check (list (pair int int)))
          "wrapper parentheses remain balanced and ordered"
          [ (3, 3); (1, 1); (1, 1); (1, 1); (1, 1); (1, 1); (1, 1) ]
          (terms
          |> List.map (fun (_, sizeof) ->
              ( sizeof
                |> Semantic_function_call_resolution.sizeof_opening_origins
                |> List.length,
                sizeof
                |> Semantic_function_call_resolution.sizeof_closing_origins
                |> List.length )));
        Alcotest.(check (list string))
          "sizeof roots retain their source-ordered function queries"
          [
            "local:local";
            "module:global-variable:Root";
            "module:global-variable:Root";
            "local:local";
            "local:local";
            "module:global-variable:Root";
            "module:global-variable:Root";
          ]
          (List.map sizeof_resolution_fact terms);
        let _, nested = List.nth statement_terms 1 in
        Alcotest.(check string)
          "keyword origin" "sizeof"
          (nested |> Semantic_function_call_resolution.sizeof_keyword_origin
         |> origin_slice source);
        Alcotest.(check string)
          "target origin" "Root"
          (nested |> Semantic_function_call_resolution.sizeof_target_origin
         |> origin_slice source);
        Alcotest.(check (list string))
          "member name origins" [ "leaf"; "value" ]
          (nested |> Semantic_function_call_resolution.sizeof_members
          |> List.map (fun member ->
              member
              |> Semantic_function_call_resolution.sizeof_member_name_origin
              |> origin_slice source));
        let _, pointer = List.hd call_terms in
        Alcotest.(check (list string))
          "pointer layer origins" [ "*"; "*" ]
          (pointer |> Semantic_function_call_resolution.sizeof_pointer_layers
          |> List.map (fun layer ->
              layer |> Semantic_function_call_resolution.sizeof_pointer_origin
              |> origin_slice source));
        terms
        |> List.map (fun (expression, sizeof) ->
            ( expression_slice source expression,
              Semantic_function_call_resolution.sizeof_keyword_spelling sizeof
            ))
      in
      let first = inspect () in
      let middle = Semantic_symbol_table.all_symbols table |> List.length in
      let second = inspect () in
      let after = Semantic_symbol_table.all_symbols table |> List.length in
      Alcotest.(check (list (pair string string)))
        "repeated sizeof retention is deterministic" first second;
      Alcotest.(check (pair int int))
        "sizeof retention does not mutate the symbol table" (before, before)
        (middle, after))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let sizeof_generated_provenance_is_deterministic_and_pure () =
  let prepared =
    prepare ~path:"function-sizeof-generated.HC"
      "#define SIZE sizeof(((Generated.member **)))\n\
       I64 Caller(){SIZE;return 0;}"
  in
  let table = Session.semantic_symbols prepared.session in
  let before = Semantic_symbol_table.all_symbols table |> List.length in
  let inspect () =
    let expression, sizeof =
      resolve prepared |> checked |> fun result ->
      function_named result "Caller"
      |> Semantic_function_call_resolution.function_expression_statements
      |> List.hd
      |> Semantic_function_call_resolution.expression_statement_expression
      |> sizeof_expressions |> List.hd
    in
    let member =
      sizeof |> Semantic_function_call_resolution.sizeof_members |> List.hd
    in
    let origins =
      [
        Semantic_function_call_resolution.argument_expression_origin expression;
        Semantic_function_call_resolution.sizeof_keyword_origin sizeof;
        Semantic_function_call_resolution.sizeof_target_origin sizeof;
        Semantic_function_call_resolution.sizeof_member_dot_origin member;
        Semantic_function_call_resolution.sizeof_member_name_origin member;
        Semantic_function_call_resolution.sizeof_member_origin member;
      ]
      @ Semantic_function_call_resolution.sizeof_opening_origins sizeof
      @ (sizeof |> Semantic_function_call_resolution.sizeof_pointer_layers
        |> List.map Semantic_function_call_resolution.sizeof_pointer_origin)
      @ Semantic_function_call_resolution.sizeof_closing_origins sizeof
    in
    Alcotest.(check bool)
      "every generated sizeof component keeps invocation provenance" true
      (origins
      |> List.for_all (fun origin ->
          origin |> source_location |> fun location ->
          Option.is_some location.generated_from));
    Alcotest.(check bool)
      "every generated sizeof component keeps definition provenance" true
      (origins
      |> List.for_all (fun origin ->
          origin |> source_location |> fun location ->
          Option.is_some location.defined_at));
    ( Semantic_function_call_resolution.sizeof_target_spelling sizeof,
      sizeof |> Semantic_function_call_resolution.sizeof_members
      |> List.map Semantic_function_call_resolution.sizeof_member_name,
      sizeof |> Semantic_function_call_resolution.sizeof_pointer_layers
      |> List.map Semantic_function_call_resolution.sizeof_pointer_depth )
  in
  let first = inspect () in
  let middle = Semantic_symbol_table.all_symbols table |> List.length in
  let second = inspect () in
  let after = Semantic_symbol_table.all_symbols table |> List.length in
  Alcotest.(check (triple string (list string) (list int)))
    "generated sizeof evidence is deterministic" first second;
  Alcotest.(check (pair int int))
    "generated sizeof retention does not mutate symbols" (before, before)
    (middle, after)

let sizeof_constructors_validate_source_and_query_evidence () =
  let open Semantic_function_call_resolution in
  let expect_error label expected = function
    | Ok _ -> Alcotest.failf "%s: expected rejection" label
    | Error actual -> Alcotest.(check string) label expected actual
  in
  let prepared =
    prepare ~path:"sizeof-constructor-query.HC"
      "I64 Caller(I64 local){sizeof(local);defined(local);return 0;}"
  in
  let queries =
    prepared.module_expressions |> Semantic_module_expression_binding.functions
    |> List.hd |> Semantic_module_expression_binding.function_queries
  in
  let sizeof_query = List.nth queries 0 in
  let defined_query = List.nth queries 1 in
  let target_origin =
    Semantic_module_expression_binding.query_origin sizeof_query
  in
  let source_origin = Semantic_symbol.Synthesized "sizeof source token" in
  let member =
    make_sizeof_member ~dot_origin:source_origin ~name:"field"
      ~name_origin:source_origin ~origin:source_origin
    |> checked
  in
  let pointer =
    make_sizeof_pointer_layer ~depth:1 ~spelling:"*" ~origin:source_origin
    |> checked
  in
  let make ?(opening_origins = [ source_origin ])
      ?(closing_origins = [ source_origin ]) ?(members = [ member ])
      ?(pointer_layers = [ pointer ]) ?(target_spelling = "local")
      ?(target_origin = target_origin)
      ?(root_resolution = Sizeof_function_query (Module_query sizeof_query)) ()
      =
    make_sizeof_argument_expression ~keyword_spelling:"sizeof"
      ~keyword_origin:source_origin ~opening_origins ~target_spelling
      ~target_origin ~members ~pointer_layers ~closing_origins ~root_resolution
  in
  (match make () |> checked with
  | Sizeof_expression sizeof ->
      Alcotest.(check string)
        "checked constructor retains its keyword" "sizeof"
        (sizeof_keyword_spelling sizeof);
      Alcotest.(check string)
        "checked constructor retains its member" "field"
        (sizeof_members sizeof |> List.hd |> sizeof_member_name);
      Alcotest.(check int)
        "checked constructor retains its pointer depth" 1
        (sizeof_pointer_layers sizeof |> List.hd |> sizeof_pointer_depth)
  | _ -> Alcotest.fail "expected checked sizeof evidence");
  expect_error "unbalanced wrappers" "sizeof wrapper parentheses are unbalanced"
    (make ~closing_origins:[] ());
  let second_pointer =
    make_sizeof_pointer_layer ~depth:2 ~spelling:"*" ~origin:source_origin
    |> checked
  in
  expect_error "noncontiguous pointers"
    "sizeof pointer layers are not contiguous"
    (make ~pointer_layers:[ second_pointer ] ());
  expect_error "wrong query role"
    "sizeof target query has the wrong semantic role"
    (make
       ~target_origin:
         (Semantic_module_expression_binding.query_origin defined_query)
       ~root_resolution:(Sizeof_function_query (Module_query defined_query)) ());
  expect_error "query spelling mismatch"
    "sizeof target spelling does not match its query"
    (make ~target_spelling:"other" ());
  expect_error "query origin mismatch"
    "sizeof target origin does not match its query"
    (make ~target_origin:(Semantic_symbol.Synthesized "different target") ());
  expect_error "empty member name" "sizeof member name cannot be empty"
    (make_sizeof_member ~dot_origin:source_origin ~name:""
       ~name_origin:source_origin ~origin:source_origin);
  expect_error "zero pointer depth" "sizeof pointer depth must be positive"
    (make_sizeof_pointer_layer ~depth:0 ~spelling:"*" ~origin:source_origin)

let sizeof_query_must_belong_to_its_function () =
  let prepared =
    prepare ~path:"sizeof-query-owner.HC"
      "I64 Target;I64 Caller(){sizeof(Target);return 0;}"
  in
  let target =
    resolve prepared |> checked |> fun result -> function_named result "Caller"
  in
  let foreign_source =
    Session.add_source prepared.session ~path:"sizeof-query-foreign.HC"
      ~contents:
        "I64 ForeignTarget;I64 Foreign(){sizeof(ForeignTarget);return 0;}"
  in
  let foreign_ast =
    Holyc_lib.parse_with_config prepared.session ~config:(config prepared.mode)
      ~source:foreign_source
    |> expect_ast
  in
  let foreign = finish_prepare prepared.mode prepared.session foreign_ast in
  let foreign_query =
    foreign.module_expressions |> Semantic_module_expression_binding.functions
    |> List.hd |> Semantic_module_expression_binding.function_queries |> List.hd
  in
  let target_origin =
    Semantic_module_expression_binding.query_origin foreign_query
  in
  let expression_kind =
    Semantic_function_call_resolution.make_sizeof_argument_expression
      ~keyword_spelling:"sizeof"
      ~keyword_origin:(Semantic_symbol.Synthesized "foreign sizeof keyword")
      ~opening_origins:[]
      ~target_spelling:
        (Semantic_module_expression_binding.query_name foreign_query)
      ~target_origin ~members:[] ~pointer_layers:[] ~closing_origins:[]
      ~root_resolution:
        (Semantic_function_call_resolution.Sizeof_function_query
           (Semantic_function_call_resolution.Module_query foreign_query))
    |> checked
  in
  let expression =
    Semantic_function_call_resolution.make_argument_expression
      ~kind:expression_kind ~origin:target_origin
  in
  let statement =
    Semantic_function_call_resolution.make_expression_statement ~index:0
      ~expression ~origin:target_origin
    |> checked
  in
  let input =
    Semantic_function_call_resolution.make_function
      ~symbol:(Semantic_function_call_resolution.function_symbol target)
      ~scope:(Semantic_function_call_resolution.function_scope target)
      ~item_index:(Semantic_function_call_resolution.function_item_index target)
      ~expression_statements:[ statement ] []
    |> checked
  in
  match
    Semantic_function_call_resolution.resolve
      ~table:(Session.semantic_symbols prepared.session)
      ~parent:(Semantic_declaration_collection.scope prepared.declarations)
      ~function_types:prepared.function_types ~functions:prepared.functions
      ~expressions:prepared.module_expressions [ input ]
  with
  | Ok _ -> Alcotest.fail "a function accepted another function's sizeof query"
  | Error error ->
      Alcotest.(check string)
        "foreign sizeof query code" "HCSEMA0039"
        (Semantic_function_call_resolution.error_code error);
      Alcotest.(check string)
        "foreign sizeof query message"
        "sizeof target uses a different function query"
        (Semantic_function_call_resolution.error_message error)

let defined_values_follow_source_local_visibility () =
  let source =
    "I64 Caller(I64 parameter,...){\n\
     defined(parameter);\n\
     defined(argc);\n\
     defined(argv);\n\
     defined(later);\n\
     I64 later;\n\
     defined(later);\n\
     defined(+);\n\
     defined(missing);\n\
     return 0;\n\
     }"
  in
  List.iter
    (fun mode ->
      let prepared = prepare ~mode ~path:"function-defined-values.HC" source in
      let result = resolve prepared |> checked in
      let caller = function_named result "Caller" in
      let terms =
        caller
        |> Semantic_function_call_resolution.function_expression_statements
        |> List.concat_map (fun statement ->
            statement
            |> Semantic_function_call_resolution.expression_statement_expression
            |> defined_expressions)
      in
      Alcotest.(check (list string))
        "defined follows the function-local source order"
        [
          "parameter:function:parameter:true";
          "argc:function:argc:true";
          "argv:function:argv:true";
          "later:nonlocal-deferred:deferred";
          "later:function:later:true";
          "+:non-name-false:false";
          "missing:nonlocal-deferred:deferred";
        ]
        (List.map defined_resolution_fact terms);
      let expected_queries =
        prepared.module_expressions
        |> Semantic_module_expression_binding.functions
        |> List.find (fun function_ ->
            function_ |> Semantic_module_expression_binding.function_symbol
            |> Semantic_symbol.name |> String.equal "Caller")
        |> Semantic_module_expression_binding.function_queries
      in
      Alcotest.(check bool)
        "every name operand keeps the exact query owned by Caller" true
        (terms
        |> List.for_all (fun (_, defined) ->
            match
              Semantic_function_call_resolution.defined_operand_resolution
                defined
            with
            | Semantic_function_call_resolution.Defined_function_query
                (Semantic_function_call_resolution.Module_query query) ->
                List.exists (( == ) query) expected_queries
            | Semantic_function_call_resolution.Defined_function_query
                (Semantic_function_call_resolution.Outer_query _) -> false
            | Semantic_function_call_resolution.Defined_non_name_false -> true
            | Semantic_function_call_resolution.Defined_top_level_query _ ->
                false
            | Semantic_function_call_resolution.Defined_top_level_name -> false)
        ))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let defined_values_follow_module_visibility () =
  let source =
    "extern class PriorType;\n\
     I64 PriorGlobal;\n\
     I64 PriorFunction(){return 0;}\n\
     I64 Shared;\n\
     I64 Caller(){\n\
     defined(PriorType);defined(PriorGlobal);defined(PriorFunction);\n\
     defined(Caller);defined(Shared);I64 Shared;defined(Shared);\n\
     defined(Later);return 0;}\n\
     I64 Later;"
  in
  List.iter
    (fun mode ->
      let prepared = prepare ~mode ~path:"function-defined-module.HC" source in
      let result = resolve prepared |> checked in
      let caller = function_named result "Caller" in
      let terms =
        caller
        |> Semantic_function_call_resolution.function_expression_statements
        |> List.concat_map (fun statement ->
            statement
            |> Semantic_function_call_resolution.expression_statement_expression
            |> defined_expressions)
      in
      Alcotest.(check (list string))
        "defined follows the module publication prefix"
        [
          "PriorType:module:aggregate:PriorType:true";
          "PriorGlobal:module:global-variable:PriorGlobal:true";
          "PriorFunction:module:function:PriorFunction:true";
          "Caller:module:function:Caller:true";
          "Shared:module:global-variable:Shared:true";
          "Shared:function:Shared:true";
          "Later:nonlocal-deferred:deferred";
        ]
        (List.map defined_resolution_fact terms);
      let publications =
        Semantic_module_expression_binding.publications
          prepared.module_expressions
      in
      Alcotest.(check bool)
        "every module result keeps an owned publication" true
        (terms
        |> List.for_all (fun (_, defined) ->
            match
              Semantic_function_call_resolution.defined_operand_resolution
                defined
            with
            | Semantic_function_call_resolution.Defined_function_query
                (Semantic_function_call_resolution.Module_query query) -> (
                match
                  Semantic_module_expression_binding.query_resolution query
                with
                | Semantic_module_expression_binding.Module_binding publication
                  -> List.exists (( == ) publication) publications
                | Semantic_module_expression_binding.Local_binding _
                | Semantic_module_expression_binding.Outer_candidate -> true)
            | Semantic_function_call_resolution.Defined_function_query
                (Semantic_function_call_resolution.Outer_query _) -> false
            | Semantic_function_call_resolution.Defined_non_name_false -> true
            | Semantic_function_call_resolution.Defined_top_level_query _ ->
                false
            | Semantic_function_call_resolution.Defined_top_level_name -> false)
        ))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let defined_values_follow_complete_outer_lookup () =
  let source =
    "I64 ModuleValue;I64 Caller(I64 LocalValue){\n\
     defined(LocalValue);defined(ModuleValue);defined(OuterValue);\n\
     defined(Missing);defined(+);return 0;}"
  in
  List.iter
    (fun mode ->
      let prepared = prepare ~mode ~path:"function-defined-outer.HC" source in
      let unresolved = resolve prepared |> checked in
      let unresolved_terms =
        function_named unresolved "Caller"
        |> Semantic_function_call_resolution.function_expression_statements
        |> List.concat_map (fun statement ->
            statement
            |> Semantic_function_call_resolution.expression_statement_expression
            |> defined_expressions)
      in
      Alcotest.(check (list string))
        "without an outer snapshot, absent module names stay deferred"
        [
          "LocalValue:function:LocalValue:true";
          "ModuleValue:module:global-variable:ModuleValue:true";
          "OuterValue:nonlocal-deferred:deferred";
          "Missing:nonlocal-deferred:deferred";
          "+:non-name-false:false";
        ]
        (List.map defined_resolution_fact unresolved_terms);
      let outer =
        outer_expressions prepared
          [
            ("LocalValue", Semantic_outer_environment.Global_variable);
            ("ModuleValue", Semantic_outer_environment.Global_variable);
            ("OuterValue", Semantic_outer_environment.Global_variable);
          ]
      in
      let result = resolve ~outer prepared |> checked in
      let terms =
        function_named result "Caller"
        |> Semantic_function_call_resolution.function_expression_statements
        |> List.concat_map (fun statement ->
            statement
            |> Semantic_function_call_resolution.expression_statement_expression
            |> defined_expressions)
      in
      Alcotest.(check (list string))
        "the complete outer chain proves hits and misses"
        [
          "LocalValue:function:LocalValue:true";
          "ModuleValue:module:global-variable:ModuleValue:true";
          "OuterValue:outer:OuterValue:true";
          "Missing:outer:undefined:false";
          "+:non-name-false:false";
        ]
        (List.map defined_resolution_fact terms);
      let caller_queries =
        outer |> Semantic_outer_expression_binding.functions
        |> List.find (fun function_ ->
            function_ |> Semantic_outer_expression_binding.function_symbol
            |> Semantic_symbol.name |> String.equal "Caller")
        |> Semantic_outer_expression_binding.function_queries
      in
      Alcotest.(check bool)
        "every name operand keeps the exact outer query owned by Caller" true
        (terms
        |> List.for_all (fun (_, defined) ->
            match
              Semantic_function_call_resolution.defined_operand_resolution
                defined
            with
            | Semantic_function_call_resolution.Defined_function_query
                (Semantic_function_call_resolution.Outer_query query) ->
                List.exists (( == ) query) caller_queries
            | Semantic_function_call_resolution.Defined_non_name_false -> true
            | Semantic_function_call_resolution.Defined_top_level_query _ ->
                false
            | Semantic_function_call_resolution.Defined_function_query
                (Semantic_function_call_resolution.Module_query _)
            | Semantic_function_call_resolution.Defined_top_level_name -> false)
        ))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let defined_generated_provenance_is_deterministic_and_pure () =
  let prepared =
    prepare ~path:"function-defined-generated.HC"
      "#define QUERY defined(Generated)\nI64 Caller(){QUERY;}"
  in
  let table = Session.semantic_symbols prepared.session in
  let before = Semantic_symbol_table.all_symbols table |> List.length in
  let inspect () =
    let result = resolve prepared |> checked in
    let statement =
      result |> fun result ->
      function_named result "Caller"
      |> Semantic_function_call_resolution.function_expression_statements
      |> List.hd
    in
    let expression, defined =
      statement
      |> Semantic_function_call_resolution.expression_statement_expression
      |> defined_expressions |> List.hd
    in
    let expression_origin =
      expression |> Semantic_function_call_resolution.argument_expression_origin
      |> source_location
    in
    let operand_origin =
      defined |> Semantic_function_call_resolution.defined_operand_origin
      |> source_location
    in
    Alcotest.(check bool)
      "generated expression keeps its invocation" true
      (Option.is_some expression_origin.generated_from);
    Alcotest.(check bool)
      "generated expression keeps its definition" true
      (Option.is_some expression_origin.defined_at);
    Alcotest.(check bool)
      "generated operand keeps its invocation" true
      (Option.is_some operand_origin.generated_from);
    Alcotest.(check bool)
      "generated operand keeps its definition" true
      (Option.is_some operand_origin.defined_at);
    defined_fact (expression, defined)
  in
  let first = inspect () in
  let middle = Semantic_symbol_table.all_symbols table |> List.length in
  let second = inspect () in
  let after = Semantic_symbol_table.all_symbols table |> List.length in
  Alcotest.(check string)
    "repeated resolution retains the same operand" first second;
  Alcotest.(check (pair int int))
    "defined retention does not mutate the symbol table" (before, before)
    (middle, after)

let defined_constructor_validates_source_evidence () =
  let open Semantic_function_call_resolution in
  let origin = Semantic_symbol.Synthesized "defined operand" in
  let retained =
    make_defined_argument_expression ~operand_kind:Defined_non_name
      ~operand_spelling:"+" ~operand_origin:origin
      ~operand_resolution:Defined_non_name_false
    |> checked
  in
  (match retained with
  | Defined_expression defined ->
      Alcotest.(check string)
        "constructor retains the non-name kind and spelling" "non-name:+"
        (defined_fact
           (make_argument_expression ~kind:retained ~origin, defined));
      Alcotest.(check bool)
        "constructor retains the operand origin" true
        (defined_operand_origin defined = origin)
  | _ -> Alcotest.fail "expected a checked defined expression");
  Alcotest.(check (result reject string))
    "an empty operand spelling is rejected"
    (Error "defined operand spelling cannot be empty")
    (make_defined_argument_expression ~operand_kind:Defined_name
       ~operand_spelling:"" ~operand_origin:origin
       ~operand_resolution:Defined_top_level_name);
  Alcotest.(check (result reject string))
    "an invalid operand origin is rejected"
    (Error "defined operand has an invalid source origin")
    (make_defined_argument_expression ~operand_kind:Defined_name
       ~operand_spelling:"Known"
       ~operand_origin:(Semantic_symbol.Synthesized "")
       ~operand_resolution:Defined_top_level_name);
  let prepared =
    prepare ~path:"defined-constructor-query.HC"
      "I64 Caller(I64 local){sizeof(local);defined(local);return 0;}"
  in
  let queries =
    prepared.module_expressions |> Semantic_module_expression_binding.functions
    |> List.hd |> Semantic_module_expression_binding.function_queries
  in
  let sizeof_query = List.nth queries 0 in
  let defined_query = List.nth queries 1 in
  let query_origin =
    Semantic_module_expression_binding.query_origin defined_query
  in
  Alcotest.(check (result reject string))
    "a name query with another role is rejected"
    (Error "defined operand query has the wrong semantic role")
    (make_defined_argument_expression ~operand_kind:Defined_name
       ~operand_spelling:
         (Semantic_module_expression_binding.query_name sizeof_query)
       ~operand_origin:
         (Semantic_module_expression_binding.query_origin sizeof_query)
       ~operand_resolution:(Defined_function_query (Module_query sizeof_query)));
  Alcotest.(check (result reject string))
    "query spelling must match the retained operand"
    (Error "defined operand spelling does not match its query")
    (make_defined_argument_expression ~operand_kind:Defined_name
       ~operand_spelling:"other" ~operand_origin:query_origin
       ~operand_resolution:(Defined_function_query (Module_query defined_query)));
  Alcotest.(check (result reject string))
    "query origin must match the retained operand"
    (Error "defined operand origin does not match its query")
    (make_defined_argument_expression ~operand_kind:Defined_name
       ~operand_spelling:"local"
       ~operand_origin:(Semantic_symbol.Synthesized "other defined operand")
       ~operand_resolution:(Defined_function_query (Module_query defined_query)));
  Alcotest.(check (result reject string))
    "a name cannot use a non-name result"
    (Error "name-shaped defined operand cannot use a non-name result")
    (make_defined_argument_expression ~operand_kind:Defined_name
       ~operand_spelling:"local" ~operand_origin:query_origin
       ~operand_resolution:Defined_non_name_false);
  Alcotest.(check (result reject string))
    "a non-name cannot use a name query"
    (Error "non-name defined operand cannot use a name query")
    (make_defined_argument_expression ~operand_kind:Defined_non_name
       ~operand_spelling:"local" ~operand_origin:query_origin
       ~operand_resolution:(Defined_function_query (Module_query defined_query)))

let defined_query_must_belong_to_its_function () =
  let prepared =
    prepare ~path:"defined-query-owner.HC"
      "I64 TargetName;I64 Caller(){defined(TargetName);return 0;}"
  in
  let target =
    resolve prepared |> checked |> fun result -> function_named result "Caller"
  in
  let foreign_source =
    Session.add_source prepared.session ~path:"defined-query-foreign.HC"
      ~contents:"I64 ForeignName;I64 Foreign(){defined(ForeignName);return 0;}"
  in
  let foreign_ast =
    Holyc_lib.parse_with_config prepared.session ~config:(config prepared.mode)
      ~source:foreign_source
    |> expect_ast
  in
  let foreign = finish_prepare prepared.mode prepared.session foreign_ast in
  let foreign_query =
    foreign.module_expressions |> Semantic_module_expression_binding.functions
    |> List.hd |> Semantic_module_expression_binding.function_queries |> List.hd
  in
  let reject_foreign_query ?outer ~name ~operand_origin operand_resolution =
    let expression_kind =
      Semantic_function_call_resolution.make_defined_argument_expression
        ~operand_kind:Semantic_function_call_resolution.Defined_name
        ~operand_spelling:name ~operand_origin ~operand_resolution
      |> checked
    in
    let expression =
      Semantic_function_call_resolution.make_argument_expression
        ~kind:expression_kind ~origin:operand_origin
    in
    let statement =
      Semantic_function_call_resolution.make_expression_statement ~index:0
        ~expression ~origin:operand_origin
      |> checked
    in
    let input =
      Semantic_function_call_resolution.make_function
        ~symbol:(Semantic_function_call_resolution.function_symbol target)
        ~scope:(Semantic_function_call_resolution.function_scope target)
        ~item_index:
          (Semantic_function_call_resolution.function_item_index target)
        ~expression_statements:[ statement ] []
      |> checked
    in
    match
      Semantic_function_call_resolution.resolve
        ~table:(Session.semantic_symbols prepared.session)
        ~parent:(Semantic_declaration_collection.scope prepared.declarations)
        ~function_types:prepared.function_types ~functions:prepared.functions
        ~expressions:prepared.module_expressions ?outer [ input ]
    with
    | Ok _ ->
        Alcotest.fail "a function accepted another function's defined query"
    | Error error ->
        Alcotest.(check string)
          "foreign query diagnostic code" "HCSEMA0039"
          (Semantic_function_call_resolution.error_code error);
        Alcotest.(check string)
          "foreign query diagnostic message"
          "defined operand uses a different function query"
          (Semantic_function_call_resolution.error_message error)
  in
  reject_foreign_query
    ~name:(Semantic_module_expression_binding.query_name foreign_query)
    ~operand_origin:
      (Semantic_module_expression_binding.query_origin foreign_query)
    (Semantic_function_call_resolution.Defined_function_query
       (Semantic_function_call_resolution.Module_query foreign_query));
  let environment = outer_environment prepared [] in
  let target_outer =
    Holyc_lib.resolve_outer_expressions prepared.session ~environment
      ~expressions:prepared.module_expressions
    |> checked
  in
  let foreign_outer =
    Holyc_lib.resolve_outer_expressions prepared.session ~environment
      ~expressions:foreign.module_expressions
    |> checked
  in
  let foreign_outer_query =
    foreign_outer |> Semantic_outer_expression_binding.functions |> List.hd
    |> Semantic_outer_expression_binding.function_queries |> List.hd
  in
  reject_foreign_query ~outer:target_outer
    ~name:(Semantic_outer_expression_binding.query_name foreign_outer_query)
    ~operand_origin:
      (Semantic_outer_expression_binding.query_origin foreign_outer_query)
    (Semantic_function_call_resolution.Defined_function_query
       (Semantic_function_call_resolution.Outer_query foreign_outer_query))

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
    match
      Semantic_function_call_resolution.argument_expression_kind expression
    with
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
    Alcotest.test_case "defined operands across function expressions" `Quick
      defined_operands_survive_function_expression_shapes;
    Alcotest.test_case "sizeof inputs across function expressions" `Quick
      sizeof_inputs_survive_function_expression_contexts;
    Alcotest.test_case "sizeof generated provenance and purity" `Quick
      sizeof_generated_provenance_is_deterministic_and_pure;
    Alcotest.test_case "sizeof constructor source validation" `Quick
      sizeof_constructors_validate_source_and_query_evidence;
    Alcotest.test_case "sizeof module query owner validation" `Quick
      sizeof_query_must_belong_to_its_function;
    Alcotest.test_case "defined source-local values" `Quick
      defined_values_follow_source_local_visibility;
    Alcotest.test_case "defined module-visible values" `Quick
      defined_values_follow_module_visibility;
    Alcotest.test_case "defined complete outer lookup" `Quick
      defined_values_follow_complete_outer_lookup;
    Alcotest.test_case "defined generated provenance and purity" `Quick
      defined_generated_provenance_is_deterministic_and_pure;
    Alcotest.test_case "defined constructor source validation" `Quick
      defined_constructor_validates_source_evidence;
    Alcotest.test_case "defined module query owner validation" `Quick
      defined_query_must_belong_to_its_function;
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
