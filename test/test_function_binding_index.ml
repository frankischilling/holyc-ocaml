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

let config mode = checked (Preprocessor.Config.create ~compilation_mode:mode ())

let contains_substring text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec search offset =
    if offset + fragment_length > text_length then false
    else if String.sub text offset fragment_length = fragment then true
    else search (offset + 1)
  in
  search 0

let parse ?(mode = Preprocessor.Jit) session ~path contents =
  let source = Session.add_source session ~path ~contents in
  Holyc_lib.parse_with_config session ~config:(config mode) ~source
  |> expect_ast

type inputs = {
  session : Session.t;
  declarations : Semantic_declaration_collection.t;
  functions : Semantic_function_collection.t;
  function_types : Semantic_function_type_resolution.t;
  local_types : Semantic_local_type_resolution.t;
}

let inputs ?(mode = Preprocessor.Jit) ~path contents =
  let session = Session.create () in
  let ast = parse ~mode session ~path contents in
  let declarations = checked (Holyc_lib.collect_declarations session ast) in
  let aggregates =
    checked (Holyc_lib.resolve_aggregates session ~declarations ast)
  in
  let functions =
    checked (Holyc_lib.collect_functions session ~declarations ast)
  in
  let function_types =
    checked
      (Holyc_lib.resolve_function_types session ~declarations ~aggregates
         ~functions ast)
  in
  let local_types =
    checked
      (Holyc_lib.resolve_local_types session ~declarations ~aggregates
         ~functions ast)
  in
  { session; declarations; functions; function_types; local_types }

let build inputs =
  Holyc_lib.index_function_bindings inputs.session
    ~declarations:inputs.declarations ~functions:inputs.functions
    ~function_types:inputs.function_types ~local_types:inputs.local_types

let prepare ?mode ~path contents = inputs ?mode ~path contents |> build |> checked

let function_named index name =
  Semantic_function_binding_index.functions index
  |> List.find (fun function_ ->
      function_ |> Semantic_function_binding_index.function_symbol
      |> Semantic_symbol.name |> String.equal name)

let binding_names function_ =
  Semantic_function_binding_index.function_bindings function_
  |> List.map (fun (binding : Semantic_function_binding_index.binding) ->
      Semantic_symbol.name binding.symbol)

let symbol_id symbol = Semantic_symbol.id symbol |> Semantic_symbol.Id.to_int

let expect_lookup index function_ name =
  let function_symbol =
    Semantic_function_binding_index.function_symbol function_
  in
  match
    Semantic_function_binding_index.lookup index ~function_:function_symbol ~name
    |> Result.map_error Semantic_function_binding_index.error_to_string
    |> checked
  with
  | Some binding -> binding
  | None -> Alcotest.failf "expected function binding %s" name

let ordered_bindings_and_lookup () =
  let source =
    "I64 Work(I64 first,U8 *second,...){I64 automatic;static U8 stored;}"
  in
  let semantic_inputs = inputs ~path:"ordered-function-bindings.HC" source in
  let table = Session.semantic_symbols semantic_inputs.session in
  let before = Semantic_symbol_table.all_symbols table |> List.length in
  let index = build semantic_inputs |> checked in
  let after = Semantic_symbol_table.all_symbols table |> List.length in
  let function_ = function_named index "Work" in
  let bindings = Semantic_function_binding_index.function_bindings function_ in
  Alcotest.(check (list string))
    "binding insertion order"
    [ "first"; "second"; "argc"; "argv"; "automatic"; "stored" ]
    (binding_names function_);
  Alcotest.(check (list string))
    "binding kinds"
    [
      "named-parameter";
      "named-parameter";
      "variadic-argc";
      "variadic-argv";
      "automatic-local";
      "static-local";
    ]
    (List.map
       (fun (binding : Semantic_function_binding_index.binding) ->
         Semantic_function_binding_index.binding_kind_name binding.kind)
       bindings);
  Alcotest.(check (list int))
    "stable ordinals" [ 0; 1; 2; 3; 4; 5 ]
    (List.map
       (fun (binding : Semantic_function_binding_index.binding) ->
         binding.ordinal)
       bindings);
  Alcotest.(check (option int))
    "named parameter index" (Some 1)
    (expect_lookup index function_ "second").parameter_index;
  Alcotest.(check (option int))
    "local declaration index" (Some 1)
    (expect_lookup index function_ "stored").local_declaration_index;
  Alcotest.(check (option int))
    "missing lookup" None
    (Semantic_function_binding_index.lookup index
       ~function_:(Semantic_function_binding_index.function_symbol function_)
       ~name:"missing"
    |> Result.map_error Semantic_function_binding_index.error_to_string
    |> checked
    |> Option.map (fun (binding : Semantic_function_binding_index.binding) ->
        symbol_id binding.symbol));
  Alcotest.(check int) "pure index build" before after

let expect_duplicate ~path source name =
  let semantic_inputs = inputs ~path source in
  match build semantic_inputs with
  | Ok _ -> Alcotest.failf "expected duplicate binding %s" name
  | Error message ->
      Alcotest.(check bool)
        "stable duplicate code" true
        (String.starts_with ~prefix:"HCSEMA0015: " message);
      Alcotest.(check bool)
        "duplicate name" true
        (contains_substring message (Printf.sprintf "%S" name));
      Alcotest.(check bool)
        "human duplicate explanation" true
        (contains_substring message "repeats symbol")

let ordinary_duplicates_fail () =
  expect_duplicate ~path:"duplicate-parameters.HC"
    "U0 Params(I64 value,I64 value){}" "value";
  expect_duplicate ~path:"duplicate-parameter-local.HC"
    "U0 ParameterLocal(I64 value){I64 value;}" "value";
  expect_duplicate ~path:"duplicate-nested-local.HC"
    "U0 Nested(){{I64 value;}if(1){static I64 value;}}" "value"

let permitted_repeats_keep_first () =
  let index =
    prepare ~path:"permitted-function-bindings.HC"
      "U0 Padding(I64 pad,I64 reserved,I64 _anon_){I64 pad;static I64 pad;I64 \
       reserved;I64 _anon_;}"
  in
  let function_ = function_named index "Padding" in
  let bindings = Semantic_function_binding_index.function_bindings function_ in
  Alcotest.(check int) "all permitted repeats retained" 7 (List.length bindings);
  List.iter
    (fun name ->
      let first =
        List.find
          (fun (binding : Semantic_function_binding_index.binding) ->
            String.equal (Semantic_symbol.name binding.symbol) name)
          bindings
      in
      let found = expect_lookup index function_ name in
      Alcotest.(check int)
        (name ^ " selects its first insertion")
        (symbol_id first.symbol) (symbol_id found.symbol))
    [ "pad"; "reserved"; "_anon_" ]

let functions_have_independent_namespaces () =
  let index =
    prepare ~path:"independent-function-bindings.HC"
      "U0 First(I64 value){I64 local;}U0 Second(I64 value){I64 local;}"
  in
  let first = function_named index "First" in
  let second = function_named index "Second" in
  Alcotest.(check (list string))
    "first namespace" [ "value"; "local" ] (binding_names first);
  Alcotest.(check (list string))
    "second namespace" [ "value"; "local" ] (binding_names second);
  let first_parameter = expect_lookup index first "value" in
  let second_parameter = expect_lookup index second "value" in
  Alcotest.(check bool)
    "parameter identities differ" true
    (not
       (Semantic_symbol.Id.equal
          (Semantic_symbol.id first_parameter.symbol)
          (Semantic_symbol.id second_parameter.symbol)))

let source_origin = function
  | Semantic_symbol.Source_location source -> source
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected source provenance"

let generated_provenance () =
  let index =
    prepare ~path:"generated-function-bindings.HC"
      "#define PARAM generated_parameter\n\
       #define LOCAL generated_local\n\
       U0 Generated(I64 PARAM){I64 LOCAL;}"
  in
  let function_ = function_named index "Generated" in
  [ "generated_parameter"; "generated_local" ]
  |> List.iter (fun name ->
      let binding = expect_lookup index function_ name in
      let origin = source_origin (Semantic_symbol.origin binding.symbol) in
      Alcotest.(check bool)
        (name ^ " keeps its invocation") true
        (Option.is_some origin.generated_from);
      Alcotest.(check bool)
        (name ^ " keeps its definition") true
        (Option.is_some origin.defined_at))

let summary index =
  Semantic_function_binding_index.functions index
  |> List.map (fun function_ ->
      ( Semantic_function_binding_index.function_symbol function_
        |> Semantic_symbol.name,
        binding_names function_,
        Semantic_function_binding_index.function_bindings function_
        |> List.map (fun (binding : Semantic_function_binding_index.binding) ->
            Semantic_function_binding_index.binding_kind_name binding.kind) ))

let modes_and_determinism () =
  let source = "U0 Stable(I64 value,...){I64 local;static U8 stored;}" in
  let jit = prepare ~mode:Preprocessor.Jit ~path:"binding-jit.HC" source in
  let jit_again = prepare ~mode:Preprocessor.Jit ~path:"binding-jit.HC" source in
  let aot = prepare ~mode:Preprocessor.Aot ~path:"binding-aot.HC" source in
  Alcotest.(check bool) "repeated build" true (summary jit = summary jit_again);
  Alcotest.(check bool) "JIT and AOT namespace" true (summary jit = summary aot)

let cross_pass_mismatch_fails () =
  let source = "U0 Mixed(I64 value){I64 local;}" in
  let left = inputs ~path:"left-bindings.HC" source in
  let right = inputs ~path:"right-bindings.HC" source in
  let table = Session.semantic_symbols left.session in
  let before = Semantic_symbol_table.all_symbols table |> List.length in
  let result =
    Holyc_lib.index_function_bindings left.session
      ~declarations:left.declarations ~functions:left.functions
      ~function_types:left.function_types ~local_types:right.local_types
  in
  let after = Semantic_symbol_table.all_symbols table |> List.length in
  (match result with
  | Ok _ -> Alcotest.fail "expected a cross-pass ownership error"
  | Error message ->
      Alcotest.(check bool)
        "stable validation code" true
        (String.starts_with ~prefix:"HCSEMA0014: " message));
  Alcotest.(check int) "failure leaves table unchanged" before after

let low_level_validation_and_lookup_errors () =
  let table = Semantic_symbol_table.create () in
  let root = Semantic_symbol_table.root table in
  let module_scope =
    checked
      (Semantic_symbol_table.create_scope table ~parent:root
         ~kind:Semantic_symbol_table.Module ~name:"test" ())
  in
  let function_symbol =
    checked
      (Semantic_symbol_table.add table ~scope:module_scope ~name:"Indexed"
         ~kind:Semantic_symbol.Function
         ~origin:(Semantic_symbol.Synthesized "function binding test"))
  in
  let function_scope =
    checked
      (Semantic_symbol_table.create_scope table ~parent:module_scope
         ~kind:Semantic_symbol_table.Function ~name:"Indexed" ())
  in
  let local =
    checked
      (Semantic_symbol_table.add table ~scope:function_scope ~name:"local"
         ~kind:Semantic_symbol.Local_variable
         ~origin:(Semantic_symbol.Synthesized "local binding test"))
  in
  let input =
    {
      Semantic_function_binding_index.function_symbol = function_symbol;
      function_scope;
      function_item_index = 0;
      function_bindings =
        [
          {
            Semantic_function_binding_index.binding_symbol = local;
            binding_kind = Semantic_function_binding_index.Automatic_local;
            parameter_index = None;
            local_declaration_index = Some 0;
            local_declarator_index = Some 0;
          };
        ];
    }
  in
  let index =
    Semantic_function_binding_index.build ~table ~parent:module_scope [ input ]
    |> Result.map_error Semantic_function_binding_index.error_to_string
    |> checked
  in
  let unindexed =
    checked
      (Semantic_symbol_table.add table ~scope:module_scope ~name:"Unindexed"
         ~kind:Semantic_symbol.Function
         ~origin:(Semantic_symbol.Synthesized "unindexed function test"))
  in
  let unindexed_scope =
    checked
      (Semantic_symbol_table.create_scope table ~parent:module_scope
         ~kind:Semantic_symbol_table.Function ~name:"Unindexed" ())
  in
  let expect_error label code = function
    | Error error ->
        Alcotest.(check string)
          label code
          (Semantic_function_binding_index.error_code error)
    | Ok _ -> Alcotest.failf "expected %s" label
  in
  (match
     Semantic_function_binding_index.lookup index ~function_:unindexed
       ~name:"local"
   with
  | Error error ->
      Alcotest.(check string)
        "unindexed function code" "HCSEMA0016"
        (Semantic_function_binding_index.error_code error)
  | Ok _ -> Alcotest.fail "expected an unindexed function error");
  let bad_input =
    {
      input with
      function_bindings =
        [
          List.hd input.function_bindings;
          List.hd input.function_bindings;
        ];
    }
  in
  (match
     Semantic_function_binding_index.build ~table ~parent:module_scope
       [ bad_input ]
   with
  | Error error ->
      Alcotest.(check string)
        "repeated symbol validation" "HCSEMA0014"
        (Semantic_function_binding_index.error_code error)
  | Ok _ -> Alcotest.fail "expected repeated binding symbol rejection");
  let parameter =
    checked
      (Semantic_symbol_table.add table ~scope:function_scope ~name:"parameter"
         ~kind:Semantic_symbol.Parameter
         ~origin:(Semantic_symbol.Synthesized "parameter binding test"))
  in
  let parameter_input =
    {
      Semantic_function_binding_index.binding_symbol = parameter;
      binding_kind = Semantic_function_binding_index.Named_parameter;
      parameter_index = Some 0;
      local_declaration_index = None;
      local_declarator_index = None;
    }
  in
  expect_error "local before parameter validation" "HCSEMA0014"
    (Semantic_function_binding_index.build ~table ~parent:module_scope
       [ { input with function_bindings = [ List.hd input.function_bindings; parameter_input ] } ]);
  let wrong_scope_local =
    checked
      (Semantic_symbol_table.add table ~scope:module_scope ~name:"wrong_scope"
         ~kind:Semantic_symbol.Local_variable
         ~origin:(Semantic_symbol.Synthesized "wrong scope binding test"))
  in
  let wrong_scope_input =
    {
      Semantic_function_binding_index.binding_symbol = wrong_scope_local;
      binding_kind = Semantic_function_binding_index.Automatic_local;
      parameter_index = None;
      local_declaration_index = Some 0;
      local_declarator_index = Some 0;
    }
  in
  expect_error "binding scope validation" "HCSEMA0014"
    (Semantic_function_binding_index.build ~table ~parent:module_scope
       [ { input with function_bindings = [ wrong_scope_input ] } ]);
  let unindexed_input =
    {
      Semantic_function_binding_index.function_symbol = unindexed;
      function_scope = unindexed_scope;
      function_item_index = 1;
      function_bindings = [];
    }
  in
  expect_error "function source order validation" "HCSEMA0014"
    (Semantic_function_binding_index.build ~table ~parent:module_scope
       [ unindexed_input; input ]);
  expect_error "parent scope kind validation" "HCSEMA0014"
    (Semantic_function_binding_index.build ~table ~parent:root [ input ]);
  let foreign = Semantic_symbol_table.create () in
  (match
     Semantic_function_binding_index.build ~table ~parent:(
       Semantic_symbol_table.root foreign) [ input ]
   with
  | Error error ->
      Alcotest.(check string)
        "foreign parent validation" "HCSEMA0014"
        (Semantic_function_binding_index.error_code error)
  | Ok _ -> Alcotest.fail "expected foreign parent rejection")

let tests =
  [
    Alcotest.test_case "ordered bindings and lookup" `Quick
      ordered_bindings_and_lookup;
    Alcotest.test_case "ordinary duplicates fail" `Quick
      ordinary_duplicates_fail;
    Alcotest.test_case "permitted repeats keep first" `Quick
      permitted_repeats_keep_first;
    Alcotest.test_case "independent function namespaces" `Quick
      functions_have_independent_namespaces;
    Alcotest.test_case "generated provenance" `Quick generated_provenance;
    Alcotest.test_case "modes and determinism" `Quick modes_and_determinism;
    Alcotest.test_case "cross-pass mismatch" `Quick cross_pass_mismatch_fails;
    Alcotest.test_case "low-level validation and lookup errors" `Quick
      low_level_validation_and_lookup_errors;
  ]
