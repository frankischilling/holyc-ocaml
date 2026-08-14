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

type prepared = {
  mode : Preprocessor.compilation_mode;
  session : Session.t;
  ast : Ast.module_;
  globals : Semantic_global_resolution.t;
  expressions : Semantic_module_expression_binding.t;
}

let prepare ?(mode = Preprocessor.Jit) ~path contents =
  let session = Session.create () in
  let source = Session.add_source session ~path ~contents in
  let ast =
    Holyc_lib.parse_with_config session ~config:(config mode) ~source
    |> expect_ast
  in
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
  let function_expressions =
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
  let expressions =
    checked
      (Holyc_lib.resolve_module_expressions session ~declarations ~aggregates
         ~functions ~globals ~expressions:function_expressions)
  in
  { mode; session; ast; globals; expressions }

let semantic_kind = function
  | Semantic_outer_environment.Aggregate -> Semantic_symbol.Aggregate_type
  | Semantic_outer_environment.Function -> Semantic_symbol.Function
  | Semantic_outer_environment.Global_variable ->
      Semantic_symbol.Global_variable
  | Semantic_outer_environment.Export_system_symbol ->
      Semantic_symbol.Assembler_symbol

let add_outer_symbol prepared name record_kind =
  let table = Session.semantic_symbols prepared.session in
  checked
    (Semantic_symbol_table.add table
       ~scope:(Semantic_symbol_table.root table)
       ~name
       ~kind:(semantic_kind record_kind)
       ~origin:(Semantic_symbol.Synthesized ("global extent fixture " ^ name)))

let checked_environment = function
  | Ok value -> value
  | Error error ->
      Alcotest.fail (Semantic_outer_environment.error_to_string error)

let make_table prepared ~table_kind ~table_index records =
  records
  |> List.mapi (fun entry_index (name, record_kind) ->
      Semantic_outer_environment.make_entry
        ~symbol:(add_outer_symbol prepared name record_kind)
        ~record_kind ~entry_index
      |> checked_environment)
  |> Semantic_outer_environment.make_table ~table_kind ~table_index
  |> checked_environment

let environment prepared tables =
  Holyc_lib.create_outer_environment prepared.session
    ~compilation_mode:prepared.mode tables
  |> checked

let jit_environment prepared task_records assembler_records =
  environment prepared
    [
      make_table prepared ~table_kind:(Semantic_outer_environment.Jit_task 0)
        ~table_index:0 task_records;
      make_table prepared ~table_kind:Semantic_outer_environment.Assembler
        ~table_index:1 assembler_records;
    ]

let aot_environment prepared parent_records assembler_records =
  environment prepared
    [
      make_table prepared ~table_kind:(Semantic_outer_environment.Aot_parent 0)
        ~table_index:0 parent_records;
      make_table prepared ~table_kind:Semantic_outer_environment.Assembler
        ~table_index:1 assembler_records;
    ]

let resolve prepared outer =
  Holyc_lib.resolve_global_dimensions prepared.session ~environment:outer
    ~expressions:prepared.expressions ~globals:prepared.globals prepared.ast
  |> checked

let global_named result name =
  Semantic_global_dimension_binding.globals result
  |> List.find (fun global ->
      global |> Semantic_global_dimension_binding.global_symbol
      |> Semantic_symbol.name |> String.equal name)

let dimensions result name =
  global_named result name
  |> Semantic_global_dimension_binding.global_dimensions

let resolution_name occurrence =
  match Semantic_global_dimension_binding.occurrence_resolution occurrence with
  | Semantic_global_dimension_binding.Module_binding publication ->
      Printf.sprintf "module:%s:%s"
        (Semantic_module_expression_binding.publication_kind publication
        |> Semantic_module_expression_binding.publication_kind_name)
        (Semantic_module_expression_binding.publication_source_symbol
           publication
        |> Semantic_symbol.name)
  | Semantic_global_dimension_binding.Outer_binding binding ->
      let table = Semantic_outer_environment.binding_table binding in
      let entry = Semantic_outer_environment.binding_entry binding in
      Printf.sprintf "outer:%s:%s:%s"
        (Semantic_outer_environment.table_kind table
        |> Semantic_outer_environment.table_kind_name)
        (Semantic_outer_environment.entry_record_kind entry
        |> Semantic_outer_environment.record_kind_name)
        (Semantic_outer_environment.entry_symbol entry |> Semantic_symbol.name)

let signature result name =
  dimensions result name
  |> List.concat_map Semantic_global_dimension_binding.dimension_occurrences
  |> List.map (fun occurrence ->
      Printf.sprintf "%d:%d:%s:%s"
        (Semantic_global_dimension_binding.occurrence_index occurrence)
        (Semantic_global_dimension_binding.occurrence_dimension_index occurrence)
        (Semantic_global_dimension_binding.occurrence_name occurrence)
        (resolution_name occurrence))

let prepublication_and_source_order () =
  let prepared =
    prepare ~path:"global-extent-order.HC"
      "I64 Self[Self],Earlier[1],After[Earlier],Before[Later],Later[1];I64 \
       BeforeItem[Future];I64 Future[1];"
  in
  let outer =
    jit_environment prepared
      [
        ("Self", Semantic_outer_environment.Global_variable);
        ("Later", Semantic_outer_environment.Global_variable);
        ("Future", Semantic_outer_environment.Global_variable);
      ]
      []
  in
  let result = resolve prepared outer in
  Alcotest.(check (list string))
    "the current record falls through"
    [ "0:0:Self:outer:jit-task-0:global-variable:Self" ]
    (signature result "Self");
  Alcotest.(check (list string))
    "an earlier comma declarator is visible"
    [ "0:0:Earlier:module:global-variable:Earlier" ]
    (signature result "After");
  Alcotest.(check (list string))
    "a later comma declarator falls through"
    [ "0:0:Later:outer:jit-task-0:global-variable:Later" ]
    (signature result "Before");
  Alcotest.(check (list string))
    "a later item falls through"
    [ "0:0:Future:outer:jit-task-0:global-variable:Future" ]
    (signature result "BeforeItem");
  let publication_indexes =
    Semantic_global_dimension_binding.globals result
    |> List.map (fun global ->
        global |> Semantic_global_dimension_binding.global_publication
        |> Semantic_module_expression_binding.publication_declaration_index)
  in
  Alcotest.(check (list int))
    "global publication order" [ 0; 1; 2; 3; 4; 5; 6 ] publication_indexes

let empty_and_multidimensional_extents () =
  let prepared =
    prepare ~path:"global-extent-multidimensional.HC"
      "I64 Count=1;I64 Values[][Count+Outer][Probe(,Count)];"
  in
  let outer =
    jit_environment prepared
      [
        ("Count", Semantic_outer_environment.Function);
        ("Outer", Semantic_outer_environment.Global_variable);
        ("Probe", Semantic_outer_environment.Function);
      ]
      []
  in
  let result = resolve prepared outer in
  let dimensions = dimensions result "Values" in
  Alcotest.(check int) "all dimensions are retained" 3 (List.length dimensions);
  Alcotest.(check (list bool))
    "only the first dimension is empty" [ false; true; true ]
    (dimensions
    |> List.map (fun dimension ->
        dimension
        |> Semantic_global_dimension_binding.dimension_expression_origin
        |> Option.is_some));
  Alcotest.(check (list int))
    "empty dimensions add no false occurrences" [ 0; 2; 2 ]
    (dimensions
    |> List.map (fun dimension ->
        dimension |> Semantic_global_dimension_binding.dimension_occurrences
        |> List.length));
  Alcotest.(check (list string))
    "occurrence identities span the complete declarator"
    [
      "0:1:Count:module:global-variable:Count";
      "1:1:Outer:outer:jit-task-0:global-variable:Outer";
      "2:2:Probe:outer:jit-task-0:function:Probe";
      "3:2:Count:module:global-variable:Count";
    ]
    (signature result "Values")

let declarator_shapes_and_modes () =
  let check mode expected_near =
    let prepared =
      prepare ~mode ~path:"global-extent-shapes.HC"
        "class Box {I64 value;} Items[Near];U0 (*Callbacks)()[Asm];I64 \
         Values[Classish];"
    in
    let outer =
      match mode with
      | Preprocessor.Jit ->
          jit_environment prepared
            [
              ("Near", Semantic_outer_environment.Global_variable);
              ("Classish", Semantic_outer_environment.Aggregate);
            ]
            [ ("Asm", Semantic_outer_environment.Export_system_symbol) ]
      | Preprocessor.Aot ->
          aot_environment prepared
            [
              ("Near", Semantic_outer_environment.Function);
              ("Classish", Semantic_outer_environment.Aggregate);
            ]
            [ ("Asm", Semantic_outer_environment.Export_system_symbol) ]
    in
    let result = resolve prepared outer in
    Alcotest.(check (list string))
      "aggregate-attached object extent"
      [ "0:0:Near:" ^ expected_near ]
      (signature result "Items");
    Alcotest.(check (list string))
      "function-pointer array extent"
      [ "0:0:Asm:outer:assembler:export-system-symbol:Asm" ]
      (signature result "Callbacks");
    Alcotest.(check (list string))
      "ordinary object extent"
      [
        Printf.sprintf "0:0:Classish:outer:%s:aggregate:Classish"
          (match mode with
          | Preprocessor.Jit -> "jit-task-0"
          | Preprocessor.Aot -> "aot-parent-0");
      ]
      (signature result "Values")
  in
  check Preprocessor.Jit "outer:jit-task-0:global-variable:Near";
  check Preprocessor.Aot "outer:aot-parent-0:function:Near"

let symbol_origin (location : Ast.location) =
  Semantic_symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

let extent_identifier (module_ : Ast.module_) =
  match module_.items with
  | Ast.Global_variable variable :: _ -> (
      match variable.array_dimensions with
      | dimension :: _ -> (
          match dimension.dimension_expression with
          | Some (Ast.Identifier_expression identifier) -> identifier
          | Some _ -> Alcotest.fail "expected an identifier extent"
          | None -> Alcotest.fail "expected an explicit extent")
      | [] -> Alcotest.fail "expected an array declarator")
  | Ast.Global_declaration declaration :: _ -> (
      match declaration.declarators with
      | declarator :: _ -> (
          match declarator.array_dimensions with
          | dimension :: _ -> (
              match dimension.dimension_expression with
              | Some (Ast.Identifier_expression identifier) -> identifier
              | Some _ -> Alcotest.fail "expected an identifier extent"
              | None -> Alcotest.fail "expected an explicit extent")
          | [] -> Alcotest.fail "expected an array declarator")
      | [] -> Alcotest.fail "expected a global declarator")
  | _ -> Alcotest.fail "expected a global declaration"

let source_origin = function
  | Semantic_symbol.Source_location source -> source
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected source provenance"

let generated_unresolved_identifier_keeps_provenance () =
  let prepared =
    prepare ~path:"global-extent-generated-missing.HC"
      "#define USE Missing\nI64 Values[USE];"
  in
  let outer = jit_environment prepared [] [] in
  let identifier = extent_identifier prepared.ast in
  let event =
    Semantic_global_dimension_binding.make_identifier ~name:identifier.spelling
      ~origin:(symbol_origin identifier.location)
      ~occurrence_index:0 ~dimension_index:0
    |> checked
  in
  let record = Semantic_global_resolution.records prepared.globals |> List.hd in
  let semantic_dimension =
    record |> Semantic_global_resolution.global_record_global
    |> Semantic_global_type_resolution.global_array_dimensions |> List.hd
  in
  let dimension =
    Semantic_global_dimension_binding.make_dimension
      ~dimension:semantic_dimension [ event ]
    |> checked
  in
  let input =
    Semantic_global_dimension_binding.make_global ~record [ dimension ]
    |> checked
  in
  let table = Session.semantic_symbols prepared.session in
  match
    Semantic_global_dimension_binding.resolve ~table ~environment:outer
      ~expressions:prepared.expressions ~globals:prepared.globals [ input ]
  with
  | Ok _ -> Alcotest.fail "expected an unresolved global array extent name"
  | Error error -> (
      Alcotest.(check string)
        "stable unresolved code" "HCSEMA0028"
        (Semantic_global_dimension_binding.error_code error);
      Alcotest.(check string)
        "specific unresolved message"
        "global array dimension 0 for \"Values\" uses ordinary identifier \
         \"Missing\", which is absent from the visible module records and the \
         complete jit outer table chain"
        (Semantic_global_dimension_binding.error_message error);
      match Semantic_global_dimension_binding.error_origin error with
      | Some origin ->
          let source = source_origin origin in
          Alcotest.(check bool)
            "macro invocation is retained" true
            (Option.is_some source.generated_from);
          Alcotest.(check bool)
            "macro definition is retained" true
            (Option.is_some source.defined_at)
      | None -> Alcotest.fail "expected an unresolved identifier origin")

let expect_low_error expected = function
  | Ok _ -> Alcotest.failf "expected %s" expected
  | Error error ->
      Alcotest.(check string)
        "stable semantic error code" expected
        (Semantic_global_dimension_binding.error_code error)

let dimension_for record =
  record |> Semantic_global_resolution.global_record_global
  |> Semantic_global_type_resolution.global_array_dimensions |> List.hd

let input_for record events =
  let dimension =
    Semantic_global_dimension_binding.make_dimension
      ~dimension:(dimension_for record) events
    |> checked
  in
  Semantic_global_dimension_binding.make_global ~record [ dimension ] |> checked

let determinism_purity_and_validation () =
  let prepared =
    prepare ~path:"global-extent-deterministic.HC"
      "#define USE Target\nI64 Values[USE],Other[1];"
  in
  let outer =
    jit_environment prepared
      [ ("Target", Semantic_outer_environment.Function) ]
      []
  in
  let table = Session.semantic_symbols prepared.session in
  let before = Semantic_symbol_table.all_symbols table |> List.length in
  let first = resolve prepared outer in
  let middle = Semantic_symbol_table.all_symbols table |> List.length in
  let second = resolve prepared outer in
  let after = Semantic_symbol_table.all_symbols table |> List.length in
  Alcotest.(check (list string))
    "repeated resolution is deterministic" (signature first "Values")
    (signature second "Values");
  Alcotest.(check (pair int int))
    "resolution does not mutate symbols" (before, before) (middle, after);
  let source =
    dimensions first "Values" |> List.hd
    |> Semantic_global_dimension_binding.dimension_occurrences |> List.hd
    |> Semantic_global_dimension_binding.occurrence_origin |> source_origin
  in
  Alcotest.(check bool)
    "successful macro invocation is retained" true
    (Option.is_some source.generated_from);
  Alcotest.(check bool)
    "successful macro definition is retained" true
    (Option.is_some source.defined_at);
  let records = Semantic_global_resolution.records prepared.globals in
  let first_record = List.nth records 0 in
  let second_record = List.nth records 1 in
  let good_event =
    Semantic_global_dimension_binding.make_identifier ~name:"Target"
      ~origin:(Semantic_symbol.Synthesized "valid global extent occurrence")
      ~occurrence_index:0 ~dimension_index:0
    |> checked
  in
  let good_inputs =
    [ input_for first_record [ good_event ]; input_for second_record [] ]
  in
  let low inputs environment =
    Semantic_global_dimension_binding.resolve ~table ~environment
      ~expressions:prepared.expressions ~globals:prepared.globals inputs
  in
  let gap_event =
    Semantic_global_dimension_binding.make_identifier ~name:"Target"
      ~origin:(Semantic_symbol.Synthesized "gapped global extent occurrence")
      ~occurrence_index:1 ~dimension_index:0
    |> checked
  in
  expect_low_error "HCSEMA0027"
    (low
       [ input_for first_record [ gap_event ]; input_for second_record [] ]
       outer);
  let wrong_dimension =
    Semantic_global_dimension_binding.make_identifier ~name:"Target"
      ~origin:(Semantic_symbol.Synthesized "misplaced global extent occurrence")
      ~occurrence_index:0 ~dimension_index:1
    |> checked
  in
  expect_low_error "HCSEMA0027"
    (low
       [
         input_for first_record [ wrong_dimension ]; input_for second_record [];
       ]
       outer);
  let drift_origin =
    Semantic_symbol.Synthesized "drifted global array dimension"
  in
  let drifted_dimension =
    Semantic_global_type_resolution.make_array_dimension ~index:0
      ~origin:drift_origin ~opening_origin:drift_origin
      ~expression_origin:drift_origin ~closing_origin:drift_origin ()
    |> checked
  in
  let drifted_input =
    Semantic_global_dimension_binding.make_dimension
      ~dimension:drifted_dimension [ good_event ]
    |> checked
    |> fun dimension ->
    Semantic_global_dimension_binding.make_global ~record:first_record
      [ dimension ]
    |> checked
  in
  expect_low_error "HCSEMA0027"
    (low [ drifted_input; input_for second_record [] ] outer);
  expect_low_error "HCSEMA0027" (low (List.rev good_inputs) outer);
  expect_low_error "HCSEMA0027" (low [ List.hd good_inputs ] outer);
  expect_low_error "HCSEMA0027"
    (low [ List.hd good_inputs; List.hd good_inputs ] outer);
  let aot_assembler =
    make_table prepared ~table_kind:Semantic_outer_environment.Assembler
      ~table_index:0 []
  in
  let aot_outer =
    Holyc_lib.create_outer_environment prepared.session
      ~compilation_mode:Preprocessor.Aot [ aot_assembler ]
    |> checked
  in
  expect_low_error "HCSEMA0027" (low good_inputs aot_outer);
  let foreign = prepare ~path:"global-extent-foreign.HC" "I64 Foreign[1];" in
  let foreign_outer = jit_environment foreign [] [] in
  expect_low_error "HCSEMA0027" (low good_inputs foreign_outer);
  (match
     Semantic_global_dimension_binding.make_identifier ~name:"Bad"
       ~origin:(Semantic_symbol.Synthesized "negative global extent index")
       ~occurrence_index:(-1) ~dimension_index:0
   with
  | Ok _ -> Alcotest.fail "expected a negative occurrence index to fail"
  | Error message ->
      Alcotest.(check string)
        "negative occurrence validation"
        "global array extent occurrence index cannot be negative" message);
  (match
     Semantic_global_dimension_binding.make_identifier ~name:"Bad"
       ~origin:(Semantic_symbol.Synthesized "negative global dimension index")
       ~occurrence_index:0 ~dimension_index:(-1)
   with
  | Ok _ -> Alcotest.fail "expected a negative dimension index to fail"
  | Error message ->
      Alcotest.(check string)
        "negative dimension validation"
        "global array extent dimension index cannot be negative" message);
  match
    Holyc_lib.resolve_global_dimensions prepared.session ~environment:outer
      ~expressions:prepared.expressions ~globals:prepared.globals foreign.ast
  with
  | Ok _ -> Alcotest.fail "expected AST drift to fail"
  | Error message ->
      Alcotest.(check bool)
        "driver validation uses the stable family" true
        (String.starts_with ~prefix:"HCSEMA0027: " message)

let tests =
  [
    Alcotest.test_case "prepublication and source order" `Quick
      prepublication_and_source_order;
    Alcotest.test_case "empty and multidimensional extents" `Quick
      empty_and_multidimensional_extents;
    Alcotest.test_case "declarator shapes and compilation modes" `Quick
      declarator_shapes_and_modes;
    Alcotest.test_case "generated unresolved name keeps provenance" `Quick
      generated_unresolved_identifier_keeps_provenance;
    Alcotest.test_case "determinism, purity, and validation" `Quick
      determinism_purity_and_validation;
  ]
