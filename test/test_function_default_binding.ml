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
  functions : Semantic_function_resolution.t;
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
  { mode; session; ast; functions; expressions }

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
       ~origin:
         (Semantic_symbol.Synthesized ("function default fixture " ^ name)))

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
  Holyc_lib.resolve_function_defaults prepared.session ~environment:outer
    ~expressions:prepared.expressions ~functions:prepared.functions prepared.ast
  |> checked

let function_at result index =
  List.nth (Semantic_function_default_binding.functions result) index

let function_named result name =
  Semantic_function_default_binding.functions result
  |> List.find (fun function_ ->
      function_ |> Semantic_function_default_binding.function_source_symbol
      |> Semantic_symbol.name |> String.equal name)

let parameter_occurrences function_ index =
  List.nth
    (Semantic_function_default_binding.function_parameters function_)
    index
  |> Semantic_function_default_binding.parameter_occurrences

let resolution_name occurrence =
  match Semantic_function_default_binding.occurrence_resolution occurrence with
  | Semantic_function_default_binding.Module_binding publication ->
      Printf.sprintf "module:%s:%s"
        (Semantic_module_expression_binding.publication_kind publication
        |> Semantic_module_expression_binding.publication_kind_name)
        (Semantic_module_expression_binding.publication_source_symbol
           publication
        |> Semantic_symbol.name)
  | Semantic_function_default_binding.Outer_binding binding ->
      let table = Semantic_outer_environment.binding_table binding in
      let entry = Semantic_outer_environment.binding_entry binding in
      Printf.sprintf "outer:%s:%s:%s"
        (Semantic_outer_environment.table_kind table
        |> Semantic_outer_environment.table_kind_name)
        (Semantic_outer_environment.entry_record_kind entry
        |> Semantic_outer_environment.record_kind_name)
        (Semantic_outer_environment.entry_symbol entry |> Semantic_symbol.name)

let occurrence_signature function_ =
  function_ |> Semantic_function_default_binding.function_parameters
  |> List.concat_map Semantic_function_default_binding.parameter_occurrences
  |> List.map (fun occurrence ->
      Printf.sprintf "%d:%d:%s:%s"
        (Semantic_function_default_binding.occurrence_index occurrence)
        (Semantic_function_default_binding.occurrence_parameter_index occurrence)
        (Semantic_function_default_binding.occurrence_name occurrence)
        (resolution_name occurrence))

let header_publication_and_parameter_boundary () =
  let prepared =
    prepare ~path:"function-default-order.HC"
      "I64 Earlier;\n\
       extern U0 Current(I64 Recursive=Current,I64 Prior=Earlier,I64 \
       Parameter=Parameter,I64 Late=Later,I64 Member=Earlier.field,I64 \
       Size=sizeof I64);\n\
       I64 Later;"
  in
  let outer =
    jit_environment prepared
      [
        ("Earlier", Semantic_outer_environment.Function);
        ("Parameter", Semantic_outer_environment.Global_variable);
        ("Later", Semantic_outer_environment.Function);
      ]
      []
  in
  let function_ = function_named (resolve prepared outer) "Current" in
  Alcotest.(check (list string))
    "header and source-order lookup"
    [
      "0:0:Current:module:function:Current";
      "1:1:Earlier:module:global-variable:Earlier";
      "2:2:Parameter:outer:jit-task-0:global-variable:Parameter";
      "3:3:Later:outer:jit-task-0:function:Later";
      "4:4:Earlier:module:global-variable:Earlier";
    ]
    (occurrence_signature function_);
  Alcotest.(check int)
    "member suffix and sizeof target stay outside ordinary lookup" 0
    (parameter_occurrences function_ 5 |> List.length)

let symbol_id symbol = Semantic_symbol.id symbol |> Semantic_symbol.Id.to_int

let joined_headers_and_lastclass () =
  let prepared =
    prepare ~path:"function-default-join.HC"
      "extern U0 Join(I64 first=Join,I64 first_kind=lastclass);\n\
       U0 Join(I64 second=Join,I64 second_kind=lastclass){}"
  in
  let outer = jit_environment prepared [] [] in
  let result = resolve prepared outer in
  let first = function_at result 0 in
  let second = function_at result 1 in
  let source_ids =
    [ first; second ]
    |> List.map Semantic_function_default_binding.function_source_symbol
    |> List.map symbol_id
  in
  let canonical_ids =
    [ first; second ]
    |> List.map Semantic_function_default_binding.function_canonical_symbol
    |> List.map symbol_id
  in
  Alcotest.(check bool)
    "source headers stay distinct" true
    (List.nth source_ids 0 <> List.nth source_ids 1);
  Alcotest.(check bool)
    "joined headers share one canonical identity" true
    (List.nth canonical_ids 0 = List.nth canonical_ids 1);
  Alcotest.(check (list string))
    "each recursive default sees its current publication"
    [ "0:0:Join:module:function:Join" ]
    (occurrence_signature first);
  Alcotest.(check (list string))
    "the joined definition sees its replacement publication"
    [ "0:0:Join:module:function:Join" ]
    (occurrence_signature second);
  List.iter
    (fun function_ ->
      let parameter =
        List.nth
          (Semantic_function_default_binding.function_parameters function_)
          1
      in
      Alcotest.(check int)
        "lastclass has no ordinary-name occurrence" 0
        (Semantic_function_default_binding.parameter_occurrences parameter
        |> List.length);
      match Semantic_function_default_binding.parameter_default parameter with
      | Some (Semantic_function_type_resolution.Lastclass_default _) -> ()
      | None | Some (Semantic_function_type_resolution.Expression_default _) ->
          Alcotest.fail "expected a retained lastclass default")
    [ first; second ]

let compilation_modes_and_outer_record_kinds () =
  let check mode expected_prefix =
    let prepared =
      prepare ~mode ~path:"function-default-outer.HC"
        "extern U0 Use(I64 a=Agg,I64 b=Fn,I64 c=Glb,I64 d=Asm);"
    in
    let records =
      [
        ("Agg", Semantic_outer_environment.Aggregate);
        ("Fn", Semantic_outer_environment.Function);
        ("Glb", Semantic_outer_environment.Global_variable);
      ]
    in
    let assembler =
      [ ("Asm", Semantic_outer_environment.Export_system_symbol) ]
    in
    let outer =
      match mode with
      | Preprocessor.Jit -> jit_environment prepared records assembler
      | Preprocessor.Aot -> aot_environment prepared records assembler
    in
    let function_ = function_named (resolve prepared outer) "Use" in
    Alcotest.(check (list string))
      "typed outer defaults"
      [
        "0:0:Agg:outer:" ^ expected_prefix ^ ":aggregate:Agg";
        "1:1:Fn:outer:" ^ expected_prefix ^ ":function:Fn";
        "2:2:Glb:outer:" ^ expected_prefix ^ ":global-variable:Glb";
        "3:3:Asm:outer:assembler:export-system-symbol:Asm";
      ]
      (occurrence_signature function_)
  in
  check Preprocessor.Jit "jit-task-0";
  check Preprocessor.Aot "aot-parent-0"

let symbol_origin (location : Ast.location) =
  Semantic_symbol.Source_location
    {
      span = location.span;
      source_segments = location.source_segments;
      generated_from = location.generated_from;
      defined_at = location.defined_at;
    }

let source_origin = function
  | Semantic_symbol.Source_location source -> source
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected source provenance"

let default_identifier (module_ : Ast.module_) =
  let parameters =
    match module_.items with
    | Ast.Function_prototype prototype :: _ -> prototype.parameters
    | Ast.Function_definition definition :: _ -> definition.parameters
    | _ -> Alcotest.fail "expected a function declaration"
  in
  match parameters with
  | parameter :: _ -> (
      match parameter.default with
      | Some
          { value = Ast.Expression_default (Ast.Identifier_expression name); _ }
        -> name
      | Some _ -> Alcotest.fail "expected an identifier default"
      | None -> Alcotest.fail "expected a function default")
  | [] -> Alcotest.fail "expected a function parameter"

let semantic_parameter declaration index =
  let parameters =
    declaration |> Semantic_function_resolution.resolved_declaration_site
    |> Semantic_function_resolution.declaration_site_function
    |> Semantic_function_type_resolution.function_signature
    |> Semantic_function_type_resolution.signature_parameters
  in
  List.nth parameters index

let generated_unresolved_identifier_keeps_provenance () =
  let prepared =
    prepare ~path:"function-default-generated-missing.HC"
      "#define USE Missing\nextern U0 Generated(I64 value=USE);"
  in
  let outer = jit_environment prepared [] [] in
  let declaration =
    Semantic_function_resolution.declarations prepared.functions |> List.hd
  in
  let parameter = semantic_parameter declaration 0 in
  let identifier = default_identifier prepared.ast in
  let event =
    Semantic_function_default_binding.make_identifier ~name:identifier.spelling
      ~origin:(symbol_origin identifier.location)
      ~occurrence_index:0 ~parameter_index:0
    |> checked
  in
  let parameter_input =
    Semantic_function_default_binding.make_parameter ~parameter [ event ]
    |> checked
  in
  let input =
    Semantic_function_default_binding.make_function ~declaration
      [ parameter_input ]
    |> checked
  in
  let table = Session.semantic_symbols prepared.session in
  match
    Semantic_function_default_binding.resolve ~table ~environment:outer
      ~expressions:prepared.expressions ~functions:prepared.functions [ input ]
  with
  | Ok _ -> Alcotest.fail "expected an unresolved function default name"
  | Error error -> (
      Alcotest.(check string)
        "stable unresolved code" "HCSEMA0030"
        (Semantic_function_default_binding.error_code error);
      Alcotest.(check string)
        "specific unresolved message"
        "default for parameter 0 of function \"Generated\" uses ordinary \
         identifier \"Missing\", which is absent from the visible module \
         records and the complete jit outer table chain"
        (Semantic_function_default_binding.error_message error);
      match Semantic_function_default_binding.error_origin error with
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
        (Semantic_function_default_binding.error_code error)

let input_for declaration event_lists =
  let parameters =
    declaration |> Semantic_function_resolution.resolved_declaration_site
    |> Semantic_function_resolution.declaration_site_function
    |> Semantic_function_type_resolution.function_signature
    |> Semantic_function_type_resolution.signature_parameters
  in
  if List.length parameters <> List.length event_lists then
    Alcotest.fail "test events do not match the function parameter count";
  let inputs =
    List.map2
      (fun parameter events ->
        Semantic_function_default_binding.make_parameter ~parameter events
        |> checked)
      parameters event_lists
  in
  Semantic_function_default_binding.make_function ~declaration inputs |> checked

let determinism_purity_and_validation () =
  let prepared =
    prepare ~path:"function-default-deterministic.HC"
      "#define USE Target\n\
       extern U0 First(I64 value=USE);\n\
       extern U0 Second(I64 kind=lastclass);"
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
  let first_signature = function_at first 0 |> occurrence_signature in
  Alcotest.(check (list string))
    "repeated resolution is deterministic" first_signature
    (function_at second 0 |> occurrence_signature);
  Alcotest.(check (pair int int))
    "resolution does not mutate symbols" (before, before) (middle, after);
  let provenance =
    parameter_occurrences (function_at first 0) 0
    |> List.hd |> Semantic_function_default_binding.occurrence_origin
    |> source_origin
  in
  Alcotest.(check bool)
    "successful macro invocation is retained" true
    (Option.is_some provenance.generated_from);
  Alcotest.(check bool)
    "successful macro definition is retained" true
    (Option.is_some provenance.defined_at);
  let declarations =
    Semantic_function_resolution.declarations prepared.functions
  in
  let first_declaration = List.nth declarations 0 in
  let second_declaration = List.nth declarations 1 in
  let good_event =
    Semantic_function_default_binding.make_identifier ~name:"Target"
      ~origin:(Semantic_symbol.Synthesized "valid function default occurrence")
      ~occurrence_index:0 ~parameter_index:0
    |> checked
  in
  let good_inputs =
    [
      input_for first_declaration [ [ good_event ] ];
      input_for second_declaration [ [] ];
    ]
  in
  let low inputs environment =
    Semantic_function_default_binding.resolve ~table ~environment
      ~expressions:prepared.expressions ~functions:prepared.functions inputs
  in
  let gap_event =
    Semantic_function_default_binding.make_identifier ~name:"Target"
      ~origin:(Semantic_symbol.Synthesized "gapped function default occurrence")
      ~occurrence_index:1 ~parameter_index:0
    |> checked
  in
  expect_low_error "HCSEMA0029"
    (low
       [
         input_for first_declaration [ [ gap_event ] ];
         input_for second_declaration [ [] ];
       ]
       outer);
  let wrong_parameter =
    Semantic_function_default_binding.make_identifier ~name:"Target"
      ~origin:
        (Semantic_symbol.Synthesized "misplaced function default occurrence")
      ~occurrence_index:0 ~parameter_index:1
    |> checked
  in
  expect_low_error "HCSEMA0029"
    (low
       [
         input_for first_declaration [ [ wrong_parameter ] ];
         input_for second_declaration [ [] ];
       ]
       outer);
  let lastclass_event =
    Semantic_function_default_binding.make_identifier ~name:"Target"
      ~origin:(Semantic_symbol.Synthesized "invalid lastclass occurrence")
      ~occurrence_index:0 ~parameter_index:0
    |> checked
  in
  expect_low_error "HCSEMA0029"
    (low
       [
         input_for first_declaration [ [ good_event ] ];
         input_for second_declaration [ [ lastclass_event ] ];
       ]
       outer);
  expect_low_error "HCSEMA0029" (low (List.rev good_inputs) outer);
  expect_low_error "HCSEMA0029" (low [ List.hd good_inputs ] outer);
  expect_low_error "HCSEMA0029"
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
  expect_low_error "HCSEMA0029" (low good_inputs aot_outer);
  let foreign =
    prepare ~path:"function-default-foreign.HC" "extern U0 Other();"
  in
  let foreign_outer = jit_environment foreign [] [] in
  expect_low_error "HCSEMA0029" (low good_inputs foreign_outer);
  (match
     Semantic_function_default_binding.make_identifier ~name:"Bad"
       ~origin:(Semantic_symbol.Synthesized "negative function default index")
       ~occurrence_index:(-1) ~parameter_index:0
   with
  | Ok _ -> Alcotest.fail "expected a negative occurrence index to fail"
  | Error message ->
      Alcotest.(check string)
        "negative occurrence validation"
        "function default occurrence index cannot be negative" message);
  match
    Holyc_lib.resolve_function_defaults prepared.session ~environment:outer
      ~expressions:prepared.expressions ~functions:prepared.functions
      foreign.ast
  with
  | Ok _ -> Alcotest.fail "expected AST drift to fail"
  | Error message ->
      Alcotest.(check bool)
        "driver validation uses the stable family" true
        (String.starts_with ~prefix:"HCSEMA0029: " message)

let tests =
  [
    Alcotest.test_case "header publication and parameter boundary" `Quick
      header_publication_and_parameter_boundary;
    Alcotest.test_case "joined headers and lastclass" `Quick
      joined_headers_and_lastclass;
    Alcotest.test_case "compilation modes and outer record kinds" `Quick
      compilation_modes_and_outer_record_kinds;
    Alcotest.test_case "generated unresolved name keeps provenance" `Quick
      generated_unresolved_identifier_keeps_provenance;
    Alcotest.test_case "determinism, purity, and validation" `Quick
      determinism_purity_and_validation;
  ]
