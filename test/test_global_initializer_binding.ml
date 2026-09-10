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
  declarations : Semantic_declaration_collection.t;
  globals : Semantic_global_resolution.t;
  expressions : Semantic_module_expression_binding.t;
}

let prepare ?(mode = Preprocessor.Jit) ?query ~path contents =
  let session = Session.create () in
  let source = Session.add_source session ~path ~contents in
  let ast =
    match query with
    | None ->
        Holyc_lib.parse_with_config session ~config:(config mode) ~source
        |> expect_ast
    | Some query ->
        let commands : Parser.command_sink =
          {
            checkpoint = None;
            implicit_output = None;
            reference = None;
            query = Some query;
            declaration = None;
            dimension_count = None;
            command = (fun _ -> Ok ());
            resume = (fun () -> Ok ());
          }
        in
        Parser.parse ~commands ~sources:(Session.sources session)
          ~symbols:(Session.symbols session)
          ~definitions:(Session.definitions session)
          ~config:(config mode) source
        |> Test_parser.expect_ast
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
  { mode; session; ast; declarations; globals; expressions }

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
         (Semantic_symbol.Synthesized ("global initializer fixture " ^ name)))

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

let resolve prepared environment =
  Holyc_lib.resolve_global_initializers prepared.session ~environment
    ~expressions:prepared.expressions ~globals:prepared.globals prepared.ast
  |> checked

let global_named result name =
  Semantic_global_initializer_binding.globals result
  |> List.find (fun global ->
      global |> Semantic_global_initializer_binding.global_symbol
      |> Semantic_symbol.name |> String.equal name)

let occurrences result name =
  global_named result name
  |> Semantic_global_initializer_binding.global_occurrences

let path_string path = path |> List.map string_of_int |> String.concat "."

let resolution_name occurrence =
  match
    Semantic_global_initializer_binding.occurrence_resolution occurrence
  with
  | Semantic_global_initializer_binding.Module_binding publication ->
      Printf.sprintf "module:%s:%s"
        (Semantic_module_expression_binding.publication_kind publication
        |> Semantic_module_expression_binding.publication_kind_name)
        (Semantic_module_expression_binding.publication_source_symbol
           publication
        |> Semantic_symbol.name)
  | Semantic_global_initializer_binding.Outer_binding binding ->
      let table = Semantic_outer_environment.binding_table binding in
      let entry = Semantic_outer_environment.binding_entry binding in
      Printf.sprintf "outer:%s:%s:%s"
        (Semantic_outer_environment.table_kind table
        |> Semantic_outer_environment.table_kind_name)
        (Semantic_outer_environment.entry_record_kind entry
        |> Semantic_outer_environment.record_kind_name)
        (Semantic_outer_environment.entry_symbol entry |> Semantic_symbol.name)

let signature occurrences =
  occurrences
  |> List.map (fun occurrence ->
      ( Semantic_global_initializer_binding.occurrence_name occurrence,
        resolution_name occurrence,
        Semantic_global_initializer_binding.occurrence_initializer_path
          occurrence
        |> path_string ))

let self_and_comma_source_order () =
  let prepared =
    prepare ~path:"global-initializer-order.HC"
      "I64 Self=Self,Earlier=Self,Before=Later,Later=1;I64 \
       BeforeItem=Future;I64 Future=1;"
  in
  let outer =
    jit_environment prepared
      [
        ("Later", Semantic_outer_environment.Global_variable);
        ("Future", Semantic_outer_environment.Global_variable);
      ]
      []
  in
  let result = resolve prepared outer in
  Alcotest.(check (list (triple string string string)))
    "self reference is visible"
    [ ("Self", "module:global-variable:Self", "") ]
    (signature (occurrences result "Self"));
  Alcotest.(check (list (triple string string string)))
    "earlier comma declarator is visible"
    [ ("Self", "module:global-variable:Self", "") ]
    (signature (occurrences result "Earlier"));
  Alcotest.(check (list (triple string string string)))
    "later comma declarator is not visible"
    [ ("Later", "outer:jit-task-0:global-variable:Later", "") ]
    (signature (occurrences result "Before"));
  Alcotest.(check (list (triple string string string)))
    "later module item is not visible"
    [ ("Future", "outer:jit-task-0:global-variable:Future", "") ]
    (signature (occurrences result "BeforeItem"));
  let indexes =
    Semantic_global_initializer_binding.globals result
    |> List.map (fun global ->
        global |> Semantic_global_initializer_binding.global_publication
        |> Semantic_module_expression_binding.publication_declaration_index)
  in
  Alcotest.(check (list int))
    "global publication indexes" [ 0; 1; 2; 3; 4; 5 ] indexes;
  let positions =
    Semantic_global_initializer_binding.globals result
    |> List.map (fun global ->
        ( Semantic_global_initializer_binding.global_item_index global,
          Semantic_global_initializer_binding.global_declarator_index global ))
  in
  Alcotest.(check (list (pair int (option int))))
    "owning AST positions"
    [
      (0, Some 0);
      (0, Some 1);
      (0, Some 2);
      (0, Some 3);
      (1, Some 0);
      (2, Some 0);
    ]
    positions

let prior_module_and_nested_paths () =
  let prepared =
    prepare ~path:"global-initializer-nested.HC"
      "I64 First=1;I64 Values[4]={First,{Outer,First},Outer};I64 \
       Unbraced[2]=First,Outer;"
  in
  let outer =
    jit_environment prepared
      [
        ("First", Semantic_outer_environment.Function);
        ("Outer", Semantic_outer_environment.Global_variable);
      ]
      []
  in
  let result = resolve prepared outer in
  Alcotest.(check (list (triple string string string)))
    "nested source order and paths"
    [
      ("First", "module:global-variable:First", "0");
      ("Outer", "outer:jit-task-0:global-variable:Outer", "1.0");
      ("First", "module:global-variable:First", "1.1");
      ("Outer", "outer:jit-task-0:global-variable:Outer", "2");
    ]
    (signature (occurrences result "Values"));
  Alcotest.(check (list (triple string string string)))
    "unbraced array paths"
    [
      ("First", "module:global-variable:First", "0");
      ("Outer", "outer:jit-task-0:global-variable:Outer", "1");
    ]
    (signature (occurrences result "Unbraced"))

let mode_specific_outer_chains () =
  let check mode expected_near =
    let prepared =
      prepare ~mode ~path:"global-initializer-mode.HC"
        "I64 Value=Near+Asm+Classish;"
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
    Alcotest.(check (list (triple string string string)))
      "mode-specific outer bindings"
      [
        ("Near", expected_near, "");
        ("Asm", "outer:assembler:export-system-symbol:Asm", "");
        ( "Classish",
          (match mode with
          | Preprocessor.Jit -> "outer:jit-task-0:aggregate:Classish"
          | Preprocessor.Aot -> "outer:aot-parent-0:aggregate:Classish"),
          "" );
      ]
      (signature (occurrences result "Value"))
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

let initializer_identifier (module_ : Ast.module_) =
  match module_.items with
  | Ast.Global_declaration declaration :: _ -> (
      match declaration.declarators with
      | declarator :: _ -> (
          match declarator.global_initial_value with
          | Some initial -> (
              match initial.global_initializer_value with
              | Ast.Scalar_initializer (Ast.Identifier_expression identifier) ->
                  identifier
              | _ -> Alcotest.fail "expected a scalar identifier initializer")
          | None -> Alcotest.fail "expected a global initializer")
      | [] -> Alcotest.fail "expected a global declarator")
  | _ -> Alcotest.fail "expected a global declaration"

let source_origin = function
  | Semantic_symbol.Source_location source -> source
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected source provenance"

let generated_unresolved_identifier_keeps_provenance () =
  let prepared =
    prepare ~path:"global-initializer-generated-missing.HC"
      "#define USE Missing\nI64 Value=USE;"
  in
  let outer = jit_environment prepared [] [] in
  let identifier = initializer_identifier prepared.ast in
  let event =
    Semantic_global_initializer_binding.make_identifier
      ~name:identifier.spelling
      ~origin:(symbol_origin identifier.location)
      ~occurrence_index:0 ~initializer_path:[]
    |> checked
  in
  let record = Semantic_global_resolution.records prepared.globals |> List.hd in
  let input =
    Semantic_global_initializer_binding.make_global ~record [ event ] |> checked
  in
  let table = Session.semantic_symbols prepared.session in
  match
    Semantic_global_initializer_binding.resolve ~table ~environment:outer
      ~expressions:prepared.expressions ~globals:prepared.globals [ input ]
  with
  | Ok _ -> Alcotest.fail "expected an unresolved global initializer name"
  | Error error -> (
      Alcotest.(check string)
        "stable unresolved code" "HCSEMA0026"
        (Semantic_global_initializer_binding.error_code error);
      Alcotest.(check string)
        "specific unresolved message"
        "global initializer for \"Value\" uses ordinary identifier \
         \"Missing\", which is absent from the visible module records and the \
         complete jit outer table chain"
        (Semantic_global_initializer_binding.error_message error);
      match Semantic_global_initializer_binding.error_origin error with
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
        (Semantic_global_initializer_binding.error_code error)

let determinism_purity_and_validation () =
  let prepared =
    prepare ~path:"global-initializer-deterministic.HC"
      "#define USE Target\nI64 Value=USE;"
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
  Alcotest.(check (list (triple string string string)))
    "repeated resolution is deterministic"
    (signature (occurrences first "Value"))
    (signature (occurrences second "Value"));
  Alcotest.(check (pair int int))
    "resolution does not mutate symbols" (before, before) (middle, after);
  (match occurrences first "Value" with
  | [ occurrence ] ->
      let source =
        occurrence |> Semantic_global_initializer_binding.occurrence_origin
        |> source_origin
      in
      Alcotest.(check bool)
        "successful macro invocation is retained" true
        (Option.is_some source.generated_from);
      Alcotest.(check bool)
        "successful macro definition is retained" true
        (Option.is_some source.defined_at)
  | _ -> Alcotest.fail "expected one generated identifier occurrence");
  let record = Semantic_global_resolution.records prepared.globals |> List.hd in
  let bad_event =
    Semantic_global_initializer_binding.make_identifier ~name:"Target"
      ~origin:(Semantic_symbol.Synthesized "bad global initializer index")
      ~occurrence_index:1 ~initializer_path:[]
    |> checked
  in
  let bad_input =
    Semantic_global_initializer_binding.make_global ~record [ bad_event ]
    |> checked
  in
  expect_low_error "HCSEMA0025"
    (Semantic_global_initializer_binding.resolve ~table ~environment:outer
       ~expressions:prepared.expressions ~globals:prepared.globals [ bad_input ]);
  let aot_assembler =
    make_table prepared ~table_kind:Semantic_outer_environment.Assembler
      ~table_index:0 []
  in
  let aot_outer =
    Holyc_lib.create_outer_environment prepared.session
      ~compilation_mode:Preprocessor.Aot [ aot_assembler ]
    |> checked
  in
  expect_low_error "HCSEMA0025"
    (Semantic_global_initializer_binding.resolve ~table ~environment:aot_outer
       ~expressions:prepared.expressions ~globals:prepared.globals [ bad_input ]);
  let ordered =
    prepare ~path:"global-initializer-input-order.HC" "I64 First=1,Second=2;"
  in
  let ordered_outer = jit_environment ordered [] [] in
  let ordered_table = Session.semantic_symbols ordered.session in
  let inputs =
    Semantic_global_resolution.records ordered.globals
    |> List.map (fun record ->
        Semantic_global_initializer_binding.make_global ~record [] |> checked)
  in
  expect_low_error "HCSEMA0025"
    (Semantic_global_initializer_binding.resolve ~table:ordered_table
       ~environment:ordered_outer ~expressions:ordered.expressions
       ~globals:ordered.globals (List.rev inputs));
  expect_low_error "HCSEMA0025"
    (Semantic_global_initializer_binding.resolve ~table:ordered_table
       ~environment:ordered_outer ~expressions:ordered.expressions
       ~globals:ordered.globals
       [ List.hd inputs ]);
  expect_low_error "HCSEMA0025"
    (Semantic_global_initializer_binding.resolve ~table:ordered_table
       ~environment:ordered_outer ~expressions:ordered.expressions
       ~globals:ordered.globals
       [ List.hd inputs; List.hd inputs ]);
  let other = prepare ~path:"global-initializer-other.HC" "I64 Other=1;" in
  let other_outer = jit_environment other [] [] in
  let valid_input =
    Semantic_global_initializer_binding.make_global ~record [] |> checked
  in
  expect_low_error "HCSEMA0025"
    (Semantic_global_initializer_binding.resolve ~table ~environment:other_outer
       ~expressions:prepared.expressions ~globals:prepared.globals
       [ valid_input ]);
  (match
     Semantic_global_initializer_binding.make_identifier ~name:"BadPath"
       ~origin:(Semantic_symbol.Synthesized "bad global initializer path")
       ~occurrence_index:0 ~initializer_path:[ -1 ]
   with
  | Ok _ -> Alcotest.fail "expected a negative initializer path to fail"
  | Error message ->
      Alcotest.(check string)
        "negative path validation"
        "global initializer path cannot contain a negative index" message);
  match
    Holyc_lib.resolve_global_initializers prepared.session ~environment:outer
      ~expressions:prepared.expressions ~globals:prepared.globals other.ast
  with
  | Ok _ -> Alcotest.fail "expected AST drift to fail"
  | Error message ->
      Alcotest.(check bool)
        "driver validation uses the stable family" true
        (String.starts_with ~prefix:"HCSEMA0025: " message)

let selected_initializer_environment () =
  let module E = Semantic_outer_environment in
  let module S = Semantic_reference_selection in
  let module G = Semantic_global_initializer_binding in
  let module T = Semantic_top_level_expression_binding in
  let prepared = prepare ~path:"selected-initializer.hc" "I64 Value=Target;" in
  let original =
    jit_environment prepared [ ("Target", E.Global_variable) ] []
  in
  let table = Session.semantic_symbols prepared.session in
  let other =
    E.create ~table ~compilation_mode:E.Jit (E.tables original)
    |> checked_environment
  in
  let initializers = resolve prepared original in
  let global = G.globals initializers |> List.hd in
  let occurrence = G.global_occurrences global |> List.hd in
  let binding =
    match G.occurrence_resolution occurrence with
    | G.Outer_binding binding -> binding
    | _ -> Alcotest.fail "expected initializer outer binding"
  in
  let second_pass environment =
    let selection =
      S.outer ~table ~name:"Target" ~environment ~binding |> checked
    in
    let event =
      T.make_selected_identifier ~selection ~name:"Target"
        ~origin:(G.occurrence_origin occurrence)
      |> checked
    in
    let input =
      T.make_global_initializer ~statement_index:0 ~initializers ~global
        [ event ]
      |> checked
    in
    T.resolve ~table
      ~parent:
        (Semantic_module_expression_binding.parent_scope prepared.expressions)
      ~module_expressions:prepared.expressions [ input ]
  in
  Alcotest.(check bool)
    "original environment supports the same initializer binding" true
    (second_pass original |> Result.is_ok);
  Alcotest.(check bool)
    "same binding in another environment cannot replace initializer selection"
    true
    (second_pass other |> Result.is_error)

let initializer_query_source_manifest () =
  let module G = Semantic_global_initializer_binding in
  let module T = Semantic_top_level_expression_binding in
  let prepared =
    prepare ~path:"initializer-query-manifest.hc"
      "I64 Value=defined Missing+defined Value;"
  in
  let outer = jit_environment prepared [] [] in
  let initializers = resolve prepared outer in
  let global = G.globals initializers |> List.hd in
  let leaf = G.global_leaves global |> List.hd in
  let nodes =
    Semantic_query_selection.source_queries
      (Semantic_initializer_source.leaf_expression_ast leaf)
  in
  let events =
    List.map
      (function
        | Ast.Defined_expression expression ->
            let operand = expression.defined_operand in
            T.make_name_query
              ~role:Semantic_function_expression_binding.Defined_operand
              ~name:operand.defined_operand_spelling
              ~origin:
                (Semantic_initializer_source.origin_of_location
                   operand.defined_operand_location)
            |> checked
        | _ -> Alcotest.fail "expected defined query")
      nodes
  in
  let accepts events =
    let input =
      T.make_global_initializer ~statement_index:0 ~initializers ~global events
      |> checked
    in
    T.resolve
      ~table:(Session.semantic_symbols prepared.session)
      ~parent:
        (Semantic_module_expression_binding.parent_scope prepared.expressions)
      ~module_expressions:prepared.expressions [ input ]
    |> Result.is_ok
  in
  Alcotest.(check (list bool))
    "initializer queries keep original order and completeness"
    [ true; false; false; false ]
    [
      accepts events;
      accepts (List.rev events);
      accepts [ List.hd events ];
      accepts (events @ events);
    ]

let selected_initializer_query_manifest () =
  let module G = Semantic_global_initializer_binding in
  let module T = Semantic_top_level_expression_binding in
  let module Q = Semantic_query_selection in
  let module I = Semantic_initializer_source in
  let receipts = ref [] in
  let query = function
    | Parser.Query_completed receipt ->
        receipts := receipt :: !receipts;
        Ok ()
    | _ -> Ok ()
  in
  let prepared =
    prepare ~query ~path:"selected-initializer-queries.hc"
      "I64 Value=defined 42+defined Missing+defined Value;"
  in
  let table = Session.semantic_symbols prepared.session in
  let outer = jit_environment prepared [] [] in
  let legacy = resolve prepared outer in
  let legacy_global = G.globals legacy |> List.hd in
  let leaf = G.global_leaves legacy_global |> List.hd in
  let receipts = List.rev !receipts in
  let select table =
    List.map (fun receipt -> Q.make ~table ~receipt () |> checked) receipts
  in
  let selections = select table in
  let pairs = List.map (fun selection -> (leaf, selection)) selections in
  let bind ?queries record =
    let input = G.make_global ?queries ~record [] |> checked in
    G.resolve ~table ~environment:outer ~expressions:prepared.expressions
      ~globals:prepared.globals [ input ]
  in
  let record = G.global_record legacy_global in
  let selected =
    bind ~queries:pairs record |> Result.map_error G.error_to_string |> checked
  in
  let global = G.globals selected |> List.hd in
  Alcotest.(check int)
    "non-name defined has a source descriptor" 3
    (List.length (G.global_queries global));
  List.iter2
    (fun selection descriptor ->
      Alcotest.(check bool)
        "descriptor retains the exact query object" true
        (G.query_selection descriptor = Some selection
        && Option.get (G.query_selection descriptor) == selection
        && G.query_expression descriptor == Q.expression selection
        && G.query_leaf descriptor == leaf))
    selections (G.global_queries global);
  let top =
    Holyc_lib.resolve_top_level_expressions prepared.session
      ~declarations:prepared.declarations
      ~module_expressions:prepared.expressions ~initializers:selected
      prepared.ast
    |> checked
  in
  Alcotest.(check int)
    "non-name descriptor is not a named event" 2
    (List.length (T.all_queries top));
  List.iter2
    (fun selection query ->
      Alcotest.(check bool)
        "driver reuses captured query without a new resolver" true
        (Option.get (T.query_selection query) == selection))
    (List.tl selections) (T.all_queries top);
  let foreign_source =
    I.create (I.source_ast (Option.get (G.global_source global)))
  in
  let foreign_leaf = I.leaves foreign_source |> List.hd in
  let reject label pairs =
    Alcotest.(check bool)
      label true
      (bind ~queries:pairs record |> Result.is_error)
  in
  reject "non-name read cannot be omitted" (List.tl pairs);
  reject "read order cannot change" (List.rev pairs);
  reject "reads cannot repeat" (pairs @ pairs);
  reject "equal source tree cannot supply a replacement leaf"
    (List.map (fun selection -> (foreign_leaf, selection)) selections);
  reject "another table cannot supply query reads"
    (List.map
       (fun selection -> (leaf, selection))
       (select (Session.semantic_symbols (Session.create ()))));
  Alcotest.(check bool)
    "missing exact leaf lookup fails" true
    (G.query_for ~global ~leaf:foreign_leaf
       ~expression:(Q.expression (List.hd selections))
    |> Result.is_error);
  let events ?(with_leaf = true) ?(unselected = false) ?(leaf = leaf) selections
      =
    List.filter_map
      (fun selection ->
        match Q.name_query_facts (Q.expression selection) with
        | None -> None
        | Some (role, name, origin) ->
            let selection = if unselected then None else Some selection in
            let event =
              if with_leaf then
                T.make_initializer_name_query ?selection ~leaf ~role ~name
                  ~origin ()
              else
                match selection with
                | None -> T.make_name_query ~role ~name ~origin
                | Some selection ->
                    T.make_selected_name_query ~selection ~role ~name ~origin
            in
            Some (checked event))
      selections
  in
  let accepts initializers events =
    let global = G.globals initializers |> List.hd in
    let input =
      T.make_global_initializer ~statement_index:0 ~initializers ~global events
      |> checked
    in
    T.resolve ~table
      ~parent:
        (Semantic_module_expression_binding.parent_scope prepared.expressions)
      ~module_expressions:prepared.expressions [ input ]
    |> Result.is_ok
  in
  Alcotest.(check (list bool))
    "second walk requires the original selection and leaf"
    [ true; false; false; false; false; false; false; false ]
    [
      accepts selected (events selections);
      accepts selected (events (select table));
      accepts selected (events ~unselected:true selections);
      accepts legacy (events selections);
      accepts selected (events ~with_leaf:false selections);
      accepts selected (events ~leaf:foreign_leaf selections);
      accepts selected (List.rev (events selections));
      accepts selected (events selections @ events selections);
    ];
  let copied =
    Semantic_global_resolution.resolve ~table
      ~parent:
        (Semantic_module_expression_binding.parent_scope prepared.expressions)
      ~compilation_mode:Semantic_global_resolution.Jit
      [ Semantic_global_resolution.global_record_declaration record ]
    |> checked |> Semantic_global_resolution.records |> List.hd
  in
  Alcotest.(check bool)
    "reconstructed record cannot replace the manifest owner" true
    (bind ~queries:pairs copied |> Result.is_error)

let initializer_braced_query_leaves () =
  let module G = Semantic_global_initializer_binding in
  let module T = Semantic_top_level_expression_binding in
  let module Q = Semantic_query_selection in
  let module I = Semantic_initializer_source in
  let receipts = ref [] in
  let query = function
    | Parser.Query_completed receipt ->
        receipts := receipt :: !receipts;
        Ok ()
    | _ -> Ok ()
  in
  let prepared =
    prepare ~query ~path:"initializer-braced-queries.hc"
      "I64 Values[2]={defined Missing,defined Values};"
  in
  let outer = jit_environment prepared [] [] in
  let table = Session.semantic_symbols prepared.session in
  let legacy = resolve prepared outer in
  let global = G.globals legacy |> List.hd in
  let leaves = G.global_leaves global in
  let selections =
    List.rev !receipts
    |> List.map (fun receipt -> Q.make ~table ~receipt () |> checked)
  in
  let bind pairs =
    let input =
      G.make_global ~queries:pairs ~record:(G.global_record global) []
      |> checked
    in
    G.resolve ~table ~environment:outer ~expressions:prepared.expressions
      ~globals:prepared.globals [ input ]
  in
  let selected =
    bind (List.combine leaves selections)
    |> Result.map_error G.error_to_string
    |> checked
  in
  List.iter
    (fun initializers ->
      let top =
        Holyc_lib.resolve_top_level_expressions prepared.session
          ~declarations:prepared.declarations
          ~module_expressions:prepared.expressions ~initializers prepared.ast
        |> checked
      in
      Alcotest.(check int)
        "both braced query leaves reach the second pass" 2
        (List.length (T.all_queries top)))
    [ legacy; selected ];
  Alcotest.(check (list (list int)))
    "queries retain distinct initializer paths" [ [ 0 ]; [ 1 ] ]
    (G.globals selected |> List.hd |> G.global_queries
    |> List.map (fun query -> G.query_leaf query |> I.leaf_path));
  Alcotest.(check bool)
    "swapped source leaves cannot replace original associations" true
    (bind (List.combine (List.rev leaves) selections) |> Result.is_error);
  let selection = List.hd selections in
  let role, name, origin =
    Q.name_query_facts (Q.expression selection) |> Option.get
  in
  Alcotest.(check bool)
    "another braced leaf cannot create the original name query" true
    (T.make_initializer_name_query ~selection ~leaf:(List.nth leaves 1) ~role
       ~name ~origin ()
    |> Result.is_error)

let tests =
  [
    Alcotest.test_case "initializer query reads retain braced leaf ownership"
      `Quick initializer_braced_query_leaves;
    Alcotest.test_case
      "selected initializer queries retain original reads and leaves" `Quick
      selected_initializer_query_manifest;
    Alcotest.test_case "initializer queries retain the complete source manifest"
      `Quick initializer_query_source_manifest;
    Alcotest.test_case "initializer second pass retains exact environment"
      `Quick selected_initializer_environment;
    Alcotest.test_case "self and comma source order" `Quick
      self_and_comma_source_order;
    Alcotest.test_case "prior module records and nested paths" `Quick
      prior_module_and_nested_paths;
    Alcotest.test_case "JIT and AOT outer chains" `Quick
      mode_specific_outer_chains;
    Alcotest.test_case "generated unresolved name keeps provenance" `Quick
      generated_unresolved_identifier_keeps_provenance;
    Alcotest.test_case "determinism, purity, and validation" `Quick
      determinism_purity_and_validation;
  ]
