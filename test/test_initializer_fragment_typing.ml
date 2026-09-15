open Holyc_lib
module D = Task_declarations
module Task = Integer_task
module Typed = Semantic_function_call_expression_result
module Tree = Semantic_top_level_expression_tree
module Fragment = Semantic_initializer_fragment
module Source = Semantic_initializer_source

let checked = Test_declaration_collection.checked
let expect = Test_integer_program.checked

let collect ?(execute_command = fun _ -> true) text =
  let session, source, ledger = Test_source_promotion.inputs text in
  let retained = ref None in
  let leaves = ref [] in
  let task () = Option.get !retained in
  let declaration event =
    Result.bind (D.observe ledger event) (fun () ->
        match event with
        | Parser.Global_declared publication ->
            Task.admit_global (task ()) publication
        | Parser.Parameter_default_completed _
        | Parser.Function_header_completed _ ->
            Task.observe_initializer (task ()) event
        | Parser.Global_initializer_leaf_completed receipt ->
            let table = Session.semantic_symbols session in
            let before =
              ( List.length (Semantic_symbol_table.all_symbols table),
                List.length (Semantic_symbol_table.all_scopes table) )
            in
            let effects =
              (Task.executed_steps (task ()), Task.initializer_steps (task ()))
            in
            Result.map
              (fun typed ->
                Alcotest.(check (pair int int))
                  "fragment typing creates no symbols or scopes" before
                  ( List.length (Semantic_symbol_table.all_symbols table),
                    List.length (Semantic_symbol_table.all_scopes table) );
                Alcotest.(check (pair int int))
                  "typing executes no instructions or preparation" effects
                  ( Task.executed_steps (task ()),
                    Task.initializer_steps (task ()) );
                leaves := (receipt, typed) :: !leaves)
              (Task.prepare_initializer (task ()) receipt)
        | _ -> Ok ())
  in
  let checkpoint event =
    Result.bind (D.observe_command ledger event) (fun () ->
        match event with
        | Parser.Sequence_started _ when Option.is_none !retained ->
            retained :=
              Some (Task.adopt_source session ~source ~ledger |> checked);
            Ok ()
        | Parser.Command_resumed completed
          when execute_command completed.command_ast ->
            Result.bind
              (Task.compile_source_ast (task ()) completed.command_ast)
              (fun command ->
                Result.map ignore (Task.execute (task ()) command))
        | _ -> Ok ())
  in
  let parsed =
    Test_source_promotion.parse ~declaration ~checkpoint session source ledger
  in
  ignore (Test_parser.expect_ast parsed);
  (session, task (), List.rev !leaves)

let root typed =
  match
    Typed.top_level_statements typed
    |> List.concat_map Typed.top_level_statement_roots
  with
  | [ root ] -> root
  | _ -> Alcotest.fail "expected exactly one initializer fragment root"

let fragment typed =
  match Tree.root_role (Typed.top_level_root_source (root typed)) with
  | Tree.Initializer_fragment fragment -> fragment
  | _ -> Alcotest.fail "expected explicit initializer fragment role"

let retained_leaf () =
  let _, _, leaves = collect {|I64 X=1;I64 N=++X+X;|} in
  List.iter
    (fun (receipt, typed) ->
      let fragment = fragment typed in
      Alcotest.(check bool)
        "exact parser receipt" true
        (Option.fold ~none:false ~some:(( == ) receipt)
           (Source.leaf_parser_receipt (Fragment.leaf fragment)));
      Alcotest.(check bool)
        "initializer value is not discarded" true
        (Option.is_none (Typed.top_level_root_result_use (root typed))))
    leaves

let array_queries () =
  let _, _, leaves =
    collect {|I64 A[2]={1,2};I64 N=A[0]+sizeof(A)+defined(A);|}
  in
  let typed = snd (List.hd (List.rev leaves)) in
  let fragment = fragment typed in
  Alcotest.(check int)
    "selected indexed array" 1
    (List.length (Fragment.references fragment));
  Alcotest.(check int)
    "frozen query reads" 2
    (List.length (Fragment.queries fragment))

let retained_call () =
  let _, _, leaves =
    collect {|I64 Add(I64 a,I64 b){return a+b;};I64 N=Add(20,22);|}
  in
  let typed = snd (List.hd leaves) in
  Alcotest.(check int)
    "shared type checker resolves retained direct call" 1
    (List.length (Typed.top_level_direct_calls typed))

let rejects label = function
  | Error _ -> ()
  | Ok _ -> Alcotest.fail label

let query_evidence () =
  let session, _, leaves =
    collect {|I64 A[2]={1,2};I64 N=sizeof(A)+defined(A);|}
  in
  let fragment = fragment (snd (List.hd (List.rev leaves))) in
  let recreate queries =
    Fragment.create
      ~table:(Session.semantic_symbols session)
      ~declaration:(Fragment.declaration fragment)
      ~leaf:(Fragment.leaf fragment)
      ~environment:(Fragment.environment fragment)
      ~references:(Fragment.references fragment)
      ~queries
  in
  let queries = Fragment.queries fragment in
  ignore (recreate queries |> checked);
  rejects "missing original query" (recreate (List.tl queries));
  rejects "reordered original query" (recreate (List.rev queries))

let nested_default_calls () =
  let _, _, leaves =
    collect
      ~execute_command:(fun (ast : Ast.module_) ->
        List.for_all
          (function
            | Ast.Global_variable _ | Ast.Global_declaration _ -> false
            | _ -> true)
          ast.items)
      {|I64 Seed(I64 n=21){return n;};I64 Sum(I64 a,I64 b){return a+b;};I64 N=Sum(Seed(),Seed());|}
  in
  let typed = snd (List.hd leaves) in
  Alcotest.(check int)
    "nested retained calls use existing defaults" 3
    (List.length (Typed.top_level_direct_calls typed))

let copied_leaves () =
  let _, _, leaves =
    collect ~execute_command:(fun _ -> false) {|U8 A[2][3]={"AB","CD"};|}
  in
  Alcotest.(check int)
    "both copied source expressions typed while incomplete" 2
    (List.length leaves);
  List.iter (fun (_, typed) -> ignore (fragment typed)) leaves

let evidence () =
  let session, task, leaves = collect {|I64 X=1;I64 N=X+X;|} in
  let receipt, typed = List.hd (List.rev leaves) in
  rejects "stale leaf callback must fail"
    (Task.prepare_initializer task receipt);
  let first = fragment (snd (List.hd leaves)) in
  let fragment = fragment typed in
  let table = Session.semantic_symbols session in
  let recreate ?(declaration = Fragment.declaration fragment)
      ?(leaf = Fragment.leaf fragment)
      ?(environment = Fragment.environment fragment) references queries =
    Fragment.create ~table ~declaration ~leaf ~environment ~references ~queries
  in
  let references = Fragment.references fragment in
  let queries = Fragment.queries fragment in
  ignore (recreate references queries |> checked);
  rejects "missing reference" (recreate (List.tl references) queries);
  rejects "reordered reference" (recreate (List.rev references) queries);
  let _, selection = List.hd references in
  let clone, _ = List.hd (List.tl references) in
  rejects "substituted equal-name identifier"
    (recreate ((clone, selection) :: List.tl references) queries);
  rejects "foreign declaration"
    (recreate ~declaration:(Fragment.declaration first) references queries);
  rejects "foreign snapshot"
    (recreate ~environment:(Fragment.environment first) references queries)

let foreign_occurrence () =
  let module Binding = Semantic_top_level_expression_binding in
  let module Outer = Semantic_top_level_outer_expression_binding in
  let module Call = Semantic_function_call_resolution in
  let session, _, leaves = collect {|I64 X=1;I64 N=X;|} in
  let typed = snd (List.hd (List.rev leaves)) in
  let fragment = fragment typed in
  let tree = Typed.top_level_source typed in
  let outer = Tree.source tree in
  let original = List.hd (Outer.statements outer) in
  let table = Session.semantic_symbols session in
  let symbol =
    Fragment.declaration fragment
    |> Semantic_compiler_record.declared_global_symbol
  in
  let parent =
    Semantic_symbol_table.all_scopes table
    |> List.find (fun scope ->
        Semantic_symbol.Scope_id.equal
          (Semantic_symbol_table.scope_id scope)
          (Semantic_symbol.scope_id symbol))
  in
  let leaf = Fragment.leaf fragment in
  let events =
    Fragment.references fragment
    |> List.map (fun ((identifier : Ast.identifier), selection) ->
        Binding.make_selected_initializer_identifier ~selection ~leaf
          ~name:identifier.spelling
          ~origin:(Source.origin_of_location identifier.location)
        |> checked)
  in
  let input = Binding.make_initializer_fragment ~fragment events |> checked in
  let bindings =
    Binding.resolve ~table ~parent
      ~module_expressions:(outer |> Outer.source |> Binding.module_expressions)
      [ input ]
    |> Result.map_error Binding.error_to_string
    |> checked
  in
  let foreign =
    Outer.resolve ~table
      ~environment:(Fragment.environment fragment)
      ~expressions:bindings
    |> Result.map_error Outer.error_to_string
    |> checked
  in
  let occurrence = List.hd (Outer.all_occurrences foreign) in
  let kind =
    Call.make_top_level_bound_identifier_argument_expression ~occurrence
    |> checked
  in
  let expression =
    Call.make_argument_expression ~kind ~origin:(Source.leaf_origin leaf)
  in
  let root =
    Tree.make_fragment_root ~index:0 ~fragment ~expression ~calls:[]
    |> Result.map_error Tree.error_to_string
    |> checked
  in
  let unmarked =
    Tree.make_root ~index:0 ~role:(Tree.Initializer_fragment fragment)
      ~expression ~origin:(Source.leaf_origin leaf)
    |> Result.map_error Tree.error_to_string
    |> checked
  in
  rejects "generic maker must not supply fragment leaf evidence"
    (Tree.make_statement ~source:original ~roots:[ unmarked ] ~calls:[]
       ~switch_cases:[]);
  let substitution =
    Result.bind
      (Tree.make_statement ~source:original ~roots:[ root ] ~calls:[]
         ~switch_cases:[]) (fun statement ->
        Tree.create ~table ~source:outer [ statement ])
  in
  rejects "foreign equal-name/origin occurrence must not enter fragment tree"
    substitution

let tests =
  [
    Alcotest.test_case "retained original leaf and update" `Quick retained_leaf;
    Alcotest.test_case "array indexing and frozen queries" `Quick array_queries;
    Alcotest.test_case "retained direct call" `Quick retained_call;
    Alcotest.test_case "exact evidence and stale callback rejection" `Quick
      evidence;
    Alcotest.test_case "foreign equal-source occurrence rejection" `Quick
      foreign_occurrence;
    Alcotest.test_case "query transcript rejection" `Quick query_evidence;
    Alcotest.test_case "nested retained defaults" `Quick nested_default_calls;
    Alcotest.test_case "copied source leaves" `Quick copied_leaves;
  ]
