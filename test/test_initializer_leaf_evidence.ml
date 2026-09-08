open Holyc_lib
module Source = Semantic_initializer_source
module Call = Semantic_function_call_resolution
module Tree = Semantic_top_level_expression_tree
module Initial = Semantic_global_initializer_binding
module Local = Semantic_local_type_resolution
module T = Test_top_level_expression_result
module F = Test_function_call_resolution

let checked = T.checked
let checked_tree = Test_top_level_expression_tree.checked_tree

let rejected label result =
  Alcotest.(check bool) label true (Result.is_error result)

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let global_tree mode contents =
  let prepared = T.prepared ~mode ~path:"initializer-leaf-global.HC" contents in
  let environment = T.empty_environment prepared in
  let globals =
    resolve_global_records prepared.session ~declarations:prepared.declarations
      ~globals:prepared.global_types ~compilation_mode:mode prepared.ast
    |> checked
  in
  let initializers =
    resolve_global_initializers prepared.session ~environment
      ~expressions:prepared.module_expressions ~globals prepared.ast
    |> checked
  in
  T.build_inputs ~environment ~initializers prepared |> fst

let global_owner root =
  match Tree.root_role root with
  | Tree.Global_initializer owner -> owner
  | _ -> Alcotest.fail "expected a global initializer root"

let global_statement tree name =
  Tree.statements tree
  |> List.find (fun statement ->
      Tree.statement_roots statement
      |> List.exists (fun root ->
          match Tree.root_role root with
          | Tree.Global_initializer owner ->
              Initial.global_symbol owner
              |> Semantic_symbol.name |> String.equal name
          | _ -> false))

let global_leaf root = Tree.root_initializer_leaf root |> Option.get

let root_calls statement root =
  Tree.statement_calls statement
  |> List.filter (fun call ->
      List.exists
        (( == ) (Tree.call_source call))
        (Tree.root_initializer_calls root))

let remake_global statement root ?index ?leaf ?expression ?calls () =
  Tree.make_initializer_root
    ~index:(Option.value index ~default:(Tree.root_index root))
    ~global:(global_owner root)
    ~leaf:(Option.value leaf ~default:(global_leaf root))
    ~expression:(Option.value expression ~default:(Tree.root_expression root))
    ~calls:(Option.value calls ~default:(root_calls statement root))
    ~origin:(Tree.root_origin root)

let remake_statement statement ?calls roots =
  Tree.make_statement
    ~source:(Tree.statement_source statement)
    ~roots
    ~calls:(Option.value calls ~default:(Tree.statement_calls statement))
    ~switch_cases:(Tree.statement_switch_cases statement)

let static_function mode contents =
  let prepared = F.prepare ~mode ~path:"initializer-leaf-static.HC" contents in
  F.resolve prepared |> checked |> fun calls -> F.function_named calls "F"

let local_source initial =
  Call.initializer_local initial
  |> Local.local_initializer |> Option.get |> Local.initializer_source
  |> Option.get

let local_leaf initial = Call.initializer_leaf initial |> Option.get

let remake_local initial ?index ?leaf ?local ?expression ?calls () =
  Call.make_initializer_leaf
    ~index:(Option.value index ~default:(Call.initializer_index initial))
    ~local:(Option.value local ~default:(Call.initializer_local initial))
    ~leaf:(Option.value leaf ~default:(local_leaf initial))
    ~expression:
      (Option.value expression ~default:(Call.initializer_expression initial))
    ~calls:(Option.value calls ~default:(Call.initializer_calls initial))
    ~origin:(Call.initializer_origin initial)

let source_call = function
  | Call.Direct_call direct -> Call.direct_source direct
  | Call.Indirect_call indirect -> Call.indirect_source indirect
  | Call.Deferred_call { call; _ } -> call

let remake_function function_ ?calls initializers =
  Call.make_function
    ~symbol:(Call.function_symbol function_)
    ~scope:(Call.function_scope function_)
    ~item_index:(Call.function_item_index function_)
    ~expression_statements:(Call.function_expression_statements function_)
    ~implicit_outputs:(Call.function_implicit_outputs function_)
    ~conditions:(Call.function_conditions function_)
    ~selectors:(Call.function_selectors function_)
    ~switch_cases:(Call.function_switch_cases function_)
    ~returns:(Call.function_returns function_)
    ~initializers
    (Option.value calls
       ~default:(List.map source_call (Call.function_calls function_)))

let expression_with_kind source kind =
  Call.make_argument_expression ~kind
    ~origin:(Call.argument_expression_origin source)

let literal_at source value =
  expression_with_kind source (Call.Integer_literal value)

let binary_expression source ~operator ~left ~right =
  match Call.argument_expression_kind source with
  | Call.Binary_expression binary ->
      Call.make_binary_argument_expression ~operator
        ~operator_origin:(Call.binary_operator_origin binary)
        ~left ~right
      |> checked
      |> expression_with_kind source
  | _ -> Alcotest.fail "expected a binary initializer leaf"

let expression_mutations literal binary =
  match Call.argument_expression_kind binary with
  | Call.Binary_expression retained ->
      [
        ("same-origin literal payload", literal, literal_at literal 99L);
        ( "same-origin binary operator",
          binary,
          binary_expression binary ~operator:Ir_opcode.Ic_sub
            ~left:(Call.binary_left retained)
            ~right:(Call.binary_right retained) );
        ( "same-origin child payload",
          binary,
          binary_expression binary
            ~operator:(Call.binary_operator retained)
            ~left:(literal_at (Call.binary_left retained) 99L)
            ~right:(Call.binary_right retained) );
        ( "reordered child subtrees",
          binary,
          binary_expression binary
            ~operator:(Call.binary_operator retained)
            ~left:(Call.binary_right retained)
            ~right:(Call.binary_left retained) );
      ]
  | _ -> Alcotest.fail "expected a binary initializer leaf"

let remake_call call arguments =
  Call.make_call ~index:(Call.call_index call)
    ~callee_occurrence_index:(Call.call_callee_occurrence_index call)
    ~callee_name:(Call.call_callee_name call)
    ~callee_origin:(Call.call_callee_origin call)
    ~callee_form:(Call.call_callee_form call)
    ?callable:(Call.call_callable call)
    ?computed_callee:(Call.call_computed_callee call)
    ~origin:(Call.call_origin call) ~syntax:(Call.call_syntax call) arguments
  |> checked

let changed_call_argument call =
  match Call.call_arguments call with
  | [ argument ] ->
      let expression = Call.argument_expression argument |> Option.get in
      let replacement =
        Call.make_argument
          ~index:(Call.argument_index argument)
          ~kind:(Call.argument_kind argument)
          ~expression:(Some (literal_at expression 99L))
          ~origin:(Call.argument_origin argument)
        |> checked
      in
      remake_call call [ replacement ]
  | _ -> Alcotest.fail "expected one retained call argument"

let check_manifest label manifest leaves paths =
  Alcotest.(check (list (list int)))
    (label ^ " paths") paths
    (List.map Source.leaf_path leaves);
  Alcotest.(check (list int))
    (label ^ " leaf indexes")
    (List.mapi (fun index _ -> index) leaves)
    (List.map Source.leaf_index leaves);
  Alcotest.(check bool)
    (label ^ " exact ordered leaves")
    true
    (List.length leaves = List.length (Source.leaves manifest)
    && List.for_all2 ( == ) leaves (Source.leaves manifest));
  Alcotest.(check bool)
    (label ^ " complete original source")
    true
    (Source.matches_ast manifest (Source.source_ast manifest));
  List.iter
    (fun leaf ->
      Alcotest.(check bool)
        (label ^ " physical membership")
        true
        (Source.owns_leaf manifest leaf);
      Alcotest.(check int)
        (label ^ " literal-only leaf retained")
        0
        (List.length (Source.leaf_identifiers leaf)))
    leaves

let valid_manifests () =
  List.iter
    (fun mode ->
      let tree =
        global_tree mode "I64 A[2][2]={{10,10},{20,2}};U8 Msg[3]=\"42\";"
      in
      let group = global_statement tree "A" in
      let roots = Tree.statement_roots group in
      let manifest =
        Initial.global_source (global_owner (List.hd roots)) |> Option.get
      in
      check_manifest "global braced" manifest
        (List.map global_leaf roots)
        [ [ 0; 0 ]; [ 0; 1 ]; [ 1; 0 ]; [ 1; 1 ] ];
      ignore (remake_statement group roots |> checked_tree);
      let message =
        global_statement tree "Msg" |> Tree.statement_roots |> List.hd
      in
      let source = Initial.global_source (global_owner message) |> Option.get in
      check_manifest "global string" source [ global_leaf message ] [ [] ];
      let function_ =
        static_function mode
          "I64 F(){static I64 A[2][2]={{10,10},{20,2}};static U8 \
           Msg[3]=\"42\";return 0;}"
      in
      let initializers = Call.function_initializers function_ in
      let array =
        List.filter
          (fun initial ->
            Call.initializer_local initial
            |> Local.local_symbol |> Semantic_symbol.name |> String.equal "A")
          initializers
      in
      check_manifest "static braced"
        (local_source (List.hd array))
        (List.map local_leaf array)
        [ [ 0; 0 ]; [ 0; 1 ]; [ 1; 0 ]; [ 1; 1 ] ];
      let message = List.nth initializers 4 in
      check_manifest "static string" (local_source message)
        [ local_leaf message ]
        [ [] ];
      ignore (remake_function function_ initializers |> checked))
    modes

let global_expression_mutations () =
  List.iter
    (fun mode ->
      let tree =
        global_tree mode
          "I64 Id(I64 n){return n;}I64 A[3]={40,20+22,Id(Id(2))};"
      in
      let statement = global_statement tree "A" in
      let roots = Tree.statement_roots statement in
      let first = List.nth roots 0 and binary = List.nth roots 1 in
      List.iter
        (fun (label, original, replacement) ->
          let root =
            List.find (fun root -> Tree.root_expression root == original) roots
          in
          rejected ("global " ^ label)
            (remake_global statement root ~expression:replacement ()))
        (expression_mutations
           (Tree.root_expression first)
           (Tree.root_expression binary));
      let root = List.nth roots 2 in
      let calls = root_calls statement root in
      Alcotest.(check int) "nested global calls retained" 2 (List.length calls);
      let inner = List.nth calls 1 in
      let changed =
        Tree.make_call
          ~source:(changed_call_argument (Tree.call_source inner))
          ~callee:(Tree.call_callee inner)
          ~callee_expression:(Tree.call_callee_expression inner)
          ~result_expression:(Tree.call_result_expression inner)
        |> checked_tree
      in
      rejected "global nested call argument payload"
        (remake_global statement root ~calls:[ List.hd calls; changed ] ());
      List.iter
        (fun (label, calls) ->
          rejected label (remake_global statement root ~calls ()))
        [
          ("global missing calls", []);
          ("global reordered calls", List.rev calls);
          ("global duplicate call", [ List.hd calls; List.hd calls ]);
        ])
    modes

let global_batch_and_call_ownership () =
  List.iter
    (fun mode ->
      let tree = global_tree mode "I64 A[2]={40,2};I64 B[2]={40,2};" in
      let statement = global_statement tree "A" in
      let roots = Tree.statement_roots statement in
      let first = List.hd roots and second = List.nth roots 1 in
      let reindex values =
        List.mapi
          (fun index root ->
            remake_global statement root ~index () |> checked_tree)
          values
      in
      List.iter
        (fun (label, values) ->
          rejected label (remake_statement statement (reindex values)))
        [
          ("global omitted batch", []);
          ("global missing leaf", [ first ]);
          ("global reordered leaves", [ second; first ]);
          ("global duplicate leaves", [ first; first ]);
        ];
      let foreign = global_statement tree "B" |> Tree.statement_roots in
      rejected "global foreign declaration batch"
        (remake_statement statement (reindex foreign));
      rejected "global foreign declaration leaf"
        (remake_global statement first ~leaf:(global_leaf (List.hd foreign)) ());
      let manifest = Initial.global_source (global_owner first) |> Option.get in
      let rebuilt =
        Source.create (Source.source_ast manifest) |> Source.leaves |> List.hd
      in
      rejected "global reconstructed same-AST leaf"
        (remake_global statement first ~leaf:rebuilt ());
      let tree =
        global_tree mode "I64 Id(I64 n){return n;}I64 A[2]={Id(40),2};"
      in
      let statement = global_statement tree "A" in
      let roots = Tree.statement_roots statement in
      let root = List.hd roots in
      let call = Tree.statement_calls statement |> List.hd in
      let copy expression =
        expression_with_kind expression
          (Call.argument_expression_kind expression)
      in
      let replacement ~callee_expression ~result_expression =
        Tree.make_call ~source:(Tree.call_source call)
          ~callee:(Tree.call_callee call) ~callee_expression ~result_expression
        |> checked_tree
      in
      let foreign_callee =
        replacement
          ~callee_expression:(copy (Tree.call_callee_expression call))
          ~result_expression:(Tree.call_result_expression call)
      in
      rejected "same call with foreign callee tree at batch join"
        (remake_statement statement ~calls:[ foreign_callee ] roots);
      let foreign_result =
        replacement
          ~callee_expression:(Tree.call_callee_expression call)
          ~result_expression:(copy (Tree.call_result_expression call))
      in
      rejected "same call with foreign result tree at batch join"
        (remake_statement statement ~calls:[ foreign_result ] roots);
      rejected "same call with disconnected result at leaf constructor"
        (remake_global statement root ~calls:[ foreign_result ] ()))
    modes

let static_expression_mutations () =
  List.iter
    (fun mode ->
      let function_ =
        static_function mode
          "I64 Id(I64 n){return n;}I64 F(){static I64 \
           A[3]={40,20+22,Id(Id(2))};return 0;}"
      in
      let initializers = Call.function_initializers function_ in
      let first = List.nth initializers 0
      and binary = List.nth initializers 1 in
      List.iter
        (fun (label, original, replacement) ->
          let initial =
            List.find
              (fun initial -> Call.initializer_expression initial == original)
              initializers
          in
          rejected ("static " ^ label)
            (remake_local initial ~expression:replacement ()))
        (expression_mutations
           (Call.initializer_expression first)
           (Call.initializer_expression binary));
      let initial = List.nth initializers 2 in
      let calls = Call.initializer_calls initial in
      Alcotest.(check int) "nested static calls retained" 2 (List.length calls);
      rejected "static nested call argument payload"
        (remake_local initial
           ~calls:[ List.hd calls; changed_call_argument (List.nth calls 1) ]
           ());
      List.iter
        (fun (label, calls) -> rejected label (remake_local initial ~calls ()))
        [
          ("static missing calls", []);
          ("static reordered calls", List.rev calls);
          ("static duplicate call", [ List.hd calls; List.hd calls ]);
        ])
    modes

let static_batch_and_call_ownership () =
  List.iter
    (fun mode ->
      let function_ =
        static_function mode
          "I64 F(){static I64 A[2]={40,2};static I64 B[2]={40,2};return 0;}"
      in
      let initializers = Call.function_initializers function_ in
      let first = List.nth initializers 0
      and second = List.nth initializers 1
      and third = List.nth initializers 2
      and fourth = List.nth initializers 3 in
      let reindex values =
        List.mapi
          (fun index initial -> remake_local initial ~index () |> checked)
          values
      in
      List.iter
        (fun (label, values) ->
          rejected label (remake_function function_ (reindex values)))
        [
          ("static missing leaf", [ first; third; fourth ]);
          ("static reordered leaves", [ second; first; third; fourth ]);
          ("static duplicate leaves", [ first; first; third; fourth ]);
          ("static repeated declaration", initializers @ [ first; second ]);
          ("static reordered declarations", [ third; fourth; first; second ]);
        ];
      rejected "static foreign declaration leaf"
        (remake_local first ~leaf:(local_leaf third) ());
      let rebuilt =
        Source.create (Source.source_ast (local_source first))
        |> Source.leaves |> List.hd
      in
      rejected "static reconstructed same-AST leaf"
        (remake_local first ~leaf:rebuilt ());
      let function_ =
        static_function mode
          "I64 Id(I64 n){return n;}I64 F(){static I64 A[2]={Id(40),2};return \
           0;}"
      in
      let initializers = Call.function_initializers function_ in
      let first = List.hd initializers in
      let call = Call.initializer_calls first |> List.hd in
      let foreign = remake_call call (Call.call_arguments call) in
      let replacement = remake_local first ~calls:[ foreign ] () |> checked in
      rejected "static foreign call publication"
        (remake_function function_ (replacement :: List.tl initializers));
      rejected "static missing function call publication"
        (remake_function function_ ~calls:[] initializers))
    modes

let global_scalar_legacy_fallback () =
  List.iter
    (fun mode ->
      let tree = global_tree mode "I64 A=42;" in
      let statement = global_statement tree "A" in
      let root = Tree.statement_roots statement |> List.hd in
      let unchanged = remake_global statement root () |> checked_tree in
      ignore (remake_statement statement [ unchanged ] |> checked_tree);
      let replacement = literal_at (Tree.root_expression root) 99L in
      rejected "source-backed scalar leaf rejects changed payload"
        (remake_global statement root ~expression:replacement ());
      let unmarked =
        Tree.make_root ~index:(Tree.root_index root) ~role:(Tree.root_role root)
          ~expression:replacement ~origin:(Tree.root_origin root)
        |> checked_tree
      in
      rejected "legacy global root cannot bypass a retained scalar source"
        (remake_statement statement [ unmarked ]))
    modes

let local_scalar_legacy_fallback () =
  List.iter
    (fun mode ->
      let function_ =
        static_function mode "I64 F(){static I64 A=42;return A;}"
      in
      let initial = Call.function_initializers function_ |> List.hd in
      let unchanged = remake_local initial () |> checked in
      ignore (remake_function function_ [ unchanged ] |> checked);
      let local = Call.initializer_local initial in
      let original = Local.local_initializer local |> Option.get in
      let legacy_initial =
        Local.make_initializer
          ~kind:(Local.initializer_kind original)
          ~origin:(Local.initializer_origin original)
          ~equals_origin:(Local.initializer_equals_origin original)
          ~value_origin:(Local.initializer_value_origin original)
      in
      let legacy_local =
        Local.make_local ~symbol:(Local.local_symbol local)
          ~declaration_index:(Local.local_declaration_index local)
          ~declarator_index:(Local.local_declarator_index local)
          ~declaration_origin:(Local.local_declaration_origin local)
          ~declarator_origin:(Local.local_declarator_origin local)
          ~storage:(Local.local_storage local)
          ~storage_origins:(Local.local_storage_origins local)
          ~type_reference:(Local.local_type_reference local)
          ~register_requests:(Local.local_register_requests local)
          ~declarator_kind:(Local.local_declarator_kind local)
          ~array_dimensions:(Local.local_array_dimensions local)
          ~initial_value:(Some legacy_initial)
          ~delimiter:(Local.local_delimiter local)
          ()
        |> checked
      in
      Alcotest.(check bool)
        "legacy scalar has no source manifest" true
        (Option.is_none (Local.initializer_source legacy_initial));
      let legacy =
        Call.make_initializer
          ~index:(Call.initializer_index initial)
          ~local:legacy_local
          ~expression:(Call.initializer_expression initial)
          ~origin:(Call.initializer_origin initial)
        |> checked
      in
      ignore (remake_function function_ [ legacy ] |> checked);
      let replacement = literal_at (Call.initializer_expression initial) 99L in
      rejected "source-backed static scalar leaf rejects changed payload"
        (remake_local initial ~expression:replacement ());
      rejected "legacy local root cannot bypass a retained scalar source"
        (Call.make_initializer
           ~index:(Call.initializer_index initial)
           ~local ~expression:replacement
           ~origin:(Call.initializer_origin initial)))
    modes

let tests =
  [
    Alcotest.test_case "valid global and static manifests" `Quick
      valid_manifests;
    Alcotest.test_case "global leaf expression witnesses" `Quick
      global_expression_mutations;
    Alcotest.test_case "global leaf batches and call ownership" `Quick
      global_batch_and_call_ownership;
    Alcotest.test_case "static leaf expression witnesses" `Quick
      static_expression_mutations;
    Alcotest.test_case "static leaf batches and call ownership" `Quick
      static_batch_and_call_ownership;
    Alcotest.test_case "global scalar legacy source fallback" `Quick
      global_scalar_legacy_fallback;
    Alcotest.test_case "local scalar legacy source fallback" `Quick
      local_scalar_legacy_fallback;
  ]
