open Holyc_lib
module D = Task_declarations
module Source = Semantic_initializer_source

let checked = Test_declaration_collection.checked
let expect = Test_integer_program.checked

let reject label result =
  Alcotest.(check bool) label true (Result.is_error result)

let leaf_events events =
  List.filter_map
    (function
      | Parser.Global_initializer_leaf_completed leaf -> Some leaf
      | _ -> None)
    events

let declarators ast =
  List.concat_map
    (function
      | Ast.Global_declaration declaration -> declaration.declarators
      | _ -> [])
    ast.Ast.items

let original_leaves value =
  let rec walk path value =
    match value with
    | Ast.Scalar_initializer _ -> [ (path, value) ]
    | Ast.Braced_initializer group -> elements path group.initializer_elements
    | Ast.Unbraced_array_initializer group ->
        elements path group.unbraced_initializer_elements
  and elements path values =
    List.mapi
      (fun index (value : Ast.initializer_element) ->
        walk (path @ [ index ]) value.initializer_element_value)
      values
    |> List.concat
  in
  walk [] value

let receipt_identity text paths =
  let events = ref [] in
  let commands =
    Test_stream_parser.declaration_sink (fun event ->
        events := event :: !events;
        (match event with
        | Parser.Global_initializer_started start ->
            Alcotest.(check bool)
              "start callback is current" true
              (Parser.initializer_start_is_current start)
        | Parser.Global_initializer_leaf_completed leaf ->
            Alcotest.(check bool)
              "leaf callback is current" true
              (Parser.initializer_leaf_is_current leaf);
            Alcotest.(check bool)
              "start is no longer current" false
              (Parser.initializer_start_is_current leaf.leaf_initializer)
        | _ -> ());
        Ok ())
  in
  let _, _, output, _, _, _ =
    Test_stream_parser.parse ~same_task:true ~commands text
  in
  let ast = Test_parser.expect_ast output in
  let leaves = leaf_events (List.rev !events) in
  let declarator = List.hd (declarators ast) in
  let initial = Option.get declarator.global_initial_value in
  let originals = original_leaves initial.global_initializer_value in
  Alcotest.(check (list (list int)))
    "original syntax paths" paths
    (List.map (fun leaf -> leaf.Parser.leaf_path) leaves);
  Alcotest.(check int)
    "one callback per source leaf" (List.length originals) (List.length leaves);
  List.iteri
    (fun index (leaf : Parser.completed_initializer_leaf) ->
      let path, value = List.nth originals index in
      Alcotest.(check bool)
        "leaf reuses original syntax" true
        (leaf.leaf_value == value && leaf.leaf_path = path);
      Alcotest.(check int) "original ordinal" index leaf.leaf_index;
      Alcotest.(check bool)
        "original predecessor" true
        (match leaf.leaf_predecessor with
        | None -> index = 0
        | Some previous -> previous == List.nth leaves (index - 1));
      Alcotest.(check bool)
        "original equals location" true
        (leaf.leaf_initializer.initializer_equals
       == initial.global_initializer_equals);
      Alcotest.(check bool)
        "original declarator owner" true
        (leaf.leaf_initializer.initializer_owner.global_name == declarator.name);
      Alcotest.(check bool)
        "callback ended" false
        (Parser.initializer_leaf_is_current leaf))
    leaves

let shapes () =
  List.iter
    (fun (text, paths) -> receipt_identity text paths)
    [
      ("I64 A=42;", [ [] ]);
      ( "I64 A[2][2]={{10,20},{5,7}};",
        [ [ 0; 0 ]; [ 0; 1 ]; [ 1; 0 ]; [ 1; 1 ] ] );
      ("I64 A[2][2]={10,20,5,7};", [ [ 0 ]; [ 1 ]; [ 2 ]; [ 3 ] ]);
      ("I64 A[2][2]=10,20,5,7;", [ [ 0; 0 ]; [ 0; 1 ]; [ 1; 0 ]; [ 1; 1 ] ]);
      ("U8 A[2][3]={\"AB\",\"CD\"};", [ [ 0 ]; [ 1 ] ]);
      ({|U8 A[4]="A"#exe {"\"B\"";}"C";|}, [ [] ]);
    ]

let inputs text =
  let session = Session.task_frontend (Session.create ()) in
  let source = Session.add_source session ~path:"leaves.hc" ~contents:text in
  let ledger = D.create_source session ~source |> checked in
  (session, source, ledger)

let retained_manifest () =
  let session, source, ledger = inputs "I64 A[2]={40,2};" in
  let saved = ref [] in
  let observe event =
    let result = D.observe ledger event in
    (match event with
    | Parser.Global_initializer_leaf_completed receipt ->
        ignore (result |> expect);
        saved :=
          (receipt, D.initializer_leaf_for ledger receipt |> expect) :: !saved;
        reject "same leaf cannot be observed twice" (D.observe ledger event)
    | Parser.Global_initializer_started _ ->
        ignore (result |> expect);
        reject "same start cannot be observed twice" (D.observe ledger event)
    | _ -> ());
    result
  in
  let output, events =
    Test_task_declarations.parse_source ~observe session ledger source
  in
  let ast = Test_parser.expect_ast output in
  let declaration = List.hd (declarators ast) in
  let initial = Option.get declaration.global_initial_value in
  let command = D.seal_source ledger ast |> expect in
  let table = Session.semantic_symbols session in
  let manifest =
    D.source_initializer_for ~table ~ast command declaration.name initial
    |> expect
  in
  let leaves = Source.leaves manifest in
  Alcotest.(check bool)
    "complete source keeps original AST" true
    (Source.source_ast manifest == initial.global_initializer_value);
  List.iter2
    (fun (receipt, saved) leaf ->
      Alcotest.(check bool)
        "completion reuses early semantic leaf" true (saved == leaf);
      Alcotest.(check bool)
        "semantic leaf retains original callback" true
        (Option.get (Source.leaf_parser_receipt leaf) == receipt))
    (List.rev !saved) leaves;
  List.iter
    (fun event ->
      match event with
      | Parser.Global_initializer_started start ->
          reject "delayed start cannot create another transcript"
            (Source.begin_parser start)
      | _ -> ())
    events;
  let copy =
    Ast.make_global_initializer ~equals:initial.global_initializer_equals
      ~value:initial.global_initializer_value
      ~location:initial.global_initializer_location
  in
  reject "equal initializer wrapper is not original"
    (D.source_initializer_for ~table ~ast command declaration.name copy);
  let wrapper = Test_task_declarations.copy_module ast ast.items in
  reject "foreign command wrapper"
    (D.source_initializer_for ~table ~ast:wrapper command declaration.name
       initial);
  let foreign = Session.create () in
  reject "foreign semantic table"
    (D.source_initializer_for
       ~table:(Session.semantic_symbols foreign)
       ~ast command declaration.name initial)

let missing_phase skip =
  let session, source, ledger = inputs "I64 A[2]={40,2};" in
  let observe event = if skip event then Ok () else D.observe ledger event in
  let output, _ =
    Test_task_declarations.parse_source ~observe session ledger source
  in
  Alcotest.(check bool)
    "missing initializer phase stops completion" true (Parser.has_errors output)

let missing_phases () =
  missing_phase (function
    | Parser.Global_initializer_started _ -> true
    | _ -> false);
  missing_phase (function
    | Parser.Global_initializer_leaf_completed leaf when leaf.leaf_index = 0 ->
        true
    | _ -> false);
  missing_phase (function
    | Parser.Global_initializer_leaf_completed leaf when leaf.leaf_index = 1 ->
        true
    | _ -> false)

let delayed_leaf () =
  let session, source, ledger = inputs "I64 A[2]={40,2};" in
  let delayed = ref None in
  let observe event =
    match event with
    | Parser.Global_initializer_leaf_completed leaf when leaf.leaf_index = 0 ->
        delayed := Some event;
        Ok ()
    | Parser.Global_initializer_leaf_completed _ ->
        reject "unobserved prior leaf cannot arrive during next callback"
          (D.observe ledger (Option.get !delayed));
        D.observe ledger event
    | _ -> D.observe ledger event
  in
  let output, _ =
    Test_task_declarations.parse_source ~observe session ledger source
  in
  Alcotest.(check bool)
    "delayed transcript remains incomplete" true (Parser.has_errors output)

exception Stop_leaf

let failure_lifetime raises =
  let entered = ref 0 in
  let saved = ref None in
  let commands =
    Test_stream_parser.declaration_sink (fun event ->
        match event with
        | Parser.Global_initializer_leaf_completed leaf ->
            saved := Some leaf;
            if raises then raise Stop_leaf
            else
              Error
                [
                  Diagnostic.make ~code:"TESTLEAF" ~severity:Diagnostic.Error
                    ~primary:leaf.leaf_initializer.initializer_equals.span
                    ~message:"rejected leaf" ();
                ]
        | _ -> Ok ())
  in
  (try
     let _, _, output, _, _, _ =
       Test_stream_parser.parse ~same_task:true ~commands
         ~on_enter:(fun () -> incr entered)
         {|I64 A[2]={40,#exe {"2";}};|}
     in
     if raises then Alcotest.fail "expected callback exception";
     Alcotest.(check bool)
       "callback error stops parsing" true (Parser.has_errors output)
   with Stop_leaf -> if not raises then raise Stop_leaf);
  Alcotest.(check int) "later directive did not run" 0 !entered;
  Alcotest.(check bool)
    "failure revokes callback lifetime" false
    (Parser.initializer_leaf_is_current (Option.get !saved))

let promoted_ir text expected_offsets =
  let module VM = Ir_integer_interpreter in
  let module Typed = Semantic_function_call_expression_result in
  let module Tree = Semantic_top_level_expression_tree in
  let module Initial = Semantic_global_initializer_binding in
  let module Init = Ir_global_initialization in
  let session, source, ledger = inputs text in
  let table = Session.semantic_symbols session in
  let runtime = VM.create_task_state ~table () |> checked in
  let early = ref [] in
  let promoted = ref false in
  let observe event =
    let result = D.observe ledger event in
    (match event with
    | Parser.Global_initializer_leaf_completed receipt ->
        ignore (result |> expect);
        early := (D.initializer_leaf_for ledger receipt |> expect) :: !early;
        if receipt.leaf_index = 1 then (
          D.promote_source ledger ~runtime session ~source |> checked;
          promoted := true)
    | _ -> ());
    result
  in
  let output, _ =
    Test_task_declarations.parse_source ~observe
      ~reference:(D.observe_reference ledger)
      session ledger source
  in
  let ast = Test_parser.expect_ast output in
  Alcotest.(check bool) "promoted during original initializer" true !promoted;
  let command = D.seal ledger ast |> expect in
  let program =
    compile_integer_task_ast ~task:runtime ~declaration_command:command session
      ~config:(Test_task_declarations.config ())
      ast
    |> expect
  in
  let program = program.value in
  let early = List.rev !early in
  let numeric_roots =
    Integer_initializer_preparation.items
      (integer_program_initializer_preparation program)
    |> List.map Integer_initializer_preparation.root
  in
  let publications =
    Init.publications (integer_program_initialization program)
  in
  let roots =
    List.map
      (fun publication ->
        match (Init.describe_publication publication).prepared_root with
        | Init.Prepared_global root -> root
        | _ -> Alcotest.fail "unexpected static publication")
      publications
  in
  let root_leaf root =
    Typed.top_level_root_source root |> Tree.root_initializer_leaf |> Option.get
  in
  List.iter
    (fun root ->
      Alcotest.(check bool)
        "numeric preparation retains original leaf" true
        (List.exists (( == ) (root_leaf root)) early))
    numeric_roots;
  Alcotest.(check int)
    "one prepared root per original leaf" (List.length early)
    (List.length roots);
  List.iter2
    (fun leaf root ->
      Alcotest.(check bool)
        "typed IR retains early semantic leaf" true
        (leaf == root_leaf root);
      let owner =
        match Tree.root_role (Typed.top_level_root_source root) with
        | Tree.Global_initializer owner -> owner
        | _ -> Alcotest.fail "expected initializer root"
      in
      Alcotest.(check bool)
        "binding owns original leaf" true
        (Source.owns_leaf (Initial.global_source owner |> Option.get) leaf);
      List.iter
        (fun query ->
          Alcotest.(check bool)
            "initializer query retains original leaf" true
            (List.exists (( == ) (Initial.query_leaf query)) early))
        (Initial.global_queries owner))
    early roots;
  Alcotest.(check (list int))
    "checked native destination offsets" expected_offsets
    (List.map Init.publication_cell_offset publications);
  List.iter2
    (fun leaf publication ->
      match (Init.describe_publication publication).prepared_root with
      | Init.Prepared_global root ->
          Alcotest.(check bool)
            "runtime publication keeps early leaf" true
            (root_leaf root == leaf)
      | _ -> Alcotest.fail "unexpected static publication")
    early publications;
  let result =
    VM.execute_task_program runtime
      ~runtime_calls:(integer_program_runtime_calls program)
      ~globals:(integer_program_globals program)
      ~initialization:(integer_program_initialization program)
      ~functions:(integer_program_functions program)
      (integer_program_entry program)
    |> function
    | Ok result -> result
    | Error errors ->
        Alcotest.fail
          (String.concat "; "
             (List.map
                (fun (error : VM.error) -> error.code ^ ": " ^ error.message)
                errors))
  in
  Alcotest.(check (option int64))
    "promoted program returns 42" (Some 42L)
    (Option.map (fun word -> word.VM.bits) (VM.final_value result))

let promotion_ir () =
  promoted_ir "I64 A[2][2]={{10,20},{5,7}};A[0][0]+A[0][1]+A[1][0]+A[1][1];"
    [ 0; 1; 2; 3 ];
  promoted_ir "I64 A[2][2]={10,20,5,7};A[0][0]+A[0][1]+A[1][0]+A[1][1];"
    [ 0; 1; 2; 3 ];
  promoted_ir "I64 A[2][2]=10,20,5,7;A[0][0]+A[0][1]+A[1][0]+A[1][1];"
    [ 0; 1; 2; 3 ];
  promoted_ir "U8 A[2][3]={\"AB\",\"CD\"};A[1][0]-A[0][0]+40;" [ 0; 3 ];
  promoted_ir "I64 A[2]={sizeof(I64),34};A[0]+A[1];" [ 0; 1 ]

let incomplete_transcript () =
  let session, source, ledger = inputs "I64 A[2]={40,2 7};" in
  let early = ref [] in
  let observe event =
    let result = D.observe ledger event in
    (match event with
    | Parser.Global_initializer_leaf_completed receipt ->
        ignore (result |> expect);
        early := receipt :: !early
    | _ -> ());
    result
  in
  let output, events =
    Test_task_declarations.parse_source ~observe session ledger source
  in
  Alcotest.(check bool)
    "malformed separator prevents AST completion" true
    (Parser.has_errors output);
  Alcotest.(check int)
    "both expressions reached leaf completion" 2 (List.length !early);
  Alcotest.(check bool)
    "global remains incomplete" false
    (List.exists
       (function
         | Parser.Global_completed _ -> true
         | _ -> false)
       events);
  List.iter
    (fun receipt -> ignore (D.initializer_leaf_for ledger receipt |> expect))
    !early

let cross_owner () =
  let session, source, ledger = inputs "I64 A=42,B=42;" in
  let first = ref None in
  let second = ref None in
  let first_pending = ref None in
  let observe event =
    let result = D.observe ledger event in
    ignore (result |> expect);
    (match event with
    | Parser.Global_initializer_started start
      when start.initializer_owner.global_name.spelling = "A" ->
        first_pending := Some (Source.begin_parser start |> checked)
    | Parser.Global_initializer_leaf_completed receipt ->
        if receipt.leaf_initializer.initializer_owner.global_name.spelling = "A"
        then (
          first := Some receipt;
          ignore
            (Source.observe_parser_leaf (Option.get !first_pending) receipt
            |> checked))
        else (
          second := Some receipt;
          reject "equal leaf from second owner cannot enter first transcript"
            (Source.observe_parser_leaf (Option.get !first_pending) receipt);
          reject "foreign ledger cannot read observed leaf"
            (D.initializer_leaf_for
               (D.create (Session.create ()) |> checked)
               receipt))
    | _ -> ());
    result
  in
  let output, _ =
    Test_task_declarations.parse_source ~observe session ledger source
  in
  let ast = Test_parser.expect_ast output in
  let command = D.seal_source ledger ast |> expect in
  let a, b =
    match declarators ast with
    | [ a; b ] -> (a, b)
    | _ -> assert false
  in
  reject "equal source initializer cannot cross declarations"
    (D.source_initializer_for
       ~table:(Session.semantic_symbols session)
       ~ast command a.name
       (Option.get b.global_initial_value));
  Alcotest.(check bool)
    "equal leaves keep distinct owners" true
    (D.initializer_leaf_for ledger (Option.get !first)
    |> expect
    != (D.initializer_leaf_for ledger (Option.get !second) |> expect))

let timing text expected =
  let phases = ref 0 in
  let seen = ref [] in
  let commands =
    Test_stream_parser.declaration_sink (fun event ->
        (match event with
        | Parser.Array_dimension_preparing _
        | Parser.Array_dimension_completed _
        | Parser.Global_declared _
        | Parser.Global_completed _ -> ()
        | _ -> incr phases);
        Ok ())
  in
  let _, _, output, _, _, _ =
    Test_stream_parser.parse ~same_task:true ~commands
      ~on_enter:(fun () -> seen := !phases :: !seen)
      text
  in
  ignore (Test_parser.expect_ast output);
  Alcotest.(check (list int))
    "initializer boundaries reached before directive" expected (List.rev !seen)

let tests =
  [
    ("original nested, unbraced and copied leaves", `Quick, shapes);
    ("complete manifest reuses early leaves", `Quick, retained_manifest);
    ("missing phases cannot complete", `Quick, missing_phases);
    ("delayed leaf cannot revive prior callback", `Quick, delayed_leaf);
    ( "callback rejection stops later directives",
      `Quick,
      fun () -> failure_lifetime false );
    ( "callback exception revokes lifetime",
      `Quick,
      fun () -> failure_lifetime true );
    ("promotion preserves leaf identity through typed IR", `Quick, promotion_ir);
    ( "malformed delimiter retains only reached leaves",
      `Quick,
      incomplete_transcript );
    ("equal source leaves cannot cross owners", `Quick, cross_owner);
    ( "scalar leaf precedes command resume",
      `Quick,
      fun () -> timing {|I64 A=40;#exe {}|} [ 2 ] );
    ( "leaf follows expression lookahead and precedes next element",
      `Quick,
      fun () -> timing {|I64 A[2]={40#exe {},#exe {"2";}};|} [ 1; 2 ] );
    ( "source ledger retains native leaves",
      `Quick,
      fun () ->
        let session = Session.create () in
        let source =
          Session.add_source session ~path:"leaves.hc"
            ~contents:"I64 A[2]={40,2};A[0]+A[1];"
        in
        let ledger =
          Task_declarations.create_source session ~source
          |> Test_declaration_collection.checked
        in
        let output, _ =
          Test_task_declarations.parse_source session ledger source
        in
        let ast = Test_parser.expect_ast output in
        ignore
          (Task_declarations.seal_source ledger ast
          |> Test_integer_program.checked) );
  ]
