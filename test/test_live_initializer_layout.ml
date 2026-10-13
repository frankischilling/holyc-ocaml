open Holyc_lib
module D = Task_declarations
module Task = Integer_task
module Layout = Ir_integer_initializer_layout
module Fragment = Semantic_initializer_fragment
module Source = Semantic_initializer_source

let checked = Test_declaration_collection.checked
let expect = Test_integer_program.checked

let parse ?(phased = false) ?execute_stream
    ?(skip_layout_delimiter = fun _ -> false) ?(on_leaf = fun _ _ _ -> Ok ())
    text =
  let session, source, ledger = Test_source_promotion.inputs text in
  let task = ref None and layout = ref None and entries = ref [] in
  let fragments = ref [] in
  let diagnose span =
    Result.map_error (fun message ->
        [
          Diagnostic.make ~code:"HCRUN0006" ~severity:Diagnostic.Error ~message
            ~primary:span ();
        ])
  in
  let declaration event =
    Result.bind (D.observe ledger event) (fun () ->
        match event with
        | Parser.Global_declared publication ->
            Task.admit_global (Option.get !task) publication
        | Parser.Global_initializer_started start ->
            let declaration =
              D.initializer_declaration ledger start |> expect
            in
            Layout.begin_live declaration
            |> diagnose start.initializer_equals.span
            |> Result.map (fun state -> layout := Some state)
        | Parser.Global_initializer_delimiter_completed receipt ->
            (if skip_layout_delimiter receipt then Ok (Option.get !layout)
             else Layout.observe_live_delimiter (Option.get !layout) receipt)
            |> diagnose receipt.delimiter_initializer.initializer_equals.span
            |> Result.map (fun state -> layout := Some state)
        | Parser.Global_initializer_leaf_preparing receipt when phased ->
            Layout.prepare_live_expression (Option.get !layout) receipt
            |> diagnose receipt.preparation_first.span
            |> Result.map (fun state -> layout := Some state)
        | Parser.Global_initializer_leaf_completed receipt ->
            let typed =
              Task.prepare_initializer (Option.get !task) receipt |> expect
            in
            let fragment = Test_initializer_fragment_typing.fragment typed in
            fragments := fragment :: !fragments;
            let state = Option.get !layout in
            Layout.prepare_live state (Fragment.leaf fragment)
            |> diagnose
                 receipt.leaf_initializer.initializer_owner.global_name.location
                   .span
            |> fun result ->
            Result.bind result (fun (next, entry) ->
                Result.map
                  (fun () ->
                    layout := Some next;
                    entries := entry :: !entries)
                  (on_leaf (Option.get !task) receipt entry))
        | _ -> Ok ())
  in
  let checkpoint event =
    Result.bind (D.observe_command ledger event) (fun () ->
        match event with
        | Parser.Sequence_started _ when Option.is_none !task ->
            task := Some (Task.adopt_source session ~source ~ledger |> checked);
            Ok ()
        | Parser.Command_resumed completed ->
            let ast = completed.command_ast in
            if Test_live_initializer_leaves.declarators ast = [] then
              Result.bind
                (Task.compile_source_ast (Option.get !task) ast)
                (fun command ->
                  Task.execute (Option.get !task) command |> Result.map ignore)
            else
              let command = D.seal ledger ast |> expect in
              let declaration =
                List.hd (Test_live_initializer_leaves.declarators ast)
              in
              let initial = Option.get declaration.global_initial_value in
              let manifest =
                D.initializer_for
                  ~table:(Session.semantic_symbols session)
                  ~ast command declaration.name initial
                |> expect
              in
              Layout.complete_live (Option.get !layout) manifest
              |> diagnose ast.span
              |> Result.map (fun complete ->
                  Alcotest.(check int)
                    "completed layout keeps reached leaves"
                    (List.length !entries)
                    (List.length (Layout.entries complete)))
        | _ -> Ok ())
  in
  let parsed =
    Test_source_promotion.parse ?execute_stream ~declaration ~checkpoint session
      source ledger
  in
  (parsed, List.rev !entries, List.rev !fragments)

let expect_layout ?(phased = false) text offsets copies =
  let parsed, entries, _ = parse ~phased text in
  ignore (Test_parser.expect_ast parsed);
  Alcotest.(check (list int))
    "original leaf destinations" offsets
    (List.map Layout.cell_offset entries);
  Alcotest.(check (list (option string)))
    "original scalar/copy decisions" copies
    (List.map
       (fun entry ->
         match Layout.operation entry with
         | Layout.Scalar_store -> None
         | Layout.Copy_bytes bytes -> Some bytes)
       entries)

let phased_cursors () =
  expect_layout ~phased:true {|I64 A[2][2]={{1,2},{3,4}};|} [ 0; 1; 2; 3 ]
    [ None; None; None; None ];
  expect_layout ~phased:true {|U8 A[2][3]={"AB","CD"};|} [ 0; 3 ]
    [ Some "AB\000"; Some "CD\000" ];
  expect_layout ~phased:true {|class Box{U8 tag;U16 value;};Box object={7,42};|}
    [ 0; 1 ] [ None; None ]

let scalars () =
  expect_layout {|I64 N=42;|} [ 0 ] [ None ];
  expect_layout {|I64 A[2][2]={{1,2},{3,4}};|} [ 0; 1; 2; 3 ]
    [ None; None; None; None ];
  expect_layout {|I64 A[2][2]={1,2,3,4};|} [ 0; 1; 2; 3 ]
    [ None; None; None; None ];
  expect_layout {|I64 A[2][2]=1,2,3,4;|} [ 0; 1; 2; 3 ]
    [ None; None; None; None ]

let copies () =
  expect_layout {|U8 A[2][3]={"AB","CD"};|} [ 0; 3 ]
    [ Some "AB\000"; Some "CD\000" ];
  expect_layout {|U8 A[2][3]={{1,2,3},"CD"};|} [ 0; 1; 2; 3 ]
    [ None; None; None; Some "CD\000" ];
  expect_layout {|U8 A[2]="ABC";|} [ 0 ] [ Some "AB" ];
  expect_layout {|U8 A[2][3]="ABCDE";|} [ 0 ] [ Some "AB" ];
  expect_layout {|U8 A[1]={"ABC"};|} [ 0 ] [ None ]

let early_failure () =
  List.iter
    (fun (text, count) ->
      let parsed, entries, _ = parse text in
      Alcotest.(check bool)
        "original delimiter/copy failure" true (Parser.has_errors parsed);
      Alcotest.(check int)
        "earlier destinations survive" count (List.length entries))
    [
      ({|I64 A[2][2]={{1},2,3,4};|}, 1);
      ({|I64 N={42};|}, 0);
      ({|U8 A[4]="AB";|}, 0);
      ({|U8 A[4][2]="AB";|}, 0);
    ]

let delimiter_timing () =
  List.iter
    (fun text ->
      let entered = ref 0 in
      let execute_stream span =
        incr entered;
        Error
          [
            Diagnostic.make ~code:"TESTENTER" ~severity:Diagnostic.Error
              ~primary:span ~message:"unexpected directive entry" ();
          ]
      in
      let parsed, _, _ = parse ~execute_stream text in
      Alcotest.(check bool)
        "invalid delimiter stops parsing" true (Parser.has_errors parsed);
      Alcotest.(check int)
        "directive after invalid delimiter never entered" 0 !entered)
    [
      {|I64 A[2][2]={{1},#exe {}2,3,4};|};
      {|I64 N={#exe {}42};|};
      {|I64 A[1]={{#exe {}42}};|};
    ]

let missing_trailing_delimiter () =
  let parsed, entries, _ =
    parse
      ~skip_layout_delimiter:(fun receipt ->
        match receipt.Parser.delimiter_value with
        | Parser.Initializer_close _ -> true
        | _ -> false)
      {|I64 A[2]={40,2};|}
  in
  Alcotest.(check int) "both leaves reached" 2 (List.length entries);
  Alcotest.(check bool)
    "completion requires original trailing close" true
    (Parser.has_errors parsed)

let ordering () =
  let parsed, _, fragments = parse {|I64 A[2]={40,2};|} in
  ignore (Test_parser.expect_ast parsed);
  let first = List.hd fragments and second = List.nth fragments 1 in
  let initial = Layout.begin_live (Fragment.declaration first) |> checked in
  let reject label result =
    Alcotest.(check bool) label true (Result.is_error result)
  in
  reject "cannot skip first original leaf"
    (Layout.prepare_live initial (Fragment.leaf second));
  let first_receipt =
    Option.get (Source.leaf_parser_receipt (Fragment.leaf first))
  in
  reject "cannot omit opening delimiter"
    (Layout.prepare_live initial (Fragment.leaf first));
  let opening = Option.get first_receipt.leaf_delimiter_predecessor in
  let initial = Layout.observe_live_delimiter initial opening |> checked in
  reject "cannot replay opening delimiter"
    (Layout.observe_live_delimiter initial opening);
  let next, _ = Layout.prepare_live initial (Fragment.leaf first) |> checked in
  reject "cannot replay first original leaf"
    (Layout.prepare_live next (Fragment.leaf first));
  let receipt = Option.get (Source.leaf_parser_receipt (Fragment.leaf first)) in
  let reconstructed =
    Source.create receipt.leaf_value |> Source.leaves |> List.hd
  in
  reject "AST-only leaf has no delimiter authority"
    (Layout.prepare_live initial reconstructed)

let isolated_snapshots () =
  let text =
    "extern class Item;class Box{Item item;U8 tail;$$=8;};Box \
     global={7,42};I64 F(){static Box object={7,42};return object.tail;}class \
     Item{U16 value;};"
  in
  let session, source, ledger = Test_source_promotion.inputs text in
  let delimiters = ref [] and leaves = ref [] and completions = ref [] in
  let declaration event =
    Result.map
      (fun () ->
        match event with
        | Parser.Static_initializer_delimiter_completed receipt ->
            delimiters := receipt :: !delimiters
        | Parser.Static_initializer_preparing receipt ->
            leaves := receipt :: !leaves
        | Parser.Static_initializer_completed receipt ->
            completions := receipt :: !completions
        | _ -> ())
      (D.observe ~isolated_initializers:true ledger event)
  in
  let parsed =
    Test_source_promotion.parse ~mode:Preprocessor.Aot ~declaration session
      source ledger
  in
  let ast = Test_parser.expect_ast parsed in
  let command = D.seal_source ledger ast |> expect in
  let table = Session.semantic_symbols session in
  let layouts = D.source_initializer_layouts ~table ~ast command |> expect in
  let snapshots =
    D.source_static_initializer_snapshots ~table ~ast command |> expect
  in
  Alcotest.(check int) "one original global cursor" 1 (List.length layouts);
  Alcotest.(check int) "one original static cursor" 1 (List.length snapshots);
  let reject label result =
    Alcotest.(check bool) label true (Result.is_error result)
  in
  let foreign_table = Session.semantic_symbols (Session.create ()) in
  reject "global layouts reject another table"
    (D.source_initializer_layouts ~table:foreign_table ~ast command);
  reject "static snapshots reject another table"
    (D.source_static_initializer_snapshots ~table:foreign_table ~ast command);
  let cloned_ast = Obj.obj (Obj.dup (Obj.repr ast)) in
  reject "global layouts reject a copied source seal"
    (D.source_initializer_layouts ~table ~ast:cloned_ast command);
  reject "static snapshots reject a copied source seal"
    (D.source_static_initializer_snapshots ~table ~ast:cloned_ast command);
  let snapshot = List.hd snapshots in
  List.iter
    (fun receipt ->
      reject "completed static cursor cannot replay a delimiter"
        (Layout.observe_static_snapshot_delimiter snapshot receipt);
      reject "copied delimiter cannot reopen a static cursor"
        (Layout.observe_static_snapshot_delimiter snapshot
           (Obj.obj (Obj.dup (Obj.repr receipt)))))
    !delimiters;
  List.iter
    (fun receipt ->
      reject "completed static cursor cannot replay a leaf"
        (Layout.prepare_static_snapshot_leaf snapshot receipt))
    !leaves;
  List.iter
    (fun receipt ->
      reject "static cursor completion is once-only"
        (Layout.complete_static_snapshot snapshot receipt))
    !completions;
  let foreign = Test_integer_globals.compile ~mode:Preprocessor.Aot text in
  let globals = integer_program_globals foreign in
  let slot = Ir_integer_globals.statics globals |> List.hd in
  let frame = Ir_integer_globals.static_frame slot
  and location = Ir_integer_globals.static_location slot in
  Alcotest.(check bool)
    "same-spelling foreign static has no snapshot" false
    (Layout.static_snapshot_matches snapshot ~frame ~location)

let tests =
  [
    Alcotest.test_case "scalar and fixed row traversal" `Quick scalars;
    Alcotest.test_case
      "prepared branches retain scalar, string and member cursors" `Quick
      phased_cursors;
    Alcotest.test_case "original copied-row decisions" `Quick copies;
    Alcotest.test_case "early delimiter and source-bound failure" `Quick
      early_failure;
    Alcotest.test_case "ordered original leaves" `Quick ordering;
    Alcotest.test_case "invalid delimiters stop before directives" `Quick
      delimiter_timing;
    Alcotest.test_case "completion requires trailing delimiters" `Quick
      missing_trailing_delimiter;
    Alcotest.test_case "isolated source cursors and foreign static frames"
      `Quick isolated_snapshots;
  ]
