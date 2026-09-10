open Holyc_lib
module O = Test_integer_output

let reached_arguments () =
  List.iter
    (fun prefix ->
      ignore
        (O.run ~mode:Preprocessor.Jit
           (prefix
          ^ "extern U0 PutChars(U64 ch);I64 Arg(){PutChars('A');return \
             42;};I64 F(I64 n){return n;}#exe {F(Arg());}")
        |> O.fault ~output:"A" "HCIRVM0030"))
    [ ""; "#exe {}" ]

let skipped_and_completed () =
  List.iter
    (fun prefix ->
      ignore
        (O.run ~mode:Preprocessor.Jit
           (prefix
          ^ "I64 F(I64 n=40){I64 add=2;return n+add;}#exe {if(0)F();}F();")
        |> O.expect ""))
    [ ""; "#exe {}" ]

let expired_admission () =
  let session = Session.create () in
  let table = Session.semantic_symbols session in
  let runtime =
    Ir_integer_interpreter.create_task_state ~table ()
    |> Test_declaration_collection.checked
  in
  let ledger =
    Task_declarations.create ~runtime session
    |> Test_declaration_collection.checked
  in
  let parsed, events =
    Test_task_declarations.parse session ledger "I64 F(I64 n){return n;}"
  in
  ignore (Test_parser.expect_ast parsed);
  let header =
    List.find_map
      (function
        | Parser.Function_header_completed h -> Some h
        | _ -> None)
      events
    |> Option.get
  in
  let scopes = List.length (Semantic_symbol_table.all_scopes table) in
  let symbols = List.length (Semantic_symbol_table.all_symbols table) in
  Alcotest.(check bool)
    "expired admission rejects" true
    (Result.is_error
       (Task_declarations.admit_function_header ledger ~runtime header));
  Alcotest.(check int)
    "expired admission allocates no scope" scopes
    (List.length (Semantic_symbol_table.all_scopes table));
  Alcotest.(check int)
    "expired admission allocates no parameter" symbols
    (List.length (Semantic_symbol_table.all_symbols table))

let nested_task_modes () =
  List.iter
    (fun mode ->
      ignore
        (O.run ~mode
           "#exe {I64 Arg(){PutChars('A');return 42;}I64 F(I64 n){return \
            n;}#exe {F(Arg());}}"
        |> O.fault ~output:"A" "HCIRVM0030");
      ignore
        (O.run ~mode
           "#exe {I64 F(I64 n=40){I64 add=2;return n+add;}#exe \
            {if(0)F();}StreamPrint(\"%d;\",F());}"
        |> O.expect ""))
    Test_integer_globals.modes

let provider_boundary () =
  List.iter
    (fun mode ->
      ignore
        (O.run ~mode "#exe {U0 PutChars(U64 ch){}#exe {PutChars('A');}}"
        |> O.fault "HCIRVM0030");
      ignore
        (O.run ~mode
           "#exe {extern U0 PutChars(U64 ch);U0 Saved(){PutChars('A');}U0 \
            PutChars(U64 ch){}#exe {Saved();}Saved();StreamPrint(\"42;\");}"
        |> O.expect "A"))
    Test_integer_globals.modes

let retained_pending_calls () =
  List.iter
    (fun mode ->
      List.iter
        (fun suffix ->
          ignore
            (O.run ~mode
               ("#exe {I64 F(I64 n=40){return n+2;}#exe {I64 Saved(){return \
                 F();}}" ^ suffix ^ "StreamPrint(\"%d;\",Saved());}")
            |> O.expect ""))
        [ ""; "I64 F(I64 n=99){return n;}" ])
    Test_integer_globals.modes

let retained_variadic_calls () =
  List.iter
    (fun mode ->
      ignore
        (O.run ~mode
           "#exe {I64 F(I64 n=40,...){return n+argc+argv[0];}#exe {I64 \
            Saved(){return F(,1);}}StreamPrint(\"%d;\",Saved());}"
        |> O.expect ""))
    Test_integer_globals.modes

let provisional_and_argument_faults () =
  List.iter
    (fun mode ->
      ignore
        (O.run ~mode "#exe {I64 F(I64 n)#exe {F(42);}{return n;}}"
        |> O.fault "HCRUN0003");
      ignore
        (O.run ~mode
           "#exe {I64 Arg(){PutChars('A');return 1/0;}I64 F(I64 n){return \
            n;}#exe {F(Arg());}}"
        |> O.fault ~output:"A" "HCIRVM0009"))
    Test_integer_globals.modes

let saved_default_effects () =
  List.iter
    (fun mode ->
      ignore
        (O.run ~mode
           "#exe {I64 N=39;I64 F(I64 n=++N){return n+N-38;}#exe {I64 \
            Saved(){return F();}}StreamPrint(\"%d;\",Saved());}"
        |> O.expect ""))
    Test_integer_globals.modes

let failure_recovery () =
  let module T = Test_integer_task in
  let session = Session.create () in
  let task = T.create session in
  let accept source =
    ignore (T.run session task source |> Test_integer_program.checked)
  in
  accept "I64 N=40;";
  T.fault "HCIRVM0030"
    (T.run session task
       "I64 F(I64 n){return n;}#exe {I64 Saved(){return F(42);}N+=2;Saved();}");
  T.value 42L (T.run session task "N;");
  accept "I64 F(I64 n){return 99;}";
  T.value 99L (T.run session task "F(42);");
  T.value 99L (T.run session task "Saved();");
  accept "I64 F(I64 n){return 77;}";
  T.value 77L (T.run session task "F(42);");
  T.value 99L (T.run session task "Saved();");
  T.value 42L (T.run session task "N;")

let nested_header_replacement_boundary () =
  List.iter
    (fun mode ->
      ignore
        (O.run ~mode
           "#exe {PutChars('A');I64 F(I64 n){return n;}#exe {I64 F(I64 \
            n){return 99;}}StreamPrint(\"%d;\",F(42));}"
        |> O.fault ~output:"A" "HCEVAL0003"))
    Test_integer_globals.modes

let resource_limits () =
  let source =
    "#exe {I64 F(I64 n=40,...){I64 v=n+argc+argv[0];return v;}#exe {I64 \
     Saved(){return F(,1);}}StreamPrint(\"%d;\",Saved());}"
  in
  List.iter
    (fun mode ->
      let report = O.run ~mode source in
      ignore (O.expect "" report);
      let measured =
        (Option.get (integer_program_report_progress report)).runtime
      in
      ignore
        (O.run ~mode ~max_steps:measured.executed_steps
           ~max_initializer_steps:measured.initializer_steps ~max_frame_bytes:32
           ~max_call_depth:2 source
        |> O.expect "");
      ignore
        (O.run ~mode ~max_steps:(measured.executed_steps - 1) source
        |> O.fault "HCIRVM0007");
      ignore
        (O.run ~mode
           ~max_initializer_steps:(measured.initializer_steps - 1)
           source
        |> O.fault "HCIRVM0007");
      ignore (O.run ~mode ~max_frame_bytes:31 source |> O.fault "HCIRVM0011");
      ignore (O.run ~mode ~max_call_depth:1 source |> O.fault "HCIRVM0015"))
    Test_integer_globals.modes

let tests =
  [
    Alcotest.test_case "pending call evaluates arguments before UndefinedExtern"
      `Quick reached_arguments;
    Alcotest.test_case "skipped call and eventual original body" `Quick
      skipped_and_completed;
    Alcotest.test_case "expired admission allocates nothing" `Quick
      expired_admission;
    Alcotest.test_case "nested pending headers in JIT and AOT directive tasks"
      `Quick nested_task_modes;
    Alcotest.test_case "pending definitions and captured provider fallback"
      `Quick provider_boundary;
    Alcotest.test_case "pending calls retain defaults and ignore later shadows"
      `Quick retained_pending_calls;
    Alcotest.test_case "pending calls retain variadic protocol" `Quick
      retained_variadic_calls;
    Alcotest.test_case "provisional headers and reached argument faults" `Quick
      provisional_and_argument_faults;
    Alcotest.test_case "saved defaults execute once before body publication"
      `Quick saved_default_effects;
    Alcotest.test_case
      "reached pending faults preserve state and source lineage" `Quick
      failure_recovery;
    Alcotest.test_case "nested header replacement remains an explicit boundary"
      `Quick nested_header_replacement_boundary;
    Alcotest.test_case "pending completion preserves exact resource limits"
      `Quick resource_limits;
  ]
