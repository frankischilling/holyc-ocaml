module O = Test_integer_output

let definition_gate () =
  List.iter
    (fun mode ->
      ignore
        (O.run ~mode
           "#exe {I64 F(...{return argc;}StreamPrint(\"%d;\",F(1)+41);}"
        |> O.expect ""))
    Test_integer_globals.modes

let ordinary_and_prototype () =
  List.iter
    (fun mode ->
      List.iter
        (fun source -> ignore (O.run ~mode source |> O.expect ""))
        [
          "I64 F(I64 n=40,...{return n+argc+argv[0];};F(,1);";
          "extern I64 F(...;I64 F(...{return argc;};F(1)+41;";
          "I64 F(... return argc;;F(1)+41;";
        ])
    Test_integer_globals.modes

let nested_versions () =
  List.iter
    (fun mode ->
      ignore
        (O.run ~mode
           "#exe {I64 F(I64 n=40,...{return n+argc+argv[0];}#exe {I64 \
            Before(...{return F(,1);}I64 F(I64 n=99,...{return 7;}I64 \
            Inner(...{return \
            F(,2);}}StreamPrint(\"%d;\",Before()+Inner()+F(,1)-108);}"
        |> O.expect ""))
    Test_integer_globals.modes

let resources () =
  let source closing =
    "#exe {I64 F(I64 n=40,..." ^ closing
    ^ "{I64 v=n+argc+argv[0];return v;}#exe {I64 Saved(){return \
       F(,1);}}StreamPrint(\"%d;\",Saved());}"
  in
  List.iter
    (fun mode ->
      let baseline = O.run ~mode (source ")") in
      ignore (O.expect "" baseline);
      let steps =
        (Option.get (Holyc_lib.integer_program_report_progress baseline))
          .runtime
      in
      let omitted = source "" in
      ignore
        (O.run ~mode ~max_steps:steps.executed_steps
           ~max_initializer_steps:steps.initializer_steps ~max_frame_bytes:32
           ~max_call_depth:2 omitted
        |> O.expect "");
      ignore
        (O.run ~mode ~max_steps:(steps.executed_steps - 1) omitted
        |> O.fault "HCIRVM0007");
      ignore
        (O.run ~mode
           ~max_initializer_steps:(steps.initializer_steps - 1)
           omitted
        |> O.fault "HCIRVM0007");
      ignore (O.run ~mode ~max_frame_bytes:31 omitted |> O.fault "HCIRVM0011");
      ignore (O.run ~mode ~max_call_depth:1 omitted |> O.fault "HCIRVM0015"))
    Test_integer_globals.modes

let tests =
  [
    Alcotest.test_case "variadic definition without closing token executes"
      `Quick definition_gate;
    Alcotest.test_case "caller owns the token after variadic signature" `Quick
      ordinary_and_prototype;
    Alcotest.test_case "omitted closes preserve nested publication versions"
      `Quick nested_versions;
    Alcotest.test_case
      "closing punctuation leaves resource accounting unchanged" `Quick
      resources;
  ]
