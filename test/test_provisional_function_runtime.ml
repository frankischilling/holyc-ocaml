module O = Test_integer_output

let fresh_branch () =
  List.iter
    (fun mode ->
      ignore
        (O.run ~mode
           "#exe {I64 F(I64 n=40)#exe {if(0&&F()) {}}{return \
            n;}StreamPrint(\"42;\");}"
        |> O.expect ""))
    Test_integer_globals.modes

let reused_branch () =
  List.iter
    (fun mode ->
      ignore
        (O.run ~mode
           "#exe {extern I64 F(I64 n);I64 F(I64 n)#exe {if(0&&F()) {}}{return \
            n;}StreamPrint(\"42;\");}"
        |> O.expect ""))
    Test_integer_globals.modes

let before_default () =
  List.iter
    (fun mode ->
      ignore
        (O.run ~mode
           "#exe {I64 F(I64 n=#exe {if(0&&F()) {}}40){return \
            n;}StreamPrint(\"42;\");}"
        |> O.expect ""))
    Test_integer_globals.modes

let reached_and_eager () =
  List.iter
    (fun mode ->
      List.iter
        (fun expression ->
          ignore
            (O.run ~mode
               ("#exe {I64 F(I64 n=40)#exe {" ^ expression ^ "}{return n;}}")
            |> O.fault "HCIRVM0030"))
        [ "F();"; "0&&F();"; "if(1&&F()) {}" ])
    Test_integer_globals.modes

let post_close_extern_completion () =
  List.iter
    (fun mode ->
      ignore
        (O.run ~mode
           "#exe {extern I64 F();StreamPrint(\"%d;\",F()#exe {I64 F(){return \
            42;}});}"
        |> O.expect ""))
    Test_integer_globals.modes

let post_close_fresh_identity () =
  List.iter
    (fun mode ->
      ignore
        (O.run ~mode
           "#exe {I64 F(){return 17;}StreamPrint(\"%d;\",F()#exe {I64 \
            F(){return 42;}});}"
        |> O.expect ~value:(Some 17L) ""))
    Test_integer_globals.modes

let count_after_name_lookahead () =
  List.iter
    (fun mode ->
      ignore
        (O.run ~mode
           "#exe {extern I64 F();if(0&&F#exe {extern I64 F(I64 \
            n);}(40)){}StreamPrint(\"42;\");}"
        |> O.expect ""))
    Test_integer_globals.modes

let hidden_outer_completion () =
  List.iter
    (fun mode ->
      ignore
        (O.run ~mode
           "#exe {I64 F()#exe {I64 F(){return 7;}I64 F(){return 42;}}{return \
            17;}StreamPrint(\"%d;\",F());}"
        |> O.expect ""))
    Test_integer_globals.modes

let tests =
  [
    Alcotest.test_case "fresh provisional calls validate in skipped branches"
      `Quick fresh_branch;
    Alcotest.test_case "reused headers reset active argument count" `Quick
      reused_branch;
    Alcotest.test_case "member exists before successful default preparation"
      `Quick before_default;
    Alcotest.test_case
      "reached and eager provisional calls fault as undefined extern" `Quick
      reached_and_eager;
    Alcotest.test_case
      "post-close lookahead installs selected extern executable" `Quick
      post_close_extern_completion;
    Alcotest.test_case "post-close new identity keeps selected executable"
      `Quick post_close_fresh_identity;
    Alcotest.test_case "argument count follows name lookahead" `Quick
      count_after_name_lookahead;
    Alcotest.test_case "hidden outer completion preserves the visible shadow"
      `Quick hidden_outer_completion;
  ]
