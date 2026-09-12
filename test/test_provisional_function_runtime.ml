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

let replaced_argument_default () =
  List.iter
    (fun mode ->
      ignore
        (O.run ~mode
           "#exe {extern I64 F();if(0&&F#exe {extern I64 F(I64 \
            n=40);}()){}StreamPrint(\"42;\");}"
        |> O.expect ""))
    Test_integer_globals.modes

let changed_emission_count () =
  List.iter
    (fun mode ->
      ignore
        (O.run ~mode
           "#exe {extern I64 F();if(0&&F(#exe {extern I64 F(I64 \
            n);})){}StreamPrint(\"42;\");}"
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
    Alcotest.test_case "post-name defaults retain the replacement member owner"
      `Quick replaced_argument_default;
    Alcotest.test_case
      "emission fixed count stays separate from pushed arguments" `Quick
      changed_emission_count;
  ]

let implicit_count_after_marker () =
  List.iter
    (fun mode ->
      List.iter
        (fun (name, marker) ->
          ignore
            (O.run ~mode
               ("#exe {extern U0 " ^ name ^ "();if(0){" ^ marker
              ^ "#exe {extern U0 " ^ name
              ^ "(I64 n);}(40);}StreamPrint(\"42;\");}")
            |> O.expect ""))
        [ ("Print", "\"\""); ("PutChars", "''") ])
    Test_integer_globals.modes

let tests =
  tests
  @ [
      Alcotest.test_case
        "implicit argument count follows empty marker lookahead" `Quick
        implicit_count_after_marker;
    ]

let implicit_provider_local_shadow () =
  List.iter
    (fun mode ->
      ignore
        (O.run ~mode
           {|I64 F(I64 Print,I64 PutChars,I64 StreamPrint){#exe {"x";StreamPrint("42;");}return Print+PutChars+StreamPrint;}F(10,20,12);|}
        |> O.expect "x"))
    Test_integer_globals.modes

let tests =
  tests
  @ [
      Alcotest.test_case "provider setup excludes and restores caller locals"
        `Quick implicit_provider_local_shadow;
    ]

let implicit_phase_runtime_gates () =
  List.iter
    (fun mode ->
      List.iter
        (fun (name, marker) ->
          List.iter
            (fun (declaration, replacement, expected) ->
              let text =
                "#exe {I64 Out=0;" ^ declaration ^ marker ^ "()#exe {"
                ^ replacement ^ "};StreamPrint(\"%d;\",Out);}"
              in
              ignore (O.run ~mode text |> O.expect ~value:(Some expected) ""))
            [
              ("extern U0 " ^ name ^ "();", "U0 " ^ name ^ "(){Out=42;}", 42L);
              ("U0 " ^ name ^ "(){Out=17;}", "U0 " ^ name ^ "(){Out=42;}", 17L);
            ];
          List.iter
            (fun body ->
              ignore
                (O.run ~mode
                   ("#exe {extern U0 " ^ name ^ "();" ^ body
                  ^ "StreamPrint(\"42;\");}")
                |> O.expect ""))
            [
              "if(0){" ^ marker ^ "(#exe {extern U0 " ^ name ^ "(I64 n);});}";
              "if(0){" ^ marker ^ "#exe {extern U0 " ^ name ^ "(I64 n=40);}();}";
              "U0 Saved(){if(0){" ^ marker ^ "#exe {extern U0 " ^ name
              ^ "(I64 n);}(40);}}Saved;";
            ];
          ignore
            (O.run ~mode
               ("#exe {extern U0 " ^ name ^ "();if(0){" ^ marker
              ^ "(#exe {extern U0 " ^ name
              ^ "(I64 n);}40#exe {Print(\"late\");});}}")
            |> O.fault "HCPARSE0167"))
        [ ("Print", "\"\""); ("PutChars", "''") ])
    Test_integer_globals.modes

let tests =
  tests
  @ [
      Alcotest.test_case "implicit calls retain argument and emission phases"
        `Quick implicit_phase_runtime_gates;
    ]

let implicit_nonempty_phase () =
  List.iter
    (fun mode ->
      List.iter
        (fun source -> ignore (O.run ~mode source |> O.expect ""))
        [
          {|#exe {extern U0 Print(U8 *s);if(0){"x"#exe {extern U0 Print(U8 *s,I64 m);};}StreamPrint("42;");}|};
          {|#exe {extern U0 PutChars(I64 n);if(0){'A'#exe {extern U0 PutChars(I64 n,I64 m);};}StreamPrint("42;");}|};
        ])
    Test_integer_globals.modes

let tests =
  tests
  @ [
      Alcotest.test_case
        "nonempty implicit marker captures count before argument lookahead"
        `Quick implicit_nonempty_phase;
    ]
