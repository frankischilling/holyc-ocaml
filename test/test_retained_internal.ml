open Holyc_lib
module O = Test_integer_output
module VM = Ir_integer_interpreter

let modes = Test_integer_globals.modes

let generated_code =
  {|extern U0 Print(U8 *fmt,...);
#exe {
  _intern 0x1e I64 ToUpper(U8 ch);
  _intern 0x84 I64 StrLen(U8 *text);
  I64 Convert(I64 ch) { return ToUpper(ch); }
  StreamPrint("Print(\"%%c:%%d\",%d,%d);",Convert('a'),StrLen("abc"));
}
42;|}

let cases =
  [
    ( "parenthesized target",
      {|#exe {_intern (0x1e) I64 Convert(U8 ch);StreamPrint("%d;",Convert('a'));}|},
      "",
      65L );
    ( "arithmetic target",
      {|#exe {_intern 0x10+0xe I64 Convert(U8 ch);StreamPrint("%d;",Convert('a'));}|},
      "",
      65L );
    ( "retained global target",
      {|#exe {I64 Code=0x1e;_intern Code I64 Convert(U8 ch);Code=0x84;StreamPrint("%d;",Convert('a'));}|},
      "",
      65L );
    ( "effectful target executes once before publication",
      {|#exe {I64 Count=0;I64 Target(){Count++;return 0x1e;}_intern Target() I64 Convert(U8 ch);StreamPrint("%d*100+%d;",Convert('a'),Count);}|},
      "",
      6501L );
    ( "target effects precede header defaults",
      {|#exe {I64 Count=0;I64 Saved=0;I64 Target(){Count++;return 0x1e;}_intern Target() I64 Convert(U8 ch=#exe {Saved=Count;}'a');StreamPrint("%d*100+%d;",Count,Saved);}|},
      "",
      101L );
    ( "target expression uses original call emission",
      {|#exe {extern I64 Target();_intern Target()#exe {I64 Target(){return 0x1e;}} I64 Convert(U8 ch);StreamPrint("%d;",Convert('a'));}|},
      "",
      65L );
    ( "header lookahead preserves saved target",
      {|#exe {I64 Code=0x1e;_intern Code I64 Convert#exe {Code=0x84;}(U8 ch);StreamPrint("%d;",Convert('a'));}|},
      "",
      65L );
    ( "target query keeps original type receipt",
      {|#exe {_intern sizeof(I64)+22 I64 Convert(U8 ch);StreamPrint("%d;",Convert('a'));}|},
      "",
      65L );
    ( "expression lookahead precedes target execution",
      {|#exe {I64 Code=0x1e;_intern Code#exe {Code=0x84;} I64 Length(U8 *text);StreamPrint("%d;",Length("abc"));}|},
      "",
      3L );
    ( "nested target preparation cannot replace the saved outer value",
      {|#exe {I64 Code=0x1e;_intern Code I64 F(U8 outer)#exe {Code=0x7f;_intern Code I64 F(U8 inner);};StreamPrint("%d;",F('a'));}|},
      "",
      65L );
    ( "target call retains default and array preparation",
      {|#exe {I64 Target(I64 code=30){I64 Values[2+1];Values[0]=code;return Values[0];}_intern Target() I64 Convert(U8 ch);StreamPrint("%d;",Convert('a'));}|},
      "",
      65L );
    ("generated calls and retained body", generated_code, "A:3", 42L);
    ( "full input word",
      {|#exe {_intern 0x1e I64 F(U8 ch);StreamPrint("%d;",F(0x161));}|},
      "",
      353L );
    ( "renamed macro target",
      "#exe {\n\
       #define OP 0x1e\n\
       public _intern OP I64 Convert(U8 ch);\n\
       #define OP 0x84\n\
       StreamPrint(\"%d;\",Convert('b'));}",
      "",
      66L );
    ( "call emission installs selected extern",
      {|#exe {extern I64 F(U8 ch);StreamPrint("%d;",F('a')#exe {_intern 0x1e I64 F(U8 ch);});}|},
      "",
      65L );
    ( "name lookahead supplies installed argument cursor",
      {|#exe {extern I64 F();StreamPrint("%d;",F#exe {_intern 0x1e I64 F(U8 ch);}('a'));}|},
      "",
      65L );
    ( "completed internal creates a fresh shadow",
      {|#exe {_intern 0x1e I64 F(U8 ch);StreamPrint("%d;",F('a')#exe {I64 F(U8 ch){return 42;}});StreamPrint("%d;",F('a'));}|},
      "",
      42L );
    ( "outer header keeps nested installed target during lookahead",
      {|#exe {_intern 0x1e I64 F(U8 outer)#exe {_intern 0x84 I64 F(U8 *inner);StreamPrint("%d;",F("abc"));};StreamPrint("42;");}|},
      "",
      42L );
    ( "ordinary outer header preserves nested internal installation",
      {|#exe {extern I64 F(U8 outer)#exe {_intern 0x1e I64 F(U8 inner);};StreamPrint("%d;",F('a'));}|},
      "",
      65L );
    ( "resumed outer internal keeps the nested executable target",
      {|#exe {I64 Value=0;_intern 0x84 I64 F(U8 *outer #exe {_intern 0x1e I64 F(U8 inner);},U8 trailing=#exe {Value=F('a');}2);StreamPrint("%d;42;",Value);}|},
      "",
      42L );
    ( "resumed ordinary header keeps nested internal flags",
      {|#exe {I64 Value=0;extern I64 F(U8 outer #exe {_intern 0x1e I64 F(U8 inner);},U8 trailing=#exe {Value=F('a');}2);StreamPrint("%d;42;",Value);}|},
      "",
      42L );
    ( "skipped incomplete internal remains an extern",
      {|#exe {_intern 0x1e I64 F(U8 ch=#exe {if(0&&F()) {}}'a');StreamPrint("42;");}|},
      "",
      42L );
    ( "source body keeps its own behavior",
      {|#exe {I64 ToUpper(U8 ch){return 42;}StreamPrint("%d;",ToUpper('a'));}|},
      "",
      42L );
    ( "retained byte domain uses the original internal operation",
      "#exe {\n" ^ Test_internal_toupper.all_bytes
      ^ "StreamPrint(\"%d;\",Check());}",
      "",
      0L );
  ]

let successful_case (label, source, output, bits) () =
  List.iter
    (fun mode ->
      let report = O.run ~mode ~max_steps:100_000 source in
      let result =
        Test_integer_functions.checked (integer_program_report_outcome report)
      in
      Alcotest.(check string)
        label output
        (integer_program_report_output_bytes report);
      Alcotest.(check bool)
        (label ^ " stream end") true
        (VM.termination result.value = VM.Stream_end);
      Test_internal_toupper.word label bits result.value)
    modes

let incomplete_headers () =
  List.iter
    (fun mode ->
      List.iter
        (fun source -> ignore (O.run ~mode source |> O.fault "HCIRVM0030"))
        [
          {|#exe {_intern 0x1e I64 F(U8 ch=#exe {F();}'a');}|};
          {|#exe {_intern 0x1e I64 F(U8 ch)#exe {F();};}|};
        ])
    modes

let unsupported_targets () =
  List.iter
    (fun mode ->
      List.iter
        (fun (declaration, call, code) ->
          let source = "#exe {Print(\"kept\");" ^ declaration ^ call ^ ";}" in
          ignore (O.run ~mode source |> O.fault ~output:"kept" code))
        [
          ("_intern 0x7f I64 F(U8 ch);", "F('a')", "HCRUN0003");
          ("_intern 0x1e I64 F(U8 *ch);", "F(\"a\")", "HCRUN0003");
          ("_intern 0x1e I64 F(U8 ch='a');", "F()", "HCRUN0003");
          ("_intern 0x1e I64 F(U8 ch,...);", "F('a')", "HCRUN0003");
          ( "I64 F(U8 outer)#exe {_intern 0x1e I64 F(U8 inner);}{return 42;}",
            "F('a')",
            "HCIR0027" );
        ])
    modes

let cumulative_budgets () =
  List.iter
    (fun mode ->
      let steps = if mode = Preprocessor.Jit then 58 else 57 in
      let exact =
        O.run ~mode ~max_steps:steps ~max_output_work:55 generated_code
        |> O.expect "A:3"
      in
      Alcotest.(check int)
        "exact cumulative steps" steps (VM.executed_steps exact);
      ignore
        (O.run ~mode ~max_steps:(steps - 1) generated_code
        |> O.fault ~output:"A:3" "HCIRVM0007");
      ignore
        (O.run ~mode ~max_output_work:54 generated_code |> O.fault "HCIRVM0023"))
    modes

let binding_failures () =
  List.iter
    (fun mode ->
      let prefix =
        {|extern U0 Print(U8 *fmt,...);#exe {I64 Target(){Print("kept");return 0x1e;}_intern Target() |}
      in
      List.iter
        (fun (suffix, code) ->
          ignore (O.run ~mode (prefix ^ suffix) |> O.fault ~output:"kept" code))
        [
          ("BadType F(U8 ch);}", "HCPARSE0001");
          ("I64 ;}", "HCPARSE0002");
          ("I64 F(U8 ch", "HCPARSE0010");
          ("I64 F(U8 ch;}", "HCPARSE0009");
        ];
      ignore
        (O.run ~mode
           {|#exe {extern I64 Target();_intern Target() BadType F(U8 ch);}|}
        |> O.fault "HCIRVM0030");
      ignore
        (O.run ~mode
           (O.print_header
          ^ {|#exe {Print("before");_intern 30.0 I64 F(U8 ch);}|})
        |> O.fault ~output:"before" "HCRUN0001");
      ignore
        (O.run ~mode
           (O.print_header
          ^ {|#exe {I64 Target(){Print("kept");return 0x7f;}_intern Target() I64 F(U8 ch);F('a');}|}
           )
        |> O.fault ~output:"kept" "HCRUN0003"))
    modes

let effectful_binding_budgets () =
  let source =
    O.print_header
    ^ {|#exe {I64 Count=0;I64 Target(){Count++;Print("%d",Count);return 0x1e;}_intern Target() I64 F(U8 ch);StreamPrint("%d*100+%d;",F('a'),Count);}|}
  in
  List.iter
    (fun mode ->
      let exact =
        O.run ~mode ~max_steps:52 ~max_initializer_steps:3 ~max_output_work:24
          source
        |> O.expect ~value:(Some 6501L) "1"
      in
      Alcotest.(check int)
        "effectful target cumulative execution" 52 (VM.executed_steps exact);
      Alcotest.(check int)
        "effectful target cumulative preparation" 3
        (VM.compiled_initializer_steps exact);
      ignore
        (O.run ~mode ~max_steps:51 source |> O.fault ~output:"1" "HCIRVM0007");
      ignore
        (O.run ~mode ~max_initializer_steps:2 source |> O.fault "HCIRVM0007");
      ignore
        (O.run ~mode ~max_output_work:23 source
        |> O.fault ~output:"1" "HCIRVM0023");
      ignore (O.run ~mode ~max_call_depth:1 source |> O.fault "HCIRVM0015");
      ignore
        (O.run ~mode ~max_frame_bytes:31 source
        |> O.fault ~output:"1" "HCIRVM0011");
      ignore (O.run ~mode ~max_literal_bytes:1 source |> O.fault "HCIRVM0021"))
    modes

let original_installation () =
  let module P = Test_provisional_function_resolution in
  let module N = Semantic_function_record_phase in
  let module R = Semantic_function_resolution in
  let module C = Semantic_function_record_classification in
  let f = P.fixture ~contents:"_intern 0x1e I64 F(U8 n)#exe {};" () in
  let snapshot = List.hd f.samples in
  Alcotest.(check bool)
    "lookahead has no installed internal target" true
    (N.internal_binding snapshot = None && N.is_extern snapshot = Some true);
  let state =
    C.make_declaration_state ~staging_mask:0L
      ~compiler_option_mask:Compiler_option.initial_mask ()
  in
  let classify ?(previous = []) resolution =
    C.classify ~previous resolution [ state ]
    |> P.checked |> C.declarations |> List.hd
  in
  let initial_fact =
    R.make_provisional_declaration ~table:f.table ~namespace:f.namespace
      ~compiler_option_mask:Compiler_option.initial_mask
      ~function_:(P.typed f snapshot)
    |> P.checked
  in
  let initial = P.resolve f initial_fact |> P.checked |> classify in
  let pending = C.classified_declaration_source initial in
  let record = C.classified_declaration_record initial in
  Alcotest.(check bool)
    "lookahead retains extern flags" true
    (C.is_extern record && not (C.is_internal record));
  let header_fact = P.header_fact f pending pending |> P.checked in
  let resolution = P.resolve ~previous:[ pending ] f header_fact |> P.checked in
  let installed = classify ~previous:[ initial ] resolution in
  let record = C.classified_declaration_record installed in
  Alcotest.(check bool)
    "original completed header installs numeric target" true
    ((not (C.is_extern record))
    && C.is_internal record
    && C.call_access record = C.Internal_operation);
  Alcotest.(check bool)
    "installed binding is resolved before command completion" true
    (C.classified_declaration_source installed
    |> R.resolved_declaration_site |> R.declaration_site_state = R.Resolved);
  let header = Semantic_compiler_record.declared_function_source f.source in
  Alcotest.(check bool)
    "exact installed source header" true
    (Option.get (N.internal_binding f.final_snapshot) == header);
  let legacy =
    R.make_pending_declaration ~table:f.table ~namespace:f.namespace
      ~compiler_option_mask:Compiler_option.initial_mask ~source:f.source
      ~function_:f.ordinary
    |> P.checked |> P.resolve f |> P.checked |> classify
    |> C.classified_declaration_record
  in
  Alcotest.(check bool)
    "header without native installation grants no operation" true
    (C.is_extern legacy && not (C.is_internal legacy));
  P.reject "original installed transition cannot replay"
    (P.resolve ~previous:[ pending ] f header_fact);
  let overwritten =
    P.fixture
      ~contents:
        "I64 F(U8 outer)#exe {_intern 0x1e I64 F(U8 inner);}{return 42;}"
      ()
  in
  let _, body_snapshot = List.hd (List.rev overwritten.events) in
  Alcotest.(check bool)
    "body installation revokes numeric target evidence" true
    (N.internal_binding body_snapshot = None)

let tests =
  List.map
    (fun ((label, _, _, _) as case) ->
      Alcotest.test_case label `Quick (successful_case case))
    cases
  @ [
      Alcotest.test_case "target effects survive declaration errors" `Quick
        binding_failures;
      Alcotest.test_case
        "effectful targets share execution and preparation budgets" `Quick
        effectful_binding_budgets;
      Alcotest.test_case "incomplete headers retain undefined extern execution"
        `Quick incomplete_headers;
      Alcotest.test_case "unsupported targets preserve earlier task output"
        `Quick unsupported_targets;
      Alcotest.test_case "shared task and generated-code budgets" `Quick
        cumulative_budgets;
      Alcotest.test_case "original header installation authority" `Quick
        original_installation;
    ]
