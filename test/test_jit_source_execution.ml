open Holyc_lib
module Output = Test_integer_output
module VM = Ir_integer_interpreter

let run = Output.run ~mode:Preprocessor.Jit

let inactive_text () =
  List.iter
    (fun source ->
      let report = run ~max_steps:3 source in
      let result = Output.expect "" report in
      Alcotest.(check int)
        "ordinary instruction count" 3 (VM.executed_steps result);
      Alcotest.(check int)
        "ordinary preparation count" 0
        (VM.compiled_initializer_steps result);
      Alcotest.(check bool)
        "no task activation" true
        (integer_program_report_progress report = None);
      Alcotest.(check bool)
        "isolated artifact retained" true
        (Option.is_some (integer_program_report_program report));
      Alcotest.(check int)
        "no task units" 0
        (List.length (integer_program_report_task_units report)))
    [
      "42;";
      "// #exe {Print(\"late\");}\n42;";
      "#if 0\n#exe {Print(\"late\");}\n#endif\n42;";
    ]

let source =
  {|extern U0 Print(U8 *fmt,...);I64 N=20;I64 Next(){return ++N;};I64 Saved(I64 n=Next()){return n;};Print("A");N=0;#exe {Print("B");StreamPrint("Saved()+Saved();");}|}

let stateful_report () =
  let report = run source in
  let result = Output.expect "AB" report in
  let progress = Option.get (integer_program_report_progress report) in
  Alcotest.(check bool)
    "stateful task is not one isolated program" true
    (integer_program_report_program report = None);
  Alcotest.(check bool)
    "individual task units retained" true
    (List.length (integer_program_report_task_units report) > 3);
  Alcotest.(check int)
    "VM owns cumulative instructions" progress.runtime.executed_steps
    (VM.executed_steps result);
  Alcotest.(check int)
    "VM owns cumulative preparation" progress.runtime.initializer_steps
    (VM.compiled_initializer_steps result);
  Alcotest.(check (option int64))
    "streams and defaults do not replace outer result" (Some 42L)
    (Option.map (fun word -> word.VM.bits) progress.runtime.final_value)

let compilation_inspection () =
  List.iter
    (fun (text, stateful) ->
      let session, config, source =
        Test_integer_functions.inputs ~mode:Preprocessor.Jit text
      in
      let report = compile_integer_program_report session ~config ~source in
      let checked =
        integer_program_compilation_result report
        |> Test_integer_functions.checked
      in
      match checked.value with
      | Isolated _ ->
          Alcotest.(check bool)
            "ordinary compilation has one artifact" false stateful
      | Stateful result ->
          Alcotest.(check bool)
            "JIT compilation exposes the task result" true stateful;
          Alcotest.(check (option int64))
            "task result" (Some 42L)
            (Option.map (fun word -> word.VM.bits) (VM.final_value result));
          Alcotest.(check bool)
            "task compilation keeps individual units" true
            (integer_program_compilation_units report <> []))
    [ ("42;", false); ("#exe {StreamPrint(\"42;\");}", true) ]

let reached_failures () =
  List.iter
    (fun (code, source) ->
      let report = run source in
      ignore (Output.fault ~output:"A" code report);
      Alcotest.(check bool)
        "earlier work remains visible" true
        ((Option.get (integer_program_report_progress report)).runtime
           .executed_steps > 0))
    [
      ( "HCIRVM0009",
        {|extern U0 Print(U8 *fmt,...);Print("A");1/0;0;#exe {Print("late");}|}
      );
      ("HCIRVM0009", {|#exe {Print("A");}1/0;|});
      ("HCPARSE0127", {|#exe {Print("A");}I64 Broken=;|});
      ("HCRUN0003", {|#exe {Print("A");}40+Missing;|});
    ]

let limits () =
  let report = run source in
  ignore (Output.expect "AB" report);
  let measured =
    (Option.get (integer_program_report_progress report)).runtime
  in
  ignore
    (run ~max_steps:measured.executed_steps
       ~max_initializer_steps:measured.initializer_steps
       ~max_global_bytes:measured.global_bytes
       ~max_literal_bytes:measured.literal_bytes
       ~max_output_bytes:(String.length measured.output_bytes)
       ~max_output_work:measured.output_work source
    |> Output.expect "AB");
  List.iter
    (fun (code, report) ->
      let errors =
        match integer_program_report_outcome report with
        | Error errors -> errors
        | Ok _ -> Alcotest.fail "one-below resource limit succeeded"
      in
      Alcotest.(check bool)
        "specific resource fault" true
        (List.exists (fun error -> error.Diagnostic.code = code) errors))
    [
      ("HCIRVM0007", run ~max_steps:(measured.executed_steps - 1) source);
      ( "HCIRVM0007",
        run ~max_initializer_steps:(measured.initializer_steps - 1) source );
      ("HCIRVM0016", run ~max_global_bytes:(measured.global_bytes - 1) source);
      ("HCIRVM0021", run ~max_literal_bytes:(measured.literal_bytes - 1) source);
      ( "HCIRVM0022",
        run ~max_output_bytes:(String.length measured.output_bytes - 1) source
      );
      ("HCIRVM0023", run ~max_output_work:(measured.output_work - 1) source);
    ]

let deferred_dimension_budget () =
  let source =
    {|extern U0 Print(U8 *fmt,...);Print("A");I64 Values[1+1];#exe {Print("B");}42;|}
  in
  let report = run ~max_initializer_steps:2 source in
  ignore (Output.fault ~output:"A" "HCIRVM0007" report);
  let progress = Option.get (integer_program_report_progress report) in
  Alcotest.(check int) "only reached dimension work" 2 progress.dimension_work;
  Alcotest.(check int)
    "exhausted shared preparation allowance" 2
    progress.runtime.initializer_steps;
  let exact = run ~max_initializer_steps:3 source in
  ignore (Output.expect "AB" exact);
  let progress = Option.get (integer_program_report_progress exact) in
  Alcotest.(check int)
    "original dimension charged once" 3 progress.dimension_work;
  Alcotest.(check int)
    "layout does not charge dimension again" 3
    progress.runtime.initializer_steps

let dimension_event_order () =
  List.iter
    (fun (body, work, reached, complete) ->
      let source = "extern U0 Print(U8 *fmt,...);" ^ body in
      let report = run ~max_initializer_steps:(work - 1) source in
      ignore (Output.fault ~output:reached "HCIRVM0007" report);
      let progress = Option.get (integer_program_report_progress report) in
      Alcotest.(check int)
        "bounded reached preparation" (work - 1)
        progress.runtime.initializer_steps;
      ignore (run ~max_initializer_steps:work source |> Output.expect complete))
    [
      ( {|Print("A");I64 X[1+1];Print("B");I64 Y[1+1];#exe {Print("C");}42;|},
        6,
        "AB",
        "ABC" );
      ({|Print("A");I64 X[1+1][#exe {Print("B");}1+1];42;|}, 6, "AB", "AB");
      ({|Print("A");#exe {}I64 X[1+1];42;|}, 3, "A", "A");
    ]

let dimensions_share_initializer_budget () =
  List.iter
    (fun prefix ->
      let source =
        "extern U0 Print(U8 *fmt,...);" ^ prefix
        ^ {|Print("A");I64 X[1+1];#exe {Print("B");}42;|}
      in
      let report = run source in
      ignore (Output.expect "AB" report);
      let work =
        (Option.get (integer_program_report_progress report)).runtime
          .initializer_steps
      in
      Alcotest.(check bool)
        "earlier value preparation also charged" true (work > 3);
      ignore (run ~max_initializer_steps:work source |> Output.expect "AB");
      ignore
        (run ~max_initializer_steps:(work - 1) source
        |> Output.fault ~output:"A" "HCIRVM0007"))
    [ "I64 N=40;"; "I64 F(I64 x=40){return x;};" ]

let ordinary_declaration_defaults () =
  List.iter
    (fun source ->
      let report = run source in
      ignore (Output.expect "" report);
      Alcotest.(check bool)
        "ordinary defaults activate their original task" true
        (Option.is_some (integer_program_report_progress report));
      Alcotest.(check bool)
        "default source exposes separate task units" true
        (integer_program_report_program report = None))
    [
      {|I64 F(I64 x=42){return x;};F();|};
      {|I64 N=20;I64 Next(){return ++N;};I64 Saved(I64 n=Next()){return n;};N=0;Saved()+Saved();|};
      {|I64 N=41;I64 Unused(I64 x=++N){return x;};N;|};
      {|I64 N=41;extern I64 Unused(I64 x=++N);N;|};
      {|I64 F(I64 x=21){return x;};I64 G(){return F()+F();};G();|};
      {|I64 F(I64 x=21){return x;};I64 G(I64 x=F()+F()){return x;};G();|};
      {|I64 F(I64 x=42){return x;};F()+defined(Print)+defined(PutChars)+defined(StreamPrint);|};
    ]

let aot_declaration_defaults () =
  List.iter
    (fun source ->
      ignore (Output.run ~mode:Preprocessor.Aot source |> Output.expect ""))
    [
      {|I64 F(I64 x=42){return x;};F();|};
      {|I64 F(I64 x=20+1){return x;};F()+F();|};
      {|I64 F(I64 x=21){return x;};I64 G(){return F()+F();};G();|};
      {|I64 F(I64 x=sizeof U8+41){return x;};F();|};
      {|I64 N=0;I64 F(I64 x=defined(N)+41){return x;};F();|};
      {|I64 F(U8 x=298){return x;};F();|};
    ]

let aot_default_order_and_limits () =
  let run = Output.run ~mode:Preprocessor.Aot in
  let source = {|#exe {Print("A");}I64 F(I64 x=20+22){return x;};F();|} in
  let report = run ~max_initializer_steps:2 source in
  ignore (Output.fault ~output:"A" "HCIRVM0007" report);
  Alcotest.(check int)
    "AOT default retains reached preparation" 2
    (Option.get (integer_program_report_progress report)).runtime
      .initializer_steps;
  ignore (run ~max_initializer_steps:5 source |> Output.expect "A");
  let source =
    {|extern I64 Unused(I64 x=20+22);#exe {I64 N=20+22;StreamPrint("42;");}|}
  in
  let measured = run source in
  ignore (Output.expect "" measured);
  let steps =
    (Option.get (integer_program_report_progress measured)).runtime
      .initializer_steps
  in
  ignore
    (run ~max_initializer_steps:(steps - 1) source |> Output.fault "HCIRVM0007");
  ignore (run ~max_initializer_steps:steps source |> Output.expect "");
  ignore
    (run {|I64 F(I64 x=42){return x;};#exe {Print("A");}F();|}
    |> Output.expect "A");
  ignore
    (run {|I64 N=42;I64 F(I64 x=N){return x;};F();|} |> Output.fault "HCRUN0006");
  ignore
    (run {|I64 G(){return 42;};I64 F(I64 x=G()){return x;};F();|}
    |> Output.fault "HCRUN0006")

let runtime_dimensions () =
  List.iter
    (fun body ->
      ignore (run (body ^ "A[0]+A[1]+N-1;") |> Output.expect "");
      List.iter
        (fun mode ->
          ignore
            (Output.run ~mode
               ("#exe {" ^ body ^ "StreamPrint(\"%d;\",A[0]+A[1]+N-1);}")
            |> Output.expect ""))
        [ Preprocessor.Jit; Preprocessor.Aot ])
    [
      "I64 N=0;I64 Next(){return ++N;};I64 A[Next()+1]=20,22;";
      "I64 N=0;I64 A[++N+1]=20,22;";
      "I64 N=1;I64 A[N+1]=20,22;";
    ];
  List.iter
    (fun source -> ignore (run source |> Output.expect ""))
    [
      "I64 N=0;I64 Next(){return ++N;};I64 F(){I64 A[Next()+1];return sizeof \
       A;};N*26+F();";
      "I64 N=0;I64 Next(){return ++N;};I64 F(){I64 A[Next()+1];return sizeof \
       A;};N*10+F()+F();";
      "I64 N=0;I64 Next(){return ++N;};I64 Unused(){I64 A[Next()+1];return \
       sizeof A;};N+41;";
      "I64 N=0;I64 Next(){return ++N;};I64 A[2][Next()];sizeof A+N+25;";
      "I64 N=0;I64 Next(){return ++N;};I64 A[Next()][2];sizeof A+N+25;";
      "I64 N=0;I64 Next(){return ++N;};I64 A[Next()][Next()];sizeof A+N+24;";
      "I64 N=2;I64 A[N];I64 B[sizeof A/8];sizeof B+26;";
      "I64 N=2;I64 A[N];I64 F(I64 x=sizeof A+26){return x;};F();";
    ]

let runtime_dimension_activation () =
  ignore (run "I64 N=2;I64 A[N];42;" |> Output.expect "");
  let source = "I64 Closed[2];I64 N=0;I64 A[++N+1];N+sizeof A+25;" in
  let report = run source in
  ignore (Output.expect "" report);
  let work = (Option.get (integer_program_report_progress report)).runtime in
  ignore
    (run ~max_steps:work.executed_steps
       ~max_initializer_steps:work.initializer_steps source
    |> Output.expect "");
  ignore
    (run ~max_initializer_steps:(work.initializer_steps - 1) source
    |> Output.fault "HCIRVM0007")

let runtime_dimension_failures () =
  List.iter
    (fun (code, body) ->
      let report =
        run
          ({|extern U0 Print(U8 *fmt,...);77;I64 Bound(I64 n){Print("A");return n;};|}
         ^ body)
      in
      ignore (Output.fault ~output:"A" code report);
      let progress = Option.get (integer_program_report_progress report) in
      Alcotest.(check (option int64))
        "dimension failure retains preceding result" (Some 77L)
        (Option.map (fun word -> word.VM.bits) progress.runtime.final_value))
    [
      ("HCRUN0004", {|I64 A[Bound(-1)][Bound(2)];Print("late");|});
      ("HCPARSE0023", {|I64 A[Bound(2);Print("late");|});
      ( "HCIRVM0009",
        {|I64 Bad(I64 n){return 1/n;};I64 A[Bad(Bound(0))];Print("late");|} );
    ];
  ignore
    (run
       {|extern U0 Print(U8 *fmt,...);I64 Bound(){Print("A");return 2;};I64 A[Bound() #exe {Print("B");}];sizeof A+26;|}
    |> Output.expect "BA");
  ignore
    (Output.run ~mode:Preprocessor.Aot "I64 N=2;I64 A[N];42;"
    |> Output.fault "HCRUN0006")

let runtime_dimension_limits () =
  let source =
    "I64 N=0;I64 Next(){return ++N;};I64 A[Next()+1][1+1];sizeof A+N+9;"
  in
  let report = run source in
  ignore (Output.expect "" report);
  let measured =
    (Option.get (integer_program_report_progress report)).runtime
  in
  ignore
    (run ~max_steps:measured.executed_steps
       ~max_initializer_steps:measured.initializer_steps source
    |> Output.expect "");
  ignore
    (run ~max_steps:(measured.executed_steps - 1) source
    |> Output.fault "HCIRVM0007");
  ignore
    (run ~max_initializer_steps:(measured.initializer_steps - 1) source
    |> Output.fault "HCIRVM0007")

let ordinary_default_failures () =
  List.iter
    (fun (code, body) ->
      let source = {|extern U0 Print(U8 *fmt,...);Print("A");77;|} ^ body in
      let report = run source in
      ignore (Output.fault ~output:"A" code report);
      let progress = Option.get (integer_program_report_progress report) in
      Alcotest.(check (option int64))
        "default fault retains earlier outer result" (Some 77L)
        (Option.map (fun word -> word.VM.bits) progress.runtime.final_value))
    [
      ( "HCIRVM0009",
        {|I64 Bad(I64 d){return 1/d;};I64 F(I64 x=Bad(0)){return x;};Print("late");|}
      );
      ("HCRUN0003", {|I64 F(I64 x=Missing){return x;};Print("late");|});
      ( "HCRUN0006",
        {|I64 G(U8 *s){return 42;};I64 F(I64 x=G("value")){return x;};Print("late");|}
      );
    ]

let ordinary_default_limits () =
  let source =
    {|extern U0 Print(U8 *fmt,...);I64 N=20;I64 Next(){return ++N;};I64 Saved(I64 n=Next()){return n;};Print("A");N=0;Saved()+Saved();|}
  in
  let report = run source in
  ignore (Output.expect "A" report);
  let measured =
    (Option.get (integer_program_report_progress report)).runtime
  in
  ignore
    (run ~max_steps:measured.executed_steps
       ~max_initializer_steps:measured.initializer_steps
       ~max_global_bytes:measured.global_bytes
       ~max_literal_bytes:measured.literal_bytes ~max_output_bytes:1
       ~max_output_work:measured.output_work source
    |> Output.expect "A");
  ignore
    (run ~max_steps:(measured.executed_steps - 1) source
    |> Output.fault ~output:"A" "HCIRVM0007");
  ignore
    (run ~max_initializer_steps:(measured.initializer_steps - 1) source
    |> Output.fault "HCIRVM0007")

let original_read_timing () =
  ignore
    (run {|I64 F(){return 42;};#exe {StreamPrint("%d;",F());}|}
    |> Output.expect "");
  ignore
    (run {|I64 F(){return 42;}#exe {StreamPrint("%d;",F());}|}
    |> Output.fault "HCIRVM0030");
  ignore
    (run
       {|I64 N=0;I64 F(I64 x=F(#exe {N=42;Print("late");StreamPrint("1");})){return x;};|}
    |> Output.fault ~output:"late" "HCPARSE0025");
  List.iter
    (fun source -> ignore (run source |> Output.fault "HCRUN0003"))
    [
      {|I64 F(){return Future;}#exe {Print("late");I64 Future=42;}F();|};
      {|I64 F(){I64 N=40;#exe {StreamPrint("%d;",N+2);}return 42;};F();|};
    ]

let tests =
  [
    Alcotest.test_case "ordinary JIT inputs retain isolated counts" `Quick
      inactive_text;
    Alcotest.test_case "stateful reports retain cumulative task ownership"
      `Quick stateful_report;
    Alcotest.test_case
      "compilation inspection distinguishes isolated and stateful artifacts"
      `Quick compilation_inspection;
    Alcotest.test_case "reached output survives parse and execution failures"
      `Quick reached_failures;
    Alcotest.test_case "outer and stream commands share exact resource limits"
      `Quick limits;
    Alcotest.test_case "deferred dimension failure preserves earlier commands"
      `Quick deferred_dimension_budget;
    Alcotest.test_case "dimension events charge before and after activation"
      `Quick dimension_event_order;
    Alcotest.test_case "dimensions share earlier initializer and default work"
      `Quick dimensions_share_initializer_budget;
    Alcotest.test_case "ordinary JIT defaults execute at declaration time"
      `Quick ordinary_declaration_defaults;
    Alcotest.test_case "ordinary AOT defaults retain output-owned constants"
      `Quick aot_declaration_defaults;
    Alcotest.test_case
      "AOT defaults preserve output namespace and shared limits" `Quick
      aot_default_order_and_limits;
    Alcotest.test_case "ordinary default failures preserve earlier execution"
      `Quick ordinary_default_failures;
    Alcotest.test_case "ordinary defaults share exact task limits" `Quick
      ordinary_default_limits;
    Alcotest.test_case "directives retain function and identifier read timing"
      `Quick original_read_timing;
    Alcotest.test_case "runtime dimensions execute at declaration time" `Quick
      runtime_dimensions;
    Alcotest.test_case
      "runtime dimension activates from its original complete trace" `Quick
      runtime_dimension_activation;
    Alcotest.test_case "runtime dimension faults preserve reached effects"
      `Quick runtime_dimension_failures;
    Alcotest.test_case "runtime dimensions share exact task allowances" `Quick
      runtime_dimension_limits;
  ]
