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

let original_read_timing () =
  ignore
    (run {|I64 F(){return 42;};#exe {StreamPrint("%d;",F());}|}
    |> Output.expect "");
  List.iter
    (fun source -> ignore (run source |> Output.fault "HCRUN0003"))
    [
      {|I64 F(){return 42;}#exe {StreamPrint("%d;",F());}|};
      {|I64 F(){return Future;}#exe {Print("late");I64 Future=42;}F();|};
      {|I64 N=0;I64 F(I64 x=F(#exe {N=42;Print("late");StreamPrint("1");})){return x;};|};
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
    Alcotest.test_case "directives retain function and identifier read timing"
      `Quick original_read_timing;
  ]
