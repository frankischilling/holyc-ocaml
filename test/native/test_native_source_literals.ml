open Holyc_lib
module Native = Native_source_execution
module Image = X86_64_program

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let diagnostics errors =
  errors
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let inputs text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-source-literals.hc" ~contents:text
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:Preprocessor.Jit () |> checked
  in
  (session, config, source)

let run ?max_literal_bytes ?max_global_bytes ?max_output_bytes ?max_output_work
    ?max_code_bytes ?max_ir_instructions ?(max_steps = 100_000) text =
  let session, config, source = inputs text in
  Native.evaluate ?max_literal_bytes ?max_global_bytes ?max_output_bytes
    ?max_output_work ?max_code_bytes ?max_ir_instructions session ~config
    ~source ~max_steps

let value ?(output = "") expected report =
  let result =
    Native.outcome report |> Result.map_error diagnostics |> checked
  in
  Alcotest.(check int64)
    "original native value" expected (Option.get result.value.final_value).bits;
  Alcotest.(check string)
    "original native bytes" output
    (Native.output_bytes report);
  let progress = Option.get (Native.source_progress report) in
  Alcotest.(check int)
    "no interpreter execution" 0 progress.runtime.executed_steps;
  List.iter
    (fun (fragment : Native.fragment) ->
      match fragment.native_outcome with
      | Some (Ok (Image.Completed _)) -> ()
      | _ -> Alcotest.fail "fragment did not complete natively")
    (Native.fragments report)

let diagnostic code report =
  match Native.outcome report with
  | Error errors ->
      Alcotest.(check bool)
        (diagnostics errors) true
        (List.exists (fun (error : Diagnostic.t) -> error.code = code) errors)
  | Ok _ -> Alcotest.fail "source unexpectedly completed"

let compare_ir expected output text =
  value ~output expected (run ~max_code_bytes:262_144 text);
  let session, config, source = inputs text in
  let result =
    run_integer_program_report session ~config ~source ~max_steps:100_000
  in
  let outcome =
    integer_program_report_outcome result
    |> Result.map_error diagnostics
    |> checked
  in
  Alcotest.(check int64)
    "independent IR result" expected
    (Option.get (Ir_integer_interpreter.final_value outcome.value)).bits;
  Alcotest.(check string)
    "independent IR output" output
    (integer_program_report_output_bytes result)

let format_source =
  "extern U0 Print(U8 *fmt,...);I64 F(){Print(\"%d;\",42);return 42;}F();"

let original_format () =
  let report = run format_source in
  value ~output:"42;" 42L report;
  let fragments = Native.fragments report in
  Alcotest.(check (list int))
    "literal admitted at original definition" [ 4; 4 ]
    (List.map
       (fun (fragment : Native.fragment) -> fragment.image.literal_bytes)
       fragments);
  Alcotest.(check (list int))
    "stable data and canonical table extent" [ 164; 164 ]
    (List.map
       (fun (fragment : Native.fragment) -> fragment.image.global_arena_bytes)
       fragments);
  compare_ir 42L "42;" format_source

let literal_forms () =
  List.iter
    (fun (expected, output, text) -> compare_ir expected output text)
    [
      (42L, "", "I64 F(){return (\"*\")[0];}F();");
      (42L, "42;", "extern U0 Print(U8 *fmt,...);Print(\"%d;\",42);42;");
      ( 42L,
        "x42;",
        "extern U0 Print(U8 *fmt,...);I64 F(){Print(\"%s%d;\",\"x\",42);return \
         42;}F();" );
      (42L, "", "I64 F(){return (\"\")[0]+42;}F();");
      (42L, "", "I64 F(){return (\"x\\0*\")[2];}F();");
      (42L, "", "I64 F(){U8 *p=\"*\";return (p+1)-p+41;}F();");
    ]

let mutation_and_new_storage () =
  List.iter
    (fun text -> compare_ir 67L "" text)
    [
      "I64 F(){U8 *p=\"A\";p[0]++;return p[0];}F();F();";
      "I64 F(){U8 *p=\"A\";p[0]++;return p[0];}F();I64 X=1;F();";
      "I64 F(){U8 *p=\"A\";p[0]++;return p[0];}F();I64 X[2]={20,22};F();";
      "I64 F(){U8 *p=\"A\";p[0]++;return p[0];}F();I64 G(){return \
       (\"B\")[0];}G();F();";
    ];
  compare_ir 42L "A42;B42;"
    "extern U0 Print(U8 *fmt,...);I64 F(){U8 \
     *p=\"A%d;\";Print(p,42);p[0]='B';return 42;}F();I64 X=1;F();"

let independent_producers () =
  compare_ir 66L ""
    "I64 F(){U8 *p=\"A\";p[0]++;return p[0];}I64 G(){U8 *p=\"A\";return \
     p[0];}F();G()+1;";
  compare_ir 66L ""
    "I64 F(){U8 *p=\"A\";p[0]++;return p[0];}I64 Old(){return F();}F();I64 \
     F(){return (\"B\")[0];}F();Old()-1;";
  compare_ir 42L "" "I64 F(){U8 *a=\"A\";U8 *b=\"A\";return (a!=b)+41;}F();"

let recursive_and_initializer_literals () =
  compare_ir 42L ""
    "I64 F(I64 n){U8 *p=\"*\";if(n){p[0]++;return F(n-1);}return p[0]-3;}F(3);";
  compare_ir 67L ""
    "I64 F(){U8 *p=\"A\";p[0]++;return p[0];}I64 A=F();I64 B=F();B;";
  compare_ir 42L "RLC42;"
    "extern U0 Print(U8 *fmt,...);extern U0 PutChars(U64 ch);I64 \
     Left(){PutChars('L');return 20;}I64 Right(){PutChars('R');return 22;}I64 \
     Add(I64 a,I64 b){PutChars('C');Print(\"%d;\",a+b);return \
     a+b;}Add(Left(),Right());"

let cumulative_literal_limits () =
  value ~output:"42;" 42L
    (run ~max_literal_bytes:4 ~max_global_bytes:1 format_source);
  let stopped = run ~max_literal_bytes:3 format_source in
  diagnostic "HCBACK0004" stopped;
  Alcotest.(check string)
    "rejected definition prints nothing" ""
    (Native.output_bytes stopped);
  let text =
    "extern U0 Print(U8 *fmt,...);Print(\"A\");I64 F(){Print(\"B\");return \
     42;}F();"
  in
  value ~output:"AB" 42L (run ~max_literal_bytes:4 ~max_code_bytes:262_144 text);
  let stopped = run ~max_literal_bytes:3 ~max_code_bytes:262_144 text in
  diagnostic "HCBACK0004" stopped;
  Alcotest.(check string)
    "later admission preserves earlier output" "A"
    (Native.output_bytes stopped);
  let repeated = "I64 F(){U8 *p=\"A\";p[0]++;return p[0];}F();F();" in
  value 67L (run ~max_literal_bytes:2 repeated)

let exact_work_and_code () =
  let report = run format_source in
  let code, ir =
    List.fold_left
      (fun (code, ir) (fragment : Native.fragment) ->
        (code + fragment.image.code_bytes, ir + fragment.image.ir_instructions))
      (0, 0) (Native.fragments report)
  in
  value ~output:"42;" 42L
    (run ~max_code_bytes:code ~max_ir_instructions:ir
       ~max_steps:(Native.executed_steps report)
       ~max_output_bytes:3
       ~max_output_work:(Native.output_work report)
       format_source);
  List.iter
    (fun (code, report) -> diagnostic code report)
    [
      ("HCBACK0005", run ~max_code_bytes:(code - 1) format_source);
      ("HCBACK0001", run ~max_ir_instructions:(ir - 1) format_source);
      ( "HCIRVM0007",
        run ~max_steps:(Native.executed_steps report - 1) format_source );
      ("HCIRVM0022", run ~max_output_bytes:2 format_source);
      ( "HCIRVM0023",
        run ~max_output_work:(Native.output_work report - 1) format_source );
    ]

let faults_preserve_reached_effects () =
  List.iter
    (fun (code, output, text) ->
      let report = run ~max_code_bytes:262_144 text in
      diagnostic code report;
      Alcotest.(check string)
        "earlier bytes survive original fault" output
        (Native.output_bytes report);
      match List.rev (Native.fragments report) with
      | { native_outcome = Some (Ok (Image.Fault _)); _ } :: _ -> ()
      | _ -> Alcotest.fail "literal fault did not reach native code")
    [
      ( "HCIRVM0009",
        "P42;",
        "extern U0 Print(U8 *fmt,...);Print(\"P\");I64 \
         F(){Print(\"%d;\",42);return 1/0;}F();" );
      ( "HCIRVM0025",
        "P",
        "extern U0 Print(U8 *fmt,...);Print(\"P\");I64 \
         F(){Print(\"%q\",42);return 42;}F();" );
      ("HCIRVM0019", "", "I64 F(){return (\"A\")[2];}F();");
    ]

let separate_tasks_and_collection () =
  let text = "I64 F(){U8 *p=\"A\";p[0]++;return p[0];}F();F();" in
  for _ = 1 to 8 do
    value 67L (run text);
    Gc.full_major ()
  done

let () =
  Alcotest.run "Native source literals"
    [
      ( "original storage",
        [
          Alcotest.test_case "retained format and exact arena" `Quick
            original_format;
          Alcotest.test_case "entry, function and byte forms" `Quick
            literal_forms;
          Alcotest.test_case "mutation survives later storage admission" `Quick
            mutation_and_new_storage;
          Alcotest.test_case "distinct producers and historical bodies" `Quick
            independent_producers;
          Alcotest.test_case "recursive and initializer calls" `Quick
            recursive_and_initializer_literals;
          Alcotest.test_case "cumulative literal and metadata limits" `Quick
            cumulative_literal_limits;
          Alcotest.test_case "exact runtime, output and compilation limits"
            `Quick exact_work_and_code;
          Alcotest.test_case "reached faults preserve output" `Quick
            faults_preserve_reached_effects;
          Alcotest.test_case "separate tasks and collection" `Quick
            separate_tasks_and_collection;
        ] );
    ]
