open Holyc_lib
module Native = Native_source_execution
module Image = X86_64_program
module Cases = Callback_expression_cases

let describe errors =
  errors
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let inputs text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-callback-expressions.hc"
      ~contents:text
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:Preprocessor.Jit ()
    |> Result.get_ok
  in
  (session, config, source)

let run ?(max_steps = 100_000) ?max_ir_instructions ?max_code_bytes
    ?max_frame_bytes text =
  let session, config, source = inputs text in
  Native.evaluate ?max_ir_instructions ?max_code_bytes ?max_frame_bytes session
    ~config ~source ~max_steps

let value expected report =
  match Native.outcome report with
  | Error errors -> Alcotest.fail (describe errors)
  | Ok result ->
      Alcotest.(check int64)
        "complete native word" expected
        (Option.get result.value.final_value).bits;
      Alcotest.(check int)
        "no interpreter execution" 0
        (Option.get (Native.source_progress report)).runtime.executed_steps;
      List.iter
        (fun (fragment : Native.fragment) ->
          match fragment.native_outcome with
          | Some (Ok (Image.Completed _)) -> ()
          | _ -> Alcotest.fail "source fragment did not execute natively")
        (Native.fragments report)

let agrees (label, text, expected) =
  let native = run text in
  value expected native;
  let session, config, source = inputs text in
  let interpreted =
    run_integer_program_report session ~config ~source ~max_steps:100_000
  in
  match integer_program_report_outcome interpreted with
  | Error errors -> Alcotest.failf "%s: %s" label (describe errors)
  | Ok result ->
      Alcotest.(check int64)
        "independent source IR" expected
        (Option.get (Ir_integer_interpreter.final_value result.value)).bits;
      Alcotest.(check string)
        "independent original effects"
        (integer_program_report_output_bytes interpreted)
        (Native.output_bytes native)

let fault kind report =
  match (Native.outcome report, List.rev (Native.fragments report)) with
  | Error _, { native_outcome = Some (Ok (Image.Fault fault)); _ } :: _ ->
      Alcotest.(check bool) "reached native fault" true (fault.kind = kind);
      Alcotest.(check int)
        "actual cumulative work" fault.executed_steps
        (Native.executed_steps report)
  | Error errors, _ -> Alcotest.fail (describe errors)
  | Ok _, _ -> Alcotest.fail "expected a reached native fault"

let words () = List.iter agrees (Cases.cases @ Cases.jit_cases)

let owned_words () =
  List.iter
    (fun consumer ->
      let text =
        "extern U0 PutChars(U64 ch);I64 F(){return 42;}I64 Run(){I64 \
         (*p)(),(*q)();p=&F;q=0;PutChars('B');return " ^ consumer ^ ";}Run();"
      in
      let report = run text in
      fault Image.Callback_owned_word_escape report;
      Alcotest.(check string)
        "output before original consumer" "B"
        (Native.output_bytes report);
      let session, config, source = inputs text in
      let interpreted =
        run_integer_program_report session ~config ~source ~max_steps:100_000
      in
      Alcotest.(check string)
        "independent reached output" "B"
        (integer_program_report_output_bytes interpreted);
      match integer_program_report_outcome interpreted with
      | Error errors ->
          Alcotest.(check bool)
            "independent owner fault" true
            (List.exists
               (fun (error : Diagnostic.t) -> error.code = "HCIRVM0024")
               errors)
      | Ok _ -> Alcotest.fail "IR extracted owned code as a number")
    Cases.owned_consumers

let effects_and_faults () =
  agrees
    ( "original indices",
      "extern U0 PutChars(U64 ch);I64 (*p)()[2];I64 Mark(I64 \
       n){PutChars(n);return 1;}I64 Run(){return \
       (p[Mark('L')]=Mark('R')*40)|2;}Run();",
      42L );
  List.iter
    (fun (kind, text) -> fault kind (run text))
    [
      (Image.Division_by_zero, "I64 (*p)()=42;p/0;");
      (Image.Division_by_zero, "I64 (*p)()=42;p%0;");
      (Image.Signed_division_overflow, "I64 (*p)()=0x8000000000000000;p/-1;");
      (Image.Signed_division_overflow, "I64 (*p)()=0x8000000000000000;p%-1;");
      (Image.Uninitialized_read, "I64 Run(){I64 (*p)();return p|2;}Run();");
      (Image.Address_out_of_bounds, "I64 (*p)()[2]={0,0};p[2]|2;");
    ];
  let text =
    "extern U0 PutChars(U64 ch);I64 F(){return 42;}I64 \
     Side(){PutChars('R');return 2;}I64 Run(){I64 (*p)();p=&F;return \
     p|Side();}Run();"
  in
  let report = run text in
  fault Image.Callback_owned_word_escape report;
  Alcotest.(check string)
    "right effects before numeric owner check" "R"
    (Native.output_bytes report)

let exact_limits () =
  let text =
    "I64 Run(){I64 (*p)(),(*q)();p=370;q=42;return ((p+1)-q)|0;}Run();"
  in
  let report = run text in
  value 42L report;
  let code, ir =
    List.fold_left
      (fun (code, ir) (fragment : Native.fragment) ->
        (code + fragment.image.code_bytes, ir + fragment.image.ir_instructions))
      (0, 0) (Native.fragments report)
  in
  value 42L
    (run
       ~max_steps:(Native.executed_steps report)
       ~max_code_bytes:code ~max_ir_instructions:ir ~max_frame_bytes:16 text);
  fault Image.Step_limit_exceeded
    (run ~max_steps:(Native.executed_steps report - 1) text);
  fault Image.Frame_limit_exceeded (run ~max_frame_bytes:15 text);
  List.iter
    (fun (code, report) ->
      match Native.outcome report with
      | Error errors ->
          Alcotest.(check bool)
            "one below compilation quota" true
            (List.exists
               (fun (error : Diagnostic.t) -> error.code = code)
               errors)
      | Ok _ -> Alcotest.fail "one below quota executed")
    [
      ("HCBACK0005", run ~max_code_bytes:(code - 1) text);
      ("HCBACK0001", run ~max_ir_instructions:(ir - 1) text);
    ];
  Gc.full_major ();
  Gc.compact ();
  agrees ("collected", text, 42L)

let () =
  Alcotest.run "Original numeric callback expressions"
    [
      ( "numeric consumers",
        [
          Alcotest.test_case "all numeric operators, classes and storage shapes"
            `Quick words;
          Alcotest.test_case "owned callbacks fault at reached consumers" `Quick
            owned_words;
          Alcotest.test_case "original effects, bounds and arithmetic faults"
            `Quick effects_and_faults;
          Alcotest.test_case "exact code, IR, frame and execution limits" `Quick
            exact_limits;
        ] );
    ]
