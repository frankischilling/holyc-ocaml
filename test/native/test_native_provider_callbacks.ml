open Holyc_lib
module Native = Native_source_execution
module Image = X86_64_program
module Cases = Provider_callback_cases

let describe errors =
  errors
  |> List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
  |> String.concat "; "

let inputs text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"provider-callbacks.hc" ~contents:text
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:Preprocessor.Jit ()
    |> Result.get_ok
  in
  (session, config, source)

let run ?(max_steps = 100_000) ?max_call_depth ?max_frame_bytes
    ?max_output_bytes ?max_output_work ?max_code_bytes ?max_ir_instructions text
    =
  let session, config, source = inputs text in
  Native.evaluate ?max_call_depth ?max_frame_bytes ?max_output_bytes
    ?max_output_work ?max_code_bytes ?max_ir_instructions session ~config
    ~source ~max_steps

let success output report =
  match Native.outcome report with
  | Error errors -> Alcotest.fail (describe errors)
  | Ok result ->
      Alcotest.(check int64)
        "complete native word" 42L (Option.get result.value.final_value).bits;
      Alcotest.(check string)
        "native provider bytes" output
        (Native.output_bytes report);
      Alcotest.(check int)
        "zero interpreter instructions" 0
        (Option.get (Native.source_progress report)).runtime.executed_steps;
      List.iter
        (fun (fragment : Native.fragment) ->
          match fragment.native_outcome with
          | Some (Ok (Image.Completed _)) -> ()
          | _ ->
              Alcotest.fail "provider source fragment did not execute natively")
        (Native.fragments report)

let reached kind output report =
  Alcotest.(check string)
    "bytes before reached native fault" output
    (Native.output_bytes report);
  match (Native.outcome report, List.rev (Native.fragments report)) with
  | Error _, { native_outcome = Some (Ok (Image.Fault fault)); _ } :: _ ->
      Alcotest.(check bool) "actual machine fault" true (fault.kind = kind);
      Alcotest.(check int)
        "cumulative native work" fault.executed_steps
        (Native.executed_steps report)
  | Error errors, _ -> Alcotest.fail (describe errors)
  | Ok _, _ -> Alcotest.fail "expected a native fault"

let sources () =
  List.iter
    (fun (name, text, output) ->
      success output (run text);
      let session, config, source = inputs text in
      let interpreted =
        run_integer_program_report session ~config ~source ~max_steps:100_000
      in
      match integer_program_report_outcome interpreted with
      | Error errors -> Alcotest.failf "%s: %s" name (describe errors)
      | Ok result ->
          Alcotest.(check int64)
            "independent IR word" 42L
            (Option.get (Ir_integer_interpreter.final_value result.value)).bits;
          Alcotest.(check string)
            "independent IR bytes" output
            (integer_program_report_output_bytes interpreted))
    Cases.cases

let signatures () =
  reached Image.Undefined_extern "" (run Cases.self_placeholder);
  List.iter
    (fun (_, text) -> reached Image.Callback_signature_mismatch "M" (run text))
    Cases.mismatches;
  reached Image.Callback_owned_word_escape "B"
    (run (Cases.header ^ "U0 (*p)(U64 ch)=&PutChars;PutChars('B');p+0;"))

let quotas () =
  let text = Cases.header ^ "U0 (*p)(U64 ch)=&PutChars;p('AB');42;" in
  let report = run text in
  success "AB" report;
  let steps = Native.executed_steps report in
  success "AB"
    (run ~max_steps:steps ~max_output_bytes:2 ~max_output_work:4 text);
  reached Image.Output_limit_exceeded "A" (run ~max_output_bytes:1 text);
  reached Image.Output_work_limit_exceeded "A" (run ~max_output_work:3 text);
  reached Image.Step_limit_exceeded "AB" (run ~max_steps:(steps - 1) text);
  let nested =
    Cases.header ^ "I64 Run(U0 (*p)(U64 ch)){p('A');return 42;}Run(&PutChars);"
  in
  reached Image.Call_depth_exceeded "" (run ~max_call_depth:1 nested);
  reached Image.Frame_limit_exceeded "" (run ~max_frame_bytes:15 nested);
  let nested_output =
    Cases.header ^ "I64 Run(U0 (*p)(U64 ch)){p('AB');return 42;}Run(&PutChars);"
  in
  reached Image.Output_limit_exceeded "A"
    (run ~max_output_bytes:1 nested_output);
  let code, ir =
    List.fold_left
      (fun (code, ir) (fragment : Native.fragment) ->
        (code + fragment.image.code_bytes, ir + fragment.image.ir_instructions))
      (0, 0) (Native.fragments report)
  in
  success "AB" (run ~max_code_bytes:code ~max_ir_instructions:ir text);
  List.iter
    (fun (code, report) ->
      match Native.outcome report with
      | Error ds ->
          Alcotest.(check bool)
            "one below compiler allowance" true
            (List.exists (fun (d : Diagnostic.t) -> d.code = code) ds)
      | Ok _ -> Alcotest.fail "one below compilation limit succeeded")
    [
      ("HCBACK0005", run ~max_code_bytes:(code - 1) text);
      ("HCBACK0001", run ~max_ir_instructions:(ir - 1) text);
    ]

let lifetimes () =
  List.iter
    (fun (_, text, output) ->
      Gc.full_major ();
      Gc.compact ();
      success output (run text))
    (List.filter
       (fun (name, _, _) ->
         name = "later joined definition"
         || name = "default before joined definition")
       Cases.cases)

let () =
  Alcotest.run "Original provider callback entries"
    [
      ( "entries",
        [
          Alcotest.test_case
            "captures, storage, defaults and independent effects" `Quick sources;
          Alcotest.test_case "reached signature and numeric-owner faults" `Quick
            signatures;
          Alcotest.test_case "exact work, output, frame and compilation limits"
            `Quick quotas;
          Alcotest.test_case "historical entries survive collection" `Quick
            lifetimes;
        ] );
    ]
