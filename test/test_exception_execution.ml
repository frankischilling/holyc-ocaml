open Holyc_lib
module VM = Ir_integer_interpreter
module Cases = Exception_execution_cases

let require_ok show = function
  | Ok value -> value
  | Error error -> Alcotest.fail (show error)

let show_diagnostics diagnostics =
  diagnostics
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let config mode =
  Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id

let run ?(max_steps = 100_000) ?max_frame_bytes ?max_call_depth
    ?max_initializer_steps ?max_global_bytes ?max_output_bytes ?max_output_work
    ~mode program =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"exception-execution.hc"
      ~contents:(Cases.headers ^ program)
  in
  run_integer_program_report ~max_steps ?max_frame_bytes ?max_call_depth
    ?max_initializer_steps ?max_global_bytes ?max_output_bytes ?max_output_work
    session ~config:(config mode) ~source

let first_error label report =
  match integer_program_report_outcome report with
  | Ok _ -> Alcotest.failf "%s unexpectedly succeeded" label
  | Error diagnostics -> (
      match
        List.find_opt
          (fun error -> error.Diagnostic.severity = Diagnostic.Error)
          diagnostics
      with
      | Some error -> error
      | None -> Alcotest.failf "%s reported no error diagnostic" label)

let expect_case (case : Cases.success) report =
  let checked =
    integer_program_report_outcome report
    |> require_ok (fun diagnostics ->
        case.name ^ ": " ^ show_diagnostics diagnostics)
  in
  let execution = checked.value in
  Alcotest.(check bool)
    (case.name ^ " reaches stream end")
    true
    (VM.termination execution = VM.Stream_end);
  Alcotest.(check string)
    (case.name ^ " keeps exact output")
    case.output
    (integer_program_report_output_bytes report);
  (match VM.final_value execution with
  | Some word ->
      Alcotest.(check bool)
        (case.name ^ " result type")
        true (word.VM.type_ = VM.I64);
      Alcotest.(check int64) (case.name ^ " value") case.value word.bits
  | None -> Alcotest.failf "%s returned no word" case.name);
  execution

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let source_execution_tests =
  List.concat_map
    (fun mode ->
      let mode_name =
        match mode with
        | Preprocessor.Jit -> "JIT"
        | Preprocessor.Aot -> "AOT"
      in
      List.map
        (fun (case : Cases.success) ->
          Alcotest.test_case
            (mode_name ^ ": " ^ case.name)
            `Quick
            (fun () ->
              let report = run ~mode case.Cases.program in
              ignore (expect_case case report)))
        Cases.success_cases)
    modes

let invalid_control_transfers () =
  List.iter
    (fun mode ->
      List.iter
        (fun case ->
          let error = first_error case.Cases.name (run ~mode case.program) in
          (* Original PrsStmt.HC emits plain IC_JMP for goto. It has no
             SysUntry for crossing exception regions; the verified source
             lowering must reject those transfers before execution. *)
          Option.iter
            (fun expected ->
              Alcotest.(check string)
                (case.name ^ " original source rejection")
                expected error.code)
            case.code)
        Cases.invalid_transfers)
    modes

let unhandled_throws () =
  List.iter
    (fun mode ->
      List.iter
        (fun case ->
          let error = first_error case.Cases.name (run ~mode case.program) in
          Option.iter
            (fun code ->
              Alcotest.(check string) (case.name ^ " code") code error.code)
            case.code)
        Cases.uncaught)
    modes

let repeat_invocation_after_exception () =
  let handled =
    List.find
      (fun (case : Cases.success) ->
        case.Cases.name = "subsequent calls register fresh exception frames")
      Cases.success_cases
  in
  List.iter
    (fun mode ->
      for _attempt = 1 to 3 do
        let execution = run ~mode handled.program |> expect_case handled in
        Alcotest.(check bool)
          "each fresh session executes independently" true
          (VM.executed_steps execution > 0)
      done)
    modes

let exact_step_boundary_and_recovery () =
  List.iter
    (fun mode ->
      let baseline = run ~mode Cases.quota_source in
      let expected = Cases.successful "quota baseline" Cases.quota_source 42L in
      let execution = expect_case expected baseline in
      let exact_steps = VM.executed_steps execution in
      Alcotest.(check bool)
        "meaningful exception work reached" true (exact_steps > 1);
      let exact = run ~mode ~max_steps:exact_steps Cases.quota_source in
      let exact_result = expect_case expected exact in
      Alcotest.(check int)
        "exact bound charges every executed instruction once" exact_steps
        (VM.executed_steps exact_result);
      let one_short =
        run ~mode ~max_steps:(exact_steps - 1) Cases.quota_source
      in
      Alcotest.(check string)
        "quota exhaustion is not swallowed by catch" "HCIRVM0007"
        (first_error "one-step-below" one_short).code;
      ignore (run ~mode Cases.quota_source |> expect_case expected))
    modes

let owned_task_memory_boundary () =
  let expected =
    Cases.successful "owned Fs task bytes" Cases.quota_source 42L
  in
  List.iter
    (fun mode ->
      let baseline = run ~mode Cases.quota_source in
      ignore (expect_case expected baseline);
      let progress =
        match integer_program_report_progress baseline with
        | Some progress -> progress
        | None ->
            Alcotest.fail "Fs exception task needs a runtime progress report"
      in
      let bytes = progress.runtime.global_bytes in
      Alcotest.(check bool)
        "source CTask through catch_except requires 889 owned bytes" true
        (bytes >= 889);
      ignore
        (run ~mode ~max_global_bytes:bytes Cases.quota_source
        |> expect_case expected);
      let rejected =
        run ~mode ~max_global_bytes:(bytes - 1) Cases.quota_source
      in
      Alcotest.(check string)
        "one less byte cannot be hidden by catch" "HCIRVM0016"
        (first_error "task byte limit" rejected).code)
    modes

let depth_and_frame_limits_are_not_holyc_throws () =
  let source =
    {|I64 Recurse(I64 n){if(n)return Recurse(n-1)+1;return 40;}I64 F(){I64 r=0;try {r=Recurse(2);}catch {r=999;Fs->catch_except=TRUE;}return r;}F();|}
  in
  List.iter
    (fun mode ->
      let baseline = Cases.successful "call depth baseline" source 42L in
      ignore (run ~mode source |> expect_case baseline);
      let limited_depth = run ~mode ~max_call_depth:1 source in
      Alcotest.(check string)
        "call-depth failure bypasses source catch" "HCIRVM0015"
        (first_error "call depth" limited_depth).code;
      let limited_frame = run ~mode ~max_frame_bytes:8 source in
      Alcotest.(check string)
        "frame quota failure bypasses source catch" "HCIRVM0011"
        (first_error "frame quota" limited_frame).code;
      ignore (run ~mode source |> expect_case baseline))
    modes

let caught_child_frame_limits () =
  let shared =
    {|I64 Saved=0;U0 Child(){I64 Held[64];Held[0]=42;Saved=Held[0];throw('Leaf');}I64 Inspect(){I64 Scratch[64];Scratch[0]=Saved;return Scratch[0];}|}
  in
  let during =
    shared
    ^ {|I64 F(){try Child();catch {Inspect();Fs->catch_except=TRUE;}return Saved;}F();|}
  and after =
    shared
    ^ {|I64 F(){try Child();catch {Fs->catch_except=TRUE;}return Inspect();}F()+F()-42;|}
  in
  List.iter
    (fun mode ->
      (* _TEST_EXCEPT keeps the throwing stack: 512 child bytes, the
         120-byte CExcept, 16 throw argument bytes and 512 inspection bytes.
         _TAKE_EXCEPT releases the child before normal source resumes. *)
      let expected = Cases.successful "live child frame at catch" during 42L in
      ignore (run ~mode ~max_frame_bytes:1160 during |> expect_case expected);
      Alcotest.(check string)
        "one byte below the live catch stack fails" "HCIRVM0011"
        (first_error "live child frame"
           (run ~mode ~max_frame_bytes:1159 during))
          .code;
      let expected =
        Cases.successful "accepted child frame is released" after 42L
      in
      ignore (run ~mode ~max_frame_bytes:648 after |> expect_case expected);
      Alcotest.(check string)
        "one byte below the throwing stack fails" "HCIRVM0011"
        (first_error "throw frame" (run ~mode ~max_frame_bytes:647 after)).code)
    modes

let earlier_output_survives_unhandled_throw () =
  let source =
    {|I64 F(){Print("kept");try {Print("body");throw('Escaped');}catch {Print("handler");}return 42;}F();|}
  in
  List.iter
    (fun mode ->
      let report = run ~mode source in
      ignore (first_error "unhandled after output" report);
      Alcotest.(check string)
        "both reached effects survive exception propagation" "keptbodyhandler"
        (integer_program_report_output_bytes report))
    modes

let unhandled_diagnostic_uses_current_task_code () =
  let source =
    {|I64 F(){try throw('Outer');catch {try throw('Inner');catch {Fs->catch_except=TRUE;}Fs->catch_except=FALSE;}return 42;}F();|}
  in
  List.iter
    (fun mode ->
      let error =
        first_error "unhandled current task code" (run ~mode source)
      in
      Alcotest.(check string)
        "still an unhandled source throw" "HCEX0001" error.code;
      Alcotest.(check bool)
        "unhandled diagnostic follows the nested throw's retained Fs code" true
        (String.ends_with ~suffix:"0x72656e6e49" error.message))
    modes

let provider_failures_bypass_catch () =
  let source =
    {|I64 F(){try {Print("A");Print("B");}catch {Print("caught");Fs->catch_except=TRUE;}return 42;}F();|}
  in
  List.iter
    (fun mode ->
      let limited = run ~mode ~max_output_bytes:1 source in
      Alcotest.(check string)
        "provider failure remains an output limit" "HCIRVM0022"
        (first_error "provider output limit" limited).code;
      Alcotest.(check string)
        "reached byte survives and catch is not entered" "A"
        (integer_program_report_output_bytes limited))
    modes

let tests =
  source_execution_tests
  @ [
      Alcotest.test_case "illegal transfers across protected regions" `Quick
        invalid_control_transfers;
      Alcotest.test_case "unhandled throw and bounded catch reentry" `Quick
        unhandled_throws;
      Alcotest.test_case "fresh exception frames on repeated invocation" `Quick
        repeat_invocation_after_exception;
      Alcotest.test_case "exception work consumes exact runtime steps" `Quick
        exact_step_boundary_and_recovery;
      Alcotest.test_case "owned task bytes respect exact global limit" `Quick
        owned_task_memory_boundary;
      Alcotest.test_case "host depth and frame quotas bypass handlers" `Quick
        depth_and_frame_limits_are_not_holyc_throws;
      Alcotest.test_case "caught child frames retain and release exact bytes"
        `Quick caught_child_frame_limits;
      Alcotest.test_case "unhandled throws retain reached output" `Quick
        earlier_output_survives_unhandled_throw;
      Alcotest.test_case "unhandled diagnostic keeps the current task code"
        `Quick unhandled_diagnostic_uses_current_task_code;
      Alcotest.test_case "provider failures bypass source catches" `Quick
        provider_failures_bypass_catch;
    ]
