open Holyc_lib
module Cases = Lock_execution_cases
module Native = Native_program
module Source = Native_source_execution
module Program = X86_64_program
module Runtime = Native_program_execution

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let mode_name = function
  | Preprocessor.Jit -> "JIT"
  | Preprocessor.Aot -> "AOT"

let diagnostics errors =
  errors
  |> List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
  |> String.concat "; "

let inputs mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-lock-execution.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
  in
  (session, config, source)

let run ?(max_steps = 100_000) ?max_frame_bytes ?max_global_bytes ~mode contents
    =
  let session, config, source = inputs mode contents in
  Native.evaluate ~max_code_bytes:1_048_576 ?max_frame_bytes ?max_global_bytes
    session ~config ~source ~max_steps

let run_source ?(max_steps = 100_000) ~mode contents =
  let session, config, source = inputs mode contents in
  Source.evaluate ~max_code_bytes:1_048_576 session ~config ~source ~max_steps

let first_error label = function
  | Ok _ -> Alcotest.fail (label ^ " unexpectedly completed")
  | Error errors -> (
      match
        List.find_opt
          (fun (d : Diagnostic.t) -> d.severity = Diagnostic.Error)
          errors
      with
      | Some error -> error
      | None -> Alcotest.fail (label ^ " failed without an error diagnostic"))

let expect (case : Cases.success) report =
  let result =
    match Native.outcome report with
    | Ok checked -> checked.value
    | Error errors -> Alcotest.fail (case.name ^ ": " ^ diagnostics errors)
  in
  Alcotest.(check bool)
    (case.name ^ " emitted native code")
    true
    (Option.is_some (Native.image report));
  (match result.execution.final_value with
  | Some word ->
      Alcotest.(check bool)
        (case.name ^ " native I64 result")
        true
        (word.Program.type_ = Program.I64);
      Alcotest.(check int64) (case.name ^ " native bits") case.value word.bits
  | None -> Alcotest.fail (case.name ^ " has no native value"));
  Alcotest.(check string)
    (case.name ^ " native output")
    case.output
    (Native.output_bytes report);
  result.execution

let expect_source (case : Cases.success) report =
  let result =
    match Source.outcome report with
    | Ok checked -> checked.value
    | Error errors -> Alcotest.fail (case.name ^ ": " ^ diagnostics errors)
  in
  (match result.final_value with
  | Some word ->
      Alcotest.(check bool)
        (case.name ^ " native task I64 result")
        true (word.type_ = Program.I64);
      Alcotest.(check int64)
        (case.name ^ " native task bits")
        case.value word.bits
  | None -> Alcotest.fail (case.name ^ " has no native source value"));
  Alcotest.(check string)
    (case.name ^ " native task output")
    case.output
    (Source.output_bytes report);
  let fragments = Source.fragments report in
  Alcotest.(check bool)
    "retained source made real native fragments" true (fragments <> []);
  List.iter
    (fun (fragment : Source.fragment) ->
      match fragment.native_outcome with
      | Some (Ok (Program.Completed _)) -> ()
      | _ ->
          Alcotest.fail
            "successful lock source has no native fragment completion")
    fragments;
  Option.iter
    (fun progress ->
      Alcotest.(check int)
        "lock source does not use interpreter runtime instructions" 0
        progress.Integer_task.runtime.executed_steps)
    (Source.source_progress report)

let source_tests =
  List.concat_map
    (fun mode ->
      List.map
        (fun (case : Cases.success) ->
          Alcotest.test_case
            (mode_name mode ^ ": " ^ case.name)
            `Quick
            (fun () -> ignore (run ~mode case.program |> expect case)))
        Cases.success_cases)
    modes

let rejects_before_native_entry () =
  List.iter
    (fun mode ->
      List.iter
        (fun (case : Cases.failure) ->
          let report = run ~mode case.program in
          Alcotest.(check string)
            case.name case.code
            (first_error case.name (Native.outcome report)).code;
          Alcotest.(check string)
            "rejection preserves zero reached output" ""
            (Native.output_bytes report);
          Alcotest.(check bool)
            "invalid LOCK consumer emits no native image" true
            (Option.is_none (Native.image report));
          Alcotest.(check (option int))
            "invalid LOCK consumer never enters native code" None
            (Native.executed_steps report))
        Cases.invalid_cases;
      List.iter
        (fun contents ->
          let report = run ~mode contents in
          ignore
            (first_error "invalid locked bit signature" (Native.outcome report));
          Alcotest.(check string)
            "invalid bit signature executes no effects" ""
            (Native.output_bytes report);
          Alcotest.(check bool)
            "invalid bit signature produces no executable image" true
            (Option.is_none (Native.image report)))
        Cases.invalid_bit_signatures)
    modes

let native_faults () =
  List.iter
    (fun mode ->
      List.iter
        (fun (case : Cases.failure) ->
          let report = run ~mode case.program in
          Alcotest.(check string)
            case.name case.code
            (first_error case.name (Native.outcome report)).code;
          Alcotest.(check string)
            (case.name ^ " keeps reached effects")
            case.output
            (Native.output_bytes report);
          Alcotest.(check bool)
            "checked access faults after code generation" true
            (Option.is_some (Native.image report));
          match Native.native_outcome report with
          | Some (Program.Fault _) -> ()
          | _ -> Alcotest.fail (case.name ^ " has no actual native fault"))
        Cases.fault_cases)
    modes

let exact_runtime_quota () =
  let case = Cases.successful "native lock quota" Cases.quota_source 42L in
  List.iter
    (fun mode ->
      let execution = run ~mode case.program |> expect case in
      let steps = execution.Program.executed_steps in
      ignore (run ~mode ~max_steps:steps case.program |> expect case);
      let short = run ~mode ~max_steps:(steps - 1) case.program in
      Alcotest.(check string)
        "one-below native lock runtime quota" "HCIRVM0007"
        (first_error case.name (Native.outcome short)).code;
      (match Native.native_outcome short with
      | Some (Program.Fault fault) ->
          Alcotest.(check bool)
            "the generated guard enforces the limit" true
            (fault.kind = Program.Step_limit_exceeded)
      | _ -> Alcotest.fail "lock quota has no native step-limit fault");
      ignore (run ~mode case.program |> expect case))
    modes

let source_task_quota () =
  let case =
    Cases.successful "retained lock quota" "I64 G=40;lock G+=2;G;" 42L
  in
  List.iter
    (fun mode ->
      let baseline = run_source ~mode case.program in
      expect_source case baseline;
      let steps = Source.executed_steps baseline in
      expect_source case (run_source ~mode ~max_steps:steps case.program);
      let short = run_source ~mode ~max_steps:(steps - 1) case.program in
      Alcotest.(check string)
        "retained atomic commands share the exact runtime allowance"
        "HCIRVM0007" (first_error case.name (Source.outcome short)).code;
      Alcotest.(check int)
        "retained source cannot exceed its native allowance" (steps - 1)
        (Source.executed_steps short))
    modes

let retained_native_sources () =
  List.iter
    (fun (case : Cases.success) ->
      expect_source case (run_source ~mode:Preprocessor.Jit case.program))
    Cases.retained_cases;
  let directive = List.hd (List.rev Cases.retained_cases) in
  expect_source directive (run_source ~mode:Preprocessor.Aot directive.program)

let () =
  match Runtime.platform () with
  | Runtime.Unsupported ->
      Alcotest.fail "native lock execution tests require x86-64"
  | Runtime.Windows_x86_64 | Runtime.Linux_x86_64 ->
      Alcotest.run "holyc native lock execution"
        [
          ("public source results", source_tests);
          ( "checked native entry and retained tasks",
            List.map
              (fun (name, test) -> Alcotest.test_case name `Quick test)
              [
                ( "invalid consumers reject before native entry",
                  rejects_before_native_entry );
                ( "native bounds initialization and alignment faults",
                  native_faults );
                ("exact native runtime allowance", exact_runtime_quota);
                ("exact retained native allowance", source_task_quota);
                ( "retained source runs actual native fragments",
                  retained_native_sources );
              ] );
        ]
