open Holyc_lib
module Cases = Compiler_exception_cases
module Native = Native_source_execution
module Helpers = Test_compiler_exceptions

let run ?(max_steps = 100_000) mode text =
  let session, source, config =
    Helpers.inputs mode
      ((if mode = Preprocessor.Jit then Cases.headers else "") ^ text)
  in
  Native.evaluate ~max_code_bytes:524_288 session ~source ~config ~max_steps

let source_failures () =
  List.iter
    (fun mode ->
      List.iter
        (fun (label, text, output) ->
          let report = run mode text in
          let diagnostics =
            match Native.outcome report with
            | Error errors -> errors
            | Ok _ -> Alcotest.fail (label ^ " unexpectedly succeeded")
          in
          Helpers.receipt
            (label ^ ": " ^ Helpers.describe diagnostics)
            diagnostics
            (Native.compiler_exceptions report);
          Alcotest.(check string)
            (label ^ " reached native output")
            output
            (Native.output_bytes report);
          Option.iter
            (fun progress ->
              Alcotest.(check int)
                "no interpreted instructions" 0
                progress.Integer_task.runtime.executed_steps)
            (Native.source_progress report);
          if output <> "" then (
            Alcotest.(check bool)
              "earlier output executed in machine code" true
              (List.exists
                 (fun (fragment : Native.fragment) ->
                   match fragment.native_outcome with
                   | Some (Ok (X86_64_program.Completed _)) -> true
                   | _ -> false)
                 (Native.fragments report));
            Alcotest.(check bool)
              "earlier native work remains charged" true
              (Native.executed_steps report > 0)))
        Cases.failures)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let other_failures () =
  List.iter
    (fun (_, text, _) ->
      let report = run Preprocessor.Jit text in
      Alcotest.(check bool)
        "ordinary source error fails" true
        (Result.is_error (Native.outcome report));
      Alcotest.(check int)
        "ordinary error does not become Compiler" 0
        (List.length (Native.compiler_exceptions report)))
    Cases.ordinary_failures;
  let report =
    run ~max_steps:1 Preprocessor.Jit {|#exe {Print("kept");return 42;}42;|}
  in
  Alcotest.(check bool)
    "earlier native quota fails" true
    (Result.is_error (Native.outcome report));
  Alcotest.(check int)
    "quota prevents later Compiler producer" 0
    (List.length (Native.compiler_exceptions report))

let active_functions () =
  List.iter
    (fun mode ->
      let report = run mode "I64 F(){return 42;}F();" in
      let checked =
        match Native.outcome report with
        | Ok checked -> checked
        | Error errors -> Alcotest.fail (Helpers.describe errors)
      in
      Alcotest.(check (option int64))
        "native function value" (Some 42L)
        (Option.map
           (fun (word : Native.word) -> word.bits)
           checked.value.final_value);
      Alcotest.(check int)
        "active return has no Compiler signal" 0
        (List.length (Native.compiler_exceptions report)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let () =
  Alcotest.run "Native compiler exceptions"
    [
      ( "source",
        [
          Alcotest.test_case "original root, directive and saved child failures"
            `Quick source_failures;
          Alcotest.test_case "native runtime and quota faults remain separate"
            `Quick other_failures;
          Alcotest.test_case "active native function returns" `Quick
            active_functions;
        ] );
    ]
