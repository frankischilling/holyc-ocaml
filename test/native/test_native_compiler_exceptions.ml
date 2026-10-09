open Holyc_lib
module Cases = Compiler_exception_cases
module Native = Native_source_execution
module Helpers = Test_compiler_exceptions

let run ?(max_steps = 100_000) ?max_output_bytes ?max_output_work mode text =
  let session, source, config =
    Helpers.inputs mode
      ((if mode = Preprocessor.Jit then Cases.headers else "") ^ text)
  in
  Native.evaluate ?max_output_bytes ?max_output_work ~max_code_bytes:524_288
    session ~source ~config ~max_steps

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

let inherited_function_failure () =
  List.iter
    (fun mode ->
      let report = run mode Cases.inherited_function_failure in
      let diagnostics =
        match Native.outcome report with
        | Error errors -> errors
        | Ok _ ->
            Alcotest.fail "unsupported inherited return unexpectedly executed"
      in
      Alcotest.(check bool)
        (Helpers.describe diagnostics)
        true
        (List.exists
           (fun (diagnostic : Diagnostic.t) -> diagnostic.code = "HCPARSE0169")
           diagnostics);
      Alcotest.(check int)
        "inherited function supplies no missing-function Compiler" 0
        (List.length (Native.compiler_exceptions report));
      Alcotest.(check string)
        "earlier native effects remain" "kept"
        (Native.output_bytes report);
      Option.iter
        (fun progress ->
          Alcotest.(check int)
            "no interpreted inherited task instructions" 0
            progress.Integer_task.runtime.executed_steps)
        (Native.source_progress report);
      Alcotest.(check bool)
        "earlier original native work remains charged" true
        (Native.executed_steps report > 0);
      Alcotest.(check bool)
        "earlier fragments completed in machine code" true
        (List.exists
           (fun (fragment : Native.fragment) ->
             match fragment.native_outcome with
             | Some (Ok (X86_64_program.Completed _)) -> true
             | _ -> false)
           (Native.fragments report)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let caught_children () =
  List.iter
    (fun mode ->
      List.iter
        (fun (label, text, output, count) ->
          let report = run mode text in
          let result =
            match Native.outcome report with
            | Ok result -> result
            | Error errors ->
                Alcotest.fail (label ^ ": " ^ Helpers.describe errors)
          in
          Alcotest.(check (option int64))
            (label ^ " original native outer value")
            (Some 42L)
            (Option.map
               (fun (word : Native.word) -> word.bits)
               result.value.final_value);
          Alcotest.(check string)
            (label ^ " reached native output")
            output
            (Native.output_bytes report);
          let exceptions = Native.compiler_exceptions report in
          Alcotest.(check int)
            (label ^ " original Compiler count")
            count (List.length exceptions);
          List.iter
            (fun exception_ ->
              Helpers.receipt label
                [ Parser.compiler_exception_diagnostic exception_ ]
                [ exception_ ])
            exceptions;
          Option.iter
            (fun progress ->
              Alcotest.(check int)
                "no interpreted caught-child instructions" 0
                progress.Integer_task.runtime.executed_steps)
            (Native.source_progress report);
          Alcotest.(check bool)
            "actual reached native work remains charged" true
            (Native.executed_steps report > 0);
          Alcotest.(check bool)
            "original fragments complete in machine code" true
            (List.exists
               (fun (fragment : Native.fragment) ->
                 match fragment.native_outcome with
                 | Some (Ok (X86_64_program.Completed _)) -> true
                 | _ -> false)
               (Native.fragments report)))
        Cases.caught_children)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let faults_after_catches () =
  List.iter
    (fun mode ->
      List.iter
        (fun (label, text) ->
          let report = run mode text in
          Alcotest.(check bool)
            (label ^ " remains a native failure")
            true
            (Result.is_error (Native.outcome report));
          Alcotest.(check int)
            (label ^ " retains only the caught Compiler")
            1
            (List.length (Native.compiler_exceptions report));
          Alcotest.(check string)
            (label ^ " no later native effects")
            "kept"
            (Native.output_bytes report);
          Option.iter
            (fun progress ->
              Alcotest.(check int)
                "failed native task does not interpret instructions" 0
                progress.Integer_task.runtime.executed_steps)
            (Native.source_progress report))
        Cases.faults_after_caught_children;
      List.iter
        (fun (limit, count, output) ->
          let report =
            run ~max_output_bytes:limit mode
              {|#exe {StreamExePrint("Print(\"kept\");return 42;");Print("after");}42;|}
          in
          Alcotest.(check bool)
            "native output quota remains a failure" true
            (Result.is_error (Native.outcome report));
          Alcotest.(check int)
            "native quota retains reached Compiler count" count
            (List.length (Native.compiler_exceptions report));
          Alcotest.(check string)
            "native quota preserves reached bytes" output
            (Native.output_bytes report))
        [ (3, 0, ""); (4, 1, "kept") ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let statement_failures () =
  List.iter
    (fun mode ->
      List.iter
        (fun (label, text, code, marker, output) ->
          let report = run mode text in
          let diagnostics =
            match Native.outcome report with
            | Error errors -> errors
            | Ok _ -> Alcotest.fail (label ^ " unexpectedly executed")
          in
          Helpers.receipt ~code ~marker label diagnostics
            (Native.compiler_exceptions report);
          Alcotest.(check string)
            (label ^ " reached native effects")
            output
            (Native.output_bytes report);
          Option.iter
            (fun progress ->
              Alcotest.(check int)
                "statement failure interprets no task instructions" 0
                progress.Integer_task.runtime.executed_steps)
            (Native.source_progress report))
        Cases.statement_failures)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let statement_caught_children () =
  List.iter
    (fun mode ->
      List.iter
        (fun (label, text, code, marker, output) ->
          let report = run mode text in
          let result =
            match Native.outcome report with
            | Ok result -> result
            | Error errors ->
                Alcotest.fail (label ^ ": " ^ Helpers.describe errors)
          in
          Alcotest.(check (option int64))
            (label ^ " resumes native parent")
            (Some 42L)
            (Option.map
               (fun (word : Native.word) -> word.bits)
               result.value.final_value);
          Helpers.receipt ~code ~marker label
            (List.map Parser.compiler_exception_diagnostic
               (Native.compiler_exceptions report))
            (Native.compiler_exceptions report);
          Alcotest.(check string)
            (label ^ " native effects remain")
            output
            (Native.output_bytes report);
          Alcotest.(check bool)
            "original native work remains charged" true
            (Native.executed_steps report > 0);
          Option.iter
            (fun progress ->
              Alcotest.(check int)
                "caught statement does not interpret task instructions" 0
                progress.Integer_task.runtime.executed_steps)
            (Native.source_progress report);
          Alcotest.(check bool)
            "original native fragments complete" true
            (List.exists
               (fun (fragment : Native.fragment) ->
                 match fragment.native_outcome with
                 | Some (Ok (X86_64_program.Completed _)) -> true
                 | _ -> false)
               (Native.fragments report)))
        Cases.statement_caught_children)
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
          Alcotest.test_case
            "inherited saved function failure has no Compiler authority" `Quick
            inherited_function_failure;
          Alcotest.test_case
            "caught child Compiler preserves original native work" `Quick
            caught_children;
          Alcotest.test_case
            "caught native children preserve incomplete bindings and quota \
             faults"
            `Quick faults_after_catches;
          Alcotest.test_case "original native statement Compiler phases" `Quick
            statement_failures;
          Alcotest.test_case "caught statement Compiler preserves native parent"
            `Quick statement_caught_children;
        ] );
    ]
