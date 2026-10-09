open Holyc_lib
module Cases = Compiler_exception_cases
module Native = Native_source_execution
module Helpers = Test_compiler_exceptions

let run ?(with_headers = true) ?(max_steps = 100_000) ?max_output_bytes
    ?max_output_work ?status_abi mode text =
  let session, source, config =
    Helpers.inputs mode
      ((if with_headers && mode = Preprocessor.Jit then Cases.headers else "")
      ^ text)
  in
  Native.evaluate ?max_output_bytes ?max_output_work ?status_abi
    ~max_code_bytes:524_288 session ~source ~config ~max_steps

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

let call_producers () =
  List.iter
    (fun (label, text, code, marker) ->
      let report = run ~with_headers:false Preprocessor.Jit text in
      let diagnostics =
        match Native.outcome report with
        | Error errors -> errors
        | Ok _ -> Alcotest.fail (label ^ " unexpectedly executed")
      in
      Helpers.call_receipt ~reported_origin:false ~code ~marker label
        diagnostics
        (Native.compiler_exceptions report))
    Cases.call_failures;
  List.iter
    (fun mode ->
      List.iter
        (fun (label, with_headers, text, code, marker, output) ->
          let report = run ~with_headers mode text in
          let result =
            match Native.outcome report with
            | Ok result -> result
            | Error errors ->
                Alcotest.fail (label ^ ": " ^ Helpers.describe errors)
          in
          Alcotest.(check (option int64))
            (label ^ " native parent resumes")
            (Some 42L)
            (Option.map
               (fun (word : Native.word) -> word.bits)
               result.value.final_value);
          Alcotest.(check string)
            (label ^ " retained native effects")
            output
            (Native.output_bytes report);
          let exceptions = Native.compiler_exceptions report in
          Helpers.call_receipt ~code ~marker label
            (List.map Parser.compiler_exception_diagnostic exceptions)
            exceptions;
          Alcotest.(check bool)
            "native execution remains charged" true
            (Native.executed_steps report > 0);
          Option.iter
            (fun progress ->
              Alcotest.(check int)
                "no interpreted task instructions" 0
                progress.Integer_task.runtime.executed_steps)
            (Native.source_progress report))
        Cases.call_caught_children)
    [ Preprocessor.Jit; Preprocessor.Aot ];
  List.iter
    (fun mode ->
      List.iter
        (fun (label, text, output) ->
          let report = run ~with_headers:false mode text in
          let result =
            match Native.outcome report with
            | Ok result -> result
            | Error errors ->
                Alcotest.fail (label ^ ": " ^ Helpers.describe errors)
          in
          Alcotest.(check (option int64))
            (label ^ " generated native result")
            (Some 42L)
            (Option.map
               (fun (word : Native.word) -> word.bits)
               result.value.final_value);
          Alcotest.(check string)
            (label ^ " output") output
            (Native.output_bytes report);
          Alcotest.(check int)
            (label ^ " no Compiler throw")
            0
            (List.length (Native.compiler_exceptions report));
          Alcotest.(check bool)
            "actual native work" true
            (Native.executed_steps report > 0);
          Option.iter
            (fun progress ->
              Alcotest.(check int)
                "no interpreted task work" 0
                progress.Integer_task.runtime.executed_steps)
            (Native.source_progress report))
        Cases.call_successes)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let expression_producers () =
  List.iter
    (fun mode ->
      let no_interpretation label report =
        Option.iter
          (fun progress ->
            Alcotest.(check int)
              (label ^ " no interpreted task instructions")
              0 progress.Integer_task.runtime.executed_steps)
          (Native.source_progress report)
      in
      List.iter
        (fun (label, text, code, marker) ->
          let report = run mode text in
          let diagnostics =
            match Native.outcome report with
            | Error errors -> errors
            | Ok _ -> Alcotest.fail (label ^ " unexpectedly executed")
          in
          (match
             if mode = Preprocessor.Aot then
               Cases.expression_native_aot_earlier_error label
             else None
           with
          | None ->
              Helpers.expression_receipt ~reported_origin:false ~code ~marker
                label diagnostics
                (Native.compiler_exceptions report)
          | Some earlier_code ->
              Alcotest.(check (list string))
                (label ^ " earlier native AOT declaration boundary")
                [ earlier_code ]
                (List.map (fun (d : Diagnostic.t) -> d.code) diagnostics);
              Alcotest.(check int)
                (label ^ " unreached type rejection has no Compiler authority")
                0
                (List.length (Native.compiler_exceptions report)));
          no_interpretation label report)
        Cases.expression_failures;
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
            (label ^ " original native parent resumes")
            (Some 42L)
            (Option.map
               (fun (word : Native.word) -> word.bits)
               result.value.final_value);
          Alcotest.(check string)
            (label ^ " retains reached native output")
            output
            (Native.output_bytes report);
          let exceptions = Native.compiler_exceptions report in
          Helpers.expression_receipt ~code ~marker label
            (List.map Parser.compiler_exception_diagnostic exceptions)
            exceptions;
          no_interpretation label report;
          Alcotest.(check bool)
            (label ^ " reached machine work remains charged")
            true
            (Native.executed_steps report > 0);
          Alcotest.(check bool)
            (label ^ " native fragments completed")
            true
            (List.exists
               (fun (fragment : Native.fragment) ->
                 match fragment.native_outcome with
                 | Some (Ok (X86_64_program.Completed _)) -> true
                 | _ -> false)
               (Native.fragments report)))
        Cases.expression_caught_children;
      List.iter
        (fun (label, text, expected) ->
          let report = run mode text in
          let result =
            match Native.outcome report with
            | Ok result -> result
            | Error errors ->
                Alcotest.fail (label ^ ": " ^ Helpers.describe errors)
          in
          Alcotest.(check (option int64))
            label (Some expected)
            (Option.map
               (fun (word : Native.word) -> word.bits)
               result.value.final_value);
          Alcotest.(check int)
            (label ^ " no Compiler throw")
            0
            (List.length (Native.compiler_exceptions report));
          no_interpretation label report)
        Cases.expression_successes;
      let public_cast = run mode Cases.expression_public_postfix_cast in
      (match Native.outcome public_cast with
      | Ok _ ->
          Alcotest.fail "public postfix native boundary unexpectedly emitted"
      | Error diagnostics ->
          Alcotest.(check bool)
            "public postfix cast retains explicit native boundary" true
            (List.exists
               (fun (diagnostic : Diagnostic.t) ->
                 diagnostic.code = "HCBACK0002")
               diagnostics));
      Alcotest.(check int)
        "public postfix cast backend failure has no Compiler authority" 0
        (List.length (Native.compiler_exceptions public_cast));
      no_interpretation "public postfix cast" public_cast;
      List.iter
        (fun (label, text) ->
          let report = run mode text in
          Alcotest.(check bool)
            (label ^ " remains failed")
            true
            (Result.is_error (Native.outcome report));
          Alcotest.(check int)
            (label ^ " no Compiler cleanup")
            0
            (List.length (Native.compiler_exceptions report));
          no_interpretation label report)
        (Cases.expression_noncompiler_failures
        @
        if mode = Preprocessor.Aot then
          Cases.expression_aot_noncompiler_failures
        else []);
      let report = run mode Cases.expression_successive_catches in
      List.iter
        (fun (label, text) ->
          let child = run mode text in
          Alcotest.(check bool)
            (label ^ " unaudited native child is not caught")
            true
            (Result.is_error (Native.outcome child));
          Alcotest.(check int)
            (label ^ " no native child Compiler authority")
            0
            (List.length (Native.compiler_exceptions child));
          Alcotest.(check string)
            (label ^ " native child reached output")
            "kept"
            (Native.output_bytes child);
          no_interpretation label child)
        Cases.expression_uncaught_children;
      let nested = run mode Cases.expression_nested_directive in
      Alcotest.(check bool)
        "native nested directive fails" true
        (Result.is_error (Native.outcome nested));
      Helpers.nested_expression_receipt "native nested directive"
        (Native.outcome nested |> Result.get_error)
        (Native.compiler_exceptions nested);
      no_interpretation "native nested directive" nested;
      let reached = run mode Cases.expression_reached_group_directive in
      Alcotest.(check string)
        "native group close retains reached directive output" "reached"
        (Native.output_bytes reached);
      Helpers.expression_receipt ~reported_origin:false ~code:"HCPARSE0019"
        ~marker:";" "native group close after directive"
        (Native.outcome reached |> Result.get_error)
        (Native.compiler_exceptions reached);
      no_interpretation "native group close after directive" reached;
      let caught = run mode Cases.expression_caught_nested_directive in
      Alcotest.(check bool)
        "native nested cleanup catch resumes" true
        (Result.is_ok (Native.outcome caught));
      Alcotest.(check string)
        "native nested cleanup output" "keptafter"
        (Native.output_bytes caught);
      let exceptions = Native.compiler_exceptions caught in
      Helpers.nested_expression_receipt "native caught nested directive"
        (List.map Parser.compiler_exception_diagnostic exceptions)
        exceptions;
      no_interpretation "native caught nested directive" caught;
      Alcotest.(check bool)
        "successive native expression catches resume" true
        (Result.is_ok (Native.outcome report));
      Alcotest.(check string)
        "successive native catch output" "abafter"
        (Native.output_bytes report);
      no_interpretation "successive catches" report;
      (match Native.compiler_exceptions report with
      | [ first; cleanup; second; cleanup2 ] ->
          List.iter
            (fun exceptions ->
              Helpers.expression_receipt ~code:"HCPARSE0018" ~marker:";"
                "successive native catch"
                (List.map Parser.compiler_exception_diagnostic exceptions)
                exceptions)
            [ [ first; cleanup ]; [ second; cleanup2 ] ]
      | _ -> Alcotest.fail "native catches must retain both two-producer chains");
      let report = run mode Cases.expression_fault_after_catch in
      Alcotest.(check bool)
        "runtime fault after native catch remains failed" true
        (Result.is_error (Native.outcome report));
      Alcotest.(check int)
        "later runtime fault creates no cleanup" 2
        (List.length (Native.compiler_exceptions report));
      Alcotest.(check string)
        "runtime fault retains native child output" "kept"
        (Native.output_bytes report);
      no_interpretation "later runtime fault" report;
      List.iter
        (fun (limit, expected_count, output) ->
          let report =
            run ~max_output_bytes:limit mode Cases.expression_quota_after_catch
          in
          Alcotest.(check bool)
            "native quota remains failed" true
            (Result.is_error (Native.outcome report));
          Alcotest.(check int)
            "native quota preserves reached producer count" expected_count
            (List.length (Native.compiler_exceptions report));
          Alcotest.(check string)
            "native quota preserves reached child bytes" output
            (Native.output_bytes report);
          no_interpretation "native quota" report)
        [ (3, 0, ""); (4, 2, "kept") ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let retained_variadic_flags_with_fixed_members () =
  let text = "extern I64 F(...);I64 F(I64 n){return n+1;}F(41);" in
  let session, source, config = Helpers.inputs Preprocessor.Jit text in
  let compiled =
    match compile_integer_program session ~source ~config with
    | Ok result -> result.value
    | Error errors -> Alcotest.fail (Helpers.describe errors)
  in
  let definition = List.hd (integer_program_functions compiled) in
  let flags = Ir_function_body.stored_flags definition.body in
  Alcotest.(check bool)
    "original joined variadic flag retained" true
    (Function_flag.Stored.is_set ~mask:flags Function_flag.Stored.Variadic);
  Alcotest.(check bool)
    "retained flag prevents deriving Ret1" false
    (Function_flag.Stored.is_set ~mask:flags Function_flag.Stored.Ret1);
  Alcotest.(check bool)
    "replacement frame has no invented argc or argv" true
    (Option.is_none
       (Semantic_function_type_resolution.function_variadic_bindings
          (Semantic_function_frame_layout.function_header definition.frame)));
  List.iter
    (fun status_abi ->
      List.iter
        (fun text ->
          let report =
            run ~with_headers:false ~status_abi Preprocessor.Jit text
          in
          let result =
            match Native.outcome report with
            | Ok result -> result
            | Error errors -> Alcotest.fail (Helpers.describe errors)
          in
          Alcotest.(check (option int64))
            "fixed joined machine result" (Some 42L)
            (Option.map
               (fun (word : Native.word) -> word.bits)
               result.value.final_value);
          List.iter
            (fun (fragment : Native.fragment) ->
              Alcotest.(check bool)
                "original fragment completes in machine code" true
                (match fragment.native_outcome with
                | Some (Ok (X86_64_program.Completed _)) -> true
                | _ -> false))
            (Native.fragments report);
          Option.iter
            (fun progress ->
              Alcotest.(check int)
                "no interpreter fallback" 0
                progress.Integer_task.runtime.executed_steps)
            (Native.source_progress report))
        [
          text;
          "extern I64 F(...);I64 F(I64 n=41){return n+1;}F();";
          "extern I64 F(...);I64 F(I64 n){if(n)return F(n-1);return 42;}F(100);";
          "argpop extern I64 F(...);I64 F(I64 n){return n+1;}F(41);";
          "noargpop extern I64 F(...);I64 F(I64 n){return n+1;}F(41);";
        ])
    [
      (if Sys.win32 then X86_64_program.Windows_x64
       else X86_64_program.System_v_x64);
    ];
  let foreign_abi =
    if Sys.win32 then X86_64_program.System_v_x64
    else X86_64_program.Windows_x64
  in
  let foreign =
    run ~with_headers:false ~status_abi:foreign_abi Preprocessor.Jit text
  in
  Alcotest.(check bool)
    "foreign task ABI still rejects before execution" true
    (match Native.outcome foreign with
    | Error errors ->
        List.exists (fun (d : Diagnostic.t) -> d.code = "HCNATIVE0002") errors
    | Ok _ -> false);
  Alcotest.(check int)
    "foreign ABI rejection has no Compiler authority" 0
    (List.length (Native.compiler_exceptions foreign));
  let report =
    run ~with_headers:false Preprocessor.Jit
      "interrupt extern I64 F(...);I64 F(I64 n){return n+1;}F(41);"
  in
  Alcotest.(check bool)
    "other calling flags still reject" true
    (match Native.outcome report with
    | Error errors ->
        List.exists (fun (d : Diagnostic.t) -> d.code = "HCBACK0002") errors
    | Ok _ -> false);
  Alcotest.(check int)
    "flag rejection cannot forge Compiler" 0
    (List.length (Native.compiler_exceptions report))

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
          Alcotest.test_case "call Compiler producers and nested native catches"
            `Quick call_producers;
          Alcotest.test_case
            "expression Compiler cleanup preserves native execution" `Quick
            expression_producers;
          Alcotest.test_case
            "retained joined flags use original fixed call frames" `Quick
            retained_variadic_flags_with_fixed_members;
        ] );
    ]
