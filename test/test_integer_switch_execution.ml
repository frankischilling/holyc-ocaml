open Holyc_lib
module VM = Ir_integer_interpreter

let require_ok show = function
  | Ok value -> value
  | Error error -> Alcotest.fail (show error)

let diagnostics_text diagnostics =
  diagnostics
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let config mode =
  Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id

let report ?max_switch_work ?(max_steps = 100_000) ~mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"integer-switch-execution.hc" ~contents
  in
  run_integer_program_report ?max_switch_work session ~config:(config mode)
    ~source ~max_steps

let outcome report = integer_program_report_outcome report
let switch_work report = integer_program_report_switch_work report

let first_error report =
  match outcome report with
  | Ok _ -> Alcotest.fail "switch source unexpectedly succeeded"
  | Error [] -> Alcotest.fail "switch source returned no diagnostic"
  | Error (first :: _) -> first

let expect_word label expected report =
  match outcome report with
  | Error diagnostics -> Alcotest.fail (diagnostics_text diagnostics)
  | Ok checked -> (
      let result = checked.value in
      Alcotest.(check bool)
        (label ^ " reaches stream end")
        true
        (VM.termination result = VM.Stream_end);
      match VM.final_value result with
      | Some word ->
          Alcotest.(check bool)
            (label ^ " returns I64") true (word.type_ = VM.I64);
          Alcotest.(check int64) (label ^ " result") expected word.bits;
          result
      | None -> Alcotest.failf "%s produced no final word" label)

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let semantic_cases () =
  let cases =
    [
      ( "dft is an ordinary function label",
        "I64 Pick(){goto dft;switch(0){case 0:return 0;dft:return 42;}return \
         -1;}Pick();",
        42L );
      ( "closed floating case truncates to I64",
        "I64 Pick(I64 n){switch(n){case 2.75:return 42;default:return \
         0;}return -1;}Pick(2);",
        42L );
      ( "signed narrow selector sign extends",
        "I64 Pick(I8 n){switch(n){case -1:return 42;default:return 0;}return \
         -1;}Pick(255);",
        42L );
      ( "unsigned narrow selector zero extends",
        "I64 Pick(U8 n){switch(n){case 255:return 42;default:return 0;}return \
         -1;}Pick(-1);",
        42L );
      ( "simple selected case",
        "I64 Pick(I64 n){switch(n){case 1:return 42;default:return 0;}return \
         -1;}Pick(1);",
        42L );
      ( "default through a table hole",
        "I64 Pick(I64 n){switch(n){case 1:return 7;case 3:return \
         9;default:return 42;}return -1;}Pick(2);",
        42L );
      ( "negative selector",
        "I64 Pick(I64 n){switch(n){case -2:return 42;default:return 0;}return \
         -1;}Pick(-2);",
        42L );
      ( "high-bit U64 selector",
        "I64 Pick(U64 n){switch(n){case 0x8000000000000000:return \
         42;default:return 0;}return -1;}Pick(0x8000000000000000);",
        42L );
      ( "selector is evaluated once",
        "I64 Pick(){I64 n=0;switch(++n){case 1:n+=41;break;default:return \
         0;}return n;}Pick();",
        42L );
      ( "first implicit case starts at zero",
        "I64 Pick(I64 n){switch(n){case:return 42;default:return 0;}return \
         -1;}Pick(0);",
        42L );
      ( "implicit continuation",
        "I64 Pick(I64 n){switch(n){case 4:return 7;case:return \
         42;default:return 0;}return -1;}Pick(5);",
        42L );
      ( "descending inclusive range",
        "I64 Pick(I64 n){switch(n){case 5...3:return 42;default:return \
         0;}return -1;}Pick(4);",
        42L );
      ( "default middle fallthrough",
        "I64 Pick(I64 n){I64 v=0;switch(n){case 1:return 7;default:v=40;case \
         3:v+=2;break;case 4:return 9;}return v;}Pick(2);",
        42L );
      ( "nested switch and loop break",
        "I64 Pick(I64 n){while(1){switch(n){case \
         1:n=42;break;default:n=0;break;}break;}return n;}Pick(1);",
        42L );
      ( "nested switch keeps the nearest break targets",
        "I64 Pick(){I64 n=0;switch(0){case 0:switch(1){case \
         1:n=40;break;default:return 0;}n+=2;break;default:return 0;}return \
         n;}Pick();",
        42L );
      ( "switch body goto outside",
        "I64 Pick(I64 n){switch(n){case 1:goto done;default:return \
         0;}done:return 42;}Pick(1);",
        42L );
      ( "goto enters switch body",
        "I64 Pick(){goto inside;switch(0){case 0:inside:return \
         42;default:return 0;}return -1;}Pick();",
        42L );
      ( "unselected fault stays unreachable",
        "I64 Pick(I64 n){switch(n){case 1:return 42;case 2:return \
         1/0;default:return 0;}return -1;}Pick(1);",
        42L );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, expected) ->
          report ~mode source |> expect_word label expected |> ignore)
        cases)
    modes

let all_scalar_selector_widths () =
  let rows = [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32"; "I64"; "U64" ] in
  List.iter
    (fun mode ->
      List.iter
        (fun type_name ->
          let source =
            Printf.sprintf
              "I64 Pick(%s n){switch(n){case 1:return 42;default:return \
               0;}return -1;}Pick(1);"
              type_name
          in
          report ~mode source
          |> expect_word (type_name ^ " selector") 42L
          |> ignore)
        rows)
    modes

let work_and_runtime_budgets () =
  let source =
    "I64 Pick(I64 n){switch(n){case 2+3:return 42;default:return 0;}return \
     -1;}Pick(5);"
  in
  List.iter
    (fun mode ->
      let exact = report ~mode ~max_switch_work:3 source in
      let execution = expect_word "exact switch work" 42L exact in
      Alcotest.(check int) "2+3 work" 3 (switch_work exact);
      let exhausted = report ~mode ~max_switch_work:2 source in
      Alcotest.(check string)
        "work one-below code" "HCSW0003" (first_error exhausted).code;
      Alcotest.(check int) "work one-below progress" 2 (switch_work exhausted);
      let steps = VM.executed_steps execution in
      let exact_runtime =
        report ~mode ~max_switch_work:3 ~max_steps:steps source
      in
      ignore (expect_word "exact runtime" 42L exact_runtime);
      let one_below =
        report ~mode ~max_switch_work:3 ~max_steps:(steps - 1) source
      in
      Alcotest.(check string)
        "runtime one-below" "HCIRVM0007" (first_error one_below).code;
      Alcotest.(check int)
        "runtime fault keeps switch work" 3 (switch_work one_below);
      let recovered = report ~mode ~max_switch_work:3 source in
      ignore (expect_word "fresh run after runtime fault" 42L recovered))
    modes

let source_time_preparation_and_fresh_runs () =
  let unused =
    "I64 Unused(I64 n){switch(n){case 2+3:return 7;default:return 0;}return \
     -1;}42;"
  in
  List.iter
    (fun mode ->
      let first = report ~mode unused in
      let second = report ~mode unused in
      ignore (expect_word "unused first" 42L first);
      ignore (expect_word "unused fresh" 42L second);
      Alcotest.(check int) "unused switch prepared" 3 (switch_work first);
      Alcotest.(check int) "fresh preparation" 3 (switch_work second);
      let fault =
        report ~mode
          "I64 Unused(I64 n){switch(n){case 1/0:return 7;default:return \
           0;}return -1;}42;"
      in
      Alcotest.(check string)
        "unused case fault" "HCSEMA0004" (first_error fault).code;
      Alcotest.(check int) "faulting case reached work" 3 (switch_work fault))
    modes

let invalid_limits_precede_parsing () =
  List.iter
    (fun mode ->
      List.iter
        (fun limit ->
          let invalid = report ~mode ~max_switch_work:limit "@invalid" in
          Alcotest.(check string)
            "invalid switch limit" "HCIRVM0001" (first_error invalid).code;
          Alcotest.(check int) "invalid limit work" 0 (switch_work invalid))
        [ 0; -1 ])
    modes

let invalid_and_unsupported_source () =
  let cases =
    [
      ( "empty",
        "I64 Bad(I64 n){switch(n){default:return 42;}return 0;}42;",
        "HCSW0001" );
      ( "too wide",
        "I64 Bad(I64 n){switch(n){case 0...65535:return 42;default:return \
         0;}return -1;}42;",
        "HCSW0001" );
      ( "sentinel implicit wide",
        "I64 Bad(I64 n){switch(n){case 0x8000000000000000:return 1;case:return \
         2;default:return 3;}return 0;}42;",
        "HCSW0001" );
      ( "duplicate case",
        "I64 Bad(I64 n){switch(n){case 1:return 1;case 1:return \
         2;default:return 3;}return 0;}42;",
        "HCSW0002" );
      ( "overlap",
        "I64 Bad(I64 n){switch(n){case 1...3:return 1;case 3...5:return \
         2;default:return 3;}return 0;}42;",
        "HCSW0002" );
      ( "no-bound",
        "I64 Bad(I64 n){switch[n]{case 1:return 1;default:return 2;}return \
         0;}42;",
        "HCRUN0001" );
      ( "subswitch",
        "I64 Bad(I64 n){switch(n){start:case 1:return 1;end:default:return \
         2;}return 0;}42;",
        "HCRUN0001" );
      ( "effectful case",
        "I64 Bad(I64 n){switch(n){case n++:return 1;default:return 2;}return \
         0;}42;",
        "HCRUN0001" );
      ( "duplicate default",
        "I64 Bad(I64 n){switch(n){case 1:return 1;default:return \
         2;default:return 3;}return 0;}42;",
        "HCRUN0001" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, code) ->
          Alcotest.(check string)
            label code (first_error (report ~mode source)).code)
        cases)
    modes

let dispatch_storage_cap () =
  let too_many =
    "I64 A(I64 n){switch(n){case 0...39999:return 1;default:return 0;}return \
     -1;} I64 B(I64 n){switch(n){case 0...39999:return 2;default:return \
     0;}return -1;} 42;"
  in
  let maximum =
    "I64 A(I64 n){switch(n){case 0...65534:return 1;default:return 0;}return \
     -1;}42;"
  in
  List.iter
    (fun mode ->
      let failed = report ~mode too_many in
      Alcotest.(check string)
        "shared table cap" "HCSW0004" (first_error failed).code;
      Alcotest.(check int) "table cap endpoint work" 4 (switch_work failed);
      let positive = report ~mode maximum in
      ignore (expect_word "one maximum table fits" 42L positive);
      Alcotest.(check int) "maximum endpoint work" 2 (switch_work positive))
    modes

let tests =
  [
    Alcotest.test_case "bounded switch source semantics" `Quick semantic_cases;
    Alcotest.test_case "all scalar selector widths" `Quick
      all_scalar_selector_widths;
    Alcotest.test_case "switch and runtime budgets" `Quick
      work_and_runtime_budgets;
    Alcotest.test_case "source-time preparation and fresh runs" `Quick
      source_time_preparation_and_fresh_runs;
    Alcotest.test_case "invalid limits precede parsing" `Quick
      invalid_limits_precede_parsing;
    Alcotest.test_case "invalid and unsupported source" `Quick
      invalid_and_unsupported_source;
    Alcotest.test_case "dispatch storage cap" `Quick dispatch_storage_cap;
  ]
