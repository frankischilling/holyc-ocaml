open Holyc_lib
module Cases = Runtime_layout_cases
module VM = Ir_integer_interpreter

let describe errors =
  errors
  |> List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
  |> String.concat "; "

let run ?max_initializer_steps ?max_steps text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"runtime-layout.hc" ~contents:text
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:Jit () |> Result.get_ok
  in
  run_integer_program_report ?max_initializer_steps session ~source ~config
    ~max_steps:(Option.value ~default:100_000 max_steps)

let value expected report =
  match integer_program_report_outcome report with
  | Error errors -> Alcotest.fail (describe errors)
  | Ok result ->
      Alcotest.(check (option int64))
        "original layout result" (Some expected)
        (VM.final_value result.value |> Option.map (fun word -> word.VM.bits))

let values () =
  List.iter (fun (text, expected) -> value expected (run text)) Cases.values

let effects () =
  let report = run Cases.output in
  value 42L report;
  Alcotest.(check string)
    "once-only dimension and offset output" "dimoff"
    (integer_program_report_output_bytes report);
  List.iter
    (fun (text, code) ->
      match run text |> integer_program_report_outcome with
      | Error errors ->
          Alcotest.(check bool)
            (describe errors) true
            (List.exists (fun (d : Diagnostic.t) -> d.code = code) errors)
      | Ok _ -> Alcotest.fail ("expected " ^ code))
    Cases.unsupported;
  List.iter
    (fun text ->
      let report = run text in
      Alcotest.(check bool)
        text true
        (Result.is_error (integer_program_report_outcome report));
      Alcotest.(check string)
        "output precedes later failure" "kept"
        (integer_program_report_output_bytes report))
    Cases.reached_failures

let quotas () =
  let baseline = run Cases.output in
  let prep = integer_program_report_preparation_work baseline |> Option.get in
  let steps =
    integer_program_report_outcome baseline |> Result.get_ok |> fun result ->
    VM.executed_steps result.value
  in
  value 42L (run ~max_initializer_steps:prep ~max_steps:steps Cases.output);
  List.iter
    (fun report ->
      match integer_program_report_outcome report with
      | Error errors ->
          Alcotest.(check bool)
            (describe errors) true
            (List.exists
               (fun (d : Diagnostic.t) -> d.code = "HCIRVM0007")
               errors)
      | Ok _ -> Alcotest.fail "one-below limit unexpectedly passed")
    [
      run ~max_initializer_steps:(prep - 1) Cases.output;
      run ~max_steps:(steps - 1) Cases.output;
    ]

let tests =
  [
    Alcotest.test_case "original bounds, layout, calls and history" `Quick
      values;
    Alcotest.test_case "once-only output and reached failure effects" `Quick
      effects;
    Alcotest.test_case "shared exact and exhausted budgets" `Quick quotas;
  ]
