open Holyc_lib
module Cases = Internal_binding_cases
module VM = Ir_integer_interpreter

let run ?max_initializer_steps text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"live-internal-bindings.hc" ~contents:text
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:Jit () |> Result.get_ok
  in
  run_integer_program_report ?max_initializer_steps session ~source ~config
    ~max_steps:100_000

let diagnostics errors =
  errors
  |> List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
  |> String.concat "; "

let values () =
  List.iter
    (fun (text, expected) ->
      match run text |> integer_program_report_outcome with
      | Ok result ->
          Alcotest.(check (option int64))
            text (Some expected)
            (VM.final_value result.value
            |> Option.map (fun word -> word.VM.bits))
      | Error errors -> Alcotest.fail (diagnostics errors))
    Cases.values

let error code report =
  match integer_program_report_outcome report with
  | Error errors ->
      Alcotest.(check bool)
        (diagnostics errors) true
        (List.exists (fun (d : Diagnostic.t) -> d.code = code) errors)
  | Ok _ -> Alcotest.fail ("expected " ^ code)

let faults () =
  List.iter (fun (text, code) -> error code (run text)) Cases.faults

let work_and_effects () =
  let text =
    "I64 N=0;I64 Target(){N++;return 0x1e;}_intern Target() I64 Convert(U8 \
     c);Convert(97)+N;"
  in
  let report = run text in
  let limit = integer_program_report_preparation_work report |> Option.get in
  (match
     run ~max_initializer_steps:limit text |> integer_program_report_outcome
   with
  | Ok result ->
      Alcotest.(check int64)
        "exact binding work" 66L
        (VM.final_value result.value |> Option.get).bits
  | Error errors -> Alcotest.fail (diagnostics errors));
  error "HCIRVM0007" (run ~max_initializer_steps:(limit - 1) text);
  let parsed, task =
    Test_live_initializer_execution.run_result
      "I64 N=0;I64 Target(){N++;return 0x1e;}_intern Target() Missing \
       Convert(U8 c);"
  in
  Alcotest.(check bool)
    "later invalid type remains rejected" true
    (Option.is_none parsed.ast);
  Alcotest.(check int64)
    "target effects precede type validation" 1L
    (Test_live_initializer_execution.read task "N;")

let tests =
  [
    Alcotest.test_case "original target values, calls and history" `Quick values;
    Alcotest.test_case "reached faults and unsupported targets" `Quick faults;
    Alcotest.test_case "original work and pre-header effects" `Quick
      work_and_effects;
  ]
