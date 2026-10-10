open Holyc_lib
module Cases = Source_inherited_layout_cases
module VM = Ir_integer_interpreter

let describe errors =
  List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message) errors
  |> String.concat "; "

let run ?max_steps ?max_initializer_steps ?max_output_bytes ?max_output_work
    mode text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"source-inherited-layouts.hc"
      ~contents:(Cases.headers ^ text)
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
  in
  run_integer_program_report ?max_initializer_steps ?max_output_bytes
    ?max_output_work
    ~max_steps:(Option.value ~default:100_000 max_steps)
    session ~source ~config

let value ?(expected = 42L) report =
  match integer_program_report_outcome report with
  | Error errors -> Alcotest.fail (describe errors)
  | Ok result ->
      Alcotest.(check (option int64))
        "original inherited size" (Some expected)
        (VM.final_value result.value |> Option.map (fun word -> word.VM.bits))

let failure code report =
  match integer_program_report_outcome report with
  | Ok _ -> Alcotest.fail ("expected " ^ code)
  | Error errors ->
      Alcotest.(check bool)
        (describe errors) true
        (List.exists (fun (d : Diagnostic.t) -> d.code = code) errors)

let effects () =
  let report = run Preprocessor.Jit Cases.effects in
  value report;
  Alcotest.(check string)
    "inherited preparation occurs once" "dimoff"
    (integer_program_report_output_bytes report);
  List.iter
    (fun (source, code) ->
      let report = run Preprocessor.Jit source in
      failure code report;
      Alcotest.(check string)
        "lookahead observes size before attachment" "0"
        (integer_program_report_output_bytes report))
    [ (Cases.bad_brace, "HCPARSE0110"); (Cases.comma, "HCPARSE0126") ]

let quotas () =
  let baseline = run Preprocessor.Jit Cases.effects in
  value baseline;
  let prep = integer_program_report_preparation_work baseline |> Option.get in
  let steps =
    integer_program_report_outcome baseline |> Result.get_ok |> fun r ->
    VM.executed_steps r.value
  in
  value
    (run ~max_steps:steps ~max_initializer_steps:prep ~max_output_bytes:6
       ~max_output_work:(integer_program_report_output_work baseline)
       Preprocessor.Jit Cases.effects);
  failure "HCIRVM0007"
    (run ~max_steps:(steps - 1) Preprocessor.Jit Cases.effects);
  failure "HCIRVM0007"
    (run ~max_initializer_steps:(prep - 1) Preprocessor.Jit Cases.effects);
  failure "HCIRVM0022" (run ~max_output_bytes:5 Preprocessor.Jit Cases.effects);
  failure "HCIRVM0023"
    (run
       ~max_output_work:(integer_program_report_output_work baseline - 1)
       Preprocessor.Jit Cases.effects)

let boundaries () =
  List.iter
    (fun mode ->
      value (run mode Cases.object_storage);
      failure "HCRUN0004" (run mode Cases.overflow))
    [ Preprocessor.Jit; Preprocessor.Aot ];
  failure "HCRUN0006" (run Preprocessor.Aot Cases.effects);
  let _, forward = List.hd Cases.jit_values in
  value ~expected:8L (run Preprocessor.Aot forward)

let () =
  Alcotest.run "Original source inherited layout metadata"
    [
      ( "IR",
        List.concat_map
          (fun mode ->
            List.map
              (fun (name, text) ->
                Alcotest.test_case
                  ((if mode = Preprocessor.Jit then "JIT " else "AOT ") ^ name)
                  `Quick
                  (fun () -> value (run mode text)))
              Cases.values)
          [ Preprocessor.Jit; Preprocessor.Aot ]
        @ List.map
            (fun (name, text) ->
              Alcotest.test_case name `Quick (fun () ->
                  value (run Preprocessor.Jit text)))
            Cases.jit_values
        @ [
            Alcotest.test_case "original effects and base attachment order"
              `Quick effects;
            Alcotest.test_case "exact shared budgets" `Quick quotas;
            Alcotest.test_case "completed object storage and AOT boundaries"
              `Quick boundaries;
          ] );
    ]
