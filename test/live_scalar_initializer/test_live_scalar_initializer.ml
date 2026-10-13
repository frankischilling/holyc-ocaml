open Holyc_lib
module VM = Ir_integer_interpreter

let run ?max_steps ?max_initializer_steps mode text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"live-scalar-initializer.hc"
      ~contents:(Cases.headers ^ text)
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
  in
  run_integer_program_report ?max_initializer_steps
    ~max_steps:(Option.value ~default:100_000 max_steps)
    session ~source ~config

let describe errors =
  List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message) errors
  |> String.concat "; "

let value ?(expected = 42L) output report =
  let result =
    match integer_program_report_outcome report with
    | Ok result -> result
    | Error errors -> Alcotest.fail (describe errors)
  in
  Alcotest.(check (option int64))
    "reached initializer bytes" (Some expected)
    (VM.final_value result.value |> Option.map (fun word -> word.VM.bits));
  Alcotest.(check string)
    "once-only expression output" output
    (integer_program_report_output_bytes report)

let failure code report =
  match integer_program_report_outcome report with
  | Ok _ -> Alcotest.fail ("expected " ^ code)
  | Error errors ->
      Alcotest.(check bool)
        (describe errors) true
        (List.exists (fun (d : Diagnostic.t) -> d.code = code) errors);
      List.iter
        (fun (d : Diagnostic.t) ->
          Alcotest.(check bool)
            "diagnostic has no duplicated code" false
            (String.starts_with ~prefix:(d.code ^ ":") d.message))
        errors

let saved_tables () =
  value "A" (run Preprocessor.Jit Cases.saved_tables);
  value ~expected:7L "A" (run Preprocessor.Aot Cases.saved_tables);
  value "A" (run Preprocessor.Jit Cases.static_saved_tables);
  value ~expected:7L "A" (run Preprocessor.Aot Cases.static_saved_tables)

let faults () =
  List.iter
    (fun (_, source, code) ->
      let report = run Preprocessor.Jit source in
      failure code report;
      Alcotest.(check string)
        "output precedes reached failure" "A"
        (integer_program_report_output_bytes report))
    (Cases.faults @ Cases.static_faults)

let quotas () =
  List.iter
    (fun source ->
      let baseline = run Preprocessor.Jit source in
      value "A" baseline;
      let work =
        integer_program_report_preparation_work baseline |> Option.get
      in
      let steps =
        integer_program_report_outcome baseline |> Result.get_ok |> fun r ->
        VM.executed_steps r.value
      in
      value "A"
        (run ~max_steps:steps ~max_initializer_steps:work Preprocessor.Jit
           source);
      let failed =
        run ~max_initializer_steps:(work - 1) Preprocessor.Jit source
      in
      failure "HCIRVM0007" failed;
      Alcotest.(check string)
        "evaluation output survives copy quota" "A"
        (integer_program_report_output_bytes failed);
      failure "HCIRVM0007" (run ~max_steps:(steps - 1) Preprocessor.Jit source))
    [
      Cases.current_leaf;
      Cases.saved_tables;
      Cases.nested_metadata;
      Cases.static_current_leaf;
      Cases.static_saved_tables;
      Cases.static_nested_metadata;
    ]

let grammar_faults () =
  List.iter
    (fun (_, source, output) ->
      let report = run Preprocessor.Jit source in
      failure "HCRUN0006" report;
      Alcotest.(check string)
        "grammar is selected before later lexer effects" output
        (integer_program_report_output_bytes report))
    Cases.grammar_faults

let partial_member_faults () =
  List.iter
    (fun mode ->
      List.iteri
        (fun index (_, source) ->
          let report = run mode source in
          failure (if index < 2 then "HCSEMA0046" else "HCIRVM0019") report;
          Alcotest.(check string)
            "earlier output remains" "A"
            (integer_program_report_output_bytes report))
        Cases.partial_member_faults)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let () =
  Alcotest.run "Live scalar initializer copies"
    [
      ( "IR",
        List.map
          (fun (name, source, output) ->
            Alcotest.test_case name `Quick (fun () ->
                value output (run Preprocessor.Jit source)))
          (Cases.jit_values @ Cases.static_jit_values)
        @ List.concat_map
            (fun mode ->
              List.map
                (fun (name, source, output) ->
                  Alcotest.test_case
                    ((if mode = Preprocessor.Jit then "JIT " else "AOT ") ^ name)
                    `Quick
                    (fun () -> value output (run mode source)))
                (Cases.both_values @ Cases.static_both_values))
            [ Preprocessor.Jit; Preprocessor.Aot ]
        @ [
            Alcotest.test_case "saved compiler table selects the reached class"
              `Quick saved_tables;
            Alcotest.test_case "reached errors keep earlier output" `Quick
              faults;
            Alcotest.test_case "actual copy bytes share exact quotas" `Quick
              quotas;
            Alcotest.test_case "static delimiter grammar and prior effects"
              `Quick grammar_faults;
            Alcotest.test_case "partial member visibility and original bounds"
              `Quick partial_member_faults;
          ] );
    ]
