open Holyc_lib

let describe errors =
  String.concat "; "
    (List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message) errors)

let values () =
  List.iter
    (fun (name, text, output, _generated) ->
      let session = Session.create () in
      let source =
        Session.add_source session ~path:"aot-source-sessions.hc"
          ~contents:(Aot_source_cases.headers ^ text)
      in
      let config =
        Preprocessor.Config.create ~compilation_mode:Aot () |> Result.get_ok
      in
      let report =
        run_integer_program_report session ~config ~source ~max_steps:100_000
      in
      let result =
        match integer_program_report_outcome report with
        | Ok result -> result
        | Error errors -> Alcotest.fail (name ^ ": " ^ describe errors)
      in
      Alcotest.(check (option int64))
        name (Some 42L)
        (Option.map
           (fun (word : Ir_integer_interpreter.word) -> word.bits)
           (Ir_integer_interpreter.final_value result.value));
      Alcotest.(check string)
        (name ^ " original output")
        output
        (integer_program_report_output_bytes report))
    Aot_source_cases.successes

let () =
  Alcotest.run "AOT source sessions"
    [
      ( "original source",
        [ Alcotest.test_case "separate module and task contexts" `Quick values ]
      );
    ]
