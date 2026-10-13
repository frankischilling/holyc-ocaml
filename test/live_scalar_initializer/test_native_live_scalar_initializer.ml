open Holyc_lib
module Native = Native_source_execution

let run ?max_steps ?max_initializer_steps ?(mode = Preprocessor.Jit) text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-live-scalar-initializer.hc"
      ~contents:(Cases.headers ^ text)
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
  in
  Native.evaluate ?max_initializer_steps
    ~max_steps:(Option.value ~default:100_000 max_steps)
    ~max_code_bytes:524_288 session ~source ~config

let describe errors =
  List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message) errors
  |> String.concat "; "

let value ?(expected = 42L) output report =
  let result =
    match Native.outcome report with
    | Ok result -> result
    | Error errors -> Alcotest.fail (describe errors)
  in
  Alcotest.(check (option int64))
    "reached initializer bytes" (Some expected)
    (Option.map (fun word -> word.Native.bits) result.value.final_value);
  Alcotest.(check string)
    "once-only expression output" output
    (Native.output_bytes report);
  Alcotest.(check int)
    "zero interpreted instructions" 0
    (Option.get (Native.source_progress report)).runtime.executed_steps

let failure code report =
  match Native.outcome report with
  | Ok _ -> Alcotest.fail ("expected " ^ code)
  | Error errors ->
      Alcotest.(check bool)
        (describe errors) true
        (List.exists (fun (d : Diagnostic.t) -> d.code = code) errors)

let saved_tables () =
  value "A" (run Cases.saved_tables);
  value ~expected:7L "A" (run ~mode:Preprocessor.Aot Cases.saved_tables);
  value "A" (run Cases.static_saved_tables);
  value ~expected:7L "A" (run ~mode:Preprocessor.Aot Cases.static_saved_tables)

let faults () =
  List.iter
    (fun (_, source, code) ->
      let report = run source in
      failure code report;
      Alcotest.(check string)
        "output precedes reached failure" "A"
        (Native.output_bytes report))
    (Cases.faults @ Cases.static_faults)

let quotas () =
  List.iter
    (fun source ->
      let baseline = run source in
      value "A" baseline;
      let work = Native.preparation_steps baseline
      and steps = Native.executed_steps baseline in
      value "A" (run ~max_steps:steps ~max_initializer_steps:work source);
      let failed = run ~max_initializer_steps:(work - 1) source in
      failure "HCIRVM0007" failed;
      Alcotest.(check string)
        "evaluation output survives copy quota" "A"
        (Native.output_bytes failed);
      failure "HCIRVM0007" (run ~max_steps:(steps - 1) source))
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
      let report = run source in
      failure "HCRUN0006" report;
      Alcotest.(check string)
        "grammar is selected before later lexer effects" output
        (Native.output_bytes report))
    Cases.grammar_faults

let () =
  Alcotest.run "Native live scalar initializer copies"
    [
      ( "native",
        List.map
          (fun (name, source, output) ->
            Alcotest.test_case name `Quick (fun () -> value output (run source)))
          (Cases.jit_values @ Cases.static_jit_values)
        @ List.concat_map
            (fun mode ->
              List.map
                (fun (name, source, output) ->
                  Alcotest.test_case
                    ((if mode = Preprocessor.Jit then "JIT " else "AOT ") ^ name)
                    `Quick
                    (fun () -> value output (run ~mode source)))
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
            Alcotest.test_case "original native capture and copy authority"
              `Quick (fun () -> Authority.source_copy ());
            Alcotest.test_case "missing copy cannot complete the initializer"
              `Quick (fun () -> Authority.source_copy ~finish:false ());
            Alcotest.test_case
              "copy receipts survive collection and reject replay" `Quick
              Authority.copy_receipts;
            Alcotest.test_case
              "static captures and copies retain original authority" `Quick
              (fun () -> Authority.static_source_copy ());
            Alcotest.test_case "missing static copy cannot publish the function"
              `Quick (fun () -> Authority.static_source_copy ~finish:false ());
          ] );
    ]
