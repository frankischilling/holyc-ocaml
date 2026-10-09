open Holyc_lib
module Cases = Automatic_aggregate_cases
module P = X86_64_program
module VM = Ir_integer_interpreter

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let inputs mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-automatic-aggregate.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
  in
  (session, config, source)

let describe errors =
  errors
  |> List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
  |> String.concat "; "

let checked = function
  | Ok value -> value
  | Error errors -> Alcotest.fail (describe errors)

let run ?max_frame_bytes ?(max_steps = 100_000) mode contents =
  let session, config, source = inputs mode contents in
  Native_program.evaluate ?max_frame_bytes session ~config ~source ~max_steps

let value expected output report =
  let native = (Native_program.outcome report |> checked).value in
  Alcotest.(check (option int64))
    "independent native word" (Some expected)
    (Option.map (fun w -> w.P.bits) native.execution.final_value);
  Alcotest.(check string)
    "native output" output
    (Native_program.output_bytes report);
  native

let case contents expected output () =
  List.iter
    (fun mode ->
      let result = value expected output (run mode contents) in
      let _, interpreted =
        Native_scalar_fixture.execute_source ~mode ~contents () |> function
        | Ok value -> value
        | Error e -> Alcotest.fail e
      in
      Alcotest.(check (option int64))
        "same closed IR, independent expected word" (Some expected)
        (Option.map (fun w -> w.VM.bits) (VM.final_value interpreted));
      Alcotest.(check int)
        "same closed IR instruction work"
        (VM.executed_steps interpreted)
        result.execution.executed_steps)
    modes

let faults () =
  List.iter
    (fun mode ->
      List.iter
        (fun (_, contents, code, output) ->
          let report = run mode contents in
          (match Native_program.outcome report with
          | Ok _ -> Alcotest.fail ("expected " ^ code)
          | Error errors ->
              Alcotest.(check bool)
                (describe errors) true
                (List.exists (fun (d : Diagnostic.t) -> d.code = code) errors));
          Alcotest.(check string)
            "native fault preserves reached output" output
            (Native_program.output_bytes report))
        Cases.faults;
      List.iter
        (fun (definition, bytes) ->
          ignore
            (value 42L ""
               (run mode (Cases.extent_source definition (bytes - 1))));
          let report = run mode (Cases.extent_source definition bytes) in
          match Native_program.outcome report with
          | Ok _ ->
              Alcotest.fail
                "aggregate layout admitted bytes beyond its exact size"
          | Error errors ->
              Alcotest.(check bool)
                (describe errors) true
                (List.exists
                   (fun (d : Diagnostic.t) -> d.code = "HCIRVM0019")
                   errors))
        Cases.extents)
    modes

let quotas () =
  List.iter
    (fun mode ->
      let baseline = value 42L "" (run mode Cases.quota_source) in
      let steps = baseline.execution.executed_steps in
      let fixture =
        Native_scalar_fixture.compile ~mode ~path:"aggregate-quota.hc"
          ~contents:Cases.quota_source ()
        |> checked
      in
      let frame =
        Holyc_lib__Driver.Integer_unit.functions fixture.unit_
        |> List.map (fun (f : VM.function_definition) ->
            Semantic_function_frame_layout.function_frame_size f.frame
            |> Int64.to_int)
        |> List.fold_left max 0
      in
      ignore
        (value 42L ""
           (run ~max_frame_bytes:frame ~max_steps:steps mode Cases.quota_source));
      let fault kind report =
        match Native_program.native_outcome report with
        | Some (P.Fault f) ->
            Alcotest.(check bool)
              "one below reaches native guard" true (f.kind = kind)
        | _ -> Alcotest.fail "one below did not fault"
      in
      fault P.Frame_limit_exceeded
        (run ~max_frame_bytes:(frame - 1) mode Cases.quota_source);
      fault P.Step_limit_exceeded
        (run ~max_steps:(steps - 1) mode Cases.quota_source))
    modes

let images () =
  List.iter
    (fun mode ->
      List.iter
        (fun status_abi ->
          let compile ?max_stack_bytes ?max_code_bytes () =
            let session, config, source = inputs mode Cases.quota_source in
            Native_program.compile ?max_stack_bytes ?max_code_bytes ~status_abi
              session ~config ~source
          in
          let image = (compile () |> checked).value in
          let frame = P.frame_bytes image and code = P.code_bytes image in
          ignore
            (compile ~max_stack_bytes:frame ~max_code_bytes:code () |> checked);
          Alcotest.(check bool)
            "byte initialization metadata stack one below" true
            (Result.is_error (compile ~max_stack_bytes:(frame - 1) ()));
          Alcotest.(check bool)
            "encoded image bytes one below" true
            (Result.is_error (compile ~max_code_bytes:(code - 1) ()));
          let host =
            match Native_program_execution.platform () with
            | Native_program_execution.Windows_x86_64 -> P.Windows_x64
            | _ -> P.System_v_x64
          in
          if status_abi = host then
            for _ = 1 to 2 do
              match
                Native_program_execution.execute ~max_steps:100_000 image
              with
              | Ok (P.Completed result) ->
                  Alcotest.(check int64)
                    "fresh object image" 42L
                    (Option.get result.final_value).bits
              | _ -> Alcotest.fail "native object image failed"
            done)
        [ P.Windows_x64; P.System_v_x64 ])
    modes

let boundaries () =
  List.iter
    (fun mode ->
      List.iter
        (fun (name, contents) ->
          Alcotest.(check bool)
            name true
            (Result.is_error (Native_program.outcome (run mode contents))))
        Cases.unsupported)
    modes;
  let session, config, source = inputs Preprocessor.Jit Cases.quota_source in
  let report =
    Native_source_execution.evaluate session ~config ~source ~max_steps:100_000
  in
  Alcotest.(check bool)
    "retained JIT class metadata cannot authorize frame storage" true
    (Result.is_error (Native_source_execution.outcome report))

let () =
  Alcotest.run "native automatic aggregate storage"
    [
      ( "values",
        List.map
          (fun (name, contents, expected, output) ->
            Alcotest.test_case name `Quick (case contents expected output))
          (Cases.values @ Cases.view_matrix) );
      ( "storage",
        [
          Alcotest.test_case "unknown bytes, bounds and independent activations"
            `Quick faults;
          Alcotest.test_case "exact runtime frame and instruction limits" `Quick
            quotas;
          Alcotest.test_case "both ABIs, exact image limits and fresh execution"
            `Quick images;
          Alcotest.test_case "metadata and numeric bits do not grant storage"
            `Quick boundaries;
        ] );
    ]
