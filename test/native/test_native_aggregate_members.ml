open Holyc_lib
module Cases = Aggregate_member_cases
module Arrays = Aggregate_array_cases
module Pointers = Aggregate_pointer_cases
module Inherited = Inherited_aggregate_cases
module Backed = Backed_aggregate_cases
module P = X86_64_program
module VM = Ir_integer_interpreter

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let inputs mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-aggregate-members.hc" ~contents
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
        (Cases.faults @ Arrays.faults @ Pointers.faults @ Inherited.faults
       @ Backed.faults);
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
        Cases.extents;
      List.iter
        (fun ((_, _, bytes) as extent) ->
          ignore
            (value 42L "" (run mode (Arrays.extent_source extent (bytes - 1))));
          match
            Native_program.outcome
              (run mode (Arrays.extent_source extent bytes))
          with
          | Ok _ ->
              Alcotest.fail "root array admitted bytes past its total extent"
          | Error errors ->
              Alcotest.(check bool)
                (describe errors) true
                (List.exists
                   (fun (d : Diagnostic.t) -> d.code = "HCIRVM0019")
                   errors))
        Arrays.extents)
    modes

let quotas () =
  let check source_contents =
    List.iter
      (fun mode ->
        let baseline = value 42L "" (run mode source_contents) in
        let steps = baseline.execution.executed_steps in
        let fixture =
          Native_scalar_fixture.compile ~mode ~path:"aggregate-quota.hc"
            ~contents:source_contents ()
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
             (run ~max_frame_bytes:frame ~max_steps:steps mode source_contents));
        let fault kind report =
          match Native_program.native_outcome report with
          | Some (P.Fault f) ->
              Alcotest.(check bool)
                "one below reaches native guard" true (f.kind = kind)
          | _ -> Alcotest.fail "one below did not fault"
        in
        fault P.Frame_limit_exceeded
          (run ~max_frame_bytes:(frame - 1) mode source_contents);
        fault P.Step_limit_exceeded
          (run ~max_steps:(steps - 1) mode source_contents))
      modes
  in
  List.iter check
    [
      Cases.quota_source;
      Arrays.quota_source;
      Pointers.quota_source;
      Inherited.quota_source;
      Backed.quota_source;
    ]

let images () =
  let check source_contents =
    List.iter
      (fun mode ->
        List.iter
          (fun status_abi ->
            let compile ?max_stack_bytes ?max_code_bytes () =
              let session, config, source = inputs mode source_contents in
              Native_program.compile ?max_stack_bytes ?max_code_bytes
                ~status_abi session ~config ~source
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
  in
  List.iter check
    [
      Cases.quota_source;
      Arrays.quota_source;
      Pointers.quota_source;
      Inherited.quota_source;
      Backed.quota_source;
    ]

let field_proofs () =
  let check contents =
    List.iter
      (fun mode ->
        let original, own, invalid =
          Aggregate_member_fixture.controls ~contents mode
        in
        let compile definition =
          P.compile_callable ~max_ir_instructions:4096 ~max_code_bytes:65_536
            ~runtime_calls:(integer_program_runtime_calls original)
            ~initialization:(integer_program_initialization original)
            ~entry:(integer_program_entry original)
            ~functions:
              (definition :: List.tl (integer_program_functions original))
            ()
        in
        (match compile (Aggregate_member_fixture.first_definition original) with
        | Ok image -> (
            match Native_program_execution.execute ~max_steps:100_000 image with
            | Ok (P.Completed value) ->
                Alcotest.(check (option int64))
                  "original sealed field proof executes" (Some 42L)
                  (Option.map (fun w -> w.P.bits) value.final_value)
            | _ -> Alcotest.fail "original sealed field proof did not execute")
        | Error errors ->
            Alcotest.fail
              (String.concat "; "
                 (List.map
                    (fun (e : P.error) -> e.code ^ ": " ^ e.message)
                    errors)));
        List.iter
          (fun (name, definition) ->
            Alcotest.(check bool)
              (name ^ " rejects before image creation")
              true
              (Result.is_error (compile definition)))
          (("rebuilt graph retains no sealed source authority", own) :: invalid))
      modes
  in
  List.iter check
    [
      Aggregate_member_fixture.contents;
      Arrays.proof_source;
      Pointers.proof_source;
      Inherited.proof_source;
      Backed.proof_source;
    ]

let boundaries () =
  List.iter
    (fun mode ->
      match Native_program.outcome (run mode Inherited.lookahead_source) with
      | Ok _ -> Alcotest.fail "isolated native #exe remains unsupported"
      | Error errors ->
          Alcotest.(check bool)
            (describe errors) true
            (List.exists (fun (d : Diagnostic.t) -> d.code = "HCPP0008") errors))
    modes;
  List.iter
    (fun mode ->
      List.iter
        (fun (name, contents) ->
          Alcotest.(check bool)
            name true
            (Result.is_error (Native_program.outcome (run mode contents))))
        (Cases.unsupported @ Arrays.unsupported @ Pointers.unsupported
       @ Inherited.unsupported @ Backed.unsupported))
    modes;
  let session, config, source = inputs Preprocessor.Jit Cases.quota_source in
  let report =
    Native_source_execution.evaluate session ~config ~source ~max_steps:100_000
  in
  Alcotest.(check bool)
    "retained JIT class metadata cannot authorize frame storage" true
    (Result.is_error (Native_source_execution.outcome report))

let () =
  Alcotest.run "native aggregate members"
    [
      ( "values",
        List.map
          (fun (name, contents, expected, output) ->
            Alcotest.test_case name `Quick (case contents expected output))
          (Cases.values @ Cases.view_matrix @ Arrays.values @ Arrays.view_matrix
         @ Pointers.values @ Pointers.view_matrix @ Inherited.values
         @ Inherited.view_matrix @ Backed.values) );
      ( "storage",
        [
          Alcotest.test_case "unknown bytes, bounds and independent activations"
            `Quick faults;
          Alcotest.test_case "exact runtime frame and instruction limits" `Quick
            quotas;
          Alcotest.test_case "both ABIs, exact image limits and fresh execution"
            `Quick images;
          Alcotest.test_case "sealed graphs retain their exact field proofs"
            `Quick field_proofs;
          Alcotest.test_case "metadata and numeric bits do not grant storage"
            `Quick boundaries;
        ] );
    ]
