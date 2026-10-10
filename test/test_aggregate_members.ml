open Holyc_lib
module Cases = Aggregate_member_cases
module Arrays = Aggregate_array_cases
module Pointers = Aggregate_pointer_cases
module Inherited = Inherited_aggregate_cases
module VM = Ir_integer_interpreter

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let inputs mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"aggregate-members.hc" ~contents
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
  run_integer_program_report ?max_frame_bytes session ~config ~source ~max_steps

let value expected output report =
  let result = (integer_program_report_outcome report |> checked).value in
  Alcotest.(check (option int64))
    "independent expected word" (Some expected)
    (Option.map (fun w -> w.VM.bits) (VM.final_value result));
  Alcotest.(check string)
    "reached output" output
    (integer_program_report_output_bytes report);
  result

let failure code output report =
  (match integer_program_report_outcome report with
  | Ok _ -> Alcotest.fail ("expected " ^ code)
  | Error errors ->
      Alcotest.(check bool)
        (describe errors) true
        (List.exists (fun (d : Diagnostic.t) -> d.code = code) errors));
  Alcotest.(check string)
    "fault preserves reached output" output
    (integer_program_report_output_bytes report)

let faults () =
  List.iter
    (fun mode ->
      List.iter
        (fun (_, source, code, output) -> failure code output (run mode source))
        (Cases.faults @ Arrays.faults @ Pointers.faults @ Inherited.faults);
      List.iter
        (fun (definition, bytes) ->
          ignore
            (value 42L ""
               (run mode (Cases.extent_source definition (bytes - 1))));
          failure "HCIRVM0019" ""
            (run mode (Cases.extent_source definition bytes)))
        Cases.extents;
      List.iter
        (fun ((_, _, bytes) as extent) ->
          ignore
            (value 42L "" (run mode (Arrays.extent_source extent (bytes - 1))));
          failure "HCIRVM0019" "" (run mode (Arrays.extent_source extent bytes)))
        Arrays.extents)
    modes

let quotas () =
  let check source_contents =
    List.iter
      (fun mode ->
        let session, config, source = inputs mode source_contents in
        let compiled =
          (compile_integer_program session ~config ~source |> checked).value
        in
        let frame_bytes =
          integer_program_functions compiled
          |> List.map (fun (f : VM.function_definition) ->
              Semantic_function_frame_layout.function_frame_size f.frame
              |> Int64.to_int)
          |> List.fold_left max 0
        in
        let baseline = value 42L "" (run mode source_contents) in
        let steps = VM.executed_steps baseline in
        ignore
          (value 42L ""
             (run ~max_frame_bytes:frame_bytes ~max_steps:steps mode
                source_contents));
        failure "HCIRVM0011" ""
          (run ~max_frame_bytes:(frame_bytes - 1) mode source_contents);
        failure "HCIRVM0007" ""
          (run ~max_steps:(steps - 1) mode source_contents))
      modes
  in
  List.iter check
    [
      Cases.quota_source;
      Arrays.quota_source;
      Pointers.quota_source;
      Inherited.quota_source;
    ]

let foreign_frame () =
  let check source_contents =
    List.iter
      (fun mode ->
        let compile () =
          let session, config, source = inputs mode source_contents in
          (compile_integer_program session ~config ~source |> checked).value
        in
        let original = compile () and foreign = compile () in
        let functions =
          List.map2
            (fun (a : VM.function_definition) (b : VM.function_definition) ->
              { a with frame = b.frame })
            (integer_program_functions original)
            (integer_program_functions foreign)
        in
        match
          VM.execute_program
            ~runtime_calls:(integer_program_runtime_calls original)
            ~globals:(integer_program_globals original)
            ~initialization:(integer_program_initialization original)
            ~max_steps:100_000 ~max_frame_bytes:1024 ~max_call_depth:16
            ~functions
            (integer_program_entry original)
        with
        | Ok _ -> Alcotest.fail "equal names and sizes admitted a foreign frame"
        | Error errors ->
            List.iter
              (fun (e : VM.error) ->
                Alcotest.(check int)
                  "ownership failure precedes execution" 0 e.executed_steps)
              errors)
      modes
  in
  List.iter check
    [
      Cases.quota_source;
      Arrays.quota_source;
      Pointers.quota_source;
      Inherited.quota_source;
    ]

let field_proofs () =
  let check contents =
    List.iter
      (fun mode ->
        let _, own, invalid =
          Aggregate_member_fixture.controls ~contents mode
        in
        let execute definition =
          VM.execute_function ~max_steps:100_000 ~max_frame_bytes:1024
            ~frame:definition.VM.frame ~arguments:[] definition.body
        in
        (match execute own with
        | Ok value ->
            Alcotest.(check (option int64))
              "rebuilt own proof executes" (Some 42L)
              (match VM.termination value with
              | VM.Returned word -> Option.map (fun w -> w.VM.bits) word
              | _ -> None)
        | Error errors ->
            Alcotest.fail
              (String.concat "; "
                 (List.map
                    (fun (e : VM.error) -> e.code ^ ": " ^ e.message)
                    errors)));
        List.iter
          (fun (name, definition) ->
            match execute definition with
            | Ok _ -> Alcotest.fail (name ^ " executed")
            | Error errors ->
                List.iter
                  (fun (e : VM.error) ->
                    Alcotest.(check int)
                      (name ^ " precedes storage")
                      0 e.executed_steps)
                  errors)
          invalid)
      modes
  in
  List.iter check
    [
      Aggregate_member_fixture.contents;
      Arrays.proof_source;
      Pointers.proof_source;
      Inherited.proof_source;
    ]

let boundaries () =
  failure "HCSEMA0046" "" (run Preprocessor.Jit Inherited.lookahead_source);
  ignore (value 42L "" (run Preprocessor.Aot Inherited.lookahead_source));
  List.iter
    (fun mode ->
      List.iter
        (fun (name, contents) ->
          Alcotest.(check bool)
            name true
            (Result.is_error
               (integer_program_report_outcome (run mode contents))))
        (Cases.unsupported @ Arrays.unsupported @ Pointers.unsupported
       @ Inherited.unsupported))
    modes

let () =
  Alcotest.run "aggregate members"
    [
      ( "values",
        List.map
          (fun (name, source, expected, output) ->
            Alcotest.test_case name `Quick (fun () ->
                List.iter
                  (fun mode -> ignore (value expected output (run mode source)))
                  modes))
          (Cases.values @ Cases.view_matrix @ Arrays.values @ Arrays.view_matrix
         @ Pointers.values @ Pointers.view_matrix @ Inherited.values
         @ Inherited.view_matrix) );
      ( "storage",
        [
          Alcotest.test_case "unknown bytes, extents and fresh activations"
            `Quick faults;
          Alcotest.test_case "exact frame and instruction limits" `Quick quotas;
          Alcotest.test_case "foreign frame rejected before touching bytes"
            `Quick foreign_frame;
          Alcotest.test_case
            "selected field proof cannot be borrowed or altered" `Quick
            field_proofs;
          Alcotest.test_case "unadmitted layouts, copies and numeric ownership"
            `Quick boundaries;
        ] );
    ]
