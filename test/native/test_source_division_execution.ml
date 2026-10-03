open Holyc_lib
module C = Division_strength_reduction_cases
module Program = X86_64_program
module Runtime = Native_program_execution

module N = struct
  let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

  let errors ds =
    ds
    |> List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
    |> String.concat "; "

  let inputs mode contents =
    let session = Session.create () in
    let source =
      Session.add_source session ~path:"native-source-division.hc" ~contents
    in
    let config =
      Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
    in
    (session, config, source)

  let report ?max_initializer_steps ?(max_steps = 100_000) mode contents =
    let session, config, source = inputs mode contents in
    Native_program.evaluate ?max_initializer_steps session ~config ~source
      ~max_steps

  let success report =
    match Native_program.outcome report with
    | Ok checked -> checked.value
    | Error ds -> Alcotest.fail (errors ds)

  let bits expected = function
    | Some (w : Program.word) ->
        Alcotest.(check int64) "source bits" expected w.bits
    | None -> Alcotest.fail "source has no native word"
end

let values () =
  List.iter
    (fun mode ->
      List.iter
        (fun (_, contents, expected) ->
          let report = N.report mode contents in
          let execution = (N.success report).execution in
          N.bits expected execution.final_value;
          N.bits expected
            (N.success
               (N.report ~max_steps:execution.executed_steps mode contents))
              .execution
              .final_value;
          (match
             Native_program.native_outcome
               (N.report
                  ~max_steps:(execution.executed_steps - 1)
                  mode contents)
           with
          | Some (Program.Fault fault) ->
              Alcotest.(check bool)
                "one-below native runtime" true
                (fault.kind = Program.Step_limit_exceeded);
              Alcotest.(check int)
                "charged reached work"
                (execution.executed_steps - 1)
                fault.executed_steps
          | _ -> Alcotest.fail "one-below native runtime succeeded");
          let session, config, source = N.inputs mode contents in
          List.iter
            (fun status_abi ->
              let image =
                match
                  Native_program.compile ~status_abi session ~config ~source
                with
                | Ok checked -> checked.value
                | Error ds -> Alcotest.fail (N.errors ds)
              in
              Alcotest.(check bool)
                "compiled status ABI" true
                (Program.status_abi image = status_abi);
              let host =
                match Runtime.platform () with
                | Runtime.Windows_x86_64 -> Program.Windows_x64
                | _ -> Program.System_v_x64
              in
              if status_abi = host then
                for _ = 1 to 2 do
                  match
                    Runtime.execute ~max_steps:execution.executed_steps image
                  with
                  | Ok (Program.Completed e) ->
                      N.bits expected e.final_value;
                      Alcotest.(check int)
                        "fresh image actual work" execution.executed_steps
                        e.executed_steps
                  | _ -> Alcotest.fail "fresh native division image failed"
                done)
            [ Program.Windows_x64; Program.System_v_x64 ])
        (C.cases () @ C.contextual_cases);
      List.iter
        (fun (source, bytes) ->
          let report = N.report mode source in
          N.bits 42L (N.success report).execution.final_value;
          Alcotest.(check string)
            "right-to-left native argument effects" bytes
            (Native_program.output_bytes report))
        C.output_cases)
    N.modes

let faults () =
  List.iter
    (fun mode ->
      List.iter
        (fun (_, source) ->
          let report = N.report mode source in
          Alcotest.(check bool)
            "compilation overflow has no image" true
            (Option.is_none (Native_program.image report));
          Alcotest.(check bool)
            "compilation overflow did not execute" true
            (Option.is_none (Native_program.native_outcome report));
          match Native_program.outcome report with
          | Error (d :: _) ->
              Alcotest.(check string)
                "constant overflow compilation" "HCIRL0007" d.code
          | _ -> Alcotest.fail "constant overflow compiled")
        (C.compilation_faults ());
      List.iter
        (fun (label, source, code, bytes) ->
          let report = N.report mode source in
          (match Native_program.outcome report with
          | Error (d :: _) -> Alcotest.(check string) label code d.code
          | _ -> Alcotest.fail (label ^ " succeeded"));
          (match Native_program.native_outcome report with
          | Some (Program.Fault fault) ->
              Alcotest.(check bool)
                "arithmetic fault retained reached span" true
                (Option.is_some fault.span);
              Alcotest.(check bool)
                "arithmetic fault consumed work" true (fault.executed_steps > 0)
          | _ -> Alcotest.fail "arithmetic failure did not reach execution");
          Alcotest.(check string)
            "native output before fault" bytes
            (Native_program.output_bytes report))
        C.reached_faults)
    N.modes

let preparation () =
  List.iter
    (fun mode ->
      let source =
        "I64 A=-7/2;I64 F(I64 n=-7%2){return n;}I64 B=9/3;A+B+F();"
      in
      let control = N.report mode source in
      N.bits (-1L) (N.success control).execution.final_value;
      let prep = Native_program.preparation_steps control in
      Alcotest.(check int) "three folded preparation harnesses" 9 prep;
      N.bits (-1L)
        (N.success (N.report ~max_initializer_steps:prep mode source)).execution
          .final_value;
      let below = N.report ~max_initializer_steps:(prep - 1) mode source in
      (match Native_program.outcome below with
      | Error (d :: _) ->
          Alcotest.(check string) "one-below preparation" "HCIRVM0007" d.code
      | _ -> Alcotest.fail "one-below native preparation succeeded");
      Alcotest.(check int)
        "failed preparation remains charged" (prep - 1)
        (Native_program.preparation_steps below);
      Alcotest.(check bool)
        "failed preparation has no image" true
        (Option.is_none (Native_program.image below));
      List.iter
        (fun source ->
          match Native_program.outcome (N.report mode source) with
          | Error (d :: _) ->
              Alcotest.(check string)
                "nonconstant preparation gate" "HCRUN0006" d.code
          | _ -> Alcotest.fail "native preparation gate widened")
        C.preparation_boundary)
    N.modes

let () =
  Alcotest.run "source division native execution"
    [
      ( "source",
        [
          Alcotest.test_case
            "captured fields, effects, both ABIs and fresh images" `Quick values;
          Alcotest.test_case "compilation and reached arithmetic faults" `Quick
            faults;
          Alcotest.test_case "exact folded preparation and remaining gates"
            `Quick preparation;
        ] );
    ]
