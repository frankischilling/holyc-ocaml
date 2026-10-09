open Holyc_lib
module C = Source_constant_shift_cases
module A = Test_internal_strlen_authority
module F = Test_constant_shifts
module Program = X86_64_program
module VM = Ir_integer_interpreter
module Runtime = Native_program_execution

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let inputs mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-source-shift.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
  in
  (session, config, source)

let errors ds =
  ds
  |> List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
  |> String.concat "; "

let report ?max_initializer_steps ?(max_steps = 100_000) mode contents =
  let session, config, source = inputs mode contents in
  Native_program.evaluate ?max_initializer_steps session ~config ~source
    ~max_steps

let success report =
  match Native_program.outcome report with
  | Ok checked -> checked.value
  | Error ds -> Alcotest.fail (errors ds)

let bits expected word =
  match word with
  | Some (w : Program.word) ->
      Alcotest.(check int64) "independent source bits" expected w.bits
  | None -> Alcotest.fail "source has no native word"

let values () =
  let fixture = F.fixture () in
  let captured =
    F.projections fixture
    |> List.map (fun p ->
        ( F.field p "field",
          F.field p "holy_c_source",
          F.observed fixture p "case_id" ))
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, contents, expected) ->
          let session, config, source = inputs mode contents in
          let source_report =
            run_integer_program_report session ~config ~source
              ~max_steps:100_000
          in
          let interpreted =
            match integer_program_report_outcome source_report with
            | Ok checked -> checked.value
            | Error ds -> Alcotest.fail (errors ds)
          in
          let native = report mode contents in
          let execution = (success native).execution in
          bits expected execution.final_value;
          (match Public_shift_class_cases.result_type label with
          | None -> ()
          | Some type_ -> (
              let expected_type = if type_ = "U64" then Program.U64 else I64 in
              Alcotest.(check bool)
                "public-class declared native result" true
                (Option.fold ~none:false
                   ~some:(fun (w : Program.word) -> w.type_ = expected_type)
                   execution.final_value);
              bits expected
                (success
                   (report ~max_steps:execution.executed_steps mode contents))
                  .execution
                  .final_value;
              match
                Native_program.native_outcome
                  (report
                     ~max_steps:(execution.executed_steps - 1)
                     mode contents)
              with
              | Some (Program.Fault fault) ->
                  Alcotest.(check bool)
                    "public-class one-below native allowance" true
                    (fault.kind = Program.Step_limit_exceeded)
              | _ ->
                  Alcotest.fail
                    "public-class one-below native execution succeeded"));
          let expected_steps =
            let declared_default =
              String.starts_with ~prefix:"folded default" label
              || label = "nested folded default"
            in
            if String.starts_with ~prefix:"intrinsic argument" label then
              let _, batch =
                Native_scalar_fixture.execute_source ~mode ~contents ()
                |> function
                | Ok value -> value
                | Error message -> Alcotest.fail message
              in
              VM.executed_steps batch
            else
              VM.executed_steps interpreted
              - if declared_default && mode = Preprocessor.Jit then 1 else 0
          in
          Alcotest.(check int)
            (label ^ " runtime instructions")
            expected_steps execution.executed_steps;
          let interpreted_preparation =
            VM.compiled_initializer_steps interpreted
          in
          let expected_preparation =
            if
              label = "folded static value persists"
              || label = "folded narrow static destination"
            then (
              Alcotest.(check int)
                "interpreted static preparation" 4 interpreted_preparation;
              3)
            else if String.starts_with ~prefix:"intrinsic argument" label then 0
            else interpreted_preparation
          in
          Alcotest.(check int)
            (label ^ " charged preparation")
            expected_preparation
            (Native_program.preparation_steps native);
          List.iter
            (fun status_abi ->
              let image =
                match
                  Native_program.compile ~status_abi session ~config ~source
                with
                | Ok checked -> checked.value
                | Error ds -> Alcotest.fail (errors ds)
              in
              Alcotest.(check bool)
                "both native status ABIs" true
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
                  | Ok (Program.Completed e) -> bits expected e.final_value
                  | _ -> Alcotest.fail "fresh native shift image failed"
                done)
            [ Program.Windows_x64; Program.System_v_x64 ])
        (C.cases @ captured))
    modes

let limits () =
  List.iter
    (fun mode ->
      let control = report mode C.limit_source in
      let steps = (success control).execution.executed_steps in
      bits 42L
        (success (report ~max_steps:steps mode C.limit_source)).execution
          .final_value;
      let below = report ~max_steps:(steps - 1) mode C.limit_source in
      (match Native_program.native_outcome below with
      | Some (Program.Fault fault) ->
          Alcotest.(check bool)
            "native runtime limit" true
            (fault.kind = Program.Step_limit_exceeded);
          Alcotest.(check int)
            "reached runtime work" (steps - 1) fault.executed_steps
      | _ -> Alcotest.fail "one-below runtime did not fault");
      Alcotest.(check string)
        "reached source bytes" "kept"
        (Native_program.output_bytes below);
      Alcotest.(check int)
        "reached source formatting work"
        (Native_program.output_work control)
        (Native_program.output_work below);
      let source =
        "I64 A=1<<3;I64 F(I64 n=(-7<<63)<<1){return n;}I64 B=2<<1;A+B+F();"
      in
      let control = report mode source in
      let prep = Native_program.preparation_steps control in
      Alcotest.(check int) "three immediate harnesses" 9 prep;
      bits 12L
        (success (report ~max_initializer_steps:prep mode source)).execution
          .final_value;
      let below = report ~max_initializer_steps:(prep - 1) mode source in
      (match Native_program.outcome below with
      | Error ds ->
          Alcotest.(check bool)
            "native preparation quota" true
            (List.exists (fun (d : Diagnostic.t) -> d.code = "HCIRVM0007") ds)
      | _ -> Alcotest.fail "one-below preparation completed");
      Alcotest.(check int)
        "exhausted preparation remains charged" (prep - 1)
        (Native_program.preparation_steps below);
      Alcotest.(check bool)
        "failed preparation has no native image" true
        (Option.is_none (Native_program.image below));
      let session, config, source = inputs mode C.limit_source in
      List.iter
        (fun status_abi ->
          let compile ?max_code_bytes ?max_ir_instructions ?max_stack_bytes () =
            Native_program.compile ?max_code_bytes ?max_ir_instructions
              ?max_stack_bytes ~status_abi session ~config ~source
          in
          let image =
            match compile () with
            | Ok checked -> checked.value
            | Error ds -> Alcotest.fail (errors ds)
          in
          let code = Program.code_bytes image
          and ir = Program.ir_instructions image
          and stack = Program.frame_bytes image in
          (match
             compile ~max_code_bytes:code ~max_ir_instructions:ir
               ~max_stack_bytes:stack ()
           with
          | Ok _ -> ()
          | Error ds -> Alcotest.fail (errors ds));
          List.iter
            (function
              | Error (_ :: _) -> ()
              | _ -> Alcotest.fail "one-below native image quota admitted")
            [
              compile ~max_code_bytes:(code - 1) ();
              compile ~max_ir_instructions:(ir - 1) ();
              compile ~max_stack_bytes:(stack - 1) ();
            ])
        [ Program.Windows_x64; Program.System_v_x64 ])
    modes

let faults_and_boundaries () =
  List.iter
    (fun mode ->
      let output = report mode C.output_source in
      bits 42L (success output).execution.final_value;
      Alcotest.(check string)
        "native output argument order" "2:1;"
        (Native_program.output_bytes output);
      List.iter
        (fun (contents, bytes) ->
          let failed = report mode contents in
          let session, config, source = inputs mode contents in
          let interpreted =
            run_integer_program_report session ~config ~source
              ~max_steps:100_000
          in
          let source_fault =
            match A.execute (A.fixture ~source:contents mode) with
            | Error (d :: _) -> d
            | _ -> Alcotest.fail "isolated source fault disappeared"
          in
          (match Native_program.native_outcome failed with
          | Some (Program.Fault fault) ->
              Alcotest.(check bool)
                "following or operand arithmetic fault" true
                (fault.kind = Program.Division_by_zero);
              Alcotest.(check int)
                "original reached fault work" source_fault.executed_steps
                fault.executed_steps;
              Alcotest.(check (option int))
                "original fault instruction" source_fault.instruction_id
                (Some fault.instruction_id);
              Alcotest.(check (option string))
                "original fault function" source_fault.function_name
                fault.function_name;
              let range = Option.map (fun (s : Span.t) -> (s.start, s.stop)) in
              Alcotest.(check (option (pair int int)))
                "original source span" (range source_fault.span)
                (range fault.span)
          | _ -> Alcotest.fail "reached arithmetic did not fault");
          Alcotest.(check string)
            "output from original reached producer" bytes
            (Native_program.output_bytes failed);
          Alcotest.(check int)
            "fault formatting work"
            (integer_program_report_output_work interpreted)
            (Native_program.output_work failed))
        C.fault_cases;
      List.iter
        (fun source ->
          match Native_program.outcome (report mode source) with
          | Error (_ :: _) -> ()
          | _ -> Alcotest.fail "nonconstant preparation widened")
        C.rejected;
      match Native_program.outcome (report mode C.retained) with
      | Error (d :: _) ->
          Alcotest.(check string)
            "retained native source boundary" "HCPP0008" d.code
      | _ -> Alcotest.fail "retained native publication widened")
    modes

let () =
  Alcotest.run "source constant shift native execution"
    [
      ( "source",
        [
          Alcotest.test_case
            "captured fields, consumers, effects and fresh ABI images" `Quick
            values;
          Alcotest.test_case "exact runtime, preparation and image allowances"
            `Quick limits;
          Alcotest.test_case "following faults and preparation boundaries"
            `Quick faults_and_boundaries;
        ] );
    ]
