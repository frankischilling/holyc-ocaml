open Holyc_lib
module T = Test_swap_internals
module VM = Ir_integer_interpreter
module Program = X86_64_program
module Runtime = Native_program_execution

let errors diagnostics = Test_internal_strlen_authority.diagnostics diagnostics

let inputs mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-swap-internals.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
  in
  (session, config, source)

let report ?(max_steps = 100_000) ?(max_call_depth = 128) mode contents =
  let session, config, source = inputs mode contents in
  Native_program.evaluate ~max_call_depth session ~config ~source ~max_steps

let success report =
  match Native_program.outcome report with
  | Ok checked -> checked.value
  | Error diagnostics -> Alcotest.fail (errors diagnostics)

let native_word expected = function
  | Some (w : Program.word) ->
      Alcotest.(check bool) "native result class" true (w.type_ = Program.I64);
      Alcotest.(check int64) "independent native result" expected w.bits
  | None -> Alcotest.fail "missing native result"

let compare_work native_report source_report source_execution =
  let native = success native_report in
  Alcotest.(check int)
    "original instruction work"
    (VM.executed_steps source_execution)
    native.execution.executed_steps;
  Alcotest.(check string)
    "original output"
    (integer_program_report_output_bytes source_report)
    (Native_program.output_bytes native_report);
  Alcotest.(check int)
    "original formatting work"
    (integer_program_report_output_work source_report)
    (Native_program.output_work native_report)

let values () =
  List.iter
    (fun mode ->
      List.iter
        (fun (_, source, expected) ->
          let native_report = report mode source in
          native_word expected (success native_report).execution.final_value;
          let source_report, source_execution = T.success mode source in
          compare_work native_report source_report source_execution)
        T.cases;
      let source =
        T.declarations ^ "I64 F(){I64 a=8,b=42;SwapI64(&a,&b);return a;}F();"
      in
      native_word 42L
        (success (report ~max_call_depth:1 mode source)).execution.final_value;
      List.iter
        (fun source ->
          match Native_program.outcome (report mode source) with
          | Error (_ :: _) -> ()
          | _ -> Alcotest.fail "unsupported swap executed natively")
        T.rejected)
    T.modes

let void_completion () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let native_report = report mode source in
          Alcotest.(check bool)
            "real void clears final latch" true
            (Option.is_none (success native_report).execution.final_value);
          let source_report, source_execution = T.success mode source in
          compare_work native_report source_report source_execution)
        T.void_cases;
      native_word 42L
        (success
           (report mode
              (T.declarations ^ "I64 a=8,b=42;42;if(0)SwapI64(&a,&b);")))
          .execution
          .final_value)
    T.modes

let faults () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, code, output) ->
          let failed = report mode source in
          let interpreted = T.run mode source in
          let source_fault =
            match
              Test_internal_strlen_authority.execute
                (Test_internal_strlen_authority.fixture ~source mode)
            with
            | Error (first :: _) -> first
            | _ -> Alcotest.fail "isolated invalid swap completed"
          in
          let kind =
            match code with
            | "HCIRVM0012" -> Program.Uninitialized_read
            | "HCIRVM0019" -> Program.Address_out_of_bounds
            | _ -> Alcotest.fail "unknown expected fault"
          in
          (match Native_program.native_outcome failed with
          | Some (Program.Fault fault) ->
              Alcotest.(check bool)
                "original reached fault" true (fault.kind = kind);
              Alcotest.(check int)
                "original attempted work" source_fault.executed_steps
                fault.executed_steps
          | _ -> (
              match Native_program.outcome failed with
              | Error diagnostics -> Alcotest.fail (errors diagnostics)
              | Ok _ -> Alcotest.fail "invalid swap completed"));
          Alcotest.(check string)
            "original reached output" output
            (Native_program.output_bytes failed);
          Alcotest.(check int)
            "original fault formatting work"
            (integer_program_report_output_work interpreted)
            (Native_program.output_work failed))
        T.fault_cases)
    T.modes

let limits () =
  List.iter
    (fun mode ->
      let control = report mode T.limit_source in
      let steps = (success control).execution.executed_steps in
      native_word 42L
        (success (report ~max_steps:steps mode T.limit_source)).execution
          .final_value;
      let below = report ~max_steps:(steps - 1) mode T.limit_source in
      (match Native_program.native_outcome below with
      | Some (Program.Fault fault) ->
          Alcotest.(check bool)
            "runtime quota" true
            (fault.kind = Program.Step_limit_exceeded);
          Alcotest.(check int) "attempted work" (steps - 1) fault.executed_steps
      | _ -> Alcotest.fail "one-below native quota admitted");
      Alcotest.(check string)
        "reached bytes" "kept"
        (Native_program.output_bytes below);
      Alcotest.(check int)
        "reached formatting work"
        (Native_program.output_work control)
        (Native_program.output_work below))
    T.modes

let preparation_boundary () =
  List.iter
    (fun mode ->
      let source =
        T.declarations
        ^ "I64 a=8,b=42;I64 Init(){SwapI64(&a,&b);return a;}I64 Saved(I64 \
           x=Init()){return x;}Saved();"
      in
      (match Native_program.outcome (report mode source) with
      | Error (first :: _) ->
          Alcotest.(check string)
            "closed native preparation" "HCRUN0006" first.code
      | _ -> Alcotest.fail "swap widened native preparation");
      match Native_program.outcome (report mode T.retained) with
      | Error (first :: _) ->
          Alcotest.(check string)
            "retained native publication" "HCPP0008" first.code
      | _ -> Alcotest.fail "retained native boundary widened")
    T.modes

let image_limits_and_abis () =
  List.iter
    (fun mode ->
      let source =
        T.declarations
        ^ "I64 F(){Bool a=-128;U8 b=42;SwapU8(&a,&b);I64 \
           c=8,d=42;SwapI64(&c,&d);return c+(a!=42)+(b!=128)+(d!=8);}F();"
      in
      let session, config, source = inputs mode source in
      List.iter
        (fun status_abi ->
          let compile ?max_code_bytes ?max_stack_bytes () =
            Native_program.compile ?max_code_bytes ?max_stack_bytes ~status_abi
              session ~config ~source
          in
          let image =
            match compile () with
            | Ok checked -> checked.value
            | Error ds -> Alcotest.fail (errors ds)
          in
          let code = Program.code_bytes image
          and stack = Program.frame_bytes image in
          (match compile ~max_code_bytes:code ~max_stack_bytes:stack () with
          | Ok checked ->
              Alcotest.(check bool)
                "compiled status ABI" true
                (Program.status_abi checked.value = status_abi)
          | Error ds -> Alcotest.fail (errors ds));
          List.iter
            (function
              | Error (_ :: _) -> ()
              | _ -> Alcotest.fail "one-below image limit admitted")
            [
              compile ~max_code_bytes:(code - 1) ();
              compile ~max_stack_bytes:(stack - 1) ();
            ];
          let host =
            match Runtime.platform () with
            | Runtime.Windows_x86_64 -> Program.Windows_x64
            | _ -> Program.System_v_x64
          in
          if status_abi = host then
            for _ = 1 to 2 do
              match Runtime.execute ~max_steps:100_000 image with
              | Ok (Program.Completed execution) ->
                  native_word 42L execution.final_value
              | _ -> Alcotest.fail "fresh swap image failed"
            done)
        [ Program.Windows_x64; Program.System_v_x64 ])
    T.modes

let () =
  match Runtime.platform () with
  | Runtime.Unsupported -> Alcotest.fail "native swap tests require x86-64"
  | Runtime.Windows_x86_64 | Runtime.Linux_x86_64 ->
      Alcotest.run "holyc native owned swap calls"
        [
          ( "owned swap calls",
            [
              Alcotest.test_case "original cells and independent swaps" `Quick
                values;
              Alcotest.test_case "real void completion and latch" `Quick
                void_completion;
              Alcotest.test_case "original fault phases and effects" `Quick
                faults;
              Alcotest.test_case "exact runtime limits" `Quick limits;
              Alcotest.test_case "separate preparation paths" `Quick
                preparation_boundary;
              Alcotest.test_case "both ABIs, image limits and fresh execution"
                `Quick image_limits_and_abis;
            ] );
        ]
