open Holyc_lib
module T = Test_internal_minmax
module Program = X86_64_program
module Runtime = Native_program_execution

let errors diagnostics =
  diagnostics
  |> List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
  |> String.concat "; "

let inputs mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-internal-minmax.hc" ~contents
  in
  let config =
    match Preprocessor.Config.create ~compilation_mode:mode () with
    | Ok config -> config
    | Error message -> Alcotest.fail message
  in
  (session, config, source)

let report ?(max_steps = 100_000) ?(max_call_depth = 128) mode contents =
  let session, config, source = inputs mode contents in
  Native_program.evaluate ~max_call_depth session ~config ~source ~max_steps

let native_word type_ expected = function
  | Some (w : Program.word) ->
      Alcotest.(check bool)
        "native result class" true
        (w.type_ = if type_ = T.VM.I64 then Program.I64 else Program.U64);
      Alcotest.(check int64) "native bits" expected w.bits
  | None -> Alcotest.fail "missing native word"

let success report =
  match Native_program.outcome report with
  | Ok checked -> checked.value
  | Error diagnostics -> Alcotest.fail (errors diagnostics)

let values () =
  List.iter
    (fun mode ->
      List.iter
        (fun (label, contents, type_, expected) ->
          let native_report = report mode contents in
          let native = success native_report in
          native_word type_ expected native.execution.final_value;
          let _, interpreted = T.success mode contents in
          T.word label type_ expected interpreted;
          let _, batch =
            Native_scalar_fixture.execute_source ~mode ~contents () |> function
            | Ok value -> value
            | Error message -> Alcotest.fail message
          in
          T.word (label ^ " closed IR") type_ expected batch;
          Alcotest.(check int)
            "same closed IR/native instruction work"
            (T.VM.executed_steps batch)
            native.execution.executed_steps;
          Alcotest.(check string)
            "no output" ""
            (Native_program.output_bytes native_report);
          Alcotest.(check int)
            "no formatting work" 0
            (Native_program.output_work native_report))
        T.cases;
      let only =
        report ~max_call_depth:1 mode
          (T.declaration ^ "I64 F(){return MinI64(42,50);}F();")
        |> success
      in
      native_word T.VM.I64 42L only.execution.final_value;
      List.iter
        (fun contents ->
          match Native_program.outcome (report mode contents) with
          | Error (_ :: _) -> ()
          | _ -> Alcotest.fail "invalid internal source executed natively")
        T.rejected)
    T.modes

let limits_and_faults () =
  List.iter
    (fun mode ->
      let source =
        T.declaration
        ^ "extern U0 Print(U8 *fmt,...);Print(\"kept\");MinI64(42,50);"
      in
      let control = report mode source in
      let native = success control in
      let steps = native.execution.executed_steps in
      native_word T.VM.I64 42L
        (success (report ~max_steps:steps mode source)).execution.final_value;
      let below = report ~max_steps:(steps - 1) mode source in
      (match Native_program.native_outcome below with
      | Some (Program.Fault fault) ->
          Alcotest.(check bool)
            "step fault" true
            (fault.kind = Program.Step_limit_exceeded);
          Alcotest.(check int) "reached work" (steps - 1) fault.executed_steps
      | _ -> Alcotest.fail "one-below budget did not reach a native fault");
      Alcotest.(check string)
        "prior bytes" "kept"
        (Native_program.output_bytes below);
      Alcotest.(check int)
        "prior work"
        (Native_program.output_work control)
        (Native_program.output_work below);
      let unknown =
        T.declaration
        ^ "extern U0 Print(U8 *fmt,...);I64 F(){I64 ch;Print(\"kept\");return \
           MaxI64(ch,42);}F();"
      in
      let failed = report mode unknown in
      (match Native_program.native_outcome failed with
      | Some (Program.Fault fault) ->
          Alcotest.(check bool)
            "argument load faults" true
            (fault.kind = Program.Uninitialized_read)
      | _ -> Alcotest.fail "unknown argument did not fault");
      Alcotest.(check string)
        "argument fault retains output" "kept"
        (Native_program.output_bytes failed))
    T.modes

let argument_faults () =
  List.iter
    (fun mode ->
      List.iter
        (fun (expression, expected) ->
          let failed = report mode (T.fault_source expression) in
          (match Native_program.native_outcome failed with
          | Some (Program.Fault fault) ->
              Alcotest.(check bool)
                "original argument fault" true
                (fault.kind = Program.Uninitialized_read)
          | _ ->
              Alcotest.fail "unknown argument did not reach a native load fault");
          Alcotest.(check string)
            "original reached argument bytes" expected
            (Native_program.output_bytes failed))
        T.fault_cases)
    T.modes

let preparation_boundary () =
  List.iter
    (fun mode ->
      List.iter
        (fun (type_, expression, _) ->
          let contents =
            T.declaration ^ type_ ^ " F(" ^ type_ ^ " x=" ^ expression
            ^ "){return x;}F();"
          in
          match Native_program.outcome (report mode contents) with
          | Error (first :: _) ->
              Alcotest.(check string)
                "constant preparation remains separate" "HCRUN0006" first.code
          | _ ->
              Alcotest.fail
                "runtime intrinsic widened native constant preparation")
        T.default_cases)
    T.modes

let operation_limits () =
  List.iter
    (fun mode ->
      List.iter
        (fun (type_, expression, expected) ->
          let contents =
            T.declaration ^ "extern U0 Print(U8 *fmt,...);Print(\"kept\");"
            ^ expression ^ ";"
          in
          let control = report mode contents |> success in
          let steps = control.execution.executed_steps in
          let word_type = if type_ = "I64" then T.VM.I64 else T.VM.U64 in
          native_word word_type expected
            (success (report ~max_steps:steps mode contents)).execution
              .final_value;
          let below = report ~max_steps:(steps - 1) mode contents in
          (match Native_program.native_outcome below with
          | Some (Program.Fault fault) ->
              Alcotest.(check bool)
                "step quota reached" true
                (fault.kind = Program.Step_limit_exceeded)
          | _ -> Alcotest.fail "one-below Min/Max operation quota admitted");
          Alcotest.(check string)
            "reached output" "kept"
            (Native_program.output_bytes below))
        T.default_cases)
    T.modes

let image_limits_and_abis () =
  List.iter
    (fun mode ->
      let contents =
        T.declaration
        ^ "I64 F(){return \
           MinI64(-3,4)+MaxI64(40,42)+MinU64(3,5)+MaxU64(0,0);}F();"
      in
      let session, config, source = inputs mode contents in
      let _, interpreted = T.success mode contents in
      let steps = T.VM.executed_steps interpreted in
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
                "compiled ABI" true
                (Program.status_abi checked.value = status_abi)
          | Error ds -> Alcotest.fail (errors ds));
          List.iter
            (function
              | Error (_ :: _) -> ()
              | _ -> Alcotest.fail "one-below native image limit accepted")
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
              match Runtime.execute ~max_steps:steps image with
              | Ok (Program.Completed execution) ->
                  native_word T.VM.I64 42L execution.final_value
              | _ -> Alcotest.fail "fresh native image failed"
            done)
        [ Program.Windows_x64; Program.System_v_x64 ])
    T.modes

let () =
  match Runtime.platform () with
  | Runtime.Unsupported ->
      Alcotest.fail "native Min/Max integer tests require x86-64"
  | Runtime.Windows_x86_64 | Runtime.Linux_x86_64 ->
      Alcotest.run "holyc native Min/Max integer calls"
        [
          ( "Min/Max integer calls",
            [
              Alcotest.test_case "public values and signatures" `Quick values;
              Alcotest.test_case "runtime limits and prior effects" `Quick
                limits_and_faults;
              Alcotest.test_case "argument fault order and reached output"
                `Quick argument_faults;
              Alcotest.test_case "all Min/Max runtime limits" `Quick
                operation_limits;
              Alcotest.test_case "runtime and constant preparation boundary"
                `Quick preparation_boundary;
              Alcotest.test_case "image limits, both ABIs and fresh execution"
                `Quick image_limits_and_abis;
            ] );
        ]
