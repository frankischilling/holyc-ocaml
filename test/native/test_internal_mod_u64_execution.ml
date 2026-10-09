open Holyc_lib
module T = Test_internal_mod_u64
module Program = X86_64_program
module Runtime = Native_program_execution

let errors diagnostics =
  diagnostics
  |> List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
  |> String.concat "; "

let inputs mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-internal-mod-u64.hc" ~contents
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
          (T.declaration
         ^ "U64 F(){U64 q=442;U64 r;r=ModU64(&q,10);return q+r;}F();")
        |> success
      in
      native_word T.VM.U64 46L only.execution.final_value;
      List.iter
        (fun contents ->
          match Native_program.outcome (report mode contents) with
          | Error (_ :: _) -> ()
          | _ -> Alcotest.fail "invalid internal source executed natively")
        T.rejected)
    T.modes

let limits () =
  List.iter
    (fun mode ->
      let control = report mode T.limit_source in
      let native = success control in
      let steps = native.execution.executed_steps in
      native_word T.VM.U64 46L
        (success (report ~max_steps:steps mode T.limit_source)).execution
          .final_value;
      let below = report ~max_steps:(steps - 1) mode T.limit_source in
      (match Native_program.native_outcome below with
      | Some (Program.Fault fault) ->
          Alcotest.(check bool)
            "step quota" true
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

let faults () =
  List.iter
    (fun mode ->
      List.iter
        (fun (body, code, output) ->
          let failed = report mode (T.fault_source body) in
          let kind =
            match code with
            | "HCIRVM0009" -> Program.Division_by_zero
            | "HCIRVM0012" -> Program.Uninitialized_read
            | "HCIRVM0019" -> Program.Address_out_of_bounds
            | _ -> Alcotest.fail "unknown expected fault"
          in
          (match Native_program.native_outcome failed with
          | Some (Program.Fault fault) ->
              Alcotest.(check bool)
                "original reached fault" true (fault.kind = kind)
          | _ -> (
              match Native_program.outcome failed with
              | Error diagnostics -> Alcotest.fail (errors diagnostics)
              | Ok _ -> Alcotest.fail "invalid operation completed"));
          Alcotest.(check string)
            "original reached output" output
            (Native_program.output_bytes failed))
        T.fault_cases)
    T.modes

let preparation_boundary () =
  List.iter
    (fun mode ->
      let source =
        T.declaration ^ "U64 q=442;U64 F(U64 x=ModU64(&q,10)){return x;}F();"
      in
      (match Native_program.outcome (report mode source) with
      | Error (first :: _) ->
          Alcotest.(check string) "closed preparation" "HCRUN0006" first.code
      | _ -> Alcotest.fail "runtime call widened native preparation");
      match Native_program.outcome (report mode T.retained) with
      | Error (first :: _) ->
          Alcotest.(check string) "retained native" "HCPP0008" first.code
      | _ -> Alcotest.fail "retained native boundary widened")
    T.modes

let image_limits_and_abis () =
  List.iter
    (fun mode ->
      let contents =
        T.declaration
        ^ "U64 F(){U64 q=442;U64 r;r=ModU64(&q,10);return q+r;}F();"
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
                  native_word T.VM.U64 46L execution.final_value
              | _ -> Alcotest.fail "fresh native image failed"
            done)
        [ Program.Windows_x64; Program.System_v_x64 ])
    T.modes

let () =
  match Runtime.platform () with
  | Runtime.Unsupported -> Alcotest.fail "native ModU64 tests require x86-64"
  | Runtime.Windows_x86_64 | Runtime.Linux_x86_64 ->
      Alcotest.run "holyc native ModU64 calls"
        [
          ( "ModU64 calls",
            [
              Alcotest.test_case "unsigned words and original mutations" `Quick
                values;
              Alcotest.test_case "exact runtime limits" `Quick limits;
              Alcotest.test_case "original fault phases and effects" `Quick
                faults;
              Alcotest.test_case "separate preparation paths" `Quick
                preparation_boundary;
              Alcotest.test_case "both ABIs, image limits and fresh execution"
                `Quick image_limits_and_abis;
            ] );
        ]
