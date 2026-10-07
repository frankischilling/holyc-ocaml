open Holyc_lib
module Cases = Stream_generation_cases
module Native = Native_source_execution
module Image = X86_64_program
module VM = Ir_integer_interpreter

let describe errors =
  errors
  |> List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
  |> String.concat "; "

let checked = function
  | Ok value -> value
  | Error errors -> Alcotest.fail (describe errors)

let unwrap = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let rejects label result =
  Alcotest.(check bool) label true (Result.is_error result)

let inputs ?max_generated_bytes text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-stream-generation.hc"
      ~contents:(Cases.headers ^ text)
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:Jit ?max_generated_bytes ()
    |> Result.get_ok
  in
  (session, source, config)

let run ?max_generated_bytes ?max_output_bytes ?max_output_work ?max_steps text
    =
  let session, source, config = inputs ?max_generated_bytes text in
  Native.evaluate ~max_code_bytes:524_288 ?max_output_bytes ?max_output_work
    session ~source ~config
    ~max_steps:(Option.value ~default:100_000 max_steps)

let value report =
  let result = Native.outcome report |> checked in
  Alcotest.(check (option int64))
    "source value" (Some 42L)
    (Option.map (fun word -> word.Native.bits) result.value.final_value);
  Alcotest.(check int)
    "zero interpreted instructions" 0
    (Option.get (Native.source_progress report)).runtime.executed_steps;
  List.iter
    (fun (fragment : Native.fragment) ->
      match fragment.native_outcome with
      | Some (Ok (Image.Completed _)) -> ()
      | _ -> Alcotest.fail "source fragment did not complete in machine code")
    (Native.fragments report)

let generated report =
  (Option.get (Native.source_progress report)).runtime.generated_bytes

let failure code report =
  match Native.outcome report with
  | Ok _ -> Alcotest.fail ("expected " ^ code)
  | Error errors ->
      Alcotest.(check bool)
        (describe errors) true
        (List.exists (fun (d : Diagnostic.t) -> d.code = code) errors)

let values () =
  List.iter
    (fun (name, text, output, bytes) ->
      let report = run text in
      value report;
      Alcotest.(check string) name output (Native.output_bytes report);
      Alcotest.(check int) (name ^ " generation") bytes (generated report);
      let session, source, config = inputs text in
      let ir =
        run_integer_program_report session ~source ~config ~max_steps:100_000
      in
      let result = integer_program_report_outcome ir |> checked in
      Alcotest.(check (option int64))
        (name ^ " independent IR value")
        (Some 42L)
        (VM.final_value result.value |> Option.map (fun word -> word.VM.bits));
      Alcotest.(check string)
        (name ^ " independent IR output")
        output
        (integer_program_report_output_bytes ir);
      Alcotest.(check int)
        (name ^ " same original formatting work")
        (integer_program_report_output_work ir)
        (Native.output_work report))
    Cases.values

let failures () =
  List.iter
    (fun (name, text, code, output, work, bytes) ->
      let report = run text in
      failure code report;
      Alcotest.(check string) name output (Native.output_bytes report);
      Alcotest.(check int)
        (name ^ " reached work") work
        (Native.output_work report);
      Alcotest.(check int)
        (name ^ " retained generation")
        bytes (generated report);
      Alcotest.(check int)
        (name ^ " no interpreter fallback")
        0 (Option.get (Native.source_progress report)).runtime.executed_steps;
      Alcotest.(check bool)
        (name ^ " reached machine fault")
        true
        (List.exists
           (fun (fragment : Native.fragment) ->
             match fragment.native_outcome with
             | Some (Ok (Image.Fault _)) -> true
             | _ -> false)
           (Native.fragments report)))
    Cases.failures

let quotas () =
  let source = Cases.quota_source in
  let baseline =
    run ~max_generated_bytes:3 ~max_output_bytes:2 ~max_output_work:12 source
  in
  value baseline;
  Alcotest.(check string)
    "ordinary output has its own bound" "ok"
    (Native.output_bytes baseline);
  Alcotest.(check int) "generation has its own bound" 3 (generated baseline);
  value (run ~max_steps:(Native.executed_steps baseline) source);
  failure "HCIRVM0007"
    (run ~max_steps:(Native.executed_steps baseline - 1) source);
  let small = run ~max_generated_bytes:2 source in
  failure "HCIRVM0028" small;
  Alcotest.(check int) "failed draft did not commit" 0 (generated small);
  let work = run ~max_output_work:6 source in
  failure "HCIRVM0023" work;
  Alcotest.(check int) "work failure did not commit" 0 (generated work);
  let partial = run ~max_output_bytes:1 source in
  failure "HCIRVM0022" partial;
  Alcotest.(check string)
    "ordinary failed draft is atomic" ""
    (Native.output_bytes partial);
  Alcotest.(check int) "previous generation is retained" 3 (generated partial);
  value (run ~max_generated_bytes:6 Cases.two_streams);
  let small = run ~max_generated_bytes:5 Cases.two_streams in
  failure "HCIRVM0028" small;
  Alcotest.(check int) "previous stream stays charged" 3 (generated small)

let capture_authority () =
  let module Task = Holyc_lib__Driver.Integer_task in
  let module Source = Holyc_lib__Driver.Integer_source_execution in
  let module Internal_vm = Holyc_lib__Ir.Integer_interpreter in
  let module Capture = Holyc_lib__Ir.Native_generation_capture in
  let module Runtime = Native_program_execution in
  let compiled = function
    | Ok value -> value
    | Error errors ->
        Alcotest.fail
          (String.concat "; "
             (List.map (fun (e : Image.error) -> e.message) errors))
  in
  let layout () =
    Image.create_task_layout_with_literals ~max_global_bytes:64
      ~max_literal_bytes:128
    |> compiled
  in
  let original_layout = layout () in
  let arena =
    Runtime.create_task_arena ~max_arena_bytes:4096 original_layout |> unwrap
  in
  let foreign =
    Runtime.create_task_arena ~max_arena_bytes:4096 (layout ()) |> unwrap
  in
  let budget = Runtime.create_budget ~max_steps:100_000 () |> unwrap in
  let saved = ref None in
  let execute request =
    rejects "foreign domain cannot claim original stream entry"
      (Domain.join
         (Domain.spawn (fun () ->
              Task.Native_dispatch.claim_command_request request)));
    let host, other =
      match Runtime.platform () with
      | Windows_x86_64 -> (Image.Windows_x64, Image.System_v_x64)
      | _ -> (Image.System_v_x64, Image.Windows_x64)
    in
    let wrong =
      Image.compile_task_command ~status_abi:other ~layout:original_layout
        request
      |> compiled
    in
    rejects "foreign ABI cannot retain stream entry"
      (Runtime.retain_task_fragment arena wrong);
    let image =
      Image.compile_task_command ~status_abi:host ~layout:original_layout
        request
      |> compiled
    in
    let target = Option.get (Image.generation image) in
    let active, _, _ = Internal_vm.native_generation_limits target |> unwrap in
    let copy = Obj.obj (Obj.dup (Obj.repr target)) in
    let foreign_target = Task.Native_dispatch.command_generation request in
    rejects "generation token belongs to its original domain"
      (Domain.join
         (Domain.spawn (fun () -> Internal_vm.native_generation_limits target)));
    rejects "equal metadata in another arena grants no entry"
      (Runtime.retain_task_fragment foreign image);
    let retained = Runtime.retain_task_fragment arena image |> unwrap in
    let report =
      Fun.protect
        ~finally:(fun () -> Runtime.release retained |> unwrap)
        (fun () ->
          Gc.full_major ();
          Gc.compact ();
          Runtime.execute_retained_budget_report budget retained)
    in
    let result =
      match Runtime.outcome report |> unwrap with
      | Image.Completed value -> value
      | Image.Fault _ -> Alcotest.fail "unexpected stream fault"
    in
    let capture = Option.get (Runtime.generation_capture report) in
    Gc.full_major ();
    Gc.compact ();
    rejects "metadata clone cannot consume an executed capture"
      (Capture.consume capture ~target:copy);
    rejects "another original target cannot consume capture"
      (Capture.consume capture ~target:foreign_target);
    rejects "actual capture was already published exactly once"
      (Capture.consume capture ~target);
    rejects "original target cannot be completed twice"
      (Internal_vm.complete_native_generation target capture);
    rejects "released command cannot enter twice"
      (Task.Native_dispatch.claim_command_request request);
    if active then saved := Some (target, capture);
    Ok
      (Task.Native_dispatch.Captured
         (Option.map
            (fun (word : Image.word) ->
              match word.type_ with
              | I64 -> Task.Native_dispatch.I64 word.bits
              | U64 -> Task.Native_dispatch.U64 word.bits)
            result.final_value))
  in
  let dispatch : Task.Native_dispatch.t =
    {
      execute_initializer =
        (fun _ -> Alcotest.fail "fixture has no initializer");
      execute_command = execute;
    }
  in
  Fun.protect
    ~finally:(fun () ->
      Runtime.release_task_arena foreign |> unwrap;
      Runtime.release_task_arena arena |> unwrap)
    (fun () ->
      let session, source, config = inputs {|#exe {StreamPrint("42;");}|} in
      let report =
        Source.run ~native_dispatch:dispatch session ~source ~config
          ~max_steps:100_000
      in
      Source.outcome report |> checked |> ignore;
      Alcotest.(check bool)
        "original generated source remains executable" true
        (Source.native_final_value report = Some (Task.Native_dispatch.I64 42L));
      Alcotest.(check int)
        "actual generation is charged once" 3
        (Option.get (Source.progress report)).runtime.generated_bytes;
      let target, capture = Option.get !saved in
      Runtime.release_task_arena arena |> unwrap;
      Gc.full_major ();
      Gc.compact ();
      let error =
        match Capture.consume capture ~target with
        | Ok _ -> Alcotest.fail "expired arena authorized source bytes"
        | Error message -> message
      in
      Alcotest.(check bool)
        "capture retains original arena expiry" true
        (String.starts_with ~prefix:"native generation capture has an expired"
           error))

let () =
  Alcotest.run "Native original stream generation"
    [
      ( "source",
        [
          Alcotest.test_case "values and retained owners" `Quick values;
          Alcotest.test_case "reached failures" `Quick failures;
          Alcotest.test_case "shared work and cumulative bytes" `Quick quotas;
          Alcotest.test_case "executed capture identity and lifetime" `Quick
            capture_authority;
        ] );
    ]
