open Holyc_lib
module Cases = Runtime_layout_cases
module Native = Native_source_execution

let describe errors =
  errors
  |> List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
  |> String.concat "; "

let checked = function
  | Ok value -> value
  | Error errors -> Alcotest.fail (describe errors)

let inputs text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-layout.hc" ~contents:text
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:Jit () |> Result.get_ok
  in
  (session, source, config)

let run ?max_initializer_steps ?max_code_bytes ?max_ir_instructions ?max_steps
    text =
  let session, source, config = inputs text in
  Native.evaluate ?max_initializer_steps
    ~max_code_bytes:(Option.value ~default:262_144 max_code_bytes)
    ?max_ir_instructions session ~source ~config
    ~max_steps:(Option.value ~default:100_000 max_steps)

let value expected report =
  let result = Native.outcome report |> checked in
  Alcotest.(check (option int64))
    "original layout result" (Some expected)
    (Option.map (fun word -> word.Native.bits) result.value.final_value);
  Alcotest.(check int)
    "zero interpreted instructions" 0
    (Option.get (Native.source_progress report)).runtime.executed_steps;
  List.iter
    (fun (fragment : Native.fragment) ->
      match fragment.native_outcome with
      | Some (Ok (X86_64_program.Completed _)) -> ()
      | _ -> Alcotest.fail "fragment did not execute natively")
    (Native.fragments report)

let failure code report =
  match Native.outcome report with
  | Error errors ->
      Alcotest.(check bool)
        (describe errors) true
        (List.exists (fun (d : Diagnostic.t) -> d.code = code) errors)
  | Ok _ -> Alcotest.fail ("expected " ^ code)

let values () =
  List.iter
    (fun (text, expected) ->
      let native = run text in
      value expected native;
      let session, source, config = inputs text in
      let ir =
        run_integer_program_report session ~source ~config ~max_steps:100_000
      in
      let result = integer_program_report_outcome ir |> checked in
      Alcotest.(check (option int64))
        "fresh interpreter comparison" (Some expected)
        (Ir_integer_interpreter.final_value result.value
        |> Option.map (fun word -> word.Ir_integer_interpreter.bits));
      Alcotest.(check string)
        "same original output"
        (integer_program_report_output_bytes ir)
        (Native.output_bytes native))
    Cases.values

let dimension_capture_authority_case expire =
  let module Task = Holyc_lib__Driver.Integer_task in
  let module Source = Holyc_lib__Driver.Integer_source_execution in
  let module Request = Task.Native_dimension in
  let module Program = Holyc_lib__Ir.Dimension_fragment_program in
  let module Capture = Holyc_lib__Ir.Native_scalar_capture in
  let module Runtime = Native_program_execution in
  let module Image = X86_64_program in
  let unwrap = function
    | Ok value -> value
    | Error message -> Alcotest.fail message
  in
  let compiled = function
    | Ok value -> value
    | Error errors ->
        Alcotest.fail
          (String.concat "; "
             (List.map (fun (e : Image.error) -> e.message) errors))
  in
  let rejects label result =
    Alcotest.(check bool) label true (Result.is_error result)
  in
  let layout () =
    Image.create_task_layout_with_literals ~max_global_bytes:64
      ~max_literal_bytes:64
    |> compiled
  in
  let original_layout = layout () in
  let arena =
    Runtime.create_task_arena ~max_arena_bytes:2048 original_layout |> unwrap
  in
  let arena_released = ref false in
  let foreign =
    Runtime.create_task_arena ~max_arena_bytes:2048 (layout ()) |> unwrap
  in
  let budget = Runtime.create_budget ~max_steps:100_000 () |> unwrap in
  let execute image =
    let retained = Runtime.retain_task_fragment arena image |> unwrap in
    Fun.protect
      ~finally:(fun () -> Runtime.release retained |> unwrap)
      (fun () -> Runtime.execute_retained_budget_report budget retained)
  in
  let completed report =
    match Runtime.outcome report |> unwrap with
    | Image.Completed result -> result
    | Image.Fault _ -> Alcotest.fail "unexpected native fault"
  in
  let consumed_capture = ref None in
  let native_dimension request =
    rejects "foreign domain cannot claim original layout"
      (Domain.join (Domain.spawn (fun () -> Request.claim request)));
    let host, other =
      match Runtime.platform () with
      | Runtime.Windows_x86_64 -> (Image.Windows_x64, Image.System_v_x64)
      | _ -> (Image.System_v_x64, Image.Windows_x64)
    in
    let substituted =
      Image.compile_task_dimension ~status_abi:other ~layout:original_layout
        request
      |> compiled
    in
    let image =
      Image.compile_task_dimension ~status_abi:host ~layout:original_layout
        request
      |> compiled
    in
    rejects "metadata cannot authorize unevaluated scalar"
      (Runtime.finish_task_dimension arena image);
    let before = (Runtime.budget_progress budget).executed_steps in
    let reached = execute image |> completed in
    Alcotest.(check (option int64))
      "actual original scalar" (Some 3L)
      (Option.map (fun (word : Image.word) -> word.bits) reached.final_value);
    let work = (Runtime.budget_progress budget).executed_steps - before in
    rejects "foreign arena cannot take capture"
      (Runtime.finish_task_dimension foreign image);
    rejects "equal program metadata cannot replace image"
      (Runtime.finish_task_dimension arena substituted);
    let capture = Runtime.finish_task_dimension arena image |> unwrap in
    rejects "capture cannot be taken twice"
      (Runtime.finish_task_dimension arena image);
    let original = Request.program request in
    let copy =
      Program.create
        ~authority:(Program.source_authority original)
        ~destination:(Program.destination original)
        ~lowered:(Program.lowered original) ~entry:(Program.entry original)
        ~initialization:(Program.initialization original)
        ~runtime_calls:(Program.runtime_calls original)
      |> unwrap
    in
    rejects "equal source, graph and work cannot replace program"
      (Capture.consume capture ~program:copy ~work);
    rejects "caller work cannot replace actual native work"
      (Capture.consume capture ~program:original ~work:(work + 1));
    Gc.full_major ();
    Request.record_steps request work |> unwrap;
    consumed_capture := Some (capture, original, work);
    if expire then (
      Runtime.release_task_arena arena |> unwrap;
      arena_released := true);
    Ok capture
  in
  let dispatch : Task.Native_dispatch.t =
    {
      execute_initializer = (fun _ -> Alcotest.fail "unexpected initializer");
      execute_command =
        (fun request ->
          let image =
            Image.compile_task_command ~layout:original_layout request
            |> compiled
          in
          let result = execute image |> completed in
          Ok
            (Task.Native_dispatch.Captured
               (Option.map
                  (fun (word : Image.word) ->
                    match word.type_ with
                    | Image.I64 -> Task.Native_dispatch.I64 word.bits
                    | U64 -> Task.Native_dispatch.U64 word.bits)
                  result.final_value)));
    }
  in
  Fun.protect
    ~finally:(fun () ->
      Runtime.release_task_arena foreign |> unwrap;
      if not !arena_released then Runtime.release_task_arena arena |> unwrap)
    (fun () ->
      let session, source, config =
        inputs "I64 Next(){return 3;}I64 A[Next()];sizeof(A)+18;"
      in
      let report =
        Source.run ~native_dispatch:dispatch ~native_dimension session ~source
          ~config ~max_steps:100_000
      in
      if expire then
        Alcotest.(check bool)
          "expired original arena cannot publish scalar" true
          (Result.is_error (Source.outcome report))
      else (
        Source.outcome report |> checked |> ignore;
        Alcotest.(check bool)
          "authentic layout remains executable" true
          (Source.native_final_value report
          = Some (Task.Native_dispatch.I64 42L)));
      let capture, program, work = Option.get !consumed_capture in
      rejects
        (if expire then "expired capture cannot be consumed directly"
         else "completed scalar capture cannot be replayed")
        (Capture.consume capture ~program ~work))

let offset_capture_authority_case expire =
  let module Task = Holyc_lib__Driver.Integer_task in
  let module Source = Holyc_lib__Driver.Integer_source_execution in
  let module Request = Task.Native_offset in
  let module Program = Holyc_lib__Ir.Offset_fragment_program in
  let module Capture = Holyc_lib__Ir.Native_scalar_capture in
  let module Runtime = Native_program_execution in
  let module Image = X86_64_program in
  let unwrap = function
    | Ok value -> value
    | Error message -> Alcotest.fail message
  in
  let compiled = function
    | Ok value -> value
    | Error errors ->
        Alcotest.fail
          (String.concat "; "
             (List.map (fun (e : Image.error) -> e.message) errors))
  in
  let rejects label result =
    Alcotest.(check bool) label true (Result.is_error result)
  in
  let layout () =
    Image.create_task_layout_with_literals ~max_global_bytes:64
      ~max_literal_bytes:64
    |> compiled
  in
  let original_layout = layout () in
  let arena =
    Runtime.create_task_arena ~max_arena_bytes:2048 original_layout |> unwrap
  in
  let arena_released = ref false in
  let foreign =
    Runtime.create_task_arena ~max_arena_bytes:2048 (layout ()) |> unwrap
  in
  let budget = Runtime.create_budget ~max_steps:100_000 () |> unwrap in
  let execute image =
    let retained = Runtime.retain_task_fragment arena image |> unwrap in
    Fun.protect
      ~finally:(fun () -> Runtime.release retained |> unwrap)
      (fun () -> Runtime.execute_retained_budget_report budget retained)
  in
  let completed report =
    match Runtime.outcome report |> unwrap with
    | Image.Completed result -> result
    | Image.Fault _ -> Alcotest.fail "unexpected native fault"
  in
  let consumed_capture = ref None in
  let native_offset request =
    rejects "foreign domain cannot claim original layout"
      (Domain.join (Domain.spawn (fun () -> Request.claim request)));
    let host, other =
      match Runtime.platform () with
      | Runtime.Windows_x86_64 -> (Image.Windows_x64, Image.System_v_x64)
      | _ -> (Image.System_v_x64, Image.Windows_x64)
    in
    let substituted =
      Image.compile_task_offset ~status_abi:other ~layout:original_layout
        request
      |> compiled
    in
    let image =
      Image.compile_task_offset ~status_abi:host ~layout:original_layout request
      |> compiled
    in
    rejects "metadata cannot authorize unevaluated scalar"
      (Runtime.finish_task_offset arena image);
    let before = (Runtime.budget_progress budget).executed_steps in
    let reached = execute image |> completed in
    Alcotest.(check (option int64))
      "actual original scalar" (Some 16L)
      (Option.map (fun (word : Image.word) -> word.bits) reached.final_value);
    let work = (Runtime.budget_progress budget).executed_steps - before in
    rejects "foreign arena cannot take capture"
      (Runtime.finish_task_offset foreign image);
    rejects "equal program metadata cannot replace image"
      (Runtime.finish_task_offset arena substituted);
    let capture = Runtime.finish_task_offset arena image |> unwrap in
    rejects "capture cannot be taken twice"
      (Runtime.finish_task_offset arena image);
    let original = Request.program request in
    let copy =
      Program.create
        ~authority:(Program.source_authority original)
        ~destination:(Program.destination original)
        ~lowered:(Program.lowered original) ~entry:(Program.entry original)
        ~initialization:(Program.initialization original)
        ~runtime_calls:(Program.runtime_calls original)
      |> unwrap
    in
    rejects "equal source, graph and work cannot replace program"
      (Capture.consume capture ~program:copy ~work);
    rejects "caller work cannot replace actual native work"
      (Capture.consume capture ~program:original ~work:(work + 1));
    Gc.full_major ();
    Request.record_steps request work |> unwrap;
    consumed_capture := Some (capture, original, work);
    if expire then (
      Runtime.release_task_arena arena |> unwrap;
      arena_released := true);
    Ok capture
  in
  let dispatch : Task.Native_dispatch.t =
    {
      execute_initializer = (fun _ -> Alcotest.fail "unexpected initializer");
      execute_command =
        (fun request ->
          let image =
            Image.compile_task_command ~layout:original_layout request
            |> compiled
          in
          let result = execute image |> completed in
          Ok
            (Task.Native_dispatch.Captured
               (Option.map
                  (fun (word : Image.word) ->
                    match word.type_ with
                    | Image.I64 -> Task.Native_dispatch.I64 word.bits
                    | U64 -> Task.Native_dispatch.U64 word.bits)
                  result.final_value)));
    }
  in
  Fun.protect
    ~finally:(fun () ->
      Runtime.release_task_arena foreign |> unwrap;
      if not !arena_released then Runtime.release_task_arena arena |> unwrap)
    (fun () ->
      let session, source, config =
        inputs
          "I64 Next(){return 16;}class C{U8 a;$$=Next();I64 b;};sizeof(C)+18;"
      in
      let report =
        Source.run ~native_dispatch:dispatch ~native_offset session ~source
          ~config ~max_steps:100_000
      in
      if expire then
        Alcotest.(check bool)
          "expired original arena cannot publish scalar" true
          (Result.is_error (Source.outcome report))
      else (
        Source.outcome report |> checked |> ignore;
        Alcotest.(check bool)
          "authentic layout remains executable" true
          (Source.native_final_value report
          = Some (Task.Native_dispatch.I64 42L)));
      let capture, program, work = Option.get !consumed_capture in
      rejects
        (if expire then "expired capture cannot be consumed directly"
         else "completed scalar capture cannot be replayed")
        (Capture.consume capture ~program ~work))

let captures () =
  dimension_capture_authority_case false;
  dimension_capture_authority_case true;
  offset_capture_authority_case false;
  offset_capture_authority_case true

let effects () =
  let report = run Cases.output in
  value 42L report;
  Alcotest.(check string)
    "once-only bounds and offsets" "dimoff"
    (Native.output_bytes report);
  List.iter
    (fun text ->
      let report = run text in
      Alcotest.(check bool) text true (Result.is_error (Native.outcome report));
      Alcotest.(check string)
        "output before later rejection" "kept"
        (Native.output_bytes report))
    Cases.reached_failures

let quotas () =
  let baseline = run Cases.output in
  value 42L baseline;
  let prep = Native.preparation_steps baseline
  and steps = Native.executed_steps baseline in
  value 42L (run ~max_initializer_steps:prep ~max_steps:steps Cases.output);
  failure "HCIRVM0007" (run ~max_initializer_steps:(prep - 1) Cases.output);
  failure "HCIRVM0007" (run ~max_steps:(steps - 1) Cases.output);
  let fragments = Native.fragments baseline in
  let ir =
    List.fold_left
      (fun n (f : Native.fragment) -> n + f.image.ir_instructions)
      0 fragments
  and bytes =
    List.fold_left
      (fun n (f : Native.fragment) -> n + f.image.code_bytes)
      0 fragments
  in
  value 42L (run ~max_ir_instructions:ir ~max_code_bytes:bytes Cases.output);
  failure "HCBACK0001" (run ~max_ir_instructions:(ir - 1) Cases.output);
  failure "HCBACK0005" (run ~max_code_bytes:(bytes - 1) Cases.output)

let () =
  Alcotest.run "Native original runtime layout"
    [
      ( "layout",
        [
          Alcotest.test_case "original bounds, calls, offsets and history"
            `Quick values;
          Alcotest.test_case "once-only output and reached failures" `Quick
            effects;
          Alcotest.test_case
            "exact instruction, initializer, code and IR limits" `Quick quotas;
          Alcotest.test_case "actual typed captures and original authority"
            `Quick captures;
        ] );
    ]
