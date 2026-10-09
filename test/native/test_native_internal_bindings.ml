open Holyc_lib
module Cases = Internal_binding_cases
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
    Session.add_source session ~path:"native-internal-bindings.hc"
      ~contents:text
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:Jit () |> Result.get_ok
  in
  (session, source, config)

let run ?max_initializer_steps ?max_code_bytes ?max_ir_instructions ?max_steps
    text =
  let session, source, config = inputs text in
  Native.evaluate ?max_initializer_steps ?max_code_bytes ?max_ir_instructions
    session ~source ~config
    ~max_steps:(Option.value ~default:100_000 max_steps)

let value expected report =
  let result = Native.outcome report |> checked in
  Alcotest.(check (option int64))
    "original binding result" (Some expected)
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

let faults () =
  List.iter (fun (text, code) -> failure code (run text)) Cases.faults

let quotas () =
  let text =
    "I64 N=0;I64 Target(){N++;return 0x1e;}_intern Target() I64 Convert(U8 \
     c);Convert(97)+N;"
  in
  let baseline = run text in
  value 66L baseline;
  let prep = Native.preparation_steps baseline
  and steps = Native.executed_steps baseline in
  value 66L (run ~max_initializer_steps:prep ~max_steps:steps text);
  failure "HCIRVM0007" (run ~max_initializer_steps:(prep - 1) text);
  failure "HCIRVM0007" (run ~max_steps:(steps - 1) text);
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
  value 66L (run ~max_ir_instructions:ir ~max_code_bytes:bytes text);
  failure "HCBACK0001" (run ~max_ir_instructions:(ir - 1) text);
  failure "HCBACK0005" (run ~max_code_bytes:(bytes - 1) text);
  let reached =
    run
      "extern U0 Print(U8 *s,...);I64 Target(){Print(\"kept\");return \
       0x1e;}_intern Target() Missing Convert(U8 c);"
  in
  Alcotest.(check bool)
    "later invalid type remains rejected" true
    (Result.is_error (Native.outcome reached));
  Alcotest.(check string)
    "target output precedes type validation" "kept"
    (Native.output_bytes reached)

let capture_authority_case expire =
  let module Task = Holyc_lib__Driver.Integer_task in
  let module Source = Holyc_lib__Driver.Integer_source_execution in
  let module Request = Task.Native_internal_binding in
  let module Program = Holyc_lib__Ir.Internal_binding_fragment_program in
  let module Capture = Holyc_lib__Ir.Native_internal_binding_capture in
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
    Image.create_task_layout_with_literals ~max_global_bytes:16
      ~max_literal_bytes:64
    |> compiled
  in
  let original_layout = layout () in
  let arena =
    Runtime.create_task_arena ~max_arena_bytes:256 original_layout |> unwrap
  in
  let arena_released = ref false in
  let foreign =
    Runtime.create_task_arena ~max_arena_bytes:256 (layout ()) |> unwrap
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
  let native_internal_binding request =
    rejects "foreign domain cannot claim original binding"
      (Domain.join (Domain.spawn (fun () -> Request.claim request)));
    let host, other =
      match Runtime.platform () with
      | Runtime.Windows_x86_64 -> (Image.Windows_x64, Image.System_v_x64)
      | _ -> (Image.System_v_x64, Image.Windows_x64)
    in
    let substituted =
      Image.compile_task_internal_binding ~status_abi:other
        ~layout:original_layout request
      |> compiled
    in
    let image =
      Image.compile_task_internal_binding ~status_abi:host
        ~layout:original_layout request
      |> compiled
    in
    rejects "metadata cannot authorize unevaluated target"
      (Runtime.finish_task_internal_binding arena image);
    let before = (Runtime.budget_progress budget).executed_steps in
    let reached = execute image |> completed in
    Alcotest.(check (option int64))
      "actual original target" (Some 30L)
      (Option.map (fun (word : Image.word) -> word.bits) reached.final_value);
    let work = (Runtime.budget_progress budget).executed_steps - before in
    rejects "foreign arena cannot take capture"
      (Runtime.finish_task_internal_binding foreign image);
    rejects "equal program metadata cannot replace image"
      (Runtime.finish_task_internal_binding arena substituted);
    let capture = Runtime.finish_task_internal_binding arena image |> unwrap in
    rejects "capture cannot be taken twice"
      (Runtime.finish_task_internal_binding arena image);
    let original = Request.program request in
    let copy =
      Program.create
        ~authority:(Program.source_authority original)
        ~destination:(Program.destination original)
        ~lowered:(Program.lowering original)
        ~entry:(Program.entry original)
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
        inputs "_intern 0x1e I64 Convert(U8 c);Convert(97);"
      in
      let report =
        Source.run ~native_dispatch:dispatch ~native_internal_binding session
          ~source ~config ~max_steps:100_000
      in
      if expire then
        Alcotest.(check bool)
          "expired original arena cannot publish target" true
          (Result.is_error (Source.outcome report))
      else (
        Source.outcome report |> checked |> ignore;
        Alcotest.(check bool)
          "authentic binding remains executable" true
          (Source.native_final_value report
          = Some (Task.Native_dispatch.I64 65L)));
      let capture, program, work = Option.get !consumed_capture in
      rejects
        (if expire then "expired capture cannot be consumed directly"
         else "completed target capture cannot be replayed")
        (Capture.consume capture ~program ~work))

let capture_authority () =
  capture_authority_case false;
  capture_authority_case true

let () =
  Alcotest.run "Native original internal bindings"
    [
      ( "bindings",
        [
          Alcotest.test_case "original values, calls and history" `Quick values;
          Alcotest.test_case "reached faults and unsupported values" `Quick
            faults;
          Alcotest.test_case "exact quotas and pre-header output" `Quick quotas;
          Alcotest.test_case
            "actual captures, equal metadata, domains and replay" `Quick
            capture_authority;
        ] );
    ]
