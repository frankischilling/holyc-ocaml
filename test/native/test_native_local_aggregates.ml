open Holyc_lib
module Cases = Local_aggregate_cases
module Native = Native_source_execution

let describe errors =
  errors
  |> List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
  |> String.concat "; "

let run ?max_initializer_steps ?max_code_bytes ?max_ir_instructions ?max_steps
    ?max_output_bytes ?max_output_work text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-local-aggregates.hc"
      ~contents:(Cases.headers ^ text)
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:Jit () |> Result.get_ok
  in
  Native.evaluate ?max_initializer_steps ?max_ir_instructions ?max_output_bytes
    ?max_output_work
    ~max_code_bytes:(Option.value ~default:524_288 max_code_bytes)
    session ~source ~config
    ~max_steps:(Option.value ~default:100_000 max_steps)

let value report =
  let result =
    match Native.outcome report with
    | Ok value -> value
    | Error errors -> Alcotest.fail (describe errors)
  in
  Alcotest.(check (option int64))
    "actual native result" (Some 42L)
    (Option.map (fun word -> word.Native.bits) result.value.final_value);
  Alcotest.(check int)
    "zero interpreted instructions" 0
    (Option.get (Native.source_progress report)).runtime.executed_steps;
  let fragments = Native.fragments report in
  Alcotest.(check bool) "original native fragments exist" true (fragments <> []);
  List.iter
    (fun (fragment : Native.fragment) ->
      match fragment.native_outcome with
      | Some (Ok (X86_64_program.Completed _)) -> ()
      | _ -> Alcotest.fail "an original fragment did not execute natively")
    fragments

let failure code report =
  match Native.outcome report with
  | Ok _ -> Alcotest.fail ("expected " ^ code)
  | Error errors ->
      Alcotest.(check bool)
        (describe errors) true
        (List.exists (fun (d : Diagnostic.t) -> d.code = code) errors)

let effects () =
  let report = run Cases.effects in
  value report;
  Alcotest.(check string)
    "once-only offset and bound output" "offdim"
    (Native.output_bytes report);
  let failed = run Cases.reached_failure in
  failure "HCPARSE0115" failed;
  Alcotest.(check string)
    "native output before malformed tail" "kept"
    (Native.output_bytes failed);
  let position = run Cases.local_position in
  value position;
  Alcotest.(check string)
    "original local position before generated input" "P"
    (Native.output_bytes position)

let quotas () =
  let baseline = run Cases.effects in
  value baseline;
  let prep = Native.preparation_steps baseline
  and steps = Native.executed_steps baseline in
  let ir, bytes =
    List.fold_left
      (fun (ir, bytes) (f : Native.fragment) ->
        (ir + f.image.ir_instructions, bytes + f.image.code_bytes))
      (0, 0)
      (Native.fragments baseline)
  in
  value
    (run ~max_initializer_steps:prep ~max_steps:steps ~max_ir_instructions:ir
       ~max_code_bytes:bytes ~max_output_bytes:6
       ~max_output_work:(Native.output_work baseline)
       Cases.effects);
  failure "HCIRVM0007" (run ~max_initializer_steps:(prep - 1) Cases.effects);
  failure "HCIRVM0007" (run ~max_steps:(steps - 1) Cases.effects);
  failure "HCBACK0001" (run ~max_ir_instructions:(ir - 1) Cases.effects);
  failure "HCBACK0005" (run ~max_code_bytes:(bytes - 1) Cases.effects);
  failure "HCIRVM0022" (run ~max_output_bytes:5 Cases.effects);
  failure "HCIRVM0023"
    (run ~max_output_work:(Native.output_work baseline - 1) Cases.effects)

let boundaries () =
  value
    (run
       "U0 Make(){class Base{U8 a;};class Child:Base{U8 b;};}sizeof(Child)+40;");
  failure "HCEVAL0003" (run "I64 F(){I64 class C{I64 a;} value;return 42;}F();")

let () =
  Alcotest.run "Native classes and unions in statements"
    [
      ( "native",
        List.map
          (fun (name, text) ->
            Alcotest.test_case name `Quick (fun () -> value (run text)))
          Cases.values
        @ [
            Alcotest.test_case "original effects and input order" `Quick effects;
            Alcotest.test_case "exact native and source limits" `Quick quotas;
            Alcotest.test_case "existing object boundaries" `Quick boundaries;
          ] );
    ]
