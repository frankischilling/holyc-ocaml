open Holyc_lib
module Cases = Source_inherited_layout_cases
module Native = Native_source_execution

let describe errors =
  List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message) errors
  |> String.concat "; "

let run ?max_steps ?max_initializer_steps ?max_code_bytes ?max_ir_instructions
    ?max_output_bytes ?max_output_work text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-inherited-layouts.hc"
      ~contents:(Cases.headers ^ text)
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:Jit () |> Result.get_ok
  in
  Native.evaluate ?max_initializer_steps ?max_ir_instructions ?max_output_bytes
    ?max_output_work
    ~max_steps:(Option.value ~default:100_000 max_steps)
    ~max_code_bytes:(Option.value ~default:524_288 max_code_bytes)
    session ~source ~config

let value report =
  let result =
    match Native.outcome report with
    | Ok result -> result
    | Error errors -> Alcotest.fail (describe errors)
  in
  Alcotest.(check (option int64))
    "original inherited native size" (Some 42L)
    (Option.map (fun word -> word.Native.bits) result.value.final_value);
  Alcotest.(check int)
    "zero interpreted instructions" 0
    (Option.get (Native.source_progress report)).runtime.executed_steps;
  let fragments = Native.fragments report in
  Alcotest.(check bool) "native fragments exist" true (fragments <> []);
  List.iter
    (fun (fragment : Native.fragment) ->
      match fragment.native_outcome with
      | Some (Ok (X86_64_program.Completed _)) -> ()
      | _ -> Alcotest.fail "original fragment did not complete natively")
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
    "native inherited preparation occurs once" "dimoff"
    (Native.output_bytes report);
  List.iter
    (fun kind ->
      let fragments =
        List.filter
          (fun (f : Native.fragment) -> f.kind = kind)
          (Native.fragments report)
      in
      Alcotest.(check int)
        "one original native preparation" 1 (List.length fragments))
    [ Native.Dimension; Native.Offset ];
  List.iter
    (fun (source, code) ->
      let report = run source in
      failure code report;
      Alcotest.(check string)
        "lookahead precedes native base attachment" "0"
        (Native.output_bytes report))
    [ (Cases.bad_brace, "HCPARSE0110"); (Cases.comma, "HCPARSE0126") ]

let quotas () =
  let baseline = run Cases.effects in
  value baseline;
  let steps = Native.executed_steps baseline
  and prep = Native.preparation_steps baseline in
  let ir, bytes =
    List.fold_left
      (fun (ir, bytes) (f : Native.fragment) ->
        (ir + f.image.ir_instructions, bytes + f.image.code_bytes))
      (0, 0)
      (Native.fragments baseline)
  in
  value
    (run ~max_steps:steps ~max_initializer_steps:prep ~max_code_bytes:bytes
       ~max_ir_instructions:ir ~max_output_bytes:6
       ~max_output_work:(Native.output_work baseline)
       Cases.effects);
  failure "HCIRVM0007" (run ~max_steps:(steps - 1) Cases.effects);
  failure "HCIRVM0007" (run ~max_initializer_steps:(prep - 1) Cases.effects);
  failure "HCBACK0005" (run ~max_code_bytes:(bytes - 1) Cases.effects);
  failure "HCBACK0001" (run ~max_ir_instructions:(ir - 1) Cases.effects);
  failure "HCIRVM0022" (run ~max_output_bytes:5 Cases.effects);
  failure "HCIRVM0023"
    (run ~max_output_work:(Native.output_work baseline - 1) Cases.effects)

let boundaries () =
  failure "HCSEMA0074" (run Cases.object_storage);
  failure "HCRUN0001" (run Cases.overflow)

let () =
  Alcotest.run "Native original inherited metadata"
    [
      ( "native",
        List.map
          (fun (name, text) ->
            Alcotest.test_case name `Quick (fun () -> value (run text)))
          (Cases.values @ Cases.jit_values)
        @ [
            Alcotest.test_case "once-only native effects and source order"
              `Quick effects;
            Alcotest.test_case "exact source and native budgets" `Quick quotas;
            Alcotest.test_case "existing storage boundaries" `Quick boundaries;
          ] );
    ]
