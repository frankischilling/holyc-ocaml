open Holyc_lib
module Cases = Local_aggregate_sizeof_cases
module Native = Native_source_execution

let describe errors =
  String.concat "; "
    (List.map
       (fun (diagnostic : Diagnostic.t) ->
         diagnostic.code ^ ": " ^ diagnostic.message)
       errors)

let run ?max_steps ?max_initializer_steps ?max_ir_instructions ?max_code_bytes
    ?max_output_bytes ?max_output_work source_text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-local-aggregate-sizeof.hc"
      ~contents:(Cases.headers ^ source_text)
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:Jit () |> Result.get_ok
  in
  Native.evaluate ?max_initializer_steps ?max_ir_instructions ?max_output_bytes
    ?max_output_work
    ~max_steps:(Option.value ~default:100_000 max_steps)
    ~max_code_bytes:(Option.value ~default:524_288 max_code_bytes)
    session ~source ~config

let value (case : Cases.case) report =
  let result =
    match Native.outcome report with
    | Ok result -> result
    | Error errors -> Alcotest.fail (describe errors)
  in
  Alcotest.(check (option int64))
    (case.label ^ " original sizeof word")
    (Some case.expected)
    (Option.map
       (fun (word : Native.word) -> word.bits)
       result.value.final_value);
  Alcotest.(check string)
    "exact reached output" case.output
    (Native.output_bytes report);
  Alcotest.(check int)
    "no interpreted runtime instructions" 0
    (Option.get (Native.source_progress report)).runtime.executed_steps;
  let fragments = Native.fragments report in
  Alcotest.(check bool) "native fragments exist" true (fragments <> []);
  List.iter
    (fun (fragment : Native.fragment) ->
      match fragment.native_outcome with
      | Some (Ok (X86_64_program.Completed _)) -> ()
      | _ -> Alcotest.fail "source fragment lacks native execution completion")
    fragments

let failure code report =
  match Native.outcome report with
  | Ok _ -> Alcotest.fail ("expected " ^ code)
  | Error errors ->
      Alcotest.(check bool)
        (describe errors) true
        (List.exists
           (fun (diagnostic : Diagnostic.t) -> diagnostic.code = code)
           errors)

let runtime_layout_effects_and_limits () =
  let case = Cases.runtime_layout in
  let baseline = run case.source in
  value case baseline;
  let preparation = Native.preparation_steps baseline
  and steps = Native.executed_steps baseline in
  let ir, code =
    List.fold_left
      (fun (ir, code) (fragment : Native.fragment) ->
        (ir + fragment.image.ir_instructions, code + fragment.image.code_bytes))
      (0, 0)
      (Native.fragments baseline)
  in
  let output_work = Native.output_work baseline in
  List.iter
    (fun (label, count) -> Alcotest.(check bool) label true (count > 0))
    [
      ("preparation executed", preparation);
      ("machine instructions executed", steps);
      ("native IR generated", ir);
      ("machine code generated", code);
      ("output formatting work executed", output_work);
    ];
  value case
    (run ~max_initializer_steps:preparation ~max_steps:steps
       ~max_ir_instructions:ir ~max_code_bytes:code
       ~max_output_bytes:(String.length case.output)
       ~max_output_work:output_work case.source);
  failure "HCIRVM0007"
    (run ~max_initializer_steps:(preparation - 1) case.source);
  failure "HCIRVM0007" (run ~max_steps:(steps - 1) case.source);
  failure "HCBACK0001" (run ~max_ir_instructions:(ir - 1) case.source);
  failure "HCBACK0005" (run ~max_code_bytes:(code - 1) case.source);
  failure "HCIRVM0022"
    (run ~max_output_bytes:(String.length case.output - 1) case.source);
  failure "HCIRVM0023" (run ~max_output_work:(output_work - 1) case.source)

let () =
  Alcotest.run "Native original local aggregate sizeof"
    [
      ( "native",
        List.map
          (fun (case : Cases.case) ->
            Alcotest.test_case case.label `Quick (fun () ->
                value case (run case.source)))
          (Cases.closed @ Cases.jit_only)
        @ [
            Alcotest.test_case "once-only native layout and exact budgets"
              `Quick runtime_layout_effects_and_limits;
          ] );
    ]
