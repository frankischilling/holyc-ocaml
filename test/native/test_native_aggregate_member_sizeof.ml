open Holyc_lib
module Cases = Aggregate_member_sizeof_cases
module Native = Native_source_execution

let describe errors =
  String.concat "; "
    (List.map
       (fun (diagnostic : Diagnostic.t) ->
         diagnostic.code ^ ": " ^ diagnostic.message)
       errors)

let contains text fragment =
  let text = String.lowercase_ascii text
  and fragment = String.lowercase_ascii fragment in
  let length = String.length fragment in
  let rec loop at =
    at + length <= String.length text
    && (String.sub text at length = fragment || loop (at + 1))
  in
  loop 0

let run ?max_steps ?max_initializer_steps ?max_ir_instructions ?max_code_bytes
    ?max_output_bytes ?max_output_work source_text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-aggregate-member-sizeof.hc"
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
    (case.label ^ " native member byte count")
    (Some case.expected)
    (Option.map
       (fun (word : Native.word) -> word.bits)
       result.value.final_value);
  Alcotest.(check string)
    "exact native output" case.output
    (Native.output_bytes report);
  Alcotest.(check int)
    "no interpreted runtime instructions" 0
    (Option.get (Native.source_progress report)).runtime.executed_steps;
  let fragments = Native.fragments report in
  Alcotest.(check bool)
    "real machine code fragments exist" true (fragments <> []);
  List.iter
    (fun (fragment : Native.fragment) ->
      match fragment.native_outcome with
      | Some (Ok (X86_64_program.Completed _)) -> ()
      | _ -> Alcotest.fail "native execution did not complete a fragment")
    fragments

let failure code report =
  match Native.outcome report with
  | Ok _ -> Alcotest.fail ("expected " ^ code)
  | Error errors ->
      Alcotest.(check bool)
        (describe errors) true
        (List.exists (fun (d : Diagnostic.t) -> d.code = code) errors)

let reject ?(expected_output = "") (case : Cases.rejection) () =
  let report = run case.source in
  match Native.outcome report with
  | Ok _ -> Alcotest.fail (case.label ^ " unexpectedly compiled as native")
  | Error errors ->
      Alcotest.(check bool) "rejection has a diagnostic" true (errors <> []);
      Option.iter
        (fun code ->
          Alcotest.(check bool)
            (describe errors) true
            (List.exists (fun (d : Diagnostic.t) -> d.code = code) errors))
        case.code;
      Option.iter
        (fun fragment ->
          Alcotest.(check bool)
            (describe errors) true
            (contains (describe errors) fragment))
        case.message_fragment;
      Alcotest.(check string)
        "exact source-reached native refusal output" expected_output
        (Native.output_bytes report)

let unsupported_storage (case : Cases.unsupported_storage_case) () =
  reject (Cases.reject ~code:case.native_code case.label case.source) ()

let runtime_layout_effects_and_exact_limits () =
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
      ("original layout preparation", preparation);
      ("machine instructions", steps);
      ("generated IR instructions", ir);
      ("generated machine-code bytes", code);
      ("native output formatting work", output_work);
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
  Alcotest.run "Native original aggregate member sizeof"
    [
      ( "native",
        List.map
          (fun (case : Cases.case) ->
            Alcotest.test_case case.label `Quick (fun () ->
                value case (run case.source)))
          (Cases.closed @ Cases.jit_only)
        @ List.map
            (fun (case : Cases.rejection) ->
              Alcotest.test_case ("reject " ^ case.label) `Quick (reject case))
            Cases.rejected
        @ List.map
            (fun (case : Cases.unsupported_storage_case) ->
              Alcotest.test_case
                ("storage boundary " ^ case.label)
                `Quick (unsupported_storage case))
            Cases.unsupported_storage
        @ List.map
            (fun ((case : Cases.rejection), expected_output) ->
              Alcotest.test_case
                ("JIT read timing " ^ case.label)
                `Quick
                (reject ~expected_output case))
            Cases.jit_rejected_effects
        @ [
            Alcotest.test_case "once-only native layout and exact budgets"
              `Quick runtime_layout_effects_and_exact_limits;
          ] );
    ]
