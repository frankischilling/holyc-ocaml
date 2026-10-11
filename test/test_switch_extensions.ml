open Holyc_lib
module Cases = Switch_extension_cases
module VM = Ir_integer_interpreter
module Graph = Ir_block_graph
module Sequence = Ir_instruction_sequence

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let mode_name = function
  | Preprocessor.Jit -> "JIT"
  | Preprocessor.Aot -> "AOT"

let describe diagnostics =
  diagnostics
  |> List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
  |> String.concat "; "

let run ?max_switch_work ~mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"switch-extensions.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
  in
  run_integer_program_report ?max_switch_work session ~config ~source
    ~max_steps:100_000

let check_value mode (case : Cases.value) () =
  let report = run ~mode case.source in
  let checked =
    match integer_program_report_outcome report with
    | Ok checked -> checked
    | Error errors -> Alcotest.fail (describe errors)
  in
  Alcotest.(check string)
    "no unexpected output" ""
    (integer_program_report_output_bytes report);
  Alcotest.(check bool)
    "program reaches stream end" true
    (VM.termination checked.value = VM.Stream_end);
  match VM.final_value checked.value with
  | None -> Alcotest.fail (case.label ^ " has no result")
  | Some word ->
      Alcotest.(check bool) "source result is I64" true (word.type_ = VM.I64);
      Alcotest.(check int64) case.label case.expected word.bits

let first_error report =
  match integer_program_report_outcome report with
  | Ok _ -> Alcotest.fail "invalid switch unexpectedly executed"
  | Error [] -> Alcotest.fail "switch failure has no diagnostics"
  | Error (first :: _) -> first

let check_parse_rejection mode (case : Cases.rejection) () =
  let report = run ~mode case.source in
  Alcotest.(check string)
    (case.label ^ " fails in its original parse phase")
    case.code (first_error report).code;
  Alcotest.(check int)
    "later case expressions are never prepared" 0
    (integer_program_report_switch_work report);
  Alcotest.(check string)
    "rejected source produces no output" ""
    (integer_program_report_output_bytes report)

let check_runtime_rejection mode (case : Cases.rejection) () =
  let report = run ~mode case.source in
  Alcotest.(check string)
    (case.label ^ " produces a reached dispatch error")
    case.code (first_error report).code;
  Alcotest.(check bool)
    "case table was prepared before dispatch" true
    (integer_program_report_switch_work report > 0);
  Alcotest.(check string)
    "faulting dispatch produced no output" ""
    (integer_program_report_output_bytes report)

let check_goto_rejection mode (case : Cases.rejection) () =
  let report = run ~mode case.source in
  Alcotest.(check string)
    (case.label ^ " rejects its source goto before execution")
    case.code (first_error report).code;
  Alcotest.(check bool)
    "source case preparation happened before lowering" true
    (integer_program_report_switch_work report > 0);
  Alcotest.(check string)
    "refused goto has no output" ""
    (integer_program_report_output_bytes report)

let check_jit_effectful_values () =
  List.iter
    (fun case -> check_value Preprocessor.Jit case ())
    Cases.jit_effectful_values

let check_jit_effectful_failure () =
  List.iter
    (fun (case : Cases.rejection) ->
      let report = run ~mode:Preprocessor.Jit case.source in
      Alcotest.(check string)
        (case.label ^ " stops at the reached failed endpoint")
        case.code (first_error report).code;
      Alcotest.(check bool)
        "the successful preceding endpoint charged work" true
        (integer_program_report_switch_work report > 0);
      Alcotest.(check string)
        "an unused function produces no output at the failed endpoint" ""
        (integer_program_report_output_bytes report))
    Cases.jit_effectful_rejections

let check_aot_effectful_rejection () =
  List.iter
    (fun (case : Cases.value) ->
      let report = run ~mode:Preprocessor.Aot case.source in
      Alcotest.(check string)
        (case.label ^ " lacks ordinary AOT source-image preparation")
        "HCRUN0006" (first_error report).code)
    Cases.jit_effectful_values

let check_jit_switch_preparation_limit () =
  let case = List.hd Cases.jit_effectful_values in
  let baseline = run ~mode:Preprocessor.Jit case.source in
  let work = integer_program_report_switch_work baseline in
  Alcotest.(check bool)
    "effectful preparation consumes real work" true (work > 1);
  (match integer_program_report_outcome baseline with
  | Ok checked -> (
      match VM.final_value checked.value with
      | Some word ->
          Alcotest.(check int64)
            "unlimited effectful result" case.expected word.bits
      | None -> Alcotest.fail "effectful preparation lost the final word")
  | Error errors -> Alcotest.fail (describe errors));
  let exact = run ~max_switch_work:work ~mode:Preprocessor.Jit case.source in
  Alcotest.(check int)
    "exact preparation work" work
    (integer_program_report_switch_work exact);
  (match integer_program_report_outcome exact with
  | Error errors -> Alcotest.fail (describe errors)
  | Ok checked -> (
      match VM.final_value checked.value with
      | Some word ->
          Alcotest.(check int64)
            "exact preparation result" case.expected word.bits
      | None -> Alcotest.fail "exact-work run lost its final word"));
  let one_below =
    run ~max_switch_work:(work - 1) ~mode:Preprocessor.Jit case.source
  in
  Alcotest.(check string)
    "one below preparation rejects" "HCSW0003" (first_error one_below).code;
  Alcotest.(check int)
    "failed preparation retains reached work" (work - 1)
    (integer_program_report_switch_work one_below)

let check_nested_result label ~output ~value report =
  let checked =
    match integer_program_report_outcome report with
    | Ok checked -> checked
    | Error errors -> Alcotest.fail (label ^ ": " ^ describe errors)
  in
  Alcotest.(check string)
    (label ^ " output") output
    (integer_program_report_output_bytes report);
  Alcotest.(check (option int64))
    (label ^ " result") (Some value)
    (Option.map
       (fun (word : VM.word) -> word.bits)
       (VM.final_value checked.value))

let check_nested_source_work mode () =
  let source = Cases.nested_source_switch () in
  let baseline = run ~mode source in
  check_nested_result "nested case endpoints" ~output:"" ~value:42L baseline;
  let work = integer_program_report_switch_work baseline in
  (match mode with
  | Preprocessor.Jit ->
      Alcotest.(check int)
        "JIT child runtime work is counted once, including closed case 2+3" 37
        work
  | Preprocessor.Aot ->
      Alcotest.(check int)
        "AOT #exe child work includes source-image preparation" 43 work);
  let closed_literal = run ~mode (Cases.nested_source_switch ~first_case:"5" ())
  and runtime_literal =
    run ~mode (Cases.nested_source_switch ~second_case:"6" ())
  in
  check_nested_result "closed literal control" ~output:"" ~value:42L
    closed_literal;
  check_nested_result "runtime literal control" ~output:"" ~value:(-1L)
    runtime_literal;
  Alcotest.(check int)
    "closed 2+3 visits both operands and the binary expression" 2
    (work - integer_program_report_switch_work closed_literal);
  Alcotest.(check int)
    "runtime ++N requires four reached sites versus one closed literal" 3
    (work - integer_program_report_switch_work runtime_literal);
  let exact = run ~max_switch_work:work ~mode source in
  check_nested_result "nested exact-work execution" ~output:"" ~value:42L exact;
  Alcotest.(check int)
    "exact-work ledger" work
    (integer_program_report_switch_work exact);
  let short = run ~max_switch_work:(work - 1) ~mode source in
  Alcotest.(check string)
    "nested one-below quota" "HCSW0003" (first_error short).code;
  Alcotest.(check int)
    "one-below retains distinct work" (work - 1)
    (integer_program_report_switch_work short)

let check_nested_child_boundaries mode () =
  let source = Cases.nested_source_switch ~markers:true () in
  let baseline = run ~mode source in
  check_nested_result "nested source with ordered side effects"
    ~output:"entered;before;after;" ~value:42L baseline;
  let full_work = integer_program_report_switch_work baseline in
  (* Find the first quota that reaches each externally visible child command.
     This derives the boundaries from the source's observable effects, so the
     test also covers changes in native/IR setup overhead. *)
  let first_visible prefix =
    let rec search lower upper =
      if upper - lower = 1 then upper
      else
        let middle = lower + ((upper - lower) / 2) in
        let report = run ~max_switch_work:middle ~mode source in
        if
          String.starts_with ~prefix
            (integer_program_report_output_bytes report)
        then search lower middle
        else search middle upper
    in
    search 0 full_work
  in
  let entered = first_visible "entered;" in
  let before = first_visible "entered;before;" in
  let after = first_visible "entered;before;after;" in
  Alcotest.(check bool)
    "the parent has already charged work before the first child command" true
    (entered > 1);
  Alcotest.(check bool)
    "child declarations separate the first command from its switch" true
    (before > entered);
  Alcotest.(check bool)
    "closed case 2+3 and runtime ++N charge work between child commands" true
    (after - before >= 4);
  let exhaust label budget output =
    let report = run ~max_switch_work:budget ~mode source in
    Alcotest.(check string)
      (label ^ " has a switch-work fault")
      "HCSW0003" (first_error report).code;
    Alcotest.(check int)
      (label ^ " retains reached work")
      budget
      (integer_program_report_switch_work report);
    Alcotest.(check string)
      (label ^ " stops before later child commands")
      output
      (integer_program_report_output_bytes report)
  in
  exhaust "no work for first child command" (entered - 1) "";
  exhaust "early child command" (before - 1) "entered;";
  exhaust "middle of nested case preparation"
    (before + ((after - before) / 2))
    "entered;before;";
  exhaust "just before the later child command" (after - 1) "entered;before;"

let check_empty_prefix_continuations mode () =
  List.iter2
    (fun (case : Cases.value) capacity ->
      let report = run ~mode case.source in
      let checked =
        match integer_program_report_outcome report with
        | Ok checked -> checked
        | Error errors -> Alcotest.fail (describe errors)
      in
      Alcotest.(check (option int64))
        case.label (Some case.expected)
        (Option.map
           (fun (word : VM.word) -> word.bits)
           (VM.final_value checked.value));
      let program =
        match integer_program_report_program report with
        | Some program -> program
        | None -> Alcotest.fail "closed switch lost its checked source program"
      in
      let fn =
        match integer_program_functions program with
        | [ fn ] -> fn
        | _ -> Alcotest.fail "expected one original source function"
      in
      let graph = fn.body |> Ir_function_body.x87 |> Ir_x87_stack.graph in
      Alcotest.(check int)
        (case.label ^ " original continuation capacity")
        capacity
        (Graph.local_subroutine_capacity graph);
      let has_local_call =
        Graph.blocks graph
        |> List.exists (fun block ->
            Graph.instructions block |> Sequence.instructions
            |> List.exists (fun instruction ->
                (Sequence.description instruction).opcode
                = Ir_opcode.Ic_sub_call))
      in
      Alcotest.(check bool)
        (case.label ^ " emits local call only for an actual prefix IC")
        (capacity > 0) has_local_call)
    Cases.empty_prefix_values [ 0; 0; 0; 1 ]

let labeled mode label = mode_name mode ^ ": " ^ label

let successes rows =
  List.concat_map
    (fun mode ->
      List.map
        (fun (case : Cases.value) ->
          Alcotest.test_case (labeled mode case.label) `Quick
            (check_value mode case))
        rows)
    modes

let failures check cases =
  List.concat_map
    (fun mode ->
      List.map
        (fun (case : Cases.rejection) ->
          Alcotest.test_case (labeled mode case.label) `Quick (check mode case))
        cases)
    modes

let tests =
  successes Cases.bracket_values
  @ successes Cases.repeated_default_values
  @ successes Cases.subswitch_values
  @ successes Cases.nested_control_values
  @ successes Cases.goto_values
  @ failures check_parse_rejection Cases.parser_rejections
  @ failures check_runtime_rejection Cases.runtime_rejections
  @ failures check_goto_rejection Cases.goto_rejections
  @ [
      Alcotest.test_case "JIT effectful cases run at original source callbacks"
        `Quick check_jit_effectful_values;
      Alcotest.test_case "JIT effectful cases stop on the original failure"
        `Quick check_jit_effectful_failure;
      Alcotest.test_case "ordinary AOT effectful cases need a source image"
        `Quick check_aot_effectful_rejection;
      Alcotest.test_case "JIT effectful case preparation work is exact" `Quick
        check_jit_switch_preparation_limit;
    ]
  @ List.concat_map
      (fun mode ->
        [
          Alcotest.test_case
            (labeled mode "empty setup creates no local continuation frame")
            `Quick
            (check_empty_prefix_continuations mode);
          Alcotest.test_case
            (labeled mode "nested #exe switch exact and one-below work")
            `Quick
            (check_nested_source_work mode);
          Alcotest.test_case
            (labeled mode "nested child source stops at reached quotas")
            `Quick
            (check_nested_child_boundaries mode);
        ])
      modes
