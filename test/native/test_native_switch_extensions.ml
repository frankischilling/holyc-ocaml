open Holyc_lib
module Cases = Switch_extension_cases
module Native = Native_program
module Native_task = Native_source_execution
module Program = X86_64_program
module VM = Ir_integer_interpreter

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let mode_name = function
  | Preprocessor.Jit -> "JIT"
  | Preprocessor.Aot -> "AOT"

let describe diagnostics =
  diagnostics
  |> List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
  |> String.concat "; "

let inputs mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-switch-extensions.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
  in
  (session, config, source)

let interpreted ?max_switch_work ~mode contents =
  let session, config, source = inputs mode contents in
  run_integer_program_report ?max_switch_work session ~config ~source
    ~max_steps:100_000

let native ?max_switch_work ~mode contents =
  let session, config, source = inputs mode contents in
  Native.evaluate ?max_switch_work session ~config ~source ~max_steps:100_000

let native_task ?max_switch_work ?max_initializer_steps ~mode contents =
  let session, config, source = inputs mode contents in
  Native_task.evaluate ?max_switch_work ?max_initializer_steps
    ~max_code_bytes:524_288 session ~config ~source ~max_steps:100_000

let native_error report =
  match Native.outcome report with
  | Ok _ -> Alcotest.fail "invalid native switch unexpectedly executed"
  | Error [] -> Alcotest.fail "native switch failure returned no diagnostics"
  | Error (first :: _) -> first

let interpreted_error report =
  match integer_program_report_outcome report with
  | Ok _ -> Alcotest.fail "invalid interpreted switch unexpectedly executed"
  | Error [] -> Alcotest.fail "interpreted switch failure has no diagnostics"
  | Error (first :: _) -> first

let task_error report =
  match Native_task.outcome report with
  | Ok _ -> Alcotest.fail "invalid native source task unexpectedly executed"
  | Error [] -> Alcotest.fail "native source task returned no diagnostics"
  | Error (first :: _) -> first

let check_word label expected = function
  | Some (word : Program.word) ->
      Alcotest.(check bool)
        (label ^ " native result is I64")
        true (word.type_ = Program.I64);
      Alcotest.(check int64) (label ^ " native result") expected word.bits
  | None -> Alcotest.fail (label ^ " native execution has no final word")

let check_value mode (case : Cases.value) () =
  let ir = interpreted ~mode case.source in
  let ir_checked =
    match integer_program_report_outcome ir with
    | Ok checked -> checked
    | Error errors -> Alcotest.fail (describe errors)
  in
  Alcotest.(check bool)
    (case.label ^ " interpreter completes")
    true
    (VM.termination ir_checked.value = VM.Stream_end);
  (match VM.final_value ir_checked.value with
  | Some word ->
      Alcotest.(check bool)
        (case.label ^ " interpreter result is I64")
        true (word.type_ = VM.I64);
      Alcotest.(check int64)
        (case.label ^ " interpreter result")
        case.expected word.bits
  | None -> Alcotest.fail (case.label ^ " interpreter has no result"));
  let native_report = native ~mode case.source in
  let result =
    match Native.outcome native_report with
    | Ok checked -> checked.value
    | Error errors -> Alcotest.fail (describe errors)
  in
  Alcotest.(check bool)
    (case.label ^ " has a real native image")
    true
    (Option.is_some (Native.image native_report));
  check_word case.label case.expected result.execution.final_value;
  Alcotest.(check int)
    (case.label ^ " native/interpreted reached steps")
    (VM.executed_steps ir_checked.value)
    result.execution.executed_steps;
  Alcotest.(check string)
    (case.label ^ " native has no output")
    ""
    (Native.output_bytes native_report)

let check_parse_rejection mode (case : Cases.rejection) () =
  let ir = interpreted ~mode case.source
  and report = native ~mode case.source in
  Alcotest.(check string)
    (case.label ^ " interpreted parse error")
    case.code (interpreted_error ir).code;
  Alcotest.(check string)
    (case.label ^ " native parse error")
    case.code (native_error report).code;
  Alcotest.(check int)
    "the later case value consumes no interpreted work" 0
    (integer_program_report_switch_work ir);
  Alcotest.(check int)
    "the later case value consumes no native work" 0
    (Native.switch_work report);
  Alcotest.(check bool)
    "parse rejection never emits executable code" true
    (Option.is_none (Native.image report));
  Alcotest.(check (option int))
    "parse rejection has no execution" None
    (Native.executed_steps report)

let check_runtime_rejection mode (case : Cases.rejection) () =
  let ir = interpreted ~mode case.source
  and report = native ~mode case.source in
  Alcotest.(check string)
    (case.label ^ " interpreted runtime fault")
    case.code (interpreted_error ir).code;
  Alcotest.(check string)
    (case.label ^ " native runtime fault")
    case.code (native_error report).code;
  Alcotest.(check bool)
    "interpreted case table is prepared" true
    (integer_program_report_switch_work ir > 0);
  Alcotest.(check bool)
    "native case table is prepared" true
    (Native.switch_work report > 0);
  Alcotest.(check bool)
    "fault occurs after native code generation" true
    (Option.is_some (Native.image report));
  match Native.native_outcome report with
  | Some (Program.Fault _) -> ()
  | Some (Program.Completed _) ->
      Alcotest.fail "out-of-table native dispatch completed"
  | None -> Alcotest.fail "out-of-table dispatch has no native fault"

let check_goto_rejection mode (case : Cases.rejection) () =
  let ir = interpreted ~mode case.source
  and report = native ~mode case.source in
  Alcotest.(check string)
    (case.label ^ " interpreted goto boundary")
    case.code (interpreted_error ir).code;
  Alcotest.(check string)
    (case.label ^ " native goto boundary")
    case.code (native_error report).code;
  Alcotest.(check bool)
    "interpreted cases prepare before goto lowering" true
    (integer_program_report_switch_work ir > 0);
  Alcotest.(check bool)
    "native cases prepare before goto lowering" true
    (Native.switch_work report > 0);
  Alcotest.(check bool)
    "invalid goto cannot produce a native image" true
    (Option.is_none (Native.image report));
  Alcotest.(check (option int))
    "invalid goto does not reach native execution" None
    (Native.executed_steps report)

let check_jit_effectful_value (case : Cases.value) () =
  let ir = interpreted ~mode:Preprocessor.Jit case.source
  and report = native_task ~mode:Preprocessor.Jit case.source in
  (match integer_program_report_outcome ir with
  | Error errors -> Alcotest.fail (describe errors)
  | Ok checked -> (
      match VM.final_value checked.value with
      | Some word ->
          Alcotest.(check bool)
            "effectful IR final type is I64" true (word.type_ = VM.I64);
          Alcotest.(check int64)
            (case.label ^ " interpreter result")
            case.expected word.bits
      | None -> Alcotest.fail "effectful IR source produced no final value"));
  (match Native_task.outcome report with
  | Error errors -> Alcotest.fail (describe errors)
  | Ok checked -> (
      match checked.value.final_value with
      | Some word ->
          Alcotest.(check bool)
            "effectful native type is I64" true (word.type_ = Program.I64);
          Alcotest.(check int64)
            (case.label ^ " actual native task result")
            case.expected word.bits
      | None -> Alcotest.fail "native source task produced no final word"));
  Alcotest.(check string)
    "effectful source has the same observable output"
    (integer_program_report_output_bytes ir)
    (Native_task.output_bytes report);
  Alcotest.(check bool)
    "original case expressions produced native switch-case fragments" true
    (List.exists
       (fun (fragment : Native_task.fragment) ->
         fragment.kind = Native_task.Switch_case
         &&
         match fragment.native_outcome with
         | Some (Ok (Program.Completed _)) -> true
         | _ -> false)
       (Native_task.fragments report))

let check_jit_effectful_failure (case : Cases.rejection) () =
  let ir = interpreted ~mode:Preprocessor.Jit case.source
  and report = native_task ~mode:Preprocessor.Jit case.source in
  Alcotest.(check string)
    (case.label ^ " interpreted failure")
    case.code (interpreted_error ir).code;
  Alcotest.(check string)
    (case.label ^ " native failure")
    case.code (task_error report).code;
  Alcotest.(check bool)
    "earlier source endpoint charged work in the interpreter" true
    (integer_program_report_switch_work ir > 0);
  Alcotest.(check bool)
    "earlier source endpoint charged native preparation" true
    (Native_task.switch_work report > 0);
  Alcotest.(check bool)
    "the preceding successful case executed native source work" true
    (List.exists
       (fun (fragment : Native_task.fragment) ->
         fragment.kind = Native_task.Switch_case)
       (Native_task.fragments report))

let check_aot_effectful_rejection (case : Cases.value) () =
  let ir = interpreted ~mode:Preprocessor.Aot case.source
  and report = native_task ~mode:Preprocessor.Aot case.source in
  Alcotest.(check string)
    (case.label ^ " ordinary AOT interpreted admission")
    "HCRUN0006" (interpreted_error ir).code;
  Alcotest.(check string)
    (case.label ^ " ordinary AOT native admission")
    "HCRUN0006" (task_error report).code;
  Alcotest.(check bool)
    "rejected ordinary AOT module never runs an AOT image" true
    (not
       (List.exists
          (fun (fragment : Native_task.fragment) ->
            fragment.kind = Native_task.Aot_module)
          (Native_task.fragments report)))

let jit_effectful_work_limit () =
  let case = List.hd Cases.jit_effectful_values in
  let source = case.source in
  let baseline_ir = interpreted ~mode:Preprocessor.Jit source
  and baseline_native = native_task ~mode:Preprocessor.Jit source in
  (match integer_program_report_outcome baseline_ir with
  | Error errors -> Alcotest.fail (describe errors)
  | Ok checked -> (
      match VM.final_value checked.value with
      | Some word ->
          Alcotest.(check int64)
            "baseline interpreted effectful result" case.expected word.bits
      | None -> Alcotest.fail "interpreter lost effectful result"));
  (match Native_task.outcome baseline_native with
  | Error errors -> Alcotest.fail (describe errors)
  | Ok checked -> (
      match checked.value.final_value with
      | Some word ->
          Alcotest.(check int64)
            "baseline source-task native effectful result" case.expected
            word.bits
      | None -> Alcotest.fail "baseline native source task lost its final word"));
  let ir_work = integer_program_report_switch_work baseline_ir in
  let native_work = Native_task.switch_work baseline_native in
  Alcotest.(check bool) "source work is nontrivial for IR" true (ir_work > 1);
  Alcotest.(check bool)
    "source work is nontrivial for native" true (native_work > 1);
  let ir_exact =
    interpreted ~max_switch_work:ir_work ~mode:Preprocessor.Jit source
  and native_exact =
    native_task ~max_switch_work:native_work ~mode:Preprocessor.Jit source
  in
  Alcotest.(check int)
    "exact IR endpoint work" ir_work
    (integer_program_report_switch_work ir_exact);
  Alcotest.(check int)
    "exact native endpoint work" native_work
    (Native_task.switch_work native_exact);
  (match integer_program_report_outcome ir_exact with
  | Error errors -> Alcotest.fail (describe errors)
  | Ok checked -> (
      match VM.final_value checked.value with
      | Some word ->
          Alcotest.(check int64) "exact IR result" case.expected word.bits
      | None -> Alcotest.fail "IR exact-work run lost its result"));
  (match Native_task.outcome native_exact with
  | Error errors -> Alcotest.fail (describe errors)
  | Ok checked -> (
      match checked.value.final_value with
      | Some word ->
          Alcotest.(check int64) "exact native result" case.expected word.bits
      | None -> Alcotest.fail "exact-work native source task lost its result"));
  let ir_below =
    interpreted ~max_switch_work:(ir_work - 1) ~mode:Preprocessor.Jit source
  and native_below =
    native_task ~max_switch_work:(native_work - 1) ~mode:Preprocessor.Jit source
  in
  Alcotest.(check string)
    "IR budget one below" "HCSW0003" (interpreted_error ir_below).code;
  Alcotest.(check string)
    "native budget one below" "HCSW0003" (task_error native_below).code;
  Alcotest.(check int)
    "IR exhaustion keeps reached work" (ir_work - 1)
    (integer_program_report_switch_work ir_below);
  Alcotest.(check int)
    "native exhaustion keeps reached work" (native_work - 1)
    (Native_task.switch_work native_below);
  Alcotest.(check bool)
    "failed endpoint prevents the final resumed command" true
    (match List.rev (Native_task.fragments native_below) with
    | {
        kind = Native_task.Switch_case;
        native_outcome = Some (Ok (Program.Fault _));
        _;
      }
      :: _ -> true
    | _ -> false)

let check_nested_task_result label ~output ~value report =
  let checked =
    match Native_task.outcome report with
    | Ok checked -> checked
    | Error errors -> Alcotest.fail (label ^ ": " ^ describe errors)
  in
  Alcotest.(check string)
    (label ^ " output") output
    (Native_task.output_bytes report);
  Alcotest.(check (option int64))
    (label ^ " source result") (Some value)
    (Option.map
       (fun (word : Native_task.word) -> word.bits)
       checked.value.final_value)

let native_nested_source_work mode () =
  let source = Cases.nested_source_switch () in
  let baseline = native_task ~mode source in
  check_nested_task_result "nested native source" ~output:"" ~value:42L baseline;
  let work = Native_task.switch_work baseline in
  Alcotest.(check int)
    "nested native callback charges child preparation without replaying ++N" 45
    work;
  Alcotest.(check bool)
    "source case expressions run in native fragments" true
    (List.exists
       (fun (fragment : Native_task.fragment) ->
         fragment.kind = Native_task.Switch_case
         &&
         match fragment.native_outcome with
         | Some (Ok (Program.Completed _)) -> true
         | _ -> false)
       (Native_task.fragments baseline));
  let closed_literal =
    native_task ~mode (Cases.nested_source_switch ~first_case:"5" ())
  and runtime_literal =
    native_task ~mode (Cases.nested_source_switch ~second_case:"6" ())
  in
  check_nested_task_result "native closed literal control" ~output:"" ~value:42L
    closed_literal;
  check_nested_task_result "native runtime literal control" ~output:""
    ~value:(-1L) runtime_literal;
  Alcotest.(check int)
    "native child 2+3 visits both operands and the operator" 2
    (work - Native_task.switch_work closed_literal);
  Alcotest.(check int)
    "native ++N executes four sites versus one closed literal" 3
    (work - Native_task.switch_work runtime_literal);
  let exact = native_task ~max_switch_work:work ~mode source in
  check_nested_task_result "nested exact native quota" ~output:"" ~value:42L
    exact;
  Alcotest.(check int)
    "exact native nested ledger" work
    (Native_task.switch_work exact);
  let short = native_task ~max_switch_work:(work - 1) ~mode source in
  Alcotest.(check string)
    "native nested one-below quota" "HCSW0003" (task_error short).code;
  Alcotest.(check int)
    "native one-below retains reached work" (work - 1)
    (Native_task.switch_work short)

let native_nested_initializer_limit mode () =
  let source = Cases.nested_source_switch () in
  let baseline = native_task ~mode source in
  check_nested_task_result "nested native initializer baseline" ~output:""
    ~value:42L baseline;
  let initializer_work = Native_task.preparation_steps baseline in
  let switch_work = Native_task.switch_work baseline in
  Alcotest.(check int)
    "parent 42 steps, child initializers 6, child scalar case 4" 52
    initializer_work;
  Alcotest.(check bool)
    "nested source has separate initializer and switch work" true
    (initializer_work > switch_work && switch_work > 1);
  let last_switch_outcome report =
    match
      List.find_opt
        (fun (fragment : Native_task.fragment) ->
          fragment.kind = Native_task.Switch_case)
        (List.rev (Native_task.fragments report))
    with
    | Some { native_outcome = Some (Ok outcome); _ } -> outcome
    | _ -> Alcotest.fail "original parent switch case has no native outcome"
  in
  let parent_steps =
    match last_switch_outcome baseline with
    | Program.Completed completed -> completed.executed_steps
    | Program.Fault _ -> Alcotest.fail "baseline parent switch did not complete"
  in
  let exact =
    native_task ~mode ~max_switch_work:switch_work
      ~max_initializer_steps:initializer_work source
  in
  check_nested_task_result "exact native initializer preparation" ~output:""
    ~value:42L exact;
  Alcotest.(check int)
    "exact initializer allowance accounts for nested preparation"
    initializer_work
    (Native_task.preparation_steps exact);
  Alcotest.(check int)
    "exact initializer allowance retains independently counted switch work"
    switch_work
    (Native_task.switch_work exact);
  let one_below = initializer_work - 1 in
  let short =
    native_task ~mode ~max_switch_work:switch_work
      ~max_initializer_steps:one_below source
  in
  Alcotest.(check string)
    "initializer quota tighter than switch reports an initializer fault"
    "HCIRVM0007" (task_error short).code;
  Alcotest.(check int)
    "reached nested initializer work stops at its exact quota" one_below
    (Native_task.preparation_steps short);
  Alcotest.(check bool)
    "initializer quota fails with available switch work" true
    (Native_task.switch_work short < switch_work);
  (match last_switch_outcome short with
  | Program.Fault fault ->
      Alcotest.(check bool)
        "physical parent faults on its instruction quota" true
        (fault.kind = Program.Step_limit_exceeded);
      Alcotest.(check bool)
        "physical parent stops before the complete case" true
        (fault.executed_steps < parent_steps);
      Alcotest.(check int)
        "fault records actual native progress without extra instructions"
        fault.executed_steps
        (Native_task.executed_steps short)
  | Program.Completed _ ->
      Alcotest.fail "one-below initializer executed the full parent case");
  List.iter
    (fun early_limit ->
      let early =
        native_task ~mode ~max_switch_work:100_000
          ~max_initializer_steps:early_limit source
      in
      Alcotest.(check string)
        (Printf.sprintf "early initializer limit %d reports its own fault"
           early_limit)
        "HCIRVM0007" (task_error early).code;
      Alcotest.(check bool)
        (Printf.sprintf "early initializer limit %d never overruns paid work"
           early_limit)
        true
        (Native_task.preparation_steps early <= early_limit);
      Alcotest.(check bool)
        (Printf.sprintf "early initializer limit %d stops actual native work"
           early_limit)
        true
        (Native_task.executed_steps early < Native_task.executed_steps baseline);
      Alcotest.(check bool)
        (Printf.sprintf "early initializer limit %d is not switch exhaustion"
           early_limit)
        true
        (Native_task.switch_work early < switch_work))
    [ 20; 30 ];
  for limit = 1 to initializer_work - 1 do
    let report =
      native_task ~mode ~max_switch_work:100_000 ~max_initializer_steps:limit
        source
    in
    let errors =
      match Native_task.outcome report with
      | Ok _ ->
          Alcotest.fail
            (Printf.sprintf "initializer cap %d unexpectedly completed" limit)
      | Error errors -> errors
    in
    Alcotest.(check bool)
      (Printf.sprintf "initializer cap %d retains a real diagnostic" limit)
      true (errors <> []);
    Alcotest.(check bool)
      (Printf.sprintf "initializer cap %d has no late generic or internal fault"
         limit)
      true
      (List.for_all
         (fun (error : Diagnostic.t) ->
           error.code <> "HCEVAL0003" && error.code <> "HCIRVM0026"
           && not
                (String.starts_with
                   ~prefix:"native switch case work has another attempt"
                   error.message))
         errors);
    Alcotest.(check bool)
      (Printf.sprintf "initializer cap %d never overcharges preparation" limit)
      true
      (Native_task.preparation_steps report <= limit);
    Alcotest.(check bool)
      (Printf.sprintf "initializer cap %d cannot exceed native baseline steps"
         limit)
      true
      (Native_task.executed_steps report <= Native_task.executed_steps baseline)
  done;
  let marked_source = Cases.nested_source_switch ~markers:true () in
  let marked_baseline = native_task ~mode marked_source in
  check_nested_task_result "marked initializer quota baseline"
    ~output:"entered;before;after;" ~value:42L marked_baseline;
  let marked_output = Native_task.output_bytes marked_baseline in
  let early_output = ref None in
  List.iter
    (fun limit ->
      let report =
        native_task ~mode ~max_switch_work:100_000 ~max_initializer_steps:limit
          marked_source
      in
      Alcotest.(check string)
        (Printf.sprintf "marked early initializer cap %d reports its fault"
           limit)
        "HCIRVM0007" (task_error report).code;
      let output = Native_task.output_bytes report in
      Alcotest.(check bool)
        (Printf.sprintf
           "marked early initializer cap %d publishes no future effects" limit)
        true
        (String.starts_with ~prefix:output marked_output);
      Option.iter
        (fun previous ->
          Alcotest.(check bool)
            "larger initializer allowance never retracts reached child effects"
            true
            (String.starts_with ~prefix:previous output))
        !early_output;
      early_output := Some output)
    [ 20; 30 ]

let native_nested_child_boundaries mode () =
  let source = Cases.nested_source_switch ~markers:true () in
  let baseline = native_task ~mode source in
  check_nested_task_result "native child effects"
    ~output:"entered;before;after;" ~value:42L baseline;
  let full_work = Native_task.switch_work baseline in
  let first_visible prefix =
    let rec search lower upper =
      if upper - lower = 1 then upper
      else
        let middle = lower + ((upper - lower) / 2) in
        let report = native_task ~max_switch_work:middle ~mode source in
        if String.starts_with ~prefix (Native_task.output_bytes report) then
          search lower middle
        else search middle upper
    in
    search 0 full_work
  in
  let entered = first_visible "entered;" in
  let before = first_visible "entered;before;" in
  let after = first_visible "entered;before;after;" in
  Alcotest.(check bool)
    "the native parent charged preparation before the first child command" true
    (entered > 1);
  Alcotest.(check bool)
    "native child declarations precede its switch" true (before > entered);
  Alcotest.(check bool)
    "native closed and runtime child case expressions separate the effects" true
    (after - before >= 4);
  let exhaust label budget output =
    let report = native_task ~max_switch_work:budget ~mode source in
    Alcotest.(check string)
      (label ^ " raises a native switch-work fault")
      "HCSW0003" (task_error report).code;
    Alcotest.(check int)
      (label ^ " native reached work")
      budget
      (Native_task.switch_work report);
    Alcotest.(check string)
      (label ^ " has no later native child effects")
      output
      (Native_task.output_bytes report)
  in
  exhaust "zero remaining before child command" (entered - 1) "";
  exhaust "early child native command" (before - 1) "entered;";
  exhaust "mid-child native switch preparation"
    (before + ((after - before) / 2))
    "entered;before;";
  exhaust "before later child native command" (after - 1) "entered;before;"

let value_tests rows =
  List.concat_map
    (fun mode ->
      List.map
        (fun (case : Cases.value) ->
          Alcotest.test_case
            (mode_name mode ^ ": " ^ case.label)
            `Quick (check_value mode case))
        rows)
    modes

let error_tests check rows =
  List.concat_map
    (fun mode ->
      List.map
        (fun (case : Cases.rejection) ->
          Alcotest.test_case
            (mode_name mode ^ ": " ^ case.label)
            `Quick (check mode case))
        rows)
    modes

let () =
  match Native_program_execution.platform () with
  | Native_program_execution.Unsupported ->
      Alcotest.fail "native switch extension tests require x86-64"
  | Native_program_execution.Windows_x86_64
  | Native_program_execution.Linux_x86_64 ->
      Alcotest.run "holyc native switch extensions"
        [
          ("bracket dispatch", value_tests Cases.bracket_values);
          ("repeated defaults", value_tests Cases.repeated_default_values);
          ("sub-switch regions", value_tests Cases.subswitch_values);
          ("nested control", value_tests Cases.nested_control_values);
          ("goto within and across setup regions", value_tests Cases.goto_values);
          ( "front-porch parser errors",
            error_tests check_parse_rejection Cases.parser_rejections );
          ( "out-of-table dispatch faults",
            error_tests check_runtime_rejection Cases.runtime_rejections );
          ( "goto setup-boundary rejections",
            error_tests check_goto_rejection Cases.goto_rejections );
          ( "effectful case source callbacks",
            List.map
              (fun (case : Cases.value) ->
                Alcotest.test_case case.label `Quick
                  (check_jit_effectful_value case))
              Cases.jit_effectful_values
            @ List.map
                (fun (case : Cases.rejection) ->
                  Alcotest.test_case case.label `Quick
                    (check_jit_effectful_failure case))
                Cases.jit_effectful_rejections
            @ List.map
                (fun (case : Cases.value) ->
                  Alcotest.test_case
                    ("AOT rejects " ^ case.label)
                    `Quick
                    (check_aot_effectful_rejection case))
                Cases.jit_effectful_values
            @ [
                Alcotest.test_case "exact and one-below preparation work" `Quick
                  jit_effectful_work_limit;
              ] );
          ( "nested source switch budgets",
            List.concat_map
              (fun mode ->
                [
                  Alcotest.test_case
                    (mode_name mode ^ ": nested native source exact work")
                    `Quick
                    (native_nested_source_work mode);
                  Alcotest.test_case
                    (mode_name mode ^ ": nested initializer exact and one-below")
                    `Quick
                    (native_nested_initializer_limit mode);
                  Alcotest.test_case
                    (mode_name mode ^ ": child commands respect reached work")
                    `Quick
                    (native_nested_child_boundaries mode);
                ])
              modes );
        ]
