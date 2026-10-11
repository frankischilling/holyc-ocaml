open Holyc_lib
module VM = Ir_integer_interpreter
module Cases = Lock_execution_cases
module Task = Integer_task
module T = Test_integer_task
module Seq = Ir_instruction_sequence

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let mode_name = function
  | Preprocessor.Jit -> "JIT"
  | Preprocessor.Aot -> "AOT"

let diagnostics errors =
  errors
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let run ?(max_steps = 100_000) ?max_frame_bytes ?max_global_bytes
    ?max_initializer_steps ~mode contents =
  let session = Session.create () in
  let source = Session.add_source session ~path:"lock-execution.hc" ~contents in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
  in
  run_integer_program_report ~max_steps ?max_frame_bytes ?max_global_bytes
    ?max_initializer_steps session ~config ~source

let first_error label report =
  match integer_program_report_outcome report with
  | Ok _ -> Alcotest.fail (label ^ " unexpectedly executed")
  | Error errors -> (
      match
        List.find_opt
          (fun (d : Diagnostic.t) -> d.severity = Diagnostic.Error)
          errors
      with
      | Some error -> error
      | None -> Alcotest.fail (label ^ " failed without an error diagnostic"))

let expect (case : Cases.success) report =
  let result =
    match integer_program_report_outcome report with
    | Ok checked -> checked.value
    | Error errors -> Alcotest.fail (case.name ^ ": " ^ diagnostics errors)
  in
  Alcotest.(check bool)
    (case.name ^ " reaches stream end")
    true
    (VM.termination result = VM.Stream_end);
  (match VM.final_value result with
  | Some word ->
      Alcotest.(check bool)
        (case.name ^ " result is I64")
        true (word.VM.type_ = VM.I64);
      Alcotest.(check int64) (case.name ^ " bits") case.value word.bits
  | None -> Alcotest.fail (case.name ^ " has no final value"));
  Alcotest.(check string)
    (case.name ^ " output") case.output
    (integer_program_report_output_bytes report);
  result

let source_tests =
  List.concat_map
    (fun mode ->
      List.map
        (fun (case : Cases.success) ->
          Alcotest.test_case
            (mode_name mode ^ ": " ^ case.name)
            `Quick
            (fun () -> ignore (run ~mode case.program |> expect case)))
        Cases.success_cases)
    modes

let invalid_before_execution () =
  List.iter
    (fun mode ->
      List.iter
        (fun (case : Cases.failure) ->
          let report = run ~mode case.program in
          Alcotest.(check string)
            case.name case.code (first_error case.name report).code;
          Alcotest.(check string)
            (case.name ^ " rejects before the function's first effect")
            ""
            (integer_program_report_output_bytes report))
        Cases.invalid_cases;
      List.iter
        (fun source ->
          let report = run ~mode source in
          ignore (first_error "locked numeric bit signature" report);
          Alcotest.(check string)
            "invalid numeric binding executes no function output" ""
            (integer_program_report_output_bytes report))
        Cases.invalid_bit_signatures)
    modes

let checked_faults () =
  List.iter
    (fun mode ->
      List.iter
        (fun (case : Cases.failure) ->
          let report = run ~mode case.program in
          Alcotest.(check string)
            case.name case.code (first_error case.name report).code;
          Alcotest.(check string)
            (case.name ^ " retains reached effects")
            case.output
            (integer_program_report_output_bytes report))
        Cases.fault_cases)
    modes

let exact_runtime_quota () =
  let expected = Cases.successful "lock quota" Cases.quota_source 42L in
  List.iter
    (fun mode ->
      let baseline = run ~mode Cases.quota_source |> expect expected in
      let steps = VM.executed_steps baseline in
      Alcotest.(check bool) "lock loop consumes runtime work" true (steps > 42);
      let exact =
        run ~mode ~max_steps:steps Cases.quota_source |> expect expected
      in
      Alcotest.(check int)
        "exact lock step allowance" steps (VM.executed_steps exact);
      let short = run ~mode ~max_steps:(steps - 1) Cases.quota_source in
      Alcotest.(check string)
        "lock does not bypass the runtime allowance" "HCIRVM0007"
        (first_error "one-below lock quota" short).code;
      ignore (run ~mode Cases.quota_source |> expect expected))
    modes

let resource_boundaries () =
  let source = "I64 G=40;I64 F(){lock G+=2;return G;}F();" in
  let expected = Cases.successful "locked global quota" source 42L in
  List.iter
    (fun mode ->
      ignore (run ~mode ~max_global_bytes:8 source |> expect expected);
      Alcotest.(check string)
        "locked global still needs its complete owned cell" "HCIRVM0016"
        (first_error "locked global one byte short"
           (run ~mode ~max_global_bytes:7 source))
          .code;
      let local = "I64 F(){I64 n=40;lock n+=2;return n;}F();" in
      ignore
        (run ~mode ~max_frame_bytes:8 local
        |> expect (Cases.successful "locked local quota" local 42L));
      Alcotest.(check string)
        "locked local still needs its complete frame" "HCIRVM0011"
        (first_error "locked local one byte short"
           (run ~mode ~max_frame_bytes:7 local))
          .code)
    modes

let descriptions graph =
  graph |> Ir_x87_stack.graph |> Ir_block_graph.blocks
  |> List.concat_map (fun block ->
      Ir_block_graph.instructions block |> Seq.instructions)
  |> List.map Seq.description

let locked (d : Seq.description) = Int64.logand d.flags 0x10000000L <> 0L

let original_lexical_scope () =
  List.iter
    (fun mode ->
      let compiled =
        Test_integer_globals.compile ~mode
          "I64 G=0;I64 Callee(){G+=1;return G;}lock G+=Callee();G+=2;G;"
      in
      let updates code =
        List.filter
          (fun (d : Seq.description) -> d.opcode = Ir_opcode.Ic_add_equ)
          code
      in
      Alcotest.(check (list bool))
        "only the wrapped caller update carries ICF_LOCK" [ true; false ]
        (integer_program_entry compiled
        |> descriptions |> updates |> List.map locked);
      let callees =
        integer_program_functions compiled
        |> List.concat_map (fun (fn : VM.function_definition) ->
            Ir_function_body.x87 fn.body |> descriptions |> updates)
      in
      Alcotest.(check (list bool))
        "the called body retains its own lexical flags" [ false ]
        (List.map locked callees);
      let folded =
        Test_integer_globals.compile ~mode "I64 G=85;lock G%=8;G;"
        |> integer_program_entry |> descriptions
      in
      Alcotest.(check bool)
        "the original power-of-two %= rewrite produces locked AND_EQU" true
        (List.exists
           (fun (d : Seq.description) ->
             d.opcode = Ir_opcode.Ic_and_equ && locked d)
           folded))
    modes

let retained_source_tasks () =
  List.iter
    (fun (case : Cases.success) ->
      ignore (run ~mode:Preprocessor.Jit case.program |> expect case))
    Cases.retained_cases

let lock_exception_composition () =
  let program =
    Exception_execution_cases.headers
    ^ "I64 F(){I64 n=40;lock try {++n;throw('Lock');}catch \
       {++n;Fs->catch_except=TRUE;}return n;}F();"
  in
  let case = Cases.successful "lock try and catch" program 42L in
  List.iter (fun mode -> ignore (run ~mode program |> expect case)) modes

let retained_storage_and_failed_commands () =
  let session = Session.create () in
  let task = T.create session in
  ignore (T.run session task "I64 G=40;" |> Test_integer_program.checked);
  T.value 42L (T.run session task "lock G+=2;");
  T.value 42L (T.run session task "G;");
  T.fault "HCIRVM0009" (T.run session task "lock G++;1/0;");
  T.value 43L (T.run session task "G;");
  let before = Task.executed_steps task in
  T.fault "HCLOCK0001" (T.run session task "lock G=G+G;");
  Alcotest.(check int)
    "invalid scalar lock consumes no runtime work" before
    (Task.executed_steps task);
  T.value 43L (T.run session task "G;");
  T.value 42L (T.run session task "lock --G;")

let original_task_owner_and_single_entry () =
  let session = Session.create () in
  let task = T.create session and other = T.create session in
  ignore (T.run session task "I64 G=40;" |> Test_integer_program.checked);
  let command = T.compile session task "lock G+=2;" in
  T.fault "HCIRVM0026" (Task.execute other command);
  T.value 42L (Task.execute task command);
  let before = Task.executed_steps task in
  T.fault "HCIRVM0026" (Task.execute task command);
  Alcotest.(check int)
    "replaying a locked command performs no second mutation" before
    (Task.executed_steps task);
  T.value 42L (T.run session task "G;")

let tests =
  source_tests
  @ List.map
      (fun (name, test) -> Alcotest.test_case name `Quick test)
      [
        ( "invalid lock consumers reject before source effects",
          invalid_before_execution );
        ( "locked accesses retain bounds and initialization checks",
          checked_faults );
        ("exact and one-below runtime allowance", exact_runtime_quota);
        ( "complete global and frame storage remain required",
          resource_boundaries );
        ("original lock scope and optimizer rewrite", original_lexical_scope);
        ("original retained source callbacks", retained_source_tasks);
        ( "lock composes with supported source exception handlers",
          lock_exception_composition );
        ( "retained writes survive faults and rejected commands",
          retained_storage_and_failed_commands );
        ( "locked commands keep task owner and single-entry checks",
          original_task_owner_and_single_entry );
      ]
