open Holyc_lib
module Fixture = Test_native_program
module Program = X86_64_program
module Runtime = Native_program_execution
module VM = Ir_integer_interpreter
module Opcode = Ir_opcode

let require_ok show = function
  | Ok value -> value
  | Error error -> Alcotest.fail (show error)

let execute ?(max_steps = 1000) label image =
  Runtime.execute ~max_steps image
  |> require_ok (fun message -> label ^ ": native execution failed: " ^ message)

let word_type_name = function
  | Program.I64 -> "I64"
  | Program.U64 -> "U64"

let vm_type_name = function
  | VM.I64 -> "I64"
  | VM.U64 -> "U64"

let check_program_word label expected_type expected_bits = function
  | None -> Alcotest.failf "%s: missing final word" label
  | Some (word : Program.word) ->
      Alcotest.(check string)
        (label ^ " type") expected_type
        (word_type_name word.type_);
      Alcotest.(check int64) (label ^ " bits") expected_bits word.bits

let completed label = function
  | Program.Completed execution -> execution
  | Program.Fault fault ->
      Alcotest.failf "%s: unexpected native fault after %d steps" label
        fault.executed_steps

let faulted label = function
  | Program.Fault fault -> fault
  | Program.Completed execution ->
      Alcotest.failf "%s: unexpectedly completed after %d steps" label
        execution.executed_steps

let compare_vm label ~max_steps graph =
  let image = Fixture.image graph in
  let native = execute ~max_steps label image |> completed label in
  let vm =
    VM.execute_program ~max_steps ~max_frame_bytes:1024 ~max_call_depth:8
      ~functions:[] graph
    |> require_ok (fun errors ->
        errors
        |> List.map (fun (error : VM.error) ->
            error.code ^ ": " ^ error.message)
        |> String.concat "; ")
  in
  Alcotest.(check int)
    (label ^ " executed steps")
    (VM.executed_steps vm) native.executed_steps;
  match (VM.final_value vm, native.final_value) with
  | None, None -> ()
  | Some expected, Some actual ->
      Alcotest.(check string)
        (label ^ " differential type")
        (vm_type_name expected.type_)
        (word_type_name actual.type_);
      Alcotest.(check int64)
        (label ^ " differential bits")
        expected.bits actual.bits
  | _ -> Alcotest.failf "%s: VM/native final-value presence differs" label

let basic_execution_and_meter () =
  let image = Fixture.image (Fixture.multiply_graph ()) in
  let execution = execute ~max_steps:5 "6*7" image |> completed "6*7" in
  Alcotest.(check int) "five reached IR instructions" 5 execution.executed_steps;
  check_program_word "6*7 independent expected value" "I64" 42L
    execution.final_value;
  let fault =
    execute ~max_steps:4 "6*7 one below" image |> faulted "one below"
  in
  Alcotest.(check string)
    "one-below is a step fault" "step-limit"
    (Fixture.kind_name fault.kind);
  Alcotest.(check int) "one-below stops before END" 4 fault.executed_steps;
  Alcotest.(check int)
    "one-below site is END sparse id" 917 fault.instruction_id;
  let empty = Fixture.image (Fixture.empty_graph ()) in
  let execution = execute ~max_steps:1 "empty" empty |> completed "empty" in
  Alcotest.(check int) "empty program charges END" 1 execution.executed_steps;
  Alcotest.(check bool)
    "empty program has no final value" true
    (Option.is_none execution.final_value);
  let empty_transfers = Fixture.image (Fixture.empty_transfer_graph ()) in
  let execution =
    execute ~max_steps:3 "empty block entry and fallthrough" empty_transfers
    |> completed "empty block entry and fallthrough"
  in
  check_program_word "entry skips earlier source block" "I64" 42L
    execution.final_value;
  Alcotest.(check int)
    "empty block transfers cost no IR steps" 3 execution.executed_steps;
  let fault =
    execute ~max_steps:2 "empty block transfers one below" empty_transfers
    |> faulted "empty block transfers one below"
  in
  Alcotest.(check int)
    "empty transfer budget names final END" 202 fault.instruction_id;
  Alcotest.(check int)
    "empty transfer budget consumed only two instructions" 2
    fault.executed_steps;
  compare_vm "empty block differential" ~max_steps:3
    (Fixture.empty_transfer_graph ());
  compare_vm "multiply differential" ~max_steps:5 (Fixture.multiply_graph ());
  compare_vm "branch differential" ~max_steps:32 (Fixture.branch_graph ());
  compare_vm "last-value differential" ~max_steps:5
    (Fixture.last_value_graph ())

let arithmetic_graph ?(type_ = Fixture.i64) opcode left right =
  Fixture.verified ~entry:0
    [
      Fixture.block 0
        [
          Fixture.imm ~type_ ~span:(Fixture.span 600) 101 1 left;
          Fixture.imm ~type_ ~span:(Fixture.span 601) 303 2 right;
          Fixture.binary ~type_ ~span:(Fixture.span 602) 707 3 opcode 1 2;
          Fixture.end_expression ~span:(Fixture.span 603) 909 3;
          Fixture.stream_end ~span:(Fixture.span 604) 1201;
        ];
    ]

let arithmetic_faults () =
  let cases =
    [
      ( "signed divide zero",
        Fixture.i64,
        Opcode.Ic_div,
        84L,
        0L,
        Program.Division_by_zero,
        Program.Divide );
      ( "signed remainder zero",
        Fixture.i64,
        Opcode.Ic_mod,
        85L,
        0L,
        Program.Division_by_zero,
        Program.Remainder );
      ( "unsigned divide zero",
        Fixture.u64,
        Opcode.Ic_div,
        -1L,
        0L,
        Program.Division_by_zero,
        Program.Divide );
      ( "unsigned remainder zero",
        Fixture.u64,
        Opcode.Ic_mod,
        Int64.min_int,
        0L,
        Program.Division_by_zero,
        Program.Remainder );
      ( "signed divide overflow",
        Fixture.i64,
        Opcode.Ic_div,
        Int64.min_int,
        -1L,
        Program.Signed_division_overflow,
        Program.Divide );
      ( "signed remainder overflow",
        Fixture.i64,
        Opcode.Ic_mod,
        Int64.min_int,
        -1L,
        Program.Signed_division_overflow,
        Program.Remainder );
    ]
  in
  List.iter
    (fun (label, type_, opcode, left, right, expected_kind, expected_operation)
       ->
      let graph = arithmetic_graph ~type_ opcode left right in
      let image = Fixture.image graph in
      let fault = execute ~max_steps:20 label image |> faulted label in
      Alcotest.(check string)
        (label ^ " fault kind")
        (Fixture.kind_name expected_kind)
        (Fixture.kind_name fault.kind);
      Alcotest.(check string)
        (label ^ " operation")
        (Fixture.operation_name (Some expected_operation))
        (Fixture.operation_name fault.operation);
      Alcotest.(check int) (label ^ " block") 0 fault.block_id;
      Alcotest.(check int)
        (label ^ " sparse instruction")
        707 fault.instruction_id;
      Alcotest.(check int) (label ^ " block position") 2 fault.position;
      Alcotest.(check int) (label ^ " global position") 2 fault.global_position;
      Alcotest.(check int)
        (label ^ " fault instruction consumed")
        3 fault.executed_steps;
      Alcotest.(check bool)
        (label ^ " exact span") true
        (fault.span = Some (Fixture.span 602)))
    cases

let source_inputs ~mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-program-execution.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id
  in
  (session, config, source)

let source_report ~mode ~max_steps contents =
  let session, config, source = source_inputs ~mode contents in
  Native_program.evaluate session ~config ~source ~max_steps

let source_success ~mode ~max_steps contents =
  let report = source_report ~mode ~max_steps contents in
  match Native_program.outcome report with
  | Ok checked ->
      Alcotest.(check bool)
        "successful source has no warning" true (checked.diagnostics = []);
      checked.value
  | Error diagnostics ->
      Alcotest.fail
        (diagnostics
        |> List.map (fun (error : Diagnostic.t) ->
            error.code ^ ": " ^ error.message)
        |> String.concat "; ")

let source_fault ~mode ~max_steps contents =
  let report = source_report ~mode ~max_steps contents in
  let fault =
    match Native_program.native_outcome report with
    | Some (Program.Fault fault) -> fault
    | Some (Program.Completed _) ->
        Alcotest.fail "source unexpectedly completed"
    | None -> Alcotest.fail "source fault produced no native outcome"
  in
  let diagnostics =
    match Native_program.outcome report with
    | Ok _ -> Alcotest.fail "source fault unexpectedly reported success"
    | Error diagnostics -> diagnostics
  in
  (report, fault, diagnostics)

let source_control_both_modes () =
  let cases =
    [
      ("if(0) 1/0; else 42;", 42L);
      ("if(1) 42; else 1/0;", 42L);
      ("if(0 && (1/0)) 1/0; else 42;", 42L);
      ("if(1 || (1/0)) 42; else 1/0;", 42L);
      ("do {42; break;} while(1/0);", 42L);
      ("for(0;1;1/0) {42; break;}", 42L);
      ("if(1) {if(0) 1/0; else 42;} else 1/0;", 42L);
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (source, expected) ->
          let result = source_success ~mode ~max_steps:200 source in
          check_program_word source "I64" expected result.execution.final_value)
        cases;
      let result = source_success ~mode ~max_steps:5 "6*7;" in
      Alcotest.(check int)
        "source multiply exact five steps" 5 result.execution.executed_steps;
      check_program_word "source multiply" "I64" 42L
        result.execution.final_value;
      let empty = source_success ~mode ~max_steps:1 "" in
      Alcotest.(check int)
        "source empty END step" 1 empty.execution.executed_steps;
      Alcotest.(check bool)
        "source empty has no value" true
        (Option.is_none empty.execution.final_value))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let skipped_and_eager_faults () =
  List.iter
    (fun mode ->
      ignore (source_success ~mode ~max_steps:100 "if(0 && (1/0)) 42; else 42;");
      ignore
        (source_success ~mode ~max_steps:100 "if(1 || (1/0)) 42; else 1/0;");
      let _, fault, diagnostics =
        source_fault ~mode ~max_steps:100 "0 && (1/0);"
      in
      Alcotest.(check string)
        "ordinary logical value remains eager" "division-by-zero"
        (Fixture.kind_name fault.kind);
      Alcotest.(check string)
        "eager fault maps to VM semantic diagnostic" "HCIRVM0009"
        (List.hd diagnostics).code)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let source_arithmetic_fault_mapping () =
  let cases =
    [
      ("84/0;", Program.Division_by_zero, "HCIRVM0009");
      ("85%0;", Program.Division_by_zero, "HCIRVM0009");
      ( "0x8000000000000000(I64i)/-1;",
        Program.Signed_division_overflow,
        "HCIRVM0010" );
      ( "0x8000000000000000(I64i)%-1;",
        Program.Signed_division_overflow,
        "HCIRVM0010" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (source, kind, code) ->
          let _, fault, diagnostics =
            source_fault ~mode ~max_steps:100 source
          in
          Alcotest.(check string)
            source (Fixture.kind_name kind)
            (Fixture.kind_name fault.kind);
          Alcotest.(check string)
            (source ^ " diagnostic") code (List.hd diagnostics).code)
        cases)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let exact_loop_budgets () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let report, fault, diagnostics =
            source_fault ~mode ~max_steps:17 source
          in
          Alcotest.(check string)
            (source ^ " fault") "step-limit"
            (Fixture.kind_name fault.kind);
          Alcotest.(check int)
            (source ^ " exact budget") 17 fault.executed_steps;
          Alcotest.(check (option int))
            (source ^ " report progress")
            (Some 17)
            (Native_program.executed_steps report);
          Alcotest.(check string)
            (source ^ " diagnostic") "HCIRVM0007" (List.hd diagnostics).code)
        [ "while(1);"; "for(0;1;0);" ])
    [ Preprocessor.Jit; Preprocessor.Aot ];
  List.iter
    (fun mode ->
      let _, fault, diagnostics = source_fault ~mode ~max_steps:4 "6*7;" in
      Alcotest.(check int)
        "multiply one-below consumed four" 4 fault.executed_steps;
      Alcotest.(check string)
        "multiply one-below diagnostic" "HCIRVM0007" (List.hd diagnostics).code)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let pressure_shift_graph () =
  Fixture.verified ~entry:0
    [
      Fixture.block 0
        [
          Fixture.imm 100 10 8L;
          Fixture.imm 200 20 1L;
          Fixture.imm 300 30 3L;
          Fixture.imm 400 40 4L;
          Fixture.imm 500 50 5L;
          Fixture.imm 600 60 6L;
          Fixture.binary 700 70 Opcode.Ic_shl 10 20;
          Fixture.binary 800 80 Opcode.Ic_add 70 10;
          Fixture.binary 900 90 Opcode.Ic_add 80 30;
          Fixture.binary 1000 100 Opcode.Ic_add 90 40;
          Fixture.binary 1100 110 Opcode.Ic_add 100 50;
          Fixture.binary 1200 120 Opcode.Ic_add 110 60;
          Fixture.end_expression 1300 120;
          Fixture.stream_end 1400;
        ];
    ]

let pressure_logical_graph () =
  Fixture.verified ~entry:0
    [
      Fixture.block 0
        [
          Fixture.imm 101 10 2L;
          Fixture.imm 201 20 4L;
          Fixture.imm 301 30 3L;
          Fixture.imm 401 40 8L;
          Fixture.imm 501 50 24L;
          Fixture.imm 601 60 0L;
          Fixture.binary 701 70 Opcode.Ic_and_and 10 20;
          Fixture.binary 801 80 Opcode.Ic_add 70 10;
          Fixture.binary 901 90 Opcode.Ic_add 80 20;
          Fixture.binary 1001 100 Opcode.Ic_add 90 30;
          Fixture.binary 1101 110 Opcode.Ic_add 100 40;
          Fixture.binary 1201 120 Opcode.Ic_add 110 50;
          Fixture.binary 1301 130 Opcode.Ic_add 120 60;
          Fixture.end_expression 1401 130;
          Fixture.stream_end 1501;
        ];
    ]

let pressure_divmod_success_graph opcode left right cancel_right =
  Fixture.verified ~entry:0
    [
      Fixture.block 0
        [
          Fixture.imm 102 10 left;
          Fixture.imm 202 20 right;
          Fixture.imm 302 30 cancel_right;
          Fixture.imm 402 40 10L;
          Fixture.imm 502 50 (-10L);
          Fixture.imm 602 60 0L;
          Fixture.binary 702 70 opcode 10 20;
          Fixture.binary 802 80 Opcode.Ic_add 70 20;
          Fixture.binary 902 90 Opcode.Ic_add 80 30;
          Fixture.binary 1002 100 Opcode.Ic_add 90 40;
          Fixture.binary 1102 110 Opcode.Ic_add 100 50;
          Fixture.binary 1202 120 Opcode.Ic_add 110 60;
          Fixture.end_expression 1302 120;
          Fixture.stream_end 1402;
        ];
    ]

let pressure_com_unsigned_graph () =
  Fixture.verified ~entry:0
    [
      Fixture.block 0
        [
          Fixture.imm ~type_:Fixture.u64 103 10 Int64.min_int;
          Fixture.imm 203 20 (-1L);
          Fixture.imm 303 30 42L;
          Fixture.imm 403 40 0L;
          Fixture.imm 503 50 0L;
          Fixture.imm 603 60 0L;
          Fixture.unary 703 70 Opcode.Ic_com 10;
          Fixture.binary 803 80 Opcode.Ic_greater 70 20;
          Fixture.binary 903 90 Opcode.Ic_add 80 30;
          Fixture.binary 1003 100 Opcode.Ic_add 90 40;
          Fixture.binary 1103 110 Opcode.Ic_add 100 50;
          Fixture.binary 1203 120 Opcode.Ic_add 110 60;
          Fixture.end_expression 1303 120;
          Fixture.stream_end 1403;
        ];
    ]

let fixed_and_scratch_pressure () =
  let cases =
    [
      ("shared-left CL shift", pressure_shift_graph ());
      ("logical two-register scratch", pressure_logical_graph ());
      ( "shared-divisor signed DIV",
        pressure_divmod_success_graph Opcode.Ic_div 84L 2L (-2L) );
      ( "shared-divisor signed MOD",
        pressure_divmod_success_graph Opcode.Ic_mod 85L 43L (-43L) );
      ("COM-forwarded unsigned comparison", pressure_com_unsigned_graph ());
    ]
  in
  List.iter
    (fun (label, graph) ->
      let image = Fixture.image graph in
      Alcotest.(check bool)
        (label ^ " reaches spill path")
        true
        (Program.frame_bytes image > 0);
      Alcotest.(check int)
        (label ^ " reaches all physical/private registers")
        7
        (Program.register_peak image);
      let execution = execute ~max_steps:100 label image |> completed label in
      check_program_word
        (label ^ " independent literal result")
        "I64" 42L execution.final_value;
      compare_vm (label ^ " VM differential") ~max_steps:100 graph)
    cases

let multi_block_pressure_graph () =
  let pressure_block ~block_id ~instruction_base ~value_base count terminator =
    let definitions =
      List.init count (fun index ->
          Fixture.imm (instruction_base + index) (value_base + index)
            (Int64.of_int (index + 1)))
    in
    let rec reduce index accumulator = function
      | [] ->
          let final_instruction = instruction_base + count + count - 1 in
          Fixture.end_expression final_instruction accumulator
          :: terminator (final_instruction + 1)
      | operand :: rest ->
          let instruction = instruction_base + count + index in
          let result = value_base + count + index in
          Fixture.binary instruction result Opcode.Ic_add accumulator operand
          :: reduce (index + 1) result rest
    in
    Fixture.block block_id
      (definitions
      @ reduce 0 value_base
          (List.init (count - 1) (fun index -> value_base + index + 1)))
  in
  Fixture.verified ~entry:0
    [
      pressure_block ~block_id:0 ~instruction_base:100 ~value_base:1000 6
        (fun id -> [ Fixture.jump id 1 ]);
      pressure_block ~block_id:1 ~instruction_base:1000 ~value_base:2000 7
        (fun id -> [ Fixture.stream_end id ]);
    ]

let spilled_divmod_graph ~type_ opcode left right =
  Fixture.verified ~entry:0
    [
      Fixture.block 0
        [
          Fixture.imm ~type_ 100 10 left;
          Fixture.imm ~type_ 200 20 right;
          Fixture.imm ~type_ 300 30 10L;
          Fixture.imm ~type_ 400 40 11L;
          Fixture.imm ~type_ 500 50 12L;
          Fixture.imm ~type_ 600 60 13L;
          Fixture.binary ~type_ ~span:(Fixture.span 700) 700 70 opcode 10 20;
          Fixture.binary ~type_ 800 80 Opcode.Ic_add 30 40;
          Fixture.binary ~type_ 900 90 Opcode.Ic_add 50 60;
          Fixture.binary ~type_ 1000 100 Opcode.Ic_add 80 90;
          Fixture.binary ~type_ 1100 110 Opcode.Ic_add 70 100;
          Fixture.end_expression 1200 110;
          Fixture.stream_end 1300;
        ];
    ]

let spills_frames_and_repeated_cleanup () =
  let graph = multi_block_pressure_graph () in
  let image = Fixture.image graph in
  Alcotest.(check int)
    "frame is max of per-block spill requirements" 24
    (Program.frame_bytes image);
  let code = Program.code image in
  let unwind = Program.windows_unwind_info image in
  for round = 1 to 32 do
    let execution =
      execute ~max_steps:100 (Printf.sprintf "multi-block spill %d" round) image
      |> completed "multi-block spill"
    in
    check_program_word "multi-block spill final" "I64" 28L execution.final_value
  done;
  Alcotest.(check string)
    "success repetition preserves code" code (Program.code image);
  Alcotest.(check string)
    "success repetition preserves unwind" unwind
    (Program.windows_unwind_info image);
  let fault_cases =
    [
      ( "signed DIV zero",
        Fixture.i64,
        Opcode.Ic_div,
        84L,
        0L,
        Program.Division_by_zero,
        Program.Divide );
      ( "signed MOD zero",
        Fixture.i64,
        Opcode.Ic_mod,
        85L,
        0L,
        Program.Division_by_zero,
        Program.Remainder );
      ( "unsigned DIV zero",
        Fixture.u64,
        Opcode.Ic_div,
        -1L,
        0L,
        Program.Division_by_zero,
        Program.Divide );
      ( "unsigned MOD zero",
        Fixture.u64,
        Opcode.Ic_mod,
        Int64.min_int,
        0L,
        Program.Division_by_zero,
        Program.Remainder );
      ( "signed DIV overflow",
        Fixture.i64,
        Opcode.Ic_div,
        Int64.min_int,
        -1L,
        Program.Signed_division_overflow,
        Program.Divide );
      ( "signed MOD overflow",
        Fixture.i64,
        Opcode.Ic_mod,
        Int64.min_int,
        -1L,
        Program.Signed_division_overflow,
        Program.Remainder );
    ]
  in
  List.iter
    (fun (label, type_, opcode, left, right, kind, operation) ->
      let fault_image =
        Fixture.image (spilled_divmod_graph ~type_ opcode left right)
      in
      Alcotest.(check bool)
        (label ^ " genuinely spills")
        true
        (Program.frame_bytes fault_image > 0);
      let fault_code = Program.code fault_image in
      let fault_unwind = Program.windows_unwind_info fault_image in
      for round = 1 to 8 do
        let fault =
          execute ~max_steps:100
            (Printf.sprintf "%s round %d" label round)
            fault_image
          |> faulted label
        in
        Alcotest.(check string)
          (label ^ " kind") (Fixture.kind_name kind)
          (Fixture.kind_name fault.kind);
        Alcotest.(check string)
          (label ^ " operation")
          (Fixture.operation_name (Some operation))
          (Fixture.operation_name fault.operation);
        Alcotest.(check int)
          (label ^ " sparse instruction")
          700 fault.instruction_id;
        Alcotest.(check int) (label ^ " block position") 6 fault.position;
        Alcotest.(check bool)
          (label ^ " exact span") true
          (fault.span = Some (Fixture.span 700));
        let success =
          execute ~max_steps:100 "cleanup success" image |> completed "cleanup"
        in
        check_program_word "cleanup success" "I64" 28L success.final_value
      done;
      Alcotest.(check string)
        (label ^ " preserves code")
        fault_code (Program.code fault_image);
      Alcotest.(check string)
        (label ^ " preserves unwind")
        fault_unwind
        (Program.windows_unwind_info fault_image))
    fault_cases

let maximum_frame_executes () =
  let image = Fixture.image (Fixture.pressure_graph 516) in
  Alcotest.(check int) "maximum program frame" 4088 (Program.frame_bytes image);
  let execution =
    execute ~max_steps:1033 "maximum frame" image |> completed "maximum frame"
  in
  Alcotest.(check int)
    "maximum-frame exact instruction count" 1033 execution.executed_steps;
  check_program_word "maximum-frame sum" "I64" 133386L execution.final_value

let foreign_status_abi_rejected () =
  let host, foreign =
    match Runtime.platform () with
    | Runtime.Windows_x86_64 -> (Program.Windows_x64, Program.System_v_x64)
    | Runtime.Linux_x86_64 -> (Program.System_v_x64, Program.Windows_x64)
    | Runtime.Unsupported ->
        Alcotest.fail "native program tests require x86-64 host"
  in
  let native = Fixture.image ~status_abi:host (Fixture.multiply_graph ()) in
  ignore (execute ~max_steps:5 "host ABI" native |> completed "host ABI");
  let foreign_image =
    Fixture.image ~status_abi:foreign (Fixture.multiply_graph ())
  in
  match Runtime.execute ~max_steps:5 foreign_image with
  | Error message ->
      Alcotest.(check bool)
        "foreign ABI error is explicit" true
        (String.length message > 0)
  | Ok _ -> Alcotest.fail "foreign status ABI unexpectedly entered native code"

let invalid_runtime_budget_rejected () =
  let image = Fixture.image (Fixture.multiply_graph ()) in
  List.iter
    (fun max_steps ->
      match Runtime.execute ~max_steps image with
      | Error message ->
          Alcotest.(check bool)
            (Printf.sprintf "budget %d has explicit error" max_steps)
            true (message <> "")
      | Ok _ ->
          Alcotest.failf "runtime accepted nonpositive max_steps=%d" max_steps)
    [ 0; -1 ]

let () =
  match Runtime.platform () with
  | Runtime.Unsupported ->
      Alcotest.fail
        "native-tests explicitly requires Windows x86-64 or Linux x86-64"
  | Runtime.Windows_x86_64 | Runtime.Linux_x86_64 ->
      Alcotest.run "holyc native programs"
        [
          ( "program execution",
            [
              Alcotest.test_case "exact per-IR meter and VM differential" `Quick
                basic_execution_and_meter;
              Alcotest.test_case "typed arithmetic fault metadata" `Quick
                arithmetic_faults;
              Alcotest.test_case "closed source control flow in both modes"
                `Quick source_control_both_modes;
              Alcotest.test_case
                "conditional skip differs from eager logical value" `Quick
                skipped_and_eager_faults;
              Alcotest.test_case "source arithmetic faults map to VM semantics"
                `Quick source_arithmetic_fault_mapping;
              Alcotest.test_case "infinite loops stop at exact native budget"
                `Quick exact_loop_budgets;
              Alcotest.test_case
                "five-value reservation pressure covers shifts divmod logical \
                 and COM"
                `Quick fixed_and_scratch_pressure;
              Alcotest.test_case "cross-block spills restore frame and status"
                `Quick spills_frames_and_repeated_cleanup;
              Alcotest.test_case "maximum bounded frame executes" `Slow
                maximum_frame_executes;
              Alcotest.test_case "foreign private ABI rejects before entry"
                `Quick foreign_status_abi_rejected;
              Alcotest.test_case
                "nonpositive runtime budget rejects before entry" `Quick
                invalid_runtime_budget_rejected;
            ] );
        ]
