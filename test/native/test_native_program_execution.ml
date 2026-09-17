open Holyc_lib
module Fixture = Test_native_program
module Program = X86_64_program
module Runtime = Native_program_execution
module VM = Ir_integer_interpreter
module Opcode = Ir_opcode

let require_ok show = function
  | Ok value -> value
  | Error error -> Alcotest.fail (show error)

let execute ?max_frame_bytes ?max_call_depth ?max_active_stack_bytes
    ?(max_steps = 1000) label image =
  Runtime.execute ?max_frame_bytes ?max_call_depth ?max_active_stack_bytes
    ~max_steps image
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
  Alcotest.(check (option int))
    "closed runtime step fault has no function owner" None fault.function_id;
  Alcotest.(check (option string))
    "closed runtime step fault has no function name" None fault.function_name;
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

let source_image ~mode contents =
  let session, config, source = source_inputs ~mode contents in
  match Native_program.compile session ~config ~source with
  | Ok checked -> checked.value
  | Error diagnostics ->
      Alcotest.fail
        (diagnostics
        |> List.map (fun (error : Diagnostic.t) ->
            error.code ^ ": " ^ error.message)
        |> String.concat "; ")

let source_report ?max_frame_bytes ?max_call_depth ?max_active_stack_bytes ~mode
    ~max_steps contents =
  let session, config, source = source_inputs ~mode contents in
  Native_program.evaluate ?max_frame_bytes ?max_call_depth
    ?max_active_stack_bytes session ~config ~source ~max_steps

let source_success ?max_frame_bytes ?max_call_depth ?max_active_stack_bytes
    ~mode ~max_steps contents =
  let report =
    source_report ?max_frame_bytes ?max_call_depth ?max_active_stack_bytes ~mode
      ~max_steps contents
  in
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

let integer_source_success ~mode ~max_steps contents =
  let session, config, source = source_inputs ~mode contents in
  match run_integer_program session ~config ~source ~max_steps with
  | Ok checked -> checked.value
  | Error diagnostics ->
      Alcotest.fail
        (diagnostics
        |> List.map (fun (error : Diagnostic.t) ->
            error.code ^ ": " ^ error.message)
        |> String.concat "; ")

let compare_source_vm ~mode ~max_steps label contents =
  let native = source_success ~mode ~max_steps contents in
  let vm = integer_source_success ~mode ~max_steps contents in
  match (VM.final_value vm, native.execution.final_value) with
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

let source_fault ?max_frame_bytes ?max_call_depth ?max_active_stack_bytes ~mode
    ~max_steps contents =
  let report =
    source_report ?max_frame_bytes ?max_call_depth ?max_active_stack_bytes ~mode
      ~max_steps contents
  in
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

let integer_source_fault ?max_frame_bytes ?max_call_depth ~mode ~max_steps
    contents =
  let session, config, source = source_inputs ~mode contents in
  let unit =
    compile_integer_program session ~config ~source
    |> require_ok (fun diagnostics ->
        diagnostics
        |> List.map (fun (error : Diagnostic.t) ->
            error.code ^ ": " ^ error.message)
        |> String.concat "; ")
    |> fun checked -> checked.value
  in
  match
    VM.execute_program
      ~runtime_calls:(integer_program_runtime_calls unit)
      ~globals:(integer_program_globals unit)
      ~initialization:(integer_program_initialization unit)
      ~functions:(integer_program_functions unit)
      ~max_frame_bytes:(Option.value max_frame_bytes ~default:1_048_576)
      ~max_call_depth:(Option.value max_call_depth ~default:128)
      ~max_steps
      (integer_program_entry unit)
  with
  | Ok _ -> Alcotest.fail "integer source unexpectedly completed"
  | Error [] -> Alcotest.fail "integer source fault returned no diagnostic"
  | Error (first :: _) -> first

let source_functions_and_locals_both_modes () =
  let add = "I64 Add(I64 a,I64 b){I64 c=a+b;return c;}\n(Add(20,22));" in
  let cases =
    [
      ( "signed direct-call variant",
        "I64 Add(I64 a,I64 b){return a+b;}\nAdd(-20,-22);",
        "I64",
        -42L );
      ( "U64 high bit survives parameter and local storage",
        "U64 Keep(U64 n){U64 saved=n;return saved;}\nKeep(0xffffffffffffffff);",
        "U64",
        -1L );
      ( "local mutation loop and early return",
        "I64 Choose(I64 n){I64 x=n;while(x>0){x--;if(x==2)return x+40;}return \
         0;}\n\
         Choose(5);",
        "I64",
        42L );
      ( "nested call preserves caller live local",
        "I64 Add(I64 a,I64 b){return a+b;}\n\
         I64 Outer(I64 n){I64 keep=n*5;return keep+Add(n+1,n+2)+keep;}\n\
         Outer(3);",
        "I64",
        39L );
      ( "fixed arguments execute right to left and bind by formal order",
        "I64 Pair(I64 left,I64 right){return left*100+right;}\n\
         I64 Probe(){I64 n=1;return Pair(n++,n++);}\n\
         Probe();",
        "I64",
        201L );
      ( "recursive calls isolate scalar locals",
        "I64 Sum(I64 n){I64 saved=n;if(n<=0)return 0;return saved+Sum(n-1);}\n\
         Sum(8);",
        "I64",
        36L );
      ( "all scalar compound operators preserve stored values",
        "I64 Update(){I64 \
         x=21;x+=9;x-=2;x*=3;x/=2;x%=25;x<<=2;x>>=1;x&=31;x|=32;x^=24;return \
         x;}\n\
         Update();",
        "I64",
        58L );
      ( "isolated compound subtraction executes on parameters",
        "I64 Update(I64 x,I64 delta){x-=delta;return x;}\nUpdate(73,31);",
        "I64",
        42L );
      ( "isolated compound multiplication executes on parameters",
        "I64 Update(I64 x,I64 factor){x*=factor;return x;}\nUpdate(6,7);",
        "I64",
        42L );
      ( "isolated compound AND executes on parameters",
        "I64 Update(I64 x,I64 mask){x&=mask;return x;}\nUpdate(0x6f,0x2a);",
        "I64",
        42L );
      ( "isolated compound OR executes on parameters",
        "I64 Update(I64 x,I64 bits){x|=bits;return x;}\nUpdate(0x20,0x0a);",
        "I64",
        42L );
      ( "isolated compound XOR executes on parameters",
        "I64 Update(I64 x,I64 bits){x^=bits;return x;}\nUpdate(0x3f,0x15);",
        "I64",
        42L );
      ( "isolated signed compound left shift keeps destination class",
        "I64 Update(I64 x,U64 count){x<<=count;return x;}\nUpdate(21,1);",
        "I64",
        42L );
      ( "unsigned compound remainder uses the entire word",
        "U64 Update(U64 x,U64 divisor){x%=divisor;return x;}\n\
         Update(0xffffffffffffffff,10);",
        "U64",
        5L );
      ( "isolated unsigned compound left shift preserves full word",
        "U64 Update(U64 x,U64 count){x<<=count;return x;}\n\
         Update(0x4000000000000001,1);",
        "U64",
        Int64.add Int64.min_int 2L );
      ( "compound assignment returns the stored word",
        "I64 Update(){I64 x=4;return (x+=3)*100+x;}\nUpdate();",
        "I64",
        707L );
      ( "compound assignment reads after right-hand storage effects",
        "I64 Update(){I64 x=3;x+=(x=7);return x;}\nUpdate();",
        "I64",
        14L );
      ( "compound division preserves caller values across its RHS call",
        "I64 Value(I64 n){return n;}\n\
         I64 Update(){I64 x=84;return 10+(x/=Value(2))+x;}\n\
         Update();",
        "I64",
        94L );
      ( "prefix and postfix updates return distinct old and new values",
        "I64 Update(){I64 x=4;I64 a=++x;I64 b=x++;I64 c=--x;I64 d=x--;return \
         a*10000+b*1000+c*100+d*10+x;}\n\
         Update();",
        "I64",
        55554L );
      ( "unsigned compound shift preserves its high-bit class",
        "U64 Update(){U64 x=0x8000000000000000;x>>=1;return x;}\nUpdate();",
        "U64",
        0x4000000000000000L );
      ( "signed compound shift keeps destination class with unsigned count",
        "I64 Update(){I64 x=-8;U64 count=1;x>>=count;return x;}\nUpdate();",
        "I64",
        -4L );
      ( "unsigned compound division uses the entire word",
        "U64 Update(){U64 x=0xffffffffffffffff;x/=2;return x;}\nUpdate();",
        "U64",
        Int64.max_int );
      ( "signed compound division keeps destination class",
        "I64 Update(){I64 x=-84;U64 divisor=2;x/=divisor;return x;}\nUpdate();",
        "I64",
        -42L );
      ( "signed compound remainder keeps destination class",
        "I64 Update(){I64 x=-85;U64 divisor=2;x%=divisor;return x;}\nUpdate();",
        "I64",
        -1L );
      ( "six live caller words survive a nested generated call",
        "I64 Leaf(I64 n){return n*2;}\n\
         I64 Outer(I64 n){return \
         (n+1)+((n+2)+((n+3)+((n+4)+((n+5)+((n+6)+Leaf(n))))));}\n\
         Outer(10);",
        "I64",
        101L );
      ( "completed outer arguments survive further nested argument calls",
        "I64 Pair(I64 left,I64 right){return left*100+right;}\n\
         Pair(Pair(1,2),Pair(3,4));",
        "I64",
        10504L );
    ]
  in
  List.iter
    (fun mode ->
      let exact = source_success ~mode ~max_steps:29 add in
      Alcotest.(check int)
        "Add(20,22) exact function step count" 29 exact.execution.executed_steps;
      check_program_word "Add(20,22)" "I64" 42L exact.execution.final_value;
      compare_source_vm ~mode ~max_steps:29 "Add(20,22)" add;
      let _, one_below, diagnostics = source_fault ~mode ~max_steps:28 add in
      Alcotest.(check int)
        "Add(20,22) one-below consumes the complete budget" 28
        one_below.executed_steps;
      Alcotest.(check string)
        "Add(20,22) one-below is a step fault" "step-limit"
        (Fixture.kind_name one_below.kind);
      Alcotest.(check string)
        "Add(20,22) one-below diagnostic" "HCIRVM0007"
        (List.hd diagnostics).code;
      List.iter
        (fun (label, source, expected_type, expected_bits) ->
          let result = source_success ~mode ~max_steps:1000 source in
          check_program_word label expected_type expected_bits
            result.execution.final_value;
          compare_source_vm ~mode ~max_steps:1000 label source)
        cases)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let function_resource_limits_and_fault_recovery () =
  let recursion =
    "I64 Recur(I64 n){if(n)return Recur(n-1);return 42;}\nRecur(3);"
  in
  let nested_division =
    "I64 Leaf(I64 n){return 84/n;}\n\
     I64 Middle(I64 n){return Leaf(n);}\n\
     I64 Top(I64 n){return Middle(n);}\n\
     Top(0);"
  in
  let healthy = "I64 Add(I64 a,I64 b){return a+b;}\nAdd(20,22);" in
  List.iter
    (fun mode ->
      let exact_depth =
        source_success ~max_call_depth:4 ~mode ~max_steps:1000 recursion
      in
      check_program_word "four active named calls" "I64" 42L
        exact_depth.execution.final_value;
      let _, depth_fault, depth_diagnostics =
        source_fault ~max_call_depth:3 ~mode ~max_steps:1000 recursion
      in
      let expected_depth =
        integer_source_fault ~max_call_depth:3 ~mode ~max_steps:1000 recursion
      in
      Alcotest.(check string)
        "one-below call depth" "call-depth"
        (Fixture.kind_name depth_fault.kind);
      Alcotest.(check string)
        "call-depth diagnostic" expected_depth.code
        (List.hd depth_diagnostics).code;
      Alcotest.(check int)
        "call-depth fault consumes the hosted step count"
        expected_depth.executed_steps depth_fault.executed_steps;
      Alcotest.(check (option int))
        "call-depth owner ID" expected_depth.function_id depth_fault.function_id;
      Alcotest.(check (option string))
        "call-depth owner" expected_depth.function_name
        depth_fault.function_name;
      Alcotest.(check (option string))
        "hosted call-depth owner sanity" (Some "Recur")
        expected_depth.function_name;
      let exact_frame =
        source_success ~max_frame_bytes:32 ~mode ~max_steps:1000 recursion
      in
      check_program_word "four eight-byte parameter frames" "I64" 42L
        exact_frame.execution.final_value;
      let _, frame_fault, frame_diagnostics =
        source_fault ~max_frame_bytes:31 ~mode ~max_steps:1000 recursion
      in
      let expected_frame =
        integer_source_fault ~max_frame_bytes:31 ~mode ~max_steps:1000 recursion
      in
      Alcotest.(check string)
        "one-below active frame bytes" "frame-limit"
        (Fixture.kind_name frame_fault.kind);
      Alcotest.(check string)
        "frame-limit diagnostic" expected_frame.code
        (List.hd frame_diagnostics).code;
      Alcotest.(check int)
        "frame-limit fault consumes the hosted step count"
        expected_frame.executed_steps frame_fault.executed_steps;
      Alcotest.(check (option int))
        "frame-limit owner ID" expected_frame.function_id
        frame_fault.function_id;
      Alcotest.(check (option string))
        "frame-limit owner" expected_frame.function_name
        frame_fault.function_name;
      Alcotest.(check (option string))
        "hosted frame-limit owner sanity" (Some "Recur")
        expected_frame.function_name;
      let _, division_fault, division_diagnostics =
        source_fault ~mode ~max_steps:1000 nested_division
      in
      let expected_division =
        integer_source_fault ~mode ~max_steps:1000 nested_division
      in
      Alcotest.(check string)
        "nested division kind" "division-by-zero"
        (Fixture.kind_name division_fault.kind);
      Alcotest.(check string)
        "nested division diagnostic" expected_division.code
        (List.hd division_diagnostics).code;
      Alcotest.(check int)
        "nested division consumes the hosted step count"
        expected_division.executed_steps division_fault.executed_steps;
      Alcotest.(check (option int))
        "nested division owner ID" expected_division.function_id
        division_fault.function_id;
      Alcotest.(check (option string))
        "nested division owner" expected_division.function_name
        division_fault.function_name;
      Alcotest.(check (option string))
        "hosted nested division owner sanity" (Some "Leaf")
        expected_division.function_name;
      let nested_healthy =
        "I64 Leaf(I64 n){return 84/n;}\n\
         I64 Middle(I64 n){return Leaf(n);}\n\
         I64 Top(I64 n){return Middle(n);}\n\
         Top(2);"
      in
      let expected_budget =
        integer_source_fault ~mode ~max_steps:16 nested_healthy
      in
      let _, budget_fault, budget_diagnostics =
        source_fault ~mode ~max_steps:16 nested_healthy
      in
      Alcotest.(check string)
        "nested budget exhaustion diagnostic" "HCIRVM0007"
        (List.hd budget_diagnostics).code;
      Alcotest.(check int)
        "nested budget exhaustion exact work" expected_budget.executed_steps
        budget_fault.executed_steps;
      Alcotest.(check (option string))
        "nested budget fault owner matches interpreter"
        expected_budget.function_name budget_fault.function_name;
      Alcotest.(check (option string))
        "nested budget reaches the innermost active call" (Some "Leaf")
        budget_fault.function_name;
      Alcotest.(check (option int))
        "nested budget fault function identity" expected_budget.function_id
        budget_fault.function_id;
      let recovered = source_success ~mode ~max_steps:29 healthy in
      check_program_word "success after whole-chain fault" "I64" 42L
        recovered.execution.final_value)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let uninitialized_automatic_locals_match_hosted_policy () =
  let cases =
    [
      "I64 F(){I64 x;return x;}\nF();";
      "I64 F(){I64 x;if(0)x=42;return x;}\nF();";
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let expected = integer_source_fault ~mode ~max_steps:1000 source in
          Alcotest.(check string)
            "hosted uninitialized diagnostic" "HCIRVM0012" expected.code;
          let _, fault, diagnostics =
            source_fault ~mode ~max_steps:1000 source
          in
          Alcotest.(check string)
            "native uninitialized kind" "uninitialized-read"
            (Fixture.kind_name fault.kind);
          Alcotest.(check string)
            "native uninitialized diagnostic" expected.code
            (List.hd diagnostics).code;
          Alcotest.(check int)
            "attempted load consumes the same exact step"
            expected.executed_steps fault.executed_steps;
          Alcotest.(check (option int))
            "uninitialized owner function id" expected.function_id
            fault.function_id;
          Alcotest.(check (option string))
            "uninitialized owner function name" expected.function_name
            fault.function_name;
          Alcotest.(check (option int))
            "uninitialized load source position"
            (Option.map (fun span -> span.Span.start) expected.span)
            (Option.map (fun span -> span.Span.start) fault.span))
        cases)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let physical_native_stack_is_bounded_independently () =
  let root_only = "42;" in
  let identity = "I64 Identity(I64 value){return value;}\nIdentity(42);" in
  let recursive = "I64 Recur(){return Recur();}\nRecur();" in
  let healthy = "I64 Add(I64 a,I64 b){return a+b;}\nAdd(20,22);" in
  let identity_call_site mode =
    let session, config, source = source_inputs ~mode identity in
    let unit =
      compile_integer_program session ~config ~source
      |> require_ok (fun diagnostics ->
          diagnostics
          |> List.map (fun (error : Diagnostic.t) ->
              error.code ^ ": " ^ error.message)
          |> String.concat "; ")
      |> fun checked -> checked.value
    in
    let calls =
      integer_program_entry unit |> Ir_x87_stack.graph |> Ir_block_graph.blocks
      |> List.concat_map (fun block ->
          Ir_block_graph.instructions block
          |> Ir_instruction_sequence.instructions
          |> List.map Ir_instruction_sequence.description)
      |> List.filter (fun (description : Ir_instruction_sequence.description) ->
          description.opcode = Opcode.Ic_call)
    in
    match calls with
    | [ call ] ->
        ( Ir_instruction_sequence.Instruction_id.to_int call.instruction_id,
          Option.map (fun (span : Span.t) -> (span.start, span.stop)) call.span
        )
    | calls ->
        Alcotest.failf "Identity entry has %d IC_CALL instructions"
          (List.length calls)
  in
  List.iter
    (fun mode ->
      let root_image = source_image ~mode root_only in
      let root_bytes = Program.entry_stack_bytes root_image in
      Alcotest.(check bool)
        "entry has a positive physical stack charge" true (root_bytes > 0);
      ignore
        (execute ~max_active_stack_bytes:root_bytes ~max_steps:100
           "exact root stack budget" root_image
        |> completed "exact root stack budget");
      (if root_bytes > 1 then
         match
           Runtime.execute ~max_active_stack_bytes:(root_bytes - 1)
             ~max_steps:100 root_image
         with
         | Error message ->
             Alcotest.(check bool)
               "root one-below fails before native entry" true (message <> "")
         | Ok _ -> Alcotest.fail "root one-below physical stack budget executed");
      let identity_image = source_image ~mode identity in
      (* The independent callable byte golden has a 32-byte entry allocation:
         return address + PUSH RBP + allocation = 48 bytes. Identity is frameless,
         so its return address + PUSH RBP costs 16 more. This private convention is
         shared by both host status ABIs, making the complete call require 64 bytes. *)
      Alcotest.(check int)
        "Identity entry physical charge matches byte golden" 48
        (Program.entry_stack_bytes identity_image);
      let exact_identity =
        execute ~max_active_stack_bytes:64 ~max_steps:100
          "Identity exact physical stack budget" identity_image
        |> completed "Identity exact physical stack budget"
      in
      check_program_word "Identity exact physical stack result" "I64" 42L
        exact_identity.final_value;
      let expected_call_id, expected_call_span = identity_call_site mode in
      let one_below =
        execute ~max_active_stack_bytes:63 ~max_steps:100
          "Identity one-below physical stack budget" identity_image
        |> faulted "Identity one-below physical stack budget"
      in
      Alcotest.(check string)
        "Identity one-below faults native stack at the call"
        "native-stack-limit"
        (Fixture.kind_name one_below.kind);
      Alcotest.(check int)
        "Identity native-stack fault keeps original IC_CALL identity"
        expected_call_id one_below.instruction_id;
      Alcotest.(check (option (pair int int)))
        "Identity native-stack fault keeps original call span"
        expected_call_span
        (Option.map
           (fun (span : Span.t) -> (span.start, span.stop))
           one_below.span);
      let reused_identity =
        execute ~max_active_stack_bytes:64 ~max_steps:100
          "Identity healthy reuse after physical fault" identity_image
        |> completed "Identity healthy reuse after physical fault"
      in
      check_program_word "Identity healthy reuse after physical fault" "I64" 42L
        reused_identity.final_value;
      let recursive_image = source_image ~mode recursive in
      let root_bytes = Program.entry_stack_bytes recursive_image in
      let allowance =
        min Runtime.hard_max_active_stack_bytes (root_bytes + 32)
      in
      let outcome =
        execute ~max_frame_bytes:1 ~max_call_depth:10000
          ~max_active_stack_bytes:allowance ~max_steps:100000
          "zero-frame recursive physical bound" recursive_image
      in
      let fault = faulted "zero-frame recursive physical bound" outcome in
      Alcotest.(check string)
        "zero-frame recursion reaches physical bound" "native-stack-limit"
        (Fixture.kind_name fault.kind);
      Alcotest.(check (option string))
        "physical stack fault owner" (Some "Recur") fault.function_name;
      let recovered = source_success ~mode ~max_steps:29 healthy in
      check_program_word "success after physical stack fault" "I64" 42L
        recovered.execution.final_value)
    [ Preprocessor.Jit; Preprocessor.Aot ]

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

let compound_faults_restore_all_calls () =
  List.iter
    (fun mode ->
      List.iter
        (fun (word_type, operation, left, right, code, kind) ->
          let contents =
            Printf.sprintf
              "%s Leaf(%s n){%s x=%s;x%s=n;return x;}\n\
               %s Outer(%s n){return Leaf(n);}\n\
               Outer(%s);"
              word_type word_type word_type left operation word_type word_type
              right
          in
          let expected = integer_source_fault ~mode ~max_steps:1000 contents in
          let _, actual, diagnostics =
            source_fault ~mode ~max_steps:1000 contents
          in
          Alcotest.(check string)
            "compound fault diagnostic" code (List.hd diagnostics).code;
          Alcotest.(check string)
            "compound fault kind" (Fixture.kind_name kind)
            (Fixture.kind_name actual.kind);
          Alcotest.(check int)
            "compound fault consumes exact IR work" expected.executed_steps
            actual.executed_steps;
          Alcotest.(check (option string))
            "compound fault callee identity" (Some "Leaf") actual.function_name;
          Alcotest.(check (option int))
            "compound fault source instruction" expected.instruction_id
            (Some actual.instruction_id);
          let recovered =
            source_success ~mode ~max_steps:1000
              "I64 Healthy(I64 n){I64 x=84;x/=n;return x;}\nHealthy(2);"
          in
          check_program_word "compound fault subsequent execution" "I64" 42L
            recovered.execution.final_value)
        [
          ("I64", "/", "84", "0", "HCIRVM0009", Program.Division_by_zero);
          ("I64", "%", "85", "0", "HCIRVM0009", Program.Division_by_zero);
          ( "U64",
            "/",
            "0xffffffffffffffff",
            "0",
            "HCIRVM0009",
            Program.Division_by_zero );
          ( "U64",
            "%",
            "0xffffffffffffffff",
            "0",
            "HCIRVM0009",
            Program.Division_by_zero );
          ( "I64",
            "/",
            "0x8000000000000000(I64i)",
            "-1",
            "HCIRVM0010",
            Program.Signed_division_overflow );
          ( "I64",
            "%",
            "0x8000000000000000(I64i)",
            "-1",
            "HCIRVM0010",
            Program.Signed_division_overflow );
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

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
              Alcotest.test_case
                "source functions locals calls recursion and argument order"
                `Quick source_functions_and_locals_both_modes;
              Alcotest.test_case
                "function depth frame faults unwind and recover" `Quick
                function_resource_limits_and_fault_recovery;
              Alcotest.test_case
                "compound arithmetic faults unwind every generated call" `Quick
                compound_faults_restore_all_calls;
              Alcotest.test_case
                "uninitialized automatic locals match hosted execution" `Quick
                uninitialized_automatic_locals_match_hosted_policy;
              Alcotest.test_case
                "physical native stack is bounded independently" `Quick
                physical_native_stack_is_bounded_independently;
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
