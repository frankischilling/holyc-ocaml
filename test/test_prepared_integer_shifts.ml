open Holyc_lib
open Yojson.Safe.Util
module A = Test_internal_strlen_authority
module C = Division_strength_reduction_cases
module S = Test_source_constant_shifts
module Calls = Ir_runtime_call_context
module Seq = Ir_instruction_sequence
module Op = Ir_opcode
module VM = Ir_integer_interpreter
module Unit = Holyc_lib__Driver.Integer_unit
module Live = Test_live_initializer_execution

let modes = A.modes
let run = Test_pointer_equality.run
let success = Test_pointer_equality.success
let fixture () = C.load "prepared-integer-shifts.json"

let cases () =
  let fixture = fixture () in
  let projections = C.projections fixture in
  Alcotest.(check int)
    "all captured preparation fields" 19 (List.length projections);
  List.map
    (fun projection ->
      let expected = C.observed fixture projection "case_id" in
      Alcotest.(check int64)
        "same-boot native repeat" expected
        (C.observed fixture projection "repeat_case_id");
      (C.field projection "field", C.field projection "holy_c_source", expected))
    projections

let values () =
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, expected) ->
          let report, e = success mode source in
          Alcotest.(check (option int64))
            label (Some expected)
            (Option.map (fun w -> w.VM.bits) (VM.final_value e));
          Alcotest.(check string)
            "held values have no repeated effects" ""
            (integer_program_report_output_bytes report))
        (cases ()))
    modes

let retained ?(declarations = "I64 N=0,X=-7;") ?(expected = "-4") body =
  "#exe {" ^ declarations ^ "I64 Init(){N++;" ^ body
  ^ "}I64 Saved(I64 n=Init()){return n;}if(N!=1)Print(\"early\");X=100;N=0;"
  ^ "if(Saved()!=" ^ expected
  ^ "||N)Print(\"replay\");StreamPrint(\"%d;\",Saved()+46);}"

let contexts () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let report, e = success mode source in
          Alcotest.(check (option int64))
            "once-only default and nested callee" (Some 42L)
            (Option.map (fun w -> w.VM.bits) (VM.final_value e));
          Alcotest.(check string)
            "source-time assertions" ""
            (integer_program_report_output_bytes report))
        [
          retained "return X/2;";
          retained "I64 x=X;x/=2;return x;";
          retained "I8 x=-7;x/=2;return x;";
          retained "I8 x=-7;I8 *p=&x;*p/=2;return x;";
          retained "I8 a[1];a[0]=-7;a[0]/=2;return a[0];";
          "#exe {I64 N=0,X=-7;I64 F(I64 x){N++;return x/2;}I64 Init(){return \
           F(X);}I64 Saved(I64 n=Init()){return \
           n;}if(N!=1)Print(\"early\");X=100;N=0;if(Saved()!=-4||N)Print(\"replay\");StreamPrint(\"%d;\",Saved()+46);}";
        ])
    modes;
  let parsed, task =
    Live.run_result
      "I64 N=0,X=-7;I64 F(I64 x){N++;return x/2;}I64 G=F(X);G+N+45;"
  in
  ignore (Test_parser.expect_ast parsed);
  Alcotest.(check (option int64))
    "live source initializer" (Some 42L)
    (Option.map
       (fun w -> w.VM.bits)
       (Integer_task.progress task).runtime.final_value)

let collected fixture =
  let unit_ = fixture.Native_scalar_fixture.unit_ in
  let body = (List.hd (Unit.functions unit_)).VM.body in
  Calls.original_preparation_shifts (Unit.runtime_calls unit_)
    ~owner:(Calls.Function body)

let authority () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, opcode, expected) ->
          let original = A.fixture ~source mode in
          A.valid_control ~expected original;
          let proof = Option.get (collected original) in
          let _, d = S.find_cell original opcode in
          Alcotest.(check bool)
            "original reduction qualifies" true
            (Calls.is_original_preparation_shift proof d);
          Alcotest.(check bool)
            "copied record has no preparation authority" false
            (Calls.is_original_preparation_shift proof
               { d with operands = List.map Fun.id d.operands });
          let foreign = A.fixture ~source mode in
          Alcotest.(check bool)
            "same-text foreign context" false
            (Calls.is_original_preparation_shift
               (Option.get (collected foreign))
               d);
          List.iter
            (fun change ->
              let changed = A.fixture ~source mode in
              let _before = Option.get (collected changed) in
              let cell, supplied = S.find_cell changed opcode in
              Obj.set_field (Obj.repr cell) 0 (Obj.repr (change supplied));
              Alcotest.(check bool)
                "changed original graph cannot collect again" true
                (Option.is_none (collected changed));
              A.rejects "VM rechecks the original source bundle"
                (A.execute changed);
              A.rejects "native admission rechecks the original source bundle"
                (A.compile changed))
            [
              (fun d -> { d with flags = 1L });
              (fun d -> { d with span = None });
              (fun d -> { d with target_type = None });
              (fun d ->
                { d with target_type = Some Test_constant_shifts.F.u64 });
              (fun d -> { d with operands = List.map Fun.id d.operands });
              (fun d -> { d with payload = Some (Seq.Integer 63L) });
            ])
        [
          ("I64 F(I64 x){return x/2;}F(-7);", Op.Ic_shr_const, -4L);
          ("I64 F(I64 x){return x<<1;}F(-7);", Op.Ic_shl_const, -14L);
          ("I64 F(I64 x){x/=2;return x;}F(-7);", Op.Ic_shr_equ, -4L);
        ])
    modes

let transitive_authority () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let original = A.fixture ~source mode in
          let proof = Option.get (collected original) in
          let _, shift = S.find_cell original Op.Ic_shl_const in
          Alcotest.(check bool)
            "complete count has original authority" true
            (Calls.is_original_preparation_shift proof shift);
          A.valid_control ~expected:(-7L) original;
          let changed = A.fixture ~source mode in
          let _before = Option.get (collected changed) in
          let cell, input = S.find_cell changed Op.Ic_deref in
          Obj.set_field (Obj.repr cell) 0
            (Obj.repr { input with operands = List.map Fun.id input.operands });
          Alcotest.(check bool)
            "copied transitive input cannot collect" true
            (Option.is_none (collected changed));
          A.rejects "VM checks the captured transitive input"
            (A.execute changed);
          A.rejects "native checks the captured transitive input"
            (A.compile changed);
          let foreign = A.fixture ~source mode in
          let body = (List.hd (Unit.functions foreign.unit_)).VM.body in
          Alcotest.(check bool)
            "foreign owner cannot collect" true
            (Option.is_none
               (Calls.original_preparation_shifts
                  (Unit.runtime_calls original.unit_)
                  ~owner:(Calls.Function body))))
        [
          "I64 F(I64 x){return (x<<63)<<1;}F(-7);";
          "I64 F(I64 x){return (x<<-1)<<1;}F(-7);";
          "I64 F(I64 x){return (x<<0x7FFFFFFFFFFFFFFF)<<1;}F(-7);";
        ];
      let source = "I64 F(I64 x,I64 n){return x>>n;}F(-7,1);" in
      let raw = A.fixture ~source mode in
      A.valid_control ~expected:(-4L) raw;
      let proof = Option.get (collected raw) in
      let cell, shift = S.find_cell raw Op.Ic_shr in
      Alcotest.(check bool)
        "raw shift has no preparation authority" false
        (Calls.is_original_preparation_shift proof shift);
      Obj.set_field (Obj.repr cell) 0
        (Obj.repr
           {
             shift with
             opcode = Op.Ic_shr_const;
             operands = [ List.hd shift.operands ];
             payload = Some (Seq.Integer 1L);
           });
      Alcotest.(check bool)
        "new shift after sealing cannot collect" true
        (Option.is_none (collected raw));
      A.rejects "VM rejects a new reduction after sealing" (A.execute raw);
      A.rejects "native rejects a new reduction after sealing" (A.compile raw);
      let changed =
        A.fixture ~source:"I64 F(I64 x){x/=2;return x;}F(-7);" mode
      in
      let _before = Option.get (collected changed) in
      let cell, count = S.find_cell changed Op.Ic_imm_i64 in
      Obj.set_field (Obj.repr cell) 0
        (Obj.repr { count with payload = Some (Seq.Integer 2L) });
      Alcotest.(check bool)
        "changed compound count cannot collect" true
        (Option.is_none (collected changed));
      A.rejects "VM checks the original compound count" (A.execute changed);
      A.rejects "native checks the original compound count" (A.compile changed))
    modes

let output_effects () =
  List.iter
    (fun mode ->
      let source = retained "Print(\"kept\");return X/2;" in
      let report, e = success mode source in
      Alcotest.(check string)
        "reached preparation output occurs once" "kept"
        (integer_program_report_output_bytes report);
      Alcotest.(check (option int64))
        "saved shifted default" (Some 42L)
        (Option.map (fun w -> w.VM.bits) (VM.final_value e)))
    modes

let ranges_and_boundaries () =
  List.iter
    (fun mode ->
      List.iter
        (fun body ->
          let _, e = success mode (retained body) in
          Alcotest.(check (option int64))
            "bounded narrow right shift" (Some 42L)
            (Option.map (fun w -> w.VM.bits) (VM.final_value e)))
        [ "I8 x=-7;x/=2;return x;"; "I8 x=-7;I8 y=x;y>>=1;return y;" ];
      List.iter
        (fun source ->
          match integer_program_report_outcome (run mode source) with
          | Error (d :: _) ->
              Alcotest.(check string)
                "unproved preparation keeps its boundary" "HCRUN0006" d.code
          | _ -> Alcotest.fail "unproved preparation succeeded")
        [
          retained "I8 x=249;x/=2;return x;";
          retained "I8 x=-7;x*=3;x/=2;return x;";
          retained "I8 x=-7;x/=2;x=249;return x;";
          retained "I8 reg RAX x=-7;x/=2;return x;";
          retained "I64 n=1;return X>>n;";
          retained "I64 x=X;x>>=64;return x;";
          "I64 F(I64 x){return x/2;}I64 G=F(-7);G;";
        ])
    modes

let faults_and_limits () =
  let fixture = fixture () in
  let projection = fixture |> member "hosted_fault_projection" in
  let source = C.field projection "holy_c_source" in
  let fault id =
    fixture |> member "checks" |> to_list
    |> List.find (fun c -> C.field c "id" = id)
    |> member "observed_fault"
  in
  let first = fault (C.field projection "case_id") in
  Alcotest.(check string)
    "captured native exception" "DivZero"
    (C.field first "exception");
  Alcotest.(check string)
    "captured original effect before failure" "0000000000000001"
    (C.field first "counter");
  Alcotest.(check bool)
    "native fault repeated within its boot" true
    (first = fault (C.field projection "repeat_case_id"));
  let parsed, task =
    Live.run_result
      "I64 Zero=0,N=0;I64 Init(I64 x){N++;return (x/Zero)>>1;}I64 Saved(I64 \
       n=Init(7)){return n;}"
  in
  Live.expect_error "HCIRVM0009" parsed;
  Alcotest.(check int64)
    "reached native counter effect survives default failure" 1L
    (Live.read task "N;");
  List.iter
    (fun mode ->
      let failed = run mode source in
      (match integer_program_report_outcome failed with
      | Error (d :: _) ->
          Alcotest.(check string)
            "reached original division fault"
            (C.field projection "hosted_code")
            d.code;
          Alcotest.(check bool)
            "original callee owns the fault" true
            (List.mem "function=Init" d.notes)
      | _ -> Alcotest.fail "prepared reached division did not fault");
      let source = retained "return X/2;" in
      let report, e = success mode source in
      let prep = Option.get (integer_program_report_preparation_work report) in
      let steps = VM.executed_steps e in
      (match
         integer_program_report_outcome
           (run ~max_initializer_steps:prep ~max_steps:steps mode source)
       with
      | Ok _ -> ()
      | Error ds -> Alcotest.fail (A.diagnostics ds));
      List.iter
        (fun report ->
          match integer_program_report_outcome report with
          | Error (d :: _) ->
              Alcotest.(check string)
                "one below exact resource work" "HCIRVM0007" d.code
          | _ -> Alcotest.fail "one-below work limit succeeded")
        [
          run ~max_steps:(steps - 1) mode source;
          run ~max_initializer_steps:(prep - 1) mode source;
        ])
    modes

let tests =
  [
    Alcotest.test_case "nineteen captured saved/shift/narrow fields" `Quick
      values;
    Alcotest.test_case "original defaults, nested and live consumers" `Quick
      contexts;
    Alcotest.test_case "original, copied, foreign and changed records" `Quick
      authority;
    Alcotest.test_case "transitive inputs, full counts and new records" `Quick
      transitive_authority;
    Alcotest.test_case "reached preparation output remains once-only" `Quick
      output_effects;
    Alcotest.test_case "narrow range invariants and remaining boundaries" `Quick
      ranges_and_boundaries;
    Alcotest.test_case "captured fault phase and exact work limits" `Quick
      faults_and_limits;
  ]
