open Holyc_lib
module A = Test_internal_strlen_authority
module T = Test_pointer_bit_internals
module F = Test_constant_shifts
module VM = Ir_integer_interpreter
module Seq = Ir_instruction_sequence
module Graph = Ir_block_graph
module Unit = Holyc_lib__Driver.Integer_unit
module Op = Ir_opcode
module Type = Semantic_type
module Live = Test_live_initializer_execution

let modes = T.modes
let run = Test_pointer_equality.run
let success = Test_pointer_equality.success
let cases = Source_constant_shift_cases.cases

let values () =
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, expected) ->
          let _, execution = success mode source in
          match VM.final_value execution with
          | Some word -> Alcotest.(check int64) label expected word.bits
          | None -> Alcotest.fail (label ^ " has no word"))
        cases;
      let fixture = F.fixture () in
      List.iter
        (fun projection ->
          let _, execution =
            success mode (F.field projection "holy_c_source")
          in
          match VM.final_value execution with
          | Some word ->
              Alcotest.(check int64)
                (F.field projection "field")
                (F.observed fixture projection "case_id")
                word.bits
          | None -> Alcotest.fail "captured source has no word")
        (F.projections fixture))
    modes

let descriptions graph =
  Ir_x87_stack.graph graph |> Graph.blocks
  |> List.concat_map (fun block ->
      Graph.instructions block |> Seq.instructions |> List.map Seq.description)

let body fixture =
  let function_ =
    List.hd (Unit.functions fixture.Native_scalar_fixture.unit_)
  in
  Ir_function_body.x87 function_.VM.body

let shape source mode = A.fixture ~source mode |> body |> descriptions

let shifts ds =
  List.filter
    (fun (d : Seq.description) ->
      List.mem d.opcode [ Op.Ic_shl; Ic_shr; Ic_shl_const; Ic_shr_const ])
    ds

let dense_ids ds =
  let ids =
    List.map
      (fun (d : Seq.description) -> Seq.Instruction_id.to_int d.instruction_id)
      ds
    |> List.sort compare
  in
  Alcotest.(check (list int))
    "consecutive original instruction identities"
    (List.init (List.length ids) Fun.id)
    ids

let public_classes () =
  let fixture = Public_shift_class_cases.fixture () in
  List.iter
    (fun mode ->
      List.iter
        (fun projection ->
          let source =
            Public_shift_class_cases.field projection "holy_c_source"
          in
          let _, execution = success mode source in
          let expected =
            Public_shift_class_cases.observed fixture projection "case_id"
          in
          let expected_type =
            if Public_shift_class_cases.field projection "result_type" = "U64"
            then VM.U64
            else VM.I64
          in
          (match VM.final_value execution with
          | Some word ->
              Alcotest.(check int64)
                "captured public-class bits" expected word.bits;
              Alcotest.(check bool)
                "declared result class" true
                (word.type_ = expected_type)
          | None -> Alcotest.fail "public-class source returned no word");
          let steps = VM.executed_steps execution in
          (match
             integer_program_report_outcome (run ~max_steps:steps mode source)
           with
          | Ok checked ->
              Alcotest.(check int)
                "exact public-class allowance" steps
                (VM.executed_steps checked.value)
          | Error ds -> Alcotest.fail (A.diagnostics ds));
          (match
             integer_program_report_outcome
               (run ~max_steps:(steps - 1) mode source)
           with
          | Error (d :: _) ->
              Alcotest.(check string)
                "one-below public-class allowance" "HCIRVM0007" d.code
          | _ -> Alcotest.fail "one-below public-class execution succeeded");
          let unit_ = (A.fixture ~source mode).Native_scalar_fixture.unit_ in
          let ds =
            Unit.functions unit_
            |> List.concat_map (fun fn ->
                let ds = Ir_function_body.x87 fn.VM.body |> descriptions in
                dense_ids ds;
                ds)
          in
          let field = Public_shift_class_cases.field projection "field" in
          match shifts ds with
          | [ d ] ->
              Alcotest.(check bool)
                "canonical right shift" true
                (d.opcode = Op.Ic_shr_const);
              Alcotest.(check int64) "zero shift flags" 0L d.flags;
              Alcotest.(check bool)
                "complete immediate count" true
                (d.payload = Some (Seq.Integer 1L));
              let primitive =
                if field = "NPUB" then Primitive_type.I64 else U64
              in
              Alcotest.(check bool)
                "forwarded consumer computation class" true
                (Option.fold ~none:false
                   ~some:(fun t ->
                     Type.base t
                     = Type.Primitive (Type.Internal_storage, primitive))
                   d.target_type)
          | _ ->
              Alcotest.fail "public-class shift has the wrong canonical shape")
        (Public_shift_class_cases.projections fixture))
    modes

let shapes () =
  List.iter
    (fun mode ->
      List.iter
        (fun (expression, opcode, count) ->
          let ds =
            shape ("I64 F(I64 x){return " ^ expression ^ ";}F(-7);") mode
          in
          dense_ids ds;
          match shifts ds with
          | [ d ] ->
              Alcotest.(check bool) "constant opcode" true (d.opcode = opcode);
              Alcotest.(check (option int64))
                "complete accumulated count" (Some count)
                (match d.payload with
                | Some (Seq.Integer n) -> Some n
                | _ -> None);
              Alcotest.(check int) "one word operand" 1 (List.length d.operands);
              Alcotest.(check int64) "unflagged canonical operation" 0L d.flags;
              Alcotest.(check bool)
                "restored internal signed class" true
                (Option.fold ~none:false
                   ~some:(fun t ->
                     Type.base t
                     = Type.Primitive (Type.Internal_storage, Primitive_type.I64))
                   d.target_type)
          | _ -> Alcotest.fail "nested source omitted a unique constant shift")
        [
          ("(x<<63)<<1", Op.Ic_shl_const, 64L);
          ("(x>>63)>>2", Op.Ic_shr_const, 65L);
          ("(x<<-1)<<1", Op.Ic_shl_const, 0L);
          ("(x<<0x7FFFFFFFFFFFFFFF)<<1", Op.Ic_shl_const, Int64.min_int);
          ("(x>>0xFFFFFFFFFFFFFFFF)>>2", Op.Ic_shr_const, 1L);
        ];
      let literal = shape "I64 F(){return (-7<<63)<<1;}F();" mode in
      Alcotest.(check int)
        "fully constant tree has no shift" 0
        (List.length (shifts literal));
      dense_ids literal;
      Alcotest.(check int)
        "fully folded tree retains one word" 1
        (List.length
           (List.filter
              (fun (d : Seq.description) -> d.opcode = Op.Ic_imm_i64)
              literal));
      let opposite = shape "I64 F(I64 x){return (x<<63)>>1;}F(-7);" mode in
      Alcotest.(check int)
        "opposite shifts remain distinct" 2
        (List.length (shifts opposite));
      let variable = shape "I64 F(I64 x,U64 n){return x>>n;}F(-7,1);" mode in
      Alcotest.(check bool)
        "variable count keeps raw shift" true
        (List.exists
           (fun (d : Seq.description) ->
             d.opcode = Op.Ic_shr && d.payload = None)
           variable);
      let sticky =
        shape "I64 F(I64 x){return (x>>0x8000000000000001)<0;}F(-7);" mode
      in
      dense_ids sticky;
      Alcotest.(check bool)
        "sticky comparison retains unsigned word view" true
        (List.exists
           (fun (d : Seq.description) ->
             d.opcode = Op.Ic_holyc_typecast
             && d.payload = Some (Seq.Integer 0L)
             && Option.fold ~none:false
                  ~some:(fun t ->
                    Type.base t
                    = Type.Primitive (Type.Internal_storage, Primitive_type.U64))
                  d.target_type)
           sticky);
      Alcotest.(check bool)
        "comparison flags stay within existing consumer domain" true
        (List.for_all (fun (d : Seq.description) -> d.flags = 0L) sticky))
    modes

let find_cell fixture opcode =
  body fixture |> Ir_x87_stack.graph |> Graph.blocks
  |> List.find_map (fun block ->
      let rec find = function
        | [] -> None
        | instruction :: rest as cell ->
            let d = Seq.description instruction in
            if d.opcode = opcode then Some (cell, d) else find rest
      in
      find (Graph.instructions block |> Seq.instructions))
  |> Option.get

let authority () =
  let source = "I64 F(I64 x){return (x<<63)<<1;}F(-7);" in
  List.iter
    (fun mode ->
      let original = A.fixture ~source mode
      and foreign = A.fixture ~source mode in
      A.valid_control ~expected:(-7L) original;
      let changed =
        A.fixture ~source:"I64 F(I64 x,I64 n){return x>>n;}F(-7,1);" mode
      in
      A.valid_control ~expected:(-4L) changed;
      let cell, d = find_cell changed Op.Ic_shr in
      let shifted = List.hd d.operands in
      Obj.set_field (Obj.repr cell) 0
        (Obj.repr
           {
             d with
             opcode = Op.Ic_shr_const;
             operands = [ shifted ];
             payload = Some (Seq.Integer 2L);
           });
      A.rejects "new constant shift cannot acquire original source authority"
        (A.execute changed);
      A.rejects
        "new native constant shift cannot acquire original source authority"
        (A.compile changed);
      A.rejects "foreign source shift context"
        (A.execute ~runtime_calls:(Unit.runtime_calls foreign.unit_) original);
      A.rejects "foreign native shift context"
        (A.compile ~runtime_calls:(Unit.runtime_calls foreign.unit_) original);
      let changed = A.fixture ~source mode in
      let cell, input = find_cell changed Op.Ic_deref in
      Obj.set_field (Obj.repr cell) 0
        (Obj.repr { input with operands = List.map Fun.id input.operands });
      A.rejects "copied transitive shift input" (A.execute changed);
      A.rejects "copied native transitive shift input" (A.compile changed);
      List.iter
        (fun transform ->
          let changed = A.fixture ~source mode in
          let cell, d = find_cell changed Op.Ic_shl_const in
          Obj.set_field (Obj.repr cell) 0 (Obj.repr (transform d));
          A.rejects "changed source shift" (A.execute changed);
          A.rejects "changed native source shift" (A.compile changed))
        [
          (fun (d : Seq.description) ->
            { d with payload = Some (Seq.Integer 0L) });
          (fun d -> { d with flags = 1L });
          (fun d -> { d with operands = List.map Fun.id d.operands });
          (fun d -> { d with opcode = Op.Ic_shr_const });
          (fun d -> { d with target_type = Some F.F.u64 });
          (fun d -> { d with span = None });
        ])
    modes

let retained = Source_constant_shift_cases.retained

let retained_values () =
  List.iter
    (fun mode ->
      let _, e = success mode retained in
      T.word "retained folded shift" VM.I64 42L e)
    modes;
  let on_observed task = function
    | Parser.Global_initializer_leaf_completed _ as event ->
        let before = Integer_task.progress task in
        Alcotest.(check bool)
          "folded leaf cannot execute twice" true
          (Result.is_error (Integer_task.observe_initializer task event));
        let clone = Obj.obj (Obj.dup (Obj.repr event)) in
        Alcotest.(check bool)
          "equal copied leaf cannot claim source ownership" true
          (Result.is_error (Integer_task.observe_initializer task clone));
        Alcotest.(check bool)
          "rejections retain exact task state" true
          (before = Integer_task.progress task)
    | _ -> ()
  in
  let text =
    "I64 N=0;I64 Touch(){N++;return 1<<2;}I64 A[2]={40,Touch()};A[1]+N+37;"
  in
  let parsed, task = Live.run_result ~on_observed text in
  ignore (Test_parser.expect_ast parsed);
  Alcotest.(check (option int64))
    "live folded callee stores once" (Some 42L)
    (Option.map
       (fun word -> word.VM.bits)
       (Integer_task.progress task).runtime.final_value);
  let parsed, task = Live.run_result "I64 A[2]={1<<2,Missing};" in
  Alcotest.(check bool)
    "later typing fault remains visible" true (Parser.has_errors parsed);
  Alcotest.(check int64)
    "earlier folded leaf survives" 4L (Live.read task "A[0];")

let limit_source = Source_constant_shift_cases.limit_source
let fault_source = Source_constant_shift_cases.fault_source

let ordered_output () =
  List.iter
    (fun mode ->
      let report, execution =
        success mode Source_constant_shift_cases.output_source
      in
      T.word "output argument effects" VM.I64 42L execution;
      Alcotest.(check string)
        "output evaluates shifted arguments right to left" "2:1;"
        (integer_program_report_output_bytes report);
      List.iter
        (fun (source, bytes) ->
          let failed = run mode source in
          (match integer_program_report_outcome failed with
          | Error (d :: _) ->
              Alcotest.(check string)
                "reached operand or consumer division" "HCIRVM0009" d.code
          | _ -> Alcotest.fail "reached operand fault disappeared");
          Alcotest.(check string)
            "output from original reached producer" bytes
            (integer_program_report_output_bytes failed))
        Source_constant_shift_cases.fault_cases)
    modes

let limits_and_faults () =
  List.iter
    (fun mode ->
      let report, e = success mode limit_source in
      let steps = VM.executed_steps e in
      let exact = run ~max_steps:steps mode limit_source in
      (match integer_program_report_outcome exact with
      | Ok checked -> T.word "exact runtime" VM.I64 42L checked.value
      | Error ds -> Alcotest.fail (A.diagnostics ds));
      let below = run ~max_steps:(steps - 1) mode limit_source in
      (match integer_program_report_outcome below with
      | Error (d :: _) ->
          Alcotest.(check string) "one-below runtime" "HCIRVM0007" d.code
      | _ -> Alcotest.fail "one-below runtime succeeded");
      Alcotest.(check string)
        "reached output" "kept"
        (integer_program_report_output_bytes below);
      Alcotest.(check int)
        "reached formatting work"
        (integer_program_report_output_work report)
        (integer_program_report_output_work below);
      let prep_source =
        "I64 A=1<<3;I64 F(I64 n=(-7<<63)<<1){return n;}I64 B=2<<1;A+B+F();"
      in
      let _, e = success mode prep_source in
      T.word "folded preparations" VM.I64 12L e;
      let prep = VM.compiled_initializer_steps e in
      Alcotest.(check int) "three folded preparation harnesses" 9 prep;
      let exact = run ~max_initializer_steps:prep mode prep_source in
      (match integer_program_report_outcome exact with
      | Ok _ -> ()
      | Error ds -> Alcotest.fail (A.diagnostics ds));
      (match
         integer_program_report_outcome
           (run ~max_initializer_steps:(prep - 1) mode prep_source)
       with
      | Error (d :: _) ->
          Alcotest.(check string) "one-below preparation" "HCIRVM0007" d.code
      | _ -> Alcotest.fail "one-below preparation succeeded");
      let fault = run mode fault_source in
      (match integer_program_report_outcome fault with
      | Error (d :: _) ->
          Alcotest.(check string)
            "following division still faults" "HCIRVM0009" d.code
      | _ -> Alcotest.fail "division fault disappeared");
      Alcotest.(check string)
        "output before following fault" "kept"
        (integer_program_report_output_bytes fault))
    modes;
  let text =
    "I64 N=0;I64 Touch(){N++;return 1<<2;}I64 A[2]={40,Touch()};A[1]+N+37;"
  in
  let parsed, task = Live.run_result text in
  ignore (Test_parser.expect_ast parsed);
  let prep = Integer_task.initializer_steps task in
  let parsed, _ = Live.run_result ~max_initializer_steps:prep text in
  ignore (Test_parser.expect_ast parsed);
  let parsed, failed = Live.run_result ~max_initializer_steps:(prep - 1) text in
  Live.expect_error "HCIRVM0007" parsed;
  Alcotest.(check int)
    "failed live preparation remains charged" (prep - 1)
    (Integer_task.initializer_steps failed)

let rejected = Source_constant_shift_cases.rejected

let boundaries () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          match integer_program_report_outcome (run mode source) with
          | Error (d :: _) ->
              Alcotest.(check string)
                "nonconstant preparation gate" "HCRUN0006" d.code
          | _ -> Alcotest.fail "nonconstant preparation widened")
        rejected;
      let narrow = shape "I64 F(I8 x){return x<<2;}F(1);" mode in
      Alcotest.(check bool)
        "narrow operand remains outside rewrite" true
        (List.exists (fun (d : Seq.description) -> d.opcode = Op.Ic_shl) narrow);
      let shared =
        shape "I64 F(I64 x){return 0<(x>>0x8000000000000001)<2;}F(-7);" mode
      in
      Alcotest.(check bool)
        "shared comparison operand remains raw" true
        (List.exists (fun (d : Seq.description) -> d.opcode = Op.Ic_shr) shared))
    modes

let tests =
  [
    Alcotest.test_case "captured fields and following consumers" `Quick values;
    Alcotest.test_case "public call and cast classes survive negation" `Quick
      public_classes;
    Alcotest.test_case "complete count payloads and dense source identities"
      `Quick shapes;
    Alcotest.test_case "transitive original source ownership" `Quick authority;
    Alcotest.test_case "retained and live preparation evaluates once" `Quick
      retained_values;
    Alcotest.test_case "reached faults, output and exact allowances" `Quick
      limits_and_faults;
    Alcotest.test_case "shifted output arguments and operand faults" `Quick
      ordered_output;
    Alcotest.test_case "remaining preparation and shared-value boundaries"
      `Quick boundaries;
  ]
