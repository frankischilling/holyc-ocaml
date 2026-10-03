open Holyc_lib
module C = Division_strength_reduction_cases
module A = Test_internal_strlen_authority
module S = Test_source_constant_shifts
module VM = Ir_integer_interpreter
module Seq = Ir_instruction_sequence
module Op = Ir_opcode
module Unit = Holyc_lib__Driver.Integer_unit
module Live = Test_live_initializer_execution

let modes = A.modes
let run = Test_pointer_equality.run
let success = Test_pointer_equality.success

let values () =
  let cases = C.cases () @ C.contextual_cases in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, expected) ->
          let _, execution = success mode source in
          (match VM.final_value execution with
          | Some word -> Alcotest.(check int64) label expected word.bits
          | None -> Alcotest.fail (label ^ " has no result"));
          let steps = VM.executed_steps execution in
          (match
             integer_program_report_outcome (run ~max_steps:steps mode source)
           with
          | Ok checked ->
              Alcotest.(check int)
                "exact runtime allowance" steps
                (VM.executed_steps checked.value)
          | Error ds -> Alcotest.fail (A.diagnostics ds));
          match
            integer_program_report_outcome
              (run ~max_steps:(steps - 1) mode source)
          with
          | Error (d :: _) ->
              Alcotest.(check string) "one-below runtime" "HCIRVM0007" d.code
          | _ -> Alcotest.fail "one-below runtime succeeded")
        cases;
      List.iter
        (fun (source, bytes) ->
          let report, execution = success mode source in
          Alcotest.(check (option int64))
            "ordered output arguments" (Some 42L)
            (Option.map (fun w -> w.VM.bits) (VM.final_value execution));
          Alcotest.(check string)
            "right-to-left argument effects" bytes
            (integer_program_report_output_bytes report))
        C.output_cases)
    modes

let functions source mode =
  let fixture = A.fixture ~source mode in
  Unit.functions fixture.Native_scalar_fixture.unit_
  |> List.concat_map (fun fn ->
      let descriptions = Ir_function_body.x87 fn.VM.body |> S.descriptions in
      S.dense_ids descriptions;
      descriptions)

let arithmetic descriptions =
  List.filter
    (fun (d : Seq.description) ->
      List.mem d.opcode
        [
          Op.Ic_div;
          Ic_mod;
          Ic_div_equ;
          Ic_mod_equ;
          Ic_shr_const;
          Ic_shr_equ;
          Ic_and;
          Ic_and_equ;
        ])
    descriptions

let shapes () =
  List.iter
    (fun mode ->
      List.iter
        (fun (expression, opcode, payload) ->
          let ds =
            functions ("I64 F(I64 x){return " ^ expression ^ ";}F(-7);") mode
          in
          match arithmetic ds with
          | [ d ] ->
              Alcotest.(check bool)
                "verified optimizer opcode" true (d.opcode = opcode);
              Alcotest.(check int64) "zero arithmetic flags" 0L d.flags;
              Alcotest.(check (option int64))
                "complete shift count" payload
                (match d.payload with
                | Some (Seq.Integer n) -> Some n
                | _ -> None)
          | _ -> Alcotest.fail "wrong optimized arithmetic shape")
        [
          ("x/2", Op.Ic_shr_const, Some 1L);
          ("x/0x8000000000000000", Op.Ic_shr_const, Some 63L);
          ("(x/2)>>63", Op.Ic_shr_const, Some 64L);
          ("(x/0x8000000000000000)/2", Op.Ic_shr_const, Some 64L);
          ("x%2", Op.Ic_mod, None);
          ("x/3", Op.Ic_div, None);
          ("x%0x8000000000000000", Op.Ic_and, None);
        ];
      Alcotest.(check int)
        "division by one is eliminated" 0
        (List.length
           (arithmetic (functions "I64 F(I64 x){return x/1(U64);}F(-7);" mode)));
      Alcotest.(check int)
        "fully constant arithmetic is folded" 0
        (List.length (arithmetic (functions "I64 F(){return -7/2;}F();" mode)));
      List.iter
        (fun (source, opcode) ->
          let ds = functions source mode in
          match arithmetic ds with
          | [ d ] ->
              Alcotest.(check bool) "compound reduction" true (d.opcode = opcode);
              Alcotest.(check int64) "compound flags" 0L d.flags;
              Alcotest.(check int)
                "address and fresh immediate" 2 (List.length d.operands)
          | _ -> Alcotest.fail "compound reduction has the wrong shape")
        [
          ("I64 F(){I8 x=-7;x/=2;return x;}F();", Op.Ic_shr_equ);
          ("I64 F(){I64 x=-7;I64 *p=&x;*p%=2;return x;}F();", Op.Ic_and_equ);
        ];
      let comparison source =
        functions source mode
        |> List.exists (fun (d : Seq.description) ->
            d.opcode = Op.Ic_holyc_typecast
            && d.target_type = Some Test_native_expression.u64)
      in
      Alcotest.(check bool)
        "division by one erases early unsigned class" false
        (comparison "I64 F(I64 x){return (x/1(U64))<0;}F(-7);");
      Alcotest.(check bool)
        "other divisors retain early unsigned comparison" true
        (comparison "I64 F(I64 x){return (x/2(U64))<0;}F(-7);"))
    modes

let phases () =
  List.iter
    (fun mode ->
      List.iter
        (fun (_, source) ->
          let report = run mode source in
          Alcotest.(check bool)
            "compilation fault has no execution" true
            (Option.is_none (integer_program_report_program report));
          match integer_program_report_outcome report with
          | Error (d :: _) ->
              Alcotest.(check string)
                "constant overflow compilation phase" "HCIRL0007" d.code
          | _ -> Alcotest.fail "constant overflow compiled")
        (C.compilation_faults ());
      List.iter
        (fun (label, source, code, bytes) ->
          let report = run mode source in
          (match integer_program_report_outcome report with
          | Error (d :: _) ->
              Alcotest.(check string) label code d.code;
              Alcotest.(check bool)
                "reached fault keeps source span" true
                (d.primary.stop > d.primary.start)
          | _ -> Alcotest.fail (label ^ " succeeded"));
          Alcotest.(check string)
            "reached output" bytes
            (integer_program_report_output_bytes report))
        C.reached_faults)
    modes;
  List.iter
    (fun mode ->
      let checked =
        Test_integer_expression.evaluate ~mode "-7/2;"
        |> Test_integer_expression.checked
      in
      Alcotest.(check int64)
        "raw expression contract remains truncating" (-3L)
        (Test_integer_expression.word checked).bits)
    modes

let preparation () =
  List.iter
    (fun mode ->
      let source =
        "I64 A=-7/2;I64 F(I64 n=-7%2){return n;}I64 B=9/3;A+B+F();"
      in
      let _, e = success mode source in
      let steps = VM.compiled_initializer_steps e in
      Alcotest.(check int) "three folded preparation harnesses" 9 steps;
      (match
         integer_program_report_outcome
           (run ~max_initializer_steps:steps mode source)
       with
      | Ok _ -> ()
      | Error ds -> Alcotest.fail (A.diagnostics ds));
      (match
         integer_program_report_outcome
           (run ~max_initializer_steps:(steps - 1) mode source)
       with
      | Error (d :: _) ->
          Alcotest.(check string) "one-below preparation" "HCIRVM0007" d.code
      | _ -> Alcotest.fail "one-below preparation succeeded");
      let _, retained = success mode C.retained in
      Alcotest.(check (option int64))
        "retained folded callee prepares once" (Some 42L)
        (Option.map (fun w -> w.VM.bits) (VM.final_value retained));
      List.iter
        (fun source ->
          match integer_program_report_outcome (run mode source) with
          | Error (d :: _) ->
              Alcotest.(check string)
                "nonconstant preparation remains gated" "HCRUN0006" d.code
          | _ -> Alcotest.fail "nonconstant preparation gate widened")
        C.preparation_boundary)
    modes;
  let source =
    "I64 N=0;I64 Touch(){N++;return -7/2;}I64 A[2]={40,Touch()};A[1]+N+44;"
  in
  let parsed, task = Live.run_result source in
  ignore (Test_parser.expect_ast parsed);
  Alcotest.(check (option int64))
    "live folded leaf stores once" (Some 42L)
    (Option.map
       (fun w -> w.VM.bits)
       (Integer_task.progress task).runtime.final_value)

let authority () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, opcode, expected) ->
          let original = A.fixture ~source mode
          and foreign = A.fixture ~source mode in
          A.valid_control ~expected original;
          A.rejects "foreign source reduction context"
            (A.execute
               ~runtime_calls:(Unit.runtime_calls foreign.unit_)
               original);
          A.rejects "foreign native reduction context"
            (A.compile
               ~runtime_calls:(Unit.runtime_calls foreign.unit_)
               original);
          List.iter
            (fun transform ->
              let changed = A.fixture ~source mode in
              let cell, d = S.find_cell changed opcode in
              Obj.set_field (Obj.repr cell) 0 (Obj.repr (transform d));
              A.rejects "changed source division reduction" (A.execute changed);
              A.rejects "changed native division reduction" (A.compile changed))
            [
              (fun (d : Seq.description) -> { d with flags = 1L });
              (fun d -> { d with operands = List.map Fun.id d.operands });
              (fun d -> { d with span = None });
            ];
          let changed = A.fixture ~source mode in
          let cell, d = S.find_cell changed opcode in
          Obj.set_field (Obj.repr cell) 0
            (Obj.repr { d with opcode = Op.Ic_xor });
          A.rejects "changed reduction opcode" (A.execute changed);
          A.rejects "changed native reduction opcode" (A.compile changed))
        [
          ("I64 F(I64 x){return (x/2)>>63;}F(-7);", Op.Ic_shr_const, -7L);
          ("U64 F(U64 x){return x%64;}F(-1);", Op.Ic_and, 63L);
          ("I64 F(I64 x){x/=64;return x;}F(-7);", Op.Ic_shr_equ, -1L);
          ("I64 F(I64 x){x%=64;return x;}F(-7);", Op.Ic_and_equ, 57L);
        ];
      let changed =
        A.fixture ~source:"I64 F(I64 x,I64 y){return x/y;}F(7,2);" mode
      in
      let cell, d = S.find_cell changed Op.Ic_div in
      Obj.set_field (Obj.repr cell) 0 (Obj.repr { d with opcode = Op.Ic_and });
      A.rejects "new mask cannot acquire source authority" (A.execute changed);
      A.rejects "new native mask cannot acquire source authority"
        (A.compile changed))
    modes

let tests =
  [
    Alcotest.test_case "38 repeated native fields and source contexts" `Quick
      values;
    Alcotest.test_case "literal, compound and following comparison shapes"
      `Quick shapes;
    Alcotest.test_case "compilation and reached arithmetic fault phases" `Quick
      phases;
    Alcotest.test_case "folded retained preparation and exact allowances" `Quick
      preparation;
    Alcotest.test_case "original transitive reduction ownership" `Quick
      authority;
  ]
