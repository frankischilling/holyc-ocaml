open Test_ir_integer_interpreter

let opcodes = [ Opcode.Ic_div; Opcode.Ic_mod ]

let check_pair label left_type left_bits right_type right_bits result_type
    expected_type quotient remainder =
  List.iter
    (fun (opcode, expected) ->
      execute_binary ~opcode ~left_type ~left_bits ~right_type ~right_bits
        ~result_type
      |> check_word
           (label ^ " " ^ Opcode.to_source_name opcode)
           expected_type expected)
    [ (Opcode.Ic_div, quotient); (Opcode.Ic_mod, remainder) ]

let signed_results () =
  List.iter
    (fun (left, right, quotient, remainder) ->
      check_pair
        (Printf.sprintf "%Ld / %Ld" left right)
        i64 left i64 right i64 VM.I64 quotient remainder)
    [
      (7L, 3L, 2L, 1L);
      (-7L, 3L, -2L, -1L);
      (7L, -3L, -2L, 1L);
      (-7L, -3L, 2L, -1L);
      (-6L, 3L, -2L, 0L);
      (0L, -3L, 0L, 0L);
      (2L, 7L, 0L, 2L);
      (-2L, 7L, 0L, -2L);
      (Int64.min_int, 1L, Int64.min_int, 0L);
      (Int64.min_int, 3L, -3074457345618258602L, -2L);
      (Int64.min_int, -3L, 3074457345618258602L, -2L);
      (Int64.min_int, Int64.min_int, 1L, 0L);
      (Int64.max_int, -1L, -9223372036854775807L, 0L);
      (Int64.max_int, Int64.min_int, 0L, Int64.max_int);
      (-1L, Int64.min_int, 0L, -1L);
    ]

let unsigned_and_mixed_results () =
  List.iter
    (fun (left_type, right_type) ->
      List.iter
        (fun (left, right, quotient, remainder) ->
          check_pair "unsigned promotion" left_type left right_type right u64
            VM.U64 quotient remainder)
        [
          (7L, 3L, 2L, 1L);
          (-1L, 1L, -1L, 0L);
          (-1L, 2L, Int64.max_int, 1L);
          (-1L, 3L, 6148914691236517205L, 0L);
          (-1L, Int64.min_int, 1L, Int64.max_int);
          (Int64.min_int, -1L, 0L, Int64.min_int);
          (7L, -3L, 0L, 7L);
          (0L, -1L, 0L, 0L);
          (-1L, -1L, 1L, 0L);
        ])
    [ (i64, u64); (u64, i64); (u64, u64) ]

let arithmetic_graph ~left_type ~right_type ~result_type opcode left right =
  verified ~entry:0
    [
      block 0
        [
          imm ~id:0 ~value:0 ~type_:left_type left;
          imm ~id:1 ~value:1 ~type_:right_type right;
          {
            (binary ~id:2 ~left:0 ~right:1 ~value:2 ~type_:result_type opcode) with
            span = Some (span 12 17);
          };
          return_value ~id:3 ~operand:2 ~type_:result_type;
          ret 4;
        ];
    ]

let faults_and_budgets () =
  List.iter
    (fun opcode ->
      List.iter
        (fun (left_type, right_type, result_type, left, right, code) ->
          let graph =
            arithmetic_graph ~left_type ~right_type ~result_type opcode left
              right
          in
          let fault = require_errors ~max_steps:3 graph |> only_error code in
          check_stage "arithmetic fault" VM.Execution fault;
          Alcotest.(check int)
            "faulting instruction costs one step" 3 fault.executed_steps;
          Alcotest.(check (option int)) "faulting block" (Some 0) fault.block_id;
          Alcotest.(check (option int))
            "faulting instruction" (Some 2) fault.instruction_id;
          Alcotest.(check bool)
            "faulting source" true
            (fault.span = Some (span 12 17));
          let exhausted =
            require_errors ~max_steps:2 graph |> only_error "HCIRVM0007"
          in
          Alcotest.(check int)
            "budget stops before arithmetic" 2 exhausted.executed_steps)
        [
          (i64, i64, i64, 7L, 0L, "HCIRVM0009");
          (i64, u64, u64, 7L, 0L, "HCIRVM0009");
          (u64, i64, u64, -1L, 0L, "HCIRVM0009");
          (u64, u64, u64, -1L, 0L, "HCIRVM0009");
          (i64, i64, i64, Int64.min_int, -1L, "HCIRVM0010");
        ];
      let finite =
        arithmetic_graph ~left_type:i64 ~right_type:i64 ~result_type:i64 opcode
          7L 3L
      in
      let completed = require_execution ~max_steps:5 finite in
      Alcotest.(check int)
        "exact successful budget" 5
        (VM.executed_steps completed);
      require_errors ~max_steps:4 finite |> has_code "HCIRVM0007")
    opcodes

let faults_follow_control_flow () =
  List.iter
    (fun opcode ->
      let fault_block =
        block 1
          [
            imm ~id:2 ~value:2 ~type_:i64 7L;
            imm ~id:3 ~value:3 ~type_:i64 0L;
            binary ~id:4 ~left:2 ~right:3 ~value:4 ~type_:i64 opcode;
            ret 5;
          ]
      in
      let graph condition =
        verified ~entry:0
          [
            block 0
              [
                imm ~id:0 ~value:0 ~type_:i64 condition;
                branch ~id:1 ~operand:0 ~target:2 Opcode.Ic_br_zero;
              ];
            fault_block;
            block 2 [ ret 6 ];
          ]
      in
      let skipped = require_execution ~max_steps:3 (graph 0L) in
      Alcotest.(check int)
        "skipped division costs no steps" 3
        (VM.executed_steps skipped);
      require_errors (graph 1L) |> has_code "HCIRVM0009";
      let graph_with_pending_return =
        verified ~entry:0
          [
            block 0
              [
                imm ~id:0 ~value:0 ~type_:i64 42L;
                return_value ~id:1 ~operand:0 ~type_:i64;
                jump ~id:6 ~target:1;
              ];
            fault_block;
          ]
      in
      require_errors graph_with_pending_return |> has_code "HCIRVM0009")
    opcodes

let preflight_precedes_arithmetic () =
  List.iter
    (fun opcode ->
      let graph =
        verified ~entry:0
          [
            block 0
              [
                imm ~id:0 ~value:0 ~type_:i64 1L;
                imm ~id:1 ~value:1 ~type_:i64 0L;
                binary ~id:2 ~left:0 ~right:1 ~value:2 ~type_:i64 opcode;
                ret 3;
              ];
            block 1
              [
                imm ~id:4 ~value:4 ~type_:i64 7L;
                imm ~id:5 ~value:5 ~type_:u64 3L;
                binary ~flags:1L ~id:6 ~left:4 ~right:5 ~value:6 ~type_:u64
                  opcode;
                description
                  ~operands:[ value_id 4; value_id 5 ]
                  ~result:(result 7) ~target_type:u64
                  ~payload:(Sequence.Integer 0L) 7 opcode;
                binary ~id:8 ~left:4 ~right:5 ~value:8 ~type_:u8 opcode;
                binary ~id:9 ~left:4 ~right:5 ~value:9 ~type_:i64 opcode;
                ret 10;
              ];
          ]
      in
      let errors = require_errors graph in
      Alcotest.(check (list string))
        "ordered validation errors"
        [ "HCIRVM0003"; "HCIRVM0004"; "HCIRVM0005"; "HCIRVM0006" ]
        (List.map (fun (error : VM.error) -> error.code) errors);
      List.iter
        (fun (error : VM.error) ->
          check_stage "preflight" VM.Preflight error;
          Alcotest.(check int)
            "invalid graph executes nothing" 0 error.executed_steps)
        errors)
    opcodes

(* Bit-at-a-time unsigned long division is independent of the host div/rem
   operations used by the VM. The carry retains the shifted-out 65th bit. *)
let unsigned_quotient_remainder dividend divisor =
  let quotient = ref 0L and remainder = ref 0L in
  for bit = 63 downto 0 do
    let carry = Int64.compare !remainder 0L < 0 in
    remainder :=
      Int64.logor
        (Int64.shift_left !remainder 1)
        (Int64.logand (Int64.shift_right_logical dividend bit) 1L);
    if carry || unsigned_order !remainder divisor >= 0 then (
      remainder := Int64.sub !remainder divisor;
      quotient := Int64.logor !quotient (Int64.shift_left 1L bit))
  done;
  (!quotient, !remainder)

let division_property =
  QCheck.Test.make ~count:500
    ~name:"integer quotient and remainder match independent long division"
    QCheck.(triple int64 int64 (int_bound 3))
    (fun (left, raw_right, selector) ->
      let right = if raw_right = 0L then 1L else raw_right in
      let left_type, left_word_type = sema_type_and_word_type selector in
      let right_type, right_word_type =
        sema_type_and_word_type (selector lsr 1)
      in
      let signed = left_word_type = VM.I64 && right_word_type = VM.I64 in
      if signed && left = Int64.min_int && right = -1L then true
      else
        let magnitude bits = if bits < 0L then Int64.neg bits else bits in
        let quotient, remainder =
          if signed then
            let quotient, remainder =
              unsigned_quotient_remainder (magnitude left) (magnitude right)
            in
            ( (if left < 0L <> (right < 0L) then Int64.neg quotient else quotient),
              if left < 0L then Int64.neg remainder else remainder )
          else unsigned_quotient_remainder left right
        in
        let result_type, expected_type =
          if signed then (i64, VM.I64) else (u64, VM.U64)
        in
        List.for_all
          (fun (opcode, expected) ->
            let word =
              execute_binary ~opcode ~left_type ~left_bits:left ~right_type
                ~right_bits:right ~result_type
            in
            word.type_ = expected_type && word.bits = expected)
          [ (Opcode.Ic_div, quotient); (Opcode.Ic_mod, remainder) ])

let tests =
  [
    Alcotest.test_case "signed quotient and remainder" `Quick signed_results;
    Alcotest.test_case "unsigned and mixed words" `Quick
      unsigned_and_mixed_results;
    Alcotest.test_case "arithmetic faults and exact budgets" `Quick
      faults_and_budgets;
    Alcotest.test_case "fault reachability and pending return" `Quick
      faults_follow_control_flow;
    Alcotest.test_case "whole-graph preflight before arithmetic" `Quick
      preflight_precedes_arithmetic;
    QCheck_alcotest.to_alcotest division_property;
  ]
