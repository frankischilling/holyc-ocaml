module Flow = Holyc_lib.Ir_control_flow
module Opcode = Holyc_lib.Ir_opcode

let kind_equal left right = left = right

let kind_testable =
  Alcotest.testable
    (fun formatter kind ->
      Format.pp_print_string formatter (Flow.kind_name kind))
    kind_equal

let target_equal left right = left = right

let target_testable =
  Alcotest.testable
    (fun formatter shape ->
      Format.pp_print_string formatter (Flow.target_shape_name shape))
    target_equal

let check_kind message expected opcode =
  Alcotest.check kind_testable message expected (Flow.classify opcode)

let complete_partition () =
  let counts = Hashtbl.create 7 in
  List.iter
    (fun opcode ->
      let kind = Flow.classify opcode in
      Hashtbl.replace counts kind
        (1 + Option.value ~default:0 (Hashtbl.find_opt counts kind)))
    Opcode.all;
  let count kind = Option.value ~default:0 (Hashtbl.find_opt counts kind) in
  Alcotest.(check int) "all opcodes" 185 (List.length Opcode.all);
  Alcotest.(check int) "fallthrough" 147 (count Flow.Fallthrough);
  Alcotest.(check int) "label" 1 (count Flow.Label);
  Alcotest.(check int)
    "unconditional branch" 1
    (count Flow.Unconditional_branch);
  Alcotest.(check int) "conditional branches" 32 (count Flow.Conditional_branch);
  Alcotest.(check int) "switches" 2 (count Flow.Switch);
  Alcotest.(check int) "returns" 1 (count Flow.Return);
  Alcotest.(check int) "end markers" 1 (count Flow.End)

let conditional_range () =
  let expected =
    List.init 32 (fun offset -> Opcode.of_code (0x85 + offset) |> Option.get)
  in
  let actual =
    List.filter
      (fun opcode -> Flow.classify opcode = Flow.Conditional_branch)
      Opcode.all
  in
  Alcotest.(check (list int))
    "complete contiguous range"
    (List.map Opcode.to_code expected)
    (List.map Opcode.to_code actual);
  check_kind "before range" Flow.Fallthrough Opcode.Ic_strlen;
  check_kind "after range" Flow.Fallthrough Opcode.Ic_swap_u8

let explicit_transfers () =
  check_kind "stream end" Flow.End Opcode.Ic_end;
  check_kind "label" Flow.Label Opcode.Ic_label;
  check_kind "jump" Flow.Unconditional_branch Opcode.Ic_jmp;
  check_kind "bounded switch" Flow.Switch Opcode.Ic_switch;
  check_kind "no-bound switch" Flow.Switch Opcode.Ic_nobound_switch;
  check_kind "return" Flow.Return Opcode.Ic_ret

let source_distinctions () =
  check_kind "return value" Flow.Fallthrough Opcode.Ic_return_val;
  check_kind "top-level return value" Flow.Fallthrough Opcode.Ic_return_val2;
  check_kind "local subroutine call" Flow.Fallthrough Opcode.Ic_sub_call;
  check_kind "ordinary call" Flow.Fallthrough Opcode.Ic_call

let target_shapes () =
  Alcotest.check target_testable "jump target" Flow.Single_target
    (Flow.target_shape Opcode.Ic_jmp);
  Alcotest.check target_testable "conditional target" Flow.Single_target
    (Flow.target_shape Opcode.Ic_br_less);
  Alcotest.check target_testable "switch targets" Flow.Switch_targets
    (Flow.target_shape Opcode.Ic_switch);
  Alcotest.check target_testable "label has no outgoing target" Flow.No_target
    (Flow.target_shape Opcode.Ic_label);
  Alcotest.check target_testable "call target is not a block target"
    Flow.No_target
    (Flow.target_shape Opcode.Ic_call)

let block_boundaries () =
  List.iter
    (fun opcode ->
      Alcotest.(check bool)
        (Opcode.to_source_name opcode ^ " ends block")
        true (Flow.ends_block opcode))
    [
      Opcode.Ic_end;
      Opcode.Ic_jmp;
      Opcode.Ic_switch;
      Opcode.Ic_nobound_switch;
      Opcode.Ic_ret;
      Opcode.Ic_br_zero;
      Opcode.Ic_br_not_btc;
    ];
  Alcotest.(check bool)
    "ordinary instruction stays in block" false
    (Flow.ends_block Opcode.Ic_add);
  Alcotest.(check bool)
    "label starts boundary" true
    (Flow.starts_label Opcode.Ic_label);
  Alcotest.(check bool)
    "jump does not start label" false
    (Flow.starts_label Opcode.Ic_jmp)

let fallthrough_edges () =
  List.iter
    (fun opcode ->
      Alcotest.(check bool)
        (Opcode.to_source_name opcode ^ " falls through")
        true
        (Flow.may_fall_through opcode))
    [ Opcode.Ic_add; Opcode.Ic_label; Opcode.Ic_br_zero ];
  List.iter
    (fun opcode ->
      Alcotest.(check bool)
        (Opcode.to_source_name opcode ^ " stops fallthrough")
        false
        (Flow.may_fall_through opcode))
    [
      Opcode.Ic_end;
      Opcode.Ic_jmp;
      Opcode.Ic_switch;
      Opcode.Ic_nobound_switch;
      Opcode.Ic_ret;
    ]

let deterministic_names () =
  Alcotest.(check (list string))
    "kind names"
    [
      "fallthrough";
      "label";
      "unconditional-branch";
      "conditional-branch";
      "switch";
      "return";
      "end";
    ]
    (List.map Flow.kind_name
       [
         Flow.Fallthrough;
         Flow.Label;
         Flow.Unconditional_branch;
         Flow.Conditional_branch;
         Flow.Switch;
         Flow.Return;
         Flow.End;
       ]);
  Alcotest.(check (list string))
    "target names"
    [ "none"; "single"; "switch-targets" ]
    (List.map Flow.target_shape_name
       [ Flow.No_target; Flow.Single_target; Flow.Switch_targets ])

let tests =
  [
    Alcotest.test_case "complete partition" `Quick complete_partition;
    Alcotest.test_case "conditional range" `Quick conditional_range;
    Alcotest.test_case "explicit transfers" `Quick explicit_transfers;
    Alcotest.test_case "source distinctions" `Quick source_distinctions;
    Alcotest.test_case "target shapes" `Quick target_shapes;
    Alcotest.test_case "block boundaries" `Quick block_boundaries;
    Alcotest.test_case "fallthrough edges" `Quick fallthrough_edges;
    Alcotest.test_case "deterministic names" `Quick deterministic_names;
  ]
