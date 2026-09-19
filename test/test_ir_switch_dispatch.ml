open Holyc_lib
module VM = Ir_integer_interpreter
module Program = X86_64_program
module Sequence = Ir_instruction_sequence
module Graph = Ir_block_graph
module X87 = Ir_x87_stack
module Opcode = Ir_opcode
module Type = Semantic_type
module Primitive = Primitive_type

let require_ok show = function
  | Ok value -> value
  | Error error -> Alcotest.fail (show error)

let sequence_error (error : Sequence.error) = error.code ^ ": " ^ error.message

let graph_errors errors =
  errors
  |> List.map (fun (error : Graph.error) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let x87_errors errors =
  errors
  |> List.map (fun (error : X87.error) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let vm_errors errors =
  errors
  |> List.map (fun (error : VM.error) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let program_errors errors =
  errors
  |> List.map (fun (error : Program.error) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let instruction_id value =
  Sequence.Instruction_id.of_int value |> require_ok sequence_error

let value_id value = Sequence.Value_id.of_int value |> require_ok sequence_error
let block_id value = Sequence.Block_id.of_int value |> require_ok sequence_error

let i64 =
  Type.make_primitive ~form:Type.Internal_storage ~primitive:Primitive.I64
    ~pointer_depth:0
  |> require_ok Fun.id

let description ?(operands = []) ?result ?target_type ?payload ?(flags = 0L) id
    opcode : Sequence.description =
  {
    instruction_id = instruction_id id;
    opcode;
    operands = List.map value_id operands;
    result = Option.map (fun id -> { Sequence.value_id = value_id id }) result;
    target_type;
    payload;
    flags;
    span = None;
  }

let imm id value bits =
  description ~result:value ~target_type:i64 ~payload:(Sequence.Integer bits) id
    Opcode.Ic_imm_i64

let switch ?(opcode = Opcode.Ic_switch) id adjusted range targets =
  description ~operands:[ adjusted; range ]
    ~payload:(Sequence.Block_targets (List.map block_id targets))
    id opcode

let end_expression id operand =
  description ~operands:[ operand ] ~flags:0x200L id Opcode.Ic_end_exp

let jump id target =
  description ~payload:(Sequence.Block (block_id target)) id Opcode.Ic_jmp

let block id instructions : Graph.block_description =
  { block_id = block_id id; instructions }

let target_block ~block_id:target ~instruction_base ~value_id:result bits =
  block target
    [
      imm instruction_base result bits;
      end_expression (instruction_base + 1) result;
      jump (instruction_base + 2) 90;
    ]

let checked_graph ~adjusted_bits ~entries =
  let range = List.length entries in
  let source_blocks =
    [
      block 0
        [
          imm 0 0 adjusted_bits;
          imm 1 1 0L;
          description ~operands:[ 0; 1 ] ~result:2 ~target_type:i64 2
            Opcode.Ic_sub;
          imm 3 3 (Int64.of_int range);
          switch 4 2 3 (10 :: entries);
        ];
      target_block ~block_id:10 ~instruction_base:10 ~value_id:10 100L;
      target_block ~block_id:20 ~instruction_base:20 ~value_id:20 200L;
      target_block ~block_id:30 ~instruction_base:30 ~value_id:30 300L;
      block 90 [ description 90 Opcode.Ic_end ];
    ]
  in
  Graph.create ~entry:(block_id 0) source_blocks |> require_ok graph_errors

let verified ~adjusted_bits ~entries =
  checked_graph ~adjusted_bits ~entries |> X87.verify |> require_ok x87_errors

let compile ?status_abi ?(max_code_bytes = 65_536) checked =
  Program.compile ?status_abi ~max_stack_bytes:Program.hard_max_stack_bytes
    ~max_blocks:4096 ~max_ir_instructions:100_000 ~max_code_bytes checked

let execute_vm ~max_steps checked =
  VM.execute_program ~max_steps ~max_frame_bytes:4096 ~max_call_depth:64
    ~functions:[] checked

let check_final_bits label expected execution =
  match VM.final_value execution with
  | Some word -> Alcotest.(check int64) label expected word.bits
  | None -> Alcotest.failf "%s: execution produced no final value" label

let execute label ~adjusted_bits ~entries ~expected =
  let checked = verified ~adjusted_bits ~entries in
  let execution = execute_vm ~max_steps:32 checked |> require_ok vm_errors in
  check_final_bits (label ^ " VM result") expected execution;
  ignore (compile checked |> require_ok program_errors)

let valid_default_and_case_dispatch () =
  execute "case zero" ~adjusted_bits:0L ~entries:[ 20; 30 ] ~expected:200L;
  execute "case one" ~adjusted_bits:1L ~entries:[ 20; 30 ] ~expected:300L;
  execute "default" ~adjusted_bits:2L ~entries:[ 20; 30 ] ~expected:100L

let unsigned_negative_dispatches_default () =
  execute "negative adjusted word" ~adjusted_bits:(-1L) ~entries:[ 20; 30 ]
    ~expected:100L

let high_bit_dispatches_default () =
  execute "high-bit adjusted word" ~adjusted_bits:Int64.min_int
    ~entries:[ 20; 30 ] ~expected:100L

let repeated_table_target_is_preserved () =
  let graph = checked_graph ~adjusted_bits:1L ~entries:[ 20; 20; 30 ] in
  let entry = Graph.entry graph in
  Alcotest.(check (list int))
    "graph successors deduplicate without rewriting the switch table"
    [ 10; 20; 30 ]
    (Graph.successors entry |> List.map Graph.Block_id.to_int);
  let checked = graph |> X87.verify |> require_ok x87_errors in
  let execution = execute_vm ~max_steps:32 checked |> require_ok vm_errors in
  check_final_bits "repeated entry dispatch" 200L execution;
  ignore (compile checked |> require_ok program_errors)

let exact_switch_meter_and_native_site () =
  let checked = verified ~adjusted_bits:0L ~entries:[ 20 ] in
  let exact = execute_vm ~max_steps:9 checked |> require_ok vm_errors in
  Alcotest.(check int)
    "switch path reaches exactly nine IR sites" 9 (VM.executed_steps exact);
  check_final_bits "exact-meter result" 200L exact;
  (match execute_vm ~max_steps:4 checked with
  | Error [ error ] ->
      Alcotest.(check string) "VM switch budget code" "HCIRVM0007" error.code;
      Alcotest.(check int) "VM stops before switch" 4 error.executed_steps;
      Alcotest.(check (option int))
        "VM fault names the single switch site" (Some 4) error.instruction_id
  | Error errors -> Alcotest.fail (vm_errors errors)
  | Ok _ -> Alcotest.fail "four-step VM budget unexpectedly reached IC_SWITCH");
  let image = compile checked |> require_ok program_errors in
  match
    Program.decode_runtime_status image ~max_steps:4 ~kind:3L ~site:5L
      ~executed_steps:4L ~value_site:0L ~bits:0L
    |> require_ok Fun.id
  with
  | Program.Fault fault ->
      Alcotest.(check int)
        "native meter maps one dense site to IC_SWITCH" 4 fault.instruction_id;
      Alcotest.(check int)
        "native switch fault preserves four consumed steps" 4
        fault.executed_steps
  | Program.Completed _ ->
      Alcotest.fail "native switch step-limit status decoded as completion"

let no_bound_switch_is_explicitly_unsupported () =
  let graph =
    Graph.create ~entry:(block_id 0)
      [
        block 0
          [
            imm 0 0 0L;
            imm 1 1 1L;
            switch ~opcode:Opcode.Ic_nobound_switch 2 0 1 [ 10; 20 ];
          ];
        block 10 [ description 10 Opcode.Ic_ret ];
        block 20 [ description 20 Opcode.Ic_ret ];
      ]
    |> require_ok graph_errors
  in
  let checked = graph |> X87.verify |> require_ok x87_errors in
  (match execute_vm ~max_steps:8 checked with
  | Error errors ->
      Alcotest.(check bool)
        "VM retains the no-bound gate" true
        (List.exists
           (fun (error : VM.error) -> error.code = "HCIRVM0002")
           errors)
  | Ok _ -> Alcotest.fail "IC_NOBOUND_SWITCH unexpectedly executed");
  match compile checked with
  | Error errors ->
      Alcotest.(check bool)
        "native retains the no-bound gate" true
        (List.exists
           (fun (error : Program.error) -> error.code = "HCBACK0002")
           errors)
  | Ok _ -> Alcotest.fail "IC_NOBOUND_SWITCH unexpectedly compiled natively"

let malformed_shape_and_range_evidence_are_rejected () =
  let wrong_payload =
    Graph.create ~entry:(block_id 0)
      [
        block 0
          [
            imm 0 0 0L;
            imm 1 1 1L;
            description ~operands:[ 0; 1 ]
              ~payload:(Sequence.Block (block_id 10))
              2 Opcode.Ic_switch;
          ];
        block 10 [ description 10 Opcode.Ic_ret ];
      ]
  in
  (match wrong_payload with
  | Error errors ->
      Alcotest.(check bool)
        "wrong payload fails the canonical switch contract" true
        (List.exists
           (fun (error : Graph.error) -> error.code = "HCIR0038")
           errors)
  | Ok _ -> Alcotest.fail "IC_SWITCH with a scalar target payload was accepted");
  let wrong_range =
    Graph.create ~entry:(block_id 0)
      [
        block 0
          [
            imm 0 0 0L;
            imm 1 1 0L;
            description ~operands:[ 0; 1 ] ~result:2 ~target_type:i64 2
              Opcode.Ic_sub;
            imm 3 3 2L;
            switch 4 2 3 [ 10; 20 ];
          ];
        block 10 [ description 10 Opcode.Ic_ret ];
        block 20 [ description 20 Opcode.Ic_ret ];
      ]
  in
  match wrong_range with
  | Error errors ->
      Alcotest.(check bool)
        "range immediate must equal table length" true
        (List.exists
           (fun (error : Graph.error) -> error.code = "HCIR0038")
           errors)
  | Ok _ ->
      Alcotest.fail "IC_SWITCH with mismatched range evidence was accepted"

let maximum_table_hits_native_code_quota_before_plan () =
  let entries = List.init 0xffff (fun i -> if i mod 2 = 0 then 20 else 30) in
  let checked = verified ~adjusted_bits:0L ~entries in
  match compile ~max_code_bytes:65_536 checked with
  | Error errors ->
      Alcotest.(check bool)
        "maximum table is rejected by native code quota" true
        (List.exists
           (fun (error : Program.error) ->
             error.code = "HCBACK0005"
             && String.ends_with ~suffix:"before allocation" error.message)
           errors)
  | Ok _ ->
      Alcotest.fail
        "maximum switch table unexpectedly fit the 64 KiB code quota"

let contiguous_targets_have_bounded_code () =
  List.iter
    (fun status_abi ->
      let compile = compile ~status_abi in
      let small = verified ~adjusted_bits:0L ~entries:[ 20 ] in
      let large =
        verified ~adjusted_bits:0L ~entries:(List.init 0xffff (fun _ -> 20))
      in
      let small_image = compile small |> require_ok program_errors in
      let large_image = compile large |> require_ok program_errors in
      let size = String.length (Program.code large_image) in
      Alcotest.(check int)
        "one run has constant encoded size"
        (String.length (Program.code small_image))
        size;
      Alcotest.(check int)
        "one run has constant instruction count"
        (Program.machine_instructions small_image)
        (Program.machine_instructions large_image);
      ignore (compile ~max_code_bytes:size large |> require_ok program_errors);
      (match compile ~max_code_bytes:(size - 1) large with
      | Error errors ->
          Alcotest.(check bool)
            "exact encoded quota remains enforced" true
            (List.exists
               (fun (e : Program.error) -> e.code = "HCBACK0005")
               errors)
      | Ok _ -> Alcotest.fail "one-below code quota admitted");
      Alcotest.(check string)
        "fresh compilation is deterministic" (Program.code large_image)
        (compile large |> require_ok program_errors |> Program.code))
    [ Program.Windows_x64; Program.System_v_x64 ]

let tests =
  [
    Alcotest.test_case "contiguous switch targets use bounded native code"
      `Quick contiguous_targets_have_bounded_code;
    Alcotest.test_case "bounded switch dispatches cases and default" `Quick
      valid_default_and_case_dispatch;
    Alcotest.test_case "adjusted negative words use unsigned default bounds"
      `Quick unsigned_negative_dispatches_default;
    Alcotest.test_case "adjusted high-bit words use unsigned default bounds"
      `Quick high_bit_dispatches_default;
    Alcotest.test_case "repeated table destinations remain dispatch entries"
      `Quick repeated_table_target_is_preserved;
    Alcotest.test_case "switch dispatch is exactly one metered IR site" `Quick
      exact_switch_meter_and_native_site;
    Alcotest.test_case "no-bound switch remains an explicit consumer gate"
      `Quick no_bound_switch_is_explicitly_unsupported;
    Alcotest.test_case "switch shape and range evidence are canonical" `Quick
      malformed_shape_and_range_evidence_are_rejected;
    Alcotest.test_case
      "maximum table respects native code quota before planning" `Slow
      maximum_table_hits_native_code_quota_before_plan;
  ]
