module X87 = Holyc_lib.Ir_x87_stack
module Graph = Holyc_lib.Ir_block_graph
module Sequence = Holyc_lib.Ir_instruction_sequence
module Opcode = Holyc_lib.Ir_opcode
module Type = Holyc_lib.Semantic_type
module Primitive = Holyc_lib.Primitive_type

let require_ok show = function
  | Ok value -> value
  | Error error -> Alcotest.fail (show error)

let show_sequence_error (error : Sequence.error) =
  error.code ^ ": " ^ error.message

let show_graph_error (error : Graph.error) = error.code ^ ": " ^ error.message
let show_x87_error (error : X87.error) = error.code ^ ": " ^ error.message

let instruction_id value =
  Sequence.Instruction_id.of_int value |> require_ok show_sequence_error

let value_id value =
  Sequence.Value_id.of_int value |> require_ok show_sequence_error

let block_id value =
  Sequence.Block_id.of_int value |> require_ok show_sequence_error

let primitive_type primitive =
  Type.make_primitive ~form:Type.Internal_storage ~primitive ~pointer_depth:0
  |> require_ok Fun.id

let i64 = primitive_type Primitive.I64
let f64 = primitive_type Primitive.F64
let result value : Sequence.value_definition = { value_id = value_id value }

let description ?(operands = []) ?result ?target_type ?payload ?(flags = 0L)
    id opcode : Sequence.description =
  {
    instruction_id = instruction_id id;
    opcode;
    operands;
    result;
    target_type;
    payload;
    flags;
    span = None;
  }

let block id instructions : Graph.block_description =
  { block_id = block_id id; instructions }

let graph ~entry blocks =
  Graph.create ~entry:(block_id entry) blocks
  |> require_ok (fun errors ->
         String.concat "; " (List.map show_graph_error errors))

let verify graph =
  X87.verify graph
  |> require_ok (fun errors -> String.concat "; " (List.map show_x87_error errors))

let errors graph =
  match X87.verify graph with
  | Ok _ -> Alcotest.fail "expected x87 verification to fail"
  | Error errors -> errors

let has_code code errors =
  Alcotest.(check bool)
    ("contains " ^ code) true
    (List.exists (fun (error : X87.error) -> error.code = code) errors)

let imm id value =
  description ~result:(result value) ~target_type:f64 id Opcode.Ic_imm_f64

let sqr ?(flags = 0L) id operand output =
  description ~operands:[ value_id operand ] ~result:(result output)
    ~target_type:f64 ~flags id Opcode.Ic_sqr

let arg1_to_f64 = 0x000000004L
let res_to_f64 = 0x000000001L
let res_to_int = 0x000000002L
let arg1_to_int = 0x000000008L
let arg2_to_f64 = 0x000000010L
let arg2_to_int = 0x000000020L
let use_f64 = 0x000000040L
let use_int = 0x000000100L
let push_cmp = 0x000040000L
let pop_cmp = 0x000080000L
let dont_push index = Int64.shift_left 1L (21 + index)
let dont_pop index = Int64.shift_left 1L (24 + index)

let bits setter mask =
  List.init 3 Fun.id
  |> List.fold_left
       (fun result index ->
         if mask land (1 lsl index) = 0 then result
         else Int64.logor result (setter index))
       0L

let popcount value =
  List.init 3 Fun.id
  |> List.fold_left
       (fun count index -> count + if value land (1 lsl index) = 0 then 0 else 1)
       0

let source_fpop_facts_are_retained () =
  Alcotest.(check int) "opcode count" 185 (List.length Opcode.all);
  List.iter
    (fun opcode ->
      Alcotest.(check bool)
        (Opcode.to_source_name opcode)
        (Opcode.info opcode).pops_float
        (X87.pops_float opcode))
    Opcode.all;
  Alcotest.(check bool) "IC_SQR fpop" true (X87.pops_float Opcode.Ic_sqr);
  Alcotest.(check bool)
    "IC_LESS fpop" false (X87.pops_float Opcode.Ic_less);
  let expected =
    [
      Opcode.Ic__pp;
      Ic__mm;
      Ic_pp_;
      Ic_mm_;
      Ic_mul;
      Ic_div;
      Ic_add;
      Ic_sub;
      Ic_sqr;
      Ic_abs;
      Ic_sqrt;
      Ic_sin;
      Ic_cos;
      Ic_tan;
      Ic_atan;
    ]
  in
  let actual = List.filter X87.pops_float Opcode.all in
  Alcotest.(check (list string))
    "complete fpop family"
    (List.map Opcode.to_source_name expected)
    (List.map Opcode.to_source_name actual)

let append_sqr_chain ~flags ~count ~next_instruction ~next_value operand =
  let rec loop instructions instruction value remaining =
    if remaining = 0 then (List.rev instructions, instruction, value, operand)
    else
      let item = sqr ~flags instruction operand value in
      loop (item :: instructions) (instruction + 1) (value + 1) (remaining - 1)
  in
  loop [] next_instruction next_value count

let suppression_combinations_are_independent () =
  for push_mask = 0 to 7 do
    for pop_mask = 0 to 7 do
      let initial, instruction, value, operand =
        append_sqr_chain ~flags:(dont_pop 0) ~count:3 ~next_instruction:1
          ~next_value:1 0
      in
      let subject_flags =
        Int64.logor arg1_to_f64 arg2_to_f64
        |> Int64.logor (bits dont_push push_mask)
        |> Int64.logor (bits dont_pop pop_mask)
      in
      let subject =
        description ~operands:[ value_id operand; value_id operand ]
          ~result:(result value) ~target_type:f64 ~flags:subject_flags
          instruction Opcode.Ic_add
      in
      let after = 3 - popcount push_mask + popcount pop_mask in
      let drain, instruction, _, _ =
        append_sqr_chain ~flags:(dont_push 0) ~count:after
          ~next_instruction:(instruction + 1) ~next_value:(value + 1) operand
      in
      let checked =
        graph ~entry:0
          [
            block 0
              ([ imm 0 0 ] @ initial @ [ subject ] @ drain
             @ [ description instruction Opcode.Ic_ret ]);
          ]
        |> verify
      in
      let subject_trace =
        X87.trace checked
        |> List.find (fun (trace : X87.instruction_trace) ->
               Sequence.Instruction_id.to_int trace.instruction_id
               = instruction - after - 1)
      in
      Alcotest.(check int)
        "three tracked slots" 3 (List.length subject_trace.slots);
      Alcotest.(check (list string))
        "ordered conversion directions"
        [ "operand-2-to-f64"; "operand-1-to-f64"; "operation" ]
        (List.map
           (fun (slot : X87.slot_trace) -> X87.slot_kind_name slot.kind)
           subject_trace.slots);
      Alcotest.(check int) "combined depth" after subject_trace.after_depth
    done
  done

let conversion_directions_are_retained () =
  let first_flags =
    Int64.logor use_int
      (Int64.logor arg2_to_int (Int64.logor arg1_to_f64 res_to_f64))
  in
  let second_flags =
    Int64.logor use_int
      (Int64.logor arg2_to_f64 (Int64.logor arg1_to_int res_to_int))
  in
  let checked =
    graph ~entry:0
      [
        block 0
          [
            imm 0 0;
            imm 1 1;
            description ~operands:[ value_id 0; value_id 1 ]
              ~result:(result 2) ~target_type:f64 ~flags:first_flags 2
              Opcode.Ic_add;
            description ~operands:[ value_id 0; value_id 1 ]
              ~result:(result 3) ~target_type:i64 ~flags:second_flags 3
              Opcode.Ic_add;
            description 4 Opcode.Ic_ret;
          ];
      ]
    |> verify
  in
  let names trace =
    List.map
      (fun (slot : X87.slot_trace) -> X87.slot_kind_name slot.kind)
      trace.X87.slots
  in
  let traces = X87.trace checked in
  Alcotest.(check (list string))
    "integer then float directions"
    [ "operand-2-to-int"; "operand-1-to-f64"; "result-to-f64" ]
    (names (List.nth traces 2));
  Alcotest.(check (list string))
    "float then integer directions"
    [ "operand-2-to-f64"; "operand-1-to-int"; "result-to-int" ]
    (names (List.nth traces 3))

let malformed_flags_are_rejected () =
  let unused_suppression =
    graph ~entry:0
      [
        block 0
          [ imm 0 0; sqr ~flags:(dont_pop 1) 1 0 1; description 2 Opcode.Ic_ret ];
      ]
  in
  errors unused_suppression |> has_code "HCIR0021";
  let conflicting_path =
    graph ~entry:0
      [
        block 0
          [
            imm 0 0;
            sqr ~flags:(Int64.logor use_f64 use_int) 1 0 1;
            description 2 Opcode.Ic_ret;
          ];
      ]
  in
  errors conflicting_path |> has_code "HCIR0021";
  let comparison_flag_on_add =
    graph ~entry:0
      [
        block 0
          [
            imm 0 0;
            description ~operands:[ value_id 0; value_id 0 ]
              ~result:(result 1) ~target_type:f64 ~flags:push_cmp 1
              Opcode.Ic_add;
            description 2 Opcode.Ic_ret;
          ];
      ]
  in
  errors comparison_flag_on_add |> has_code "HCIR0021"

let underflow_and_overflow_are_rejected () =
  let underflow =
    graph ~entry:0
      [
        block 0
          [ imm 0 0; sqr ~flags:(dont_push 0) 1 0 1; description 2 Opcode.Ic_ret ];
      ]
  in
  errors underflow |> has_code "HCIR0022";
  let pushes =
    List.init 9 (fun index -> sqr ~flags:(dont_pop 0) (index + 1) 0 (index + 1))
  in
  let overflow =
    graph ~entry:0
      [ block 0 ([ imm 0 0 ] @ pushes @ [ description 10 Opcode.Ic_ret ]) ]
  in
  errors overflow |> has_code "HCIR0023";
  let eight_live = List.filteri (fun index _ -> index < 8) pushes in
  let transient_overflow =
    graph ~entry:0
      [
        block 0
          ([ imm 0 0 ] @ eight_live
          @ [ sqr 9 0 9; description 10 Opcode.Ic_ret ]);
      ]
  in
  errors transient_overflow |> has_code "HCIR0023"

let merge_depths_must_agree () =
  let checked_graph =
    graph ~entry:0
      [
        block 0
          [
            description ~result:(result 0) ~target_type:i64 0 Opcode.Ic_imm_i64;
            description ~operands:[ value_id 0 ]
              ~payload:(Sequence.Block (block_id 2)) 1 Opcode.Ic_br_zero;
          ];
        block 1
          [
            imm 2 1;
            sqr ~flags:(dont_pop 0) 3 1 2;
            description ~payload:(Sequence.Block (block_id 3)) 4 Opcode.Ic_jmp;
          ];
        block 2
          [ description ~payload:(Sequence.Block (block_id 3)) 5 Opcode.Ic_jmp ];
        block 3 [ description 6 Opcode.Ic_ret ];
      ]
  in
  errors checked_graph |> has_code "HCIR0024"

let balanced_loop_is_accepted () =
  let checked =
    graph ~entry:0
      [
        block 0
          [ description ~payload:(Sequence.Block (block_id 1)) 0 Opcode.Ic_jmp ];
        block 1
          [
            imm 1 0;
            sqr ~flags:(dont_pop 0) 2 0 1;
            sqr ~flags:(dont_push 0) 3 0 2;
            description ~result:(result 3) ~target_type:i64 4 Opcode.Ic_imm_i64;
            description ~operands:[ value_id 3 ]
              ~payload:(Sequence.Block (block_id 1)) 5 Opcode.Ic_br_zero;
          ];
        block 2 [ description 6 Opcode.Ic_ret ];
      ]
    |> verify
  in
  Alcotest.(check int) "loop trace" 7 (List.length (X87.trace checked))

let terminal_marker_requires_empty_stack () =
  let balanced = graph ~entry:0 [ block 0 [ description 0 Opcode.Ic_end ] ] in
  ignore (verify balanced);
  let unbalanced =
    graph ~entry:0
      [ block 0 [ imm 0 0; sqr ~flags:(dont_pop 0) 1 0 1; description 2 Opcode.Ic_end ] ]
  in
  errors unbalanced |> has_code "HCIR0025"

let calls_preserve_depth_and_exits_require_empty_stack () =
  let call =
    graph ~entry:0
      [
        block 0
          [
            imm 0 0;
            sqr ~flags:(dont_pop 0) 1 0 1;
            description 2 Opcode.Ic_call;
            sqr ~flags:(dont_push 0) 3 0 2;
            description 4 Opcode.Ic_ret;
          ];
      ]
    |> verify
  in
  let call_trace = List.nth (X87.trace call) 2 in
  Alcotest.(check int) "call input depth" 1 call_trace.before_depth;
  Alcotest.(check int) "call output depth" 1 call_trace.after_depth;
  let exit =
    graph ~entry:0
      [
        block 0
          [ imm 0 0; sqr ~flags:(dont_pop 0) 1 0 1; description 2 Opcode.Ic_ret ];
      ]
  in
  errors exit |> has_code "HCIR0025"

let comparison_transfers_are_separate () =
  List.iter
    (fun transfer_flags ->
      let flags = Int64.logor use_f64 transfer_flags in
      let checked =
        graph ~entry:0
          [
            block 0
              [
                imm 0 0;
                imm 1 1;
                description ~operands:[ value_id 0; value_id 1 ]
                  ~result:(result 2) ~target_type:i64 ~flags 2 Opcode.Ic_less;
                description 3 Opcode.Ic_ret;
              ];
          ]
        |> verify
      in
      let comparison = List.nth (X87.trace checked) 2 in
      Alcotest.(check int)
        "one float operation" 1 (List.length comparison.slots);
      Alcotest.(check int)
        "comparison remains balanced" 0 comparison.after_depth)
    [ push_cmp; pop_cmp; Int64.logor push_cmp pop_cmp ];
  let marker =
    graph ~entry:0
      [
        block 0
          [
            description ~result:(result 0) ~target_type:f64 ~flags:res_to_f64 0
              Opcode.Ic_push_cmp;
            description 1 Opcode.Ic_ret;
          ];
      ]
    |> verify |> X87.trace |> List.hd
  in
  Alcotest.(check int)
    "IC_PUSH_CMP conversion is balanced" 0 (List.length marker.slots)

let deterministic_trace () =
  let checked =
    graph ~entry:4
      [
        block 4
          [
            imm 0 0;
            sqr ~flags:(dont_pop 0) 1 0 1;
            sqr ~flags:(dont_push 0) 2 0 2;
            description 3 Opcode.Ic_ret;
          ];
      ]
    |> verify
  in
  let expected =
    "holyc-ir-x87-v1 reference=c26482bb6ad3f80106d28504ec5db3c6a360732c\n\
     ^b4 !i0 depth=0->0 fpop=false use-f64=false use-int=false cmp-push=false cmp-pop=false\n\
     ^b4 !i1 depth=0->1 fpop=true use-f64=false use-int=false cmp-push=false cmp-pop=false 0:operation:no-pop:0->1\n\
     ^b4 !i2 depth=1->0 fpop=true use-f64=false use-int=false cmp-push=false cmp-pop=false 0:operation:no-push:1->0\n\
     ^b4 !i3 depth=0->0 fpop=false use-f64=false use-int=false cmp-push=false cmp-pop=false\n"
  in
  Alcotest.(check string) "versioned trace" expected (X87.human checked)

let failures_are_deterministic () =
  let checked_graph =
    graph ~entry:0
      [
        block 0
          [ imm 0 0; sqr ~flags:(dont_push 0) 1 0 1; description 2 Opcode.Ic_ret ];
      ]
  in
  let signature errors =
    List.map
      (fun (error : X87.error) ->
        ( error.code,
          error.message,
          error.block_id,
          error.instruction_id,
          error.span ))
      errors
  in
  Alcotest.(check bool)
    "repeat failure" true
    (signature (errors checked_graph) = signature (errors checked_graph))

let tests =
  [
    Alcotest.test_case "source fpop facts" `Quick
      source_fpop_facts_are_retained;
    Alcotest.test_case "suppression combinations" `Quick
      suppression_combinations_are_independent;
    Alcotest.test_case "conversion directions" `Quick
      conversion_directions_are_retained;
    Alcotest.test_case "malformed flags" `Quick malformed_flags_are_rejected;
    Alcotest.test_case "underflow and overflow" `Quick
      underflow_and_overflow_are_rejected;
    Alcotest.test_case "merge depths" `Quick merge_depths_must_agree;
    Alcotest.test_case "balanced loop" `Quick balanced_loop_is_accepted;
    Alcotest.test_case "call and exit boundaries" `Quick
      calls_preserve_depth_and_exits_require_empty_stack;
    Alcotest.test_case "terminal marker" `Quick
      terminal_marker_requires_empty_stack;
    Alcotest.test_case "comparison transfers" `Quick
      comparison_transfers_are_separate;
    Alcotest.test_case "deterministic trace" `Quick deterministic_trace;
    Alcotest.test_case "deterministic failures" `Quick
      failures_are_deterministic;
  ]
