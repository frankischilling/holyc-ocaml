module VM = Holyc_lib.Ir_integer_interpreter
module X87 = Holyc_lib.Ir_x87_stack
module Graph = Holyc_lib.Ir_block_graph
module Sequence = Holyc_lib.Ir_instruction_sequence
module Opcode = Holyc_lib.Ir_opcode
module Type = Holyc_lib.Semantic_type
module Primitive = Holyc_lib.Primitive_type
module Span = Holyc_lib.Span
module Source_id = Holyc_lib.Source_id

let require_ok show = function
  | Ok value -> value
  | Error error -> Alcotest.fail (show error)

let show_sequence_error (error : Sequence.error) =
  error.code ^ ": " ^ error.message

let show_graph_error (error : Graph.error) = error.code ^ ": " ^ error.message
let show_x87_error (error : X87.error) = error.code ^ ": " ^ error.message

let stage_name = function
  | VM.Configuration -> "configuration"
  | VM.Preflight -> "preflight"
  | VM.Execution -> "execution"

let show_vm_error (error : VM.error) =
  Printf.sprintf "%s/%s after %d step(s): %s" (stage_name error.stage)
    error.code error.executed_steps error.message

let show_vm_errors errors = String.concat "; " (List.map show_vm_error errors)

let instruction_id value =
  Sequence.Instruction_id.of_int value |> require_ok show_sequence_error

let value_id value =
  Sequence.Value_id.of_int value |> require_ok show_sequence_error

let block_id value =
  Sequence.Block_id.of_int value |> require_ok show_sequence_error

let primitive_type ?(form = Type.Internal_storage) ?(pointer_depth = 0)
    primitive =
  Type.make_primitive ~form ~primitive ~pointer_depth |> require_ok Fun.id

let i64 = primitive_type Primitive.I64
let u64 = primitive_type Primitive.U64
let u8 = primitive_type Primitive.U8
let public_i64 = primitive_type ~form:Type.Public_spelling Primitive.I64
let public_u64 = primitive_type ~form:Type.Public_spelling Primitive.U64
let i64_pointer = primitive_type ~pointer_depth:1 Primitive.I64
let source = Source_id.of_int 9 |> require_ok Fun.id
let span start stop = Span.unsafe_make ~source ~start ~stop
let result value : Sequence.value_definition = { value_id = value_id value }

let description ?(operands = []) ?result ?target_type ?payload ?(flags = 0L)
    ?span id opcode : Sequence.description =
  {
    instruction_id = instruction_id id;
    opcode;
    operands;
    result;
    target_type;
    payload;
    flags;
    span;
  }

let imm ?(flags = 0L) ?span ~id ~value ~type_ bits =
  description ~result:(result value) ~target_type:type_
    ~payload:(Sequence.Integer bits) ~flags ?span id Opcode.Ic_imm_i64

let unary ?(flags = 0L) ~id ~operand ~value ~type_ opcode =
  description
    ~operands:[ value_id operand ]
    ~result:(result value) ~target_type:type_ ~flags id opcode

let binary ?(flags = 0L) ~id ~left ~right ~value ~type_ opcode =
  description
    ~operands:[ value_id left; value_id right ]
    ~result:(result value) ~target_type:type_ ~flags id opcode

let return_value ~id ~operand ~type_ =
  description
    ~operands:[ value_id operand ]
    ~target_type:type_ id Opcode.Ic_return_val

let end_expression ?(flags = 0x000000200L) ~id ~operand () =
  description ~operands:[ value_id operand ] ~flags id Opcode.Ic_end_exp

let jump ~id ~target =
  description ~payload:(Sequence.Block (block_id target)) id Opcode.Ic_jmp

let branch ~id ~operand ~target opcode =
  description
    ~operands:[ value_id operand ]
    ~payload:(Sequence.Block (block_id target))
    id opcode

let ret id = description id Opcode.Ic_ret
let stream_end id = description id Opcode.Ic_end

let block id instructions : Graph.block_description =
  { block_id = block_id id; instructions }

let verified ~entry blocks =
  Graph.create ~entry:(block_id entry) blocks
  |> require_ok (fun errors ->
      String.concat "; " (List.map show_graph_error errors))
  |> X87.verify
  |> require_ok (fun errors ->
      String.concat "; " (List.map show_x87_error errors))

let execute ?(max_steps = 64) checked = VM.execute ~max_steps checked

let require_execution ?max_steps checked =
  execute ?max_steps checked |> require_ok show_vm_errors

let require_errors ?max_steps checked =
  match execute ?max_steps checked with
  | Ok _ -> Alcotest.fail "integer execution unexpectedly succeeded"
  | Error errors -> errors

let word_type_name = function
  | VM.I64 -> "i64"
  | VM.U64 -> "u64"

let termination_name = function
  | VM.Stream_end -> "stream-end"
  | VM.Returned None -> "returned:none"
  | VM.Returned (Some word) ->
      Printf.sprintf "returned:%s:0x%016Lx"
        (word_type_name word.type_)
        word.bits

let require_returned execution =
  match VM.termination execution with
  | VM.Returned (Some word) -> word
  | termination ->
      Alcotest.failf "expected a returned word, got %s"
        (termination_name termination)

let check_word label expected_type expected_bits (word : VM.word) =
  Alcotest.(check string)
    (label ^ " type")
    (word_type_name expected_type)
    (word_type_name word.type_);
  Alcotest.(check int64) (label ^ " bits") expected_bits word.bits

let check_stage label expected (error : VM.error) =
  Alcotest.(check string) label (stage_name expected) (stage_name error.stage)

let has_code code errors =
  Alcotest.(check bool)
    ("contains " ^ code) true
    (List.exists (fun (error : VM.error) -> error.code = code) errors)

let only_error expected_code = function
  | [ error ] ->
      Alcotest.(check string) "diagnostic code" expected_code error.VM.code;
      error
  | errors ->
      Alcotest.failf "expected one %s diagnostic, got %d: %s" expected_code
        (List.length errors) (show_vm_errors errors)

let returned_graph ~producer_type ~return_type bits =
  verified ~entry:0
    [
      block 0
        [
          imm ~id:0 ~value:0 ~type_:producer_type bits;
          return_value ~id:1 ~operand:0 ~type_:return_type;
          ret 2;
        ];
    ]

let execute_unary ~opcode ~operand_type ~result_type bits =
  verified ~entry:0
    [
      block 0
        [
          imm ~id:0 ~value:0 ~type_:operand_type bits;
          unary ~id:1 ~operand:0 ~value:1 ~type_:result_type opcode;
          return_value ~id:2 ~operand:1 ~type_:result_type;
          ret 3;
        ];
    ]
  |> require_execution |> require_returned

let execute_binary ~opcode ~left_type ~left_bits ~right_type ~right_bits
    ~result_type =
  verified ~entry:0
    [
      block 0
        [
          imm ~id:0 ~value:0 ~type_:left_type left_bits;
          imm ~id:1 ~value:1 ~type_:right_type right_bits;
          binary ~id:2 ~left:0 ~right:1 ~value:2 ~type_:result_type opcode;
          return_value ~id:3 ~operand:2 ~type_:result_type;
          ret 4;
        ];
    ]
  |> require_execution |> require_returned

let integer_operations_use_exact_word_rules () =
  let unary_cases =
    [
      ("com u64", Opcode.Ic_com, u64, i64, 0L, VM.I64, -1L);
      ("com i64", Opcode.Ic_com, i64, i64, 1L, VM.I64, -2L);
      ("not u64", Opcode.Ic_not, u64, u64, 0L, VM.U64, 1L);
      ("not i64", Opcode.Ic_not, i64, i64, -1L, VM.I64, 0L);
      ( "negate u64 min",
        Opcode.Ic_unary_minus,
        u64,
        i64,
        Int64.min_int,
        VM.I64,
        Int64.min_int );
    ]
  in
  List.iter
    (fun ( name,
           opcode,
           operand_type,
           result_type,
           bits,
           expected_type,
           expected_bits ) ->
      execute_unary ~opcode ~operand_type ~result_type bits
      |> check_word name expected_type expected_bits)
    unary_cases;
  let binary_cases =
    [
      ( "i64 add wraps",
        Opcode.Ic_add,
        i64,
        Int64.max_int,
        i64,
        1L,
        i64,
        VM.I64,
        Int64.min_int );
      ("u64 add wraps", Opcode.Ic_add, u64, -1L, i64, 1L, u64, VM.U64, 0L);
      ( "i64 sub wraps",
        Opcode.Ic_sub,
        i64,
        Int64.min_int,
        i64,
        1L,
        i64,
        VM.I64,
        Int64.max_int );
      ("mixed sub is u64", Opcode.Ic_sub, i64, 0L, u64, 1L, u64, VM.U64, -1L);
      ( "i64 mul wraps",
        Opcode.Ic_mul,
        i64,
        Int64.min_int,
        i64,
        -1L,
        i64,
        VM.I64,
        Int64.min_int );
      ("u64 mul wraps", Opcode.Ic_mul, u64, -1L, u64, 2L, u64, VM.U64, -2L);
      ( "i64 and i64",
        Opcode.Ic_and,
        i64,
        0x0f0f0f0f0f0f0f0fL,
        i64,
        0x3333333333333333L,
        i64,
        VM.I64,
        0x0303030303030303L );
      ( "i64 and u64",
        Opcode.Ic_and,
        i64,
        -1L,
        u64,
        0x0123456789abcdefL,
        u64,
        VM.U64,
        0x0123456789abcdefL );
      ( "u64 and i64",
        Opcode.Ic_and,
        u64,
        0x5555555555555555L,
        i64,
        0x3333333333333333L,
        u64,
        VM.U64,
        0x1111111111111111L );
      ( "u64 and u64",
        Opcode.Ic_and,
        u64,
        -1L,
        u64,
        Int64.min_int,
        u64,
        VM.U64,
        Int64.min_int );
      ( "i64 or i64",
        Opcode.Ic_or,
        i64,
        0x0f0f0f0f0f0f0f0fL,
        i64,
        0x3030303030303030L,
        i64,
        VM.I64,
        0x3f3f3f3f3f3f3f3fL );
      ( "i64 or u64",
        Opcode.Ic_or,
        i64,
        0L,
        u64,
        Int64.min_int,
        u64,
        VM.U64,
        Int64.min_int );
      ( "u64 or i64",
        Opcode.Ic_or,
        u64,
        0x0123456789abcdefL,
        i64,
        -1L,
        u64,
        VM.U64,
        -1L );
      ( "u64 or u64",
        Opcode.Ic_or,
        u64,
        0x5555555555555555L,
        u64,
        0x2222222222222222L,
        u64,
        VM.U64,
        0x7777777777777777L );
      ( "i64 xor i64",
        Opcode.Ic_xor,
        i64,
        0x0f0f0f0f0f0f0f0fL,
        i64,
        0x3333333333333333L,
        i64,
        VM.I64,
        0x3c3c3c3c3c3c3c3cL );
      ("i64 xor u64", Opcode.Ic_xor, i64, -1L, u64, 0L, u64, VM.U64, -1L);
      ( "u64 xor i64",
        Opcode.Ic_xor,
        u64,
        Int64.min_int,
        i64,
        -1L,
        u64,
        VM.U64,
        Int64.max_int );
      ( "u64 xor u64",
        Opcode.Ic_xor,
        u64,
        0x5555555555555555L,
        u64,
        0x3333333333333333L,
        u64,
        VM.U64,
        0x6666666666666666L );
    ]
  in
  List.iter
    (fun ( name,
           opcode,
           left_type,
           left_bits,
           right_type,
           right_bits,
           result_type,
           expected_type,
           expected_bits ) ->
      execute_binary ~opcode ~left_type ~left_bits ~right_type ~right_bits
        ~result_type
      |> check_word name expected_type expected_bits)
    binary_cases;
  let seed = 0x5555555555555555L in
  let other = 0x3030303030303030L in
  let composed =
    verified ~entry:0
      [
        block 0
          [
            imm ~id:0 ~value:0 ~type_:u64 seed;
            binary ~id:1 ~left:0 ~right:0 ~value:1 ~type_:u64 Opcode.Ic_and;
            imm ~id:2 ~value:2 ~type_:i64 other;
            binary ~id:3 ~left:1 ~right:2 ~value:3 ~type_:u64 Opcode.Ic_or;
            binary ~id:4 ~left:3 ~right:3 ~value:4 ~type_:u64 Opcode.Ic_xor;
            binary ~id:5 ~left:4 ~right:2 ~value:5 ~type_:u64 Opcode.Ic_add;
            end_expression ~id:6 ~operand:5 ();
            unary ~id:7 ~operand:4 ~value:6 ~type_:u64 Opcode.Ic_not;
            return_value ~id:8 ~operand:5 ~type_:u64;
            branch ~id:9 ~operand:6 ~target:2 Opcode.Ic_br_not_zero;
          ];
        block 1 [ ret 10 ];
        block 2 [ ret 11 ];
      ]
    |> require_execution
  in
  Alcotest.(check int)
    "composed instruction steps" 11
    (VM.executed_steps composed);
  composed |> require_returned |> check_word "composed and aliased" VM.U64 other

let branch_graph opcode condition =
  verified ~entry:0
    [
      block 0
        [
          imm ~id:0 ~value:0 ~type_:i64 condition;
          branch ~id:1 ~operand:0 ~target:2 opcode;
        ];
      block 1
        [
          imm ~id:2 ~value:1 ~type_:i64 11L;
          return_value ~id:3 ~operand:1 ~type_:i64;
          ret 4;
        ];
      block 2
        [
          imm ~id:5 ~value:2 ~type_:i64 22L;
          return_value ~id:6 ~operand:2 ~type_:i64;
          ret 7;
        ];
    ]

let zero_and_nonzero_branches_choose_exact_edges () =
  [
    (Opcode.Ic_br_zero, 0L, 22L);
    (Opcode.Ic_br_zero, 1L, 11L);
    (Opcode.Ic_br_not_zero, 0L, 11L);
    (Opcode.Ic_br_not_zero, -1L, 22L);
  ]
  |> List.iter (fun (opcode, condition, expected) ->
      let execution = branch_graph opcode condition |> require_execution in
      Alcotest.(check int)
        "executed branch path" 5
        (VM.executed_steps execution);
      require_returned execution
      |> check_word (Opcode.to_source_name opcode) VM.I64 expected)

let conditional_target_equal_to_fallthrough_is_not_duplicated () =
  let run condition =
    verified ~entry:0
      [
        block 0
          [
            imm ~id:0 ~value:0 ~type_:i64 condition;
            branch ~id:1 ~operand:0 ~target:1 Opcode.Ic_br_zero;
          ];
        block 1 [ stream_end 2 ];
      ]
    |> require_execution
  in
  List.iter
    (fun condition ->
      let execution = run condition in
      Alcotest.(check int) "one successor path" 3 (VM.executed_steps execution);
      Alcotest.(check string)
        "stream end" "stream-end"
        (VM.termination execution |> termination_name))
    [ 0L; 1L ]

let empty_blocks_fall_through_without_spending_steps () =
  let execution =
    verified ~entry:0 [ block 0 []; block 1 []; block 2 [ stream_end 0 ] ]
    |> require_execution ~max_steps:1
  in
  Alcotest.(check int)
    "only IC_END costs a step" 1
    (VM.executed_steps execution);
  Alcotest.(check string)
    "stream end" "stream-end"
    (VM.termination execution |> termination_name)

let return_value_survives_a_block_transfer () =
  let execution =
    verified ~entry:0
      [
        block 0
          [
            imm ~id:0 ~value:0 ~type_:u64 (-1L);
            return_value ~id:1 ~operand:0 ~type_:public_u64;
            jump ~id:2 ~target:2;
          ];
        block 1 [ ret 3 ];
        block 2 [ ret 4 ];
      ]
    |> require_execution
  in
  Alcotest.(check int) "latch, jump, and return" 4 (VM.executed_steps execution);
  require_returned execution |> check_word "latched return" VM.U64 (-1L);
  let latest =
    verified ~entry:0
      [
        block 0
          [
            imm ~id:0 ~value:0 ~type_:u64 (-1L);
            return_value ~id:1 ~operand:0 ~type_:public_u64;
            imm ~id:2 ~value:1 ~type_:i64 7L;
            return_value ~id:3 ~operand:1 ~type_:public_i64;
            jump ~id:4 ~target:2;
          ];
        block 1 [ ret 5 ];
        block 2 [ ret 6 ];
      ]
    |> require_execution
  in
  Alcotest.(check int)
    "latest latch, jump, and return" 6 (VM.executed_steps latest);
  require_returned latest |> check_word "latest latched return" VM.I64 7L

let termination_forms_and_expression_discard_are_distinct () =
  let ended =
    verified ~entry:0 [ block 0 [ stream_end 0 ] ] |> require_execution
  in
  Alcotest.(check string)
    "IC_END" "stream-end"
    (VM.termination ended |> termination_name);
  let returned_none =
    verified ~entry:0 [ block 0 [ ret 0 ] ] |> require_execution
  in
  Alcotest.(check string)
    "IC_RET" "returned:none"
    (VM.termination returned_none |> termination_name);
  let discarded =
    verified ~entry:0
      [
        block 0
          [
            imm ~id:0 ~value:0 ~type_:i64 42L;
            end_expression ~id:1 ~operand:0 ();
            stream_end 2;
          ];
      ]
    |> require_execution
  in
  Alcotest.(check int) "immediate, discard, end" 3 (VM.executed_steps discarded);
  Alcotest.(check string)
    "discarded result" "stream-end"
    (VM.termination discarded |> termination_name)

let exact_step_boundaries_and_loop_exhaustion_are_reported () =
  let finite = returned_graph ~producer_type:i64 ~return_type:i64 7L in
  let complete = require_execution ~max_steps:3 finite in
  Alcotest.(check int) "exact boundary succeeds" 3 (VM.executed_steps complete);
  let exhausted =
    require_errors ~max_steps:2 finite |> only_error "HCIRVM0007"
  in
  check_stage "finite stage" VM.Execution exhausted;
  Alcotest.(check int) "finite executed steps" 2 exhausted.executed_steps;
  Alcotest.(check (option int)) "next finite block" (Some 0) exhausted.block_id;
  Alcotest.(check (option int))
    "next finite instruction" (Some 2) exhausted.instruction_id;
  let loop =
    verified ~entry:0
      [
        block 0
          [
            imm ~id:0 ~value:0 ~type_:i64 0L;
            branch ~id:1 ~operand:0 ~target:0 Opcode.Ic_br_zero;
          ];
        block 1 [ ret 2 ];
      ]
  in
  let loop_error =
    require_errors ~max_steps:5 loop |> only_error "HCIRVM0007"
  in
  check_stage "loop stage" VM.Execution loop_error;
  Alcotest.(check int) "loop executed steps" 5 loop_error.executed_steps;
  Alcotest.(check (option int)) "next loop block" (Some 0) loop_error.block_id;
  Alcotest.(check (option int))
    "next loop instruction" (Some 1) loop_error.instruction_id;
  List.iter
    (fun max_steps ->
      let error = require_errors ~max_steps finite |> only_error "HCIRVM0001" in
      check_stage "configuration stage" VM.Configuration error;
      Alcotest.(check int)
        "configuration executes nothing" 0 error.executed_steps)
    [ -1; 0 ]

let whole_graph_preflight_includes_unreachable_blocks () =
  let unsupported_span = span 20 25 in
  let checked =
    verified ~entry:0
      [
        block 0 [ ret 0 ];
        block 1
          [
            description ~span:unsupported_span 1 Opcode.Ic_label;
            imm ~id:2 ~value:2 ~type_:i64 2L;
            imm ~id:3 ~value:3 ~type_:u64 3L;
            binary ~flags:1L ~id:4 ~left:2 ~right:3 ~value:4 ~type_:u64
              Opcode.Ic_and;
            description
              ~operands:[ value_id 2; value_id 3 ]
              ~result:(result 5) ~target_type:u64 ~payload:(Sequence.Integer 0L)
              5 Opcode.Ic_or;
            binary ~id:6 ~left:2 ~right:3 ~value:6 ~type_:u8 Opcode.Ic_xor;
            binary ~id:7 ~left:2 ~right:3 ~value:7 ~type_:i64 Opcode.Ic_xor;
            ret 8;
          ];
      ]
  in
  let errors = require_errors checked in
  Alcotest.(check (list string))
    "source-ordered diagnostic codes"
    [ "HCIRVM0002"; "HCIRVM0003"; "HCIRVM0004"; "HCIRVM0005"; "HCIRVM0006" ]
    (List.map (fun (error : VM.error) -> error.code) errors);
  List.iter
    (fun code -> has_code code errors)
    [ "HCIRVM0002"; "HCIRVM0003"; "HCIRVM0004"; "HCIRVM0005"; "HCIRVM0006" ];
  List.iter
    (fun (error : VM.error) ->
      check_stage (error.code ^ " stage") VM.Preflight error;
      Alcotest.(check int) (error.code ^ " steps") 0 error.executed_steps)
    errors;
  match
    List.find_opt (fun (error : VM.error) -> error.code = "HCIRVM0002") errors
  with
  | None -> Alcotest.fail "missing unsupported-opcode diagnostic"
  | Some error ->
      Alcotest.(check (option int))
        "unreachable block context" (Some 1) error.block_id;
      Alcotest.(check (option int))
        "unreachable instruction context" (Some 1) error.instruction_id;
      Alcotest.(check bool)
        "unreachable source context" true
        (error.span = Some unsupported_span)

let producer_domains_and_return_types_are_bounded () =
  let unsupported = [ public_i64; u8; i64_pointer ] in
  List.iter
    (fun producer_type ->
      returned_graph ~producer_type ~return_type:producer_type 1L
      |> require_errors |> has_code "HCIRVM0005")
    unsupported;
  let wrong_return =
    returned_graph ~producer_type:i64 ~return_type:public_u64 1L
    |> require_errors
  in
  has_code "HCIRVM0006" wrong_return;
  let public_spelling_at_the_return_boundary =
    returned_graph ~producer_type:i64 ~return_type:public_i64 (-7L)
    |> require_execution |> require_returned
  in
  check_word "public return target" VM.I64 (-7L)
    public_spelling_at_the_return_boundary

let read_file path =
  let path =
    if Sys.file_exists path then path else Filename.concat "test" path
  in
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let human_output_is_versioned_and_deterministic () =
  let checked = returned_graph ~producer_type:i64 ~return_type:i64 (-1L) in
  let first = require_execution checked in
  let repeated = require_execution checked in
  Alcotest.(check string)
    "reviewed execution dump"
    (read_file "golden/integer-interpreter.txt")
    (VM.human first);
  Alcotest.(check string)
    "repeat execution" (VM.human first) (VM.human repeated)

let sema_type_and_word_type selector =
  if selector land 1 = 0 then (i64, VM.I64) else (u64, VM.U64)

let binary_word_property =
  QCheck.Test.make ~count:500
    ~name:"bounded binary execution matches direct 64-bit word operations"
    QCheck.(quad int64 int64 (int_bound 5) (int_bound 3))
    (fun (left_bits, right_bits, operation, type_selector) ->
      let left_type, left_word_type = sema_type_and_word_type type_selector in
      let right_type, right_word_type =
        sema_type_and_word_type (type_selector lsr 1)
      in
      let result_type, expected_word_type =
        match (left_word_type, right_word_type) with
        | VM.I64, VM.I64 -> (i64, VM.I64)
        | VM.I64, VM.U64 | VM.U64, VM.I64 | VM.U64, VM.U64 -> (u64, VM.U64)
      in
      let opcode, expected_bits =
        match operation with
        | 0 -> (Opcode.Ic_add, Int64.add left_bits right_bits)
        | 1 -> (Opcode.Ic_sub, Int64.sub left_bits right_bits)
        | 2 -> (Opcode.Ic_mul, Int64.mul left_bits right_bits)
        | 3 -> (Opcode.Ic_and, Int64.logand left_bits right_bits)
        | 4 -> (Opcode.Ic_or, Int64.logor left_bits right_bits)
        | _ -> (Opcode.Ic_xor, Int64.logxor left_bits right_bits)
      in
      let word =
        execute_binary ~opcode ~left_type ~left_bits ~right_type ~right_bits
          ~result_type
      in
      word.type_ = expected_word_type && Int64.equal word.bits expected_bits)

let branch_truth_property =
  QCheck.Test.make ~count:500
    ~name:"bounded conditional execution follows direct zero truth semantics"
    QCheck.(pair int64 bool)
    (fun (condition, branch_when_zero) ->
      let opcode =
        if branch_when_zero then Opcode.Ic_br_zero else Opcode.Ic_br_not_zero
      in
      let takes_target =
        if branch_when_zero then Int64.equal condition 0L
        else not (Int64.equal condition 0L)
      in
      let expected = if takes_target then 22L else 11L in
      let word =
        branch_graph opcode condition |> require_execution |> require_returned
      in
      word.type_ = VM.I64 && Int64.equal word.bits expected)

let tests =
  [
    Alcotest.test_case "word arithmetic, bitwise, and unary rules" `Quick
      integer_operations_use_exact_word_rules;
    Alcotest.test_case "zero and nonzero branches" `Quick
      zero_and_nonzero_branches_choose_exact_edges;
    Alcotest.test_case "deduplicated conditional fallthrough" `Quick
      conditional_target_equal_to_fallthrough_is_not_duplicated;
    Alcotest.test_case "empty block fallthrough" `Quick
      empty_blocks_fall_through_without_spending_steps;
    Alcotest.test_case "return latch across jump" `Quick
      return_value_survives_a_block_transfer;
    Alcotest.test_case "termination and expression discard" `Quick
      termination_forms_and_expression_discard_are_distinct;
    Alcotest.test_case "step boundaries and loop exhaustion" `Quick
      exact_step_boundaries_and_loop_exhaustion_are_reported;
    Alcotest.test_case "whole-graph preflight" `Quick
      whole_graph_preflight_includes_unreachable_blocks;
    Alcotest.test_case "bounded producer and return types" `Quick
      producer_domains_and_return_types_are_bounded;
    Alcotest.test_case "deterministic execution dump" `Quick
      human_output_is_versioned_and_deterministic;
    QCheck_alcotest.to_alcotest binary_word_property;
    QCheck_alcotest.to_alcotest branch_truth_property;
  ]
