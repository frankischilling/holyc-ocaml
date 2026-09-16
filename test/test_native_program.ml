open Holyc_lib
module Program = X86_64_program
module Expression = X86_64_expression
module Encoder = X86_64_encoder
module Sequence = Ir_instruction_sequence
module Graph = Ir_block_graph
module X87 = Ir_x87_stack
module Opcode = Ir_opcode
module Type = Semantic_type

let require_ok show = function
  | Ok value -> value
  | Error errors -> Alcotest.fail (show errors)

let sequence_error (error : Sequence.error) = error.code ^ ": " ^ error.message

let graph_errors errors =
  errors
  |> List.map (fun (error : Graph.error) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let x87_errors errors =
  errors
  |> List.map (fun (error : X87.error) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let program_errors errors =
  errors
  |> List.map (fun (error : Program.error) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let instruction_id value =
  Sequence.Instruction_id.of_int value |> require_ok sequence_error

let value_id value = Sequence.Value_id.of_int value |> require_ok sequence_error
let block_id value = Sequence.Block_id.of_int value |> require_ok sequence_error

let primitive primitive =
  Type.make_primitive ~form:Type.Internal_storage ~primitive ~pointer_depth:0
  |> require_ok Fun.id

let i64 = primitive Primitive_type.I64
let u64 = primitive Primitive_type.U64
let fixture_source = Source_id.of_int 657 |> require_ok Fun.id
let span start = Span.unsafe_make ~source:fixture_source ~start ~stop:(start + 1)

let description ?(operands = []) ?result ?target_type ?payload ?(flags = 0L)
    ?span id opcode : Sequence.description =
  {
    instruction_id = instruction_id id;
    opcode;
    operands = List.map value_id operands;
    result = Option.map (fun id -> { Sequence.value_id = value_id id }) result;
    target_type;
    payload;
    flags;
    span;
  }

let imm ?(type_ = i64) ?span id value bits =
  description ~result:value ~target_type:type_ ~payload:(Sequence.Integer bits)
    ?span id Opcode.Ic_imm_i64

let binary ?(type_ = i64) ?span id value opcode left right =
  description ~operands:[ left; right ] ~result:value ~target_type:type_ ?span
    id opcode

let unary ?(type_ = i64) ?span id value opcode operand =
  description ~operands:[ operand ] ~result:value ~target_type:type_ ?span id
    opcode

let end_expression ?span id operand =
  description ~operands:[ operand ] ~flags:0x200L ?span id Opcode.Ic_end_exp

let jump ?span id target =
  description ~payload:(Sequence.Block (block_id target)) ?span id Opcode.Ic_jmp

let branch ?span id opcode operand target =
  description ~operands:[ operand ]
    ~payload:(Sequence.Block (block_id target))
    ?span id opcode

let stream_end ?span id = description ?span id Opcode.Ic_end

let block id instructions : Graph.block_description =
  { block_id = block_id id; instructions }

let verified ~entry blocks =
  Graph.create ~entry:(block_id entry) blocks
  |> require_ok graph_errors |> X87.verify |> require_ok x87_errors

let compile ?status_abi ?(max_stack_bytes = 4088) ?(max_blocks = 4096)
    ?(max_ir_instructions = 100000) ?(max_code_bytes = 16 * 1024 * 1024) graph =
  Program.compile ?status_abi ~max_stack_bytes ~max_blocks ~max_ir_instructions
    ~max_code_bytes graph

let image ?status_abi graph =
  compile ?status_abi graph |> require_ok program_errors

let source_program_compile ?(mode = Preprocessor.Jit) contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-program-compile.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id
  in
  Native_program.compile session ~config ~source

let reject ?code label = function
  | Ok _ -> Alcotest.failf "%s unexpectedly compiled" label
  | Error [] -> Alcotest.failf "%s returned no diagnostic" label
  | Error errors ->
      Option.iter
        (fun expected ->
          Alcotest.(check bool)
            (label ^ " contains " ^ expected)
            true
            (List.exists
               (fun (error : Program.error) -> error.code = expected)
               errors))
        code;
      errors

let type_name = function
  | Program.I64 -> "I64"
  | Program.U64 -> "U64"

let kind_name = function
  | Program.Division_by_zero -> "division-by-zero"
  | Program.Signed_division_overflow -> "signed-overflow"
  | Program.Step_limit_exceeded -> "step-limit"

let operation_name = function
  | None -> "none"
  | Some Program.Divide -> "divide"
  | Some Program.Remainder -> "remainder"

let multiply_graph () =
  verified ~entry:17
    [
      block 17
        [
          imm ~span:(span 10) 101 201 6L;
          imm ~span:(span 20) 305 405 7L;
          binary ~span:(span 30) 509 609 Opcode.Ic_mul 201 405;
          end_expression ~span:(span 40) 713 609;
          stream_end ~span:(span 50) 917;
        ];
    ]

let empty_graph () =
  verified ~entry:23 [ block 23 [ stream_end ~span:(span 60) 3001 ] ]

let last_value_graph () =
  verified ~entry:5
    [
      block 5
        [
          imm ~span:(span 70) 11 21 7L;
          end_expression ~span:(span 71) 31 21;
          imm ~type_:u64 ~span:(span 72) 101 111 (-1L);
          end_expression ~span:(span 73) 201 111;
          stream_end ~span:(span 74) 301;
        ];
    ]

let dense_fault_graph () =
  verified ~entry:42
    [
      block 42
        [
          imm ~span:(span 100) 100 10 1L;
          branch ~span:(span 101) 501 Opcode.Ic_br_zero 10 99;
        ];
      block 7
        [
          imm ~span:(span 102) 900 20 84L;
          imm ~span:(span 103) 1200 30 0L;
          binary ~span:(span 104) 1700 40 Opcode.Ic_div 20 30;
          end_expression ~span:(span 105) 1900 40;
          jump ~span:(span 106) 2300 99;
        ];
      block 99 [ stream_end ~span:(span 107) 4000 ];
    ]

let branch_graph () =
  verified ~entry:10
    [
      block 10
        [
          imm ~span:(span 120) 10 10 0L;
          branch ~span:(span 121) 20 Opcode.Ic_br_zero 10 30;
        ];
      block 20
        [
          imm ~span:(span 122) 30 30 1L;
          end_expression ~span:(span 123) 40 30;
          jump ~span:(span 124) 50 40;
        ];
      block 30
        [
          imm ~span:(span 125) 60 60 42L;
          end_expression ~span:(span 126) 70 60;
          jump ~span:(span 127) 80 40;
        ];
      block 40 [ stream_end ~span:(span 128) 90 ];
    ]

let control_rel32_graph () =
  verified ~entry:0
    [
      block 0 [ jump ~span:(span 160) 100 2 ];
      block 1 [ jump ~span:(span 161) 200 3 ];
      block 2
        [
          imm ~span:(span 162) 300 300 0L;
          branch ~span:(span 163) 400 Opcode.Ic_br_not_zero 300 1;
        ];
      block 3 [ stream_end ~span:(span 164) 500 ];
    ]

let empty_transfer_graph () =
  verified ~entry:42
    [
      block 5 [ imm 100 100 7L; end_expression 101 100 ];
      block 42 [];
      block 17 [];
      block 99 [ imm 200 200 42L; end_expression 201 200; stream_end 202 ];
    ]

let pressure_graph count =
  if count < 2 then invalid_arg "pressure graph needs at least two values";
  let definitions =
    List.init count (fun index ->
        imm
          ~span:(span (200 + index))
          (1000 + (index * 3))
          index
          (Int64.of_int (index + 1)))
  in
  let rec reductions next_id next_value accumulator = function
    | [] ->
        [
          end_expression
            ~span:(span (4000 + next_value))
            (next_id + 7) accumulator;
          stream_end ~span:(span (5000 + next_value)) (next_id + 19);
        ]
    | operand :: rest ->
        binary
          ~span:(span (3000 + next_value))
          next_id next_value Opcode.Ic_add accumulator operand
        :: reductions (next_id + 11) (next_value + 1) next_value rest
  in
  let operands = List.init (count - 1) (fun index -> index + 1) in
  verified ~entry:0
    [ block 0 (definitions @ reductions 100000 count 0 operands) ]

let decoder_completion () =
  let image = image (multiply_graph ()) in
  Alcotest.(check int) "five IR instructions" 5 (Program.ir_instructions image);
  Alcotest.(check int) "one source block" 1 (Program.block_count image);
  match
    Program.decode_runtime_status image ~max_steps:5 ~kind:0L ~site:0L
      ~executed_steps:5L ~value_site:4L ~bits:42L
    |> require_ok Fun.id
  with
  | Program.Completed execution -> (
      Alcotest.(check int) "all five steps reached" 5 execution.executed_steps;
      match execution.final_value with
      | Some word ->
          Alcotest.(check string) "last value type" "I64" (type_name word.type_);
          Alcotest.(check int64) "last value bits" 42L word.bits
      | None -> Alcotest.fail "multiply completion lost END_EXP value")
  | Program.Fault _ -> Alcotest.fail "multiply completion decoded as a fault"

let decoder_budget_and_empty () =
  let multiply = image (multiply_graph ()) in
  (match
     Program.decode_runtime_status multiply ~max_steps:4 ~kind:3L ~site:5L
       ~executed_steps:4L ~value_site:4L ~bits:42L
     |> require_ok Fun.id
   with
  | Program.Fault fault ->
      Alcotest.(check string)
        "budget fault kind" "step-limit" (kind_name fault.kind);
      Alcotest.(check string)
        "budget has no arithmetic operation" "none"
        (operation_name fault.operation);
      Alcotest.(check int) "budget block id" 17 fault.block_id;
      Alcotest.(check int)
        "budget sparse instruction id" 917 fault.instruction_id;
      Alcotest.(check int) "budget block position" 4 fault.position;
      Alcotest.(check int) "budget global position" 4 fault.global_position;
      Alcotest.(check int) "budget consumed steps" 4 fault.executed_steps;
      Alcotest.(check bool)
        "budget source span" true
        (fault.span = Some (span 50))
  | Program.Completed _ -> Alcotest.fail "budget fault decoded as completion");
  let empty = image (empty_graph ()) in
  Alcotest.(check int)
    "empty program peak includes R10/R11 context plus RAX exit scratch" 3
    (Program.register_peak empty);
  match
    Program.decode_runtime_status empty ~max_steps:1 ~kind:0L ~site:0L
      ~executed_steps:1L ~value_site:0L ~bits:0L
    |> require_ok Fun.id
  with
  | Program.Completed execution ->
      Alcotest.(check int)
        "empty END consumes one step" 1 execution.executed_steps;
      Alcotest.(check bool)
        "empty stream has no value" true
        (Option.is_none execution.final_value)
  | Program.Fault _ -> Alcotest.fail "empty stream decoded as a fault"

let decoder_last_value () =
  let image = image (last_value_graph ()) in
  match
    Program.decode_runtime_status image ~max_steps:5 ~kind:0L ~site:0L
      ~executed_steps:5L ~value_site:4L ~bits:(-1L)
    |> require_ok Fun.id
  with
  | Program.Completed { final_value = Some word; _ } ->
      Alcotest.(check string)
        "later END_EXP determines type" "U64" (type_name word.type_);
      Alcotest.(check int64) "later END_EXP determines bits" (-1L) word.bits
  | Program.Completed _ -> Alcotest.fail "last END_EXP value was lost"
  | Program.Fault _ -> Alcotest.fail "last-value completion decoded as a fault"

let dense_fault_sites () =
  let image = image (dense_fault_graph ()) in
  (* Sites are dense over source-block order despite sparse block/instruction IDs:
     two instructions in block 42 precede the third instruction in block 7. *)
  match
    Program.decode_runtime_status image ~max_steps:20 ~kind:1L ~site:5L
      ~executed_steps:5L ~value_site:0L ~bits:0L
    |> require_ok Fun.id
  with
  | Program.Fault fault ->
      Alcotest.(check string)
        "arithmetic kind" "division-by-zero" (kind_name fault.kind);
      Alcotest.(check string)
        "arithmetic operation" "divide"
        (operation_name fault.operation);
      Alcotest.(check int) "original block id" 7 fault.block_id;
      Alcotest.(check int) "sparse instruction id" 1700 fault.instruction_id;
      Alcotest.(check int) "zero-based block position" 2 fault.position;
      Alcotest.(check int) "zero-based global position" 4 fault.global_position;
      Alcotest.(check int)
        "faulting instruction consumed" 5 fault.executed_steps;
      Alcotest.(check bool)
        "original operator span" true
        (fault.span = Some (span 104))
  | Program.Completed _ ->
      Alcotest.fail "arithmetic fault decoded as completion"

let branch_step_site_metadata () =
  let image = image (branch_graph ()) in
  match
    Program.decode_runtime_status image ~max_steps:1 ~kind:3L ~site:2L
      ~executed_steps:1L ~value_site:0L ~bits:0L
    |> require_ok Fun.id
  with
  | Program.Fault fault ->
      Alcotest.(check string)
        "branch meter kind" "step-limit" (kind_name fault.kind);
      Alcotest.(check int) "branch source block id" 10 fault.block_id;
      Alcotest.(check int)
        "branch sparse instruction id" 20 fault.instruction_id;
      Alcotest.(check int) "branch block position" 1 fault.position;
      Alcotest.(check int) "branch global position" 1 fault.global_position;
      Alcotest.(check bool)
        "branch exact source span" true
        (fault.span = Some (span 121));
      Alcotest.(check int)
        "branch fault preserves consumed work" 1 fault.executed_steps
  | Program.Completed _ ->
      Alcotest.fail "branch budget site decoded as completion"

let malformed_status_rejected () =
  let image = image (multiply_graph ()) in
  let reject_status ?(compiled = image) ?expected label ~max_steps ~kind ~site
      ~executed_steps ~value_site ~bits =
    match
      Program.decode_runtime_status compiled ~max_steps ~kind ~site
        ~executed_steps ~value_site ~bits
    with
    | Error message ->
        Alcotest.(check bool)
          (label ^ " explains rejection")
          true (message <> "");
        Option.iter
          (fun expected ->
            Alcotest.(check string)
              (label ^ " validation path")
              expected message)
          expected
    | Ok _ -> Alcotest.failf "%s unexpectedly decoded" label
  in
  reject_status "unknown kind"
    ~expected:"native program status has an unknown fault kind" ~max_steps:5
    ~kind:9L ~site:1L ~executed_steps:0L ~value_site:0L ~bits:0L;
  reject_status "zero execution budget" ~max_steps:0 ~kind:3L ~site:1L
    ~executed_steps:0L ~value_site:0L ~bits:0L;
  reject_status "negative execution budget" ~max_steps:(-1) ~kind:3L ~site:1L
    ~executed_steps:0L ~value_site:0L ~bits:0L;
  reject_status "success with site" ~max_steps:5 ~kind:0L ~site:1L
    ~executed_steps:5L ~value_site:4L ~bits:42L;
  reject_status "executed count exceeds budget" ~max_steps:5 ~kind:0L ~site:0L
    ~executed_steps:6L ~value_site:4L ~bits:42L;
  reject_status "step fault before full budget" ~max_steps:5 ~kind:3L ~site:5L
    ~executed_steps:4L ~value_site:4L ~bits:42L;
  reject_status "unknown value site" ~max_steps:5 ~kind:0L ~site:0L
    ~expected:"native program status names an unknown execution site"
    ~executed_steps:5L ~value_site:6L ~bits:42L;
  reject_status "known non-END_EXP value site" ~max_steps:5 ~kind:0L ~site:0L
    ~expected:"native program value site does not identify an IC_END_EXP"
    ~executed_steps:5L ~value_site:3L ~bits:42L;
  reject_status "no value with nonzero bits" ~max_steps:5 ~kind:0L ~site:0L
    ~executed_steps:5L ~value_site:0L ~bits:1L;
  reject_status "arithmetic kind on non-arithmetic site" ~max_steps:5 ~kind:1L
    ~site:3L ~executed_steps:3L ~value_site:0L ~bits:0L;
  List.iter
    (fun opcode ->
      let compiled =
        verified ~entry:0
          [
            block 0
              [
                imm ~type_:u64 1 1 Int64.min_int;
                imm ~type_:u64 2 2 (-1L);
                binary ~type_:u64 3 3 opcode 1 2;
                end_expression 4 3;
                stream_end 5;
              ];
          ]
        |> compile |> require_ok program_errors
      in
      reject_status ~compiled "signed overflow at an unsigned arithmetic site"
        ~expected:"native program signed-overflow status names an unsigned site"
        ~max_steps:5 ~kind:2L ~site:3L ~executed_steps:3L ~value_site:0L
        ~bits:0L)
    [ Opcode.Ic_div; Opcode.Ic_mod ]

let unreachable_preflight () =
  let graph =
    verified ~entry:0
      [
        block 0 [ jump 10 2 ];
        block 1 [ description ~span:(span 150) 20 Opcode.Ic_nop1; jump 30 2 ];
        block 2 [ stream_end 40 ];
      ]
  in
  ignore
    (compile graph |> reject ~code:"HCBACK0002" "unreachable unsupported opcode")

let exact_limits () =
  let graph = branch_graph () in
  let baseline = image graph in
  let ir = Program.ir_instructions baseline in
  let blocks = Program.block_count baseline in
  let bytes = String.length (Program.code baseline) in
  ignore
    (compile ~max_ir_instructions:ir ~max_blocks:blocks ~max_code_bytes:bytes
       graph
    |> require_ok program_errors);
  ignore
    (compile ~max_ir_instructions:(ir - 1) graph
    |> reject ~code:"HCBACK0001" "IR one-below");
  ignore
    (compile ~max_blocks:(blocks - 1) graph
    |> reject ~code:"HCBACK0001" "block one-below");
  ignore
    (compile ~max_code_bytes:(bytes - 1) graph
    |> reject ~code:"HCBACK0005" "code one-below")

let spill_limits () =
  let graph = pressure_graph 6 in
  let register_only = image (pressure_graph 5) in
  Alcotest.(check int)
    "five simultaneous values fit the program register set" 0
    (Program.frame_bytes register_only);
  let baseline = image graph in
  Alcotest.(check int)
    "six live values need one program spill slot" 8
    (Program.frame_bytes baseline);
  Alcotest.(check int)
    "five value registers plus R10/R11 private state" 7
    (Program.register_peak baseline);
  ignore (compile ~max_stack_bytes:8 graph |> require_ok program_errors);
  ignore
    (compile ~max_stack_bytes:7 graph
    |> reject ~code:"HCBACK0004" "frame one-below");
  ignore
    (compile ~max_stack_bytes:0 graph
    |> reject ~code:"HCBACK0004" "spilling disabled")

let maximum_frame () =
  let graph = pressure_graph 516 in
  let baseline = image graph in
  Alcotest.(check int)
    "five registers plus 511 slots" 4088
    (Program.frame_bytes baseline);
  ignore (compile ~max_stack_bytes:4088 graph |> require_ok program_errors);
  ignore
    (compile ~max_stack_bytes:4087 graph
    |> reject ~code:"HCBACK0004" "maximum frame one-below")

let hard_limit_graph () =
  let count = 100_000 in
  let blocks =
    List.init count (fun index ->
        if index = count - 1 then block index [ stream_end index ]
        else block index [ imm index index (Int64.of_int index) ])
  in
  verified ~entry:0 blocks

let hard_ir_and_block_limits () =
  let graph = hard_limit_graph () in
  let exact =
    compile ~max_ir_instructions:100_000 ~max_blocks:100_000
      ~max_code_bytes:(16 * 1024 * 1024)
      graph
    |> require_ok program_errors
  in
  Alcotest.(check int)
    "hard IR bound admits exactly 100000" 100_000
    (Program.ir_instructions exact);
  Alcotest.(check int)
    "hard block bound admits exactly 100000" 100_000
    (Program.block_count exact);
  ignore
    (compile ~max_ir_instructions:99_999 ~max_blocks:100_000 graph
    |> reject ~code:"HCBACK0001" "hard IR one-below");
  ignore
    (compile ~max_ir_instructions:100_000 ~max_blocks:99_999 graph
    |> reject ~code:"HCBACK0001" "hard block one-below");
  ignore
    (compile ~max_blocks:100_001 graph
    |> reject ~code:"HCBACK0001" "block configuration above hard maximum")

let immutable_exports () =
  let image = image (pressure_graph 6) in
  let code = Program.code image in
  let unwind = Program.windows_unwind_info image in
  let original_code = Bytes.to_string (Bytes.of_string code) in
  let original_unwind = Bytes.to_string (Bytes.of_string unwind) in
  if String.length code > 0 then
    Bytes.set (Bytes.unsafe_of_string code) 0 '\xff';
  if String.length unwind > 0 then
    Bytes.set (Bytes.unsafe_of_string unwind) 0 '\xff';
  Alcotest.(check string)
    "code getter returns a fresh immutable copy" original_code
    (Program.code image);
  Alcotest.(check string)
    "unwind getter returns a fresh immutable copy" original_unwind
    (Program.windows_unwind_info image)

let legacy_expression_bytes () =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"legacy-native-expression.hc"
      ~contents:"6*7;"
  in
  let config = Preprocessor.Config.create () |> require_ok Fun.id in
  let image =
    Native_expression.compile session ~config ~source
    |> require_ok (fun errors ->
        errors
        |> List.map (fun (error : Diagnostic.t) ->
            error.code ^ ": " ^ error.message)
        |> String.concat "; ")
  in
  let hex text =
    String.to_seq text
    |> Seq.map (fun byte -> Printf.sprintf "%02x" (Char.code byte))
    |> List.of_seq |> String.concat ""
  in
  Alcotest.(check string)
    "program backend addition does not change legacy expression bytes"
    "48b8060000000000000048b90700000000000000480fafc1c3"
    (hex (Expression.code image))

let source_gate_is_compile_only () =
  let accepted =
    "if(0 && (1/0)) 1/0; else {6*7;} while(1){break;1/0;} do {42;} while(0); \
     for(0;0;1/0) 1/0;"
  in
  List.iter
    (fun mode ->
      let checked =
        source_program_compile ~mode accepted
        |> require_ok (fun diagnostics ->
            diagnostics
            |> List.map (fun (error : Diagnostic.t) ->
                error.code ^ ": " ^ error.message)
            |> String.concat "; ")
      in
      Alcotest.(check bool)
        "compile-only closed source keeps control blocks" true
        (Program.block_count checked.value > 1);
      Alcotest.(check bool)
        "compile-only closed source has no warnings" true
        (checked.diagnostics = []);
      List.iter
        (fun source ->
          match source_program_compile ~mode source with
          | Ok _ -> Alcotest.failf "unsupported source compiled: %s" source
          | Error diagnostics ->
              Alcotest.(check bool)
                "source gate reports a diagnostic" true (diagnostics <> []);
              Alcotest.(check bool)
                "rejected source performed no VM/runtime arithmetic" false
                (List.exists
                   (fun (error : Diagnostic.t) ->
                     String.starts_with ~prefix:"HCIRVM" error.code
                     || String.starts_with ~prefix:"HCNATIVE" error.code)
                   diagnostics))
        [
          "I64 x=1/0; 42;";
          "I64 F(){return 1/0;} F();";
          "\"output\";";
          "#exe {1/0;}\n42;";
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let hex text =
  String.to_seq text
  |> Seq.map (fun byte -> Printf.sprintf "%02x" (Char.code byte))
  |> List.of_seq |> String.concat ""

let private_context_encoder_bytes () =
  let cases =
    [
      ( "load max-steps into R10",
        Encoder.Load_context (Encoder.R10, 16),
        "4d8b5310" );
      ( "store executed steps from RAX",
        Encoder.Store_context (24, Encoder.Rax),
        "49894318" );
      ( "store step-fault kind",
        Encoder.Store_context_imm (0, 3),
        "49c7430003000000" );
      ("decrement private meter", Encoder.Dec Encoder.R10, "49ffca");
    ]
  in
  List.iter
    (fun (label, instruction, expected) ->
      Alcotest.(check string) label expected (hex (Encoder.encode instruction)))
    cases;
  List.iter
    (fun instruction ->
      match
        try Ok (Encoder.encode instruction)
        with Invalid_argument message -> Error message
      with
      | Error message ->
          Alcotest.(check bool)
            "invalid private context access explains failure" true
            (message <> "")
      | Ok _ -> Alcotest.fail "invalid private context access encoded")
    [
      Encoder.Load_context (Encoder.Rax, 7);
      Encoder.Store_context (48, Encoder.Rax);
    ]

let compiled_control_rel32_bytes () =
  let image = image ~status_abi:Program.System_v_x64 (control_rel32_graph ()) in
  Alcotest.(check int)
    "control golden is frameless" 0
    (Program.frame_bytes image);
  let code = Program.code image in
  let slice offset length =
    if String.length code < offset + length then
      Alcotest.failf "control image ended before byte range %d..%d" offset
        (offset + length - 1);
    String.sub code offset length |> hex
  in
  (* Independent offsets from fixed instruction widths, not backend size reports:
     prefix Capture/Load/JMP = 3+4+5 = 12 bytes.
     block 0: meter 3+6+3, then JMP at 24 -> block 2 byte 46: +17.
     block 1 starts 29: its JMP at 41 -> block 3 byte 94: +48.
     block 2's backward JNE is at 83, ends 89 -> block 1 byte 29: -60.
     Its explicit fallthrough trampoline ends exactly at block 3 byte 94. *)
  Alcotest.(check string)
    "forward block0 -> block2 rel32" "e911000000" (slice 24 5);
  Alcotest.(check string)
    "forward block1 -> block3 rel32" "e930000000" (slice 41 5);
  Alcotest.(check string)
    "backward block2 -> block1 JNE rel32" "0f85c4ffffff" (slice 83 6);
  Alcotest.(check string)
    "adjacent branch fallthrough keeps JMP rel32=0" "e900000000" (slice 89 5)

let tests =
  [
    Alcotest.test_case "decoder preserves completion and exact step count"
      `Quick decoder_completion;
    Alcotest.test_case "budget and empty-stream status semantics" `Quick
      decoder_budget_and_empty;
    Alcotest.test_case "END_EXP publishes the last reached word" `Quick
      decoder_last_value;
    Alcotest.test_case
      "dense sites retain sparse block and instruction metadata" `Quick
      dense_fault_sites;
    Alcotest.test_case "branch step site retains block ID span and sparse ID"
      `Quick branch_step_site_metadata;
    Alcotest.test_case "malformed private context is rejected" `Quick
      malformed_status_rejected;
    Alcotest.test_case "unreachable unsupported producers fail preflight" `Quick
      unreachable_preflight;
    Alcotest.test_case "IR block and code limits are exact" `Quick exact_limits;
    Alcotest.test_case "five-register spill frame has exact one-below boundary"
      `Quick spill_limits;
    Alcotest.test_case "maximum 4088-byte frame has exact boundary" `Slow
      maximum_frame;
    Alcotest.test_case "100000 IR and block hard bounds are real" `Slow
      hard_ir_and_block_limits;
    Alcotest.test_case "program code and unwind exports are immutable" `Quick
      immutable_exports;
    Alcotest.test_case "private six-word context encodings are literal goldens"
      `Quick private_context_encoder_bytes;
    Alcotest.test_case
      "compiled control uses literal forward/back rel32 offsets" `Quick
      compiled_control_rel32_bytes;
    Alcotest.test_case
      "source gate compiles control without VM or #exe execution" `Quick
      source_gate_is_compile_only;
    Alcotest.test_case "legacy expression byte golden remains unchanged" `Quick
      legacy_expression_bytes;
  ]
