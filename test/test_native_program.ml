open Holyc_lib
module Program = X86_64_program
module Expression = X86_64_expression
module Encoder = X86_64_encoder
module Sequence = Ir_instruction_sequence
module Graph = Ir_block_graph
module X87 = Ir_x87_stack
module VM = Ir_integer_interpreter
module Opcode = Ir_opcode
module Type = Semantic_type
module Runtime = Ir_runtime_call_context
module Native_defaults = Native_parameter_defaults
module Function_body = Ir_function_body
module Function_resolution = Semantic_function_resolution
module Headers = Semantic_function_type_resolution

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

let integer_unit ?(mode = Preprocessor.Jit) contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-program-callable.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id
  in
  compile_integer_program session ~config ~source
  |> require_ok (fun diagnostics ->
      diagnostics
      |> List.map (fun (error : Diagnostic.t) ->
          error.code ^ ": " ^ error.message)
      |> String.concat "; ")
  |> fun checked -> checked.value

let compile_callable ?status_abi unit =
  Program.compile_callable ?status_abi ~max_stack_bytes:4088 ~max_blocks:4096
    ~max_ir_instructions:4096 ~max_code_bytes:65536
    ~runtime_calls:(integer_program_runtime_calls unit)
    ~initialization:(integer_program_initialization unit)
    ~entry:(integer_program_entry unit)
    ~functions:(integer_program_functions unit)
    ()

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
  | Program.Call_depth_exceeded -> "call-depth"
  | Program.Frame_limit_exceeded -> "frame-limit"
  | Program.Native_stack_limit_exceeded -> "native-stack-limit"
  | Program.Uninitialized_read -> "uninitialized-read"

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
      Alcotest.(check (option int))
        "closed budget fault has no function owner" None fault.function_id;
      Alcotest.(check (option string))
        "closed budget fault has no function name" None fault.function_name;
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
      Alcotest.(check (option int))
        "closed arithmetic fault has no function owner" None fault.function_id;
      Alcotest.(check (option string))
        "closed arithmetic fault has no function name" None fault.function_name;
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
        "branch fault preserves consumed work" 1 fault.executed_steps;
      Alcotest.(check (option int))
        "closed branch fault has no function owner" None fault.function_id;
      Alcotest.(check (option string))
        "closed branch fault has no function name" None fault.function_name
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
  Alcotest.(check int)
    "encoded byte count excludes unwind metadata"
    (String.length original_code)
    (Program.code_bytes image);
  if String.length code > 0 then
    Bytes.set (Bytes.unsafe_of_string code) 0 '\xff';
  if String.length unwind > 0 then
    Bytes.set (Bytes.unsafe_of_string unwind) 0 '\xff';
  Alcotest.(check string)
    "code getter returns a fresh immutable copy" original_code
    (Program.code image);
  Alcotest.(check int)
    "export mutation preserves encoded byte count"
    (String.length original_code)
    (Program.code_bytes image);
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
    [
      "if(0 && (1/0)) 1/0; else {6*7;} while(1){break;1/0;} do {42;} while(0); \
       for(0;0;1/0) 1/0;";
      "I64 NeverCalled(){return 1/0;}\n\
       I64 Add(I64 a,I64 b){I64 sum=a+b;return sum;}\n\
       Add(20,22);";
      "I64 G=42; I64 F(){return G;} F();";
      "I64 F(){static I64 n=0;return ++n;} F();";
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let checked =
            source_program_compile ~mode source
            |> require_ok (fun diagnostics ->
                diagnostics
                |> List.map (fun (error : Diagnostic.t) ->
                    error.code ^ ": " ^ error.message)
                |> String.concat "; ")
          in
          Alcotest.(check bool)
            "compile-only source emits a nonempty bounded image" true
            (Program.block_count checked.value > 0);
          Alcotest.(check bool)
            "compile-only source has no warnings" true (checked.diagnostics = []))
        accepted;
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
          "I64 x[1]={1/0}; 42;";
          "I64 Bad(){F64 x=1.0;return 0;} 42;";
          "F64 Bad(){return 1.0;} 42;";
          "I64 F(){return 42;} I64 G=F(); G;";
          "I64 F(){static I64 n=1<<2;return ++n;} F();";
          "I64 Bad(){static I64 n={1/0};return 0;}42;";
          "I64 Bad(){static I64 *n;return 0;}42;";
          "I64 Bad(){static I64 n[2];return 0;}42;";
          "I64 Bad(){static I64 reg n;return 0;}42;";
          "I64 F(I64 **p){return **p;} 42;";
          "I64 F(I64 n,...){return n;} F(42);";
          "extern I64 F(I64 n); 42;";
          "I64 F(I64 n=1<<3){return n;} F(1);";
          "I64 Missing(I64 n){if(n)return 42;} Missing(1);";
          "I64 Missing(){42;} 0;";
          "I64 Apply(I64 (*fp)(I64),I64 n){return fp(n);}\n\
           I64 Inc(I64 n){return n+1;} Apply(&Inc,41);";
          "\"output\";";
          "#exe {1/0;}\n42;";
        ];
      List.iter
        (fun call ->
          match
            source_program_compile ~mode ("I64 F(I64 n=42){return n;} " ^ call)
          with
          | Ok _ -> ()
          | Error diagnostics ->
              Alcotest.failf
                "original scalar default failed source compilation: %s"
                (String.concat "; "
                   (List.map
                      (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
                      diagnostics)))
        [ "F();"; "F(1);"; "42;" ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let callable_ownership_joins_are_exact () =
  let closed = integer_unit "42;" in
  let left = integer_unit "I64 Left(I64 n){return n+1;}\nLeft(41);" in
  let right = integer_unit "I64 Right(I64 n){return n+2;}\nRight(40);" in
  let left_function = List.hd (integer_program_functions left) in
  let right_function = List.hd (integer_program_functions right) in
  let reject_callable label runtime_calls initialization entry functions =
    match
      Program.compile_callable ~max_stack_bytes:4088 ~max_blocks:4096
        ~max_ir_instructions:4096 ~max_code_bytes:65536 ~runtime_calls
        ~initialization ~entry ~functions ()
    with
    | Ok _ -> Alcotest.failf "%s unexpectedly compiled" label
    | Error [] -> Alcotest.failf "%s returned no backend error" label
    | Error _ -> ()
  in
  let closed_direct =
    Program.compile ~max_stack_bytes:4088 ~max_blocks:4096
      ~max_ir_instructions:4096 ~max_code_bytes:65536
      (integer_program_entry closed)
    |> require_ok program_errors
  in
  let closed_via_callable =
    compile_callable closed |> require_ok program_errors
  in
  Alcotest.(check string)
    "empty function bundle preserves closed image bytes"
    (Program.code closed_direct)
    (Program.code closed_via_callable);
  Alcotest.(check int)
    "empty function bundle remains a closed image" 0
    (Program.function_count closed_via_callable);
  Alcotest.(check int)
    "empty function bundle preserves entry stack charge"
    (Program.entry_stack_bytes closed_direct)
    (Program.entry_stack_bytes closed_via_callable);
  Alcotest.(check bool)
    "empty function bundle preserves unwind owner records" true
    (Program.windows_unwind_functions closed_direct
    = Program.windows_unwind_functions closed_via_callable);
  reject_callable "closed entry joined to foreign sealed call metadata"
    (integer_program_runtime_calls left)
    (integer_program_initialization closed)
    (integer_program_entry closed)
    [];
  reject_callable "closed entry joined to foreign empty initialization"
    (integer_program_runtime_calls closed)
    (integer_program_initialization left)
    (integer_program_entry closed)
    [];
  ignore (compile_callable left |> require_ok program_errors);
  let foreign_frame : VM.function_definition =
    { frame = right_function.frame; body = left_function.body }
  in
  reject_callable "body joined to a foreign checked frame"
    (integer_program_runtime_calls left)
    (integer_program_initialization left)
    (integer_program_entry left)
    [ foreign_frame ];
  reject_callable "entry joined to foreign sealed call metadata"
    (integer_program_runtime_calls right)
    (integer_program_initialization left)
    (integer_program_entry left)
    (integer_program_functions left);
  reject_callable "direct call with its checked definition omitted"
    (integer_program_runtime_calls left)
    (integer_program_initialization left)
    (integer_program_entry left)
    [];
  reject_callable "callable bundle joined to foreign empty initialization"
    (integer_program_runtime_calls left)
    (integer_program_initialization right)
    (integer_program_entry left)
    (integer_program_functions left)

let callable_prepared_defaults_are_rejected_at_argument_producer () =
  let compile_aot call =
    integer_unit ~mode:Preprocessor.Aot
      ("I64 F(I64 n=42){return n;}\n" ^ call ^ ";")
  in
  let compile_plain_aot call =
    integer_unit ~mode:Preprocessor.Aot
      ("I64 F(I64 n){return n;}\n" ^ call ^ ";")
  in
  let fixed_argument unit =
    let runtime_calls = integer_program_runtime_calls unit in
    let call_start =
      integer_program_entry unit |> X87.graph |> Graph.blocks
      |> List.concat_map (fun block ->
          Graph.instructions block |> Sequence.instructions)
      |> List.map Sequence.description
      |> List.find (fun (description : Sequence.description) ->
          description.opcode = Opcode.Ic_call_start)
    in
    let call =
      Runtime.find_start runtime_calls ~owner:Runtime.Entry
        call_start.instruction_id
      |> Option.get
    in
    let argument =
      Runtime.arguments call
      |> List.find (fun argument ->
          Runtime.argument_role argument = Runtime.Fixed 0)
    in
    (runtime_calls, argument)
  in
  let check_empty_initialization label unit =
    let initialization = integer_program_initialization unit in
    let globals = Ir_global_initialization.globals initialization in
    Alcotest.(check int)
      (label ^ " has no global bytes")
      0
      (Ir_integer_globals.byte_size globals);
    Alcotest.(check bool)
      (label ^ " has no global initializers")
      false
      (Ir_integer_globals.has_initializers globals);
    Alcotest.(check int)
      (label ^ " has no initialization regions")
      0
      (List.length (Ir_global_initialization.regions initialization));
    Alcotest.(check int)
      (label ^ " has no static initialization regions")
      0
      (List.length (Ir_global_initialization.static_regions initialization));
    Alcotest.(check int)
      (label ^ " has no initialization publications")
      0
      (List.length (Ir_global_initialization.publications initialization));
    Alcotest.(check int)
      (label ^ " has no prepared storage steps")
      0
      (Ir_global_initialization.prepared_steps initialization)
  in
  let explicit = compile_plain_aot "F(1)" in
  check_empty_initialization "explicit no-default unit" explicit;
  let explicit_calls, explicit_argument = fixed_argument explicit in
  Alcotest.(check bool)
    "explicit argument producer is not a prepared default" false
    (Runtime.is_prepared_default explicit_calls ~owner:Runtime.Entry
       (Runtime.argument_producer explicit_argument));
  ignore (compile_callable explicit |> require_ok program_errors);
  let defaulted = compile_aot "F()" in
  check_empty_initialization "omitted default-bearing unit" defaulted;
  let default_calls, default_argument = fixed_argument defaulted in
  Alcotest.(check bool)
    "omitted argument producer retains prepared-default metadata" true
    (Runtime.is_prepared_default default_calls ~owner:Runtime.Entry
       (Runtime.argument_producer default_argument));
  let default_initialization = integer_program_initialization defaulted in
  let default_globals =
    Ir_global_initialization.globals default_initialization
  in
  let default_prepared =
    Runtime.argument_prepared_default default_argument |> Option.get
  in
  Alcotest.(check bool)
    "a prepared value alone cannot mint native default authority" true
    (Native_defaults.create ~globals:default_globals
       ~runtime_calls:(integer_program_runtime_calls defaulted)
       ~initialization:default_initialization
       ~entry:(integer_program_entry defaulted)
       ~functions:(integer_program_functions defaulted)
       ~prepared:[ default_prepared ] ~completions:[]
    |> Result.is_error);
  (match compile_callable defaulted with
  | Ok _ -> Alcotest.fail "prepared default argument unexpectedly compiled"
  | Error [] -> Alcotest.fail "prepared default rejection returned no error"
  | Error (first :: _) ->
      Alcotest.(check string)
        "prepared default rejects at callable producer guard" "HCBACK0002"
        first.code;
      Alcotest.(check string)
        "prepared default guard reports its exact unsupported boundary"
        "IC_IMM_I64: native callable programs do not admit prepared parameter \
         defaults"
        first.message);
  let supplied_default = compile_aot "F(1)" in
  check_empty_initialization "supplied default-bearing unit" supplied_default;
  let supplied_calls, supplied_argument = fixed_argument supplied_default in
  Alcotest.(check bool)
    "supplied argument does not masquerade as a prepared default" false
    (Runtime.is_prepared_default supplied_calls ~owner:Runtime.Entry
       (Runtime.argument_producer supplied_argument));
  (match compile_callable supplied_default with
  | Ok _ -> Alcotest.fail "default-bearing source header unexpectedly compiled"
  | Error [] -> Alcotest.fail "default-bearing header returned no error"
  | Error (first :: _) ->
      Alcotest.(check string)
        "declared default rejects after supported explicit producer preflight"
        "HCBACK0002" first.code;
      Alcotest.(check string)
        "declared default uses the source-header boundary"
        "native source functions do not admit parameter defaults" first.message);
  let unused_default =
    integer_unit ~mode:Preprocessor.Aot "I64 F(I64 n=42){return n;}\n0;"
  in
  check_empty_initialization "unused default-bearing unit" unused_default;
  match compile_callable unused_default with
  | Ok _ ->
      Alcotest.fail "unused default-bearing function unexpectedly compiled"
  | Error [] ->
      Alcotest.fail "unused default-bearing function returned no error"
  | Error (first :: _) ->
      Alcotest.(check string)
        "unused declared default rejects without a call producer" "HCBACK0002"
        first.code;
      Alcotest.(check string)
        "unused declared default uses the source-header boundary"
        "native source functions do not admit parameter defaults" first.message

let callable_mid_block_ret_is_rejected () =
  let unit = integer_unit "I64 F(){return 42;}\nF();" in
  let definition = List.hd (integer_program_functions unit) in
  let graph = definition.body |> Ir_function_body.x87 |> X87.graph in
  let terminal_ret =
    Graph.blocks graph
    |> List.exists (fun block ->
        match Graph.instructions block |> Sequence.instructions |> List.rev with
        | instruction :: _ ->
            (Sequence.description instruction).opcode = Opcode.Ic_ret
        | [] -> false)
  in
  Alcotest.(check bool)
    "canonical callable retains a terminal shared IC_RET" true terminal_ret;
  ignore (compile_callable unit |> require_ok program_errors);
  let instructions =
    Graph.blocks graph
    |> List.find_map (fun block ->
        match Graph.instructions block |> Sequence.instructions with
        | first :: _ :: _ as instructions
          when (Sequence.description first).opcode <> Opcode.Ic_ret ->
            Some instructions
        | _ -> None)
    |> Option.get
  in
  let first = List.hd instructions |> Sequence.description in
  let mid_block_ret : Sequence.description =
    {
      first with
      opcode = Opcode.Ic_ret;
      operands = [];
      result = None;
      target_type = None;
      payload = None;
      flags = 0L;
    }
  in
  (* Graph/sequence constructors already reject instructions after terminators.
     Corrupt the immutable low-level fixture in place so this exercises the
     backend admission boundary itself rather than an earlier graph check. *)
  Obj.set_field (Obj.repr instructions) 0 (Obj.repr mid_block_ret);
  match compile_callable unit with
  | Ok _ -> Alcotest.fail "mid-block IC_RET unexpectedly compiled"
  | Error [] -> Alcotest.fail "mid-block IC_RET rejection returned no error"
  | Error (first :: _) ->
      Alcotest.(check string)
        "mid-block IC_RET rejects in callable preflight" "HCBACK0003" first.code;
      Alcotest.(check string)
        "mid-block IC_RET reports the exact terminator contract"
        "IC_RET: IC_RET must terminate its native source function block"
        first.message

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
      ( "load semantic-frame quota",
        Encoder.Load_context (Encoder.Rax, 48),
        "498b4330" );
      ( "store semantic-frame quota",
        Encoder.Store_context (48, Encoder.Rax),
        "49894330" );
      ( "load call-depth quota",
        Encoder.Load_context (Encoder.Rax, 56),
        "498b4338" );
      ( "store call-depth quota",
        Encoder.Store_context (56, Encoder.Rax),
        "49894338" );
      ( "load physical-stack quota",
        Encoder.Load_context (Encoder.Rax, 64),
        "498b4340" );
      ( "store physical-stack quota",
        Encoder.Store_context (64, Encoder.Rax),
        "49894340" );
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
      Encoder.Store_context (72, Encoder.Rax);
    ]

let callable_frame_encoder_bytes () =
  let frame_slot offset = Encoder.frame_slot ~offset |> require_ok Fun.id in
  let call_frame bytes = Encoder.call_frame ~bytes |> require_ok Fun.id in
  let cases =
    [
      ("push frame pointer", Encoder.Push_rbp, "55");
      ("establish frame pointer", Encoder.Mov_rbp_rsp, "4889e5");
      ( "load negative RBP slot",
        Encoder.Load_frame (Encoder.Rax, frame_slot (-8)),
        "488b85f8ffffff" );
      ( "store negative RBP slot",
        Encoder.Store_frame (frame_slot (-16), Encoder.Rcx),
        "48898df0ffffff" );
      ( "allocate 32-byte call frame",
        Encoder.Alloc_call_frame (call_frame 32),
        "4881ec20000000" );
      ("relative direct CALL", Encoder.Call 0x12345678L, "e878563412");
      ( "free 32-byte call frame",
        Encoder.Free_call_frame (call_frame 32),
        "4881c420000000" );
      ("pop frame pointer", Encoder.Pop_rbp, "5d");
    ]
  in
  List.iter
    (fun (label, instruction, expected) ->
      Alcotest.(check string) label expected (hex (Encoder.encode instruction));
      Alcotest.(check int)
        (label ^ " exact size")
        (String.length (Encoder.encode instruction))
        (Encoder.size instruction))
    cases

let int32_le text offset =
  let byte index = Int32.of_int (Char.code text.[offset + index]) in
  Int32.logor (byte 0)
    (Int32.logor
       (Int32.shift_left (byte 1) 8)
       (Int32.logor
          (Int32.shift_left (byte 2) 16)
          (Int32.shift_left (byte 3) 24)))

let compiled_callable_frame_and_rel32_bytes () =
  let unit =
    integer_unit "I64 Identity(I64 value){return value;}\nIdentity(42);"
  in
  let image =
    compile_callable ~status_abi:Program.System_v_x64 unit
    |> require_ok program_errors
  in
  let code = Program.code image in
  Alcotest.(check int) "one named callable" 1 (Program.function_count image);
  Alcotest.(check int)
    "identity bundle maximum frame" 32
    (Program.frame_bytes image);
  Alcotest.(check int)
    "identity entry physical stack charge" 48
    (Program.entry_stack_bytes image);
  match Program.windows_unwind_functions image with
  | [
   (0, entry_end, entry_unwind); (function_begin, function_end, function_unwind);
  ] -> (
      Alcotest.(check int)
        "entry ends where identity begins" entry_end function_begin;
      Alcotest.(check int)
        "identity range covers image tail" (String.length code) function_end;
      Alcotest.(check string)
        "entry PUSH/MOV/SUB frame bytes" "554889e54881ec20000000"
        (String.sub code 0 11 |> hex);
      Alcotest.(check string)
        "entry callable unwind bytes" "010b02000b320150" (hex entry_unwind);
      Alcotest.(check string)
        "identity PUSH/MOV frame bytes" "554889e5"
        (String.sub code function_begin 4 |> hex);
      Alcotest.(check string)
        "identity frameless unwind bytes" "0104010001500000"
        (hex function_unwind);
      Alcotest.(check string)
        "identity POP/RET bytes" "5dc3"
        (String.sub code (function_end - 2) 2 |> hex);
      let calls = ref [] in
      for offset = 0 to entry_end - 5 do
        if Char.code code.[offset] = 0xe8 then
          let displacement = int32_le code (offset + 1) |> Int32.to_int in
          if offset + 5 + displacement = function_begin then
            calls := offset :: !calls
      done;
      match List.rev !calls with
      | [ call_offset ] ->
          let displacement = int32_le code (call_offset + 1) |> Int32.to_int in
          Alcotest.(check int)
            "compiled CALL rel32 resolves to Identity owner" function_begin
            (call_offset + 5 + displacement)
      | offsets ->
          Alcotest.failf "identity entry has %d candidate CALL opcodes"
            (List.length offsets))
  | functions ->
      Alcotest.failf "identity bundle published %d unwind owners"
        (List.length functions)

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

let native_global_admission () =
  let module Globals = Ir_integer_globals in
  let module Layout = Holyc_lib__Backend.X86_64_global_storage in
  let source = "I8 G;G=42;G;" in
  List.iter
    (fun mode ->
      let unit = integer_unit ~mode source in
      let other = integer_unit ~mode source in
      let image = compile_callable unit |> require_ok program_errors in
      Alcotest.(check int) "logical scalar width" 1 (Program.global_bytes image);
      let exported = Program.global_image image in
      let original = Bytes.to_string (Bytes.of_string exported) in
      Bytes.fill
        (Bytes.unsafe_of_string exported)
        0 (String.length exported) '\255';
      Alcotest.(check string)
        "exported arena cannot mutate image" original
        (Program.global_image image);
      ignore
        (reject ~code:"HCBACK0003" "foreign initialization"
           (Program.compile_callable ~max_ir_instructions:4096
              ~max_code_bytes:65536
              ~runtime_calls:(integer_program_runtime_calls unit)
              ~initialization:(integer_program_initialization other)
              ~entry:(integer_program_entry unit)
              ~functions:[] ()));
      ignore
        (reject ~code:"HCBACK0003" "foreign calls"
           (Program.compile_callable ~max_ir_instructions:4096
              ~max_code_bytes:65536
              ~runtime_calls:(integer_program_runtime_calls other)
              ~initialization:(integer_program_initialization unit)
              ~entry:(integer_program_entry unit)
              ~functions:[] ()));
      let layout =
        Layout.create ~functions:[] ~max_global_bytes:1
          ~initialization:(integer_program_initialization unit)
          ~entry:(integer_program_entry unit)
        |> require_ok (fun es ->
            String.concat "; "
              (List.map (fun (e : Layout.error) -> e.message) es))
      in
      let own =
        Globals.slots (integer_program_globals unit)
        |> List.hd |> Globals.slot_symbol
      in
      let foreign =
        Globals.slots (integer_program_globals other)
        |> List.hd |> Globals.slot_symbol
      in
      Alcotest.(check bool)
        "owned symbol lookup" true
        (Option.is_some (Layout.find_symbol layout own));
      Alcotest.(check bool)
        "equal-spelling foreign symbol" true
        (Option.is_none (Layout.find_symbol layout foreign));
      let mutations =
        [
          ( "wrong address opcode",
            fun (d : Sequence.description) ->
              {
                d with
                opcode =
                  (if mode = Preprocessor.Jit then Opcode.Ic_abs_addr
                   else Opcode.Ic_imm_i64);
              } );
          ( "foreign symbol",
            fun d -> { d with payload = Some (Sequence.Symbol foreign) } );
          ( "wrong address type",
            fun d ->
              {
                d with
                target_type = Some (Type.pointer_to u64 |> require_ok Fun.id);
              } );
        ]
      in
      List.iter
        (fun (label, mutate) ->
          let unit = integer_unit ~mode source in
          ignore (compile_callable unit |> require_ok program_errors);
          let graph = integer_program_entry unit |> X87.graph in
          let rec producer = function
            | [] -> None
            | first :: rest as cell -> (
                let d = Sequence.description first in
                match d.payload with
                | Some (Sequence.Symbol _) -> Some (cell, d)
                | _ -> producer rest)
          in
          let cell, d =
            Graph.blocks graph
            |> List.find_map (fun b ->
                producer (Graph.instructions b |> Sequence.instructions))
            |> Option.get
          in
          (* Preserve the sealed graph owner while corrupting one producer, so the
         backend guard itself must reject the malformed storage instruction. *)
          Obj.set_field (Obj.repr cell) 0 (Obj.repr (mutate d));
          ignore (reject ~code:"HCBACK0003" label (compile_callable unit)))
        mutations)
    [ Preprocessor.Jit; Preprocessor.Aot ];
  List.iter
    (fun source ->
      let unit = integer_unit ~mode:Preprocessor.Aot source in
      ignore
        (reject ~code:"HCBACK0002" "unsupported storage" (compile_callable unit)))
    [ "I64 G=42;G;"; "I64 G[1];42;"; "I64 F(){static I64 G=1;return 42;}F();" ]

let native_static_admission () =
  let module Globals = Ir_integer_globals in
  let module Layout = Holyc_lib__Backend.X86_64_global_storage in
  let source =
    "I8 G;I64 F(){static I8 n;n=20;return n;}I64 H(){static I8 n;n=22;return \
     n;}F()+H();"
  in
  List.iter
    (fun mode ->
      let unit = integer_unit ~mode source in
      let other = integer_unit ~mode source in
      let definitions = integer_program_functions unit in
      let first = List.hd definitions in
      let second = List.nth definitions 1 in
      List.iter
        (fun abi ->
          ignore
            (compile_callable ~status_abi:abi unit |> require_ok program_errors))
        [ Program.System_v_x64; Program.Windows_x64 ];
      let layout functions =
        Layout.create ~functions ~max_global_bytes:17
          ~initialization:(integer_program_initialization unit)
          ~entry:(integer_program_entry unit)
      in
      let own =
        layout definitions
        |> require_ok (fun es ->
            String.concat "; "
              (List.map (fun (e : Layout.error) -> e.message) es))
      in
      let check_bad label functions =
        Alcotest.(check bool) label true (Result.is_error (layout functions))
      in
      check_bad "missing static function" [ second ];
      check_bad "duplicate static function" (first :: definitions);
      check_bad "foreign functions" (integer_program_functions other);
      check_bad "cross-function frame"
        [ { first with frame = second.frame }; second ];
      check_bad "reconstructed frame"
        [
          { first with frame = Obj.obj (Obj.dup (Obj.repr first.frame)) };
          second;
        ];
      let statics = Globals.statics (integer_program_globals unit) in
      let symbol slot = Globals.static_storage slot |> Globals.storage_symbol in
      let slot =
        Layout.find_symbol own (symbol (List.hd statics)) |> Option.get
      in
      Alcotest.(check bool)
        "reconstructed static symbol" true
        (Option.is_none
           (Layout.find_symbol own
              (Obj.obj (Obj.dup (Obj.repr (symbol (List.hd statics)))))));
      Alcotest.(check bool)
        "static exact function" true
        (Layout.owns_address slot (Runtime.Function first.body));
      Alcotest.(check bool)
        "static wrong function" false
        (Layout.owns_address slot (Runtime.Function second.body));
      Alcotest.(check bool)
        "static entry" false
        (Layout.owns_address slot Runtime.Entry);
      List.iter
        (fun (label, change) ->
          let unit = integer_unit ~mode source in
          ignore (compile_callable unit |> require_ok program_errors);
          let definitions = integer_program_functions unit in
          let first = List.hd definitions in
          let rec producer = function
            | [] -> None
            | head :: rest as cell -> (
                let d = Sequence.description head in
                match d.payload with
                | Some (Sequence.Symbol _) -> Some (cell, d)
                | _ -> producer rest)
          in
          let cell, d =
            Function_body.x87 first.body
            |> X87.graph |> Graph.blocks
            |> List.find_map (fun b ->
                producer (Graph.instructions b |> Sequence.instructions))
            |> Option.get
          in
          Obj.set_field (Obj.repr cell) 0 (Obj.repr (change unit d));
          ignore (reject ~code:"HCBACK0003" label (compile_callable unit)))
        [
          ( "cross-function static address",
            fun unit d ->
              {
                d with
                payload =
                  Some
                    (Sequence.Symbol
                       ( Globals.statics (integer_program_globals unit)
                       |> fun slots -> symbol (List.nth slots 1) ));
              } );
          ( "foreign static address",
            fun _ d ->
              {
                d with
                payload =
                  Some
                    (Sequence.Symbol
                       (Globals.statics (integer_program_globals other)
                       |> List.hd |> symbol));
              } );
          ( "wrong static address opcode",
            fun _ d ->
              {
                d with
                opcode =
                  (if mode = Preprocessor.Jit then Opcode.Ic_abs_addr
                   else Opcode.Ic_imm_i64);
              } );
          ( "wrong static address type",
            fun _ d ->
              {
                d with
                target_type = Some (Type.pointer_to u64 |> require_ok Fun.id);
              } );
          ( "static impersonates RBP",
            fun _ d -> { d with opcode = Opcode.Ic_rbp } );
          ( "numeric static address",
            fun _ d -> { d with payload = Some (Sequence.Integer 0L) } );
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let tests =
  [
    Alcotest.test_case "native static storage authority" `Quick
      native_static_admission;
    Alcotest.test_case "native global storage authority" `Quick
      native_global_admission;
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
    Alcotest.test_case "callable frame and CALL encodings are literal goldens"
      `Quick callable_frame_encoder_bytes;
    Alcotest.test_case "compiled callable frame and rel32 bytes are stable"
      `Quick compiled_callable_frame_and_rel32_bytes;
    Alcotest.test_case
      "compiled control uses literal forward/back rel32 offsets" `Quick
      compiled_control_rel32_bytes;
    Alcotest.test_case
      "source gate compiles control without VM or #exe execution" `Quick
      source_gate_is_compile_only;
    Alcotest.test_case "callable frame and call ownership joins are exact"
      `Quick callable_ownership_joins_are_exact;
    Alcotest.test_case
      "callable prepared defaults reject at the exact argument producer" `Quick
      callable_prepared_defaults_are_rejected_at_argument_producer;
    Alcotest.test_case "callable IC_RET must be terminal within its block"
      `Quick callable_mid_block_ret_is_rejected;
    Alcotest.test_case "legacy expression byte golden remains unchanged" `Quick
      legacy_expression_bytes;
  ]
