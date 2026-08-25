module Ir = Holyc_lib.Ir_instruction_sequence
module Opcode = Holyc_lib.Ir_opcode
module Type = Holyc_lib.Semantic_type
module Primitive = Holyc_lib.Primitive_type
module Symbol = Holyc_lib.Semantic_symbol

let require_ok show = function
  | Ok value -> value
  | Error error -> Alcotest.fail (show error)

let show_ir_error (error : Ir.error) = error.code ^ ": " ^ error.message

let instruction_id value =
  Ir.Instruction_id.of_int value |> require_ok show_ir_error

let value_id value = Ir.Value_id.of_int value |> require_ok show_ir_error
let block_id value = Ir.Block_id.of_int value |> require_ok show_ir_error

let primitive_type ?(pointer_depth = 0) ?(form = Type.Internal_storage)
    primitive =
  Type.make_primitive ~form ~primitive ~pointer_depth |> require_ok Fun.id

let i64 = primitive_type Primitive.I64
let f64 = primitive_type Primitive.F64

let u8_pointer =
  primitive_type ~pointer_depth:1 ~form:Type.Public_spelling Primitive.U8

let label_symbol =
  Symbol.create ~id:(Symbol.Id.of_int 9) ~scope_id:(Symbol.Scope_id.of_int 2)
    ~name:"entry\npoint" ~kind:Symbol.Label
    ~origin:(Symbol.Synthesized "IR test")

let description ?(operands = []) ?result ?target_type ?payload ?(flags = 0L)
    ?span id opcode =
  {
    Ir.instruction_id = instruction_id id;
    opcode;
    operands;
    result;
    target_type;
    payload;
    flags;
    span;
  }

let result id = Ir.{ value_id = value_id id }

let require_sequence descriptions =
  match Ir.create descriptions with
  | Ok sequence -> sequence
  | Error errors ->
      Alcotest.fail (String.concat "; " (List.map show_ir_error errors))

let error_codes descriptions =
  match Ir.create descriptions with
  | Ok _ -> Alcotest.fail "expected IR validation to fail"
  | Error errors -> List.map (fun (error : Ir.error) -> error.code) errors

let has_code expected codes =
  Alcotest.(check bool) expected true (List.mem expected codes)

let ids_are_checked () =
  let check_negative label result =
    match result with
    | Ok _ -> Alcotest.failf "%s accepted a negative ID" label
    | Error (error : Ir.error) ->
        Alcotest.(check string) label "HCIR0001" error.code
  in
  check_negative "instruction" (Ir.Instruction_id.of_int (-1));
  check_negative "value" (Ir.Value_id.of_int (-1));
  check_negative "block" (Ir.Block_id.of_int (-1));
  Alcotest.(check int)
    "instruction round trip" 4
    (Ir.Instruction_id.to_int (instruction_id 4));
  Alcotest.(check int) "value round trip" 5 (Ir.Value_id.to_int (value_id 5));
  Alcotest.(check int) "block round trip" 6 (Ir.Block_id.to_int (block_id 6))

let metadata_shapes_are_enforced () =
  let zero =
    description ~result:(result 0) ~target_type:i64 0 Opcode.Ic_imm_i64
  in
  ignore (require_sequence [ zero ]);
  let one = description ~operands:[ value_id 0 ] 1 Opcode.Ic_end_exp in
  ignore (require_sequence [ zero; one ]);
  let second =
    description ~result:(result 1) ~target_type:i64 2 Opcode.Ic_imm_i64
  in
  let two =
    description
      ~operands:[ value_id 0; value_id 1 ]
      ~result:(result 2) ~target_type:i64 3 Opcode.Ic_add
  in
  ignore (require_sequence [ zero; second; two ]);
  let variable =
    description
      ~operands:[ value_id 0; value_id 1; value_id 2 ]
      4 Opcode.Ic_add_rsp
  in
  ignore (require_sequence [ zero; second; two; variable ])

let every_opcode_accepts_its_declared_shape () =
  List.iter
    (fun opcode ->
      let info = Opcode.info opcode in
      let operand_count =
        match info.argument_count with
        | Opcode.Zero -> 0
        | Opcode.One -> 1
        | Opcode.Two -> 2
        | Opcode.Variable -> 3
      in
      let inputs =
        List.init operand_count (fun index ->
            description ~result:(result index) ~target_type:i64 index
              Opcode.Ic_imm_i64)
      in
      let target =
        description
          ~operands:(List.init operand_count value_id)
          ?result:
            (if info.result_count = 0 then None
             else if info.result_count = 1 then Some (result 100)
             else
               Alcotest.failf "%s declares unsupported result count %d"
                 info.source_name info.result_count)
          ?target_type:(if info.result_count = 1 then Some i64 else None)
          100 opcode
      in
      ignore (require_sequence (inputs @ [ target ])))
    Opcode.all

let operand_count_errors_are_stable () =
  description 0 Opcode.Ic_add |> fun invalid ->
  error_codes [ invalid ] |> has_code "HCIR0004"

let result_count_errors_are_stable () =
  let missing = description ~target_type:i64 0 Opcode.Ic_imm_i64 in
  error_codes [ missing ] |> has_code "HCIR0005";
  let extra = description ~result:(result 0) ~target_type:i64 0 Opcode.Ic_end in
  error_codes [ extra ] |> has_code "HCIR0005"

let value_results_require_a_type () =
  let invalid = description ~result:(result 0) 0 Opcode.Ic_imm_i64 in
  error_codes [ invalid ] |> has_code "HCIR0010";
  let typed_no_result = description ~target_type:i64 0 Opcode.Ic_end in
  ignore (require_sequence [ typed_no_result ])

let duplicate_ids_are_rejected () =
  let first =
    description ~result:(result 0) ~target_type:i64 0 Opcode.Ic_imm_i64
  in
  let duplicate_instruction =
    description ~result:(result 1) ~target_type:i64 0 Opcode.Ic_imm_i64
  in
  error_codes [ first; duplicate_instruction ] |> has_code "HCIR0006";
  let duplicate_value =
    description ~result:(result 0) ~target_type:i64 1 Opcode.Ic_imm_i64
  in
  error_codes [ first; duplicate_value ] |> has_code "HCIR0007"

let invalid_value_uses_are_rejected () =
  let forward =
    description
      ~operands:[ value_id 1; value_id 1 ]
      ~result:(result 0) ~target_type:i64 0 Opcode.Ic_add
  in
  let later =
    description ~result:(result 1) ~target_type:i64 1 Opcode.Ic_imm_i64
  in
  error_codes [ forward; later ] |> has_code "HCIR0008";
  let undefined = description ~operands:[ value_id 9 ] 0 Opcode.Ic_end_exp in
  error_codes [ undefined ] |> has_code "HCIR0009"

let spans_and_flags_are_checked () =
  let source = Holyc_lib.Source_id.of_int 3 |> require_ok Fun.id in
  let invalid_span : Holyc_lib.Span.t = { source; start = 8; stop = 2 } in
  let invalid =
    description ~span:invalid_span ~flags:0x00100000L 0 Opcode.Ic_end
  in
  let codes = error_codes [ invalid ] in
  has_code "HCIR0002" codes;
  has_code "HCIR0003" codes;
  (match Ir.create [ invalid ] with
  | Ok _ -> Alcotest.fail "expected invalid metadata to fail"
  | Error errors ->
      List.iter
        (fun (error : Ir.error) ->
          Alcotest.(check (option int))
            "offending instruction" (Some 0) error.instruction_id;
          Alcotest.(check bool)
            "offending span" true
            (error.span = Some invalid_span))
        errors);
  Alcotest.(check int64)
    "source-grounded flag mask" 0x1ffe1ffffL Ir.known_flag_mask

let deterministic_human_dump () =
  let source = Holyc_lib.Source_id.of_int 7 |> require_ok Fun.id in
  let span = Holyc_lib.Span.unsafe_make ~source ~start:2 ~stop:9 in
  let descriptions =
    [
      description ~result:(result 0) ~target_type:i64
        ~payload:(Ir.Integer Int64.min_int) ~span 0 Opcode.Ic_imm_i64;
      description ~result:(result 1) ~target_type:f64
        ~payload:(Ir.Float_bits 0x3ff0000000000000L) 1 Opcode.Ic_imm_f64;
      description ~result:(result 2) ~target_type:u8_pointer
        ~payload:(Ir.Bytes "A\000\n\"\\\255") 2 Opcode.Ic_str_const;
      description ~target_type:i64 ~payload:(Ir.Symbol label_symbol) 3
        Opcode.Ic_label;
      description ~payload:(Ir.Block (block_id 2)) 4 Opcode.Ic_jmp;
    ]
  in
  let sequence = require_sequence descriptions in
  let repeated = require_sequence descriptions in
  let expected =
    "holyc-ir-v1 reference=c26482bb6ad3f80106d28504ec5db3c6a360732c\n\
     !i0 %v0:internal:I64 = IC_IMM_I64 i64:-9223372036854775808 \
     flags=0x000000000 @source=7:2..9\n\
     !i1 %v1:internal:F64 = IC_IMM_F64 f64:0x3ff0000000000000 flags=0x000000000\n\
     !i2 %v2:public:U8* = IC_STR_CONST bytes:\"A\\x00\\n\\\"\\\\\\xff\" \
     flags=0x000000000\n\
     !i3 IC_LABEL type=internal:I64 symbol:@s9:label:\"entry\\npoint\" \
     flags=0x000000000\n\
     !i4 IC_JMP block:^b2 flags=0x000000000\n"
  in
  Alcotest.(check string) "versioned dump" expected (Ir.human sequence);
  Alcotest.(check string)
    "repeat construction" (Ir.human sequence) (Ir.human repeated);
  Alcotest.(check int) "sequence length" 5 (Ir.length sequence);
  Alcotest.(check int)
    "ordered traversal" 4
    ( Ir.instructions sequence |> fun items ->
      List.nth_opt items 4 |> Option.get |> Ir.description |> fun item ->
      Ir.Instruction_id.to_int item.instruction_id )

let valid_chain values =
  let initial =
    description ~result:(result 0) ~target_type:i64 ~payload:(Ir.Integer 0L) 0
      Opcode.Ic_imm_i64
  in
  let _, reversed =
    List.fold_left
      (fun (next, instructions) value ->
        let constant =
          description ~result:(result next) ~target_type:i64
            ~payload:(Ir.Integer value)
            ((next * 2) - 1)
            Opcode.Ic_imm_i64
        in
        let added =
          description
            ~operands:[ value_id (next - 1); value_id next ]
            ~result:(result (next + 1))
            ~target_type:i64 (next * 2) Opcode.Ic_add
        in
        (next + 2, added :: constant :: instructions))
      (1, [ initial ]) values
  in
  List.rev reversed

let deterministic_property =
  QCheck.Test.make ~count:500 ~name:"valid IR dumps are deterministic"
    QCheck.(list_small int64)
    (fun values ->
      match Ir.create (valid_chain values) with
      | Error _ -> false
      | Ok sequence -> String.equal (Ir.human sequence) (Ir.human sequence))

let error_signature errors =
  List.map
    (fun (error : Ir.error) ->
      (error.code, error.message, error.instruction_id, error.span))
    errors

let deterministic_error_property =
  QCheck.Test.make ~count:500 ~name:"IR validation errors are deterministic"
    QCheck.(int_bound 10_000)
    (fun raw_id ->
      let invalid =
        description ~operands:[ value_id (raw_id + 1) ] raw_id Opcode.Ic_end_exp
      in
      match (Ir.create [ invalid ], Ir.create [ invalid ]) with
      | Error first, Error second ->
          error_signature first = error_signature second
      | Ok _, _ | _, Ok _ -> false)

let tests =
  [
    Alcotest.test_case "checked IDs" `Quick ids_are_checked;
    Alcotest.test_case "metadata shapes" `Quick metadata_shapes_are_enforced;
    Alcotest.test_case "all opcode shapes" `Quick
      every_opcode_accepts_its_declared_shape;
    Alcotest.test_case "operand count errors" `Quick
      operand_count_errors_are_stable;
    Alcotest.test_case "result count errors" `Quick
      result_count_errors_are_stable;
    Alcotest.test_case "result target types" `Quick value_results_require_a_type;
    Alcotest.test_case "duplicate IDs" `Quick duplicate_ids_are_rejected;
    Alcotest.test_case "value use order" `Quick invalid_value_uses_are_rejected;
    Alcotest.test_case "span and flag validation" `Quick
      spans_and_flags_are_checked;
    Alcotest.test_case "deterministic human dump" `Quick
      deterministic_human_dump;
    QCheck_alcotest.to_alcotest deterministic_property;
    QCheck_alcotest.to_alcotest deterministic_error_property;
  ]
