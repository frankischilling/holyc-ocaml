module Literal = Holyc_lib.Ir_literal_lowering
module Sequence = Holyc_lib.Ir_instruction_sequence
module Opcode = Holyc_lib.Ir_opcode
module Type = Holyc_lib.Semantic_type
module Primitive = Holyc_lib.Primitive_type
module Source_id = Holyc_lib.Source_id
module Span = Holyc_lib.Span

let require_ok show = function
  | Ok value -> value
  | Error error -> Alcotest.fail (show error)

let show_sequence_error (error : Sequence.error) =
  error.code ^ ": " ^ error.message

let instruction_id value =
  Sequence.Instruction_id.of_int value |> require_ok show_sequence_error

let value_id value =
  Sequence.Value_id.of_int value |> require_ok show_sequence_error

let lower ?span literal =
  Literal.lower
    { instruction_id = instruction_id 7; value_id = value_id 11; literal; span }
  |> require_ok (fun errors ->
      String.concat "; " (List.map show_sequence_error errors))

let only_description lowered =
  match Literal.sequence lowered |> Sequence.instructions with
  | [ instruction ] -> Sequence.description instruction
  | instructions ->
      Alcotest.failf "expected one instruction, got %d"
        (List.length instructions)

let test_negative_integer_uses_u64 () =
  let lowered = lower (Literal.Integer Int64.min_int) in
  let description = only_description lowered in
  Alcotest.(check int)
    "instruction ID" 7
    (Sequence.Instruction_id.to_int description.instruction_id);
  Alcotest.(check int)
    "value ID" 11
    (match description.result with
    | Some result -> Sequence.Value_id.to_int result.value_id
    | None -> Alcotest.fail "expected a produced value");
  Alcotest.(check bool)
    "opcode" true
    (Opcode.equal Opcode.Ic_imm_i64 description.opcode);
  Alcotest.(check bool)
    "payload" true
    (description.payload = Some (Sequence.Integer Int64.min_int));
  Alcotest.(check bool)
    "target type" true
    (match description.target_type with
    | Some type_ -> (
        match (Type.base type_, Type.pointer_depth type_) with
        | Type.Primitive (Type.Internal_storage, Primitive.U64), 0 -> true
        | _ -> false)
    | None -> false)

let test_nonnegative_integers_use_i64 () =
  List.iter
    (fun value ->
      let lowered = lower (Literal.Integer value) in
      let type_ = Literal.result_type lowered in
      Alcotest.(check bool)
        (Int64.to_string value) true
        (match (Type.base type_, Type.pointer_depth type_) with
        | Type.Primitive (Type.Internal_storage, Primitive.I64), 0 -> true
        | _ -> false))
    [ 0L; Int64.max_int ]

let test_character_uses_integer_path () =
  let lowered = lower (Literal.Character 0x4142L) in
  let description = only_description lowered in
  Alcotest.(check bool)
    "opcode" true
    (Opcode.equal Opcode.Ic_imm_i64 description.opcode);
  Alcotest.(check bool)
    "payload" true
    (description.payload = Some (Sequence.Integer 0x4142L));
  Alcotest.(check bool)
    "target type" true
    (match description.target_type with
    | Some type_ -> (
        match (Type.base type_, Type.pointer_depth type_) with
        | Type.Primitive (Type.Internal_storage, Primitive.I64), 0 -> true
        | _ -> false)
    | None -> false)

let test_f64_preserves_exact_bits () =
  let bits = 0x7ff8000000000042L in
  let lowered = lower (Literal.Float_bits bits) in
  let description = only_description lowered in
  Alcotest.(check bool)
    "opcode" true
    (Opcode.equal Opcode.Ic_imm_f64 description.opcode);
  Alcotest.(check bool)
    "payload" true
    (description.payload = Some (Sequence.Float_bits bits));
  Alcotest.(check bool)
    "target type" true
    (match description.target_type with
    | Some type_ -> (
        match (Type.base type_, Type.pointer_depth type_) with
        | Type.Primitive (Type.Internal_storage, Primitive.F64), 0 -> true
        | _ -> false)
    | None -> false)

let test_string_preserves_decoded_bytes () =
  let bytes = "A\000\n\"\\\255" in
  let lowered = lower (Literal.String_bytes bytes) in
  let description = only_description lowered in
  Alcotest.(check bool)
    "opcode" true
    (Opcode.equal Opcode.Ic_str_const description.opcode);
  Alcotest.(check bool)
    "payload" true
    (description.payload = Some (Sequence.Bytes bytes));
  Alcotest.(check bool)
    "target type" true
    (match description.target_type with
    | Some type_ -> (
        match (Type.base type_, Type.pointer_depth type_) with
        | Type.Primitive (Type.Internal_storage, Primitive.U8), 1 -> true
        | _ -> false)
    | None -> false)

let test_deterministic_literal_dump () =
  let source = Source_id.of_int 5 |> require_ok Fun.id in
  let span = Span.unsafe_make ~source ~start:13 ~stop:21 in
  let literal = Literal.String_bytes "A\000\n\"\\\255" in
  let lowered = lower ~span literal in
  let repeated = lower ~span literal in
  let expected =
    "holyc-ir-literal-v1 reference=c26482bb6ad3f80106d28504ec5db3c6a360732c\n\
     kind=string result-type=internal:U8*\n\
     !i7 %v11:internal:U8* = IC_STR_CONST bytes:\"A\\x00\\n\\\"\\\\\\xff\" \
     flags=0x000000000 @source=5:13..21\n"
  in
  Alcotest.(check string) "versioned dump" expected (Literal.human lowered);
  Alcotest.(check string)
    "repeat construction" (Literal.human lowered) (Literal.human repeated)

let test_invalid_span_is_rejected () =
  let source = Source_id.of_int 2 |> require_ok Fun.id in
  let span : Span.t = { source; start = 9; stop = 4 } in
  let result =
    Literal.lower
      {
        instruction_id = instruction_id 19;
        value_id = value_id 23;
        literal = Literal.Integer 0L;
        span = Some span;
      }
  in
  match result with
  | Ok _ -> Alcotest.fail "expected the checked sequence to reject the span"
  | Error [ error ] ->
      Alcotest.(check string) "diagnostic code" "HCIR0002" error.code;
      Alcotest.(check (option int))
        "instruction context" (Some 19) error.instruction_id;
      Alcotest.(check bool) "span context" true (error.span = Some span)
  | Error errors ->
      Alcotest.failf "expected one error, got %d" (List.length errors)

let tests =
  [
    Alcotest.test_case "negative integer uses U64" `Quick
      test_negative_integer_uses_u64;
    Alcotest.test_case "nonnegative integers use I64" `Quick
      test_nonnegative_integers_use_i64;
    Alcotest.test_case "character uses integer path" `Quick
      test_character_uses_integer_path;
    Alcotest.test_case "F64 preserves exact bits" `Quick
      test_f64_preserves_exact_bits;
    Alcotest.test_case "string preserves decoded bytes" `Quick
      test_string_preserves_decoded_bytes;
    Alcotest.test_case "deterministic literal dump" `Quick
      test_deterministic_literal_dump;
    Alcotest.test_case "invalid span is rejected" `Quick
      test_invalid_span_is_rejected;
  ]
