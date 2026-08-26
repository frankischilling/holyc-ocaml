module Literal = Holyc_lib.Ir_literal_lowering
module Sequence = Holyc_lib.Ir_instruction_sequence
module Opcode = Holyc_lib.Ir_opcode
module Type = Holyc_lib.Semantic_type
module Primitive = Holyc_lib.Primitive_type
module Ast = Holyc_lib.Ast
module Parser = Holyc_lib.Parser
module Preprocessor = Holyc_lib.Preprocessor
module Session = Holyc_lib.Session
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

let parsed_location span = Ast.make_location ~span ~source_segments:[ span ] ()

let parsed_literal constructor ~span ~spelling value =
  constructor
    (Ast.make_expression_literal ~origin:Ast.Source_literal ~spelling ~value
       ~location:(parsed_location span))

let lower_parsed expression =
  Literal.lower_expression ~instruction_id:(instruction_id 7)
    ~value_id:(value_id 11) expression
  |> require_ok (fun errors ->
      String.concat "; " (List.map show_sequence_error errors))

let require_lowered = function
  | Literal.Lowered lowered -> lowered
  | Literal.Not_literal -> Alcotest.fail "expected a parsed literal"

let parse_initializer contents =
  let session = Session.create () in
  let source = Session.add_source session ~path:"literal.HC" ~contents in
  let config =
    Preprocessor.Config.create ~working_directory:(Sys.getcwd ()) ()
    |> require_ok Fun.id
  in
  let output = Holyc_lib.parse_detailed session ~config ~source in
  let ast =
    match output.Parser.ast with
    | Some ast -> ast
    | None ->
        Alcotest.failf "expected parsed literal, got diagnostics: %s"
          (String.concat "; "
             (List.map
                (fun diagnostic ->
                  diagnostic.Holyc_lib.Diagnostic.code ^ ": "
                  ^ diagnostic.message)
                output.diagnostics))
  in
  match ast.Ast.items with
  | [ Ast.Global_declaration declaration ] -> (
      match declaration.declarators with
      | [ declarator ] -> (
          match declarator.global_initial_value with
          | Some global_initial -> (
              match global_initial.global_initializer_value with
              | Ast.Scalar_initializer expression -> expression
              | Ast.Braced_initializer _ | Ast.Unbraced_array_initializer _ ->
                  Alcotest.fail "expected one scalar global initializer")
          | None -> Alcotest.fail "expected one scalar global initializer")
      | declarators ->
          Alcotest.failf "expected one global declarator, got %d"
            (List.length declarators))
  | items ->
      Alcotest.failf "expected one global declaration, got %d items"
        (List.length items)

let test_parsed_literals_preserve_payloads_and_spans () =
  let source = Source_id.of_int 8 |> require_ok Fun.id in
  let span start stop = Span.unsafe_make ~source ~start ~stop in
  let cases =
    [
      ( parsed_literal
          (fun value -> Ast.Integer_literal value)
          ~span:(span 1 3) ~spelling:"-7" (Ast.Integer_value (-7L)),
        Literal.Integer (-7L),
        span 1 3 );
      ( parsed_literal
          (fun value -> Ast.Character_literal value)
          ~span:(span 4 7) ~spelling:"'A'" (Ast.Integer_value 65L),
        Literal.Character 65L,
        span 4 7 );
      ( parsed_literal
          (fun value -> Ast.Float_literal value)
          ~span:(span 8 11) ~spelling:"nan"
          (Ast.Float_value (Int64.float_of_bits 0x7ff8000000000042L)),
        Literal.Float_bits 0x7ff8000000000042L,
        span 8 11 );
      ( parsed_literal
          (fun value -> Ast.String_literal value)
          ~span:(span 12 18) ~spelling:"\"A\\n\"" (Ast.Bytes_value "A\n"),
        Literal.String_bytes "A\n",
        span 12 18 );
    ]
  in
  List.iter
    (fun (expression, expected_literal, expected_span) ->
      let lowered = lower_parsed expression |> require_lowered in
      let description = only_description lowered in
      Alcotest.(check bool)
        "literal payload" true
        (Literal.human lowered
        = Literal.human (lower ~span:expected_span expected_literal));
      Alcotest.(check bool)
        "source span" true
        (description.span = Some expected_span))
    cases

let test_nonliteral_expression_is_explicit () =
  let source = Source_id.of_int 9 |> require_ok Fun.id in
  let span = Span.unsafe_make ~source ~start:2 ~stop:6 in
  let identifier =
    Ast.make_identifier ~spelling:"value" ~location:(parsed_location span)
  in
  match lower_parsed (Ast.Identifier_expression identifier) with
  | Literal.Not_literal -> ()
  | Literal.Lowered _ -> Alcotest.fail "nonliteral expression produced IR"

let test_parser_literals_cross_checked_boundary () =
  let cases =
    [
      ("I64 value=42;", Sequence.Integer 42L, 10, 12);
      ("I64 value='A';", Sequence.Integer 65L, 10, 13);
      ("F64 value=1.25;", Sequence.Float_bits (Int64.bits_of_float 1.25), 10, 14);
      ("U8 *value=\"A\\n\";", Sequence.Bytes "A\n", 10, 15);
    ]
  in
  List.iter
    (fun (source_text, expected_payload, expected_start, expected_stop) ->
      let expression = parse_initializer source_text in
      let lowered = lower_parsed expression |> require_lowered in
      let description = only_description lowered in
      Alcotest.(check bool)
        "decoded parser payload" true
        (description.payload = Some expected_payload);
      match description.span with
      | Some span ->
          Alcotest.(check int) "parser span start" expected_start span.start;
          Alcotest.(check int) "parser span stop" expected_stop span.stop
      | None -> Alcotest.fail "parsed literal lost its source span")
    cases

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
    Alcotest.test_case "parsed literals preserve payloads and spans" `Quick
      test_parsed_literals_preserve_payloads_and_spans;
    Alcotest.test_case "nonliteral expression is explicit" `Quick
      test_nonliteral_expression_is_explicit;
    Alcotest.test_case "parser literals cross checked boundary" `Quick
      test_parser_literals_cross_checked_boundary;
  ]
