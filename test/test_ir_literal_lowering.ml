module Literal = Holyc_lib.Ir_literal_lowering
module Sequence = Holyc_lib.Ir_instruction_sequence
module Opcode = Holyc_lib.Ir_opcode
module Type = Holyc_lib.Semantic_type
module Primitive = Holyc_lib.Primitive_type
module Semantic_result = Holyc_lib.Semantic_function_call_expression_result
module Semantic_source = Holyc_lib.Semantic_function_call_resolution
module Semantic_symbol = Holyc_lib.Semantic_symbol
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

let identity instruction value : Literal.identity =
  { instruction_id = instruction_id instruction; value_id = value_id value }

let lower_parsed_result ?(unary_identities = []) expression =
  Literal.lower_expression ~instruction_id:(instruction_id 7)
    ~value_id:(value_id 11) ~unary_identities expression

let lower_parsed ?(unary_identities = []) expression =
  lower_parsed_result ~unary_identities expression
  |> require_ok (fun errors ->
      String.concat "; " (List.map show_sequence_error errors))

let require_lowered = function
  | Literal.Lowered lowered -> lowered
  | Literal.Not_literal -> Alcotest.fail "expected a parsed literal"

let parenthesize ~location expression =
  Ast.Parenthesized_expression
    (Ast.make_parenthesized_expression ~opening_parenthesis:location ~expression
       ~closing_parenthesis:location ~location)

let prefix ~operator_kind ~spelling ~location operand =
  let operator = Ast.make_expression_operator ~spelling ~location in
  Ast.Prefix_expression
    (Ast.make_prefix_expression ~operator_kind ~operator ~operand ~location)

let unary_plus ~location expression =
  prefix ~operator_kind:Ast.Unary_plus ~spelling:"+" ~location expression

let unary_minus ~location expression =
  prefix ~operator_kind:Ast.Unary_minus ~spelling:"-" ~location expression

let logical_not ~location expression =
  prefix ~operator_kind:Ast.Logical_not ~spelling:"!" ~location expression

let bitwise_not ~location expression =
  prefix ~operator_kind:Ast.Bitwise_not ~spelling:"~" ~location expression

let dereference ~location expression =
  prefix ~operator_kind:Ast.Dereference ~spelling:"*" ~location expression

let address_of ~location expression =
  prefix ~operator_kind:Ast.Address_of ~spelling:"&" ~location expression

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

let test_grouped_parser_literals_are_transparent () =
  let check plain_text grouped_text expected_literal expected_payload
      (expected_start, expected_stop) =
    let plain_expression = parse_initializer plain_text in
    let plain =
      lower_parsed plain_expression |> require_lowered |> only_description
    in
    let grouped_expression = parse_initializer grouped_text in
    let grouped = lower_parsed grouped_expression |> require_lowered in
    let description = only_description grouped in
    Alcotest.(check bool)
      "same checked instruction except source position" true
      ({ plain with span = description.span } = description);
    Alcotest.(check bool)
      "grouped payload" true
      (description.payload = Some expected_payload);
    match description.span with
    | Some span ->
        Alcotest.(check int)
          "inner literal span start" expected_start span.start;
        Alcotest.(check int) "inner span stop" expected_stop span.stop;
        Alcotest.(check string)
          "source kind and checked instruction"
          (Literal.human (lower ~span expected_literal))
          (Literal.human grouped)
    | None -> Alcotest.fail "grouped literal lost its source span"
  in
  check "I64 value=42;" "I64 value=((42));" (Literal.Integer 42L)
    (Sequence.Integer 42L) (12, 14);
  check "I64 value='A';" "I64 value=(('A'));" (Literal.Character 65L)
    (Sequence.Integer 65L) (12, 15);
  check "F64 value=1.25;" "F64 value=((1.25));"
    (Literal.Float_bits (Int64.bits_of_float 1.25))
    (Sequence.Float_bits (Int64.bits_of_float 1.25))
    (12, 16);
  check "U8 *value=\"A\\n\";" "U8 *value=((\"A\\n\"));"
    (Literal.String_bytes "A\n") (Sequence.Bytes "A\n") (12, 17)

let test_grouped_nonliteral_is_explicit () =
  let source = Source_id.of_int 10 |> require_ok Fun.id in
  let span = Span.unsafe_make ~source ~start:3 ~stop:8 in
  let location = parsed_location span in
  let identifier = Ast.make_identifier ~spelling:"value" ~location in
  let expression =
    Ast.Identifier_expression identifier |> parenthesize ~location
    |> parenthesize ~location
  in
  match lower_parsed expression with
  | Literal.Not_literal -> ()
  | Literal.Lowered _ -> Alcotest.fail "grouped nonliteral produced IR"

let test_deep_grouping_uses_constant_host_stack () =
  let source = Source_id.of_int 11 |> require_ok Fun.id in
  let literal_span = Span.unsafe_make ~source ~start:20 ~stop:22 in
  let group_span = Span.unsafe_make ~source ~start:0 ~stop:42 in
  let group_location = parsed_location group_span in
  let expression =
    ref
      (parsed_literal
         (fun value -> Ast.Integer_literal value)
         ~span:literal_span ~spelling:"42" (Ast.Integer_value 42L))
  in
  for _ = 1 to 100_000 do
    expression := parenthesize ~location:group_location !expression
  done;
  let lowered = lower_parsed !expression |> require_lowered in
  let description = only_description lowered in
  Alcotest.(check bool)
    "deep payload" true
    (description.payload = Some (Sequence.Integer 42L));
  Alcotest.(check bool)
    "inner literal span" true
    (description.span = Some literal_span);
  Alcotest.(check int)
    "instruction ID" 7
    (Sequence.Instruction_id.to_int description.instruction_id);
  Alcotest.(check int)
    "value ID" 11
    (match description.result with
    | Some result -> Sequence.Value_id.to_int result.value_id
    | None -> Alcotest.fail "expected a produced value")

let test_unary_plus_parser_literals_are_transparent () =
  let check plain_text wrapped_text expected_literal expected_payload
      (expected_start, expected_stop) =
    let plain_expression = parse_initializer plain_text in
    let plain =
      lower_parsed plain_expression |> require_lowered |> only_description
    in
    let wrapped_expression = parse_initializer wrapped_text in
    let wrapped = lower_parsed wrapped_expression |> require_lowered in
    let description = only_description wrapped in
    Alcotest.(check bool)
      "same checked instruction except source position" true
      ({ plain with span = description.span } = description);
    Alcotest.(check bool)
      "unary-plus payload" true
      (description.payload = Some expected_payload);
    match description.span with
    | Some span ->
        Alcotest.(check int) "inner span start" expected_start span.start;
        Alcotest.(check int) "inner span stop" expected_stop span.stop;
        Alcotest.(check string)
          "source kind and checked instruction"
          (Literal.human (lower ~span expected_literal))
          (Literal.human wrapped)
    | None -> Alcotest.fail "unary-plus literal lost its source span"
  in
  check "I64 value=42;" "I64 value=+((+42));" (Literal.Integer 42L)
    (Sequence.Integer 42L) (14, 16);
  check "I64 value='A';" "I64 value=+((+'A'));" (Literal.Character 65L)
    (Sequence.Integer 65L) (14, 17);
  check "F64 value=1.25;" "F64 value=+((+1.25));"
    (Literal.Float_bits (Int64.bits_of_float 1.25))
    (Sequence.Float_bits (Int64.bits_of_float 1.25))
    (14, 18);
  check "U8 *value=\"A\\n\";" "U8 *value=+((+\"A\\n\"));"
    (Literal.String_bytes "A\n") (Sequence.Bytes "A\n") (14, 19)

let test_unary_minus_parser_literals_emit_instructions () =
  let check plain_text negated_text (operator_start, operator_stop)
      (literal_start, literal_stop) =
    let plain =
      parse_initializer plain_text
      |> lower_parsed |> require_lowered |> only_description
    in
    let lowered =
      parse_initializer negated_text
      |> lower_parsed ~unary_identities:[ identity 8 12 ]
      |> require_lowered
    in
    match Literal.sequence lowered |> Sequence.instructions with
    | [ literal_instruction; unary_instruction ] ->
        let literal = Sequence.description literal_instruction in
        let unary = Sequence.description unary_instruction in
        Alcotest.(check bool)
          "literal instruction unchanged except source position" true
          ({ plain with span = literal.span } = literal);
        Alcotest.(check bool)
          "unary opcode" true
          (Opcode.equal Opcode.Ic_unary_minus unary.opcode);
        Alcotest.(check bool)
          "unary operand" true
          (unary.operands = [ value_id 11 ]);
        Alcotest.(check bool)
          "unary result" true
          (unary.result = Some { value_id = value_id 12 });
        Alcotest.(check bool)
          "forwarded result type" true
          (unary.target_type = literal.target_type);
        Alcotest.(check bool) "no folded payload" true (unary.payload = None);
        Alcotest.(check bool)
          "literal span" true
          (match literal.span with
          | Some span -> span.start = literal_start && span.stop = literal_stop
          | None -> false);
        Alcotest.(check bool)
          "operator span" true
          (match unary.span with
          | Some span ->
              span.start = operator_start && span.stop = operator_stop
          | None -> false)
    | instructions ->
        Alcotest.failf "expected two instructions, got %d"
          (List.length instructions)
  in
  check "I64 value=42;" "I64 value=-42;" (10, 11) (11, 13);
  check "I64 value='A';" "I64 value=-'A';" (10, 11) (11, 14);
  check "F64 value=1.25;" "F64 value=-1.25;" (10, 11) (11, 15);
  check "U8 *value=\"A\\n\";" "U8 *value=-\"A\\n\";" (10, 11) (11, 16)

let test_logical_not_parser_literals_emit_instructions () =
  let check plain_text negated_text (operator_start, operator_stop)
      (literal_start, literal_stop) =
    let plain =
      parse_initializer plain_text
      |> lower_parsed |> require_lowered |> only_description
    in
    let lowered =
      parse_initializer negated_text
      |> lower_parsed ~unary_identities:[ identity 8 12 ]
      |> require_lowered
    in
    match Literal.sequence lowered |> Sequence.instructions with
    | [ literal_instruction; unary_instruction ] ->
        let literal = Sequence.description literal_instruction in
        let unary = Sequence.description unary_instruction in
        Alcotest.(check bool)
          "literal instruction unchanged except source position" true
          ({ plain with span = literal.span } = literal);
        Alcotest.(check bool)
          "logical-not opcode" true
          (Opcode.equal Opcode.Ic_not unary.opcode);
        Alcotest.(check bool)
          "logical-not instruction identity" true
          (unary.instruction_id = instruction_id 8);
        Alcotest.(check bool)
          "logical-not links" true
          (unary.operands = [ value_id 11 ]
          && unary.result = Some { value_id = value_id 12 });
        Alcotest.(check bool)
          "forwarded result type" true
          (unary.target_type = literal.target_type);
        Alcotest.(check bool) "no folded payload" true (unary.payload = None);
        Alcotest.(check int64) "no instruction flags" 0L unary.flags;
        Alcotest.(check bool)
          "literal span" true
          (match literal.span with
          | Some span -> span.start = literal_start && span.stop = literal_stop
          | None -> false);
        Alcotest.(check bool)
          "operator span" true
          (match unary.span with
          | Some span ->
              span.start = operator_start && span.stop = operator_stop
          | None -> false)
    | instructions ->
        Alcotest.failf "expected two instructions, got %d"
          (List.length instructions)
  in
  check "I64 value=42;" "I64 value=!42;" (10, 11) (11, 13);
  check "I64 value='A';" "I64 value=!'A';" (10, 11) (11, 14);
  check "F64 value=1.25;" "F64 value=!1.25;" (10, 11) (11, 15);
  check "U8 *value=\"A\\n\";" "U8 *value=!\"A\\n\";" (10, 11) (11, 16)

let test_bitwise_not_parser_literals_emit_i64_instructions () =
  let check text (operator_start, operator_stop) (literal_start, literal_stop) =
    let lowered =
      parse_initializer text
      |> lower_parsed ~unary_identities:[ identity 8 12 ]
      |> require_lowered
    in
    match Literal.sequence lowered |> Sequence.instructions with
    | [ literal_instruction; unary_instruction ] ->
        let literal = Sequence.description literal_instruction in
        let unary = Sequence.description unary_instruction in
        let is_i64 = function
          | Some type_ -> (
              match (Type.base type_, Type.pointer_depth type_) with
              | Type.Primitive (Type.Internal_storage, Primitive.I64), 0 -> true
              | _ -> false)
          | None -> false
        in
        Alcotest.(check bool)
          "bitwise-complement opcode" true
          (Opcode.equal Opcode.Ic_com unary.opcode);
        Alcotest.(check bool)
          "bitwise-complement links" true
          (unary.operands = [ value_id 11 ]
          && unary.result = Some { value_id = value_id 12 });
        Alcotest.(check bool)
          "internal I64 instruction type" true (is_i64 unary.target_type);
        Alcotest.(check bool)
          "internal I64 final type" true
          (is_i64 (Some (Literal.result_type lowered)));
        Alcotest.(check bool) "no folded payload" true (unary.payload = None);
        Alcotest.(check bool)
          "source spans" true
          (match (literal.span, unary.span) with
          | Some literal_span, Some operator_span ->
              literal_span.start = literal_start
              && literal_span.stop = literal_stop
              && operator_span.start = operator_start
              && operator_span.stop = operator_stop
          | _ -> false)
    | instructions ->
        Alcotest.failf "expected two instructions, got %d"
          (List.length instructions)
  in
  check "I64 value=~42;" (10, 11) (11, 13);
  check "I64 value=~'A';" (10, 11) (11, 14);
  check "F64 value=~1.25;" (10, 11) (11, 15);
  check "U8 *value=~\"A\\n\";" (10, 11) (11, 16)

let test_dereference_parser_literals_emit_instructions () =
  let check text (operator_start, operator_stop) (literal_start, literal_stop)
      ~removes_pointer =
    let lowered =
      parse_initializer text
      |> lower_parsed ~unary_identities:[ identity 8 12 ]
      |> require_lowered
    in
    match Literal.sequence lowered |> Sequence.instructions with
    | [ literal_instruction; dereference_instruction ] ->
        let literal = Sequence.description literal_instruction in
        let dereference = Sequence.description dereference_instruction in
        Alcotest.(check bool)
          "dereference opcode" true
          (Opcode.equal Opcode.Ic_deref dereference.opcode);
        Alcotest.(check bool)
          "dereference links" true
          (dereference.operands = [ value_id 11 ]
          && dereference.result = Some { value_id = value_id 12 });
        Alcotest.(check bool)
          "dereference type" true
          (match (literal.target_type, dereference.target_type) with
          | Some operand_type, Some result_type ->
              Type.base operand_type = Type.base result_type
              && Type.pointer_depth result_type
                 = Type.pointer_depth operand_type
                   - if removes_pointer then 1 else 0
          | _ -> false);
        Alcotest.(check bool)
          "final result type" true
          (dereference.target_type = Some (Literal.result_type lowered));
        Alcotest.(check bool)
          "no folded payload" true
          (dereference.payload = None);
        Alcotest.(check int64) "no instruction flags" 0L dereference.flags;
        Alcotest.(check bool)
          "source spans" true
          (match (literal.span, dereference.span) with
          | Some literal_span, Some operator_span ->
              literal_span.start = literal_start
              && literal_span.stop = literal_stop
              && operator_span.start = operator_start
              && operator_span.stop = operator_stop
          | _ -> false)
    | instructions ->
        Alcotest.failf "expected two instructions, got %d"
          (List.length instructions)
  in
  check "I64 value=*42;" (10, 11) (11, 13) ~removes_pointer:false;
  check "I64 value=*'A';" (10, 11) (11, 14) ~removes_pointer:false;
  check "F64 value=*1.25;" (10, 11) (11, 15) ~removes_pointer:false;
  check "U8 value=*\"A\\n\";" (9, 10) (10, 15) ~removes_pointer:true

let test_address_of_parser_literals_emit_instructions () =
  let check text (operator_start, operator_stop) (literal_start, literal_stop) =
    let lowered =
      parse_initializer text
      |> lower_parsed ~unary_identities:[ identity 8 12 ]
      |> require_lowered
    in
    match Literal.sequence lowered |> Sequence.instructions with
    | [ literal_instruction; address_instruction ] ->
        let literal = Sequence.description literal_instruction in
        let address = Sequence.description address_instruction in
        Alcotest.(check bool)
          "address opcode" true
          (Opcode.equal Opcode.Ic_addr address.opcode);
        Alcotest.(check bool)
          "address links" true
          (address.operands = [ value_id 11 ]
          && address.result = Some { value_id = value_id 12 });
        Alcotest.(check bool)
          "address adds one pointer layer" true
          (match (literal.target_type, address.target_type) with
          | Some operand_type, Some result_type ->
              Type.base operand_type = Type.base result_type
              && Type.pointer_depth result_type
                 = Type.pointer_depth operand_type + 1
          | _ -> false);
        Alcotest.(check bool)
          "final result type" true
          (address.target_type = Some (Literal.result_type lowered));
        Alcotest.(check bool) "no folded payload" true (address.payload = None);
        Alcotest.(check int64) "no instruction flags" 0L address.flags;
        Alcotest.(check bool)
          "source spans" true
          (match (literal.span, address.span) with
          | Some literal_span, Some operator_span ->
              literal_span.start = literal_start
              && literal_span.stop = literal_stop
              && operator_span.start = operator_start
              && operator_span.stop = operator_stop
          | _ -> false)
    | instructions ->
        Alcotest.failf "expected two instructions, got %d"
          (List.length instructions)
  in
  check "I64 value=&42;" (10, 11) (11, 13);
  check "I64 value=&'A';" (10, 11) (11, 14);
  check "F64 value=&1.25;" (10, 11) (11, 15);
  check "U8 **value=&\"A\\n\";" (11, 12) (12, 17)

let test_address_of_cancels_immediate_dereference () =
  let check text expected_depth =
    let lowered =
      parse_initializer text
      |> lower_parsed ~unary_identities:[ identity 8 12 ]
      |> require_lowered
    in
    match Literal.sequence lowered |> Sequence.instructions with
    | [ literal_instruction; address_instruction ] ->
        let literal = Sequence.description literal_instruction in
        let address = Sequence.description address_instruction in
        Alcotest.(check bool)
          "only address remains" true
          (Opcode.equal Opcode.Ic_addr address.opcode);
        Alcotest.(check bool)
          "address consumes the literal" true
          (address.operands = [ value_id 11 ]);
        Alcotest.(check bool)
          "address owns the only unary identity" true
          (address.instruction_id = instruction_id 8
          && address.result = Some { value_id = value_id 12 });
        Alcotest.(check (option int))
          "canceled dereference span is absent" (Some 10)
          (Option.map (fun (span : Span.t) -> span.start) address.span);
        Alcotest.(check int)
          "source-selected pointer depth" expected_depth
          (Type.pointer_depth (Literal.result_type lowered));
        Alcotest.(check bool)
          "base type is retained" true
          (match literal.target_type with
          | Some operand_type ->
              Type.base operand_type = Type.base (Literal.result_type lowered)
          | None -> false)
    | instructions ->
        Alcotest.failf "expected literal and address instructions, got %d"
          (List.length instructions)
  in
  check "I64 value=&*42;" 1;
  check "U8 *value=&*\"A\";" 1;
  let lowered =
    parse_initializer "I64 value=*&42;"
    |> lower_parsed ~unary_identities:[ identity 8 12; identity 9 13 ]
    |> require_lowered
  in
  let opcodes =
    Literal.sequence lowered |> Sequence.instructions |> List.tl
    |> List.map (fun instruction -> (Sequence.description instruction).opcode)
  in
  Alcotest.(check bool)
    "dereference outside address is not canceled" true
    (match opcodes with
    | [ address; dereference ] ->
        Opcode.equal Opcode.Ic_addr address
        && Opcode.equal Opcode.Ic_deref dereference
    | _ -> false);
  Alcotest.(check int)
    "outer dereference restores the literal type" 0
    (Type.pointer_depth (Literal.result_type lowered))

let test_address_of_pointer_depth_failure_is_typed () =
  let expression = parse_initializer "U8 *value=&(&(&(&\"A\")));" in
  match
    lower_parsed_result
      ~unary_identities:
        [ identity 8 12; identity 9 13; identity 10 14; identity 11 15 ]
      expression
  with
  | Error [ (error : Sequence.error) ] ->
      Alcotest.(check string) "diagnostic code" "HCIRL0002" error.code;
      Alcotest.(check string)
        "diagnostic message"
        "cannot lower prefix address-of: semantic pointer depth 5 exceeds \
         HolyC's limit of 4"
        error.message;
      Alcotest.(check (option int))
        "failing instruction" (Some 11) error.instruction_id;
      Alcotest.(check bool) "operator span" true (Option.is_some error.span)
  | Error errors ->
      Alcotest.failf "expected one pointer-depth error, got %d"
        (List.length errors)
  | Ok _ -> Alcotest.fail "expected pointer-depth failure"

let test_nested_unary_minus_uses_inner_to_outer_order () =
  let source = Source_id.of_int 14 |> require_ok Fun.id in
  let literal_span = Span.unsafe_make ~source ~start:20 ~stop:22 in
  let inner_span = Span.unsafe_make ~source ~start:10 ~stop:11 in
  let outer_span = Span.unsafe_make ~source ~start:2 ~stop:3 in
  let literal =
    parsed_literal
      (fun value -> Ast.Integer_literal value)
      ~span:literal_span ~spelling:"42" (Ast.Integer_value 42L)
  in
  let expression =
    literal
    |> unary_plus ~location:(parsed_location literal_span)
    |> unary_minus ~location:(parsed_location inner_span)
    |> parenthesize ~location:(parsed_location inner_span)
    |> unary_plus ~location:(parsed_location outer_span)
    |> unary_minus ~location:(parsed_location outer_span)
  in
  let descriptions =
    lower_parsed ~unary_identities:[ identity 8 12; identity 9 13 ] expression
    |> require_lowered |> Literal.sequence |> Sequence.instructions
    |> List.map Sequence.description
  in
  match descriptions with
  | [ literal; inner; outer ] ->
      Alcotest.(check bool)
        "instruction IDs" true
        (List.map
           (fun (description : Sequence.description) ->
             Sequence.Instruction_id.to_int description.instruction_id)
           descriptions
        = [ 7; 8; 9 ]);
      Alcotest.(check bool)
        "value chain" true
        (inner.operands = [ value_id 11 ]
        && inner.result = Some { value_id = value_id 12 }
        && outer.operands = [ value_id 12 ]
        && outer.result = Some { value_id = value_id 13 });
      Alcotest.(check bool)
        "spans" true
        (literal.span = Some literal_span
        && inner.span = Some inner_span
        && outer.span = Some outer_span)
  | _ -> Alcotest.fail "expected a literal and two unary instructions"

let test_mixed_unary_operators_use_inner_to_outer_order () =
  let source = Source_id.of_int 16 |> require_ok Fun.id in
  let literal_span = Span.unsafe_make ~source ~start:20 ~stop:22 in
  let inner_span = Span.unsafe_make ~source ~start:12 ~stop:13 in
  let complement_span = Span.unsafe_make ~source ~start:9 ~stop:10 in
  let minus_span = Span.unsafe_make ~source ~start:6 ~stop:7 in
  let outer_span = Span.unsafe_make ~source ~start:2 ~stop:3 in
  let expression =
    parsed_literal
      (fun value -> Ast.Float_literal value)
      ~span:literal_span ~spelling:"1.5" (Ast.Float_value 1.5)
    |> logical_not ~location:(parsed_location inner_span)
    |> bitwise_not ~location:(parsed_location complement_span)
    |> unary_minus ~location:(parsed_location minus_span)
    |> logical_not ~location:(parsed_location outer_span)
  in
  let lowered =
    lower_parsed
      ~unary_identities:
        [ identity 8 12; identity 9 13; identity 10 14; identity 11 15 ]
      expression
    |> require_lowered
  in
  let descriptions =
    Literal.sequence lowered |> Sequence.instructions
    |> List.map Sequence.description
  in
  match descriptions with
  | [ literal; inner; complement; minus; outer ] ->
      Alcotest.(check bool)
        "opcode order" true
        (Opcode.equal Opcode.Ic_not inner.opcode
        && Opcode.equal Opcode.Ic_com complement.opcode
        && Opcode.equal Opcode.Ic_unary_minus minus.opcode
        && Opcode.equal Opcode.Ic_not outer.opcode);
      Alcotest.(check bool)
        "value chain" true
        (inner.operands = [ value_id 11 ]
        && inner.result = Some { value_id = value_id 12 }
        && complement.operands = [ value_id 12 ]
        && complement.result = Some { value_id = value_id 13 }
        && minus.operands = [ value_id 13 ]
        && minus.result = Some { value_id = value_id 14 }
        && outer.operands = [ value_id 14 ]
        && outer.result = Some { value_id = value_id 15 });
      Alcotest.(check bool)
        "type transitions" true
        (inner.target_type = literal.target_type
        && complement.target_type <> literal.target_type
        && minus.target_type = complement.target_type
        && outer.target_type = complement.target_type
        && Some (Literal.result_type lowered) = outer.target_type);
      Alcotest.(check bool)
        "operator spans" true
        (inner.span = Some inner_span
        && complement.span = Some complement_span
        && minus.span = Some minus_span
        && outer.span = Some outer_span)
  | _ -> Alcotest.fail "expected a literal and four unary instructions"

let test_mixed_dereference_chain_tracks_type_order () =
  let source = Source_id.of_int 17 |> require_ok Fun.id in
  let literal_span = Span.unsafe_make ~source ~start:20 ~stop:23 in
  let minus_span = Span.unsafe_make ~source ~start:12 ~stop:13 in
  let dereference_span = Span.unsafe_make ~source ~start:9 ~stop:10 in
  let complement_span = Span.unsafe_make ~source ~start:6 ~stop:7 in
  let outer_span = Span.unsafe_make ~source ~start:2 ~stop:3 in
  let expression =
    parsed_literal
      (fun value -> Ast.String_literal value)
      ~span:literal_span ~spelling:"\"A\"" (Ast.Bytes_value "A")
    |> unary_minus ~location:(parsed_location minus_span)
    |> dereference ~location:(parsed_location dereference_span)
    |> bitwise_not ~location:(parsed_location complement_span)
    |> logical_not ~location:(parsed_location outer_span)
  in
  let descriptions =
    lower_parsed
      ~unary_identities:
        [ identity 8 12; identity 9 13; identity 10 14; identity 11 15 ]
      expression
    |> require_lowered |> Literal.sequence |> Sequence.instructions
    |> List.map Sequence.description
  in
  match descriptions with
  | [ literal; minus; dereference; complement; outer ] ->
      Alcotest.(check bool)
        "opcode order" true
        (Opcode.equal Opcode.Ic_unary_minus minus.opcode
        && Opcode.equal Opcode.Ic_deref dereference.opcode
        && Opcode.equal Opcode.Ic_com complement.opcode
        && Opcode.equal Opcode.Ic_not outer.opcode);
      Alcotest.(check bool)
        "value chain" true
        (minus.operands = [ value_id 11 ]
        && minus.result = Some { value_id = value_id 12 }
        && dereference.operands = [ value_id 12 ]
        && dereference.result = Some { value_id = value_id 13 }
        && complement.operands = [ value_id 13 ]
        && complement.result = Some { value_id = value_id 14 }
        && outer.operands = [ value_id 14 ]
        && outer.result = Some { value_id = value_id 15 });
      Alcotest.(check bool)
        "type transitions" true
        (minus.target_type = literal.target_type
        && Option.map Type.pointer_depth literal.target_type = Some 1
        && Option.map Type.pointer_depth dereference.target_type = Some 0
        && complement.target_type <> dereference.target_type
        && complement.target_type = outer.target_type);
      Alcotest.(check bool)
        "operator spans" true
        (minus.span = Some minus_span
        && dereference.span = Some dereference_span
        && complement.span = Some complement_span
        && outer.span = Some outer_span)
  | _ -> Alcotest.fail "expected a literal and four unary instructions"

let test_unary_identity_count_is_checked () =
  let unary_expression = parse_initializer "I64 value=~42;" in
  let mixed_expression = parse_initializer "I64 value=*~!-42;" in
  let canceled_expression = parse_initializer "I64 value=&*42;" in
  let direct_expression = parse_initializer "I64 value=42;" in
  let check expected_message expected_span = function
    | Error [ (error : Sequence.error) ] ->
        Alcotest.(check string) "diagnostic code" "HCIRL0001" error.code;
        Alcotest.(check string)
          "diagnostic message" expected_message error.message;
        Alcotest.(check bool)
          "diagnostic span" true
          (error.span = Some expected_span);
        Alcotest.(check (option int))
          "no unchecked instruction" None error.instruction_id
    | Error errors ->
        Alcotest.failf "expected one identity error, got %d"
          (List.length errors)
    | Ok _ -> Alcotest.fail "expected an identity-count error"
  in
  lower_parsed_result unary_expression
  |> check "expected 1 unary instruction identity, got 0"
       (Ast.expression_location unary_expression).span;
  lower_parsed_result
    ~unary_identities:[ identity 8 12; identity 9 13; identity 10 14 ]
    mixed_expression
  |> check "expected 4 unary instruction identities, got 3"
       (Ast.expression_location mixed_expression).span;
  lower_parsed_result ~unary_identities:[ identity 8 12 ] direct_expression
  |> check "expected 0 unary instruction identities, got 1"
       (Ast.expression_location direct_expression).span;
  lower_parsed_result canceled_expression
  |> check "expected 1 unary instruction identity, got 0"
       (Ast.expression_location canceled_expression).span

let test_other_prefixes_and_supported_nonliteral_are_explicit () =
  let source = Source_id.of_int 12 |> require_ok Fun.id in
  let span = Span.unsafe_make ~source ~start:5 ~stop:10 in
  let location = parsed_location span in
  let identifier = Ast.make_identifier ~spelling:"value" ~location in
  let require_not_literal ?(unary_identities = []) label expression =
    match lower_parsed ~unary_identities expression with
    | Literal.Not_literal -> ()
    | Literal.Lowered _ -> Alcotest.failf "%s produced literal IR" label
  in
  Ast.Identifier_expression identifier |> unary_plus ~location
  |> require_not_literal "unary-plus nonliteral";
  Ast.Identifier_expression identifier |> unary_minus ~location
  |> require_not_literal
       ~unary_identities:[ identity 8 12 ]
       "unary-minus nonliteral";
  Ast.Identifier_expression identifier |> logical_not ~location
  |> require_not_literal
       ~unary_identities:[ identity 8 12 ]
       "logical-not nonliteral";
  Ast.Identifier_expression identifier |> bitwise_not ~location
  |> require_not_literal
       ~unary_identities:[ identity 8 12 ]
       "bitwise-complement nonliteral";
  Ast.Identifier_expression identifier |> dereference ~location
  |> require_not_literal
       ~unary_identities:[ identity 8 12 ]
       "dereference nonliteral";
  Ast.Identifier_expression identifier |> address_of ~location
  |> require_not_literal
       ~unary_identities:[ identity 8 12 ]
       "address-of nonliteral"

let test_deep_transparent_wrappers_use_constant_host_stack () =
  let source = Source_id.of_int 13 |> require_ok Fun.id in
  let literal_span = Span.unsafe_make ~source ~start:20 ~stop:22 in
  let wrapper_span = Span.unsafe_make ~source ~start:0 ~stop:42 in
  let wrapper_location = parsed_location wrapper_span in
  let expression =
    ref
      (parsed_literal
         (fun value -> Ast.Integer_literal value)
         ~span:literal_span ~spelling:"42" (Ast.Integer_value 42L))
  in
  for depth = 1 to 100_000 do
    let wrap =
      if depth mod 2 = 0 then parenthesize ~location:wrapper_location
      else unary_plus ~location:wrapper_location
    in
    expression := wrap !expression
  done;
  let lowered = lower_parsed !expression |> require_lowered in
  let description = only_description lowered in
  Alcotest.(check bool)
    "deep payload" true
    (description.payload = Some (Sequence.Integer 42L));
  Alcotest.(check bool)
    "inner literal span" true
    (description.span = Some literal_span)

let test_deep_mixed_unary_chain_uses_constant_host_stack () =
  let source = Source_id.of_int 15 |> require_ok Fun.id in
  let literal_span = Span.unsafe_make ~source ~start:20 ~stop:22 in
  let wrapper_span = Span.unsafe_make ~source ~start:0 ~stop:42 in
  let wrapper_location = parsed_location wrapper_span in
  let expression =
    ref
      (parsed_literal
         (fun value -> Ast.Integer_literal value)
         ~span:literal_span ~spelling:"42" (Ast.Integer_value 42L))
  in
  let unary_count = ref 0 in
  let reversed_identities = ref [] in
  for depth = 1 to 100_000 do
    if depth mod 100 = 0 then (
      incr unary_count;
      reversed_identities :=
        identity (1_000 + !unary_count) (100_000 + !unary_count)
        :: !reversed_identities;
      if !unary_count mod 5 = 0 then
        expression :=
          !expression
          |> dereference ~location:wrapper_location
          |> address_of ~location:wrapper_location
      else
        let wrap =
          match !unary_count mod 4 with
          | 0 -> dereference
          | 1 -> bitwise_not
          | 2 -> unary_minus
          | _ -> logical_not
        in
        expression := wrap ~location:wrapper_location !expression)
    else if depth mod 2 = 0 then
      expression := parenthesize ~location:wrapper_location !expression
    else expression := unary_plus ~location:wrapper_location !expression
  done;
  let lowered =
    lower_parsed ~unary_identities:(List.rev !reversed_identities) !expression
    |> require_lowered
  in
  Alcotest.(check int)
    "instruction count" (!unary_count + 1)
    (Literal.sequence lowered |> Sequence.length);
  let last =
    Literal.sequence lowered |> Sequence.instructions |> List.rev |> List.hd
    |> Sequence.description
  in
  Alcotest.(check bool)
    "outermost unary result" true
    (last.result = Some { value_id = value_id (100_000 + !unary_count) });
  Alcotest.(check bool)
    "outermost unary span" true
    (last.span = Some wrapper_span)

let semantic_span result =
  match Semantic_result.result_origin result with
  | Semantic_symbol.Source_location source -> source.span
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected a typed source location"

let semantic_literal_span result =
  let source = ref (Semantic_result.result_source result) in
  let unwrapping = ref true in
  while !unwrapping do
    match Semantic_source.argument_expression_kind !source with
    | Semantic_source.Parenthesized_expression inner -> source := inner
    | Semantic_source.Prefix_expression prefix
      when Semantic_source.prefix_operator prefix = Semantic_source.Unary_plus
      -> source := Semantic_source.prefix_operand prefix
    | Semantic_source.Integer_literal _
    | Semantic_source.Float_literal _
    | Semantic_source.Character_literal _
    | Semantic_source.String_literal _
    | Semantic_source.Prefix_expression _
    | Semantic_source.Postfix_expression _
    | Semantic_source.Postfix_cast_expression _
    | Semantic_source.Binary_expression _
    | Semantic_source.Index_expression _
    | Semantic_source.Member_access_expression _
    | Semantic_source.Bound_identifier_expression _
    | Semantic_source.Top_level_bound_identifier_expression _
    | Semantic_source.Unresolved_expression _ -> unwrapping := false
  done;
  match Semantic_source.argument_expression_origin !source with
  | Semantic_symbol.Source_location source -> source.span
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected a typed literal source location"

let typed_lowering_result ?(unary_identities = []) ~instruction ~value result =
  Literal.lower_typed_result
    ~instruction_id:(instruction_id instruction)
    ~value_id:(value_id value) ~unary_identities result

let lower_typed ?(unary_identities = []) ~instruction ~value result =
  typed_lowering_result ~unary_identities ~instruction ~value result
  |> require_ok (fun errors ->
      String.concat "; " (List.map show_sequence_error errors))

let typed_unary_minus_spans result =
  let source = ref (Semantic_result.result_source result) in
  let operators = ref [] in
  let unwrapping = ref true in
  while !unwrapping do
    match Semantic_source.argument_expression_kind !source with
    | Semantic_source.Parenthesized_expression inner -> source := inner
    | Semantic_source.Prefix_expression prefix
      when Semantic_source.prefix_operator prefix = Semantic_source.Unary_plus
      -> source := Semantic_source.prefix_operand prefix
    | Semantic_source.Prefix_expression prefix
      when Semantic_source.prefix_operator prefix = Semantic_source.Unary_minus
      -> (
        match Semantic_source.prefix_operator_origin prefix with
        | Semantic_symbol.Source_location location ->
            operators := location.span :: !operators;
            source := Semantic_source.prefix_operand prefix
        | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
            Alcotest.fail "expected a unary-minus operator source location")
    | Semantic_source.Integer_literal _
    | Semantic_source.Float_literal _
    | Semantic_source.Character_literal _
    | Semantic_source.String_literal _
    | Semantic_source.Prefix_expression _
    | Semantic_source.Postfix_expression _
    | Semantic_source.Postfix_cast_expression _
    | Semantic_source.Binary_expression _
    | Semantic_source.Index_expression _
    | Semantic_source.Member_access_expression _
    | Semantic_source.Bound_identifier_expression _
    | Semantic_source.Top_level_bound_identifier_expression _
    | Semantic_source.Unresolved_expression _ -> unwrapping := false
  done;
  let leaf_span =
    match Semantic_source.argument_expression_origin !source with
    | Semantic_symbol.Source_location location -> location.span
    | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
        Alcotest.fail "expected a typed literal source location"
  in
  (leaf_span, !operators)

let check_typed_unary_minus ~index ~expected_payload ~expected_kind result =
  let instruction = 800 + index in
  let value = 1000 + index in
  let leaf_span, operator_spans = typed_unary_minus_spans result in
  let minus_count = List.length operator_spans in
  Alcotest.(check bool)
    "unary minus is present" true (minus_count > 0);
  Alcotest.(check bool)
    "outer span is not the literal span" true
    (semantic_span result <> leaf_span);
  let unary_identities =
    List.init minus_count (fun offset ->
        identity (instruction + 1 + offset) (value + 1 + offset))
  in
  let lowered =
    lower_typed ~unary_identities ~instruction ~value result |> require_lowered
  in
  let descriptions =
    Literal.sequence lowered |> Sequence.instructions
    |> List.map Sequence.description
  in
  Alcotest.(check int)
    "literal plus unary instructions"
    (minus_count + 1)
    (List.length descriptions);
  (match descriptions with
  | (literal : Sequence.description) :: unaries ->
      Alcotest.(check int)
        "caller-owned literal instruction ID" instruction
        (Sequence.Instruction_id.to_int literal.instruction_id);
      Alcotest.(check int)
        "caller-owned literal value ID" value
        (match literal.result with
        | Some result -> Sequence.Value_id.to_int result.value_id
        | None -> Alcotest.fail "expected a typed literal result value");
      Alcotest.(check bool)
        "semantic payload" true
        (literal.payload = Some expected_payload);
      Alcotest.(check bool)
        "literal uses the leaf span" true
        (literal.span = Some leaf_span);
      Alcotest.(check bool)
        "literal type is the outer checked type" true
        (literal.target_type = Semantic_result.result_type result);
      List.iteri
        (fun offset (unary : Sequence.description) ->
          let operator_span = List.nth operator_spans offset in
          Alcotest.(check bool)
            "unary-minus opcode" true
            (Opcode.equal Opcode.Ic_unary_minus unary.opcode);
          Alcotest.(check int)
            "caller-owned unary instruction ID"
            (instruction + 1 + offset)
            (Sequence.Instruction_id.to_int unary.instruction_id);
          Alcotest.(check bool)
            "unary operand" true
            (unary.operands = [ value_id (value + offset) ]);
          Alcotest.(check bool)
            "unary result" true
            (unary.result
            = Some { value_id = value_id (value + 1 + offset) });
          Alcotest.(check bool)
            "forwarded result type" true
            (unary.target_type = literal.target_type);
          Alcotest.(check bool) "no folded payload" true (unary.payload = None);
          Alcotest.(check int64) "no instruction flags" 0L unary.flags;
          Alcotest.(check bool)
            "operator span" true
            (unary.span = Some operator_span);
          Alcotest.(check bool)
            "operator span is not the literal span" true
            (unary.span <> Some leaf_span))
        unaries
  | [] -> Alcotest.fail "expected a literal instruction");
  Alcotest.(check bool)
    "final result type" true
    (Some (Literal.result_type lowered) = Semantic_result.result_type result);
  let kind_line =
    Literal.human lowered |> String.split_on_char '\n' |> fun lines ->
    List.nth lines 1
  in
  Alcotest.(check bool)
    "semantic literal kind" true
    (String.starts_with ~prefix:("kind=" ^ expected_kind ^ " ") kind_line);
  let repeated =
    lower_typed ~unary_identities ~instruction ~value result |> require_lowered
  in
  Alcotest.(check string)
    "deterministic typed unary-minus lowering" (Literal.human lowered)
    (Literal.human repeated)

let check_typed_literal ?expected_span ~index ~expected_payload ~expected_kind
    result =
  let instruction = 700 + index in
  let value = 900 + index in
  let lowered = lower_typed ~instruction ~value result |> require_lowered in
  let description = only_description lowered in
  Alcotest.(check int)
    "caller-owned instruction ID" instruction
    (Sequence.Instruction_id.to_int description.instruction_id);
  Alcotest.(check int)
    "caller-owned value ID" value
    (match description.result with
    | Some result -> Sequence.Value_id.to_int result.value_id
    | None -> Alcotest.fail "expected a typed literal result value");
  Alcotest.(check bool)
    "semantic payload" true
    (description.payload = Some expected_payload);
  Alcotest.(check bool)
    "semantic result type" true
    (description.target_type = Semantic_result.result_type result);
  Alcotest.(check bool)
    "semantic source span" true
    (description.span
    = Some (Option.value ~default:(semantic_span result) expected_span));
  let kind_line =
    Literal.human lowered |> String.split_on_char '\n' |> fun lines ->
    List.nth lines 1
  in
  Alcotest.(check bool)
    "semantic literal kind" true
    (String.starts_with ~prefix:("kind=" ^ expected_kind ^ " ") kind_line);
  let repeated = lower_typed ~instruction ~value result |> require_lowered in
  Alcotest.(check string)
    "deterministic typed lowering" (Literal.human lowered)
    (Literal.human repeated)

let test_function_typed_literals_lower_without_ast_join () =
  let prepared =
    Test_function_call_expression_result.prepare
      ~path:"ir-typed-function-literals.HC"
      "extern I64 Target(I64 wrapped,I64 character,F64 floating,U8 *text);\n\
       I64 Caller(){return \
       Target(0xFFFFFFFFFFFFFFFF,'ABC',0.1,\"a\\n\\x42\\d\");}"
  in
  let _, results = Test_function_call_expression_result.analyze prepared in
  let roots =
    Test_function_call_expression_result.root_results results "Caller"
  in
  let expected =
    [
      (Sequence.Integer (-1L), "integer");
      (Sequence.Integer 0x434241L, "character");
      (Sequence.Float_bits (Int64.bits_of_float 0.1), "f64");
      (Sequence.Bytes "a\nB$", "string");
    ]
  in
  Alcotest.(check int) "typed function literals" 4 (List.length roots);
  List.iteri
    (fun index (result, (expected_payload, expected_kind)) ->
      check_typed_literal ~index ~expected_payload ~expected_kind result)
    (List.combine roots expected)

let test_top_level_typed_literals_lower_in_both_modes () =
  List.iter
    (fun mode ->
      let prepared =
        Test_top_level_expression_result.prepared ~mode
          ~path:"ir-typed-top-level-literals.HC"
          "0xFFFFFFFFFFFFFFFF;'ABC';0.1;\"a\\n\\x42\\d\";"
      in
      let _, _, _, results =
        Test_top_level_expression_result.analyze prepared
      in
      let roots = Test_top_level_expression_result.root_values results in
      let expected =
        [
          (Sequence.Integer (-1L), "integer");
          (Sequence.Integer 0x434241L, "character");
          (Sequence.Float_bits (Int64.bits_of_float 0.1), "f64");
          (Sequence.Bytes "a\nB$", "string");
        ]
      in
      Alcotest.(check int) "typed top-level literals" 4 (List.length roots);
      List.iteri
        (fun index (result, (expected_payload, expected_kind)) ->
          check_typed_literal ~index:(index + 10) ~expected_payload
            ~expected_kind result)
        (List.combine roots expected))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let check_grouped_typed_literals ~index roots =
  let expected =
    [
      (Sequence.Integer (-1L), "integer");
      (Sequence.Integer 0x434241L, "character");
      (Sequence.Float_bits (Int64.bits_of_float 0.1), "f64");
      (Sequence.Bytes "a\nB$", "string");
    ]
  in
  Alcotest.(check int) "grouped typed literals" 4 (List.length roots);
  List.iteri
    (fun offset (result, (expected_payload, expected_kind)) ->
      let expected_span = semantic_literal_span result in
      Alcotest.(check bool)
        "grouping has an outer source span" true
        (semantic_span result <> expected_span);
      check_typed_literal ~expected_span ~index:(index + offset)
        ~expected_payload ~expected_kind result)
    (List.combine roots expected)

let test_function_typed_grouping_is_transparent () =
  let prepared =
    Test_function_call_expression_result.prepare
      ~path:"ir-typed-function-grouping.HC"
      "extern I64 Target(I64 wrapped,I64 character,F64 floating,U8 *text);\n\
       I64 Caller(){return \
       Target((((0xFFFFFFFFFFFFFFFF))),((('ABC'))),(((0.1))),(((\"a\\n\\x42\\d\"))));}"
  in
  let _, results = Test_function_call_expression_result.analyze prepared in
  Test_function_call_expression_result.root_results results "Caller"
  |> check_grouped_typed_literals ~index:30

let test_top_level_typed_grouping_is_transparent_in_both_modes () =
  List.iter
    (fun mode ->
      let prepared =
        Test_top_level_expression_result.prepared ~mode
          ~path:"ir-typed-top-level-grouping.HC"
          "(((0xFFFFFFFFFFFFFFFF)));((('ABC')));(((0.1)));(((\"a\\n\\x42\\d\")));"
      in
      let _, _, _, results =
        Test_top_level_expression_result.analyze prepared
      in
      Test_top_level_expression_result.root_values results
      |> check_grouped_typed_literals ~index:40)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let test_typed_grouped_nonliteral_is_explicit () =
  let prepared =
    Test_function_call_expression_result.prepare
      ~path:"ir-typed-grouped-nonliteral.HC"
      "extern I64 Target(I64 value);\n\
       I64 Caller(I64 value){return Target(((value)));}"
  in
  let _, results = Test_function_call_expression_result.analyze prepared in
  let result =
    Test_function_call_expression_result.root_results results "Caller"
    |> List.hd
  in
  match lower_typed ~instruction:760 ~value:960 result with
  | Literal.Not_literal -> ()
  | Literal.Lowered _ -> Alcotest.fail "typed grouped nonliteral produced IR"

let test_function_typed_unary_plus_is_transparent () =
  let prepared =
    Test_function_call_expression_result.prepare
      ~path:"ir-typed-function-unary-plus.HC"
      "extern I64 Target(I64 wrapped,I64 character,F64 floating,U8 *text);\n\
       I64 Caller(){return \
       Target(+((+0xFFFFFFFFFFFFFFFF)),+((+'ABC')),+((+0.1)),+((+\"a\\n\\x42\\d\")));}"
  in
  let _, results = Test_function_call_expression_result.analyze prepared in
  Test_function_call_expression_result.root_results results "Caller"
  |> check_grouped_typed_literals ~index:50

let test_top_level_typed_unary_plus_is_transparent_in_both_modes () =
  List.iter
    (fun mode ->
      let prepared =
        Test_top_level_expression_result.prepared ~mode
          ~path:"ir-typed-top-level-unary-plus.HC"
          "+((+0xFFFFFFFFFFFFFFFF));+((+'ABC'));+((+0.1));+((+\"a\\n\\x42\\d\"));"
      in
      let _, _, _, results =
        Test_top_level_expression_result.analyze prepared
      in
      Test_top_level_expression_result.root_values results
      |> check_grouped_typed_literals ~index:60)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let test_typed_unary_plus_nonliteral_is_explicit () =
  let prepared =
    Test_function_call_expression_result.prepare
      ~path:"ir-typed-unary-plus-nonliteral.HC"
      "extern I64 Target(I64 value);\n\
       I64 Caller(I64 value){return Target(+((+value)));}"
  in
  let _, results = Test_function_call_expression_result.analyze prepared in
  let result =
    Test_function_call_expression_result.root_results results "Caller"
    |> List.hd
  in
  match lower_typed ~instruction:770 ~value:970 result with
  | Literal.Not_literal -> ()
  | Literal.Lowered _ -> Alcotest.fail "typed unary-plus nonliteral produced IR"

let check_typed_unary_minus_roots ~index roots =
  let expected =
    [
      (Sequence.Integer (-1L), "integer");
      (Sequence.Integer 0x434241L, "character");
      (Sequence.Float_bits (Int64.bits_of_float 0.1), "f64");
      (Sequence.Bytes "a\nB$", "string");
    ]
  in
  Alcotest.(check int) "unary-minus typed literals" 4 (List.length roots);
  List.iteri
    (fun offset (result, (expected_payload, expected_kind)) ->
      check_typed_unary_minus ~index:(index + offset) ~expected_payload
        ~expected_kind result)
    (List.combine roots expected)

let test_function_typed_unary_minus_emits_instructions () =
  let prepared =
    Test_function_call_expression_result.prepare
      ~path:"ir-typed-function-unary-minus.HC"
      "extern I64 Target(I64 wrapped,I64 character,F64 floating,U8 *text);\n\
       I64 Caller(){return \
       Target(-((+0xFFFFFFFFFFFFFFFF)),-((+'ABC')),-((+0.1)),-((+\"a\\n\\x42\\d\")));}"
  in
  let _, results = Test_function_call_expression_result.analyze prepared in
  Test_function_call_expression_result.root_results results "Caller"
  |> check_typed_unary_minus_roots ~index:0

let test_top_level_typed_unary_minus_in_both_modes () =
  List.iter
    (fun mode ->
      let prepared =
        Test_top_level_expression_result.prepared ~mode
          ~path:"ir-typed-top-level-unary-minus.HC"
          "-((+0xFFFFFFFFFFFFFFFF));-((+'ABC'));-((+0.1));-((+\"a\\n\\x42\\d\"));"
      in
      let _, _, _, results =
        Test_top_level_expression_result.analyze prepared
      in
      Test_top_level_expression_result.root_values results
      |> check_typed_unary_minus_roots ~index:10)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let test_typed_nested_unary_minus_uses_inner_to_outer_order () =
  let prepared =
    Test_function_call_expression_result.prepare
      ~path:"ir-typed-nested-unary-minus.HC"
      "extern I64 Target(I64 wrapped);\n\
       I64 Caller(){return Target(-(+(-((+0xFFFFFFFFFFFFFFFF)))));}"
  in
  let _, results = Test_function_call_expression_result.analyze prepared in
  let result =
    Test_function_call_expression_result.root_results results "Caller"
    |> List.hd
  in
  let _, operator_spans = typed_unary_minus_spans result in
  Alcotest.(check int) "nested unary minus count" 2 (List.length operator_spans);
  check_typed_unary_minus ~index:20 ~expected_payload:(Sequence.Integer (-1L))
    ~expected_kind:"integer" result

let test_typed_unary_minus_nonliteral_is_explicit () =
  let prepared =
    Test_function_call_expression_result.prepare
      ~path:"ir-typed-unary-minus-nonliteral.HC"
      "extern I64 Target(I64 value);\n\
       I64 Caller(I64 value){return Target(-((+value)));}"
  in
  let _, results = Test_function_call_expression_result.analyze prepared in
  let result =
    Test_function_call_expression_result.root_results results "Caller"
    |> List.hd
  in
  match
    lower_typed ~unary_identities:[ identity 791 991 ] ~instruction:790
      ~value:990 result
  with
  | Literal.Not_literal -> ()
  | Literal.Lowered _ -> Alcotest.fail "typed unary-minus nonliteral produced IR"

let test_typed_unary_minus_identity_count_is_checked () =
  let prepared =
    Test_function_call_expression_result.prepare
      ~path:"ir-typed-unary-minus-identities.HC"
      "extern I64 Target(I64 wrapped);\n\
       I64 Caller(){return Target(-((+0xFFFFFFFFFFFFFFFF)));}"
  in
  let _, results = Test_function_call_expression_result.analyze prepared in
  let result =
    Test_function_call_expression_result.root_results results "Caller"
    |> List.hd
  in
  match typed_lowering_result ~instruction:792 ~value:992 result with
  | Error [ error ] ->
      Alcotest.(check string) "identity diagnostic" "HCIRL0001" error.code
  | Error errors ->
      Alcotest.failf "expected one identity diagnostic, got %d"
        (List.length errors)
  | Ok Literal.Not_literal ->
      Alcotest.fail "missing identities returned Not_literal"
  | Ok (Literal.Lowered _) ->
      Alcotest.fail "missing identities produced IR"

let test_typed_literal_keeps_generated_source_location () =
  Test_function_call_expression_result.with_included_source
    "#define VALUE 0xFFFFFFFFFFFFFFFF\n\
     extern I64 Target(I64 value);\n\
     I64 Caller(){return Target(VALUE);}" (fun prepared ->
      let _, results = Test_function_call_expression_result.analyze prepared in
      let result =
        Test_function_call_expression_result.root_results results "Caller"
        |> List.hd
      in
      let source = semantic_span result in
      (match Semantic_result.result_origin result with
      | Semantic_symbol.Source_location location ->
          Alcotest.(check bool)
            "generated invocation origin" true
            (Option.is_some location.generated_from);
          Alcotest.(check bool)
            "definition origin" true
            (Option.is_some location.defined_at)
      | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
          Alcotest.fail "expected generated literal provenance");
      let lowered = lower_typed ~instruction:740 ~value:940 result in
      let description = lowered |> require_lowered |> only_description in
      Alcotest.(check bool)
        "generated semantic span" true
        (description.span = Some source))

let test_typed_nonliteral_is_explicit () =
  let prepared =
    Test_function_call_expression_result.prepare ~path:"ir-typed-nonliteral.HC"
      "extern I64 Target(I64 value);\n\
       I64 Caller(I64 value){return Target(value);}"
  in
  let _, results = Test_function_call_expression_result.analyze prepared in
  let result =
    Test_function_call_expression_result.root_results results "Caller"
    |> List.hd
  in
  match lower_typed ~instruction:750 ~value:950 result with
  | Literal.Not_literal -> ()
  | Literal.Lowered _ -> Alcotest.fail "typed nonliteral produced IR"

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
    Alcotest.test_case "grouped parser literals are transparent" `Quick
      test_grouped_parser_literals_are_transparent;
    Alcotest.test_case "grouped nonliteral is explicit" `Quick
      test_grouped_nonliteral_is_explicit;
    Alcotest.test_case "deep grouping uses constant host stack" `Quick
      test_deep_grouping_uses_constant_host_stack;
    Alcotest.test_case "unary-plus parser literals are transparent" `Quick
      test_unary_plus_parser_literals_are_transparent;
    Alcotest.test_case "unary-minus parser instructions" `Quick
      test_unary_minus_parser_literals_emit_instructions;
    Alcotest.test_case "logical-not parser instructions" `Quick
      test_logical_not_parser_literals_emit_instructions;
    Alcotest.test_case "bitwise-complement parser instructions" `Quick
      test_bitwise_not_parser_literals_emit_i64_instructions;
    Alcotest.test_case "dereference parser instructions" `Quick
      test_dereference_parser_literals_emit_instructions;
    Alcotest.test_case "address-of parser instructions" `Quick
      test_address_of_parser_literals_emit_instructions;
    Alcotest.test_case "address-of cancels dereference" `Quick
      test_address_of_cancels_immediate_dereference;
    Alcotest.test_case "address-of pointer-depth failure" `Quick
      test_address_of_pointer_depth_failure_is_typed;
    Alcotest.test_case "nested unary minus uses source order" `Quick
      test_nested_unary_minus_uses_inner_to_outer_order;
    Alcotest.test_case "mixed unary operators use source order" `Quick
      test_mixed_unary_operators_use_inner_to_outer_order;
    Alcotest.test_case "mixed dereference tracks type order" `Quick
      test_mixed_dereference_chain_tracks_type_order;
    Alcotest.test_case "unary identity count is checked" `Quick
      test_unary_identity_count_is_checked;
    Alcotest.test_case "other prefixes stay explicit" `Quick
      test_other_prefixes_and_supported_nonliteral_are_explicit;
    Alcotest.test_case "deep transparent wrappers" `Quick
      test_deep_transparent_wrappers_use_constant_host_stack;
    Alcotest.test_case "deep mixed unary chain" `Quick
      test_deep_mixed_unary_chain_uses_constant_host_stack;
    Alcotest.test_case "typed function literals" `Quick
      test_function_typed_literals_lower_without_ast_join;
    Alcotest.test_case "typed top-level literals" `Quick
      test_top_level_typed_literals_lower_in_both_modes;
    Alcotest.test_case "typed function grouping" `Quick
      test_function_typed_grouping_is_transparent;
    Alcotest.test_case "typed top-level grouping" `Quick
      test_top_level_typed_grouping_is_transparent_in_both_modes;
    Alcotest.test_case "typed grouped nonliteral is explicit" `Quick
      test_typed_grouped_nonliteral_is_explicit;
    Alcotest.test_case "typed function unary plus" `Quick
      test_function_typed_unary_plus_is_transparent;
    Alcotest.test_case "typed top-level unary plus" `Quick
      test_top_level_typed_unary_plus_is_transparent_in_both_modes;
    Alcotest.test_case "typed unary-plus nonliteral is explicit" `Quick
      test_typed_unary_plus_nonliteral_is_explicit;
    Alcotest.test_case "typed function unary minus" `Quick
      test_function_typed_unary_minus_emits_instructions;
    Alcotest.test_case "typed top-level unary minus" `Quick
      test_top_level_typed_unary_minus_in_both_modes;
    Alcotest.test_case "typed nested unary minus uses source order" `Quick
      test_typed_nested_unary_minus_uses_inner_to_outer_order;
    Alcotest.test_case "typed unary-minus nonliteral is explicit" `Quick
      test_typed_unary_minus_nonliteral_is_explicit;
    Alcotest.test_case "typed unary-minus identity count" `Quick
      test_typed_unary_minus_identity_count_is_checked;
    Alcotest.test_case "typed generated literal provenance" `Quick
      test_typed_literal_keeps_generated_source_location;
    Alcotest.test_case "typed nonliteral is explicit" `Quick
      test_typed_nonliteral_is_explicit;
  ]
