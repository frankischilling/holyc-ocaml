module Expression = Holyc_lib.Ir_expression_lowering
module Sequence = Holyc_lib.Ir_instruction_sequence
module Opcode = Holyc_lib.Ir_opcode
module Type = Holyc_lib.Semantic_type
module Primitive = Holyc_lib.Primitive_type
module Preprocessor = Holyc_lib.Preprocessor

let require_ok show = function
  | Ok value -> value
  | Error error -> Alcotest.fail (show error)

let show_sequence_error (error : Sequence.error) =
  error.code ^ ": " ^ error.message

let show_sequence_errors errors =
  String.concat "; " (List.map show_sequence_error errors)

let instruction_id value =
  Sequence.Instruction_id.of_int value |> require_ok show_sequence_error

let value_id value =
  Sequence.Value_id.of_int value |> require_ok show_sequence_error

let lower_result ?(instruction = 10) ?(value = 20) result =
  Expression.lower_typed_result
    ~instruction_id:(instruction_id instruction)
    ~value_id:(value_id value) result

let lower ?instruction ?value result =
  lower_result ?instruction ?value result |> require_ok show_sequence_errors

let require_lowered = function
  | Expression.Lowered lowered -> lowered
  | Expression.Unsupported_expression ->
      Alcotest.fail "expected a checked integer expression tree"

let descriptions lowered =
  lowered |> Expression.sequence |> Sequence.instructions
  |> List.map Sequence.description

let function_roots ~mode ~path source =
  let prepared =
    Test_function_call_expression_result.prepare ~mode ~path source
  in
  let _, results = Test_function_call_expression_result.analyze prepared in
  Test_function_call_expression_result.root_results results "Caller"

let top_level_roots ~mode ~path source =
  let prepared = Test_top_level_expression_result.prepared ~mode ~path source in
  let _, _, _, results = Test_top_level_expression_result.analyze prepared in
  Test_top_level_expression_result.root_values results

let opcode_names lowered =
  descriptions lowered
  |> List.map (fun (description : Sequence.description) ->
      Opcode.to_source_name description.opcode)

let internal_i64_type = function
  | Some type_ -> (
      match (Type.base type_, Type.pointer_depth type_) with
      | Type.Primitive (Type.Internal_storage, Primitive.I64), 0 -> true
      | _ -> false)
  | None -> false

let accepted_operators_lower_in_both_modes () =
  let expressions =
    [
      ("1<<2", "IC_SHL");
      ("1>>2", "IC_SHR");
      ("1*2", "IC_MUL");
      ("1/2", "IC_DIV");
      ("1%2", "IC_MOD");
      ("1&2", "IC_AND");
      ("1|2", "IC_OR");
      ("1^2", "IC_XOR");
      ("1+2", "IC_ADD");
      ("1-2", "IC_SUB");
      ("1==2", "IC_EQU_EQU");
      ("1!=2", "IC_NOT_EQU");
      ("1<2", "IC_LESS");
      ("1>=2", "IC_GREATER_EQU");
      ("1>2", "IC_GREATER");
      ("1<=2", "IC_LESS_EQU");
      ("1&&2", "IC_AND_AND");
      ("1||2", "IC_OR_OR");
      ("1^^2", "IC_XOR_XOR");
    ]
  in
  let parameters =
    List.mapi (fun index _ -> Printf.sprintf "I64 a%d" index) expressions
    |> String.concat ","
  in
  let arguments = expressions |> List.map fst |> String.concat "," in
  let source =
    Printf.sprintf "extern I64 Target(%s);I64 Caller(){return Target(%s);}"
      parameters arguments
  in
  List.iter
    (fun mode ->
      let roots =
        function_roots ~mode ~path:"ir-integer-binary-operators.HC" source
      in
      Alcotest.(check int)
        "one root per accepted operator" (List.length expressions)
        (List.length roots);
      List.iter2
        (fun root (_, expected_opcode) ->
          let lowered = lower root |> require_lowered in
          Alcotest.(check (list string))
            expected_opcode
            [ "IC_IMM_I64"; "IC_IMM_I64"; expected_opcode ]
            (opcode_names lowered))
        roots expressions)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let nested_precedence_keeps_source_order_and_consecutive_ids () =
  List.iter
    (fun mode ->
      let root =
        function_roots ~mode ~path:"ir-integer-binary-nesting.HC"
          "extern I64 Target(I64 value);I64 Caller(){return Target(1+2*3);}"
        |> List.hd
      in
      let lowered = lower root |> require_lowered in
      let items = descriptions lowered in
      Alcotest.(check (list string))
        "postorder opcodes"
        [ "IC_IMM_I64"; "IC_IMM_I64"; "IC_IMM_I64"; "IC_MUL"; "IC_ADD" ]
        (opcode_names lowered);
      Alcotest.(check (list int))
        "consecutive instruction IDs" [ 10; 11; 12; 13; 14 ]
        (List.map
           (fun (description : Sequence.description) ->
             Sequence.Instruction_id.to_int description.instruction_id)
           items);
      Alcotest.(check (list int))
        "consecutive value IDs" [ 20; 21; 22; 23; 24 ]
        (List.map
           (fun (description : Sequence.description) ->
             match description.result with
             | Some result -> Sequence.Value_id.to_int result.value_id
             | None -> Alcotest.fail "expected a produced expression value")
           items);
      let multiply = List.nth items 3 in
      let add = List.nth items 4 in
      Alcotest.(check (list int))
        "multiply operands" [ 21; 22 ]
        (List.map Sequence.Value_id.to_int multiply.operands);
      Alcotest.(check (list int))
        "add operands" [ 20; 23 ]
        (List.map Sequence.Value_id.to_int add.operands);
      let check_operator_span label expected_start
          (description : Sequence.description) =
        match description.span with
        | Some span ->
            Alcotest.(check int) (label ^ " start") expected_start span.start;
            Alcotest.(check int)
              (label ^ " stop") (expected_start + 1) span.stop
        | None -> Alcotest.fail (label ^ " lost its source span")
      in
      check_operator_span "multiply operator" 59 multiply;
      check_operator_span "add operator" 57 add;
      Alcotest.(check int)
        "root value" 24
        (Expression.result_value lowered |> Sequence.Value_id.to_int);
      Alcotest.(check int)
        "next instruction ID" 15
        (Expression.next_instruction_id lowered
        |> Sequence.Instruction_id.to_int);
      Alcotest.(check int)
        "next value ID" 25
        (Expression.next_value_id lowered |> Sequence.Value_id.to_int);
      Alcotest.(check bool)
        "integer result type" true
        (match
           ( Type.base (Expression.result_type lowered),
             Type.pointer_depth (Expression.result_type lowered) )
         with
        | Type.Primitive (Type.Internal_storage, Primitive.I64), 0 -> true
        | _ -> false))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let unary_operators_compose_at_each_binary_position () =
  let source =
    "extern I64 Target(I64 a,I64 b,I64 c);I64 Caller(){return \
     Target((-1)+2,1+(!2),~(1+2));}"
  in
  let cases =
    [
      ( [ "IC_IMM_I64"; "IC_UNARY_MINUS"; "IC_IMM_I64"; "IC_ADD" ],
        1,
        20,
        String.index source '-' );
      ( [ "IC_IMM_I64"; "IC_IMM_I64"; "IC_NOT"; "IC_ADD" ],
        2,
        21,
        String.index source '!' );
      ( [ "IC_IMM_I64"; "IC_IMM_I64"; "IC_ADD"; "IC_COM" ],
        3,
        22,
        String.index source '~' );
    ]
  in
  List.iter
    (fun mode ->
      let roots =
        function_roots ~mode ~path:"ir-integer-unary-trees.HC" source
      in
      Alcotest.(check int) "one root per unary position" 3 (List.length roots);
      List.iter2
        (fun root (expected_opcodes, unary_index, operand_value, span_start) ->
          let lowered = lower root |> require_lowered in
          let items = descriptions lowered in
          Alcotest.(check (list string))
            "unary and binary postorder" expected_opcodes (opcode_names lowered);
          let unary = List.nth items unary_index in
          Alcotest.(check (list int))
            "unary operand" [ operand_value ]
            (List.map Sequence.Value_id.to_int unary.operands);
          Alcotest.(check bool)
            "checked unary result type" true
            (internal_i64_type unary.target_type);
          (match unary.span with
          | Some span ->
              Alcotest.(check int) "unary span start" span_start span.start;
              Alcotest.(check int) "unary span stop" (span_start + 1) span.stop
          | None -> Alcotest.fail "unary instruction lost its operator span");
          Alcotest.(check int)
            "composed result value" 23
            (Expression.result_value lowered |> Sequence.Value_id.to_int);
          Alcotest.(check int)
            "composed next instruction" 14
            (Expression.next_instruction_id lowered
            |> Sequence.Instruction_id.to_int);
          Alcotest.(check int)
            "composed next value" 24
            (Expression.next_value_id lowered |> Sequence.Value_id.to_int))
        roots cases)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let top_level_characters_grouping_and_unary_plus_are_transparent () =
  List.iter
    (fun mode ->
      let root =
        top_level_roots ~mode ~path:"ir-top-level-integer-binary.HC"
          "(+('AB'))+((+2)<<1);"
        |> List.hd
      in
      let lowered = lower ~instruction:40 ~value:60 root |> require_lowered in
      Alcotest.(check (list string))
        "transparent wrappers emit no instructions"
        [ "IC_IMM_I64"; "IC_IMM_I64"; "IC_IMM_I64"; "IC_SHL"; "IC_ADD" ]
        (opcode_names lowered);
      let first = descriptions lowered |> List.hd in
      Alcotest.(check bool)
        "character payload" true
        (first.payload = Some (Sequence.Integer 0x4241L)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let top_level_mixed_unary_and_binary_tree_keeps_postorder () =
  let source = "-(('AB')+(+(!~2)));" in
  List.iter
    (fun mode ->
      let root =
        top_level_roots ~mode ~path:"ir-top-level-integer-unary.HC" source
        |> List.hd
      in
      let lowered = lower ~instruction:40 ~value:60 root |> require_lowered in
      let items = descriptions lowered in
      Alcotest.(check (list string))
        "mixed top-level postorder"
        [
          "IC_IMM_I64";
          "IC_IMM_I64";
          "IC_COM";
          "IC_NOT";
          "IC_ADD";
          "IC_UNARY_MINUS";
        ]
        (opcode_names lowered);
      let check_operand index expected =
        let description = List.nth items index in
        Alcotest.(check (list int))
          "mixed unary operand" [ expected ]
          (List.map Sequence.Value_id.to_int description.operands)
      in
      check_operand 2 61;
      check_operand 3 62;
      check_operand 5 64;
      Alcotest.(check (list int))
        "mixed add operands" [ 60; 63 ]
        ((List.nth items 4).operands |> List.map Sequence.Value_id.to_int);
      Alcotest.(check int)
        "mixed result value" 65
        (Expression.result_value lowered |> Sequence.Value_id.to_int);
      Alcotest.(check int)
        "mixed next instruction" 46
        (Expression.next_instruction_id lowered
        |> Sequence.Instruction_id.to_int);
      Alcotest.(check int)
        "mixed next value" 66
        (Expression.next_value_id lowered |> Sequence.Value_id.to_int))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let deterministic_dump_records_result_and_next_ids () =
  let root =
    function_roots ~mode:Preprocessor.Jit ~path:"ir-integer-binary-dump.HC"
      "extern I64 Target(I64 value);I64 Caller(){return Target((1)+(2));}"
    |> List.hd
  in
  let lowered = lower ~instruction:70 ~value:90 root |> require_lowered in
  let repeated = lower ~instruction:70 ~value:90 root |> require_lowered in
  let dump = Expression.human lowered in
  Alcotest.(check string)
    "deterministic replay" dump
    (Expression.human repeated);
  Alcotest.(check bool)
    "version and reference" true
    (String.starts_with
       ~prefix:
         "holyc-ir-expression-v1 \
          reference=c26482bb6ad3f80106d28504ec5db3c6a360732c\n"
       dump);
  let lines = String.split_on_char '\n' dump in
  Alcotest.(check string)
    "result and next identities"
    "result=%v92 result-type=internal:I64 next-instruction=73 next-value=93"
    (List.nth lines 1)

let unsupported_shapes_return_no_sequence () =
  let source =
    "extern I64 Helper();extern I64 Target(I64 a,I64 b,I64 c,I64 d,I64 e,I64 \
     f);I64 Caller(I64 x){return Target(1+2.0,1`2,x=1,x+1,*1+2,Helper()+1);}"
  in
  List.iter
    (fun mode ->
      let prepared =
        Test_function_call_expression_result.prepare ~mode
          ~path:"ir-unsupported-binary-shapes.HC" source
      in
      let _, results = Test_function_call_expression_result.analyze prepared in
      let roots =
        Test_function_call_expression_result.direct_named results "Caller"
          "Target"
        |> Test_function_call_expression_result.provided_results
      in
      Alcotest.(check int) "unsupported root count" 6 (List.length roots);
      List.iter
        (fun root ->
          match lower root with
          | Expression.Unsupported_expression -> ()
          | Expression.Lowered _ ->
              Alcotest.fail "unsupported expression returned a partial sequence")
        roots)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let exhausted_starting_ids_are_rejected () =
  let root =
    function_roots ~mode:Preprocessor.Jit ~path:"ir-binary-id-exhaustion.HC"
      "extern I64 Target(I64 value);I64 Caller(){return Target(1+2);}"
    |> List.hd
  in
  let check result =
    match result with
    | Error [ (error : Sequence.error) ] ->
        Alcotest.(check string) "diagnostic code" "HCIRL0005" error.code;
        Alcotest.(check string)
          "diagnostic message"
          "cannot allocate another expression identity because the host \
           integer range is exhausted"
          error.message
    | Error errors ->
        Alcotest.failf "expected one identity error, got %d"
          (List.length errors)
    | Ok _ -> Alcotest.fail "expected identity exhaustion"
  in
  check (lower_result ~instruction:Int.max_int root);
  check (lower_result ~value:Int.max_int root)

let deep_mixed_tree_uses_the_explicit_worklist () =
  let leaf_count = 2_000 in
  let expression =
    List.init leaf_count (fun index ->
        match index mod 3 with
        | 0 -> "-1"
        | 1 -> "!1"
        | _ -> "~1")
    |> String.concat "+"
  in
  let source =
    Printf.sprintf
      "extern I64 Target(I64 value);I64 Caller(){return Target(%s);}" expression
  in
  let root =
    function_roots ~mode:Preprocessor.Jit ~path:"ir-deep-integer-binary.HC"
      source
    |> List.hd
  in
  let lowered = lower ~instruction:100 ~value:1_000 root |> require_lowered in
  let instruction_count = (leaf_count * 3) - 1 in
  Alcotest.(check int)
    "one instruction per literal, unary, and binary node" instruction_count
    (Expression.sequence lowered |> Sequence.length);
  Alcotest.(check int)
    "next instruction ID" (100 + instruction_count)
    (Expression.next_instruction_id lowered |> Sequence.Instruction_id.to_int);
  Alcotest.(check int)
    "next value ID"
    (1_000 + instruction_count)
    (Expression.next_value_id lowered |> Sequence.Value_id.to_int)

let tests =
  [
    Alcotest.test_case "accepted integer binary operators" `Quick
      accepted_operators_lower_in_both_modes;
    Alcotest.test_case "nested precedence and IDs" `Quick
      nested_precedence_keeps_source_order_and_consecutive_ids;
    Alcotest.test_case "unary operators inside binary positions" `Quick
      unary_operators_compose_at_each_binary_position;
    Alcotest.test_case "top-level transparent integer leaves" `Quick
      top_level_characters_grouping_and_unary_plus_are_transparent;
    Alcotest.test_case "top-level mixed unary and binary tree" `Quick
      top_level_mixed_unary_and_binary_tree_keeps_postorder;
    Alcotest.test_case "deterministic expression dump" `Quick
      deterministic_dump_records_result_and_next_ids;
    Alcotest.test_case "unsupported expression shapes" `Quick
      unsupported_shapes_return_no_sequence;
    Alcotest.test_case "identity exhaustion" `Quick
      exhausted_starting_ids_are_rejected;
    Alcotest.test_case "deep explicit worklist" `Slow
      deep_mixed_tree_uses_the_explicit_worklist;
  ]
