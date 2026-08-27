module Expression = Holyc_lib.Ir_expression_lowering
module Sequence = Holyc_lib.Ir_instruction_sequence
module Opcode = Holyc_lib.Ir_opcode
module Graph = Holyc_lib.Ir_block_graph
module X87 = Holyc_lib.Ir_x87_stack
module Type = Holyc_lib.Semantic_type
module Primitive = Holyc_lib.Primitive_type
module Preprocessor = Holyc_lib.Preprocessor
module Semantic_result = Holyc_lib.Semantic_function_call_expression_result
module Semantic_symbol = Holyc_lib.Semantic_symbol

let require_ok show = function
  | Ok value -> value
  | Error error -> Alcotest.fail (show error)

let show_sequence_error (error : Sequence.error) =
  error.code ^ ": " ^ error.message

let show_sequence_errors errors =
  String.concat "; " (List.map show_sequence_error errors)

let show_graph_error (error : Graph.error) = error.code ^ ": " ^ error.message
let show_x87_error (error : X87.error) = error.code ^ ": " ^ error.message

let instruction_id value =
  Sequence.Instruction_id.of_int value |> require_ok show_sequence_error

let value_id value =
  Sequence.Value_id.of_int value |> require_ok show_sequence_error

let block_id value =
  Sequence.Block_id.of_int value |> require_ok show_sequence_error

let lower_result ?(instruction = 10) ?(value = 20) result =
  Expression.lower_typed_result
    ~instruction_id:(instruction_id instruction)
    ~value_id:(value_id value) result

let lower ?instruction ?value result =
  lower_result ?instruction ?value result |> require_ok show_sequence_errors

let require_lowered = function
  | Expression.Lowered lowered -> lowered
  | Expression.Unsupported_expression ->
      Alcotest.fail "expected a checked numeric expression tree"

let descriptions lowered =
  lowered |> Expression.sequence |> Sequence.instructions
  |> List.map Sequence.description

let function_roots ~mode ~path source =
  let prepared =
    Test_function_call_expression_result.prepare ~mode ~path source
  in
  let _, results = Test_function_call_expression_result.analyze prepared in
  Test_function_call_expression_result.root_results results "Caller"

let function_roots_with_outer ~mode ~path ~records source =
  let prepared =
    Test_function_call_conversion_policy.prepare ~mode ~path source
  in
  let table = Holyc_lib.Session.semantic_symbols prepared.session in
  let semantic_kind = function
    | Holyc_lib.Semantic_outer_environment.Aggregate ->
        Holyc_lib.Semantic_symbol.Aggregate_type
    | Holyc_lib.Semantic_outer_environment.Function ->
        Holyc_lib.Semantic_symbol.Function
    | Holyc_lib.Semantic_outer_environment.Global_variable ->
        Holyc_lib.Semantic_symbol.Global_variable
    | Holyc_lib.Semantic_outer_environment.Export_system_symbol ->
        Holyc_lib.Semantic_symbol.Assembler_symbol
  in
  let entries =
    records
    |> List.mapi (fun entry_index (name, record_kind) ->
        let symbol =
          Holyc_lib.Semantic_symbol_table.add table
            ~scope:(Holyc_lib.Semantic_symbol_table.root table)
            ~name
            ~kind:(semantic_kind record_kind)
            ~origin:
              (Holyc_lib.Semantic_symbol.Synthesized ("IR outer fixture " ^ name))
          |> require_ok Fun.id
        in
        Holyc_lib.Semantic_outer_environment.make_entry ~symbol ~record_kind
          ~entry_index
        |> require_ok Holyc_lib.Semantic_outer_environment.error_to_string)
  in
  let table_kind =
    match mode with
    | Preprocessor.Jit -> Holyc_lib.Semantic_outer_environment.Jit_task 0
    | Preprocessor.Aot -> Holyc_lib.Semantic_outer_environment.Aot_parent 0
  in
  let outer_table =
    Holyc_lib.Semantic_outer_environment.make_table ~table_kind ~table_index:0
      entries
    |> require_ok Holyc_lib.Semantic_outer_environment.error_to_string
  in
  let assembler =
    Holyc_lib.Semantic_outer_environment.make_table
      ~table_kind:Holyc_lib.Semantic_outer_environment.Assembler ~table_index:1
      []
    |> require_ok Holyc_lib.Semantic_outer_environment.error_to_string
  in
  let environment =
    Holyc_lib.create_outer_environment prepared.session ~compilation_mode:mode
      [ outer_table; assembler ]
    |> require_ok Fun.id
  in
  let outer =
    Holyc_lib.resolve_outer_expressions prepared.session ~environment
      ~expressions:prepared.module_expressions
    |> require_ok Fun.id
  in
  let calls =
    Holyc_lib.resolve_function_calls prepared.session
      ~declarations:prepared.declarations ~members:prepared.members
      ~function_types:prepared.function_types ~local_types:prepared.local_types
      ~global_types:prepared.global_types ~functions:prepared.functions
      ~expressions:prepared.module_expressions ~outer prepared.ast
    |> require_ok Fun.id
  in
  let policies =
    Holyc_lib.analyze_function_call_conversions prepared.session
      ~declarations:prepared.declarations ~headers:prepared.headers ~calls
    |> require_ok
         Holyc_lib.Semantic_function_call_conversion_policy.error_to_string
  in
  let results =
    Holyc_lib.type_function_call_expressions prepared.session
      ~members:prepared.members ~policies
    |> require_ok
         Holyc_lib.Semantic_function_call_expression_result.error_to_string
  in
  Test_function_call_expression_result.root_results results "Caller"

let top_level_roots ~mode ~path source =
  let prepared = Test_top_level_expression_result.prepared ~mode ~path source in
  let _, _, _, results = Test_top_level_expression_result.analyze prepared in
  Test_top_level_expression_result.root_values results

let top_level_roots_with_outer ~mode ~path ~names source =
  let prepared = Test_top_level_expression_result.prepared ~mode ~path source in
  let entries =
    names
    |> List.mapi (fun entry_index name ->
        Test_top_level_expression_result.make_outer_entry prepared ~entry_index
          ~name ())
  in
  let environment =
    Test_top_level_expression_result.outer_environment prepared entries
  in
  let _, _, _, results =
    Test_top_level_expression_result.analyze ~environment prepared
  in
  Test_top_level_expression_result.root_values results

let opcode_names lowered =
  descriptions lowered
  |> List.map (fun (description : Sequence.description) ->
      Opcode.to_source_name description.opcode)

let instruction_flags lowered =
  descriptions lowered
  |> List.map (fun (description : Sequence.description) -> description.flags)

let result_to_f64 = 0x000000001L
let use_f64 = 0x000000040L

let verify_x87 lowered =
  let terminal : Sequence.description =
    {
      instruction_id = Expression.next_instruction_id lowered;
      opcode = Opcode.Ic_end;
      operands = [];
      result = None;
      target_type = None;
      payload = None;
      flags = 0L;
      span = None;
    }
  in
  let graph =
    Graph.create ~entry:(block_id 0)
      [
        {
          Graph.block_id = block_id 0;
          instructions = descriptions lowered @ [ terminal ];
        };
      ]
    |> require_ok (fun errors ->
        String.concat "; " (List.map show_graph_error errors))
  in
  X87.verify graph
  |> require_ok (fun errors ->
      String.concat "; " (List.map show_x87_error errors))

let contains_substring text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec search index =
    if index + fragment_length > text_length then false
    else if String.sub text index fragment_length = fragment then true
    else search (index + 1)
  in
  fragment_length = 0 || search 0

let internal_i64_type = function
  | Some type_ -> (
      match (Type.base type_, Type.pointer_depth type_) with
      | Type.Primitive (Type.Internal_storage, Primitive.I64), 0 -> true
      | _ -> false)
  | None -> false

let internal_f64_type = function
  | Some type_ -> (
      match (Type.base type_, Type.pointer_depth type_) with
      | Type.Primitive (Type.Internal_storage, Primitive.F64), 0 -> true
      | _ -> false)
  | None -> false

let public_primitive_type expected = function
  | Some type_ -> (
      match (Type.base type_, Type.pointer_depth type_) with
      | Type.Primitive (Type.Public_spelling, primitive), 0 ->
          Primitive.equal expected primitive
      | _ -> false)
  | None -> false

let internal_i64_value_type type_ =
  match (Type.base type_, Type.pointer_depth type_) with
  | Type.Primitive (Type.Internal_storage, Primitive.I64), 0 -> true
  | _ -> false

let source_span result =
  match Semantic_result.result_origin result with
  | Semantic_symbol.Source_location location -> location.span
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected an expression backed by source text"

let pointer_depth expected = function
  | Some type_ -> Type.pointer_depth type_ = expected
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

let f64_arithmetic_operators_lower_in_both_modes () =
  let expressions =
    [
      ("1.0*2.0", "IC_MUL");
      ("1.0/2.0", "IC_DIV");
      ("1.0+2.0", "IC_ADD");
      ("1.0-2.0", "IC_SUB");
    ]
  in
  let parameters =
    List.mapi (fun index _ -> Printf.sprintf "F64 a%d" index) expressions
    |> String.concat ","
  in
  let arguments = expressions |> List.map fst |> String.concat "," in
  let source =
    Printf.sprintf "extern F64 Target(%s);F64 Caller(){return Target(%s);}"
      parameters arguments
  in
  List.iter
    (fun mode ->
      let roots =
        function_roots ~mode ~path:"ir-f64-arithmetic-operators.HC" source
      in
      Alcotest.(check int)
        "one root per floating operator" (List.length expressions)
        (List.length roots);
      List.iter2
        (fun root (_, expected_opcode) ->
          let lowered = lower root |> require_lowered in
          Alcotest.(check (list string))
            expected_opcode
            [ "IC_IMM_F64"; "IC_IMM_F64"; expected_opcode ]
            (opcode_names lowered);
          Alcotest.(check bool)
            "floating result type" true
            (internal_f64_type (Some (Expression.result_type lowered)));
          Alcotest.(check bool)
            "first literal keeps exact bits" true
            ((descriptions lowered |> List.hd).payload
            = Some (Sequence.Float_bits (Int64.bits_of_float 1.0))))
        roots expressions)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let f64_comparison_operators_lower_in_both_modes () =
  let expressions =
    [
      ("1.0==2.0", "==", "IC_EQU_EQU");
      ("1.0!=2.0", "!=", "IC_NOT_EQU");
      ("1.0<2.0", "<", "IC_LESS");
      ("1.0>=2.0", ">=", "IC_GREATER_EQU");
      ("1.0>2.0", ">", "IC_GREATER");
      ("1.0<=2.0", "<=", "IC_LESS_EQU");
    ]
  in
  let parameters =
    List.mapi (fun index _ -> Printf.sprintf "I64 a%d" index) expressions
    |> String.concat ","
  in
  let arguments =
    expressions |> List.map (fun (source, _, _) -> source) |> String.concat ","
  in
  let source =
    Printf.sprintf "extern I64 Target(%s);I64 Caller(){return Target(%s);}"
      parameters arguments
  in
  List.iter
    (fun mode ->
      let roots =
        function_roots ~mode ~path:"ir-f64-comparison-operators.HC" source
      in
      Alcotest.(check int)
        "one root per floating comparison" (List.length expressions)
        (List.length roots);
      List.iter2
        (fun root (_, operator, expected_opcode) ->
          let lowered = lower root |> require_lowered in
          let items = descriptions lowered in
          Alcotest.(check (list string))
            expected_opcode
            [ "IC_IMM_F64"; "IC_IMM_F64"; expected_opcode ]
            (opcode_names lowered);
          Alcotest.(check (list int64))
            "floating comparison flags" [ 0L; 0L; use_f64 ]
            (instruction_flags lowered);
          let comparison = List.nth items 2 in
          Alcotest.(check (list int))
            "comparison operands" [ 20; 21 ]
            (List.map Sequence.Value_id.to_int comparison.operands);
          Alcotest.(check bool)
            "comparison result is internal I64" true
            (internal_i64_type comparison.target_type);
          (match comparison.span with
          | Some span ->
              Alcotest.(check string)
                "comparison operator span" operator
                (String.sub source span.start (span.stop - span.start))
          | None -> Alcotest.fail "floating comparison lost its operator span");
          ignore (verify_x87 lowered))
        roots expressions)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let mixed_f64_comparisons_mark_integer_subtree_roots () =
  let source =
    "extern I64 Target(I64 left,I64 right,I64 nested);I64 Caller(){return \
     Target(1<2.0,1.0<2,(1+2)<3.0);}"
  in
  let expected =
    [
      ( [ "IC_IMM_I64"; "IC_IMM_F64"; "IC_LESS" ],
        [ result_to_f64; 0L; use_f64 ],
        0 );
      ( [ "IC_IMM_F64"; "IC_IMM_I64"; "IC_LESS" ],
        [ 0L; result_to_f64; use_f64 ],
        1 );
      ( [ "IC_IMM_I64"; "IC_IMM_I64"; "IC_ADD"; "IC_IMM_F64"; "IC_LESS" ],
        [ 0L; 0L; result_to_f64; 0L; use_f64 ],
        2 );
    ]
  in
  List.iter
    (fun mode ->
      let roots =
        function_roots ~mode ~path:"ir-mixed-f64-comparisons.HC" source
      in
      Alcotest.(check int) "three mixed comparisons" 3 (List.length roots);
      List.iter2
        (fun root (expected_opcodes, expected_flags, converted_index) ->
          let lowered = lower root |> require_lowered in
          let items = descriptions lowered in
          Alcotest.(check (list string))
            "mixed comparison postorder" expected_opcodes (opcode_names lowered);
          Alcotest.(check (list int64))
            "mixed comparison flags" expected_flags
            (instruction_flags lowered);
          Alcotest.(check bool)
            "converted producer remains integer" true
            (internal_i64_type (List.nth items converted_index).target_type);
          Alcotest.(check bool)
            "mixed comparison result is integer" true
            (internal_i64_type (List.hd (List.rev items)).target_type);
          ignore (verify_x87 lowered))
        roots expected)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let f64_comparison_flags_compose_with_a_parent_conversion () =
  let root =
    function_roots ~mode:Preprocessor.Jit
      ~path:"ir-f64-comparison-parent-conversion.HC"
      "extern F64 Target(F64 value);F64 Caller(){return Target((1.0<2.0)+3.0);}"
    |> List.hd
  in
  let lowered = lower root |> require_lowered in
  Alcotest.(check (list string))
    "comparison feeds floating arithmetic"
    [ "IC_IMM_F64"; "IC_IMM_F64"; "IC_LESS"; "IC_IMM_F64"; "IC_ADD" ]
    (opcode_names lowered);
  Alcotest.(check (list int64))
    "comparison keeps execution and result conversion flags"
    [ 0L; 0L; Int64.logor use_f64 result_to_f64; 0L; 0L ]
    (instruction_flags lowered);
  ignore (verify_x87 lowered)

let deterministic_f64_comparison_dump_records_execution_domain () =
  let root =
    function_roots ~mode:Preprocessor.Jit ~path:"ir-f64-comparison-dump.HC"
      "extern I64 Target(I64 value);I64 Caller(){return Target(1<2.0);}"
    |> List.hd
  in
  let lowered = lower ~instruction:70 ~value:90 root |> require_lowered in
  let repeated = lower ~instruction:70 ~value:90 root |> require_lowered in
  let dump = Expression.human lowered in
  Alcotest.(check string)
    "deterministic comparison replay" dump
    (Expression.human repeated);
  Alcotest.(check bool)
    "dump records conversion on the integer producer" true
    (contains_substring dump "IC_IMM_I64 i64:1 flags=0x000000001");
  Alcotest.(check bool)
    "dump records the F64 comparison path" true
    (contains_substring dump "IC_LESS %v90 %v91 flags=0x000000040");
  Alcotest.(check string)
    "comparison result and next identities"
    "result=%v92 result-type=internal:I64 next-instruction=73 next-value=93"
    (List.nth (String.split_on_char '\n' dump) 1)

let top_level_f64_comparisons_use_the_same_lowering () =
  List.iter
    (fun mode ->
      let root =
        top_level_roots ~mode ~path:"ir-top-level-f64-comparison.HC" "1.0<=2;"
        |> List.hd
      in
      let lowered = lower ~instruction:40 ~value:60 root |> require_lowered in
      Alcotest.(check (list string))
        "top-level comparison"
        [ "IC_IMM_F64"; "IC_IMM_I64"; "IC_LESS_EQU" ]
        (opcode_names lowered);
      Alcotest.(check (list int64))
        "top-level comparison flags"
        [ 0L; result_to_f64; use_f64 ]
        (instruction_flags lowered);
      Alcotest.(check int)
        "top-level next instruction" 43
        (Expression.next_instruction_id lowered
        |> Sequence.Instruction_id.to_int);
      Alcotest.(check int)
        "top-level next value" 63
        (Expression.next_value_id lowered |> Sequence.Value_id.to_int);
      ignore (verify_x87 lowered))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let deep_mixed_f64_comparison_uses_the_explicit_worklist () =
  let leaf_count = 2_000 in
  let integer_tree = List.init leaf_count (fun _ -> "1") |> String.concat "+" in
  let source =
    Printf.sprintf
      "extern I64 Target(I64 value);I64 Caller(){return Target((%s)<0.5);}"
      integer_tree
  in
  let root =
    function_roots ~mode:Preprocessor.Jit ~path:"ir-deep-f64-comparison.HC"
      source
    |> List.hd
  in
  let lowered = lower ~instruction:100 ~value:1_000 root |> require_lowered in
  let flags = instruction_flags lowered in
  let instruction_count = (leaf_count * 2) + 1 in
  Alcotest.(check int)
    "deep comparison instruction count" instruction_count
    (Expression.sequence lowered |> Sequence.length);
  Alcotest.(check int64)
    "deep integer root converts" result_to_f64
    (List.nth flags ((leaf_count * 2) - 2));
  Alcotest.(check int64)
    "deep comparison uses F64" use_f64
    (List.hd (List.rev flags));
  Alcotest.(check int)
    "deep comparison next instruction" (100 + instruction_count)
    (Expression.next_instruction_id lowered |> Sequence.Instruction_id.to_int);
  Alcotest.(check int)
    "deep comparison next value"
    (1_000 + instruction_count)
    (Expression.next_value_id lowered |> Sequence.Value_id.to_int)

let power_operand_domains_lower_in_both_modes () =
  let cases =
    [
      ("2.0`3.0", [ 0L; 0L; 0L ]);
      ("2`3.0", [ result_to_f64; 0L; 0L ]);
      ("2.0`3", [ 0L; result_to_f64; 0L ]);
      ("2`3", [ result_to_f64; result_to_f64; 0L ]);
    ]
  in
  let parameters =
    List.mapi (fun index _ -> Printf.sprintf "F64 a%d" index) cases
    |> String.concat ","
  in
  let arguments = cases |> List.map fst |> String.concat "," in
  let source =
    Printf.sprintf "extern F64 Target(%s);F64 Caller(){return Target(%s);}"
      parameters arguments
  in
  List.iter
    (fun mode ->
      let roots = function_roots ~mode ~path:"ir-power-domains.HC" source in
      Alcotest.(check int) "four power domains" 4 (List.length roots);
      List.iter2
        (fun root (_, expected_flags) ->
          let lowered = lower root |> require_lowered in
          let items = descriptions lowered in
          let expected_opcodes =
            List.map2
              (fun flag opcode ->
                if Int64.equal flag result_to_f64 && opcode = "IC_IMM_F64" then
                  "IC_IMM_I64"
                else opcode)
              expected_flags
              [ "IC_IMM_F64"; "IC_IMM_F64"; "IC_POWER" ]
          in
          Alcotest.(check (list string))
            "power operand forms" expected_opcodes (opcode_names lowered);
          Alcotest.(check (list int64))
            "power conversion flags" expected_flags
            (instruction_flags lowered);
          let power = List.nth items 2 in
          Alcotest.(check (list int))
            "power operands" [ 20; 21 ]
            (List.map Sequence.Value_id.to_int power.operands);
          Alcotest.(check bool)
            "power result is internal F64" true
            (internal_f64_type power.target_type);
          Alcotest.(check int64)
            "power carries no operation flag" 0L power.flags;
          (match power.span with
          | Some span ->
              Alcotest.(check string)
                "power operator span" "`"
                (String.sub source span.start (span.stop - span.start))
          | None -> Alcotest.fail "power lost its operator span");
          ignore (verify_x87 lowered))
        roots cases)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let right_associated_power_and_unary_minus_keep_checked_order () =
  let source =
    "extern F64 Target(F64 chain,F64 negative);F64 Caller(){return \
     Target(2`3`4,-2`3);}"
  in
  List.iter
    (fun mode ->
      let roots = function_roots ~mode ~path:"ir-power-order.HC" source in
      Alcotest.(check int) "two power roots" 2 (List.length roots);
      let chain = List.nth roots 0 |> lower |> require_lowered in
      let chain_items = descriptions chain in
      Alcotest.(check (list string))
        "right-associated postorder"
        [ "IC_IMM_I64"; "IC_IMM_I64"; "IC_IMM_I64"; "IC_POWER"; "IC_POWER" ]
        (opcode_names chain);
      Alcotest.(check (list int64))
        "each integer power operand converts"
        [ result_to_f64; result_to_f64; result_to_f64; 0L; 0L ]
        (instruction_flags chain);
      Alcotest.(check (list int))
        "inner right operands" [ 21; 22 ]
        (List.map Sequence.Value_id.to_int (List.nth chain_items 3).operands);
      Alcotest.(check (list int))
        "outer power operands" [ 20; 23 ]
        (List.map Sequence.Value_id.to_int (List.nth chain_items 4).operands);
      ignore (verify_x87 chain);
      let negative = List.nth roots 1 |> lower |> require_lowered in
      Alcotest.(check (list string))
        "unary minus follows power"
        [ "IC_IMM_I64"; "IC_IMM_I64"; "IC_POWER"; "IC_UNARY_MINUS" ]
        (opcode_names negative);
      Alcotest.(check (list int64))
        "negative power flags"
        [ result_to_f64; result_to_f64; 0L; 0L ]
        (instruction_flags negative);
      ignore (verify_x87 negative))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let top_level_power_uses_the_same_lowering () =
  List.iter
    (fun mode ->
      let root =
        top_level_roots ~mode ~path:"ir-top-level-power.HC" "2`3;" |> List.hd
      in
      let lowered = lower ~instruction:40 ~value:60 root |> require_lowered in
      Alcotest.(check (list string))
        "top-level power"
        [ "IC_IMM_I64"; "IC_IMM_I64"; "IC_POWER" ]
        (opcode_names lowered);
      Alcotest.(check (list int64))
        "top-level power conversions"
        [ result_to_f64; result_to_f64; 0L ]
        (instruction_flags lowered);
      Alcotest.(check bool)
        "top-level result is F64" true
        (internal_f64_type (Some (Expression.result_type lowered)));
      Alcotest.(check int)
        "top-level next instruction" 43
        (Expression.next_instruction_id lowered
        |> Sequence.Instruction_id.to_int);
      Alcotest.(check int)
        "top-level next value" 63
        (Expression.next_value_id lowered |> Sequence.Value_id.to_int);
      ignore (verify_x87 lowered))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let deterministic_power_dump_records_both_conversions () =
  let root =
    function_roots ~mode:Preprocessor.Jit ~path:"ir-power-dump.HC"
      "extern F64 Target(F64 value);F64 Caller(){return Target(2`3);}"
    |> List.hd
  in
  let lowered = lower ~instruction:70 ~value:90 root |> require_lowered in
  let repeated = lower ~instruction:70 ~value:90 root |> require_lowered in
  let dump = Expression.human lowered in
  Alcotest.(check string)
    "deterministic power replay" dump
    (Expression.human repeated);
  Alcotest.(check int)
    "dump contains both conversion flags" 2
    (String.split_on_char '\n' dump
    |> List.filter (fun line -> contains_substring line "flags=0x000000001")
    |> List.length);
  Alcotest.(check bool)
    "power remains unflagged" true
    (contains_substring dump "IC_POWER %v90 %v91 flags=0x000000000");
  Alcotest.(check string)
    "power result and next identities"
    "result=%v92 result-type=internal:F64 next-instruction=73 next-value=93"
    (List.nth (String.split_on_char '\n' dump) 1)

let deep_power_tree_uses_the_explicit_worklist () =
  let leaf_count = 200 in
  let expression = List.init leaf_count (fun _ -> "1") |> String.concat "`" in
  let source =
    Printf.sprintf
      "extern F64 Target(F64 value);F64 Caller(){return Target(%s);}" expression
  in
  let root =
    function_roots ~mode:Preprocessor.Jit ~path:"ir-deep-power.HC" source
    |> List.hd
  in
  let lowered = lower ~instruction:100 ~value:1_000 root |> require_lowered in
  let instruction_count = (leaf_count * 2) - 1 in
  let flags = instruction_flags lowered in
  Alcotest.(check int)
    "deep power instruction count" instruction_count
    (Expression.sequence lowered |> Sequence.length);
  Alcotest.(check int)
    "every integer leaf converts" leaf_count
    (List.filter (Int64.equal result_to_f64) flags |> List.length);
  Alcotest.(check int)
    "every power remains unflagged" (leaf_count - 1)
    (List.filter (Int64.equal 0L) flags |> List.length);
  Alcotest.(check int)
    "deep power next instruction" (100 + instruction_count)
    (Expression.next_instruction_id lowered |> Sequence.Instruction_id.to_int);
  Alcotest.(check int)
    "deep power next value"
    (1_000 + instruction_count)
    (Expression.next_value_id lowered |> Sequence.Value_id.to_int)

let f64_unary_and_binary_tree_keeps_checked_postorder () =
  let source =
    "extern F64 Target(F64 value);F64 Caller(){return \
     Target(-((!1.5)+(+2.0)*3.0));}"
  in
  let minus = String.index source '-' in
  let logical_not = String.index source '!' in
  let add = String.index source '+' in
  let multiply = String.index source '*' in
  List.iter
    (fun mode ->
      let root =
        function_roots ~mode ~path:"ir-f64-arithmetic-nesting.HC" source
        |> List.hd
      in
      let lowered = lower ~instruction:40 ~value:60 root |> require_lowered in
      let items = descriptions lowered in
      Alcotest.(check (list string))
        "floating postorder"
        [
          "IC_IMM_F64";
          "IC_NOT";
          "IC_IMM_F64";
          "IC_IMM_F64";
          "IC_MUL";
          "IC_ADD";
          "IC_UNARY_MINUS";
        ]
        (opcode_names lowered);
      Alcotest.(check (list (list int)))
        "floating operands"
        [ []; [ 60 ]; []; []; [ 62; 63 ]; [ 61; 64 ]; [ 65 ] ]
        (List.map
           (fun (description : Sequence.description) ->
             List.map Sequence.Value_id.to_int description.operands)
           items);
      Alcotest.(check bool)
        "every floating node has the checked type" true
        (List.for_all
           (fun (description : Sequence.description) ->
             internal_f64_type description.target_type)
           items);
      let check_span index expected =
        match (List.nth items index).span with
        | Some span ->
            Alcotest.(check int) "operator span start" expected span.start;
            Alcotest.(check int) "operator span stop" (expected + 1) span.stop
        | None -> Alcotest.fail "floating operator lost its source span"
      in
      check_span 1 logical_not;
      check_span 4 multiply;
      check_span 5 add;
      check_span 6 minus;
      Alcotest.(check int)
        "floating result value" 66
        (Expression.result_value lowered |> Sequence.Value_id.to_int);
      Alcotest.(check int)
        "floating next instruction" 47
        (Expression.next_instruction_id lowered
        |> Sequence.Instruction_id.to_int);
      Alcotest.(check int)
        "floating next value" 67
        (Expression.next_value_id lowered |> Sequence.Value_id.to_int))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let f64_division_by_zero_remains_unfolded_ir () =
  let root =
    function_roots ~mode:Preprocessor.Jit ~path:"ir-f64-zero-divisor.HC"
      "extern F64 Target(F64 value);F64 Caller(){return Target(4.0/0.0);}"
    |> List.hd
  in
  let lowered = lower root |> require_lowered in
  Alcotest.(check (list string))
    "lowering does not evaluate division"
    [ "IC_IMM_F64"; "IC_IMM_F64"; "IC_DIV" ]
    (opcode_names lowered);
  Alcotest.(check bool)
    "zero divisor keeps its bit pattern" true
    (Option.bind
       (List.nth_opt (descriptions lowered) 1)
       (fun (description : Sequence.description) -> description.payload)
    = Some (Sequence.Float_bits 0L))

let unsupported_f64_domains_return_no_sequence () =
  let source =
    "extern F64 Target(F64 modulo,I64 logical,I64 complement);F64 \
     Caller(){return Target(1%2.0,1&&2.0,~1.0);}"
  in
  List.iter
    (fun mode ->
      let roots =
        function_roots ~mode ~path:"ir-f64-unsupported-domains.HC" source
      in
      Alcotest.(check int) "unsupported floating roots" 3 (List.length roots);
      List.iter
        (fun root ->
          match lower root with
          | Expression.Unsupported_expression -> ()
          | Expression.Lowered _ ->
              Alcotest.fail
                "unsupported floating expression returned a partial sequence")
        roots)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let deterministic_f64_dump_records_result_and_next_ids () =
  let root =
    function_roots ~mode:Preprocessor.Jit ~path:"ir-f64-arithmetic-dump.HC"
      "extern F64 Target(F64 value);F64 Caller(){return Target(1.0+2.0);}"
    |> List.hd
  in
  let lowered = lower ~instruction:70 ~value:90 root |> require_lowered in
  let repeated = lower ~instruction:70 ~value:90 root |> require_lowered in
  let dump = Expression.human lowered in
  Alcotest.(check string)
    "deterministic F64 replay" dump
    (Expression.human repeated);
  Alcotest.(check string)
    "F64 result and next identities"
    "result=%v92 result-type=internal:F64 next-instruction=73 next-value=93"
    (List.nth (String.split_on_char '\n' dump) 1)

let deep_f64_tree_uses_the_explicit_worklist () =
  let leaf_count = 2_000 in
  let expression = List.init leaf_count (fun _ -> "1.0") |> String.concat "+" in
  let source =
    Printf.sprintf
      "extern F64 Target(F64 value);F64 Caller(){return Target(%s);}" expression
  in
  let root =
    function_roots ~mode:Preprocessor.Jit ~path:"ir-deep-f64-arithmetic.HC"
      source
    |> List.hd
  in
  let lowered = lower ~instruction:100 ~value:1_000 root |> require_lowered in
  let instruction_count = (leaf_count * 2) - 1 in
  Alcotest.(check int)
    "one instruction per floating literal and add" instruction_count
    (Expression.sequence lowered |> Sequence.length);
  Alcotest.(check int)
    "deep floating next instruction" (100 + instruction_count)
    (Expression.next_instruction_id lowered |> Sequence.Instruction_id.to_int);
  Alcotest.(check int)
    "deep floating next value"
    (1_000 + instruction_count)
    (Expression.next_value_id lowered |> Sequence.Value_id.to_int)

let mixed_f64_arithmetic_marks_each_integer_operand () =
  let cases =
    [
      ("1*2.0", "IC_MUL", 0, 1L);
      ("1.0*2", "IC_MUL", 1, 2L);
      ("1/2.0", "IC_DIV", 0, 1L);
      ("1.0/2", "IC_DIV", 1, 2L);
      ("1+2.0", "IC_ADD", 0, 1L);
      ("1.0+2", "IC_ADD", 1, 2L);
      ("1-2.0", "IC_SUB", 0, 1L);
      ("1.0-2", "IC_SUB", 1, 2L);
    ]
  in
  let parameters =
    List.mapi (fun index _ -> Printf.sprintf "F64 a%d" index) cases
    |> String.concat ","
  in
  let arguments = cases |> List.map (fun (source, _, _, _) -> source) in
  let source =
    Printf.sprintf "extern F64 Target(%s);F64 Caller(){return Target(%s);}"
      parameters
      (String.concat "," arguments)
  in
  List.iter
    (fun mode ->
      let roots =
        function_roots ~mode ~path:"ir-mixed-f64-arithmetic.HC" source
      in
      Alcotest.(check int)
        "one root per mixed arithmetic form" (List.length cases)
        (List.length roots);
      List.iter2
        (fun root (_, expected_opcode, converted_index, integer_payload) ->
          let lowered = lower root |> require_lowered in
          let items = descriptions lowered in
          let expected_opcodes =
            if converted_index = 0 then
              [ "IC_IMM_I64"; "IC_IMM_F64"; expected_opcode ]
            else [ "IC_IMM_F64"; "IC_IMM_I64"; expected_opcode ]
          in
          let expected_flags =
            if converted_index = 0 then [ result_to_f64; 0L; 0L ]
            else [ 0L; result_to_f64; 0L ]
          in
          Alcotest.(check (list string))
            "mixed postorder" expected_opcodes (opcode_names lowered);
          Alcotest.(check (list int64))
            "one integer conversion flag" expected_flags
            (instruction_flags lowered);
          let converted = List.nth items converted_index in
          Alcotest.(check bool)
            "converted producer keeps its integer type" true
            (internal_i64_type converted.target_type);
          Alcotest.(check bool)
            "converted producer keeps its integer payload" true
            (converted.payload = Some (Sequence.Integer integer_payload));
          Alcotest.(check bool)
            "mixed arithmetic result is F64" true
            (internal_f64_type (List.nth items 2).target_type))
        roots cases)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let mixed_conversion_reaches_only_the_integer_subtree_root () =
  let source =
    "extern F64 Target(F64 left,F64 right);F64 Caller(){return \
     Target(((1+2))+3.0,3.0+(+((4+5))));}"
  in
  let first_inner_add = String.index source '+' in
  let second_inner_add = String.rindex source '+' in
  List.iter
    (fun mode ->
      let roots =
        function_roots ~mode ~path:"ir-mixed-f64-subtrees.HC" source
      in
      Alcotest.(check int) "two mixed subtrees" 2 (List.length roots);
      let left = List.nth roots 0 |> lower |> require_lowered in
      let left_items = descriptions left in
      Alcotest.(check (list string))
        "left integer subtree postorder"
        [ "IC_IMM_I64"; "IC_IMM_I64"; "IC_ADD"; "IC_IMM_F64"; "IC_ADD" ]
        (opcode_names left);
      Alcotest.(check (list int64))
        "only left subtree root converts"
        [ 0L; 0L; result_to_f64; 0L; 0L ]
        (instruction_flags left);
      Alcotest.(check (list (list int)))
        "left subtree operands"
        [ []; []; [ 20; 21 ]; []; [ 22; 23 ] ]
        (List.map
           (fun (description : Sequence.description) ->
             List.map Sequence.Value_id.to_int description.operands)
           left_items);
      (match (List.nth left_items 2).span with
      | Some span ->
          Alcotest.(check int)
            "left conversion producer span" first_inner_add span.start
      | None -> Alcotest.fail "left conversion producer lost its span");
      let right = List.nth roots 1 |> lower |> require_lowered in
      let right_items = descriptions right in
      Alcotest.(check (list string))
        "right integer subtree postorder"
        [ "IC_IMM_F64"; "IC_IMM_I64"; "IC_IMM_I64"; "IC_ADD"; "IC_ADD" ]
        (opcode_names right);
      Alcotest.(check (list int64))
        "transparent wrappers reach the right subtree root"
        [ 0L; 0L; 0L; result_to_f64; 0L ]
        (instruction_flags right);
      Alcotest.(check (list (list int)))
        "right subtree operands"
        [ []; []; []; [ 21; 22 ]; [ 20; 23 ] ]
        (List.map
           (fun (description : Sequence.description) ->
             List.map Sequence.Value_id.to_int description.operands)
           right_items);
      match (List.nth right_items 3).span with
      | Some span ->
          Alcotest.(check int)
            "right conversion producer span" second_inner_add span.start
      | None -> Alcotest.fail "right conversion producer lost its span")
    [ Preprocessor.Jit; Preprocessor.Aot ]

let mixed_conversion_marks_unary_and_dereference_producers () =
  let source =
    "extern F64 Target(F64 minus,F64 logical,F64 complement,F64 \
     dereference);F64 Caller(){return \
     Target((-1)+2.0,(!1)+2.0,(~1)+2.0,*(&1)+2.0);}"
  in
  let expected =
    [
      ([ "IC_IMM_I64"; "IC_UNARY_MINUS"; "IC_IMM_F64"; "IC_ADD" ], 1);
      ([ "IC_IMM_I64"; "IC_NOT"; "IC_IMM_F64"; "IC_ADD" ], 1);
      ([ "IC_IMM_I64"; "IC_COM"; "IC_IMM_F64"; "IC_ADD" ], 1);
      ([ "IC_IMM_I64"; "IC_ADDR"; "IC_DEREF"; "IC_IMM_F64"; "IC_ADD" ], 2);
    ]
  in
  List.iter
    (fun mode ->
      let roots =
        function_roots ~mode ~path:"ir-mixed-f64-producers.HC" source
      in
      List.iter2
        (fun root (expected_opcodes, converted_index) ->
          let lowered = lower root |> require_lowered in
          let items = descriptions lowered in
          Alcotest.(check (list string))
            "mixed producer postorder" expected_opcodes (opcode_names lowered);
          Alcotest.(check int)
            "one result conversion" 1
            (instruction_flags lowered
            |> List.filter (Int64.equal result_to_f64)
            |> List.length);
          Alcotest.(check int64)
            "conversion is on the retained producer" result_to_f64
            (List.nth items converted_index).flags;
          Alcotest.(check bool)
            "converted producer is integer" true
            (internal_i64_type (List.nth items converted_index).target_type);
          Alcotest.(check bool)
            "outer arithmetic is F64" true
            (internal_f64_type
               (List.nth items (List.length items - 1)).target_type))
        roots expected)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let deterministic_mixed_dump_records_conversion_intent () =
  let root =
    function_roots ~mode:Preprocessor.Jit ~path:"ir-mixed-f64-dump.HC"
      "extern F64 Target(F64 value);F64 Caller(){return Target((1+2)+3.0);}"
    |> List.hd
  in
  let lowered = lower ~instruction:70 ~value:90 root |> require_lowered in
  let repeated = lower ~instruction:70 ~value:90 root |> require_lowered in
  let dump = Expression.human lowered in
  Alcotest.(check string)
    "deterministic mixed replay" dump
    (Expression.human repeated);
  Alcotest.(check bool)
    "dump names the exact conversion flag" true
    (contains_substring dump "IC_ADD %v90 %v91 flags=0x000000001");
  Alcotest.(check string)
    "mixed result and next identities"
    "result=%v94 result-type=internal:F64 next-instruction=75 next-value=95"
    (List.nth (String.split_on_char '\n' dump) 1)

let deep_mixed_f64_tree_uses_the_explicit_worklist () =
  let integer_leaf_count = 2_000 in
  let integer_tree =
    List.init integer_leaf_count (fun _ -> "1") |> String.concat "+"
  in
  let source =
    Printf.sprintf
      "extern F64 Target(F64 value);F64 Caller(){return Target((%s)+0.5);}"
      integer_tree
  in
  let root =
    function_roots ~mode:Preprocessor.Jit ~path:"ir-deep-mixed-f64.HC" source
    |> List.hd
  in
  let lowered = lower ~instruction:100 ~value:1_000 root |> require_lowered in
  let instruction_count = (integer_leaf_count * 2) + 1 in
  let flags = instruction_flags lowered in
  Alcotest.(check int)
    "deep mixed instruction count" instruction_count
    (Expression.sequence lowered |> Sequence.length);
  Alcotest.(check int)
    "one deep subtree conversion" 1
    (flags |> List.filter (Int64.equal result_to_f64) |> List.length);
  Alcotest.(check int64)
    "deep integer root converts" result_to_f64
    (List.nth flags ((integer_leaf_count * 2) - 2));
  Alcotest.(check int)
    "deep mixed next instruction" (100 + instruction_count)
    (Expression.next_instruction_id lowered |> Sequence.Instruction_id.to_int);
  Alcotest.(check int)
    "deep mixed next value"
    (1_000 + instruction_count)
    (Expression.next_value_id lowered |> Sequence.Value_id.to_int)

let pointer_prefixes_compose_with_existing_trees () =
  let source =
    "extern I64 Target(I64 a,I64 *b,I64 c);I64 Caller(){return \
     Target(*(1+2),&((~1)+2),*(&((!1)+2)));}"
  in
  let body = String.index source '{' in
  let first_dereference = String.index_from source body '*' in
  let address = String.index_from source (first_dereference + 1) '&' in
  let outer_dereference = String.index_from source (address + 1) '*' in
  let inner_address = String.index_from source (outer_dereference + 1) '&' in
  List.iter
    (fun mode ->
      let roots =
        function_roots ~mode ~path:"ir-pointer-unary-trees.HC" source
      in
      Alcotest.(check int) "one root per pointer shape" 3 (List.length roots);
      let first = List.nth roots 0 |> lower |> require_lowered in
      let first_items = descriptions first in
      Alcotest.(check (list string))
        "dereference follows binary result"
        [ "IC_IMM_I64"; "IC_IMM_I64"; "IC_ADD"; "IC_DEREF" ]
        (opcode_names first);
      let first_deref = List.nth first_items 3 in
      Alcotest.(check (list int))
        "dereference operand" [ 22 ]
        (List.map Sequence.Value_id.to_int first_deref.operands);
      Alcotest.(check bool)
        "dereference result depth" true
        (pointer_depth 0 first_deref.target_type);
      (match first_deref.span with
      | Some span ->
          Alcotest.(check int)
            "dereference span start" first_dereference span.start
      | None -> Alcotest.fail "dereference lost its operator span");
      let second = List.nth roots 1 |> lower |> require_lowered in
      let second_items = descriptions second in
      Alcotest.(check (list string))
        "address follows numeric tree"
        [ "IC_IMM_I64"; "IC_COM"; "IC_IMM_I64"; "IC_ADD"; "IC_ADDR" ]
        (opcode_names second);
      let second_address = List.nth second_items 4 in
      Alcotest.(check (list int))
        "address operand" [ 23 ]
        (List.map Sequence.Value_id.to_int second_address.operands);
      Alcotest.(check bool)
        "address result depth" true
        (pointer_depth 1 second_address.target_type);
      (match second_address.span with
      | Some span ->
          Alcotest.(check int) "address span start" address span.start
      | None -> Alcotest.fail "address lost its operator span");
      let third = List.nth roots 2 |> lower |> require_lowered in
      let third_items = descriptions third in
      Alcotest.(check (list string))
        "dereference outside address remains"
        [
          "IC_IMM_I64"; "IC_NOT"; "IC_IMM_I64"; "IC_ADD"; "IC_ADDR"; "IC_DEREF";
        ]
        (opcode_names third);
      let third_address = List.nth third_items 4 in
      let third_deref = List.nth third_items 5 in
      Alcotest.(check (list int))
        "nested address operand" [ 23 ]
        (List.map Sequence.Value_id.to_int third_address.operands);
      Alcotest.(check (list int))
        "outer dereference operand" [ 24 ]
        (List.map Sequence.Value_id.to_int third_deref.operands);
      Alcotest.(check bool)
        "nested address depth" true
        (pointer_depth 1 third_address.target_type);
      Alcotest.(check bool)
        "outer dereference depth" true
        (pointer_depth 0 third_deref.target_type);
      (match (third_address.span, third_deref.span) with
      | Some address_span, Some dereference_span ->
          Alcotest.(check int)
            "nested address span" inner_address address_span.start;
          Alcotest.(check int)
            "outer dereference span" outer_dereference dereference_span.start
      | _ -> Alcotest.fail "nested pointer operations lost their spans");
      Alcotest.(check int)
        "pointer tree next instruction" 16
        (Expression.next_instruction_id third |> Sequence.Instruction_id.to_int);
      Alcotest.(check int)
        "pointer tree next value" 26
        (Expression.next_value_id third |> Sequence.Value_id.to_int))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let address_cancels_the_nearest_dereference_before_allocation () =
  let source =
    "extern I64 Target(I64 *a,I64 *b,I64 *c);I64 Caller(){return \
     Target(&*((1+2)),&(+(*((1+2)))),&*&*1);}"
  in
  List.iter
    (fun mode ->
      let roots =
        function_roots ~mode ~path:"ir-address-dereference-cancellation.HC"
          source
      in
      Alcotest.(check int)
        "one root per cancellation shape" 3 (List.length roots);
      List.iter
        (fun index ->
          let lowered = List.nth roots index |> lower |> require_lowered in
          Alcotest.(check (list string))
            "transparent wrappers do not block cancellation"
            [ "IC_IMM_I64"; "IC_IMM_I64"; "IC_ADD"; "IC_ADDR" ]
            (opcode_names lowered);
          let address = List.nth_opt (descriptions lowered) 3 in
          match address with
          | Some address ->
              Alcotest.(check (list int))
                "retained address consumes the binary result" [ 22 ]
                (List.map Sequence.Value_id.to_int address.operands);
              Alcotest.(check int)
                "canceled tree next instruction" 14
                (Expression.next_instruction_id lowered
                |> Sequence.Instruction_id.to_int)
          | None -> Alcotest.fail "canceled tree lost its address instruction")
        [ 0; 1 ];
      let nested = List.nth roots 2 |> lower |> require_lowered in
      Alcotest.(check (list string))
        "each nested address removes its immediate dereference"
        [ "IC_IMM_I64"; "IC_ADDR"; "IC_ADDR" ]
        (opcode_names nested);
      let nested_items = descriptions nested in
      Alcotest.(check (list int))
        "inner retained address operand" [ 20 ]
        ((List.nth nested_items 1).operands |> List.map Sequence.Value_id.to_int);
      Alcotest.(check (list int))
        "outer retained address operand" [ 21 ]
        ((List.nth nested_items 2).operands |> List.map Sequence.Value_id.to_int);
      Alcotest.(check int)
        "nested cancellation next instruction" 13
        (Expression.next_instruction_id nested |> Sequence.Instruction_id.to_int);
      Alcotest.(check int)
        "nested cancellation next value" 23
        (Expression.next_value_id nested |> Sequence.Value_id.to_int))
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

let postfix_casts_lower_in_both_modes () =
  let source =
    "extern I64 Target(I8 a,F64 b,I64 c,F64 d);I64 Caller(){return \
     Target(1(I8),(1)(F64),1(F64)(I64),(1+2)(F64));}"
  in
  let cases =
    [
      ( [ "IC_IMM_I64"; "IC_HOLYC_TYPECAST" ],
        [ []; [ 20 ] ],
        [ (Primitive.I8, 0L) ] );
      ( [ "IC_IMM_I64"; "IC_HOLYC_TYPECAST" ],
        [ []; [ 20 ] ],
        [ (Primitive.F64, 1L) ] );
      ( [ "IC_IMM_I64"; "IC_HOLYC_TYPECAST"; "IC_HOLYC_TYPECAST" ],
        [ []; [ 20 ]; [ 21 ] ],
        [ (Primitive.F64, 0L); (Primitive.I64, 0L) ] );
      ( [ "IC_IMM_I64"; "IC_IMM_I64"; "IC_ADD"; "IC_HOLYC_TYPECAST" ],
        [ []; []; [ 20; 21 ]; [ 22 ] ],
        [ (Primitive.F64, 1L) ] );
    ]
  in
  List.iter
    (fun mode ->
      let roots = function_roots ~mode ~path:"ir-postfix-casts.HC" source in
      Alcotest.(check int) "one root per cast form" 4 (List.length roots);
      List.iter2
        (fun root (expected_opcodes, expected_operands, expected_casts) ->
          let lowered = lower root |> require_lowered in
          let items = descriptions lowered in
          Alcotest.(check (list string))
            "cast postorder" expected_opcodes (opcode_names lowered);
          Alcotest.(check (list (list int)))
            "cast operand identities" expected_operands
            (List.map
               (fun (description : Sequence.description) ->
                 List.map Sequence.Value_id.to_int description.operands)
               items);
          Alcotest.(check (list int))
            "consecutive cast instruction IDs"
            (List.init (List.length items) (fun index -> 10 + index))
            (List.map
               (fun (description : Sequence.description) ->
                 Sequence.Instruction_id.to_int description.instruction_id)
               items);
          Alcotest.(check (list int))
            "consecutive cast value IDs"
            (List.init (List.length items) (fun index -> 20 + index))
            (List.map
               (fun (description : Sequence.description) ->
                 match description.result with
                 | Some result -> Sequence.Value_id.to_int result.value_id
                 | None -> Alcotest.fail "cast tree instruction lost its result")
               items);
          let casts =
            List.filter
              (fun (description : Sequence.description) ->
                Opcode.equal description.opcode Opcode.Ic_holyc_typecast)
              items
          in
          Alcotest.(check int)
            "expected cast count"
            (List.length expected_casts)
            (List.length casts);
          List.iter2
            (fun (primitive, was_parenthesized)
                 (description : Sequence.description) ->
              Alcotest.(check bool)
                "cast keeps its checked public target" true
                (public_primitive_type primitive description.target_type);
              Alcotest.(check bool)
                "cast records PrsUnaryModifier's was_paren bit" true
                (description.payload = Some (Sequence.Integer was_parenthesized)))
            expected_casts casts;
          let expected_span = source_span root in
          let outer_cast = List.hd (List.rev casts) in
          Alcotest.(check bool)
            "outer cast keeps its full source span" true
            (outer_cast.span = Some expected_span);
          Alcotest.(check int)
            "cast result value"
            (20 + List.length items - 1)
            (Expression.result_value lowered |> Sequence.Value_id.to_int);
          Alcotest.(check int)
            "next cast instruction ID"
            (10 + List.length items)
            (Expression.next_instruction_id lowered
            |> Sequence.Instruction_id.to_int);
          Alcotest.(check int)
            "next cast value ID"
            (20 + List.length items)
            (Expression.next_value_id lowered |> Sequence.Value_id.to_int))
        roots cases)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let top_level_postfix_cast_uses_the_same_lowering () =
  List.iter
    (fun mode ->
      let root =
        top_level_roots ~mode ~path:"ir-top-level-postfix-cast.HC" "(1)(I16);"
        |> List.hd
      in
      let lowered = lower root |> require_lowered in
      let items = descriptions lowered in
      Alcotest.(check (list string))
        "top-level cast postorder"
        [ "IC_IMM_I64"; "IC_HOLYC_TYPECAST" ]
        (opcode_names lowered);
      let cast = List.nth items 1 in
      Alcotest.(check bool)
        "top-level target is public I16" true
        (public_primitive_type Primitive.I16 cast.target_type);
      Alcotest.(check bool)
        "top-level grouping is retained" true
        (cast.payload = Some (Sequence.Integer 1L));
      Alcotest.(check bool)
        "top-level cast span" true
        (cast.span = Some (source_span root)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let mixed_arithmetic_marks_the_postfix_cast_producer () =
  let source =
    "extern F64 Target(F64 value);F64 Caller(){return Target(1(I8)+2.0);}"
  in
  List.iter
    (fun mode ->
      let root =
        function_roots ~mode ~path:"ir-postfix-cast-mixed-f64.HC" source
        |> List.hd
      in
      let lowered = lower root |> require_lowered in
      let items = descriptions lowered in
      Alcotest.(check (list string))
        "mixed cast postorder"
        [ "IC_IMM_I64"; "IC_HOLYC_TYPECAST"; "IC_IMM_F64"; "IC_ADD" ]
        (opcode_names lowered);
      Alcotest.(check (list int64))
        "conversion belongs to the cast producer"
        [ 0L; result_to_f64; 0L; 0L ]
        (instruction_flags lowered);
      let cast = List.nth items 1 in
      Alcotest.(check bool)
        "mixed cast keeps the I8 target" true
        (public_primitive_type Primitive.I8 cast.target_type);
      Alcotest.(check bool)
        "mixed cast keeps the ungrouped payload" true
        (cast.payload = Some (Sequence.Integer 0L));
      Alcotest.(check bool)
        "mixed result remains F64" true
        (internal_f64_type (List.nth items 3).target_type))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let deterministic_postfix_cast_dump_records_payload () =
  let root =
    top_level_roots ~mode:Preprocessor.Jit ~path:"ir-postfix-cast-dump.HC"
      "(1)(I16);"
    |> List.hd
  in
  let lowered = lower ~instruction:70 ~value:90 root |> require_lowered in
  let repeated = lower ~instruction:70 ~value:90 root |> require_lowered in
  let dump = Expression.human lowered in
  Alcotest.(check string)
    "deterministic cast replay" dump
    (Expression.human repeated);
  Alcotest.(check bool)
    "cast dump includes target, operand, and payload" true
    (contains_substring dump
       "%v91:public:I16 = IC_HOLYC_TYPECAST %v90 i64:1 flags=0x000000000")

let unsupported_postfix_cast_operand_returns_no_sequence () =
  let source =
    "extern I64 Helper();extern I64 Target(I64 value);I64 Caller(){return \
     Target(Helper()(I64));}"
  in
  List.iter
    (fun mode ->
      let prepared =
        Test_function_call_expression_result.prepare ~mode
          ~path:"ir-unsupported-postfix-cast.HC" source
      in
      let _, results = Test_function_call_expression_result.analyze prepared in
      let root =
        Test_function_call_expression_result.direct_named results "Caller"
          "Target"
        |> Test_function_call_expression_result.provided_results |> List.hd
      in
      match lower root with
      | Expression.Unsupported_expression -> ()
      | Expression.Lowered _ ->
          Alcotest.fail "unsupported cast operand returned a partial sequence")
    [ Preprocessor.Jit; Preprocessor.Aot ]

let current_position_leaves_lower_in_both_modes () =
  let check_owner label root =
    let lowered = lower root |> require_lowered in
    let item = List.hd (descriptions lowered) in
    Alcotest.(check (list string)) label [ "IC_RIP" ] (opcode_names lowered);
    Alcotest.(check (list int))
      (label ^ " operands") []
      (List.map Sequence.Value_id.to_int item.operands);
    Alcotest.(check bool)
      (label ^ " result") true
      (match item.result with
      | Some result -> Sequence.Value_id.to_int result.value_id = 20
      | None -> false);
    Alcotest.(check bool)
      (label ^ " target type") true
      (Option.fold ~none:false ~some:internal_i64_value_type item.target_type);
    Alcotest.(check bool)
      (label ^ " payload") true
      (Option.is_none item.payload);
    Alcotest.(check int64) (label ^ " flags") 0L item.flags;
    Alcotest.(check bool)
      (label ^ " source span") true
      (item.span = Some (source_span root));
    Alcotest.(check int)
      (label ^ " next instruction")
      11
      (Expression.next_instruction_id lowered |> Sequence.Instruction_id.to_int);
    Alcotest.(check int)
      (label ^ " next value") 21
      (Expression.next_value_id lowered |> Sequence.Value_id.to_int)
  in
  List.iter
    (fun mode ->
      let function_root =
        function_roots ~mode ~path:"ir-current-position-function.HC"
          "extern I64 Target(I64 value);I64 Caller(){return Target($$);}"
        |> List.hd
      in
      let top_level_root =
        top_level_roots ~mode ~path:"ir-current-position-top-level.HC" "$$;"
        |> List.hd
      in
      check_owner "function current position" function_root;
      check_owner "top-level current position" top_level_root)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let current_position_composes_with_numeric_trees () =
  let expected =
    [
      ([ "IC_RIP"; "IC_IMM_F64"; "IC_ADD" ], [ result_to_f64; 0L; 0L ]);
      ([ "IC_RIP"; "IC_IMM_I64"; "IC_ADD" ], [ 0L; 0L; 0L ]);
      ([ "IC_RIP"; "IC_UNARY_MINUS" ], [ 0L; 0L ]);
      ([ "IC_RIP"; "IC_HOLYC_TYPECAST" ], [ 0L; 0L ]);
    ]
  in
  List.iter
    (fun mode ->
      let roots =
        function_roots ~mode ~path:"ir-current-position-composition.HC"
          "extern I64 Target(F64 a,I64 b,I64 c,I64 d);I64 Caller(){return \
           Target($$+2.0,($$)+1,-$$,($$)(I64));}"
      in
      Alcotest.(check int)
        "four current-position expressions" 4 (List.length roots);
      List.iter2
        (fun root (opcodes, flags) ->
          let lowered = lower root |> require_lowered in
          Alcotest.(check (list string))
            "composed opcodes" opcodes (opcode_names lowered);
          Alcotest.(check (list int64))
            "composed flags" flags
            (instruction_flags lowered);
          let rip = List.hd (descriptions lowered) in
          Alcotest.(check bool)
            "IC_RIP keeps internal RT_PTR storage" true
            (Option.fold ~none:false ~some:internal_i64_value_type
               rip.target_type))
        roots expected)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let deterministic_current_position_dump () =
  let root =
    top_level_roots ~mode:Preprocessor.Jit ~path:"ir-current-position-dump.HC"
      "$$;"
    |> List.hd
  in
  let lower_once () =
    lower_result ~instruction:70 ~value:90 root
    |> require_ok show_sequence_errors
    |> require_lowered
  in
  let first = lower_once () in
  let second = lower_once () in
  let dump = Expression.human first in
  Alcotest.(check string)
    "current-position lowering replays deterministically" dump
    (Expression.human second);
  Alcotest.(check bool)
    "dump records a typed zero-operand RIP producer" true
    (String.split_on_char '\n' dump
    |> List.exists (fun line ->
        String.equal line
          "!i70 %v90:internal:I64 = IC_RIP flags=0x000000000 @source=0:0..2"))

let defined_constants_lower_in_both_modes () =
  let source =
    "extern I64 Target(I64 parameter,I64 local,I64 token,F64 mixed);I64 \
     Caller(I64 parameter){I64 local;return \
     Target(defined(parameter),defined(local),defined(+),defined(local)+2.0);}"
  in
  List.iter
    (fun mode ->
      let roots = function_roots ~mode ~path:"ir-defined-constants.HC" source in
      Alcotest.(check int) "four defined roots" 4 (List.length roots);
      List.iter2
        (fun root expected ->
          let lowered = lower root |> require_lowered in
          let item = List.hd (descriptions lowered) in
          Alcotest.(check (list string))
            "defined emits one integer immediate" [ "IC_IMM_I64" ]
            (opcode_names lowered);
          Alcotest.(check bool)
            "defined retains its Boolean payload" true
            (item.payload = Some (Sequence.Integer expected));
          Alcotest.(check bool)
            "defined retains internal I64 storage" true
            (internal_i64_type item.target_type);
          Alcotest.(check bool)
            "defined keeps its complete expression span" true
            (item.span = Some (source_span root));
          Alcotest.(check (list int))
            "defined has no operands" []
            (List.map Sequence.Value_id.to_int item.operands);
          Alcotest.(check int)
            "defined result identity" 20
            (Expression.result_value lowered |> Sequence.Value_id.to_int);
          Alcotest.(check int)
            "defined advances one instruction" 11
            (Expression.next_instruction_id lowered
            |> Sequence.Instruction_id.to_int);
          Alcotest.(check int)
            "defined advances one value" 21
            (Expression.next_value_id lowered |> Sequence.Value_id.to_int))
        (List.filteri (fun index _ -> index < 3) roots)
        [ 1L; 1L; 0L ];
      let mixed = List.nth roots 3 |> lower |> require_lowered in
      Alcotest.(check (list string))
        "defined composes with mixed F64 arithmetic"
        [ "IC_IMM_I64"; "IC_IMM_F64"; "IC_ADD" ]
        (opcode_names mixed);
      Alcotest.(check (list int64))
        "the conversion flag belongs to the defined producer"
        [ result_to_f64; 0L; 0L ] (instruction_flags mixed);
      Alcotest.(check bool)
        "mixed defined remains one" true
        ((List.hd (descriptions mixed)).payload = Some (Sequence.Integer 1L)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let unresolved_function_and_checked_top_level_defined_names () =
  List.iter
    (fun mode ->
      let function_roots =
        function_roots ~mode ~path:"ir-defined-deferred-function.HC"
          "extern I64 Target(I64 before,I64 missing);I64 \
           Caller(){Target(defined(later),defined(missing));I64 later;return \
           0;}"
      in
      Alcotest.(check int)
        "two deferred function names" 2
        (List.length function_roots);
      List.iter
        (fun root ->
          match lower root with
          | Expression.Unsupported_expression -> ()
          | Expression.Lowered _ ->
              Alcotest.fail
                "deferred function defined expression returned a sequence")
        function_roots;
      let roots =
        top_level_roots ~mode ~path:"ir-defined-top-level.HC"
          "defined(+);defined(name);"
      in
      let false_root = List.nth roots 0 in
      let false_lowered = lower false_root |> require_lowered in
      Alcotest.(check bool)
        "top-level non-name lowers to false" true
        ((List.hd (descriptions false_lowered)).payload
       = Some (Sequence.Integer 0L));
      match List.nth roots 1 |> lower with
      | Expression.Unsupported_expression ->
          Alcotest.fail "checked top-level absence did not lower"
      | Expression.Lowered lowered ->
          Alcotest.(check bool)
            "complete top-level miss lowers to false" true
            ((List.hd (descriptions lowered)).payload
           = Some (Sequence.Integer 0L));
          let module_roots =
            top_level_roots ~mode ~path:"ir-defined-top-level-module.HC"
              "I64 ModuleValue;defined(ModuleValue);"
          in
          let module_lowered =
            List.hd module_roots |> lower |> require_lowered
          in
          Alcotest.(check bool)
            "top-level module hit lowers to true" true
            ((List.hd (descriptions module_lowered)).payload
           = Some (Sequence.Integer 1L));
          let outer_roots =
            top_level_roots_with_outer ~mode
              ~path:"ir-defined-top-level-outer.HC" ~names:[ "OuterValue" ]
              "defined(OuterValue);defined(Missing);"
          in
          List.iter2
            (fun root expected ->
              let lowered = lower root |> require_lowered in
              Alcotest.(check bool)
                "top-level outer query lowers its checked Boolean" true
                ((List.hd (descriptions lowered)).payload
               = Some (Sequence.Integer expected)))
            outer_roots [ 1L; 0L ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let module_defined_names_lower_in_both_modes () =
  let source =
    "extern class PriorType;I64 PriorGlobal;extern I64 Target(I64 \
     type_name,I64 global_name,I64 recursive_name,I64 later_name);I64 \
     Caller(){return \
     Target(defined(PriorType),defined(PriorGlobal),defined(Caller),defined(Later));}I64 \
     Later;"
  in
  List.iter
    (fun mode ->
      let roots = function_roots ~mode ~path:"ir-defined-module.HC" source in
      Alcotest.(check int) "four module query roots" 4 (List.length roots);
      roots
      |> List.filteri (fun index _ -> index < 3)
      |> List.iter (fun root ->
          let lowered = lower root |> require_lowered in
          let item = List.hd (descriptions lowered) in
          Alcotest.(check (list string))
            "module query emits one integer immediate" [ "IC_IMM_I64" ]
            (opcode_names lowered);
          Alcotest.(check bool)
            "module query emits true" true
            (item.payload = Some (Sequence.Integer 1L));
          Alcotest.(check bool)
            "module query keeps its complete expression span" true
            (item.span = Some (source_span root)));
      match List.nth roots 3 |> lower with
      | Expression.Unsupported_expression -> ()
      | Expression.Lowered _ ->
          Alcotest.fail "a later module name was visible before publication")
    [ Preprocessor.Jit; Preprocessor.Aot ]

let outer_defined_hits_and_misses_lower_in_both_modes () =
  let source =
    "extern I64 Target(I64 present,I64 absent);I64 Caller(){return \
     Target(defined(OuterValue),defined(Missing));}"
  in
  List.iter
    (fun mode ->
      let roots =
        function_roots_with_outer ~mode ~path:"ir-defined-outer.HC"
          ~records:
            [
              ( "OuterValue",
                Holyc_lib.Semantic_outer_environment.Global_variable );
            ]
          source
      in
      Alcotest.(check int)
        "one outer hit and one proven miss" 2 (List.length roots);
      List.iter2
        (fun root expected ->
          let lowered = lower root |> require_lowered in
          let item = List.hd (descriptions lowered) in
          Alcotest.(check (list string))
            "outer defined emits one integer immediate" [ "IC_IMM_I64" ]
            (opcode_names lowered);
          Alcotest.(check bool)
            "outer defined retains its Boolean payload" true
            (item.payload = Some (Sequence.Integer expected));
          Alcotest.(check bool)
            "outer defined keeps internal I64 storage" true
            (internal_i64_type item.target_type);
          Alcotest.(check bool)
            "outer defined keeps its complete expression span" true
            (item.span = Some (source_span root)))
        roots [ 1L; 0L ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let deterministic_defined_dump_records_payload () =
  let root =
    top_level_roots ~mode:Preprocessor.Jit ~path:"ir-defined-dump.HC"
      "defined(+);"
    |> List.hd
  in
  let lower_once () = lower ~instruction:70 ~value:90 root |> require_lowered in
  let first = lower_once () in
  let dump = Expression.human first in
  Alcotest.(check string)
    "defined lowering replays deterministically" dump
    (Expression.human (lower_once ()));
  Alcotest.(check bool)
    "dump records the false internal I64 immediate" true
    (contains_substring dump
       "%v90:internal:I64 = IC_IMM_I64 i64:0 flags=0x000000000")

let inconsistent_postfix_cast_metadata_reports_no_partial_sequence () =
  let root =
    top_level_roots ~mode:Preprocessor.Jit
      ~path:"ir-postfix-cast-invalid-metadata.HC" "(&(&(&(&(&1)))))(I64);"
    |> List.hd
  in
  match lower_result root with
  | Error [ (error : Sequence.error) ] ->
      Alcotest.(check string) "diagnostic code" "HCIRL0004" error.code;
      Alcotest.(check string)
        "diagnostic message"
        "typed semantic expression does not have a checked result type"
        error.message;
      Alcotest.(check (option int))
        "no identity was consumed" None error.instruction_id;
      Alcotest.(check bool)
        "cast diagnostic has a source span" true
        (Option.is_some error.span)
  | Error errors ->
      Alcotest.failf "expected one cast metadata error, got %d"
        (List.length errors)
  | Ok _ -> Alcotest.fail "inconsistent cast metadata produced expression IR"

let unsupported_shapes_return_no_sequence () =
  let source =
    "extern I64 Helper();extern I64 Target(I64 a,I64 b,I64 c,I64 d,I64 e,I64 \
     f);I64 Caller(I64 x){return \
     Target(1+2.0,1%2.0,x=1,x+1,(&1)+2,Helper()+1);}"
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
      Alcotest.(check int) "root count" 6 (List.length roots);
      (match List.hd roots |> lower with
      | Expression.Lowered _ -> ()
      | Expression.Unsupported_expression ->
          Alcotest.fail "mixed arithmetic remained unsupported");
      List.iter
        (fun root ->
          match lower root with
          | Expression.Unsupported_expression -> ()
          | Expression.Lowered _ ->
              Alcotest.fail "unsupported expression returned a partial sequence")
        (List.tl roots))
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

let excessive_address_depth_reports_checked_metadata () =
  let root =
    top_level_roots ~mode:Preprocessor.Jit ~path:"ir-address-depth-metadata.HC"
      "&(&(&(&(&1))));"
    |> List.hd
  in
  match lower_result root with
  | Error [ (error : Sequence.error) ] ->
      Alcotest.(check string) "diagnostic code" "HCIRL0004" error.code;
      Alcotest.(check string)
        "diagnostic message"
        "typed semantic expression does not have a checked result type"
        error.message;
      Alcotest.(check (option int))
        "no identity was consumed" None error.instruction_id;
      Alcotest.(check bool)
        "outer address span" true
        (Option.is_some error.span)
  | Error errors ->
      Alcotest.failf "expected one pointer metadata error, got %d"
        (List.length errors)
  | Ok _ -> Alcotest.fail "excessive pointer depth produced expression IR"

let deep_mixed_tree_uses_the_explicit_worklist () =
  let leaf_count = 2_000 in
  let expression =
    List.init leaf_count (fun index ->
        match index mod 4 with
        | 0 -> "-1"
        | 1 -> "!1"
        | 2 -> "~1"
        | _ -> "*(&1)")
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
  let pointer_leaf_count = leaf_count / 4 in
  let instruction_count =
    (leaf_count * 2) + pointer_leaf_count + (leaf_count - 1)
  in
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
    Alcotest.test_case "F64 arithmetic operators" `Quick
      f64_arithmetic_operators_lower_in_both_modes;
    Alcotest.test_case "F64 comparison operators" `Quick
      f64_comparison_operators_lower_in_both_modes;
    Alcotest.test_case "mixed F64 comparison conversions" `Quick
      mixed_f64_comparisons_mark_integer_subtree_roots;
    Alcotest.test_case "F64 comparison parent conversion" `Quick
      f64_comparison_flags_compose_with_a_parent_conversion;
    Alcotest.test_case "deterministic F64 comparison dump" `Quick
      deterministic_f64_comparison_dump_records_execution_domain;
    Alcotest.test_case "top-level F64 comparison" `Quick
      top_level_f64_comparisons_use_the_same_lowering;
    Alcotest.test_case "deep F64 comparison explicit worklist" `Slow
      deep_mixed_f64_comparison_uses_the_explicit_worklist;
    Alcotest.test_case "power operand domains" `Quick
      power_operand_domains_lower_in_both_modes;
    Alcotest.test_case "power associativity and unary minus" `Quick
      right_associated_power_and_unary_minus_keep_checked_order;
    Alcotest.test_case "top-level power" `Quick
      top_level_power_uses_the_same_lowering;
    Alcotest.test_case "deterministic power dump" `Quick
      deterministic_power_dump_records_both_conversions;
    Alcotest.test_case "deep power explicit worklist" `Slow
      deep_power_tree_uses_the_explicit_worklist;
    Alcotest.test_case "F64 unary and binary postorder" `Quick
      f64_unary_and_binary_tree_keeps_checked_postorder;
    Alcotest.test_case "F64 division by zero remains IR" `Quick
      f64_division_by_zero_remains_unfolded_ir;
    Alcotest.test_case "unsupported F64 domains" `Quick
      unsupported_f64_domains_return_no_sequence;
    Alcotest.test_case "deterministic F64 dump" `Quick
      deterministic_f64_dump_records_result_and_next_ids;
    Alcotest.test_case "deep F64 explicit worklist" `Slow
      deep_f64_tree_uses_the_explicit_worklist;
    Alcotest.test_case "mixed F64 arithmetic conversion flags" `Quick
      mixed_f64_arithmetic_marks_each_integer_operand;
    Alcotest.test_case "mixed integer subtree conversion" `Quick
      mixed_conversion_reaches_only_the_integer_subtree_root;
    Alcotest.test_case "mixed unary and dereference conversion" `Quick
      mixed_conversion_marks_unary_and_dereference_producers;
    Alcotest.test_case "deterministic mixed F64 dump" `Quick
      deterministic_mixed_dump_records_conversion_intent;
    Alcotest.test_case "deep mixed F64 explicit worklist" `Slow
      deep_mixed_f64_tree_uses_the_explicit_worklist;
    Alcotest.test_case "pointer prefixes inside expression trees" `Quick
      pointer_prefixes_compose_with_existing_trees;
    Alcotest.test_case "address and dereference cancellation" `Quick
      address_cancels_the_nearest_dereference_before_allocation;
    Alcotest.test_case "deterministic expression dump" `Quick
      deterministic_dump_records_result_and_next_ids;
    Alcotest.test_case "postfix casts in JIT and AOT" `Quick
      postfix_casts_lower_in_both_modes;
    Alcotest.test_case "top-level postfix cast" `Quick
      top_level_postfix_cast_uses_the_same_lowering;
    Alcotest.test_case "mixed postfix cast conversion" `Quick
      mixed_arithmetic_marks_the_postfix_cast_producer;
    Alcotest.test_case "deterministic postfix cast dump" `Quick
      deterministic_postfix_cast_dump_records_payload;
    Alcotest.test_case "unsupported postfix cast operand" `Quick
      unsupported_postfix_cast_operand_returns_no_sequence;
    Alcotest.test_case "current-position leaves" `Quick
      current_position_leaves_lower_in_both_modes;
    Alcotest.test_case "current-position composition" `Quick
      current_position_composes_with_numeric_trees;
    Alcotest.test_case "deterministic current-position dump" `Quick
      deterministic_current_position_dump;
    Alcotest.test_case "defined constant lowering" `Quick
      defined_constants_lower_in_both_modes;
    Alcotest.test_case "function deferral and checked top-level defined" `Quick
      unresolved_function_and_checked_top_level_defined_names;
    Alcotest.test_case "module defined lowering" `Quick
      module_defined_names_lower_in_both_modes;
    Alcotest.test_case "outer defined lowering" `Quick
      outer_defined_hits_and_misses_lower_in_both_modes;
    Alcotest.test_case "deterministic defined dump" `Quick
      deterministic_defined_dump_records_payload;
    Alcotest.test_case "postfix cast metadata validation" `Quick
      inconsistent_postfix_cast_metadata_reports_no_partial_sequence;
    Alcotest.test_case "unsupported expression shapes" `Quick
      unsupported_shapes_return_no_sequence;
    Alcotest.test_case "identity exhaustion" `Quick
      exhausted_starting_ids_are_rejected;
    Alcotest.test_case "address depth metadata" `Quick
      excessive_address_depth_reports_checked_metadata;
    Alcotest.test_case "deep explicit worklist" `Slow
      deep_mixed_tree_uses_the_explicit_worklist;
  ]
