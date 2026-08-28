module Statement = Holyc_lib.Ir_expression_statement_lowering
module Sequence = Holyc_lib.Ir_instruction_sequence
module Opcode = Holyc_lib.Ir_opcode
module Semantic_result = Holyc_lib.Semantic_function_call_expression_result
module Semantic_source = Holyc_lib.Semantic_function_call_resolution
module Semantic_symbol = Holyc_lib.Semantic_symbol

module Top_level_target =
  Holyc_lib.Semantic_top_level_function_call_target_classification

module Top_level_source = Holyc_lib.Semantic_top_level_expression_tree
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

let require_lowered = function
  | Statement.Lowered lowered -> lowered
  | Statement.Unsupported_expression ->
      Alcotest.fail "expected a checked expression-statement sequence"

let lower_function ?(instruction = 10) ?(value = 20) statement =
  Statement.lower_function_statement
    ~instruction_id:(instruction_id instruction)
    ~value_id:(value_id value) statement

let lower_top_level ?(instruction = 10) ?(value = 20) root =
  Statement.lower_top_level_statement
    ~instruction_id:(instruction_id instruction)
    ~value_id:(value_id value) root

let top_level_call_inputs ~mode ~path source =
  let prepared = Test_top_level_expression_result.prepared ~mode ~path source in
  let _, _, _, results = Test_top_level_expression_result.analyze prepared in
  let records =
    Holyc_lib.classify_function_records prepared.session
      ~resolution:prepared.functions prepared.ast
    |> require_ok Fun.id
  in
  let calls = Semantic_result.top_level_direct_calls results in
  let roots =
    Test_top_level_expression_result.roots results
    |> List.filter (fun root ->
        match
          root |> Semantic_result.top_level_root_source
          |> Top_level_source.root_role
        with
        | Top_level_source.Expression_statement _ -> true
        | Top_level_source.Implicit_output_fixed _
        | Top_level_source.Implicit_output_argument _
        | Top_level_source.Condition _
        | Top_level_source.Switch_selector _
        | Top_level_source.Switch_case_value _
        | Top_level_source.Local_array_dimension _
        | Top_level_source.Local_initializer _
        | Top_level_source.Return_value _ -> false)
  in
  (records, calls, roots)

let top_level_call_target records call =
  Top_level_target.classify ~records call
  |> require_ok Top_level_target.error_to_string

let lower_top_level_call ?(instruction = 10) ?(value = 20) records call root =
  Statement.lower_top_level_direct_call_statement
    ~instruction_id:(instruction_id instruction)
    ~value_id:(value_id value)
    ~target:(top_level_call_target records call)
    root

let descriptions lowered =
  lowered |> Statement.sequence |> Sequence.instructions
  |> List.map Sequence.description

let opcode_names lowered =
  descriptions lowered
  |> List.map (fun (description : Sequence.description) ->
      Opcode.to_source_name description.opcode)

let span_of_origin = function
  | Semantic_symbol.Source_location location -> location.span
  | Semantic_symbol.Pinned_source _ | Semantic_symbol.Synthesized _ ->
      Alcotest.fail "expected source-backed statement provenance"

let function_statements ~mode ~path source =
  let prepared =
    Test_function_call_expression_result.prepare ~mode ~path source
  in
  let _, results = Test_function_call_expression_result.analyze prepared in
  Test_function_call_expression_result.expression_statements_named results
    "Caller"

let top_level_roots ~mode ~path source =
  let prepared = Test_top_level_expression_result.prepared ~mode ~path source in
  let _, _, _, results = Test_top_level_expression_result.analyze prepared in
  Test_top_level_expression_result.roots results

let expression_statement_roots roots =
  List.filter
    (fun root ->
      match
        root |> Semantic_result.top_level_root_source
        |> Top_level_source.root_role
      with
      | Top_level_source.Expression_statement _ -> true
      | Top_level_source.Implicit_output_fixed _
      | Top_level_source.Implicit_output_argument _
      | Top_level_source.Condition _
      | Top_level_source.Switch_selector _
      | Top_level_source.Switch_case_value _
      | Top_level_source.Local_array_dimension _
      | Top_level_source.Local_initializer _
      | Top_level_source.Return_value _ -> false)
    roots

let check_terminator ~expected_span lowered =
  let items = descriptions lowered in
  let terminator = List.hd (List.rev items) in
  Alcotest.(check string)
    "terminator opcode" "IC_END_EXP"
    (Opcode.to_source_name terminator.opcode);
  Alcotest.(check (list int))
    "terminator consumes the exact expression value"
    [ Statement.expression_value lowered |> Sequence.Value_id.to_int ]
    (List.map Sequence.Value_id.to_int terminator.operands);
  Alcotest.(check bool)
    "terminator has no result" true
    (Option.is_none terminator.result);
  Alcotest.(check bool)
    "terminator has no target type" true
    (Option.is_none terminator.target_type);
  Alcotest.(check bool)
    "terminator has no payload" true
    (Option.is_none terminator.payload);
  Alcotest.(check int64)
    "unused-result flag remains on the terminator" 0x000000200L terminator.flags;
  Alcotest.(check bool)
    "terminator keeps the complete statement span" true
    (terminator.span = Some expected_span);
  Alcotest.(check int)
    "terminator identity accessor"
    (Sequence.Instruction_id.to_int terminator.instruction_id)
    (Statement.terminator_id lowered |> Sequence.Instruction_id.to_int)

let function_statements_end_in_both_modes () =
  let source = "I64 Caller(){1+2;-(3);(4)(I16);1(I8)+2.0;return 0;}" in
  let expected =
    [
      [ "IC_IMM_I64"; "IC_IMM_I64"; "IC_ADD"; "IC_END_EXP" ];
      [ "IC_IMM_I64"; "IC_UNARY_MINUS"; "IC_END_EXP" ];
      [ "IC_IMM_I64"; "IC_HOLYC_TYPECAST"; "IC_END_EXP" ];
      [
        "IC_IMM_I64"; "IC_HOLYC_TYPECAST"; "IC_IMM_F64"; "IC_ADD"; "IC_END_EXP";
      ];
    ]
  in
  List.iter
    (fun mode ->
      let statements =
        function_statements ~mode ~path:"ir-function-end-expression.HC" source
      in
      Alcotest.(check int)
        "all numeric function statements" 4 (List.length statements);
      List.iter2
        (fun statement expected_opcodes ->
          let lowered =
            lower_function statement
            |> require_ok show_sequence_errors
            |> require_lowered
          in
          Alcotest.(check (list string))
            "expression precedes its terminator" expected_opcodes
            (opcode_names lowered);
          let expected_span =
            statement |> Semantic_result.expression_statement_source
            |> Semantic_source.expression_statement_origin |> span_of_origin
          in
          check_terminator ~expected_span lowered;
          let expression_count = List.length expected_opcodes - 1 in
          Alcotest.(check int)
            "terminator advances only the instruction identity"
            (11 + expression_count)
            (Statement.next_instruction_id lowered
            |> Sequence.Instruction_id.to_int);
          Alcotest.(check int)
            "terminator leaves the next value identity unchanged"
            (20 + expression_count)
            (Statement.next_value_id lowered |> Sequence.Value_id.to_int))
        statements expected)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let top_level_statements_use_the_same_terminator () =
  let source = "1+2;(3)(I16);" in
  List.iter
    (fun mode ->
      let roots =
        top_level_roots ~mode ~path:"ir-top-level-end-expression.HC" source
        |> expression_statement_roots
      in
      Alcotest.(check int)
        "two top-level expression roots" 2 (List.length roots);
      List.iter
        (fun root ->
          let lowered =
            lower_top_level root
            |> require_ok show_sequence_errors
            |> require_lowered
          in
          let expected_span =
            root |> Semantic_result.top_level_root_source
            |> Top_level_source.root_origin |> span_of_origin
          in
          check_terminator ~expected_span lowered;
          Alcotest.(check string)
            "top-level sequence ends at the statement boundary" "IC_END_EXP"
            (List.hd (List.rev (opcode_names lowered))))
        roots)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let producer_flags_survive_statement_lowering () =
  List.iter
    (fun mode ->
      let statement =
        function_statements ~mode ~path:"ir-end-expression-flags.HC"
          "I64 Caller(){1(I8)+2.0;return 0;}"
        |> List.hd
      in
      let lowered =
        lower_function statement
        |> require_ok show_sequence_errors
        |> require_lowered
      in
      Alcotest.(check (list int64))
        "the cast conversion and terminator flags remain separate"
        [ 0L; 0x000000001L; 0L; 0L; 0x000000200L ]
        (descriptions lowered
        |> List.map (fun (description : Sequence.description) ->
            description.flags)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let deterministic_statement_dump () =
  let statement =
    function_statements ~mode:Preprocessor.Jit ~path:"ir-end-expression-dump.HC"
      "I64 Caller(){(1)(I16);return 0;}"
    |> List.hd
  in
  let lower_once () =
    lower_function ~instruction:70 ~value:90 statement
    |> require_ok show_sequence_errors
    |> require_lowered
  in
  let first = lower_once () in
  let second = lower_once () in
  let dump = Statement.human first in
  Alcotest.(check string)
    "statement lowering replays deterministically" dump (Statement.human second);
  Alcotest.(check bool)
    "dump records the no-result terminator" true
    (String.split_on_char '\n' dump
    |> List.exists (fun line ->
        String.equal line
          "!i72 IC_END_EXP %v91 flags=0x000000200 @source=0:13..22"))

let current_position_statements_use_the_checked_terminator () =
  let check label expected_span lowered =
    Alcotest.(check (list string))
      label [ "IC_RIP"; "IC_END_EXP" ] (opcode_names lowered);
    Alcotest.(check (list int64))
      (label ^ " flags") [ 0L; 0x000000200L ]
      (descriptions lowered
      |> List.map (fun (description : Sequence.description) ->
          description.flags));
    check_terminator ~expected_span lowered
  in
  List.iter
    (fun mode ->
      let function_statement =
        function_statements ~mode ~path:"ir-current-position-statement.HC"
          "I64 Caller(){$$;return 0;}"
        |> List.hd
      in
      let function_span =
        function_statement |> Semantic_result.expression_statement_source
        |> Semantic_source.expression_statement_origin |> span_of_origin
      in
      let function_lowered =
        lower_function function_statement
        |> require_ok show_sequence_errors
        |> require_lowered
      in
      check "function current-position statement" function_span function_lowered;
      let top_level_root =
        top_level_roots ~mode ~path:"ir-current-position-top-level-statement.HC"
          "$$;"
        |> expression_statement_roots |> List.hd
      in
      let top_level_span =
        top_level_root |> Semantic_result.top_level_root_source
        |> Top_level_source.root_origin |> span_of_origin
      in
      let top_level_lowered =
        lower_top_level top_level_root
        |> require_ok show_sequence_errors
        |> require_lowered
      in
      check "top-level current-position statement" top_level_span
        top_level_lowered)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let defined_statements_use_the_checked_terminator () =
  let check label expected statement_span lowered =
    Alcotest.(check (list string))
      label
      [ "IC_IMM_I64"; "IC_END_EXP" ]
      (opcode_names lowered);
    let producer = List.hd (descriptions lowered) in
    Alcotest.(check bool)
      (label ^ " payload") true
      (producer.payload = Some (Sequence.Integer expected));
    Alcotest.(check int64) (label ^ " producer flags") 0L producer.flags;
    check_terminator ~expected_span:statement_span lowered
  in
  List.iter
    (fun mode ->
      let statements =
        function_statements ~mode ~path:"ir-defined-statement.HC"
          "I64 Caller(I64 local){defined(local);defined(+);return 0;}"
      in
      Alcotest.(check int)
        "two function defined statements" 2 (List.length statements);
      List.iter2
        (fun statement expected ->
          let span =
            statement |> Semantic_result.expression_statement_source
            |> Semantic_source.expression_statement_origin |> span_of_origin
          in
          let lowered =
            lower_function statement
            |> require_ok show_sequence_errors
            |> require_lowered
          in
          check "function defined statement" expected span lowered)
        statements [ 1L; 0L ];
      let top_level =
        top_level_roots ~mode ~path:"ir-defined-top-level-statement.HC"
          "I64 name;defined(+);defined(name);defined(missing);"
        |> expression_statement_roots
      in
      Alcotest.(check int)
        "three top-level defined statements" 3 (List.length top_level);
      List.iter2
        (fun root expected ->
          let span =
            root |> Semantic_result.top_level_root_source
            |> Top_level_source.root_origin |> span_of_origin
          in
          let lowered =
            root |> lower_top_level
            |> require_ok show_sequence_errors
            |> require_lowered
          in
          check "top-level defined statement" expected span lowered)
        top_level [ 0L; 1L; 0L ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let aggregate_offset_statements_use_the_checked_terminator () =
  List.iter
    (fun mode ->
      let top_level =
        top_level_roots ~mode ~path:"ir-top-level-offset-end.HC"
          "class Base {I8 inherited;};class Box : Base {I16 \
           prefix;};0+Box.prefix;"
        |> expression_statement_roots |> List.hd
      in
      let lowered =
        lower_top_level top_level
        |> require_ok show_sequence_errors
        |> require_lowered
      in
      Alcotest.(check (list string))
        "top-level offset uses the same terminator"
        [ "IC_IMM_I64"; "IC_IMM_I64"; "IC_ADD"; "IC_END_EXP" ]
        (opcode_names lowered);
      Alcotest.(check bool)
        "top-level statement keeps offset one" true
        ((List.nth (descriptions lowered) 1).payload
       = Some (Sequence.Integer 1L));
      let expected_span =
        top_level |> Semantic_result.top_level_root_source
        |> Top_level_source.root_origin |> span_of_origin
      in
      check_terminator ~expected_span lowered;
      let function_statement =
        function_statements ~mode ~path:"ir-function-offset-end.HC"
          "class Base {I8 inherited;};class Box : Base {I16 prefix;};I64 \
           Caller(){0+Box.prefix;return 0;}"
        |> List.hd
      in
      let function_lowered =
        lower_function function_statement
        |> require_ok show_sequence_errors
        |> require_lowered
      in
      Alcotest.(check (list string))
        "function offset uses the same terminator"
        [ "IC_IMM_I64"; "IC_IMM_I64"; "IC_ADD"; "IC_END_EXP" ]
        (opcode_names function_lowered);
      Alcotest.(check bool)
        "function statement keeps offset one" true
        ((List.nth (descriptions function_lowered) 1).payload
       = Some (Sequence.Integer 1L));
      let function_span =
        function_statement |> Semantic_result.expression_statement_source
        |> Semantic_source.expression_statement_origin |> span_of_origin
      in
      check_terminator ~expected_span:function_span function_lowered)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let standalone_offset_statements_use_the_checked_terminator () =
  List.iter
    (fun mode ->
      let top_level =
        top_level_roots ~mode ~path:"ir-top-level-standalone-offset-end.HC"
          "class Base {I8 inherited;};class Box : Base {I16 prefix;};Box \
           global;offset(global.prefix);"
        |> expression_statement_roots |> List.hd
      in
      let lowered =
        lower_top_level top_level
        |> require_ok show_sequence_errors
        |> require_lowered
      in
      Alcotest.(check (list string))
        "top-level standalone offset uses one immediate and one terminator"
        [ "IC_IMM_I64"; "IC_END_EXP" ]
        (opcode_names lowered);
      Alcotest.(check bool)
        "top-level standalone statement keeps offset one" true
        ((List.hd (descriptions lowered)).payload = Some (Sequence.Integer 1L));
      let expected_span =
        top_level |> Semantic_result.top_level_root_source
        |> Top_level_source.root_origin |> span_of_origin
      in
      check_terminator ~expected_span lowered;
      let function_statement =
        function_statements ~mode ~path:"ir-function-standalone-offset-end.HC"
          "class Base {I8 inherited;};class Box : Base {I16 prefix;};I64 \
           Caller(){Box automatic;offset(automatic.prefix);return 0;}"
        |> List.hd
      in
      let function_lowered =
        lower_function function_statement
        |> require_ok show_sequence_errors
        |> require_lowered
      in
      Alcotest.(check (list string))
        "function standalone offset uses one immediate and one terminator"
        [ "IC_IMM_I64"; "IC_END_EXP" ]
        (opcode_names function_lowered);
      Alcotest.(check bool)
        "function standalone statement keeps offset one" true
        ((List.hd (descriptions function_lowered)).payload
       = Some (Sequence.Integer 1L));
      let function_span =
        function_statement |> Semantic_result.expression_statement_source
        |> Semantic_source.expression_statement_origin |> span_of_origin
      in
      check_terminator ~expected_span:function_span function_lowered)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let unsupported_statement_values_return_no_sequence () =
  List.iter
    (fun mode ->
      let function_statement =
        function_statements ~mode ~path:"ir-unsupported-end-expression.HC"
          "I64 Caller(I64 value){value;return 0;}"
        |> List.hd
      in
      (match
         lower_function function_statement |> require_ok show_sequence_errors
       with
      | Statement.Unsupported_expression -> ()
      | Statement.Lowered _ ->
          Alcotest.fail "unsupported function value returned a sequence");
      let top_level =
        top_level_roots ~mode ~path:"ir-unsupported-top-level-end.HC"
          "I64 value;value;"
        |> expression_statement_roots |> List.hd
      in
      match lower_top_level top_level |> require_ok show_sequence_errors with
      | Statement.Unsupported_expression -> ()
      | Statement.Lowered _ ->
          Alcotest.fail "unsupported top-level value returned a sequence")
    [ Preprocessor.Jit; Preprocessor.Aot ]

let nonstatement_top_level_roots_are_rejected () =
  let root =
    top_level_roots ~mode:Preprocessor.Jit ~path:"ir-end-expression-role.HC"
      "if(1) 2;"
    |> List.find (fun root ->
        match
          root |> Semantic_result.top_level_root_source
          |> Top_level_source.root_role
        with
        | Top_level_source.Condition _ -> true
        | Top_level_source.Expression_statement _
        | Top_level_source.Implicit_output_fixed _
        | Top_level_source.Implicit_output_argument _
        | Top_level_source.Switch_selector _
        | Top_level_source.Switch_case_value _
        | Top_level_source.Local_array_dimension _
        | Top_level_source.Local_initializer _
        | Top_level_source.Return_value _ -> false)
  in
  match lower_top_level root with
  | Error [ (error : Sequence.error) ] ->
      Alcotest.(check string) "diagnostic code" "HCIRL0004" error.code;
      Alcotest.(check string)
        "diagnostic message"
        "top-level root is not an unused expression statement" error.message;
      Alcotest.(check bool)
        "diagnostic keeps the root span" true
        (Option.is_some error.span)
  | Error errors ->
      Alcotest.failf "expected one role diagnostic, got %d" (List.length errors)
  | Ok _ ->
      Alcotest.fail "condition root was accepted as an expression statement"

let terminator_identity_exhaustion_is_diagnostic () =
  let statement =
    function_statements ~mode:Preprocessor.Jit
      ~path:"ir-end-expression-identity.HC" "I64 Caller(){1;return 0;}"
    |> List.hd
  in
  match lower_function ~instruction:(Int.max_int - 1) statement with
  | Error [ (error : Sequence.error) ] ->
      Alcotest.(check string) "diagnostic code" "HCIRL0005" error.code;
      Alcotest.(check string)
        "diagnostic message"
        "cannot allocate another statement instruction identity because the \
         host integer range is exhausted"
        error.message;
      Alcotest.(check bool)
        "diagnostic keeps the statement span" true
        (Option.is_some error.span)
  | Error errors ->
      Alcotest.failf "expected one identity diagnostic, got %d"
        (List.length errors)
  | Ok _ -> Alcotest.fail "exhausted terminator identity was accepted"

let top_level_direct_calls_end_at_statement_boundaries () =
  let run mode source expected_calls =
    let records, calls, roots =
      top_level_call_inputs ~mode ~path:"ir-top-level-call-statement.HC" source
    in
    Alcotest.(check int)
      "call roots and targets remain paired"
      (List.length expected_calls)
      (List.length calls);
    List.iter2
      (fun (expected_opcode, call) root ->
        let lowered =
          lower_top_level_call records call root
          |> require_ok show_sequence_errors
          |> require_lowered
        in
        Alcotest.(check (list string))
          "the complete call precedes one unused-result terminator"
          [
            "IC_CALL_START";
            "IC_IMM_I64";
            "IC_IMM_I64";
            "IC_IMM_I64";
            expected_opcode;
            "IC_ADD_RSP";
            "IC_CALL_END";
            "IC_END_EXP";
          ]
          (opcode_names lowered);
        let expected_span =
          root |> Semantic_result.top_level_root_source
          |> Top_level_source.root_origin |> span_of_origin
        in
        check_terminator ~expected_span lowered;
        Alcotest.(check (pair int int))
          "the terminator advances only the instruction cursor" (18, 24)
          ( Statement.next_instruction_id lowered
            |> Sequence.Instruction_id.to_int,
            Statement.next_value_id lowered |> Sequence.Value_id.to_int ))
      (List.combine expected_calls calls)
      roots
  in
  run Preprocessor.Jit
    "_extern _BOUND I64 Direct(I64 first,...);\n\
     extern I64 External(I64 first,...);\n\
     Direct(1,2);External(3,4);"
    [ "IC_CALL"; "IC_CALL_INDIRECT2" ];
  run Preprocessor.Aot
    "_extern _BOUND I64 Direct(I64 first,...);\n\
     import I64 Imported(I64 first,...);\n\
     Direct(1,2);Imported(3,4);"
    [ "IC_CALL"; "IC_CALL_IMPORT" ]

let top_level_direct_call_statement_ownership_is_checked () =
  let records, calls, roots =
    top_level_call_inputs ~mode:Preprocessor.Jit
      ~path:"ir-top-level-call-statement-owner.HC"
      "I64 First(){return 1;}I64 Second(){return 2;}First();Second();"
  in
  (match lower_top_level_call records (List.hd calls) (List.nth roots 1) with
  | Error [ error ] ->
      Alcotest.(check string)
        "mismatched statement ownership code" "HCIRL0004" error.Sequence.code
  | Error _ -> Alcotest.fail "expected one statement ownership error"
  | Ok _ -> Alcotest.fail "a mismatched call statement was accepted");
  let condition_prepared =
    Test_top_level_expression_result.prepared ~mode:Preprocessor.Jit
      ~path:"ir-top-level-call-statement-role.HC"
      "I64 Callee(){return 1;}if(Callee()) 1;"
  in
  let _, _, _, condition_results =
    Test_top_level_expression_result.analyze condition_prepared
  in
  let condition_records =
    Holyc_lib.classify_function_records condition_prepared.session
      ~resolution:condition_prepared.functions condition_prepared.ast
    |> require_ok Fun.id
  in
  let condition_call =
    Semantic_result.top_level_direct_calls condition_results |> List.hd
  in
  let condition_root =
    Test_top_level_expression_result.roots condition_results
    |> List.find (fun root ->
        match
          root |> Semantic_result.top_level_root_source
          |> Top_level_source.root_role
        with
        | Top_level_source.Condition _ -> true
        | Top_level_source.Expression_statement _
        | Top_level_source.Implicit_output_fixed _
        | Top_level_source.Implicit_output_argument _
        | Top_level_source.Switch_selector _
        | Top_level_source.Switch_case_value _
        | Top_level_source.Local_array_dimension _
        | Top_level_source.Local_initializer _
        | Top_level_source.Return_value _ -> false)
  in
  (match
     lower_top_level_call condition_records condition_call condition_root
   with
  | Error [ error ] ->
      Alcotest.(check string)
        "nonstatement call root code" "HCIRL0004" error.Sequence.code
  | Error _ -> Alcotest.fail "expected one call-root role error"
  | Ok _ -> Alcotest.fail "a condition call was accepted as a statement");
  let exhausted_records, exhausted_calls, exhausted_roots =
    top_level_call_inputs ~mode:Preprocessor.Jit
      ~path:"ir-top-level-call-statement-identity.HC"
      "I64 Callee(){return 1;}Callee();"
  in
  match
    lower_top_level_call ~instruction:(Int.max_int - 4) exhausted_records
      (List.hd exhausted_calls) (List.hd exhausted_roots)
  with
  | Error [ error ] ->
      Alcotest.(check string)
        "call terminator exhaustion code" "HCIRL0005" error.Sequence.code
  | Error _ -> Alcotest.fail "expected one call terminator exhaustion error"
  | Ok _ -> Alcotest.fail "an exhausted call terminator was accepted"

let unsupported_top_level_direct_call_statements_return_no_sequence () =
  let check source =
    let records, calls, roots =
      top_level_call_inputs ~mode:Preprocessor.Jit
        ~path:"ir-top-level-call-statement-unsupported.HC" source
    in
    match
      lower_top_level_call records (List.hd calls) (List.hd roots)
      |> require_ok show_sequence_errors
    with
    | Statement.Unsupported_expression -> ()
    | Statement.Lowered _ ->
        Alcotest.fail "an unsupported call statement returned a sequence"
  in
  check "_intern 42 I64 Internal();Internal();";
  check "I64 Defaulted(I64 value=1);Defaulted();"

let tests =
  [
    Alcotest.test_case "aggregate offset statement terminators" `Quick
      aggregate_offset_statements_use_the_checked_terminator;
    Alcotest.test_case "standalone offset statement terminators" `Quick
      standalone_offset_statements_use_the_checked_terminator;
    Alcotest.test_case "function expression terminators" `Quick
      function_statements_end_in_both_modes;
    Alcotest.test_case "top-level expression terminators" `Quick
      top_level_statements_use_the_same_terminator;
    Alcotest.test_case "producer flags and statement terminator" `Quick
      producer_flags_survive_statement_lowering;
    Alcotest.test_case "deterministic statement dump" `Quick
      deterministic_statement_dump;
    Alcotest.test_case "current-position statement terminators" `Quick
      current_position_statements_use_the_checked_terminator;
    Alcotest.test_case "defined statement terminators" `Quick
      defined_statements_use_the_checked_terminator;
    Alcotest.test_case "unsupported statement values" `Quick
      unsupported_statement_values_return_no_sequence;
    Alcotest.test_case "top-level role validation" `Quick
      nonstatement_top_level_roots_are_rejected;
    Alcotest.test_case "terminator identity exhaustion" `Quick
      terminator_identity_exhaustion_is_diagnostic;
    Alcotest.test_case "top-level direct-call statement terminators" `Quick
      top_level_direct_calls_end_at_statement_boundaries;
    Alcotest.test_case "top-level direct-call statement ownership" `Quick
      top_level_direct_call_statement_ownership_is_checked;
    Alcotest.test_case "unsupported top-level direct-call statements" `Quick
      unsupported_top_level_direct_call_statements_return_no_sequence;
  ]
