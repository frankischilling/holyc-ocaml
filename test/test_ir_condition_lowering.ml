module Condition = Holyc_lib.Ir_condition_lowering
module Sequence = Holyc_lib.Ir_instruction_sequence
module Opcode = Holyc_lib.Ir_opcode
module Preprocessor = Holyc_lib.Preprocessor
module Semantic_result = Holyc_lib.Semantic_function_call_expression_result
module Top_level_condition = Holyc_lib.Semantic_top_level_condition_result

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

let block_id value =
  Sequence.Block_id.of_int value |> require_ok show_sequence_error

let conditions ~mode ~path source =
  let prepared =
    Test_function_call_expression_result.prepare ~mode ~path source
  in
  let _, results = Test_function_call_expression_result.analyze prepared in
  results |> Semantic_result.functions
  |> List.find (fun function_ ->
      function_ |> Semantic_result.function_symbol
      |> Holyc_lib.Semantic_symbol.name |> String.equal "Caller")
  |> Semantic_result.function_conditions

let top_conditions ~mode ~path source =
  let prepared =
    Test_function_call_conversion_policy.prepare ~mode ~path source
  in
  let _, _, _, results = Test_top_level_expression_result.analyze prepared in
  match Holyc_lib.collect_top_level_conditions prepared.session results with
  | Ok conditions -> Top_level_condition.conditions conditions
  | Error error -> Alcotest.fail (Top_level_condition.error_to_string error)

let lower ?(instruction = 10) ?(value = 20) ?(target = 3) condition =
  Condition.lower_function_condition
    ~instruction_id:(instruction_id instruction)
    ~value_id:(value_id value) ~target:(block_id target) condition
  |> require_ok show_sequence_errors

let lower_top ?(instruction = 10) ?(value = 20) ?(target = 3) condition =
  Condition.lower_top_level_condition
    ~instruction_id:(instruction_id instruction)
    ~value_id:(value_id value) ~target:(block_id target) condition
  |> require_ok show_sequence_errors

let require_lowered = function
  | Condition.Lowered lowered -> lowered
  | Condition.Unsupported_expression ->
      Alcotest.fail "expected a supported checked condition"

let descriptions lowered =
  lowered |> Condition.sequence |> Sequence.instructions
  |> List.map Sequence.description

let opcode_names lowered =
  descriptions lowered
  |> List.map (fun (description : Sequence.description) ->
      Opcode.to_source_name description.opcode)

let all_condition_roles_lower_in_both_modes () =
  let source =
    "I64 Caller(){if(1+2);while(3.0);do ;while(4);for(;5;);return 0;}"
  in
  List.iter
    (fun mode ->
      let checked = conditions ~mode ~path:"ir-condition-roles.HC" source in
      Alcotest.(check int) "four checked conditions" 4 (List.length checked);
      let lowered =
        checked
        |> List.mapi (fun index condition ->
            lower
              ~instruction:(10 + (index * 10))
              ~value:(20 + (index * 10))
              ~target:(30 + index) condition
            |> require_lowered)
      in
      Alcotest.(check (list (list string)))
        "condition expression and branch order"
        [
          [ "IC_IMM_I64"; "IC_IMM_I64"; "IC_ADD"; "IC_BR_ZERO" ];
          [ "IC_IMM_F64"; "IC_BR_ZERO" ];
          [ "IC_IMM_I64"; "IC_BR_NOT_ZERO" ];
          [ "IC_IMM_I64"; "IC_BR_ZERO" ];
        ]
        (List.map opcode_names lowered);
      Alcotest.(check (list int))
        "caller-owned targets are retained" [ 30; 31; 32; 33 ]
        (List.map
           (fun lowered ->
             lowered |> Condition.target |> Sequence.Block_id.to_int)
           lowered);
      List.iter
        (fun lowered ->
          let items = descriptions lowered in
          let branch = List.nth items (List.length items - 1) in
          Alcotest.(check (list int))
            "branch consumes the final condition value"
            [ Condition.condition_value lowered |> Sequence.Value_id.to_int ]
            (List.map Sequence.Value_id.to_int branch.operands);
          Alcotest.(check int64) "branch has no flags" 0L branch.flags;
          Alcotest.(check bool)
            "branch keeps a source span" true
            (Option.is_some branch.span))
        lowered)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let deterministic_dump_records_branch_identity_and_target () =
  let condition =
    conditions ~mode:Preprocessor.Jit ~path:"ir-condition-dump.HC"
      "I64 Caller(){do ;while((1+2));return 0;}"
    |> List.hd
  in
  let lower_once () =
    lower ~instruction:70 ~value:90 ~target:12 condition |> require_lowered
  in
  let first = lower_once () in
  let dump = Condition.human first in
  Alcotest.(check string)
    "condition replay is deterministic" dump
    (Condition.human (lower_once ()));
  let expected_branch =
    "!i73 IC_BR_NOT_ZERO %v92 block:^b12 flags=0x000000000"
    ^ " @source=0:13..30"
  in
  Alcotest.(check string)
    "dump records the do-while back edge" expected_branch
    (List.nth (String.split_on_char '\n' dump) 5);
  Alcotest.(check string)
    "dump records result and next identities"
    "value=%v92 value-type=internal:I64 branch=!i73 target=^b12 \
     next-instruction=74 next-value=93"
    (List.nth (String.split_on_char '\n' dump) 1)

let unsupported_conditions_and_exhaustion_return_no_partial_sequence () =
  let unsupported =
    conditions ~mode:Preprocessor.Jit ~path:"ir-unsupported-condition.HC"
      "I64 Caller(I64 flag){if(flag);return 0;}"
    |> List.hd
  in
  (match lower unsupported with
  | Condition.Unsupported_expression -> ()
  | Condition.Lowered _ ->
      Alcotest.fail "unsupported identifier condition returned partial IR");
  let literal =
    conditions ~mode:Preprocessor.Jit ~path:"ir-condition-exhaustion.HC"
      "I64 Caller(){if(1);return 0;}"
    |> List.hd
  in
  match
    Condition.lower_function_condition
      ~instruction_id:(instruction_id (Int.max_int - 1))
      ~value_id:(value_id 20) ~target:(block_id 3) literal
  with
  | Error [ (error : Sequence.error) ] ->
      Alcotest.(check string) "exhaustion code" "HCIRL0005" error.code;
      Alcotest.(check (option int))
        "no branch identity is exposed" None error.instruction_id
  | Error errors ->
      Alcotest.failf "expected one exhaustion error, got %d"
        (List.length errors)
  | Ok _ -> Alcotest.fail "condition branch identity exhaustion was accepted"

let all_top_level_condition_roles_lower_in_both_modes () =
  let source = "if(1+2);while(3.0);do ;while(4);for(;5;);" in
  List.iter
    (fun mode ->
      let checked =
        top_conditions ~mode ~path:"ir-top-condition-roles.HC" source
      in
      let count = List.length checked in
      Alcotest.(check int) "four top-level conditions" 4 count;
      let lowered =
        checked
        |> List.mapi (fun index condition ->
            lower_top
              ~instruction:(10 + (index * 10))
              ~value:(20 + (index * 10))
              ~target:(30 + index) condition
            |> require_lowered)
      in
      Alcotest.(check (list (list string)))
        "top-level expression and branch order"
        [
          [ "IC_IMM_I64"; "IC_IMM_I64"; "IC_ADD"; "IC_BR_ZERO" ];
          [ "IC_IMM_F64"; "IC_BR_ZERO" ];
          [ "IC_IMM_I64"; "IC_BR_NOT_ZERO" ];
          [ "IC_IMM_I64"; "IC_BR_ZERO" ];
        ]
        (List.map opcode_names lowered);
      Alcotest.(check (list int))
        "top-level caller targets are retained" [ 30; 31; 32; 33 ]
        (List.map
           (fun lowered ->
             lowered |> Condition.target |> Sequence.Block_id.to_int)
           lowered);
      List.iter
        (fun lowered ->
          let items = descriptions lowered in
          let branch = List.nth items (List.length items - 1) in
          Alcotest.(check (list int))
            "top-level branch consumes final condition value"
            [ Condition.condition_value lowered |> Sequence.Value_id.to_int ]
            (List.map Sequence.Value_id.to_int branch.operands);
          let flags = branch.flags in
          Alcotest.(check int64) "top-level branch has no flags" 0L flags;
          Alcotest.(check bool)
            "top-level branch keeps expression span" true
            (Option.is_some branch.span))
        lowered)
    [ Preprocessor.Jit; Preprocessor.Aot ]

let deterministic_top_level_dump_records_branch () =
  let source = "do ;while((1+2));" in
  let condition =
    top_conditions ~mode:Preprocessor.Jit ~path:"ir-top-condition-dump.HC"
      source
    |> List.hd
  in
  let lower_once () =
    let result = lower_top ~instruction:70 ~value:90 ~target:12 condition in
    require_lowered result
  in
  let first = lower_once () in
  let dump = Condition.human first in
  Alcotest.(check string)
    "top-level replay is deterministic" dump
    (Condition.human (lower_once ()));
  let expected_branch =
    "!i73 IC_BR_NOT_ZERO %v92 block:^b12 flags=0x000000000"
    ^ " @source=0:10..15"
  in
  Alcotest.(check string)
    "dump records the top-level back edge" expected_branch
    (List.nth (String.split_on_char '\n' dump) 5)

let unsupported_top_level_condition_and_exhaustion () =
  let unsupported =
    top_conditions ~mode:Preprocessor.Jit
      ~path:"ir-top-unsupported-condition.HC" "I64 flag;if(flag);"
    |> List.hd
  in
  (match lower_top unsupported with
  | Condition.Unsupported_expression -> ()
  | Condition.Lowered _ ->
      Alcotest.fail "unsupported top-level condition returned partial IR");
  let path = "ir-top-condition-exhaustion.HC" in
  let checked = top_conditions ~mode:Preprocessor.Jit ~path "if(1);" in
  let literal = List.hd checked in
  match
    Condition.lower_top_level_condition
      ~instruction_id:(instruction_id (Int.max_int - 1))
      ~value_id:(value_id 20) ~target:(block_id 3) literal
  with
  | Error [ (error : Sequence.error) ] ->
      let code = error.code in
      Alcotest.(check string) "top-level exhaustion code" "HCIRL0005" code;
      Alcotest.(check (option int))
        "no top-level branch identity is exposed" None error.instruction_id
  | Error errors ->
      Alcotest.failf "expected one top-level exhaustion error, got %d"
        (List.length errors)
  | Ok _ -> Alcotest.fail "top-level branch identity exhaustion was accepted"

let tests =
  [
    Alcotest.test_case "all condition roles in JIT and AOT" `Quick
      all_condition_roles_lower_in_both_modes;
    Alcotest.test_case "deterministic condition dump" `Quick
      deterministic_dump_records_branch_identity_and_target;
    Alcotest.test_case "unsupported condition and exhaustion" `Quick
      unsupported_conditions_and_exhaustion_return_no_partial_sequence;
    Alcotest.test_case "all top-level condition roles in JIT and AOT" `Quick
      all_top_level_condition_roles_lower_in_both_modes;
    Alcotest.test_case "deterministic top-level condition dump" `Quick
      deterministic_top_level_dump_records_branch;
    Alcotest.test_case "unsupported top-level condition and exhaustion" `Quick
      unsupported_top_level_condition_and_exhaustion;
  ]
