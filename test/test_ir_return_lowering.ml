module Return = Holyc_lib.Ir_return_lowering
module Sequence = Holyc_lib.Ir_instruction_sequence
module Opcode = Holyc_lib.Ir_opcode
module Preprocessor = Holyc_lib.Preprocessor
module Semantic_result = Holyc_lib.Semantic_function_call_expression_result
module Test_results = Test_function_call_expression_result

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

let returns ~mode ~path source name =
  let prepared = Test_results.prepare ~mode ~path source in
  let _, results = Test_results.analyze prepared in
  Test_results.returns_named results name

let lower ?(instruction = 10) ?(value = 20) ?(leave = 3) return_ =
  Return.lower_function_return
    ~instruction_id:(instruction_id instruction)
    ~value_id:(value_id value) ~leave:(block_id leave) return_
  |> require_ok show_sequence_errors

let require_lowered = function
  | Return.Lowered lowered -> lowered
  | Return.Unsupported_expression ->
      Alcotest.fail "expected a supported checked return"

let descriptions lowered =
  lowered |> Return.sequence |> Sequence.instructions
  |> List.map Sequence.description

let opcode_names lowered =
  descriptions lowered
  |> List.map (fun (description : Sequence.description) ->
      Opcode.to_source_name description.opcode)

let check_int message expected actual =
  Alcotest.(check int) message expected actual

let check_int64 message expected actual =
  Alcotest.(check int64) message expected actual

let check_string message expected actual =
  Alcotest.(check string) message expected actual

let check_strings message expected actual =
  Alcotest.(check (list string)) message expected actual

let check_optional_int message expected actual =
  Alcotest.(check (option int)) message expected actual

let check_bool message expected actual =
  Alcotest.(check bool) message expected actual

let block_payload = function
  | Some (Sequence.Block target) -> Sequence.Block_id.to_int target
  | _ -> Alcotest.fail "instruction has no block target"

type valued_expectation = {
  lowered : Return.t;
  opcodes : string list;
  conversion_flags : int64;
  target : string;
  leave : int;
}

let check_valued_return expectation =
  let lowered = expectation.lowered in
  check_strings "value, return, and jump order" expectation.opcodes
    (opcode_names lowered);
  let items = descriptions lowered in
  let producer = List.nth items 0 in
  let return_instruction = List.nth items 1 in
  let jump = List.nth items 2 in
  check_int64 "root conversion is retained" expectation.conversion_flags
    producer.flags;
  let target_type = Option.get return_instruction.target_type in
  check_string "declared return target is retained" expectation.target
    (Sequence.type_name target_type);
  let return_value = Return.return_value lowered |> Option.get in
  let expected_operands = [ Sequence.Value_id.to_int return_value ] in
  let actual_operands =
    List.map Sequence.Value_id.to_int return_instruction.operands
  in
  Alcotest.(check (list int))
    "return consumes the final value" expected_operands actual_operands;
  check_int "jump keeps the leave target" expectation.leave
    (block_payload jump.payload);
  check_int64 "return instruction has no flags" 0L return_instruction.flags;
  check_int64 "return jump has no flags" 0L jump.flags;
  let spans_are_present =
    Option.is_some return_instruction.span && Option.is_some jump.span
  in
  check_bool "return instructions keep the statement span" true
    spans_are_present

let valued_returns_keep_target_and_conversion () =
  let source = "F64 ToF64(){return 1;} I64 ToI64(){return 2.0;}" in
  let path = "ir-return-values.HC" in
  List.iter
    (fun mode ->
      let parsed_f64 = returns ~mode ~path source "ToF64" in
      let checked_f64 = List.hd parsed_f64 in
      let to_f64 = lower ~leave:30 checked_f64 |> require_lowered in
      let parsed_i64 = returns ~mode ~path source "ToI64" in
      let checked_i64 = List.hd parsed_i64 in
      let lowered_i64 = lower ~instruction:40 ~value:50 ~leave:31 checked_i64 in
      let to_i64 = require_lowered lowered_i64 in
      let f64 =
        {
          lowered = to_f64;
          opcodes = [ "IC_IMM_I64"; "IC_RETURN_VAL"; "IC_JMP" ];
          conversion_flags = 1L;
          target = "public:F64";
          leave = 30;
        }
      in
      let i64 =
        {
          lowered = to_i64;
          opcodes = [ "IC_IMM_F64"; "IC_RETURN_VAL"; "IC_JMP" ];
          conversion_flags = 2L;
          target = "public:I64";
          leave = 31;
        }
      in
      List.iter check_valued_return [ f64; i64 ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let valueless_returns_jump_directly () =
  List.iter
    (fun mode ->
      let source = "I64 Empty(){return;}" in
      let parsed = returns ~mode ~path:"ir-return-empty.HC" source "Empty" in
      let checked = List.hd parsed in
      let lowered = lower ~instruction:70 ~value:80 ~leave:9 checked in
      let return_ = require_lowered lowered in
      check_strings "valueless return has one jump" [ "IC_JMP" ]
        (opcode_names return_);
      let returned_value =
        Return.return_value return_ |> Option.map Sequence.Value_id.to_int
      in
      check_optional_int "valueless return has no value" None returned_value;
      let return_instruction =
        Return.return_id return_ |> Option.map Sequence.Instruction_id.to_int
      in
      check_optional_int "valueless return has no value instruction" None
        return_instruction;
      let jump_id = Return.jump_id return_ |> Sequence.Instruction_id.to_int in
      check_int "jump identity" 70 jump_id;
      let next_instruction =
        Return.next_instruction_id return_ |> Sequence.Instruction_id.to_int
      in
      check_int "next instruction identity" 71 next_instruction;
      let next_value =
        Return.next_value_id return_ |> Sequence.Value_id.to_int
      in
      check_int "value identity is unchanged" 80 next_value;
      let jump = List.hd (descriptions return_) in
      check_int "valueless jump target" 9 (block_payload jump.payload);
      check_bool "valueless jump keeps the return span" true
        (Option.is_some jump.span))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let deterministic_return_dump () =
  let source = "I64 Value(){return 1+2;}" in
  let parsed =
    returns ~mode:Preprocessor.Jit ~path:"ir-return-dump.HC" source "Value"
  in
  let checked = List.hd parsed in
  let lowered = lower ~instruction:70 ~value:90 ~leave:12 checked in
  let return_ = require_lowered lowered in
  let dump = Return.human return_ in
  let replayed =
    returns ~mode:Preprocessor.Jit ~path:"ir-return-dump.HC" source "Value"
  in
  let replayed_checked = List.hd replayed in
  let replayed_lowered =
    lower ~instruction:70 ~value:90 ~leave:12 replayed_checked
  in
  let replay = require_lowered replayed_lowered |> Return.human in
  check_string "return replay is deterministic" dump replay;
  check_string "dump records result identities"
    "value=%v92 return-type=public:I64 return=!i73 jump=!i74 leave=^b12 \
     next-instruction=75 next-value=93"
    (List.nth (String.split_on_char '\n' dump) 1)

let unsupported_return_and_exhaustion () =
  let unsupported =
    returns ~mode:Preprocessor.Jit ~path:"ir-return-unsupported.HC"
      "I64 Bad(I64 value){return value;}" "Bad"
    |> List.hd
  in
  (match lower unsupported with
  | Return.Unsupported_expression -> ()
  | Return.Lowered _ ->
      Alcotest.fail "unsupported return value exposed partial IR");
  let empty =
    returns ~mode:Preprocessor.Jit ~path:"ir-return-exhaustion.HC"
      "I64 Empty(){return;}" "Empty"
    |> List.hd
  in
  let exhausted_start = instruction_id Int.max_int in
  let exhausted =
    Return.lower_function_return ~instruction_id:exhausted_start
      ~value_id:(value_id 20) ~leave:(block_id 3) empty
  in
  (match exhausted with
  | Error [ (error : Sequence.error) ] ->
      check_string "exhaustion code" "HCIRL0005" error.code;
      check_optional_int "no partial identity is exposed" None
        error.instruction_id
  | Error errors ->
      Alcotest.failf "expected one exhaustion error, got %d"
        (List.length errors)
  | Ok _ -> Alcotest.fail "return identity exhaustion was accepted");
  let valued =
    returns ~mode:Preprocessor.Jit ~path:"ir-return-exhaustion.HC"
      "I64 Value(){return 1;}" "Value"
    |> List.hd
  in
  List.iter
    (fun instruction ->
      let start = instruction_id instruction in
      let exhausted =
        Return.lower_function_return ~instruction_id:start
          ~value_id:(value_id 20) ~leave:(block_id 3) valued
      in
      match exhausted with
      | Error [ (error : Sequence.error) ] ->
          check_string "valued exhaustion code" "HCIRL0005" error.code
      | Error errors ->
          Alcotest.failf "expected one valued exhaustion error, got %d"
            (List.length errors)
      | Ok _ -> Alcotest.fail "valued return exhaustion was accepted")
    [ Int.max_int - 2; Int.max_int - 1 ]

let tests =
  [
    Alcotest.test_case "valued return conversions" `Quick
      valued_returns_keep_target_and_conversion;
    Alcotest.test_case "valueless return jump" `Quick
      valueless_returns_jump_directly;
    Alcotest.test_case "deterministic return dump" `Quick
      deterministic_return_dump;
    Alcotest.test_case "unsupported return and exhaustion" `Quick
      unsupported_return_and_exhaustion;
  ]
