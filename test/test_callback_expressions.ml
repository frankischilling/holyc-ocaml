open Holyc_lib
module Cases = Callback_expression_cases
module F = Test_integer_functions
module G = Test_integer_globals
module R = Semantic_function_call_expression_result

let words () =
  List.iter
    (fun mode ->
      List.iter
        (fun (label, text, expected) ->
          ignore label;
          let type_ =
            if label = "unsigned division" || label = "unsigned shift" then
              Ir_integer_interpreter.U64
            else Ir_integer_interpreter.I64
          in
          ignore (G.run ~mode text |> F.expect ~type_ expected))
        Cases.cases)
    G.modes;
  List.iter
    (fun (_, text, expected) -> ignore (G.run text |> F.expect expected))
    Cases.jit_cases

let owned_faults () =
  List.iter
    (fun mode ->
      List.iter
        (fun consumer ->
          let text =
            "extern U0 PutChars(U64 ch);I64 F(){return 42;}I64 Run(){I64 \
             (*p)(),(*q)();p=&F;q=0;PutChars('B');return " ^ consumer
            ^ ";}Run();"
          in
          ignore
            (Test_integer_output.run ~mode text
            |> Test_integer_output.fault ~output:"B" "HCIRVM0024"))
        Cases.owned_consumers)
    G.modes

let selected_types () =
  List.iter
    (fun mode ->
      let source =
        Test_callback_storage.prepare mode
          "class Pair{I64 a;I64 b;};I64 Caller(){Pair \
           (*p)();p=40;(p=40)|2;(p|2)+1;return 0;}"
      in
      let _, results = Test_function_call_expression_result.analyze source in
      let values = Test_callback_storage.statements results in
      let assignment = List.hd values in
      let left, _ = Test_callback_storage.assignment assignment in
      Test_callback_storage.check_word_pointer 1 (R.result_storage_type left);
      Test_callback_storage.check_word_pointer 0
        (R.result_computation_type left);
      Alcotest.(check string)
        "original return class" "Pair"
        (Test_function_call_expression_result.type_name left);
      Alcotest.(check bool)
        "assignment retains exact original destination" true
        (Option.get (R.result_callback_update_operand assignment) == left);
      List.iter
        (fun value ->
          Alcotest.(check bool)
            "original parser class retained" true
            (Option.is_some (R.result_callback_parser_pointer value));
          Test_callback_storage.check_word_pointer 0
            (R.result_computation_type value);
          Alcotest.(check bool)
            "numeric result has no callback signature" true
            (Option.is_none (R.result_callback_pointer value)))
        values;
      let source =
        Test_callback_storage.prepare mode
          "I64 Caller(){I64 x[2];I64 *p=&x[0];p+1;return 0;}"
      in
      let _, results = Test_function_call_expression_result.analyze source in
      List.iter
        (fun value ->
          Alcotest.(check bool)
            "ordinary pointer has no callback provenance" true
            (Option.is_none (R.result_callback_parser_pointer value)))
        (Test_callback_storage.statements results))
    G.modes

let effects_and_faults () =
  List.iter
    (fun mode ->
      let text =
        "extern U0 PutChars(U64 ch);I64 (*p)()[2];I64 Mark(I64 \
         n){PutChars(n);return 1;}I64 Run(){return \
         (p[Mark('L')]=Mark('R')*40)|2;}Run();"
      in
      ignore
        (Test_integer_output.run ~mode text |> Test_integer_output.expect "LR");
      List.iter
        (fun (consumer, code) ->
          ignore
            ( G.run ~mode ("I64 (*p)()=42;" ^ consumer ^ ";") |> F.first_error
            |> fun error ->
              Alcotest.(check string) "reached arithmetic fault" code error.code
            ))
        [ ("p/0", "HCIRVM0009"); ("p%0", "HCIRVM0009") ])
    G.modes

let exact_work () =
  let text =
    "I64 Run(){I64 (*p)(),(*q)();p=370;q=42;return ((p+1)-q)|0;}Run();"
  in
  List.iter
    (fun mode ->
      let baseline = G.run ~mode text |> F.expect 42L in
      let steps = Ir_integer_interpreter.executed_steps baseline in
      ignore (G.run ~mode ~max_steps:steps text |> F.expect 42L);
      Alcotest.(check string)
        "one below actual work" "HCIRVM0007"
        (F.first_error (G.run ~mode ~max_steps:(steps - 1) text)).code)
    G.modes

let tests =
  [
    Alcotest.test_case "numeric operators and all storage shapes" `Quick words;
    Alcotest.test_case "owned words fault at numeric consumers" `Quick
      owned_faults;
    Alcotest.test_case "original parser and computation classes" `Quick
      selected_types;
    Alcotest.test_case "original effects and arithmetic faults" `Quick
      effects_and_faults;
    Alcotest.test_case "exact cumulative numeric work" `Quick exact_work;
  ]
