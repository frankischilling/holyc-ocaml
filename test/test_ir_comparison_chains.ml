open Holyc_lib
module Expression = Ir_expression_lowering
module Sequence = Ir_instruction_sequence
module Opcode = Ir_opcode
module Helpers = Test_ir_expression_lowering
module VM = Ir_integer_interpreter

let root text =
  Helpers.top_level_roots ~mode:Preprocessor.Jit ~path:"chain.hc" text
  |> List.hd

let lower text = root text |> Helpers.lower |> Helpers.require_lowered

let values () =
  List.iter
    (fun (text, bits) ->
      Test_integer_expression.expect_word text VM.I64 bits ())
    [
      ("2==2==2;", 1L);
      ("3<2<1;", 0L);
      ("5>4>3>2;", 1L);
      ("2<=2<=3;", 1L);
      ("3>=3>=2;", 1L);
      ("1!=2!=3;", 1L);
      ("1<2==2;", 1L);
      ("1<2==2<3;", 0L);
      ("(3<2)<1;", 1L);
      ("0==1<2;", 0L);
      ("1<(2<3);", 0L);
      ("(2==2==2)+41;", 42L);
      ("1==(2<3)==1;", 1L);
      ("(1==2<3)==1;", 1L);
      ("0xFFFFFFFFFFFFFFFF>0>-1;", 0L);
      ("0xFFFFFFFFFFFFFFFF>0==0>-1;", 0L);
      ("0xFFFFFFFFFFFFFFFF>0<-1;", 1L);
      ("0xFFFFFFFFFFFFFFFF>(-7/2)>-1;", 0L);
      ("0xFFFFFFFFFFFFFFFF>(-7/2)<0x8000000000000000;", 0L);
      ("0<0xFFFFFFFFFFFFFFFF>1;", 1L);
      ("-2<-1<0;", 1L);
    ]

let shared_operand () =
  let text = "1<(8/4)<3;" in
  let lowered = lower text in
  let items = Helpers.descriptions lowered in
  let division =
    List.filter
      (fun (d : Sequence.description) -> d.opcode = Opcode.Ic_div)
      items
  in
  Alcotest.(check int)
    "middle expression is emitted once" 1 (List.length division);
  let middle =
    (List.hd division).result |> Option.get |> fun r -> r.Sequence.value_id
  in
  let comparisons =
    List.filter
      (fun (d : Sequence.description) -> d.opcode = Opcode.Ic_less)
      items
  in
  Alcotest.(check int) "two adjacent comparisons" 2 (List.length comparisons);
  Alcotest.(check bool)
    "first comparison uses the middle on its right" true
    (Sequence.Value_id.equal middle (List.nth (List.hd comparisons).operands 1));
  Alcotest.(check bool)
    "second comparison reuses the middle on its left" true
    (Sequence.Value_id.equal middle (List.hd (List.nth comparisons 1).operands));
  let combination =
    List.find
      (fun (d : Sequence.description) -> d.opcode = Opcode.Ic_and_and)
      items
  in
  let comparison_values =
    List.map
      (fun (d : Sequence.description) ->
        d.result |> Option.get |> fun r -> Sequence.Value_id.to_int r.value_id)
      comparisons
  in
  Alcotest.(check (list int))
    "combine the comparison values" comparison_values
    (List.map Sequence.Value_id.to_int combination.operands);
  Alcotest.(check (list int))
    "comparison source positions" [ 1; 7 ]
    (List.map
       (fun (d : Sequence.description) -> (Option.get d.span).start)
       comparisons);
  ignore (Helpers.verify_x87 lowered);
  Alcotest.(check string)
    "deterministic replay" (Expression.human lowered)
    (Expression.human (lower text));
  let result =
    Test_integer_expression.evaluate ~max_steps:10 text
    |> Test_integer_expression.checked
  in
  Alcotest.(check int)
    "one execution of each operand" 10 (VM.executed_steps result);
  Alcotest.(check string)
    "exact budget boundary" "HCIRVM0007"
    (Test_integer_expression.error ~max_steps:9 text).code

let eager_faults () =
  List.iter
    (fun text ->
      Alcotest.(check string)
        text "HCIRVM0009" (Test_integer_expression.error text).code)
    [ "3<2<1/0;"; "1<(8/0)<3;"; "0&&(3<2<1/0);" ];
  let session, config, source = Test_integer_expression.inputs "3<2<1/0;" in
  let graph =
    lower_integer_expression session ~config ~source
    |> Test_integer_expression.checked
  in
  match VM.execute ~max_steps:20 graph with
  | Error [ error ] ->
      Alcotest.(check int) "fault consumes its step" 6 error.executed_steps;
      Alcotest.(check (option int))
        "fault instruction" (Some 5) error.instruction_id;
      Alcotest.(check (option int)) "fault block" (Some 0) error.block_id;
      Alcotest.(check int)
        "fault source position" 5 (Option.get error.span).start
  | _ -> Alcotest.fail "expected one reached division fault"

let program_values_and_conditions () =
  Test_integer_program.succeeds "2==2==2; {1<(8/4)<3;}" ();
  List.iter
    (fun text ->
      Alcotest.(check string)
        text "HCRUN0003" (Test_integer_program.diagnostic text).code)
    [
      "if(2==2==2);"; "while(1<2<3);"; "if((2==2==2)+1);"; "if(0 && (2==2==2));";
    ]

let checked_contexts () =
  List.iter
    (fun mode ->
      Helpers.function_roots ~mode ~path:"chain-function.hc"
        "U0 Target(I64 value); U0 Caller(){ Target((2==2==2)+41); }"
      |> List.iter (fun result ->
          let lowered = Helpers.lower result |> Helpers.require_lowered in
          ignore (Helpers.verify_x87 lowered)))
    [ Preprocessor.Jit; Preprocessor.Aot ];
  ignore (Helpers.verify_x87 (lower "(1<2<3)+0.5;"));
  ignore (Helpers.verify_x87 (lower "(1.0<2)<3;"));
  List.iter
    (fun text ->
      match root text |> Helpers.lower with
      | Expression.Unsupported_expression -> ()
      | Expression.Lowered _ ->
          Alcotest.fail ("floating comparison chain was accepted: " ^ text))
    [ "1.0<2<3;"; "1<2.0<3;"; "1<2<3.0;"; "1.0<2<3<4;" ]

let multiple_pending_comparisons () =
  List.iter
    (fun text ->
      match root text |> Helpers.lower with
      | Expression.Unsupported_expression -> ()
      | Expression.Lowered _ ->
          Alcotest.fail
            ("multiple pending comparison reductions were accepted: " ^ text))
    [ "1==2<3==1;"; "1==2<3<4==1;"; "1!=2>=3!=1;" ]

let chain_resource_boundaries () =
  let text = String.concat "==" (List.init 2001 (fun _ -> "1")) ^ ";" in
  let execution =
    Test_integer_expression.evaluate ~max_steps:6002 text
    |> Test_integer_expression.checked
  in
  let word = Test_integer_expression.word execution in
  Alcotest.(check int64) "long chain value" 1L word.bits;
  Alcotest.(check int)
    "long chain exact budget" 6002
    (VM.executed_steps execution);
  let root = root "1<2<3;" in
  List.iter
    (fun (instruction, value) ->
      match Helpers.lower_result ~instruction ~value root with
      | Error [ error ] ->
          Alcotest.(check string)
            "second link allocation exhausts IDs" "HCIRL0005" error.code
      | _ ->
          Alcotest.fail "identity exhaustion must not publish a partial chain")
    [ (Int.max_int - 5, 0); (0, Int.max_int - 5) ]

let tests =
  [
    Alcotest.test_case "source values and grouping" `Quick values;
    Alcotest.test_case "shared operand, source spans and exact budget" `Quick
      shared_operand;
    Alcotest.test_case "eager reached faults" `Quick eager_faults;
    Alcotest.test_case "program values and conditional boundary" `Quick
      program_values_and_conditions;
    Alcotest.test_case "function contexts, conversions and floating boundary"
      `Quick checked_contexts;
    Alcotest.test_case "multiple pending comparison reductions stay unsupported"
      `Quick multiple_pending_comparisons;
    Alcotest.test_case "long chain and atomic identity exhaustion" `Quick
      chain_resource_boundaries;
  ]
