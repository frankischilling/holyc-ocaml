open Holyc_lib
module Expression = Ir_expression_lowering
module Sequence = Ir_instruction_sequence
module Opcode = Ir_opcode
module Helpers = Test_ir_expression_lowering
module VM = Ir_integer_interpreter

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let root ?(mode = Preprocessor.Jit) text =
  Helpers.top_level_roots ~mode ~path:"chain.hc" text |> List.hd

let lower ?mode text =
  root ?mode text |> Helpers.lower |> Helpers.require_lowered

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

let forwarded_classes () =
  (* OptPass012.HC:141-150,809-822 carries the unsigned comparison class
     through each shared operand, even when COM's result is stored as I64. *)
  List.iter
    (fun mode ->
      List.iter
        (fun (text, bits) ->
          Test_integer_expression.expect_word ~mode text VM.I64 bits ())
        [
          ("(~0x8000000000000000)>0>-1;", 0L);
          ("(~0x8000000000000000)>0<-1;", 1L);
          ("0<(~0x8000000000000000)>0>-1;", 0L);
          ("0<1<(~0x8000000000000000)>0>-1;", 0L);
          ("0<1<(~0x8000000000000000)>0<-1;", 1L);
          ("((+(~0x8000000000000000)))>0>-1;", 0L);
          ("(~(~0x8000000000000000))>0>-1;", 0L);
          ("((~0x8000000000000000)>0)>-1;", 1L);
          ("-(~0x8000000000000000)<0>-1;", 1L);
          ("((~0x8000000000000000)>0>-1)-2;", -2L);
          ("(~0x8000000000000000)>(-7/2)<0x8000000000000000;", 0L);
        ])
    modes

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

let forwarded_shared_operand () =
  let internal primitive (description : Sequence.description) =
    match description.target_type with
    | Some type_ -> (
        Semantic_type.pointer_depth type_ = 0
        &&
        match Semantic_type.base type_ with
        | Semantic_type.Primitive (Semantic_type.Internal_storage, actual) ->
            Primitive_type.equal primitive actual
        | _ -> false)
    | None -> false
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (text, expected_views, steps, bits) ->
          let lowered = lower ~mode text in
          let items = Helpers.descriptions lowered in
          let with_opcode opcode =
            List.filter
              (fun (d : Sequence.description) -> d.opcode = opcode)
              items
          in
          let views = with_opcode Opcode.Ic_holyc_typecast in
          Alcotest.(check int)
            (text ^ " word views") expected_views (List.length views);
          let complements = with_opcode Opcode.Ic_com in
          Alcotest.(check int) "COM is emitted once" 1 (List.length complements);
          Alcotest.(check bool)
            "COM retains its I64 result" true
            (internal Primitive_type.I64 (List.hd complements));
          let comparisons =
            List.filter
              (fun (d : Sequence.description) ->
                d.opcode = Opcode.Ic_less || d.opcode = Opcode.Ic_greater)
              items
          in
          Alcotest.(check int)
            "two adjacent comparisons" 2 (List.length comparisons);
          List.iter
            (fun description ->
              Alcotest.(check bool)
                "comparison and conjunction results remain I64" true
                (internal Primitive_type.I64 description))
            (comparisons @ with_opcode Opcode.Ic_and_and);
          let first = List.hd comparisons and second = List.nth comparisons 1 in
          let shared = List.nth first.operands 1 in
          let reused = List.hd second.operands in
          (match views with
          | [] ->
              Alcotest.(check bool)
                "already forwarded middle is reused directly" true
                (Sequence.Value_id.equal shared reused)
          | [ view ] ->
              Alcotest.(check bool)
                "shared word view is internal U64" true
                (internal Primitive_type.U64 view);
              Alcotest.(check int64) "word view has no flags" 0L view.flags;
              Alcotest.(check bool)
                "word view has the synthetic zero payload" true
                (view.payload = Some (Sequence.Integer 0L));
              Alcotest.(check (list int))
                "word view reuses the original middle"
                [ Sequence.Value_id.to_int shared ]
                (List.map Sequence.Value_id.to_int view.operands);
              Alcotest.(check bool)
                "next comparison consumes the shared view" true
                (Sequence.Value_id.equal reused
                   (Option.get view.result).value_id);
              Alcotest.(check bool)
                "view keeps the next comparison's source span" true
                (view.span = second.span)
          | _ -> Alcotest.fail "unexpected extra comparison views");
          ignore (Helpers.verify_x87 lowered);
          let result =
            Test_integer_expression.evaluate ~mode ~max_steps:steps text
            |> Test_integer_expression.checked
          in
          let word = Test_integer_expression.word result in
          Alcotest.(check bool) "chain returns I64" true (word.type_ = VM.I64);
          Alcotest.(check int64) "chain result" bits word.bits;
          Alcotest.(check int)
            "exact comparison work" steps (VM.executed_steps result);
          match
            Test_integer_expression.evaluate ~mode ~max_steps:(steps - 1) text
          with
          | Error [ error ] ->
              Alcotest.(check string)
                "one below the required work" "HCIRVM0007" error.code
          | _ -> Alcotest.fail "comparison work limit must prevent a result")
        [
          ("(~0x8000000000000000)>0>-1;", 1, 11, 0L);
          ("0<(~0x8000000000000000)>0;", 0, 9, 1L);
        ])
    modes

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
    (fun mode ->
      List.iter
        (fun text ->
          (match root ~mode text |> Helpers.lower with
          | Expression.Unsupported_expression -> ()
          | Expression.Lowered _ ->
              Alcotest.fail
                ("multiple pending comparison reductions were accepted: " ^ text));
          match Test_integer_expression.evaluate ~mode text with
          | Error [ error ] ->
              Alcotest.(check string)
                "public driver retains the lowering boundary" "HCEVAL0002"
                error.code
          | _ -> Alcotest.fail "multiple pending comparisons must not execute")
        [ "1==2<3==1;"; "1==2<3<4==1;"; "1!=2>=3!=1;" ];
      List.iter
        (fun (text, bits) ->
          Test_integer_expression.expect_word ~mode text VM.I64 bits ())
        [ ("1==(2<3)==1;", 1L); ("(1==2<3)==1;", 1L); ("1<2==2<3;", 0L) ])
    modes

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
    Alcotest.test_case "forwarded comparison classes in JIT and AOT" `Quick
      forwarded_classes;
    Alcotest.test_case "shared operand, source spans and exact budget" `Quick
      shared_operand;
    Alcotest.test_case "forwarded shared operands and exact work" `Quick
      forwarded_shared_operand;
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
