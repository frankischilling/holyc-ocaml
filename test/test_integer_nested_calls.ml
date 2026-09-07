open Holyc_lib
module F = Test_integer_functions
module VM = Ir_integer_interpreter
module D = Test_ir_direct_call_lowering
module E = Ir_expression_lowering
module Seq = Ir_instruction_sequence
module H = Test_ir_integer_interpreter

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]
let identity = "I64 Id(I64 n){return n;}"

let source =
  "I64 Add(I64 a,I64 b){I64 c=a+b;return c;} I64 Twice(I64 n){return \
   Add(n,n);} I64 Sum(I64 n){I64 \
   total=0;while(n>0){total=Add(total,n);n=n-1;}return total;} \
   (Sum(7)+Twice(7));"

let source_gate () =
  List.iter (fun mode -> ignore (F.run ~mode source |> F.expect 42L)) modes

let contexts () =
  List.iter
    (fun mode ->
      List.iter
        (fun (text, expected) ->
          ignore (F.run ~mode (identity ^ text) |> F.expect expected))
        [
          ("I64 F(){I64 a=Id(20),b=Id(a+2);return a+b;}(F());", 42L);
          ("I64 Sub(I64 a,I64 b){return a-b;}(Sub(Id(20),Id(22)));", -2L);
          ("(Id(40)+Id(2));", 42L);
          ("(Id(10)*(Id(5)-Id(1))+Id(2));", 42L);
          ("I64 F(){I64 a=7;return (a+Id(a=20))+a;}(F());", 47L);
          ( "I64 Sub(I64 a,I64 b){return a-b;}I64 F(){I64 n=0;I64 \
             v=Sub(Id(n=1),Id(n=2));return v*10+n;}(F());",
            -9L );
          ("(Id(Id(Id(42))));", 42L);
          ("(Id(0)+Id(-7)+Id(2));", -5L);
          ("(+Id(42));", 42L);
          ("(~Id(0));", -1L);
          ("I64 F(){return Id(42);return Id(1/0);}(F());", 42L);
        ])
    modes

let control_flow () =
  List.iter
    (fun mode ->
      List.iter
        (fun (body, expected) ->
          ignore
            (F.run ~mode (identity ^ "I64 F(){I64 n=0;" ^ body ^ "}(F());")
            |> F.expect expected))
        [
          ("while(Id(n)<5){n=n+1;if(Id(n)==3)break;}return n;", 3L);
          ("for(;Id(n)<5;n=Id(n+1)){if(n==3)return Id(n+39);}return 0;", 42L);
          ("do{n=Id(n+1);}while(Id(n)<3);return n;", 3L);
          ("if(Id(0)&&Id(n=1))return 9;if(Id(1)||Id(n=2)){}return n;", 0L);
          ("I64 b=Id(0)&&Id(n=1);return n;", 1L);
          ("I64 c=0<Id(n=n+1)<3;return n*10+c;", 11L);
          ("if(Id(0<Id(n=n+1)<3))return n+41;return 0;", 42L);
        ])
    modes

let signedness () =
  List.iter
    (fun mode ->
      let definitions = "U64 U(I64 n){return n;}I64 S(U64 n){return n;}" in
      ignore
        (F.run ~mode (definitions ^ "(U(S(U(-1))));")
        |> F.expect ~type_:VM.U64 (-1L));
      ignore
        (F.run ~mode (definitions ^ "(-U(7));") |> F.expect ~type_:VM.U64 (-7L));
      ignore
        (F.run ~mode
           (definitions ^ "I64 F(){U64 a=U(-1);I64 b=S(a);return b;}(F());")
        |> F.expect (-1L));
      ignore (F.run ~mode (definitions ^ "(U(-1)>0>-1);") |> F.expect 0L))
    modes

let recursion_and_limits () =
  let text =
    "I64 Fact(I64 n){if(n<=1)return 1;return n*Fact(n-1);}(Fact(5));"
  in
  List.iter
    (fun mode ->
      let result =
        F.run ~mode ~max_call_depth:5 ~max_frame_bytes:40 text |> F.expect 120L
      in
      let steps = VM.executed_steps result in
      ignore (F.run ~mode ~max_steps:steps text |> F.expect 120L);
      List.iter
        (fun (result, code) ->
          Alcotest.(check string)
            "shared bound" code (F.first_error result).code)
        [
          (F.run ~mode ~max_steps:(steps - 1) text, "HCIRVM0007");
          (F.run ~mode ~max_call_depth:4 text, "HCIRVM0015");
          (F.run ~mode ~max_frame_bytes:39 text, "HCIRVM0011");
        ];
      ignore
        (F.run ~mode
           "I64 Fib(I64 n){if(n<=1)return n;return Fib(n-1)+Fib(n-2);}(Fib(8));"
        |> F.expect 21L);
      let dump () =
        let session, config, source = F.inputs ~mode text in
        (F.checked (compile_integer_program session ~config ~source)).value
        |> integer_program_human
      in
      Alcotest.(check string)
        "deterministic nested-call graph" (dump ()) (dump ()))
    modes

let faults () =
  List.iter
    (fun mode ->
      let text = identity ^ "I64 Fail(){return 1/0;}(Id(Fail()));" in
      let error = F.first_error (F.run ~mode text) in
      Alcotest.(check string) "nested callee fault" "HCIRVM0009" error.code;
      Alcotest.(check int)
        "exact fault operator" (String.index text '/') error.primary.start;
      Alcotest.(check bool)
        "nested owner" true
        (List.mem "function=Fail" error.notes);
      Alcotest.(check bool)
        "execution phase" true
        (List.mem "stage=execution" error.notes);
      let definitions = identity ^ "I64 Fail(){return 1/0;}" in
      ignore
        (F.run ~mode (definitions ^ "if(0&&Id(Fail())){}42;") |> F.expect 42L);
      Alcotest.(check string)
        "ordinary logical values remain eager" "HCIRVM0009"
        (F.first_error (F.run ~mode (definitions ^ "(0&&Id(Fail()));"))).code;
      Alcotest.(check string)
        "independent nested frames" "HCIRVM0012"
        (F.first_error
           (F.run ~mode
              (identity
             ^ "I64 F(I64 set){I64 a;if(set)a=42;return a;}(Id(F(1))+Id(F(0)));"
              )))
          .code;
      let error =
        F.first_error
          (F.run ~mode (identity ^ "I64 Bad(){1.0;return Id(1);}(Id(42));"))
      in
      Alcotest.(check bool)
        "unused definition preflight" true
        (List.mem "executed_steps=0" error.notes))
    modes

let unsupported () =
  List.iter
    (fun mode ->
      List.iter
        (fun text -> ignore (F.first_error (F.run ~mode text)))
        [
          "I64 F(I64 n=1){return n;}(1+F());";
          "I64 F(I64 n,...){return n;}(1+F(2,3));";
          "extern I64 F();(1+F());";
          "I64 F(I64 *n){return 1;}(1+F(0));";
          "F64 F(){return 1.0;}(F()+1.0);";
          identity ^ "if(0<Id(1)<3)42;";
          identity ^ "(Id(1==2<3==1));";
        ])
    modes

let callback_boundary () =
  List.iter
    (fun mode ->
      let results, records =
        D.prepared mode "I64 Leaf(){return 7;}I64 Caller(){return Leaf();}"
        |> D.analyze
      in
      let target = D.target records (D.direct_call results "Caller") in
      let root = D.return_root results "Caller" in
      let provider ~instruction_id ~value_id result =
        Ir_direct_call_lowering.lower ~instruction_id ~value_id ~target result
        |> Result.map (function
          | Ir_direct_call_lowering.Lowered result ->
              Some (Ir_direct_call_lowering.sequence result)
          | Ir_direct_call_lowering.Unsupported_call -> None)
      in
      let lower ?lower_call () =
        E.lower_typed_result ?lower_call ~instruction_id:(D.instruction_id 10)
          ~value_id:(D.value_id 20) root
      in
      let result =
        lower ~lower_call:provider ()
        |> H.require_ok (fun _ -> "call expression failed")
      in
      (match result with
      | E.Lowered result ->
          Alcotest.(check int)
            "call expression result" 20
            (E.result_value result |> Seq.Value_id.to_int)
      | E.Unsupported_expression -> Alcotest.fail "call expression unsupported");
      List.iter
        (fun result ->
          match result with
          | Ok E.Unsupported_expression -> ()
          | _ -> Alcotest.fail "expected atomic unsupported call")
        [
          lower ();
          lower ~lower_call:(fun ~instruction_id:_ ~value_id:_ _ -> Ok None) ();
        ];
      let mutate change ~instruction_id ~value_id result =
        Result.bind (provider ~instruction_id ~value_id result) (function
          | None -> Ok None
          | Some sequence ->
              Seq.create
                (Seq.instructions sequence |> List.map Seq.description |> change)
              |> Result.map Option.some)
      in
      let last change items =
        match List.rev items with
        | item :: rest -> List.rev (change item :: rest)
        | [] -> assert false
      in
      let bad_providers =
        [
          (fun ~instruction_id:_ ~value_id result ->
            provider ~instruction_id:(D.instruction_id 11) ~value_id result);
          (fun ~instruction_id ~value_id:_ result ->
            provider ~instruction_id ~value_id:(D.value_id 21) result);
          mutate
            (last (fun item ->
                 { item with Seq.target_type = Some H.public_u64 }));
          mutate (last (fun item -> { item with Seq.span = None }));
          mutate (last (fun item -> { item with Seq.flags = 0x2000L }));
          (fun ~instruction_id:_ ~value_id:_ _ -> Error []);
        ]
      in
      List.iter
        (fun provider ->
          match lower ~lower_call:provider () with
          | Error errors ->
              Alcotest.(check bool)
                "invalid fragment evidence" true
                (List.exists
                   (fun (error : Seq.error) -> error.code = "HCIRL0004")
                   errors)
          | _ -> Alcotest.fail "invalid call fragment was published")
        bad_providers)
    modes

let callback_conversion () =
  List.iter
    (fun mode ->
      let results, records =
        D.prepared mode "I64 Leaf(){return 7;}F64 Caller(){return Leaf()+1.0;}"
        |> D.analyze
      in
      let target = D.target records (D.direct_call results "Caller") in
      let lower_call ~instruction_id ~value_id result =
        Ir_direct_call_lowering.lower ~instruction_id ~value_id ~target result
        |> Result.map (function
          | Ir_direct_call_lowering.Lowered result ->
              Some (Ir_direct_call_lowering.sequence result)
          | Ir_direct_call_lowering.Unsupported_call -> None)
      in
      match
        E.lower_typed_result ~lower_call ~instruction_id:(D.instruction_id 0)
          ~value_id:(D.value_id 0)
          (D.return_root results "Caller")
      with
      | Ok (E.Lowered expression) ->
          let items =
            E.sequence expression |> Seq.instructions
            |> List.map Seq.description
          in
          let call_end =
            List.find
              (fun item -> item.Seq.opcode = Ir_opcode.Ic_call_end)
              items
          in
          Alcotest.(check int64)
            "conversion on returned producer" 1L call_end.flags;
          List.iter
            (fun item ->
              if
                List.mem item.Seq.opcode
                  [ Ir_opcode.Ic_call_start; Ic_call; Ic_add_rsp; Ic_add_rsp1 ]
              then
                Alcotest.(check int64)
                  "call protocol has no result conversion" 0L item.flags)
            items;
          Alcotest.(check string)
            "parent keeps floating result" "internal:F64"
            (E.result_type expression |> Seq.type_name)
      | _ -> Alcotest.fail "converted call expression did not lower")
    modes

let tests =
  [
    Alcotest.test_case "source nested call gate" `Quick source_gate;
    Alcotest.test_case "value contexts and argument order" `Quick contexts;
    Alcotest.test_case "conditions, loops and shared operands" `Quick
      control_flow;
    Alcotest.test_case "public word classes" `Quick signedness;
    Alcotest.test_case "returned recursion and shared limits" `Quick
      recursion_and_limits;
    Alcotest.test_case "nested fault provenance and preflight" `Quick faults;
    Alcotest.test_case "remaining unsupported domains" `Quick unsupported;
    Alcotest.test_case "checked callback boundary" `Quick callback_boundary;
    Alcotest.test_case "conversion on call result" `Quick callback_conversion;
  ]
