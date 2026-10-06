open Holyc_lib
module Native = Native_source_execution
module Image = X86_64_program

let describe errors =
  errors
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let inputs text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-source-anonymous-defaults.hc"
      ~contents:text
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:Preprocessor.Jit () |> checked
  in
  (session, config, source)

let run ?(max_steps = 100_000) ?max_global_bytes ?max_ir_instructions
    ?max_code_bytes ?max_initializer_steps ?max_default_bytes ?max_frame_bytes
    ?max_call_depth ?max_output_bytes ?max_output_work text =
  let session, config, source = inputs text in
  Native.evaluate ?max_global_bytes ?max_ir_instructions ?max_code_bytes
    ?max_initializer_steps ?max_default_bytes ?max_frame_bytes ?max_call_depth
    ?max_output_bytes ?max_output_work session ~config ~source ~max_steps

let value expected report =
  let result = Native.outcome report |> Result.map_error describe |> checked in
  Alcotest.(check int64)
    "actual native value" expected (Option.get result.value.final_value).bits;
  Alcotest.(check int)
    "no interpreter execution" 0
    (Option.get (Native.source_progress report)).runtime.executed_steps;
  List.iter
    (fun (fragment : Native.fragment) ->
      match fragment.native_outcome with
      | Some (Ok (Image.Completed _)) -> ()
      | _ -> Alcotest.fail "original fragment did not execute natively")
    (Native.fragments report)

let diagnostic code report =
  match Native.outcome report with
  | Error errors ->
      Alcotest.(check bool)
        (describe errors) true
        (List.exists (fun (error : Diagnostic.t) -> error.code = code) errors)
  | Ok _ -> Alcotest.fail ("expected " ^ code)

let fault kind report =
  match List.rev (Native.fragments report) with
  | { native_outcome = Some (Ok (Image.Fault reached)); _ } :: _ ->
      Alcotest.(check bool)
        "reached native owning fault" true (reached.kind = kind);
      Alcotest.(check bool)
        "fault retains actual native work" true
        (reached.executed_steps > 0);
      Alcotest.(check int)
        "cumulative fault work" reached.executed_steps
        (Native.executed_steps report)
  | _ -> Alcotest.fail "failure did not reach original native code"

let agrees expected text =
  value expected (run text);
  let session, config, source = inputs text in
  let interpreted =
    run_integer_program_report session ~config ~source ~max_steps:100_000
  in
  let result =
    integer_program_report_outcome interpreted
    |> Result.map_error describe |> checked
  in
  Alcotest.(check int64)
    "independent original IR value" expected
    (Option.get (Ir_integer_interpreter.final_value result.value)).bits

let header_shapes () =
  List.iter (agrees 42L)
    [
      "I64 Seed(){return 40;}I64 F(I64 n){return n+2;}I64 (*p)(I64 \
       n=Seed())=&F;p();";
      "I64 Seed(){return 40;}I64 F(I64 n){return n+2;}I64 Run(){I64 (*p)(I64 \
       n=Seed());p=&F;return p();}Run();";
      "I64 Seed(){return 40;}I64 F(I64 n){return n+2;}I64 Apply(I64 (*p)(I64 \
       n=Seed())){return p();}Apply(&F);";
      "I64 Seed(){return 40;}I64 F(I64 n){return n+2;}I64 Apply(I64 (*p)(I64 \
       n=Seed())=0){p=&F;return p();}Apply();";
      "I64 Seed(){return 40;}I64 F(I64 n){return n+2;}I64 (*p)(I64 \
       n=Seed())[2]={0,&F};p[1]();";
      "I64 Seed(){return 298;}I64 F(U8 n){return n;}I64 (*p)(U8 \
       n=Seed())=&F;p();";
      "I64 Seed(){return 258;}I64 F(I8 n){return n+40;}I64 (*p)(I8 \
       n=Seed())=&F;p();";
      "I64 Seed(){return 40;}I64 F(I64 n,...){return n+argc;}I64 (*p)(I64 \
       n=Seed(),...)=&F;p(,1,2);";
      "I64 Seed(){return 40;}I64 F(I64 a,I64 b){return a+b;}I64 (*p)(;I64 \
       a=Seed(),I64 b=2;)=&F;p();";
      "U0 F(I64 n){}I64 Seed(){return 40;}U0 (*p)(I64 n=Seed())=&F;p();42;";
      "I64 Seed(){return 99;}I64 F(I64 n){return n+2;}I64 (*p)(I64 \
       n=Seed())=&F;p(40);";
      "I64 Seed(){return 40;}I64 F(I64 n){return n+2;}I64 (*p)(I64 \
       n=Seed())=&F;I64 result=p();result;";
      "I64 Seed(){return 40;}I64 F(I64 n){return n+2;}I64 (*p)(I64 \
       n=Seed())=&F;I64 Run(){static I64 result=p();return \
       result;}Run();Run();";
    ];
  let static =
    "I64 Seed(){return 40;}I64 F(I64 n){return n+2;}I64 Run(){static I64 \
     (*p)(I64 n=Seed());p=&F;return p();}Run();"
  in
  diagnostic "HCRUN0001" (run static)

let defaults_call_callbacks () =
  List.iter (agrees 42L)
    [
      "I64 F(){return 42;}I64 (*seed)()=&F;I64 Id(I64 n){return n;}I64 \
       (*p)(I64 n=seed())=&Id;p();";
      "I64 F(){return 42;}I64 (*seed)()[2]={0,&F};I64 Id(I64 n){return n;}I64 \
       (*p)(I64 n=seed[1]())=&Id;p();";
      "I64 F(){return 42;}I64 (*seed)()=&F;I64 Id(I64 n=seed()){return n;}Id();";
      "I64 F(){return 40;}I64 (*seed)()=&F;I64 Id(I64 n){return n+2;}I64 \
       Run(){I64 (*p)(I64 n=seed());p=&Id;return p();}Run();";
    ]

let saved_nested_owners () =
  List.iter (agrees 42L)
    [
      "I64 F(){return 42;}I64 Call(I64 (*q)()){return q();}I64 (*p)(I64 \
       (*q)()=&F)=&Call;p();";
      "I64 F(){return 42;}I64 G(){return 17;}I64 (*q)()=&F;I64 Call(I64 \
       (*r)()){return r();}I64 (*p)(I64 (*r)()=q)=&Call;q=&G;p();";
      "I64 F(){return 42;}I64 G(){return 17;}I64 (*q)()[2]={0,&F};I64 Call(I64 \
       (*r)()){return r();}I64 (*p)(I64 (*r)()=q[1])=&Call;q[1]=&G;p();";
      "I64 F(){return 42;}I64 Call(I64 (*q)()){return q();}I64 Run(){I64 \
       (*p)(I64 (*q)()=&F);p=&Call;return p();}Run();";
      "I64 F(){return 42;}I64 Call(I64 (*q)()){return q();}I64 Apply(I64 \
       (*p)(I64 (*q)()=&F)){return p();}Apply(&Call);";
      "I64 F(){return 42;}I64 Call(I64 (*q)()){return q();}I64 (*p)(I64 \
       (*q)()=&F)=&Call;I64 F(){return 17;}p();";
      "I64 F(){return 40;}I64 Call(I64 (*q)()){return q()+2;}I64 Run(){I64 \
       (*p)(I64 (*q)()=&F);p=&Call;return p();}I64 Old(){return Run();}I64 \
       G(){return 17;}I64 Run(){I64 (*p)(I64 (*q)()=&G);p=&Call;return \
       p();}Old();";
      "I64 Read(I64 (*q)()){return q;}I64 (*p)(I64 (*q)()=42)=&Read;p();";
      "U0 F(){}U0 Call(U0 (*q)()){q();}U0 (*p)(U0 (*q)()=&F)=&Call;p();42;";
      "I64 F(){return 42;}I64 Call(I64 (*q)()){return q();}I64 (*p)(I64 \
       (*q)()=&F)=&Call;I64 result=p();result;";
      "I64 F(){return 42;}I64 Call(I64 (*q)()){return q();}I64 (*p)(I64 \
       (*q)()=&F)=&Call;I64 Run(){static I64 result=p();return \
       result;}Run();Run();";
    ]

let declaration_effects () =
  List.iter (agrees 42L)
    [
      "I64 Count=40;I64 Seed(){return ++Count;}I64 F(I64 n){return n+1;}I64 \
       (*p)(I64 n=Seed())=&F;p();Count=100;p();";
      "I64 Count=40;I64 Seed(){return ++Count;}I64 F(I64 n){return n+1;}I64 \
       Run(){I64 (*p)(I64 n=Seed());p=&F;return p();}Run();Run();";
      "I64 Count=41;I64 Seed(){return ++Count;}I64 Unused(){I64 (*p)(I64 \
       n=Seed());return 0;}Count;";
      "I64 F(){return 42;}I64 G(){return 17;}I64 (*q)()=&G;I64 Call(I64 \
       (*r)()){return r();}I64 Unused(){I64 (*p)(I64 (*r)()=(q=&F));return \
       0;}q();";
      "I64 F(){return 42;}I64 G(){return 17;}I64 (*q)()=&G;I64 Call(I64 \
       (*r)()){return r();}I64 (*p)(I64 (*r)()=(q=&F))=&Call;p(&G);q();";
    ];
  let report =
    run
      "extern U0 PutChars(U64 ch);I64 Seed(){PutChars('D');return 40;}I64 \
       F(I64 n){return n+2;}I64 Run(){I64 (*p)(I64 n=Seed());p=&F;return \
       p();}Run();Run();"
  in
  value 42L report;
  Alcotest.(check string)
    "one original header execution" "D"
    (Native.output_bytes report);
  Alcotest.(check int)
    "one saved word despite repeated calls" 8
    (Native.default_bytes report)

let saved_capture_and_latch () =
  let report =
    run "I64 F(){return 42;}17;I64 Unused(){I64 (*p)(I64 (*q)()=&F);return 0;}"
  in
  value 17L report;
  let defaults =
    Native.fragments report
    |> List.filter (fun (fragment : Native.fragment) ->
        fragment.kind = Native.Default)
  in
  Alcotest.(check int) "one original anonymous capture" 1 (List.length defaults);
  (match (List.hd defaults).native_outcome with
  | Some
      (Ok
         (Image.Completed
            { final_value = None; captured_callback = Some saved; _ })) ->
      let module Saved = Holyc_lib__Ir.Saved_parameter_value in
      Alcotest.(check (option int64))
        "owner has no exported word" None (Saved.word_bits saved);
      let link, _ = Option.get (Saved.callback_source saved) in
      Alcotest.(check string)
        "original captured owner" "F"
        (Holyc_lib__Sema.Symbol.name
           (Holyc_lib__Ir.Retained_function.symbol link))
  | _ ->
      Alcotest.fail "anonymous default did not capture its actual native owner");
  Alcotest.(check int)
    "saved payload charged to header" 8
    (Native.default_bytes report)

let reached_faults () =
  let report =
    run
      "extern U0 PutChars(U64 ch);I64 Seed(){PutChars('D');return 0;}I64 \
       Unused(){I64 (*p)(I64 n=42/Seed());return 0;}PutChars('X');"
  in
  diagnostic "HCIRVM0009" report;
  fault Image.Division_by_zero report;
  Alcotest.(check string)
    "failed header preserves reached output" "D"
    (Native.output_bytes report);
  Alcotest.(check int)
    "failed header publishes no saved payload" 0
    (Native.default_bytes report);
  let report =
    run
      "extern I64 F();I64 Call(I64 (*q)()){return q();}I64 (*p)(I64 \
       (*q)()=&F)=&Call;I64 F(){return 42;}p();"
  in
  diagnostic "HCIRVM0030" report;
  fault Image.Undefined_extern report;
  let report =
    run
      "I64 F(I64 n){return n;}I64 Call(I64 (*q)()){return q();}I64 (*p)(I64 \
       (*q)()=&F)=&Call;p();"
  in
  diagnostic "HCIRVM0014" report;
  fault Image.Callback_signature_mismatch report;
  let report =
    run "I64 Call(I64 (*q)()){return q();}I64 (*p)(I64 (*q)()=42)=&Call;p();"
  in
  diagnostic "HCIRVM0024" report;
  fault Image.Callback_unowned_address report;
  let report =
    run
      "I64 F(){return 42;}I64 Read(I64 (*q)()){return q;}I64 (*p)(I64 \
       (*q)()=&F)=&Read;p();"
  in
  diagnostic "HCIRVM0024" report;
  fault Image.Callback_owned_word_escape report

let argument_order_and_history () =
  let report =
    run
      "extern U0 PutChars(U64 ch);I64 Seed(I64 n){PutChars(n);return n;}I64 \
       F(){return 0;}I64 (*p)(I64 a=Seed(68),I64 \
       b=Seed(69))=&F;p(Seed(65),Seed(66));"
  in
  diagnostic "HCIRVM0014" report;
  fault Image.Callback_signature_mismatch report;
  Alcotest.(check string)
    "defaults precede reverse reached arguments" "DEBA"
    (Native.output_bytes report);
  Alcotest.(check int)
    "both original saved parameters charged" 16
    (Native.default_bytes report);
  let report =
    run
      "extern U0 PutChars(U64 ch);I64 Seed(I64 n){PutChars(n);return n;}I64 \
       (*p)(I64 a=Seed(68),I64 b=Seed(69))=0;p(Seed(65),Seed(66));"
  in
  diagnostic "HCIRVM0024" report;
  fault Image.Callback_unowned_address report;
  Alcotest.(check string)
    "unowned call retains header and argument effects" "DEBA"
    (Native.output_bytes report)

let quotas () =
  let text =
    "I64 Seed(){return 40;}I64 F(I64 n){return n+2;}I64 Run(){I64 (*p)(I64 \
     n=Seed());p=&F;return p();}Run();"
  in
  let report = run ~max_global_bytes:1 text in
  value 42L report;
  let code, ir =
    List.fold_left
      (fun (code, ir) (fragment : Native.fragment) ->
        (code + fragment.image.code_bytes, ir + fragment.image.ir_instructions))
      (0, 0) (Native.fragments report)
  in
  value 42L
    (run
       ~max_steps:(Native.executed_steps report)
       ~max_initializer_steps:(Native.preparation_steps report)
       ~max_default_bytes:8 ~max_code_bytes:code ~max_ir_instructions:ir text);
  diagnostic "HCIRVM0011" (run ~max_default_bytes:7 text);
  diagnostic "HCIRVM0007"
    (run ~max_initializer_steps:(Native.preparation_steps report - 1) text);
  fault Image.Step_limit_exceeded
    (run ~max_steps:(Native.executed_steps report - 1) text);
  diagnostic "HCBACK0005" (run ~max_code_bytes:(code - 1) text);
  diagnostic "HCBACK0001" (run ~max_ir_instructions:(ir - 1) text);
  let output =
    "extern U0 PutChars(U64 ch);I64 Seed(){PutChars('D');PutChars('E');return \
     42;}I64 Id(I64 n){return n;}I64 (*p)(I64 n=Seed())=&Id;p();"
  in
  let report = run output in
  value 42L report;
  value 42L
    (run ~max_output_bytes:2
       ~max_output_work:(Native.output_work report)
       output);
  fault Image.Output_limit_exceeded (run ~max_output_bytes:1 output);
  fault Image.Output_work_limit_exceeded
    (run ~max_output_work:(Native.output_work report - 1) output)

let () =
  Alcotest.run "Native source anonymous defaults"
    [
      ( "original callback headers",
        [
          Alcotest.test_case "live scalar defaults and header shapes" `Quick
            header_shapes;
          Alcotest.test_case "original callback calls inside defaults" `Quick
            defaults_call_callbacks;
          Alcotest.test_case "nested saved owners and historical bodies" `Quick
            saved_nested_owners;
          Alcotest.test_case "once-only declaration-time effects" `Quick
            declaration_effects;
          Alcotest.test_case "actual native capture and command latch" `Quick
            saved_capture_and_latch;
          Alcotest.test_case "reached original native faults" `Quick
            reached_faults;
          Alcotest.test_case "reverse arguments and failed targets" `Quick
            argument_order_and_history;
          Alcotest.test_case "exact and one-below cumulative quotas" `Quick
            quotas;
        ] );
    ]
