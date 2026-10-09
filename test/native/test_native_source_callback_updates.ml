open Holyc_lib
module Native = Native_source_execution
module Image = X86_64_program

let inputs text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-callback-updates.hc" ~contents:text
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:Preprocessor.Jit ()
    |> Result.get_ok
  in
  (session, config, source)

let run ?(max_steps = 100_000) ?max_ir_instructions ?max_code_bytes
    ?max_frame_bytes ?max_call_depth text =
  let session, config, source = inputs text in
  Native.evaluate ?max_ir_instructions ?max_code_bytes ?max_frame_bytes
    ?max_call_depth session ~config ~source ~max_steps

let describe errors =
  errors
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let value expected report =
  match Native.outcome report with
  | Error errors -> Alcotest.fail (describe errors)
  | Ok result ->
      Alcotest.(check int64)
        "original update value" expected
        (Option.get result.value.final_value).bits;
      List.iter
        (fun (fragment : Native.fragment) ->
          match fragment.native_outcome with
          | Some (Ok (Image.Completed _)) -> ()
          | _ -> Alcotest.fail "update fragment did not execute natively")
        (Native.fragments report);
      Alcotest.(check int)
        "updates execute no VM instructions" 0
        (Option.get (Native.source_progress report)).runtime.executed_steps

let agrees expected text =
  value expected (run text);
  let session, config, source = inputs text in
  match run_integer_program session ~config ~source ~max_steps:100_000 with
  | Error errors -> Alcotest.fail (describe errors)
  | Ok result ->
      Alcotest.(check int64)
        "independent original IR update" expected
        (Option.get (Ir_integer_interpreter.final_value result.value)).bits

let fault expected report =
  match (Native.outcome report, List.rev (Native.fragments report)) with
  | Error _, { native_outcome = Some (Ok (Image.Fault fault)); _ } :: _ ->
      Alcotest.(check bool)
        "reached original update fault" true (fault.kind = expected);
      Alcotest.(check int)
        "actual cumulative native work"
        (Native.executed_steps report)
        fault.executed_steps
  | Error errors, _ -> Alcotest.fail (describe errors)
  | Ok _, _ -> Alcotest.fail "update should reach its native fault"

let diagnostic expected report =
  match Native.outcome report with
  | Error errors ->
      Alcotest.(check bool)
        expected true
        (List.exists
           (fun (error : Diagnostic.t) -> error.code = expected)
           errors)
  | Ok _ -> Alcotest.fail "expected diagnostic"

let numeric_word_updates () =
  let cases =
    [
      ("+=", 35L, 7L);
      ("-=", 49L, 7L);
      ("*=", 6L, 7L);
      ("/=", 84L, 2L);
      ("%=", 127L, 85L);
      ("&=", 63L, 42L);
      ("|=", 40L, 2L);
      ("^=", 40L, 2L);
      ("<<=", 21L, 1L);
      (">>=", 84L, 1L);
    ]
  in
  List.iter
    (fun (operation, initial, operand) ->
      let right = Printf.sprintf "I64 (*p)();p=%Ld;" operand in
      let update target = target ^ operation ^ "p;" in
      List.iter (agrees 42L)
        [
          Printf.sprintf "I64 Run(){I64 x=%Ld;%s%sreturn x;}Run();" initial
            right (update "x");
          Printf.sprintf "I64 x=%Ld;I64 Run(){%s%sreturn x;}Run();" initial
            right (update "x");
          Printf.sprintf "I64 x[2]={0,%Ld};I64 Run(){%s%sreturn x[1];}Run();"
            initial right (update "x[1]");
          Printf.sprintf "I64 x=%Ld;I64 Run(){I64 *q=&x;%s%sreturn x;}Run();"
            initial right (update "*q");
        ])
    cases;
  List.iter
    (fun (operation, initial, operand) ->
      let initial =
        match operation with
        | "+=" -> -14L
        | "-=" -> 98L
        | _ -> initial
      in
      List.iter (agrees 42L)
        [
          Printf.sprintf
            "I64 Run(){I64 (*p)(),(*q)();p=%Ld;q=%Ld;q%sp;return q;}Run();"
            operand initial operation;
          Printf.sprintf "I64 (*q)()[2]={0,%Ld};I64 (*p)()=%Ld;q[1]%sp;q[1];"
            initial operand operation;
        ])
    cases;
  agrees 2L "I64 Run(){U8 x=254;I64 (*p)();p=4;x+=p;return x;}Run();";
  agrees (-42L) "I64 Run(){I8 x=-84;I64 (*p)();p=2;x/=p;return x;}Run();";
  agrees 0x7fffffffffffffffL
    "I64 Run(){U64 x=0xffffffffffffffff;I64 (*p)();p=2;x/=p;return x;}Run();";
  agrees 42L "I64 Run(){I64 (*p)(),(*q)();p=63;q=42;p&=q;return p;}Run();";
  List.iter (agrees 42L)
    [
      "I64 Run(){I64 x=35;F64 (*p)();p=7;x+=p;return x;}Run();";
      "I64 Run(){I64 x=35;U0 (*p)();p=7;x+=p;return x;}Run();";
      "I64 x=35;I64 (*p)()=7;I64 Read(I64 (*q)()){x+=q;return x;}Read(p);";
      "I64 x=35;I64 (*p)()[2]={0,7};x+=p[1];x;";
    ]

let numeric_update_fault_order () =
  List.iter
    (fun (expected, text) -> fault expected (run text))
    [
      ( Image.Callback_owned_word_escape,
        "I64 F(){return 42;}I64 Run(){I64 x=0;I64 (*p)();p=&F;x+=p;return \
         x;}Run();" );
      ( Image.Uninitialized_read,
        "I64 F(){return 42;}I64 x;I64 Run(){I64 (*p)();p=&F;x+=p;return \
         x;}Run();" );
      ( Image.Callback_owned_word_escape,
        "I64 F(){return 42;}I64 x=0;I64 Run(){I64 (*p)();p=&F;x+=p;return \
         x;}Run();" );
      ( Image.Callback_owned_word_escape,
        "I64 F(){return 42;}I64 x[2]={0,0};I64 Run(){I64 \
         (*p)();p=&F;x[1]+=p;return x[1];}Run();" );
      ( Image.Callback_owned_word_escape,
        "I64 F(){return 42;}I64 x=0;I64 Run(){I64 *q=&x;I64 \
         (*p)();p=&F;*q+=p;return x;}Run();" );
      ( Image.Callback_update_owned_address,
        "I64 F(){return 42;}I64 Run(){I64 (*p)(),(*q)();p=&F;q=&F;p&=q;return \
         42;}Run();" );
      ( Image.Callback_owned_word_escape,
        "I64 F(){return 42;}I64 Run(){I64 (*p)(),(*q)();p=63;q=&F;p&=q;return \
         42;}Run();" );
      ( Image.Callback_owned_word_escape,
        "I64 F(){return 42;}I64 Run(){I64 (*p)(),(*q)();p=0;q=&F;p+=q;return \
         42;}Run();" );
      ( Image.Address_out_of_bounds,
        "I64 F(){return 42;}I64 x[2]={0,0};I64 (*p)()=&F;x[2]+=p;" );
      ( Image.Uninitialized_read,
        "I64 F(){return 42;}I64 x[2];I64 (*p)()=&F;x[1]+=p;" );
      ( Image.Uninitialized_read,
        "I64 F(){return 42;}I64 Run(){I64 x;I64 *q=&x;I64 \
         (*p)();p=&F;*q+=p;return 42;}Run();" );
    ]

let arithmetic_faults () =
  List.iter
    (fun operation ->
      List.iter
        (fun (expected, diagnostic, initial, operand) ->
          List.iter
            (fun text ->
              let report = run text in
              fault expected report;
              let session, config, source = inputs text in
              match
                run_integer_program session ~config ~source ~max_steps:100_000
              with
              | Ok _ -> Alcotest.fail "independent IR missed arithmetic fault"
              | Error errors ->
                  Alcotest.(check bool)
                    "independent IR arithmetic diagnostic" true
                    (List.exists
                       (fun (error : Diagnostic.t) -> error.code = diagnostic)
                       errors))
            [
              Printf.sprintf "I64 x=%s;I64 (*p)()=%s;x%s=p;" initial operand
                operation;
              Printf.sprintf
                "I64 Run(){I64 x=%s;I64 (*p)();p=%s;x%s=p;return x;}Run();"
                initial operand operation;
            ])
        [
          (Image.Division_by_zero, "HCIRVM0009", "42", "0");
          ( Image.Signed_division_overflow,
            "HCIRVM0010",
            "0x8000000000000000",
            "-1" );
        ])
    [ "/"; "%" ]

let effects () =
  let text =
    "extern U0 PutChars(U64 ch);I64 Seed(){PutChars('I');return 21;}I64 \
     Run(){I64 n;I64 total=0;I64 \
     (*p)();for(n=0;n<2;n++){p=Seed();total+=p;}return total;}Run();"
  in
  let report = run text in
  value 42L report;
  Alcotest.(check string)
    "reached loop effects" "II"
    (Native.output_bytes report);
  agrees 42L text;
  let indexed =
    "extern U0 PutChars(U64 ch);I64 x[2]={0,35};I64 (*p)()[2]={0,7};I64 \
     Mark(I64 n){PutChars(n);return 1;}x[Mark('L')]+=p[Mark('R')];x[1];"
  in
  let native = run indexed in
  value 42L native;
  let session, config, source = inputs indexed in
  let interpreted =
    run_integer_program_report session ~config ~source ~max_steps:100_000
  in
  Alcotest.(check string)
    "original left and right index effect order"
    (integer_program_report_output_bytes interpreted)
    (Native.output_bytes native);
  let owned =
    run
      "extern U0 PutChars(U64 ch);I64 F(){return 42;}I64 x[2]={0,35};I64 \
       (*p)()[2]={0,&F};I64 Mark(I64 n){PutChars(n);return \
       1;}x[Mark('L')]+=p[Mark('R')];"
  in
  fault Image.Callback_owned_word_escape owned;
  Alcotest.(check string)
    "index effects precede reached ownership guard"
    (Native.output_bytes native)
    (Native.output_bytes owned);
  agrees 42L "I64 x=35;I64 (*p)()=7;I64 Run(){if(0){x+=p;}return x+7;}Run();"

let quotas_and_collection () =
  let text = "I64 Run(){I64 (*p)(),(*q)();p=-14;q=7;p+=q;return p;}Run();" in
  let report = run text in
  value 42L report;
  let code, ir =
    List.fold_left
      (fun (code, ir) (fragment : Native.fragment) ->
        (code + fragment.image.code_bytes, ir + fragment.image.ir_instructions))
      (0, 0) (Native.fragments report)
  in
  value 42L
    (run ~max_code_bytes:code ~max_ir_instructions:ir
       ~max_steps:(Native.executed_steps report)
       ~max_call_depth:1 ~max_frame_bytes:16 text);
  diagnostic "HCBACK0005" (run ~max_code_bytes:(code - 1) text);
  diagnostic "HCBACK0001" (run ~max_ir_instructions:(ir - 1) text);
  fault Image.Step_limit_exceeded
    (run ~max_steps:(Native.executed_steps report - 1) text);
  fault Image.Frame_limit_exceeded (run ~max_frame_bytes:15 text);
  let nested =
    "I64 Step(I64 n){I64 (*p)();p=1;n+=p;if(n<42)return Step(n);return \
     n;}Step(0);"
  in
  value 42L (run ~max_call_depth:42 nested);
  fault Image.Call_depth_exceeded (run ~max_call_depth:41 nested);
  Gc.full_major ();
  Gc.compact ();
  agrees 42L text

let () =
  Alcotest.run "Native numeric callback update operands"
    [
      ( "original word views",
        [
          Alcotest.test_case
            "all scalar destinations, operators and callback forms" `Quick
            numeric_word_updates;
          Alcotest.test_case "ownership and original left fault order" `Quick
            numeric_update_fault_order;
          Alcotest.test_case "division and remainder faults" `Quick
            arithmetic_faults;
          Alcotest.test_case "reached effects and skipped updates" `Quick
            effects;
          Alcotest.test_case "exact limits, recursion and collection" `Quick
            quotas_and_collection;
        ] );
    ]
