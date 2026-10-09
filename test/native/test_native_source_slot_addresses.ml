open Holyc_lib
module Native = Native_source_execution
module Image = X86_64_program

let describe errors =
  errors
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let run ?(max_steps = 100_000) ?max_ir_instructions ?max_code_bytes
    ?max_active_stack_bytes text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-source-slot-addresses.hc"
      ~contents:text
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:Preprocessor.Jit ()
    |> Result.get_ok
  in
  Native.evaluate ?max_ir_instructions ?max_code_bytes ?max_active_stack_bytes
    session ~config ~source ~max_steps

let value expected report =
  match Native.outcome report with
  | Error errors -> Alcotest.fail (describe errors)
  | Ok result ->
      Alcotest.(check int64)
        "original native slot result" expected
        (Option.get result.value.final_value).bits;
      List.iter
        (fun (fragment : Native.fragment) ->
          match fragment.native_outcome with
          | Some (Ok (Image.Completed _)) -> ()
          | _ ->
              Alcotest.fail "original slot fragment did not complete natively")
        (Native.fragments report);
      let progress = Native.source_progress report |> Option.get in
      Alcotest.(check int)
        "native slots execute no interpreter instructions" 0
        progress.runtime.executed_steps

let fault expected report =
  match (Native.outcome report, List.rev (Native.fragments report)) with
  | Error _, { native_outcome = Some (Ok (Image.Fault fault)); _ } :: _ ->
      Alcotest.(check bool)
        "reached native slot fault" true (fault.kind = expected);
      Alcotest.(check int)
        "original cumulative work"
        (Native.executed_steps report)
        fault.executed_steps
  | Error errors, _ -> Alcotest.fail (describe errors)
  | Ok _, _ -> Alcotest.fail "expected a reached slot fault"

let own_body () =
  List.iter
    (fun text -> value 42L (run text))
    [
      "I64 F(){I64 (*p)();p=&F;if(p==&F)return 42;return 0;}F();";
      "I64 F(I64 n){I64 (*p)(I64 n);p=&F;if(n)return p(n-1)+1;return 0;}F(42);";
      "I64 F(I64 n){I64 (*p)(I64 n)[2];p[1]=&F;if(n)return p[1](n-1)+1;return \
       0;}F(42);";
    ]

let installed_slot () =
  List.iter
    (fun text -> value 42L (run text))
    [
      "extern I64 F();I64 Call(){I64 (*p)();p=&F;return p();}I64 F(){return \
       42;}Call();";
      "extern I64 F();I64 Call(){I64 (*p)();I64 (*q)();p=&F;q=p;p=0;return \
       q();}I64 F(){return 42;}Call();";
      "extern I64 F();I64 Call(){I64 (*p)();p=&F;return p();}I64 F(){return \
       42;}I64 F(){return 17;}Call();";
    ]

let original_placeholder () =
  value 1L (run "extern I64 F();extern I64 G();&F==&G;");
  value 1L (run "extern I64 F();I64 (*p)()=&F;extern I64 G();p==&G;");
  value 0L (run "extern I64 F();I64 (*p)()=&F;I64 F(){return 42;}p==&F;");
  List.iter
    (fun text -> fault Image.Undefined_extern (run text))
    [
      "extern I64 F();I64 (*p)()=&F;p();";
      "extern I64 F();I64 (*p)()=&F;I64 F(){return 42;}p();";
      "extern I64 F();I64 (*p)()=&F;I64 (*q)()=p;p=0;I64 F(){return 42;}q();";
      "extern I64 F();I64 (*p)()[2]={0,&F};I64 F(){return 42;}p[1]();";
    ]

let capture_before_arguments () =
  let report =
    run
      "extern U0 PutChars(U64 ch);extern I64 F(I64 x,I64 y);I64 (*p)(I64 x,I64 \
       y)=&F;I64 F(I64 x,I64 y){return 42;}I64 Mark(I64 \
       ch){PutChars(ch);p=&F;return ch;}p(Mark(65),Mark(66));"
  in
  fault Image.Undefined_extern report;
  Alcotest.(check string)
    "arguments finish after original placeholder capture" "BA"
    (Native.output_bytes report)

let slot_quotas () =
  let text =
    "extern I64 F();I64 Call(){I64 (*p)();p=&F;return p();}I64 F(){return \
     42;}Call();"
  in
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
       text);
  List.iter
    (fun report ->
      match Native.outcome report with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "one-below compilation allowance executed")
    [
      run ~max_code_bytes:(code - 1) text;
      run ~max_ir_instructions:(ir - 1) text;
    ];
  fault Image.Step_limit_exceeded
    (run ~max_steps:(Native.executed_steps report - 1) text);
  let placeholder = run "extern I64 F();I64 (*p)()=&F;p();" in
  fault Image.Undefined_extern placeholder;
  let stack =
    List.fold_left
      (fun highest (fragment : Native.fragment) ->
        max highest fragment.image.entry_stack_bytes)
      0
      (Native.fragments placeholder)
  in
  fault Image.Undefined_extern
    (run ~max_active_stack_bytes:(stack + 16)
       "extern I64 F();I64 (*p)()=&F;p();");
  fault Image.Native_stack_limit_exceeded
    (run ~max_active_stack_bytes:(stack + 15)
       "extern I64 F();I64 (*p)()=&F;p();");
  Gc.full_major ();
  fault Image.Undefined_extern (run "extern I64 F();I64 (*p)()=&F;p();")

let saved_defaults () =
  List.iter
    (fun text -> fault Image.Undefined_extern (run text))
    [
      "extern I64 F();I64 Call(I64 (*p)()=&F){return p();}I64 F(){return \
       42;}Call();";
      "extern I64 F();I64 (*q)()=&F;I64 Call(I64 (*p)()=q){return p();}I64 \
       F(){return 42;}q=&F;Call();";
    ];
  value 0L (run "I64 F(I64 (*p)()=&F){return p==&F;}F();")

let reached_slot_guards () =
  fault Image.Callback_signature_mismatch
    (run
       "extern I64 F(I64 n);I64 Call(){I64 (*p)(I64 n);p=&F;return p(42);}I64 \
        F(){return 42;}Call();");
  fault Image.Callback_owned_word_escape
    (run "extern I64 F();I64 (*p)()=&F;I64 Read(){return p;}Read();");
  fault Image.Callback_update_owned_address
    (run "extern I64 F();I64 (*p)()=&F;p++;");
  fault Image.Code_comparison_invalid_word (run "extern I64 F();&F==42;");
  let provider =
    run "extern U0 PutChars(U64 ch);U0 (*p)(U64 ch)=&PutChars;p(65);42;"
  in
  value 42L provider;
  Alcotest.(check string)
    "original provider callback bytes" "A"
    (Native.output_bytes provider);
  match
    Native.outcome
      (run
         "extern U0 StreamPrint(U8 *fmt,...);U0 (*p)(U8 \
          *fmt,...)=&StreamPrint;p(\"A\");")
  with
  | Error errors ->
      Alcotest.(check bool)
        "StreamPrint callback reaches its inactive context check" true
        (List.exists
           (fun (error : Diagnostic.t) -> error.code = "HCIRVM0027")
           errors)
  | Ok _ ->
      Alcotest.fail
        "StreamPrint callback outside a stream must fault after formatting"

let independent_interpreter () =
  List.iter
    (fun (expected, text) ->
      let session = Session.create () in
      let source =
        Session.add_source session ~path:"ir-original-slot.hc" ~contents:text
      in
      let config =
        Preprocessor.Config.create ~compilation_mode:Preprocessor.Jit ()
        |> Result.get_ok
      in
      match
        run_integer_program_report session ~config ~source ~max_steps:100_000
        |> integer_program_report_outcome
      with
      | Error errors -> Alcotest.fail (describe errors)
      | Ok result ->
          Alcotest.(check int64)
            "independent original IR execution" expected
            (Option.get (Ir_integer_interpreter.final_value result.value)).bits)
    [
      ( 42L,
        "I64 F(I64 n){I64 (*p)(I64 n);p=&F;if(n)return p(n-1)+1;return \
         0;}F(42);" );
      ( 42L,
        "extern I64 F();I64 Call(){I64 (*p)();p=&F;return p();}I64 F(){return \
         42;}I64 F(){return 17;}Call();" );
      (1L, "extern I64 F();extern I64 G();&F==&G;");
      (0L, "extern I64 F();I64 (*p)()=&F;I64 F(){return 42;}p==&F;");
      (0L, "I64 F(I64 (*p)()=&F){return p==&F;}F();");
    ]

let () =
  Alcotest.run "Native original function slots"
    [
      ( "slot execution",
        [
          Alcotest.test_case "own body, copies and recursion" `Quick own_body;
          Alcotest.test_case "installed and historical logical slots" `Quick
            installed_slot;
          Alcotest.test_case "shared placeholder and pre-installation capture"
            `Quick original_placeholder;
          Alcotest.test_case "capture precedes argument effects" `Quick
            capture_before_arguments;
          Alcotest.test_case "cumulative quotas and independent tasks" `Quick
            slot_quotas;
          Alcotest.test_case "original saved placeholder defaults" `Quick
            saved_defaults;
          Alcotest.test_case "signature, escape and provider boundaries" `Quick
            reached_slot_guards;
          Alcotest.test_case "independent interpreter slot execution" `Quick
            independent_interpreter;
        ] );
    ]
