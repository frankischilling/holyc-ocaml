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
    Session.add_source session ~path:"native-source-owned-defaults.hc"
      ~contents:text
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:Preprocessor.Jit () |> checked
  in
  (session, config, source)

let run ?(max_steps = 100_000) ?max_global_bytes ?max_ir_instructions
    ?max_code_bytes ?max_initializer_steps ?max_default_bytes text =
  let session, config, source = inputs text in
  Native.evaluate ?max_global_bytes ?max_ir_instructions ?max_code_bytes
    ?max_initializer_steps ?max_default_bytes session ~config ~source ~max_steps

let value expected report =
  let result = Native.outcome report |> Result.map_error describe |> checked in
  let word = Option.get result.value.final_value in
  Alcotest.(check int64) "complete native callback word" expected word.bits;
  Alcotest.(check int)
    "native defaults execute no VM instructions" 0
    (Option.get (Native.source_progress report)).runtime.executed_steps;
  List.iter
    (fun (fragment : Native.fragment) ->
      match fragment.native_outcome with
      | Some (Ok (Image.Completed _)) -> ()
      | _ -> Alcotest.fail "source fragment did not execute natively")
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
  | { native_outcome = Some (Ok (Image.Fault fault)); _ } :: _ ->
      Alcotest.(check bool)
        "reached native owning fault" true (fault.kind = kind);
      Alcotest.(check bool)
        "fault executes original native work" true (fault.executed_steps > 0);
      Alcotest.(check int)
        "fault retains cumulative native work" fault.executed_steps
        (Native.executed_steps report)
  | _ -> Alcotest.fail "failure did not reach native code"

let agrees expected text =
  let native = run text in
  value expected native;
  let session, config, source = inputs text in
  let interpreted =
    run_integer_program_report session ~config ~source ~max_steps:100_000
  in
  let result =
    integer_program_report_outcome interpreted
    |> Result.map_error describe |> checked
  in
  let word = Option.get (Ir_integer_interpreter.final_value result.value) in
  Alcotest.(check int64) "independent original IR word" expected word.bits

let original_saved_owners () =
  List.iter (agrees 42L)
    [
      "I64 F(){return 42;}I64 Call(I64 (*q)()=&F){return q();}Call();";
      "I64 F(){return 42;}I64 Call(I64 (*q)()=&F){return q();}I64 F(){return \
       17;}Call();";
      "I64 F(){return 42;}I64 G(){return 17;}I64 (*p)()=&F;I64 Call(I64 \
       (*q)()=p){return q();}p=&G;Call();";
      "I64 F(){return 42;}I64 G(){return 17;}I64 (*p)()[2]={0,&F};I64 Call(I64 \
       (*q)()=p[1]){return q();}p[1]=&G;Call();";
      "I64 F(){return 42;}I64 G(){return 17;}I64 Call(I64 (*q)()=&F){return \
       q();}I64 Old(){return Call();}I64 Call(I64 (*q)()=&G){return \
       q();}Old();";
      "I64 F(){return 42;}I64 Call(I64 (*q)()=&F){I64 (*copy)();copy=q;return \
       copy();}Call();";
    ]

let original_header_effects () =
  agrees 42L
    "I64 F(){return 42;}I64 G(){return 17;}I64 (*p)()=&G;I64 Call(I64 \
     (*q)()=(p=&F)){return q();}Call(&G);p();";
  agrees 17L
    "I64 F(){return 42;}I64 G(){return 17;}I64 (*p)()=&G;I64 Call(I64 \
     (*q)()=(p=&F)){return q();}p=&G;Call();p();";
  agrees 42L
    "I64 F(){return 42;}I64 G(){return 17;}I64 (*p)()=&G;I64 Unused(I64 \
     (*q)()=(p=&F)){return q();}p();";
  agrees 42L
    "I64 F(){return 42;}I64 G(){return 17;}I64 (*p)()=&G;I64 Unused(I64 \
     (*q)()=(p=&F)){return q();}if(0)Unused();p();";
  agrees 42L
    "I64 F(){return 42;}I64 (*p)()=&F;I64 Read(I64 (*q)()=p){return \
     q();}p=0;Read();Read();"

let shapes_and_numeric_words () =
  agrees 42L "U0 F(){}U0 Call(U0 (*q)()=&F){q();}Call();42;";
  agrees 42L
    "I64 F(...){return argc+argv[0]+argv[1];}I64 Call(I64 (*q)(...)=&F){return \
     q(20,20);}Call();";
  agrees 42L
    "I64 A=42;I64 F(I64 *p){return *p;}I64 Call(I64 (*q)(I64 *p)=&F){return \
     q(&A);}Call();";
  agrees 42L
    "I64 F(){return 42;}I64 Read(I64 (*q)()=42){return q;}I64 Call(I64 \
     (*q)()=&F){return q();}Call();Read();";
  agrees 42L "I64 (*p)()=34;I64 Read(I64 (*q)()=++p){return q;}Read();";
  agrees (-1L)
    "U64 F(){return 0xffffffffffffffff;}U64 Call(U64 (*q)()=&F){return \
     q();}Call();"

let reached_faults () =
  let escaped =
    run "I64 F(){return 42;}I64 Read(I64 (*q)()=&F){return q;}Read();"
  in
  diagnostic "HCIRVM0024" escaped;
  fault Image.Callback_owned_word_escape escaped;
  let signature =
    run "I64 F(I64 x){return x;}I64 Call(I64 (*q)()=&F){return q();}Call();"
  in
  diagnostic "HCIRVM0014" signature;
  fault Image.Callback_signature_mismatch signature;
  let report =
    run
      "extern U0 PutChars(U64 ch);I64 Z=0;I64 F(){PutChars('F');return \
       1/Z;}I64 Call(I64 (*q)()=&F){return q();}Call();"
  in
  diagnostic "HCIRVM0009" report;
  fault Image.Division_by_zero report;
  Alcotest.(check string)
    "original body output survives reached fault" "F"
    (Native.output_bytes report);
  let report =
    run
      "I64 F(){return 42;}I64 (*p)()=&F;I64 Call(I64 (*q)()=++p){return \
       q();}Call();"
  in
  diagnostic "HCIRVM0024" report;
  fault Image.Callback_update_owned_address report

let actual_saved_capture_and_latch () =
  let text = "I64 F(){return 42;}17;I64 Call(I64 (*q)()=&F){return q();}" in
  let report = run text in
  value 17L report;
  let defaults =
    Native.fragments report
    |> List.filter (fun (fragment : Native.fragment) ->
        fragment.kind = Native.Default)
  in
  Alcotest.(check int)
    "one original declaration-time capture" 1 (List.length defaults);
  (match (List.hd defaults).native_outcome with
  | Some
      (Ok
         (Image.Completed
            { final_value = None; captured_callback = Some saved; _ })) ->
      let module Saved = Holyc_lib__Ir.Saved_parameter_value in
      Alcotest.(check (option int64))
        "an owner has no exported integer bits" None (Saved.word_bits saved);
      let link, _ = Option.get (Saved.callback_source saved) in
      Alcotest.(check string)
        "captured original native function" "F"
        (Holyc_lib__Sema.Symbol.name
           (Holyc_lib__Ir.Retained_function.symbol link))
  | _ -> Alcotest.fail "default did not capture its actual native owner");
  Alcotest.(check int)
    "saved owner payload remains charged" 8
    (Native.default_bytes report)

let quotas () =
  let text = "I64 F(){return 42;}I64 Call(I64 (*q)()=&F){return q();}Call();" in
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
  diagnostic "HCBACK0001" (run ~max_ir_instructions:(ir - 1) text)

let () =
  Alcotest.run "Native source owned defaults"
    [
      ( "original callback defaults",
        [
          Alcotest.test_case "saved original owners and header history" `Quick
            original_saved_owners;
          Alcotest.test_case "once-only declaration-time effects" `Quick
            original_header_effects;
          Alcotest.test_case "U0, pointers, tails and numeric words" `Quick
            shapes_and_numeric_words;
          Alcotest.test_case "reached original native faults" `Quick
            reached_faults;
          Alcotest.test_case "actual owner capture and preserved command latch"
            `Quick actual_saved_capture_and_latch;
          Alcotest.test_case "exact and one-below cumulative quotas" `Quick
            quotas;
        ] );
    ]
