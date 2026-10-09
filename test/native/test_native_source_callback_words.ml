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
    Session.add_source session ~path:"native-source-callback-words.hc"
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

let cells_and_complete_words () =
  List.iter
    (fun bits -> agrees bits (Printf.sprintf "I64 (*p)()=0x%Lx;p;" bits))
    [ 0L; 42L; Int64.min_int; Int64.max_int; -1L ];
  List.iter (agrees 42L)
    [
      "I64 (*p)()=34;++p;p;";
      "U0 (*p)()=50;--p;p;";
      "F64 (*p)()=34;++p;p;";
      "U8 *(*p)()=42;p;";
      "I64 (*p)()=34;I64 (*q)()=++p;q;";
      "I64 (*p)();p=42;p;";
      "I64 (*p)()=34;I64 (*q)()=p;++q;q;";
      "I64 (*p)()=34;I64 (*q)()=0;q=p;++q;q;";
      "I64 (*p)()=50;I64 (*q)()=p--;q=34;++q;q;";
    ]

let indexed_words_and_updates () =
  List.iter (agrees 42L)
    [
      "I64 (*p)()[2]={34,0};p[1]=p[0];++p[1];p[1];";
      "I64 (*p)()[2][2]={{41,0},{0,34}};++p[1][1];p[1][1];";
      "I64 (*p)()[2][2];p[1][1]=50;--p[1][1];p[1][1];";
      "I64 (*p)()[2]={0,50};I64 (*q)()=p[1];--q;q;";
    ]

let frames_forwarding_and_saved_defaults () =
  List.iter (agrees 42L)
    [
      "I64 F(){I64 (*p)();p=34;++p;return p;}F();";
      "I64 F(){I64 (*p)()[2];p[1]=34;++p[1];return p[1];}F();";
      "I64 (*p)()=42;I64 Read(I64 (*q)()){return q;}Read(p);";
      "I64 Read(I64 (*q)()=42){return q;}Read();";
      "I64 Read(I64 (*q)()=42){return q;}I64 F(){return Read();}F();";
      "I64 (*p)()=42;I64 Read(I64 (*q)(),...){return q;}Read(p,1,2);";
      "I64 (*p)()=42;I64 F(){if(0)p();return 42;}F();";
    ];
  agrees 298L "U8 Read(I64 (*q)()=298){return q;}Read();";
  let default_report =
    run
      "extern U0 PutChars(U64 ch);I64 Seed(){PutChars('D');return 42;}I64 \
       Read(I64 (*q)()=Seed()){return q;}Read();Read();"
  in
  value 42L default_report;
  Alcotest.(check string)
    "named callback parameter saves its original integer default once" "D"
    (Native.output_bytes default_report);
  let text = "I64 Read(I64 (*q)()=42){return q;}Read();" in
  value 42L (run ~max_default_bytes:8 text);
  diagnostic "HCIRVM0011" (run ~max_default_bytes:7 text)

let source_history_and_initializer_effects () =
  agrees 42L "I64 (*p)()=34;I64 F(){return ++p;}I64 (*p)()=50;F();";
  agrees 50L "I64 (*p)()=34;I64 F(){return ++p;}I64 (*p)()=50;F();p;";
  let report =
    run
      "extern U0 PutChars(U64 ch);I64 Seed(){PutChars('I');return 34;}I64 \
       (*p)()=Seed();++p;p;"
  in
  value 42L report;
  Alcotest.(check string)
    "live numeric initializer effects execute once" "I"
    (Native.output_bytes report);
  for _ = 1 to 5 do
    Gc.full_major ();
    agrees 42L "I64 (*p)()=42;I64 Read(){return p;}Read();"
  done

let reached_numeric_calls () =
  List.iter
    (fun text ->
      let report = run text in
      diagnostic "HCIRVM0024" report;
      fault Image.Callback_unowned_address report)
    [
      "I64 (*p)()=0;p();";
      "I64 (*p)()=0xffffffffffffffff;p();";
      "I64 (*p)()[2]={42,0};p[0]();";
      "I64 (*p)()=42;I64 (*q)()=p;q();";
      "I64 (*p)()=42;I64 F(){return p();}F();";
      "I64 F(){I64 (*p)();p=42;return p();}F();";
      "I64 (*p)()=42;I64 Read(I64 (*q)()){return q();}Read(p);";
      "I64 Read(I64 (*q)()=42){return q();}Read();";
      "I64 (*p)()=42;I64 A=p();A;";
      "I64 (*p)()=42;I64 F(){static I64 A=p();return A;}F();";
    ]

let reverse_argument_effects_and_callee_capture () =
  let prefix =
    "extern U0 PutChars(U64 ch);I64 (*p)(I64 a,I64 b)=0;I64 Mark(I64 \
     n){PutChars(n);p=42;return n;}"
  in
  List.iter
    (fun suffix ->
      let text = prefix ^ suffix in
      let report = run text in
      diagnostic "HCIRVM0024" report;
      fault Image.Callback_unowned_address report;
      Alcotest.(check string)
        "callee is captured before reverse arguments" "BA"
        (Native.output_bytes report);
      let session, config, source = inputs text in
      let interpreted =
        run_integer_program_report session ~config ~source ~max_steps:100_000
      in
      Alcotest.(check string)
        "independent IR argument output" "BA"
        (integer_program_report_output_bytes interpreted))
    [ "p(Mark(65),Mark(66));"; "I64 F(){return p(Mark(65),Mark(66));}F();" ]

let uninitialized_bounds_and_ownership_gate () =
  List.iter
    (fun text -> fault Image.Uninitialized_read (run text))
    [
      "I64 (*p)();p;";
      "I64 (*p)();p();";
      "I64 (*p)()[2];p[0]=42;p[1];";
      "I64 (*p)()[2];p[0]=42;p[1]();";
    ];
  fault Image.Address_out_of_bounds (run "I64 (*p)()[2]={42,0};p[2];");
  List.iter (agrees 42L)
    [
      "I64 F(){return 42;}I64 (*p)()=&F;p();";
      "I64 F(){return 42;}I64 G(){I64 (*p)();p=&F;return 42;}G();";
      "I64 F(){return 42;}I64 G(){if(0){I64 (*p)();p=&F;}return 42;}G();";
    ]

let limits_and_native_extents () =
  let text = "I64 (*p)()[2][2]={{0,34},{50,0}};I64 (*q)()=p[0][1];++q;q;" in
  let report = run ~max_global_bytes:40 text in
  value 42L report;
  let fragments = Native.fragments report in
  Alcotest.(check (list int))
    "callback data plus independent flags and owners"
    [ 96; 96; 96; 96; 120; 120; 120 ]
    (List.map
       (fun (fragment : Native.fragment) -> fragment.image.global_arena_bytes)
       fragments);
  diagnostic "HCIRVM0016" (run ~max_global_bytes:39 text);
  value 42L (run ~max_global_bytes:8 "I64 (*p)()=42;p;");
  diagnostic "HCIRVM0016" (run ~max_global_bytes:7 "I64 (*p)()=42;p;");
  let code, ir =
    List.fold_left
      (fun (code, ir) (fragment : Native.fragment) ->
        (code + fragment.image.code_bytes, ir + fragment.image.ir_instructions))
      (0, 0) fragments
  in
  let work = Native.preparation_steps report in
  value 42L
    (run ~max_code_bytes:code ~max_ir_instructions:ir
       ~max_steps:(Native.executed_steps report)
       ~max_initializer_steps:work text);
  diagnostic "HCBACK0005" (run ~max_code_bytes:(code - 1) text);
  diagnostic "HCBACK0001" (run ~max_ir_instructions:(ir - 1) text);
  fault Image.Step_limit_exceeded
    (run ~max_steps:(Native.executed_steps report - 1) text);
  diagnostic "HCIRVM0007" (run ~max_initializer_steps:(work - 1) text)

let () =
  Alcotest.run "Native source callback words"
    [
      ( "original storage",
        [
          Alcotest.test_case "cells, copies and complete numeric words" `Quick
            cells_and_complete_words;
          Alcotest.test_case "indexed arrays and numeric updates" `Quick
            indexed_words_and_updates;
          Alcotest.test_case "frames, forwarding and saved defaults" `Quick
            frames_forwarding_and_saved_defaults;
          Alcotest.test_case "source history and live initializer effects"
            `Quick source_history_and_initializer_effects;
          Alcotest.test_case "reached numeric callback calls" `Quick
            reached_numeric_calls;
          Alcotest.test_case "reverse argument effects and callee capture"
            `Quick reverse_argument_effects_and_callee_capture;
          Alcotest.test_case "initialization, bounds and executable ownership"
            `Quick uninitialized_bounds_and_ownership_gate;
          Alcotest.test_case "exact native extents and cumulative limits" `Quick
            limits_and_native_extents;
        ] );
    ]
