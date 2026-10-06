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

let inputs mode text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"static-callback-initializers.hc"
      ~contents:text
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> checked
  in
  (session, config, source)

let run ?(max_steps = 100_000) ?max_global_bytes ?max_code_bytes
    ?max_ir_instructions ?max_default_bytes ?max_initializer_steps text =
  let session, config, source = inputs Preprocessor.Jit text in
  Native.evaluate ?max_global_bytes ?max_code_bytes ?max_ir_instructions
    ?max_default_bytes ?max_initializer_steps session ~config ~source ~max_steps

let value expected report =
  let result = Native.outcome report |> Result.map_error describe |> checked in
  Alcotest.(check int64)
    "actual native value" expected (Option.get result.value.final_value).bits;
  Alcotest.(check int)
    "no interpreter instructions in native execution" 0
    (Option.get (Native.source_progress report)).runtime.executed_steps

let diagnostic code report =
  match Native.outcome report with
  | Error errors ->
      Alcotest.(check bool)
        (describe errors) true
        (List.exists (fun (error : Diagnostic.t) -> error.code = code) errors)
  | Ok _ -> Alcotest.fail ("expected " ^ code)

let interpreted ?(max_steps = 100_000) ?max_global_bytes text =
  let session, config, source = inputs Preprocessor.Jit text in
  run_integer_program_report ?max_global_bytes session ~config ~source
    ~max_steps

let agrees expected text =
  let native = run text in
  value expected native;
  let ir = interpreted text in
  let result =
    integer_program_report_outcome ir |> Result.map_error describe |> checked
  in
  Alcotest.(check int64)
    "independent original IR value" expected
    (Option.get (Ir_integer_interpreter.final_value result.value)).bits;
  Alcotest.(check string)
    "original effects agree"
    (integer_program_report_output_bytes ir)
    (Native.output_bytes native)

let original_cells_and_leaf_order () =
  List.iter (agrees 42L)
    [
      "I64 F(){return 42;}I64 Run(){static I64(*p)()=&F;return p();}Run();";
      "I64 F(){return 42;}I64 Run(){static \
       I64(*p)()[2]={&F,p[0]};p[0]=0;return p[1]();}Run();";
      "I64 F(){return 42;}I64 Run(){static I64(*p)()[2]={&F,p[0]()};return \
       p[1];}Run();";
      "I64 F(){return 42;}I64 Run(){static \
       I64(*p)()[2][2]={{&F,p[0][0]},{p[0][1],p[0][0]()}};p[0][0]=0;return \
       p[1][0]();}Run();";
      "I64 F(){return 42;}I64 Run(){static I64(*a)()=&F,(*b)()=a;return \
       b();}Run();";
      "I64 Run(){static I64 n=40;static I64(*p)()=n+2;return p;}Run();";
      "I64 Earlier(){static I64 n=40;return n;}I64 Run(){static \
       I64(*p)()=Earlier()+2;return p;}Run();";
      "U8 F(){return 42;}I64 Run(){static U8(*p)()=&F;return p();}Run();";
      "I64 Run(){static I64(*p)()=-14;p+=7;return p;}Run();";
    ];
  List.iter
    (fun return_type ->
      agrees 0x800000000000002aL
        ("I64 Run(){static " ^ return_type
       ^ "(*p)()=0x800000000000002a;return p;}Run();"))
    [ "I8"; "U8"; "U0"; "F64"; "I64 *" ]

let header_history_and_timing () =
  List.iter (agrees 42L)
    [
      "I64 G=41;I64 Run(){static I64(*p)()=++G;return p;}G=100;Run();Run();";
      "I64 F(){return 42;}I64 Run(){static I64(*p)()=&F;return p();}I64 \
       F(){return 17;}Run();";
      "I64 G=41;I64 Unused(){if(0){static I64(*p)()=++G;}return 0;}G;";
      "I64 F(){return 42;}I64 Run(){static I64(*p)()=&F;return p();}I64 \
       Old(){return Run();}I64 Run(){static I64(*p)()=17;return \
       p;}Run();Old();";
    ];
  let text =
    "extern U0 PutChars(U64 ch);I64 G=40;I64 Seed(){PutChars('D');return \
     ++G;}I64 F(I64 n){return n+1;}I64 Run(){static I64(*p)(I64 \
     n=Seed())[3]={&F,p[0],p[0]()};return p[1]();}G=100;Run();Run();"
  in
  let report = run text in
  value 42L report;
  Alcotest.(check string)
    "header effects occur once before initialization" "D"
    (Native.output_bytes report);
  Alcotest.(check int)
    "one original saved default" 8
    (Native.default_bytes report);
  agrees 42L text;
  Gc.full_major ();
  Gc.compact ();
  agrees 42L text

let reached_faults_and_unresolved_capture () =
  List.iter
    (fun (kind, code, text) ->
      let native = run text in
      diagnostic code native;
      (match List.rev (Native.fragments native) with
      | { native_outcome = Some (Ok (Image.Fault fault)); _ } :: _ ->
          Alcotest.(check bool)
            "actual reached native fault" true (fault.kind = kind)
      | _ ->
          Alcotest.fail "original native leaf or call did not reach the fault");
      match integer_program_report_outcome (interpreted text) with
      | Error errors ->
          Alcotest.(check bool)
            (describe errors) true
            (List.exists
               (fun (error : Diagnostic.t) -> error.code = code)
               errors)
      | Ok _ -> Alcotest.fail ("IR expected " ^ code))
    [
      ( Image.Undefined_extern,
        "HCIRVM0030",
        "I64 Run(){static I64(*p)()=&Run;return p();}Run();" );
      ( Image.Undefined_extern,
        "HCIRVM0030",
        "extern I64 F();I64 Run(){static I64(*p)()=&F;return p();}I64 \
         F(){return 42;}Run();" );
      ( Image.Uninitialized_read,
        "HCIRVM0012",
        "I64 Run(){static I64(*p)()=p;return 42;}Run();" );
      ( Image.Uninitialized_read,
        "HCIRVM0012",
        "I64 F(){return 42;}I64 Run(){static I64(*p)()[2]={p[1],&F};return \
         42;}Run();" );
      ( Image.Callback_unowned_address,
        "HCIRVM0024",
        "I64 Run(){static I64(*p)()=42;return p();}Run();" );
      ( Image.Callback_unowned_address,
        "HCIRVM0024",
        "I64 F(){return 42;}I64 Run(){static I64(*p)()=&F;p=42;return \
         p();}Run();" );
    ]

let exact_limits () =
  let text =
    "I64 G=40;I64 Seed(){return ++G;}I64 F(I64 n){return n+1;}I64 Run(){static \
     I64(*p)(I64 n=Seed())=&F;return p();}Run();"
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
    (run ~max_global_bytes:16 ~max_default_bytes:8
       ~max_steps:(Native.executed_steps report)
       ~max_code_bytes:code ~max_ir_instructions:ir
       ~max_initializer_steps:(Native.preparation_steps report)
       text);
  List.iter
    (fun (code, report) -> diagnostic code report)
    [
      ("HCIRVM0016", run ~max_global_bytes:15 text);
      ("HCIRVM0011", run ~max_default_bytes:7 text);
      ("HCBACK0005", run ~max_code_bytes:(code - 1) text);
      ("HCBACK0001", run ~max_ir_instructions:(ir - 1) text);
      ("HCIRVM0007", run ~max_steps:(Native.executed_steps report - 1) text);
      ( "HCIRVM0007",
        run ~max_initializer_steps:(Native.preparation_steps report - 1) text );
    ];
  let ir_report = interpreted text in
  let result =
    integer_program_report_outcome ir_report
    |> Result.map_error describe |> checked
  in
  let steps = Ir_integer_interpreter.executed_steps result.value in
  integer_program_report_outcome
    (interpreted ~max_steps:steps ~max_global_bytes:16 text)
  |> Result.map_error describe |> checked |> ignore;
  List.iter
    (fun (code, report) ->
      match integer_program_report_outcome report with
      | Error errors ->
          Alcotest.(check bool)
            (describe errors) true
            (List.exists (fun (e : Diagnostic.t) -> e.code = code) errors)
      | Ok _ -> Alcotest.fail ("IR expected " ^ code))
    [
      ("HCIRVM0007", interpreted ~max_steps:(steps - 1) text);
      ("HCIRVM0016", interpreted ~max_global_bytes:15 text);
    ]

let () =
  Alcotest.run "Original static callback initializer execution"
    [
      ( "live leaves",
        [
          Alcotest.test_case "physical cells and original ordered leaf calls"
            `Quick original_cells_and_leaf_order;
          Alcotest.test_case
            "header effects, source timing and retained history" `Quick
            header_history_and_timing;
          Alcotest.test_case "reached faults and preinstallation captures"
            `Quick reached_faults_and_unresolved_capture;
          Alcotest.test_case "exact cumulative execution and allocation limits"
            `Quick exact_limits;
        ] );
    ]
