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
    Session.add_source session ~path:"native-source-named-callback-types.hc"
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
  Alcotest.(check int64)
    "independent original IR value" expected
    (Option.get (Ir_integer_interpreter.final_value result.value)).bits;
  Alcotest.(check string)
    "independent original effects"
    (integer_program_report_output_bytes interpreted)
    (Native.output_bytes native)

let pair = "class Pair{I64 a;I64 b;};"

let physical_cells_and_queries () =
  List.iter
    (fun text -> agrees 42L (pair ^ text))
    [
      "I64 Run(){Pair (*p)();p=42;return p;}Run();";
      "Pair (*p)()=42;p;";
      "I64 Run(){static Pair (*p)()=42;return p;}Run();Run();";
      "I64 Run(){static Pair (*p)()[2]={0,42};return p[1];}Run();";
      "I64 Run(){Pair (*p)(),*(*q)();p=42;q=2;if(q!=2)return 0;return p;}Run();";
      "I64 Run(){I64 (*p)(Pair *v);p=42;return p;}Run();";
      "I64 Run(){I64 (*p)(Pair (*q)());p=42;return p;}Run();";
      "I64 Run(Pair (*p)()){return p;}Run(42);";
      "I64 Run(){Pair (*p)();return 34+sizeof(p);}Run();";
      "I64 Run(){static Pair (*p)()[2];return 26+sizeof(p);}Run();";
      "Pair (*p)();34+sizeof(p);";
      "I64 Run(){static Pair (*p)()[2][2]={0,0,0,42};return p[1][1];}Run();";
    ];
  agrees 0x800000000000002aL
    (pair ^ "I64 Run(){static Pair (*p)()=0x800000000000002a;return p;}Run();");
  agrees 42L "class I64{U8 x;};U64 Run(){I64 (*p)();p=42;return p;}Run();"

let saved_headers_and_history () =
  List.iter
    (fun text -> agrees 42L (pair ^ text))
    [
      "I64 Consume(Pair (*q)()){return q;}I64 Run(){I64 (*p)(Pair \
       (*q)()=42);p=&Consume;return p();}Run();";
      "I64 Run(Pair (*q)()=42){return q;}Run();";
      "I64 Counter=0;I64 Seed(){return ++Counter+41;}I64 Consume(Pair \
       (*q)()){return q;}I64 Run(){static I64 (*p)(Pair \
       (*q)()=Seed());p=&Consume;return p();}class Pair{U8 \
       different;};Counter=99;Run();Run();";
      "I64 Run(){static Pair (*p)()=42;return p;}I64 Old(){return Run();}class \
       Pair{U8 different;};I64 Run(){static Pair (*p)()=17;return \
       p;}Run();Old();";
      "I64 Run(Pair (*p)()=42)#exe {class Pair{U8 different;};}{return \
       p;}Run();";
    ];
  agrees 42L
    "extern class Pair;I64 Run(Pair (*p)()=42){return p;}class Pair{I64 a;I64 \
     b;};Run();";
  let text =
    "extern U0 PutChars(U64 ch);" ^ pair
    ^ "I64 Seed(){PutChars('D');return 42;}I64 Consume(Pair (*q)()){return \
       q;}I64 Run(){static I64 (*p)(Pair (*q)()=Seed());p=&Consume;return \
       p();}class Pair{U8 different;};Run();Run();"
  in
  let report = run text in
  value 42L report;
  Alcotest.(check string)
    "original header effect executes once" "D"
    (Native.output_bytes report);
  Alcotest.(check int)
    "one physical saved callback word" 8
    (Native.default_bytes report);
  agrees 42L text;
  Gc.full_major ();
  Gc.compact ();
  agrees 42L text

let reached_faults_and_type_boundaries () =
  List.iter
    (fun (kind, code, text) ->
      let native = run (pair ^ text) in
      diagnostic code native;
      match List.rev (Native.fragments native) with
      | { native_outcome = Some (Ok (Image.Fault fault)); _ } :: _ ->
          Alcotest.(check bool)
            "original native callback reaches its fault" true (fault.kind = kind)
      | _ -> Alcotest.fail "callback fault did not enter original native code")
    [
      ( Image.Uninitialized_read,
        "HCIRVM0012",
        "I64 Run(){Pair (*p)();return p;}Run();" );
      ( Image.Callback_unowned_address,
        "HCIRVM0024",
        "I64 Run(){I64 (*p)(Pair (*q)());p=42;return p(42);}Run();" );
      ( Image.Callback_unowned_address,
        "HCIRVM0024",
        "I64 Run(){static I64 (*p)(Pair (*q)())[2]={0,42};return \
         p[1](42);}Run();" );
    ];
  List.iter
    (fun (code, text) -> diagnostic code (run (pair ^ text)))
    [
      ("HCBACK0002", "Pair *F(){return 0;}Pair *(*p)()=&F;p();");
      ("HCBACK0002", "I64 Run(){Pair (*p)();p=42;return p();}Run();");
      ("HCRUN0003", "I64 F(Pair *p){if(p==0)return 42;return 0;}F(0);");
    ]

let exact_limits () =
  let text =
    pair
    ^ "I64 Counter=0;I64 Seed(){return ++Counter+41;}I64 Consume(Pair \
       (*q)()){return q;}I64 Run(){static I64 (*p)(Pair \
       (*q)()=Seed())=&Consume;return p();}class Pair{U8 different;};Run();"
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
    ]

let () =
  Alcotest.run "Original named callback type execution"
    [
      ( "selected classes",
        [
          Alcotest.test_case "physical cells, arrays and sizeof" `Quick
            physical_cells_and_queries;
          Alcotest.test_case
            "saved headers, forward classes and retained history" `Quick
            saved_headers_and_history;
          Alcotest.test_case "reached faults and aggregate execution boundaries"
            `Quick reached_faults_and_type_boundaries;
          Alcotest.test_case "exact cumulative allocation and execution limits"
            `Quick exact_limits;
        ] );
    ]
