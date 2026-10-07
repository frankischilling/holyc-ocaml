open Holyc_lib
module Native = Native_source_execution
module Image = X86_64_program

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let diagnostics errors =
  errors
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let inputs text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-source-functions.hc" ~contents:text
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:Preprocessor.Jit () |> checked
  in
  (session, config, source)

let run ?(max_steps = 100_000) ?max_ir_instructions ?max_code_bytes
    ?max_initializer_steps ?max_default_bytes ?max_global_bytes ?max_frame_bytes
    ?max_call_depth ?max_active_stack_bytes ?max_output_bytes ?max_output_work
    text =
  let session, config, source = inputs text in
  Native.evaluate ?max_ir_instructions ?max_code_bytes ?max_initializer_steps
    ?max_default_bytes ?max_global_bytes ?max_frame_bytes ?max_call_depth
    ?max_active_stack_bytes ?max_output_bytes ?max_output_work session ~config
    ~source ~max_steps

let value ?(output = "") expected report =
  let result =
    Native.outcome report |> Result.map_error diagnostics |> checked
  in
  let word = Option.get result.value.final_value in
  Alcotest.(check int64) "original native function result" expected word.bits;
  Alcotest.(check string)
    "captured ordinary output" output
    (Native.output_bytes report);
  word

let rejection report =
  match Native.outcome report with
  | Error errors when errors <> [] -> errors
  | _ -> Alcotest.fail "unsupported native function source executed"

let diagnostic code report =
  let errors = rejection report in
  Alcotest.(check bool)
    (diagnostics errors) true
    (List.exists (fun (error : Diagnostic.t) -> error.code = code) errors)

let fault kind report =
  ignore (rejection report);
  match List.rev (Native.fragments report) with
  | { native_outcome = Some (Ok (Image.Fault fault)); _ } :: _ ->
      Alcotest.(check bool)
        "reached original native fault" true (fault.kind = kind);
      Alcotest.(check int)
        "native fault retains cumulative work" fault.executed_steps
        (Native.executed_steps report);
      fault
  | _ ->
      Alcotest.failf "function failure has no native fault outcome: %s"
        (diagnostics (rejection report))

let completed report =
  List.iter
    (fun (fragment : Native.fragment) ->
      match fragment.native_outcome with
      | Some (Ok (Image.Completed _)) -> ()
      | _ -> Alcotest.fail "original source fragment did not complete natively")
    (Native.fragments report)

let source = "I64 A=41; I64 F(){return A+1;} F();"

let original_definition_and_call () =
  let report = run source in
  ignore (value 42L report);
  completed report;
  let fragments = Native.fragments report in
  Alcotest.(check int)
    "initializer, definition and resumed call" 3 (List.length fragments);
  Alcotest.(check (list int))
    "all fragments share the original arena" [ 16; 16; 16 ]
    (List.map
       (fun (fragment : Native.fragment) -> fragment.image.global_arena_bytes)
       fragments);
  Alcotest.(check (list int))
    "definition and call retain the exact function" [ 0; 1; 1 ]
    (List.map
       (fun (fragment : Native.fragment) -> fragment.image.function_count)
       fragments);
  let progress = Option.get (Native.source_progress report) in
  Alcotest.(check int)
    "native functions execute no interpreter instructions" 0
    progress.runtime.executed_steps;
  let session, config, source = inputs source in
  let interpreted =
    run_integer_program_report session ~config ~source ~max_steps:100_000
    |> integer_program_report_outcome
    |> Result.map_error diagnostics
    |> checked
  in
  let word =
    Ir_integer_interpreter.final_value interpreted.value |> Option.get
  in
  Alcotest.(check int64) "independent IR source result" 42L word.bits

let direct_source_forms () =
  List.iter
    (fun text -> ignore (value 42L (run text)))
    [
      "I64 F(){return 42;} F();";
      "I64 F(){return 42;} F;";
      "I64 Add(I64 a,I64 b){return a+b;} Add(20,22);";
      "I64 Add(I64 a,I64 b){return a+b;} I64 Answer(){return Add(19,23);} \
       Answer();";
      "I64 F(I64 n){I64 A[2];A[0]=n;A[1]=2;return A[0]+A[1];} F(40);";
      "I64 F(I64 n){I64 total=0;while(n){total+=n;n--;}return total;} F(8)+6;";
      "I64 A=40; U0 F(){A+=2;} F(); A;";
      "42; I64 F(){return 100;}";
      "I64 F(){return 42;} F(); I64 G(){return 100;}";
    ]

let declared_function_widths () =
  List.iter
    (fun (type_name, argument, added) ->
      let text =
        Printf.sprintf "%s F(%s n){return n;} F(%s)+%s;" type_name type_name
          argument added
      in
      ignore (value 42L (run text)))
    [
      ("I8", "255", "43");
      ("U8", "257", "41");
      ("I16", "65535", "43");
      ("U16", "65537", "41");
      ("I32", "4294967295", "43");
      ("U32", "4294967297", "41");
      ("I64", "41", "1");
      ("U64", "41", "1");
      ("Bool", "255", "43");
    ];
  let result =
    run "U64 F(U64 n){return n+1;} F(0x8000000000000000);"
    |> value (Int64.succ Int64.min_int)
  in
  Alcotest.(check bool)
    "retained native function keeps its unsigned return" true
    (result.type_ = Image.U64)

let original_initializers_and_arrays () =
  List.iter
    (fun text -> ignore (value 42L (run text)))
    [
      "I64 A=41; I64 F(){return A+1;} I64 B=F(); B;";
      "I64 A=40; I64 Next(){return ++A;} I64 B=Next(); A+B-40;";
      "I64 A=40; I64 Next(){return ++A;} I64 B[2]={Next(),Next()}; \
       B[0]+B[1]-41;";
      "I64 A[2]={41,1}; I64 F(){return A[0]+A[1];} F();";
      "U8 A[2]={255,41}; I64 F(){A[0]+=2;return A[0]+A[1];} F();";
      "I64 A[2][2]={{20,1},{20,1}}; I64 F(){return A[0][2]+A[1][-1]+21;} F();";
      "I64 A[2]={40,2}; I64 Sum(I64 *p){return p[0]+p[1];} Sum(A);";
    ];
  let report = run "I64 A=41; I64 F(){return A+1;} I64 B=F(); B;" in
  ignore (value 42L report);
  let fragment = List.nth (Native.fragments report) 2 in
  Alcotest.(check bool)
    "F executes at the original live B leaf" true
    (fragment.kind = Native.Initializer && fragment.image.function_count = 1);
  completed report

let retained_source_ownership () =
  List.iter
    (fun text -> ignore (value 42L (run text)))
    [
      "I64 A=40; I64 Next(){return ++A;} Next(); I64 A=100; Next();";
      "I64 A=40; I64 Next(){return ++A;} Next(); I64 A=100; I64 X[2]={1,2}; \
       Next();";
      "I64 Base(){return 33;} I64 Wrap(){return Base()+9;} I64 Base(){return \
       100;} Wrap();";
      "I64 Base(){return 33;} I64 Wrap(){return Base()+9;} I64 Base(){return \
       100;} Wrap()+Base()-100;";
      "I64 A[2]={40,2}; I64 Old(){return A[0]+A[1];} I64 A[2]={1,1}; Old();";
      "I64 F(I64 n){return n+1;} I64 G(){return F(41);} I64 F(I64 a,I64 \
       b){return a+b;} G();";
    ]

let once_only_calls_and_recursion () =
  List.iter
    (fun text -> ignore (value 42L (run text)))
    [
      "I64 N=0; I64 Next(){return ++N;} I64 Pair(I64 a,I64 b){return a*10+b;} \
       Pair(Next(),Next())+N+19;";
      "I64 N=0; I64 Next(){return ++N;} I64 Add(I64 a,I64 b){return a+b;} I64 \
       Wrap(){return Add(Next(),Next());} Wrap()+N+37;";
      "I64 Recur(I64 n){if(n)return 1+Recur(n-1);return 40;} I64 \
       Other(){return 0;} Recur(2);";
      "I64 A=0; I64 Recur(I64 n){A++;if(n)return Recur(n-1);return A;} \
       Recur(3)+38;";
      "I64 Recur(I64 n){if(n)return Recur(n-1);return 42;} I64 Old(){return \
       Recur(2);} I64 Recur(I64 n){return 100;} Old();";
      "I64 Base(I64 x){return x+1;} I64 Middle(I64 x){return Base(x)+1;} I64 \
       Top(){return Middle(40);} Top();";
      "I64 A=0; I64 Broken(){A=99;return 1/0;} if(0)Broken(); A+42;";
      "I64 A=0; I64 F(){A++;return 42;} F()+A-1;";
    ]

let reached_function_faults () =
  let report = run "I64 A=0; I64 Broken(){A=41;return 1/0;} Broken(); A=99;" in
  let reached = fault Image.Division_by_zero report in
  Alcotest.(check (option string))
    "fault identifies original function" (Some "Broken") reached.function_name;
  Alcotest.(check bool)
    "fault retains source location" true
    (Option.is_some reached.span);
  Alcotest.(check int)
    "later source is never entered" 3
    (List.length (Native.fragments report));
  List.iter
    (fun (kind, text) -> ignore (fault kind (run text)))
    [
      (Image.Uninitialized_read, "I64 A; I64 F(){return A;} F();");
      (Image.Uninitialized_read, "I64 A[2]; I64 F(){A[0]=42;return A[1];} F();");
      ( Image.Address_out_of_bounds,
        "I64 A[2]={41,1}; I64 F(){return A[2];} F();" );
    ];
  let recursive =
    "I64 Recur(I64 n){if(n)return 1+Recur(n-1);return 40;} Recur(2);"
  in
  ignore (value 42L (run ~max_call_depth:3 recursive));
  ignore (fault Image.Call_depth_exceeded (run ~max_call_depth:2 recursive))

let retained_word_tails () =
  List.iter
    (fun text -> ignore (value 42L (run text)))
    [
      "I64 F(...){return argv[0]+argv[1]+argc;} F(20,20);";
      "I64 F(I64 n,...){return n+argc;} F(42);";
      "I64 F(I64 n,...){argc=0;return argv[1];} F(0,20,42);";
      "I64 Count(...){return argc;} I64 F(){return Count(1,2);} F()+40;";
      "I64 N=0; I64 Next(){return ++N;} I64 Tail(...){return \
       argv[0]*10+argv[1];} Tail(Next(),Next())+N+19;";
    ];
  let source = "I64 F(...){argc=100;return argv[0];} F();" in
  ignore (fault Image.Address_out_of_bounds (run source))

let cumulative_function_limits () =
  let baseline = run source in
  ignore (value 42L baseline);
  let steps = Native.executed_steps baseline in
  ignore (value 42L (run ~max_steps:steps source));
  ignore (fault Image.Step_limit_exceeded (run ~max_steps:(steps - 1) source));
  let code, ir =
    List.fold_left
      (fun (code, ir) (fragment : Native.fragment) ->
        (code + fragment.image.code_bytes, ir + fragment.image.ir_instructions))
      (0, 0)
      (Native.fragments baseline)
  in
  ignore (value 42L (run ~max_code_bytes:code ~max_ir_instructions:ir source));
  diagnostic "HCBACK0005" (run ~max_code_bytes:(code - 1) source);
  diagnostic "HCBACK0001" (run ~max_ir_instructions:(ir - 1) source);
  ignore (value 42L (run ~max_global_bytes:8 source));
  let too_small = run ~max_global_bytes:7 source in
  ignore (rejection too_small);
  Alcotest.(check int)
    "storage failure precedes native definition or call" 0
    (Native.executed_steps too_small);
  let preparation = Native.preparation_steps baseline in
  ignore (value 42L (run ~max_initializer_steps:preparation source));
  ignore (rejection (run ~max_initializer_steps:(preparation - 1) source))

let independent_function_tasks () =
  for _ = 1 to 3 do
    let report = run source in
    ignore (value 42L report);
    Gc.full_major ();
    Gc.compact ();
    ignore (value 42L report);
    ignore
      (fault Image.Uninitialized_read (run "I64 A; I64 F(){return A;} F();"))
  done;
  let worker = Domain.spawn (fun () -> run source) in
  ignore (value 42L (run source));
  ignore (value 42L (Domain.join worker))

let transitive_closure_limits () =
  let source =
    "I64 Base(I64 n){return n+1;} I64 Middle(I64 n){return Base(n)+1;} I64 \
     Top(){return Middle(40);} Top();"
  in
  let baseline = run source in
  ignore (value 42L baseline);
  let fragments = Native.fragments baseline in
  Alcotest.(check (list int))
    "each original caller includes its exact callees" [ 1; 2; 3; 3 ]
    (List.map
       (fun (fragment : Native.fragment) -> fragment.image.function_count)
       fragments);
  let code, ir =
    List.fold_left
      (fun (code, ir) (fragment : Native.fragment) ->
        (code + fragment.image.code_bytes, ir + fragment.image.ir_instructions))
      (0, 0) fragments
  in
  ignore (value 42L (run ~max_code_bytes:code ~max_ir_instructions:ir source));
  let limited = run ~max_ir_instructions:(ir - 1) source in
  diagnostic "HCBACK0001" limited;
  Alcotest.(check int)
    "closure exhaustion preserves the original declarations" 3
    (List.length (Native.fragments limited));
  let limited = run ~max_code_bytes:(code - 1) source in
  diagnostic "HCBACK0005" limited;
  Alcotest.(check int)
    "code exhaustion does not enter the final call" 3
    (List.length (Native.fragments limited))

let unsupported_persistent_function_storage () =
  let report = run "I64 F(){return 42;} I64 (*p)()=&F; p();" in
  ignore (value 42L report);
  completed report;
  Alcotest.(check int)
    "saved executable callback enters no VM instructions" 0
    (Option.get (Native.source_progress report)).runtime.executed_steps;
  List.iter
    (fun text -> ignore (rejection (run text)))
    [
      "extern I64 Missing(); I64 F(){return Missing();} F();";
      "I64 F(F64 n=42.0){return 42;} F();";
      "I64 F(){1.0;return 42;} F();";
    ]

let retained_provider_output () =
  List.iter
    (fun (output, text) ->
      let report = run ~max_code_bytes:262_144 text in
      ignore (value ~output 42L report);
      completed report;
      let session, config, source = inputs text in
      let interpreted =
        run_integer_program_report session ~config ~source ~max_steps:100_000
      in
      let result =
        integer_program_report_outcome interpreted
        |> Result.map_error diagnostics
        |> checked
      in
      let word = Option.get (Ir_integer_interpreter.final_value result.value) in
      Alcotest.(check int64) "independent IR result" 42L word.bits;
      Alcotest.(check string)
        "independent IR bytes" output
        (integer_program_report_output_bytes interpreted))
    [
      ("A", "extern U0 PutChars(U64 ch);I64 F(){PutChars('A');return 42;}F();");
      ("AB", "extern U0 PutChars(U64 ch);U0 F(){PutChars('AB');}F();42;");
      ( "42;",
        "extern U0 Print(U8 *fmt,...);U8 Format[4]={37,100,59,0};I64 \
         F(){Print(Format,42);return 42;}F();" );
      ( "42;42;",
        "extern U0 Print(U8 *fmt,...);U8 Format[4]={37,100,59,0};I64 \
         F(){Print(Format,42);return 42;}F();I64 B=F();B;" );
    ]

let retained_static_counter () =
  List.iter
    (fun (expected, text) ->
      let report = run text in
      ignore (value expected report);
      completed report;
      let progress = Option.get (Native.source_progress report) in
      Alcotest.(check int)
        "static execution does not run IR instructions" 0
        progress.runtime.executed_steps;
      Alcotest.(check int)
        "initializer leaves have no value preparation"
        (Native.dimension_work report)
        (Native.preparation_steps report))
    [
      (43L, "I64 F(){static I64 A=41;return ++A;}F();F();");
      (42L, "I64 F(){static I64 A;A=42;return A;}F();");
      (42L, "I64 F(){static U8 A=297;return ++A;}F();");
      (43L, "I64 F(){static I64 A[2]={40,41};return ++A[1];}F();F();");
    ]

let retained_static_effects () =
  let text =
    "extern U0 PutChars(U64 ch);I64 N=40;I64 Next(){PutChars('I');return \
     ++N;}I64 F(){static I64 A=Next();return ++A;}F();F();"
  in
  let report = run text in
  ignore (value ~output:"I" 43L report);
  completed report;
  let baseline =
    run
      "extern U0 PutChars(U64 ch);I64 N=40;I64 Next(){PutChars('I');return \
       ++N;}Next();"
  in
  Alcotest.(check int)
    "static call adds no IR preparation"
    (Native.preparation_steps baseline)
    (Native.preparation_steps report);
  let progress = Option.get (Native.source_progress report) in
  Alcotest.(check int)
    "effectful static leaf runs no IR instructions" 0
    progress.runtime.executed_steps;
  List.iter
    (fun body ->
      ignore
        (value ~output:"I" 41L
           (run
              ("extern U0 PutChars(U64 ch);I64 N=40;I64 \
                Next(){PutChars('I');return ++N;}I64 F(){" ^ body ^ "}N;"))))
    [
      "static I64 A=Next();return A;";
      "if(0){static I64 A=Next();}return 0;";
      "return 0;static I64 A=Next();";
    ];
  List.iter
    (fun (expected, text) -> ignore (value expected (run text)))
    [
      (42L, "I64 F(){static I64 A=40,B=A+2;return B;}F();");
      (42L, "I64 F(){static U8 A=297;static I64 B=++A;return B;}F();");
      (42L, "I64 F(){static I64 A[2]={40,41};static I64 B=A[1]+1;return B;}F();");
    ]

let retained_static_order_and_faults () =
  let prefix =
    "extern U0 PutChars(U64 ch);I64 N=40;I64 Next(){PutChars('I');return ++N;}"
  in
  let report =
    run (prefix ^ "I64 F(){static I64 A=Next(),B=A+1;return B;}F();F();")
  in
  ignore (value ~output:"I" 42L report);
  completed report;
  let report =
    run
      (prefix ^ "I64 F(){static I64 A[2]={Next(),Next()};return A[1];}F();F();")
  in
  ignore (value ~output:"II" 42L report);
  completed report;
  let broken =
    run
      (prefix
     ^ "I64 Z=0;I64 F(){static I64 A=Next(),B=1/Z,C=Next();return C;}F();")
  in
  ignore (fault Image.Division_by_zero broken);
  Alcotest.(check string)
    "later static fault preserves earlier initializer output" "I"
    (Native.output_bytes broken);
  let malformed =
    run (prefix ^ "I64 F(){static I64 A=Next(),;return A;}F();")
  in
  ignore (rejection malformed);
  Alcotest.(check string)
    "later delimiter failure preserves live initializer output" "I"
    (Native.output_bytes malformed);
  List.iter
    (fun (kind, text) -> ignore (fault kind (run text)))
    [
      (Image.Uninitialized_read, "I64 F(){static I64 A;return A;}F();");
      (Image.Uninitialized_read, "I64 F(){static I64 A[3];return A[2];}F();");
      ( Image.Address_out_of_bounds,
        "I64 F(){static U8 A[2]={40,41};return A[2];}F();" );
    ];
  let limited =
    run ~max_global_bytes:16
      (prefix
     ^ "I64 F(){static I64 A=Next();return A;}I64 G(){static I64 \
        A=Next();return A;}G();")
  in
  diagnostic "HCIRVM0016" limited;
  Alcotest.(check string)
    "padded quota failure preserves earlier native initializer" "I"
    (Native.output_bytes limited);
  diagnostic "HCRUN0006" (run "I64 F(I64 n){static I64 A=n;return A;}F(42);");
  diagnostic "HCRUN0006"
    (run "I64 F(){I64 n=40;static I64 A=n+2;return A;}F();");
  diagnostic "HCRUN0006"
    (run "I64 F(){static I64 A[3]={40,41};return A[2];}F();")

let retained_static_history () =
  List.iter
    (fun (expected, text) -> ignore (value expected (run text)))
    [
      ( 45L,
        "I64 F(){static I64 A=40;return ++A;}F();I64 G(){static I64 \
         A=42;return ++A;}G();F()+3;" );
      ( 43L,
        "I64 F(){static I64 A=40;return ++A;}I64 G(){return F();}G();G();G();"
      );
      (42L, "I64 F(){static I8 A=255;return A+43;}F();");
      (42L, "I64 F(){static U32 A=4294967338;return A;}F();");
      ( 43L,
        "I64 F(){static I64 A=40;return ++A;}I64 Old(){return F();}Old();I64 \
         F(){static I64 A=100;return ++A;}F();Old();Old();" );
      ( 43L,
        "I64 F(){static U8 A[2][2]={{39,40},{41,42}};return ++A[1][0];}F();F();"
      );
      ( 67L,
        "I64 F(){static I64 A=0;U8 *p=\"A\";A++;p[0]++;return p[0];}F();I64 \
         B[2]={20,22};F();" );
    ]

let retained_static_limits () =
  let text = "I64 F(){static U8 A=41;return ++A;}F();F();" in
  let report = run ~max_global_bytes:8 text in
  ignore (value 43L report);
  completed report;
  let code, ir =
    List.fold_left
      (fun (code, ir) (fragment : Native.fragment) ->
        (code + fragment.image.code_bytes, ir + fragment.image.ir_instructions))
      (0, 0) (Native.fragments report)
  in
  ignore
    (value 43L
       (run ~max_global_bytes:8 ~max_code_bytes:code ~max_ir_instructions:ir
          ~max_steps:(Native.executed_steps report)
          text));
  List.iter
    (fun (code, report) -> diagnostic code report)
    [
      ("HCIRVM0016", run ~max_global_bytes:7 text);
      ("HCBACK0005", run ~max_code_bytes:(code - 1) text);
      ("HCBACK0001", run ~max_ir_instructions:(ir - 1) text);
      ("HCIRVM0007", run ~max_steps:(Native.executed_steps report - 1) text);
    ];
  let session, config, source = inputs text in
  let interpreted =
    run_integer_program_report session ~config ~source ~max_steps:100_000
  in
  let result =
    integer_program_report_outcome interpreted
    |> Result.map_error diagnostics
    |> checked
  in
  let word = Option.get (Ir_integer_interpreter.final_value result.value) in
  Alcotest.(check int64) "independent narrow static counter" 43L word.bits

let retained_static_string_copies () =
  List.iter
    (fun (expected, text) ->
      let report = run text in
      ignore (value expected report);
      completed report;
      let copies = Native.static_copies report in
      Alcotest.(check bool)
        "real direct native copy completed" true
        (copies <> []
        && List.for_all
             (fun (copy : Native.static_copy) -> Result.is_ok copy.outcome)
             copies);
      let copy_work =
        List.fold_left
          (fun n (copy : Native.static_copy) -> n + copy.byte_count)
          0 copies
      in
      Alcotest.(check bool)
        "copied bytes charge the original preparation allowance" true
        (Native.preparation_steps report
        >= Native.dimension_work report + copy_work);
      Alcotest.(check int)
        "byte copy evaluates no IR instructions" 0
        (Option.get (Native.source_progress report)).runtime.executed_steps;
      let session, config, source = inputs text in
      let interpreted =
        run_integer_program_report session ~config ~source ~max_steps:100_000
        |> integer_program_report_outcome
        |> Result.map_error diagnostics
        |> checked
      in
      Alcotest.(check int64)
        "independent source execution agrees" expected
        (Option.get (Ir_integer_interpreter.final_value interpreted.value)).bits)
    [
      (66L, "I64 F(){static U8 A[3]=\"AB\";return A[1];}F();");
      (0L, "I64 F(){static U8 A[3]=\"AB\";return A[2];}F();");
      (65L, "I64 F(){static U8 A[1]=\"ABC\";return A[0];}F();");
      (66L, "I64 F(){static I8 A[2]=\"AB\";return A[1];}F();");
      ( 67L,
        "I64 F(){static U8 A[2][2]={{A[1][0]=99,66},\"CD\"};return \
         A[1][0];}F();" );
      (0L, "I64 F(){static U8 A[1]=\"\";return A[0];}F();");
      (0L, "I64 F(){static U8 A[3]=\"A\\0B\";return A[1];}F();");
      (68L, "I64 F(){static U8 A[2][3]={\"AB\",\"CD\"};return A[1][1];}F();");
      (70L, "I64 F(){static U8 A[2][2]={\"AB\",{69,70}};return A[1][1];}F();");
      ( 68L,
        "I64 F(){static U8 A[3]=\"AB\";return ++A[1];}F();U8 Z[2]={20,22};F();"
      );
      ( 68L,
        "I64 F(){static U8 A[3]=\"AB\";return ++A[1];}I64 Old(){return \
         F();}Old();I64 F(){static U8 A[3]=\"XY\";return ++A[1];}F();Old();" );
    ];
  ignore
    (fault Image.Address_out_of_bounds
       (run "I64 F(){static U8 A[3]=\"AB\";return A[3];}F();"));
  ignore (rejection (run "I64 F(){static U8 A[4]=\"AB\";return A[0];}F();"));
  let malformed = run "I64 F(){static U8 A[3]=\"AB\",;return 0;}" in
  ignore (rejection malformed);
  Alcotest.(check int)
    "earlier direct copy survives later parsing failure" 1
    (List.length (Native.static_copies malformed));
  let ordered =
    run
      "extern U0 PutChars(U64 ch);I64 Next(){PutChars('I');return 65;}I64 \
       F(){static U8 A[2][2]={{Next(),66},\"CD\"};static I64 B=A[1][0];return \
       B;}F();F();"
  in
  ignore (value ~output:"I" 67L ordered)

let retained_static_copy_limits () =
  let text = "I64 F(){static U8 A[2][3]={\"AB\",\"CD\"};return A[1][1];}F();" in
  let report = run ~max_global_bytes:8 text in
  ignore (value 68L report);
  let work = Native.preparation_steps report in
  ignore (value 68L (run ~max_global_bytes:8 ~max_initializer_steps:work text));
  let limited = run ~max_initializer_steps:(work - 1) text in
  diagnostic "HCIRVM0007" limited;
  Alcotest.(check (list bool))
    "earlier copy succeeds before later allowance failure" [ true; false ]
    (List.map
       (fun (copy : Native.static_copy) -> Result.is_ok copy.outcome)
       (Native.static_copies limited));
  Alcotest.(check int)
    "unentered copy spends no byte allowance"
    (Native.dimension_work limited + 3)
    (Native.preparation_steps limited);
  diagnostic "HCIRVM0016" (run ~max_global_bytes:7 text);
  let report = run "I64 F(){static U8 A[3]=\"AB\";return A[1];}F();" in
  let code, ir =
    List.fold_left
      (fun (code, ir) (fragment : Native.fragment) ->
        (code + fragment.image.code_bytes, ir + fragment.image.ir_instructions))
      (0, 0) (Native.fragments report)
  in
  ignore
    (value 66L
       (run ~max_code_bytes:code ~max_ir_instructions:ir
          ~max_steps:(Native.executed_steps report)
          "I64 F(){static U8 A[3]=\"AB\";return A[1];}F();"));
  Alcotest.(check int)
    "direct copy creates no extra expression image" 2
    (List.length (Native.fragments report))

let retained_output_effect_order () =
  let text =
    "extern U0 PutChars(U64 ch);I64 N=0;I64 Left(){N++;PutChars('L');return \
     20;}I64 Right(){N++;PutChars('R');return 22;}I64 Add(I64 a,I64 \
     b){PutChars('C');return a+b;}I64 F(){return Add(Left(),Right());}F()+N-2;"
  in
  ignore (value ~output:"RLC" 42L (run text));
  let text =
    "extern U0 PutChars(U64 ch);I64 N=40;I64 Next(){PutChars('I');return \
     ++N;}I64 A[2]={Next(),Next()};A[1];"
  in
  ignore (value ~output:"II" 42L (run text));
  let text =
    "extern U0 PutChars(U64 ch);I64 Recur(I64 n){PutChars('R');if(n)return \
     Recur(n-1);return 42;}Recur(2);"
  in
  ignore (value ~output:"RRR" 42L (run text))

let retained_output_limits_and_faults () =
  let text =
    "extern U0 PutChars(U64 ch);I64 F(){PutChars('ABC');return \
     42;}PutChars('P');F();"
  in
  let report = run text in
  ignore (value ~output:"PABC" 42L report);
  let work = Native.output_work report in
  let steps = Native.executed_steps report in
  ignore
    (value ~output:"PABC" 42L
       (run ~max_output_bytes:4 ~max_output_work:work ~max_steps:steps text));
  let report = run ~max_output_bytes:3 text in
  ignore (fault Image.Output_limit_exceeded report);
  Alcotest.(check string)
    "PutChars retains reached prefix" "PAB"
    (Native.output_bytes report);
  let report = run ~max_output_work:(work - 1) text in
  ignore (fault Image.Output_work_limit_exceeded report);
  Alcotest.(check int)
    "output work consumes exact remaining allowance" (work - 1)
    (Native.output_work report);
  let report = run ~max_steps:(steps - 1) text in
  ignore (fault Image.Step_limit_exceeded report);
  Alcotest.(check string)
    "later instruction fault retains ordinary output" "PABC"
    (Native.output_bytes report);
  let report =
    run
      "extern U0 PutChars(U64 ch);I64 F(){PutChars('A');return \
       1/0;}PutChars('P');F();PutChars('Z');"
  in
  ignore (fault Image.Division_by_zero report);
  Alcotest.(check string)
    "earlier output survives reached function fault" "PA"
    (Native.output_bytes report)

let retained_print_atomic_faults () =
  let text =
    "extern U0 PutChars(U64 ch);extern U0 Print(U8 *fmt,...);U8 \
     Format[4]={37,100,59,0};I64 F(){Print(Format,42);return \
     42;}PutChars('P');F();"
  in
  let report = run text in
  ignore (value ~output:"P42;" 42L report);
  let work = Native.output_work report in
  ignore
    (value ~output:"P42;" 42L
       (run ~max_output_bytes:4 ~max_output_work:work text));
  let report = run ~max_output_bytes:3 text in
  ignore (fault Image.Output_limit_exceeded report);
  Alcotest.(check string)
    "Print publishes no partial draft" "P"
    (Native.output_bytes report);
  let report = run ~max_output_work:(work - 1) text in
  ignore (fault Image.Output_work_limit_exceeded report);
  Alcotest.(check string)
    "Print work failure retains earlier output" "P"
    (Native.output_bytes report);
  let report =
    run
      "extern U0 PutChars(U64 ch);extern U0 Print(U8 *fmt,...);U8 \
       Format[3]={65,37,115};U8 Text[2]={66,0};I64 \
       F(){Print(Format,Text);return 42;}PutChars('P');F();"
  in
  ignore (fault Image.Address_out_of_bounds report);
  Alcotest.(check string)
    "faulting format scan publishes no draft" "P"
    (Native.output_bytes report)

let retained_provider_history () =
  let text =
    "extern U0 Print(U8 *fmt,...);U8 Format[4]={37,100,59,0};I64 \
     F(){Print(Format,42);return 42;}U8 Format[2]={88,0};F();"
  in
  ignore (value ~output:"42;" 42L (run text));
  let text =
    "extern U0 PutChars(U64 ch);I64 F(){PutChars('A');return 42;}I64 \
     Old(){return F();}I64 F(){PutChars('N');return 100;}Old();"
  in
  ignore (value ~output:"A" 42L (run text));
  let text =
    "I64 A=0;U0 PutChars(U64 ch){A++;}I64 F(){PutChars('A');return 41;}F()+A;"
  in
  ignore (value 42L (run text));
  let text =
    "extern U0 PutChars(U64 ch);I64 A=0;I64 F(){PutChars('A');return 42;}U0 \
     PutChars(U64 ch){A++;}F();"
  in
  let report = run text in
  ignore (value 42L report);
  completed report;
  Alcotest.(check string)
    "the joined body supplies the original provider slot" ""
    (Native.output_bytes report)

let retained_provider_compilation_limits () =
  let text =
    "extern U0 PutChars(U64 ch);extern U0 Print(U8 *fmt,...);U8 \
     Format[4]={37,100,59,0};I64 Emit(){PutChars('A');Print(Format,42);return \
     42;}Emit();"
  in
  let report = run text in
  ignore (value ~output:"A42;" 42L report);
  let code, ir =
    List.fold_left
      (fun (code, ir) (fragment : Native.fragment) ->
        (code + fragment.image.code_bytes, ir + fragment.image.ir_instructions))
      (0, 0) (Native.fragments report)
  in
  ignore
    (value ~output:"A42;" 42L
       (run ~max_code_bytes:code ~max_ir_instructions:ir text));
  List.iter
    (fun (diagnostic_code, report) ->
      diagnostic diagnostic_code report;
      Alcotest.(check string)
        "rejected caller publishes no output" ""
        (Native.output_bytes report);
      Alcotest.(check int)
        "earlier initializer and declaration fragments remain" 5
        (List.length (Native.fragments report)))
    [
      ("HCBACK0005", run ~max_code_bytes:(code - 1) text);
      ("HCBACK0001", run ~max_ir_instructions:(ir - 1) text);
    ]

let retained_live_defaults () =
  List.iter
    (fun (expected, text) ->
      let report = run text in
      ignore (value expected report);
      completed report;
      Alcotest.(check int)
        "default execution runs no interpreted instructions" 0
        (Option.get (Native.source_progress report)).runtime.executed_steps;
      let defaults =
        List.filter
          (fun (fragment : Native.fragment) -> fragment.kind = Native.Default)
          (Native.fragments report)
      in
      Alcotest.(check bool)
        "original default enters a real native image" true (defaults <> []);
      let session, config, source = inputs text in
      let ir =
        run_integer_program_report session ~config ~source ~max_steps:100_000
      in
      let result =
        integer_program_report_outcome ir
        |> Result.map_error diagnostics
        |> checked
      in
      Alcotest.(check int64)
        "independent IR saved-default result" expected
        (Option.get (Ir_integer_interpreter.final_value result.value)).bits)
    [
      (42L, "I64 Seed(){return 41;}I64 F(I64 n=Seed()){return n+1;}F();");
      ( 123L,
        "I64 N=40;I64 Seed(){return ++N;}I64 F(I64 n=Seed()){return n;}I64 \
         A=F();F()+A+N;" );
      (2L, "I64 N=0;I64 F(I64 n=++N){return n;}F(99);F();F()+N;");
      (1L, "I64 N=0;I64 F(I64 n=++N){return n;}N;");
      ( 42L,
        "I64 N=40;I64 A(I64 n=++N){return n;}I64 B(I64 n=A()){return n+1;}B();"
      );
      ( 45L,
        "I64 N=40;I64 F(I64 n=++N){return n;}I64 Old(){return F();}I64 N=1;I64 \
         F(I64 n=++N){return n;}Old()+F()+N;" );
      (255L, "I64 F(U8 n=511){return n;}F();");
      (-1L, "I64 F(I8 n=511){return n;}F();");
      (2L, "I64 N=0;I64 F(I64 n=++N){if(n<=0)return 0;return F(n-1)+1;}F()+N;");
      (42L, "I64 Seed(){return 40;}I64 F(I64 n=Seed()){return n+2;}I64 A=F();A;");
      ( 42L,
        "I64 Seed(){return 40;}I64 F(I64 n=Seed()){return n+2;}I64 G(){static \
         I64 A=F();return A;}G();" );
      (42L, "I64 Seed(){return 40;}I64 F(I64 a=Seed(),I64 b=2){return a+b;}F();");
    ]

let retained_default_effects_and_faults () =
  let prefix =
    "extern U0 PutChars(U64 ch);I64 Seed(){PutChars('A');return 41;}"
  in
  let text =
    prefix ^ "I64 F(I64 n=Seed()){return n+1;}PutChars('P');F();F();"
  in
  let report = run text in
  ignore (value ~output:"AP" 42L report);
  completed report;
  let steps = Native.executed_steps report in
  ignore (value ~output:"AP" 42L (run ~max_steps:steps text));
  let broken =
    run
      "extern U0 PutChars(U64 ch);I64 Seed(){PutChars('A');return 1/0;}I64 \
       F(I64 n=Seed()){return n;}PutChars('Z');F();"
  in
  ignore (fault Image.Division_by_zero broken);
  Alcotest.(check string)
    "fault preserves declaration-time output" "A"
    (Native.output_bytes broken);
  Alcotest.(check bool)
    "fault belongs to original default expression" true
    ((List.hd (List.rev (Native.fragments broken))).kind = Native.Default);
  let parsed = run (prefix ^ "I64 F(I64 n=Seed()){") in
  ignore (rejection parsed);
  Alcotest.(check string)
    "later parse failure preserves original default effects" "A"
    (Native.output_bytes parsed);
  let report =
    run ~max_output_bytes:1
      (prefix ^ "I64 F(I64 a=Seed(),I64 b=Seed()){return a+b;}F();")
  in
  ignore (fault Image.Output_limit_exceeded report);
  Alcotest.(check string)
    "second default shares output allowance" "A"
    (Native.output_bytes report)

let retained_default_limits () =
  let text =
    "I64 N=40;I64 Seed(){return ++N;}I64 F(I64 a=Seed(),I64 b=2){return \
     a+b;}F();"
  in
  let report = run text in
  ignore (value 43L report);
  let preparation = Native.preparation_steps report in
  Alcotest.(check int)
    "each default retains one full word" 16
    (Native.default_bytes report);
  ignore (value 43L (run ~max_default_bytes:16 text));
  let payload_limited = run ~max_default_bytes:15 text in
  diagnostic "HCIRVM0011" payload_limited;
  Alcotest.(check int)
    "earlier saved word remains after quota rejection" 8
    (Native.default_bytes payload_limited);
  let code, ir =
    List.fold_left
      (fun (code, ir) (fragment : Native.fragment) ->
        (code + fragment.image.code_bytes, ir + fragment.image.ir_instructions))
      (0, 0) (Native.fragments report)
  in
  ignore
    (value 43L
       (run ~max_initializer_steps:preparation
          ~max_steps:(Native.executed_steps report)
          ~max_code_bytes:code ~max_ir_instructions:ir text));
  let stopped = run ~max_initializer_steps:(preparation - 1) text in
  ignore (fault Image.Step_limit_exceeded stopped);
  Alcotest.(check int)
    "default quota stops at exact preparation count" (preparation - 1)
    (Native.preparation_steps stopped);
  Alcotest.(check bool)
    "default quota is a real reached native fault" true
    ((List.hd (List.rev (Native.fragments stopped))).kind = Native.Default);
  List.iter
    (fun (code, report) -> diagnostic code report)
    [
      ("HCBACK0005", run ~max_code_bytes:(code - 1) text);
      ("HCBACK0001", run ~max_ir_instructions:(ir - 1) text);
      ("HCIRVM0007", run ~max_steps:(Native.executed_steps report - 1) text);
    ];
  Gc.full_major ();
  ignore (value 43L (run text))

let retained_extern_slots () =
  List.iter
    (fun text ->
      let report = run text in
      ignore (value 42L report);
      completed report;
      Alcotest.(check int)
        "slot code executes no VM instructions" 0
        (Option.get (Native.source_progress report)).runtime.executed_steps;
      let session, config, source = inputs text in
      let oracle =
        run_integer_program_report session ~config ~source ~max_steps:100_000
      in
      let result =
        integer_program_report_outcome oracle
        |> Result.map_error diagnostics
        |> checked
      in
      Alcotest.(check int64)
        "independent original slot result" 42L
        (Option.get (Ir_integer_interpreter.final_value result.value)).bits)
    [
      "extern I64 Answer();I64 Old(){return Answer();}I64 Answer(){return \
       42;}Old();";
      "extern I64 Missing();I64 Old(){return Missing();}42;";
      "extern I64 Answer();I64 Old(I64 n){if(n)return Answer();return \
       42;}Old(0);I64 Answer(){return 42;}Old(1);";
      "extern I64 Answer(I64 n=41);I64 Old(){return Answer();}I64 Answer(I64 \
       n){return n+1;}I64 Answer(I64 n){return 100;}Old();";
      "extern I64 Answer(I64 n,...);I64 Old(){return Answer(1,20,21);}I64 \
       Answer(I64 n,...){return n+argv[0]+argv[1];}Old();";
      "extern I8 Answer(U8 n);I64 Old(){return Answer(298);}I8 Answer(U8 \
       n){return n;}Old();";
      "extern U64 Answer(U64 n);I64 Old(){return \
       Answer(0xffffffffffffffff);}U64 Answer(U64 n){return n+43;}Old();";
      "extern I64 Answer();I64 Left(){return Answer();}I64 Right(){return \
       Answer();}I64 Answer(){return 21;}Left()+Right();";
      "extern I64 Answer(I64 n);I64 Old(I64 n){return Answer(n);}I64 \
       Answer(I64 n){if(n)return Old(n-1);return 42;}Old(3);";
      "extern U0 Answer();I64 A=41;U0 Old(){Answer();}U0 Answer(){A++;}Old();A;";
      "extern I64 Answer(U8 *p);I64 Old(){U8 A[2];A[0]=41;A[1]=1;return \
       Answer(A);}I64 Answer(U8 *p){return p[0]+p[1];}Old();";
      "extern U0 Print(U8 *fmt,...);I64 A=40;I64 \
       Old(){Print(\"unused\",1,2);return A;}U0 Print(U8 \
       *fmt,...){A+=argc;}Old();";
      "extern I64 Answer();I64 Old(){return Answer();}I64 Answer(){return \
       41;}I64 A=Old();A+1;";
      "extern I64 Answer();I64 Old(){return Answer();}I64 Answer(){return \
       41;}I64 F(I64 n=Old()){return n+1;}F();";
      "extern I64 Answer();I64 Old(){return Answer();}I64 Answer(){return \
       41;}I64 F(){static I64 A=Old();return ++A;}F();";
    ];
  Gc.full_major ();
  ignore
    (value 42L
       (run
          "extern I64 Answer();I64 Old(){return Answer();}I64 Answer(){return \
           42;}Old();"))

let retained_extern_faults () =
  List.iter
    (fun (kind, code, text) ->
      let report = run text in
      diagnostic code report;
      let native_fault = fault kind report in
      Alcotest.(check (option string))
        "original caller owns the fault" (Some "Old") native_fault.function_name;
      Alcotest.(check string)
        "arguments precede slot failure in reverse order" "BA"
        (Native.output_bytes report);
      Alcotest.(check int)
        "no interpreter fallback for slot failure" 0
        (Option.get (Native.source_progress report)).runtime.executed_steps;
      let session, config, source = inputs text in
      let oracle =
        run_integer_program_report session ~config ~source ~max_steps:100_000
      in
      Alcotest.(check bool)
        "independent IR slot diagnostic" true
        (match integer_program_report_outcome oracle with
        | Error errors ->
            List.exists (fun (error : Diagnostic.t) -> error.code = code) errors
        | Ok _ -> false);
      Alcotest.(check string)
        "independent IR preserves argument output" "BA"
        (integer_program_report_output_bytes oracle);
      diagnostic code (run ~max_steps:(Native.executed_steps report) text);
      diagnostic "HCIRVM0007"
        (run ~max_steps:(Native.executed_steps report - 1) text))
    [
      ( Image.Undefined_extern,
        "HCIRVM0030",
        "extern I64 Answer(I64 a,I64 b);extern U0 PutChars(U64 ch);I64 \
         A(){PutChars('A');return 1;}I64 B(){PutChars('B');return 2;}I64 \
         Old(){return Answer(A(),B());}Old();I64 Answer(I64 a,I64 b){return \
         42;}" );
      ( Image.Extern_signature_mismatch,
        "HCIRVM0014",
        "extern I64 Answer(I64 a,I64 b);extern U0 PutChars(U64 ch);I64 \
         A(){PutChars('A');return 1;}I64 B(){PutChars('B');return 2;}I64 \
         Old(){return Answer(A(),B());}I64 Answer(U8 a,I64 b){return \
         42;}Old();" );
    ]

let retained_extern_limits () =
  let text =
    "extern I64 Answer(I64 n);I64 Old(I64 n){return Answer(n);}I64 Answer(I64 \
     n){if(n)return Old(n-1);return 42;}Old(3);"
  in
  let report = run text in
  ignore (value 42L report);
  let code, ir =
    List.fold_left
      (fun (code, ir) (fragment : Native.fragment) ->
        (code + fragment.image.code_bytes, ir + fragment.image.ir_instructions))
      (0, 0) (Native.fragments report)
  in
  ignore
    (value 42L
       (run ~max_code_bytes:code ~max_ir_instructions:ir
          ~max_steps:(Native.executed_steps report)
          text));
  ignore (value 42L (run ~max_frame_bytes:64 ~max_call_depth:8 text));
  ignore (fault Image.Frame_limit_exceeded (run ~max_frame_bytes:63 text));
  ignore (fault Image.Call_depth_exceeded (run ~max_call_depth:7 text));
  diagnostic "HCBACK0005" (run ~max_code_bytes:(code - 1) text);
  diagnostic "HCBACK0001" (run ~max_ir_instructions:(ir - 1) text);
  ignore (fault Image.Call_depth_exceeded (run ~max_call_depth:1 text));
  ignore (fault Image.Frame_limit_exceeded (run ~max_frame_bytes:8 text));
  Alcotest.(check int)
    "cyclic source closure is bounded and deduplicated" 2
    (List.hd (List.rev (Native.fragments report))).image.function_count

let () =
  Alcotest.run "Native source functions"
    [
      ( "retained source",
        [
          Alcotest.test_case "original joined extern slots and saved headers"
            `Quick retained_extern_slots;
          Alcotest.test_case
            "reached extern faults and original argument effects" `Quick
            retained_extern_faults;
          Alcotest.test_case
            "cyclic extern closures and cumulative native limits" `Quick
            retained_extern_limits;
          Alcotest.test_case "original definition and direct call" `Quick
            original_definition_and_call;
          Alcotest.test_case "direct forms and automatic frames" `Quick
            direct_source_forms;
          Alcotest.test_case "declared parameter and return widths" `Quick
            declared_function_widths;
          Alcotest.test_case "original initializer calls and arrays" `Quick
            original_initializers_and_arrays;
          Alcotest.test_case "historical globals and function bodies" `Quick
            retained_source_ownership;
          Alcotest.test_case "once-only argument effects and recursion" `Quick
            once_only_calls_and_recursion;
          Alcotest.test_case "original function faults and depth" `Quick
            reached_function_faults;
          Alcotest.test_case
            "retained integer variadic words and original count" `Quick
            retained_word_tails;
          Alcotest.test_case "cumulative source function limits" `Quick
            cumulative_function_limits;
          Alcotest.test_case "collection and independent function tasks" `Quick
            independent_function_tasks;
          Alcotest.test_case
            "transitive closure and cumulative compilation limits" `Quick
            transitive_closure_limits;
          Alcotest.test_case "persistent function storage boundaries" `Quick
            unsupported_persistent_function_storage;
          Alcotest.test_case "live native static storage and retained counter"
            `Quick retained_static_counter;
          Alcotest.test_case "live native static initializer effects" `Quick
            retained_static_effects;
          Alcotest.test_case "native static order, faults and padded quotas"
            `Quick retained_static_order_and_faults;
          Alcotest.test_case "native static historical bodies and widths" `Quick
            retained_static_history;
          Alcotest.test_case "native static exact and one-below limits" `Quick
            retained_static_limits;
          Alcotest.test_case "native static original byte-string copies" `Quick
            retained_static_string_copies;
          Alcotest.test_case "native static byte-copy exact allowances" `Quick
            retained_static_copy_limits;
          Alcotest.test_case "retained ordinary output providers" `Quick
            retained_provider_output;
          Alcotest.test_case "retained output effects and original order" `Quick
            retained_output_effect_order;
          Alcotest.test_case "retained output limits and faults" `Quick
            retained_output_limits_and_faults;
          Alcotest.test_case "retained Print atomic drafts and faults" `Quick
            retained_print_atomic_faults;
          Alcotest.test_case "original providers, formats and joined bodies"
            `Quick retained_provider_history;
          Alcotest.test_case "provider closures and cumulative compile limits"
            `Quick retained_provider_compilation_limits;
          Alcotest.test_case "live declaration defaults and saved header calls"
            `Quick retained_live_defaults;
          Alcotest.test_case "native default effects and reached faults" `Quick
            retained_default_effects_and_faults;
          Alcotest.test_case
            "native default exact preparation and execution limits" `Quick
            retained_default_limits;
        ] );
    ]
