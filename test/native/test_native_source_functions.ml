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
    ?max_initializer_steps ?max_global_bytes ?max_frame_bytes ?max_call_depth
    ?max_active_stack_bytes text =
  let session, config, source = inputs text in
  Native.evaluate ?max_ir_instructions ?max_code_bytes ?max_initializer_steps
    ?max_global_bytes ?max_frame_bytes ?max_call_depth ?max_active_stack_bytes
    session ~config ~source ~max_steps

let value expected report =
  let result =
    Native.outcome report |> Result.map_error diagnostics |> checked
  in
  let word = Option.get result.value.final_value in
  Alcotest.(check int64) "original native function result" expected word.bits;
  Alcotest.(check string)
    "quiet source task output" ""
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
  | _ -> Alcotest.fail "function failure has no native fault outcome"

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
    "all fragments share the original arena" [ 9; 9; 9 ]
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
  List.iter
    (fun text -> ignore (rejection (run text)))
    [
      "I64 F(){static I64 A=42;return A;} F();";
      "I64 F(){static I64 A;A=42;return A;} F();";
      "extern I64 Missing(); I64 F(){return Missing();} F();";
      "extern I64 Later(); I64 F(){return Later();} I64 Later(){return 42;} \
       F();";
      "I64 F(){return (\"*\")[0];} F();";
      "I64 F(I64 n=42){return n;} F();";
      "I64 F(){return 42;} I64 (*p)()=&F; p();";
      "I64 F(){1.0;return 42;} F();";
    ]

let () =
  Alcotest.run "Native source functions"
    [
      ( "retained source",
        [
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
        ] );
    ]
