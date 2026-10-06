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

let storage_and_history () =
  List.iter (agrees 42L)
    [
      "I64 F(){return 42;}I64 (*p)()=&F;p();p();";
      "I64 F(){return 42;}I64 (*p)()=&F;I64 F(){return 17;}p();";
      "I64 F(){return 42;}I64 (*p)()=&F;I64 (*q)()=p;p=0;q();";
      "I64 F(){return 42;}I64 \
       (*p)()[2][2]={{0,&F},{0,0}};p[1][1]=p[0][1];p[0][1]=0;p[1][1]();";
      "I64 F(){return 42;}I64 G(){I64 (*p)();p=&F;return p();}G();";
      "I64 F(){return 42;}I64 G(){I64 (*p)()[2];p[1]=&F;return p[1]();}G();";
      "I64 F(){return 42;}I64 Call(I64 (*p)()){return p();}Call(&F);";
      "I64 F(){return 42;}I64 (*p)()=&F;I64 Call(I64 (*q)()){return \
       q();}Call(p);";
      "I64 F(I64 n){if(n)return F(n-1)+1;return 40;}I64 (*p)(I64 n)=&F;p(2);";
      "I64 F(){return 42;}I64 (*p)()=&F;I64 (*q)()=42;I64 Read(I64 \
       (*x)()){return x;}Read(q);";
      "I64 F(){return 42;}I64 (*p)()=&F;p=42;I64 Read(I64 (*x)()){return \
       x;}Read(p);";
    ];
  agrees 1L "I64 F(){return 42;}I64 (*p)()=&F;I64 (*q)()=p;p==q;";
  agrees 0L "I64 F(){return 42;}I64 (*p)()=&F;I64 F(){return 17;}p==&F;";
  agrees 1L "I64 F(){return 42;}I64 (*p)()=&F;p==&F;";
  for _ = 1 to 5 do
    Gc.full_major ();
    agrees 42L "I64 F(){return 42;}I64 (*p)()=&F;I64 (*q)()=p;p=0;q();"
  done

let capture_and_signatures () =
  let prefix =
    "extern U0 PutChars(U64 ch);I64 F(I64 a,I64 b){return 42;}I64 G(I64 a,I64 \
     b){return 17;}I64 (*p)(I64 a,I64 b)=&F;I64 Mark(I64 \
     c){PutChars(c);p=&G;return c;}"
  in
  List.iter
    (fun suffix ->
      let report = run (prefix ^ suffix) in
      value 42L report;
      Alcotest.(check string)
        "original callee capture precedes reverse arguments" "BA"
        (Native.output_bytes report))
    [
      "p(Mark(65),Mark(66));"; "I64 Call(){return p(Mark(65),Mark(66));}Call();";
    ];
  let report =
    run
      "extern U0 PutChars(U64 ch);I64 F(I64 x){return x;}I64 (*p)(I64 a,I64 \
       b)=&F;I64 Mark(I64 c){PutChars(c);return c;}p(Mark(65),Mark(66));"
  in
  diagnostic "HCIRVM0014" report;
  fault Image.Callback_signature_mismatch report;
  Alcotest.(check string)
    "reached mismatch preserves completed reverse arguments" "BA"
    (Native.output_bytes report);
  let report =
    run
      "extern U0 PutChars(U64 ch);I64 (*p)(I64 x)=0;I64 F(I64 x){return \
       42;}I64 Mark(){p=&F;PutChars('A');return 0;}p(Mark());"
  in
  diagnostic "HCIRVM0024" report;
  fault Image.Callback_unowned_address report;
  Alcotest.(check string)
    "numeric capture is not retargeted by an argument" "A"
    (Native.output_bytes report)

let source_consumers_and_shapes () =
  List.iter (agrees 42L)
    [
      "I64 F(){return 42;}I64 (*p)()=&F;I64 A=p();A;";
      "I64 F(){return 42;}I64 (*p)()=&F;I64 Call(){return p();}I64 G(I64 \
       x=Call()){return x;}G();G();";
      "I64 N=0;U0 F(){N=42;}U0 (*p)()=&F;p();N;";
      "I64 F(...){return argc+argv[0]+argv[1];}I64 (*p)(...)=&F;p(20,20);";
      "I64 A=42;I64 F(I64 *q){return *q;}I64 (*p)(I64 *q)=&F;p(&A);";
    ];
  value 42L
    (run
       "I64 F(){return 42;}I64 (*p)()=&F;I64 G(){static I64 A=p();return \
        A;}G();G();");
  value 42L
    (run
       "extern I64 Next();I64 F(){return Next();}I64 (*p)()=&F;I64 \
        Next(){return 42;}p();");
  agrees 42L "I64 F(I64 x){return x+1;}I64 (*p)(I64 x=41)=&F;p();";
  agrees 3L "I64 F(...){return argc;}I64 (*p)(...)=&F;p(9);p(1,2,3);";
  agrees 42L
    "I64 F(){return 42;}I64 Apply(I64 (*q)()){return q();}I64 (*p)(I64 \
     (*q)())=&Apply;p(&F);";
  List.iter
    (fun type_ ->
      agrees 42L
        (Printf.sprintf "%s F(%s x){return x;}%s (*p)(%s x)=&F;p(42);" type_
           type_ type_ type_))
    [ "I8"; "U8"; "I16"; "U16"; "I32"; "U32"; "I64"; "U64" ];
  agrees (-1L) "U64 F(){return 0xffffffffffffffff;}U64 (*p)()=&F;p();";
  let report = run "I64 F(){return 42;}I64 (*p)()=&F;p;" in
  let result = Native.outcome report |> Result.map_error describe |> checked in
  Alcotest.(check bool)
    "bare owner is not captured as integer bits" true
    (result.value.final_value = None);
  let report = run "I64 F(){return 42;}&F;" in
  let result = Native.outcome report |> Result.map_error describe |> checked in
  Alcotest.(check bool)
    "metadata-only owner does not export address bits" true
    (result.value.final_value = None)

let empty_result_capture () =
  List.iter
    (fun text ->
      let report = run text in
      let native =
        Native.outcome report |> Result.map_error describe |> checked
      in
      Alcotest.(check bool)
        "executed empty expression clears the earlier native result" true
        (native.value.final_value = None);
      let session, config, source = inputs text in
      let interpreted =
        run_integer_program_report session ~config ~source ~max_steps:100_000
      in
      let result =
        integer_program_report_outcome interpreted
        |> Result.map_error describe |> checked
      in
      Alcotest.(check bool)
        "original IR empty result" true
        (Ir_integer_interpreter.final_value result.value = None))
    [
      "I64 F(){return 17;}I64 (*p)()=&F;42;p;";
      "I64 F(){return 17;}42;&F;";
      "I64 F(){return 17;}I64 (*p)()=&F;42;p=&F;";
      "U0 F(){}42;F();";
      "I64 A=17;42;&A;";
    ];
  agrees 42L "I64 F(){return 17;}I64 (*p)()=&F;42;if(0)p;";
  agrees 42L "I64 F(){return 17;}I64 (*p)()=&F;42;I64 G(){return 0;}";
  agrees 42L "I64 F(){return 17;}I64 (*p)()=&F;p;42;"

let reached_faults_and_no_integer_escape () =
  List.iter
    (fun text ->
      let report = run text in
      diagnostic "HCIRVM0024" report;
      fault Image.Callback_owned_word_escape report)
    [
      "I64 F(){return 42;}I64 (*p)()=&F;I64 Read(I64 (*q)()){return q;}Read(p);";
      "I64 F(){return 42;}I64 (*p)()=&F;I64 Read(I64 x){return x;}Read(p);";
    ];
  fault Image.Callback_update_owned_address
    (run "I64 F(){return 42;}I64 (*p)()=&F;++p;");
  fault Image.Code_comparison_invalid_word
    (run "I64 F(){return 42;}I64 (*p)()=&F;p==42;");
  let report =
    run
      "extern U0 PutChars(U64 ch);I64 Z=0;I64 F(){PutChars('F');return \
       1/Z;}I64 (*p)()=&F;p();"
  in
  diagnostic "HCIRVM0009" report;
  fault Image.Division_by_zero report;
  Alcotest.(check string)
    "body fault retains output" "F"
    (Native.output_bytes report);
  match List.rev (Native.fragments report) with
  | { native_outcome = Some (Ok (Image.Fault fault)); _ } :: _ ->
      Alcotest.(check (option string))
        "fault table names actual original body" (Some "F") fault.function_name
  | _ -> Alcotest.fail "expected original body fault"

let quotas_include_entries () =
  let text = "I64 F(){return 42;}I64 (*p)()=&F;p();" in
  let report = run ~max_global_bytes:8 text in
  value 42L report;
  Alcotest.(check (list int))
    "address and target cells belong to arena" [ 0; 33; 33 ]
    (List.map
       (fun (fragment : Native.fragment) -> fragment.image.global_arena_bytes)
       (Native.fragments report));
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
  diagnostic "HCBACK0005" (run ~max_code_bytes:(code - 1) text);
  diagnostic "HCBACK0001" (run ~max_ir_instructions:(ir - 1) text);
  fault Image.Step_limit_exceeded
    (run ~max_steps:(Native.executed_steps report - 1) text);
  diagnostic "HCIRVM0016" (run ~max_global_bytes:7 text);
  value 42L
    (run ~max_global_bytes:1
       "I64 F(){return 42;}I64 Call(I64 (*p)()){return p();}Call(&F);")

let () =
  Alcotest.run "Native source executable callbacks"
    [
      ( "original owners",
        [
          Alcotest.test_case "storage, copies and original history" `Quick
            storage_and_history;
          Alcotest.test_case "capture timing and reached signature checks"
            `Quick capture_and_signatures;
          Alcotest.test_case "source consumers and supported call shapes" `Quick
            source_consumers_and_shapes;
          Alcotest.test_case "reached empty-result capture and original latch"
            `Quick empty_result_capture;
          Alcotest.test_case "body faults and integer escape guards" `Quick
            reached_faults_and_no_integer_escape;
          Alcotest.test_case "exact quotas include stable entries" `Quick
            quotas_include_entries;
        ] );
    ]
