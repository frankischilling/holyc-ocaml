open Holyc_lib
module Program = X86_64_program
module Runtime = Native_program_execution
module VM = Ir_integer_interpreter

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let diagnostics errors =
  errors
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let inputs mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-callback-dereference.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> checked
  in
  (session, config, source)

let native ?(max_steps = 100_000) mode contents =
  let session, config, source = inputs mode contents in
  Native_program.evaluate session ~config ~source ~max_steps

let ir mode contents =
  let session, config, source = inputs mode contents in
  run_integer_program_report session ~config ~source ~max_steps:100_000

let native_value expected report =
  let result =
    Native_program.outcome report |> Result.map_error diagnostics |> checked
    |> fun checked -> checked.value.execution
  in
  Alcotest.(check int64)
    "native final word" expected (Option.get result.final_value).bits;
  result.executed_steps

let compare mode contents expected output =
  let public = ir mode contents in
  let result =
    integer_program_report_outcome public
    |> Result.map_error diagnostics
    |> checked
    |> fun checked -> checked.value
  in
  Alcotest.(check int64)
    "public IR final word" expected (Option.get (VM.final_value result)).bits;
  Alcotest.(check string)
    "public IR output" output
    (integer_program_report_output_bytes public);
  let report = native mode contents in
  let steps = native_value expected report in
  Alcotest.(check string)
    "native output" output
    (Native_program.output_bytes report);
  steps

let public_steps mode contents =
  let report = ir mode contents in
  integer_program_report_outcome report
  |> Result.map_error diagnostics
  |> checked
  |> fun checked -> checked.value |> VM.executed_steps

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]
let target = "I64 Target(I64 n){return n+2;}"

let expect_error expected = function
  | Ok _ -> Alcotest.fail "expected a checked execution failure"
  | Error errors ->
      Alcotest.(check bool)
        ("expected " ^ expected ^ "; received " ^ diagnostics errors)
        true
        (List.exists
           (fun (error : Diagnostic.t) -> error.code = expected)
           errors)

let storage_and_signatures () =
  let sources =
    [
      target ^ "I64 Run(){I64 (*p)(I64 n);p=&Target;return (*p)(40);}Run;";
      target ^ "I64 Run(I64 (*p)(I64 n)){return ((*p))(40);}Run(&Target);";
      target
      ^ "I64 Run(){static I64 (*p)(I64 n);p=&Target;return (*p)(40);}Run;Run;";
      target ^ "I64 (*p)(I64 n);p=&Target;(*p)(40);";
      target ^ "I64 (*p)(I64 n);I64 Run(){p=&Target;return (*p)(40);}Run;";
      target ^ "I64 Run(){I64 (*p)(I64 n=40);p=&Target;return (*p)();}Run;";
      "I64 Target(I64 n,...){return n+argc+argv[0];}"
      ^ "I64 Run(){I64 (*p)(I64 n,...);p=&Target;return (*p)(39,2);}Run;";
      "I64 G=0;U0 Target(I64 n){G=n;}"
      ^ "I64 Run(){U0 (*p)(I64 n);p=&Target;(*p)(42);return G;}Run;";
      target
      ^ "I64 Run(){I64 (*p)(I64 n);I64 (*q)(I64 n);*p=&Target;q=*p;return \
         q(40);}Run;";
      "I64 Run(){F64 (*p)(I64 n);*p=42;if(*p==42)return 42;return 0;}Run;";
      "I64 Run(){U8 *(*p)(I64 n);*p=42;if(*p==42)return 42;return 0;}Run;";
    ]
  in
  List.iter
    (fun mode ->
      List.iter (fun source -> ignore (compare mode source 42L "")) sources)
    modes

let callee_capture_and_argument_order () =
  let source =
    "extern U0 PutChars(U64 ch);I64 (*p)(I64 a,I64 b);"
    ^ "I64 Old(I64 a,I64 b){PutChars('C');return a+b;}"
    ^ "I64 New(I64 a,I64 b){PutChars('N');return 99;}"
    ^ "I64 Left(){PutChars('L');return 20;}"
    ^ "I64 Right(){p=&New;PutChars('R');return 22;}"
    ^ "p=&Old;(*p)(Left(),Right());"
  in
  List.iter (fun mode -> ignore (compare mode source 42L "RLC")) modes

let indexed_reads_stores_and_updates () =
  let rows =
    [
      ( "automatic callback copy",
        target
        ^ "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 \
           n)[2],(*q)(I64 n);p[1]=&Target;N=0;q=*p[Index()];return \
           (q==&Target)*40+N+1;}Run();" );
      ( "static two-dimensional callback equality",
        target
        ^ "I64 N;I64 Index(){N++;return 1;}I64 Run(){static I64 (*p)(I64 \
           n)[2][2];p[1][1]=&Target;N=0;return \
           ((*p[Index()][Index()])==&Target)*40+N;}Run();" );
      ( "automatic callback store",
        target
        ^ "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 \
           n)[2];N=0;*p[Index()]=&Target;return p[1](39)+N;}Run();" );
      ( "automatic numeric update result",
        "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 \
         n)[2];p[1]=34;N=0;return (++*p[Index()])+N-1;}Run();" );
      ( "static two-dimensional numeric update",
        "I64 N;I64 Index(){N++;return 1;}I64 Run(){static I64 (*p)(I64 \
         n)[2][2];p[1][1]=34;N=0;*p[Index()][Index()]+=1;return \
         (p[1][1]==42)*40+N;}Run();" );
      ( "pointer-return callback numeric update",
        "I64 N;I64 Index(){N++;return 1;}I64 Run(){F64 **(*p)(I64 \
         n)[2];p[1]=34;N=0;return (++*p[Index()])+N-1;}Run();" );
      ( "signed add consumer keeps callback stride and raw sign",
        "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 n)[2];I64 \
         one=1;p[1]=0x7ffffffffffffff0;N=0;return \
         (((++*p[Index()])+one)<0)*40+(p[1]==0x7ffffffffffffff8)+N;}Run();" );
      ( "unsigned add consumer keeps callback stride and U64 shift",
        "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 n)[2];U64 \
         one=1;p[1]=0x7ffffffffffffff0;N=0;return \
         (((((++*p[Index()])+one)>>63)==1)*40)+(p[1]==0x7ffffffffffffff8)+N;}Run();"
      );
      ( "signed subtract consumer keeps callback stride",
        "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 n)[2];I64 \
         one=1;p[1]=0x7fffffffffffffff;N=0;return \
         (((++*p[Index()])-one)>0)*40+(p[1]==0x8000000000000007)+N;}Run();" );
      ( "unsigned subtract consumer keeps callback stride",
        "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 n)[2];U64 \
         one=1;p[1]=0x7fffffffffffffff;N=0;return \
         (((((++*p[Index()])-one)>>63)==0)*40)+(p[1]==0x8000000000000007)+N;}Run();"
      );
      ( "nested signed add subtract chain keeps update result class",
        "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 n)[2];I64 \
         one=1;p[1]=0x7ffffffffffffff0;N=0;return \
         (((((((++*p[Index()])+one)-one)+one)>>63)==-1)*40)+(p[1]==0x7ffffffffffffff8)+N;}Run();"
      );
      ( "nested unsigned add subtract chain keeps U64 class",
        "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 n)[2];U64 \
         one=1;p[1]=0x7ffffffffffffff0;N=0;return \
         (((((((++*p[Index()])+one)-one)+one)>>63)==1)*40)+(p[1]==0x7ffffffffffffff8)+N;}Run();"
      );
      ( "unsigned add comparison keeps U64 domain",
        "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 n)[2];U64 \
         one=1;p[1]=0x7ffffffffffffff0;N=0;return \
         ((((++*p[Index()])+one)>0)*40)+(p[1]==0x7ffffffffffffff8)+N;}Run();" );
      ( "nested U64 chain retains class across signed RHS",
        "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 n)[2];U64 \
         one=1;I64 signed_one=1;p[1]=0x7ffffffffffffff0;N=0;return \
         (((((((++*p[Index()])+one)-signed_one)+signed_one)>>63)==1)*40)+(p[1]==0x7ffffffffffffff8)+N;}Run();"
      );
      ( "grouped postfix update result keeps callback arithmetic",
        "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 n)[2];I64 \
         one=1;p[1]=34;N=0;return \
         ((((*p[Index()])++)+one)==42)*40+(p[1]==42)+N;}Run();" );
      ( "compound update result keeps callback arithmetic",
        "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 n)[2];I64 \
         two=2;p[1]=18;N=0;return \
         (((*p[Index()]+=1)+two)==42)*40+(p[1]==26)+N;}Run();" );
      ( "global callback read inside function",
        target
        ^ "I64 N;I64 Index(){N++;return 1;}I64 (*P)(I64 n)[2];I64 \
           Run(){P[1]=&Target;N=0;return \
           ((*P[Index()])==&Target)*40+N+1;}Run();" );
      ( "top-level global callback equality",
        target
        ^ "I64 N;I64 Index(){N++;return 1;}I64 (*P)(I64 \
           n)[2];P[1]=&Target;N=0;((*P[Index()])==&Target)*40+N+1;" );
      ( "top-level global callback store",
        target
        ^ "I64 N;I64 Index(){N++;return 1;}I64 (*P)(I64 \
           n)[2];N=0;*P[Index()]=&Target;P[1](39)+N;" );
      ( "top-level global numeric update",
        "I64 N;I64 Index(){N++;return 1;}I64 (*P)(I64 \
         n)[2];P[1]=34;N=0;*P[Index()]+=1;(P[1]==42)*40+N+1;" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter (fun (_, source) -> ignore (compare mode source 42L "")) rows;
      let make update =
        "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 n)[2];I64 \
         one=1;p[1]=0x7ffffffffffffff0;N=0;return (((((" ^ update
        ^ ")+one)-one)+one)>>63==-1)*40+(p[1]==0x7ffffffffffffff8)+N;}Run();"
      in
      let plain_source = make "++p[Index()]" in
      let explicit_source = make "++*p[Index()]" in
      let plain_native = compare mode plain_source 42L "" in
      let explicit_native = compare mode explicit_source 42L "" in
      Alcotest.(check int)
        "indexed canceled update star adds no native instruction work"
        plain_native explicit_native;
      Alcotest.(check int)
        "indexed canceled update star adds no public instruction work"
        (public_steps mode plain_source)
        (public_steps mode explicit_source);
      ignore
        (native ~max_steps:explicit_native mode explicit_source
        |> native_value 42L);
      expect_error "HCIRVM0007"
        (native ~max_steps:(explicit_native - 1) mode explicit_source
        |> Native_program.outcome))
    modes

let indexed_calls_keep_capture_defaults_and_work () =
  let capture =
    "extern U0 PutChars(U64 ch);I64 N;I64 (*p)(I64 a,I64 b)[2];"
    ^ "I64 Old(I64 a,I64 b){PutChars('C');return a+b+N-1;}"
    ^ "I64 New(I64 a,I64 b){PutChars('N');return 99;}"
    ^ "I64 Index(){N++;PutChars('I');return 1;}"
    ^ "I64 Left(){PutChars('L');return 20;}"
    ^ "I64 Right(){p[1]=&New;PutChars('R');return 22;}"
    ^ "I64 Run(){p[1]=&Old;N=0;return (*p[Index()])(Left(),Right());}Run();"
  in
  let rows =
    [
      (capture, 42L, "IRLC");
      ( "I64 Target(I64 n=17){return n;}I64 Run(){I64 (*p)(I64 \
         n=42)[2];p[1]=&Target;return (*p[1])();}Run();",
        42L,
        "" );
      ( target
        ^ "I64 Run(){static I64 (*p)(I64 n)[2][2];p[1][1]=&Target;return \
           (*p[1][1])(40);}Run();",
        42L,
        "" );
      ( "extern U0 PutChars(U64 ch);I64 Target(I64 n){return n+2;}I64 \
         Row(){PutChars('R');return 1;}I64 Column(){PutChars('C');return \
         1;}I64 Run(){I64 (*p)(I64 n)[2][2];p[1][1]=&Target;return \
         (*p[Row()][Column()])(40);}Run();",
        42L,
        "RC" );
      (target ^ "I64 (*P)(I64 n)[2];P[1]=&Target;(*P[1])(40);", 42L, "");
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (source, expected, output) ->
          ignore (compare mode source expected output))
        rows;
      let make callee =
        target
        ^ "I64 N;I64 Index(){N++;return 1;}I64 Run(){I64 (*p)(I64 \
           n)[2];p[1]=&Target;N=0;return " ^ callee ^ "(40)+N-1;}Run();"
      in
      let plain_source = make "p[Index()]" in
      let explicit_source = make "(*p[Index()])" in
      let plain_native = compare mode plain_source 42L "" in
      let explicit_native = compare mode explicit_source 42L "" in
      Alcotest.(check int)
        "indexed canceled star adds no native instruction work" plain_native
        explicit_native;
      Alcotest.(check int)
        "indexed canceled star adds no public instruction work"
        (public_steps mode plain_source)
        (public_steps mode explicit_source);
      ignore
        (native ~max_steps:explicit_native mode explicit_source
        |> native_value 42L);
      expect_error "HCIRVM0007"
        (native ~max_steps:(explicit_native - 1) mode explicit_source
        |> Native_program.outcome))
    modes

let faults_keep_reached_effects () =
  let prefix =
    "extern U0 PutChars(U64 ch);I64 Arg(){PutChars('A');return 40;}"
  in
  let rows =
    [
      ("I64 Run(){I64 (*p)(I64 n);return (*p)(Arg());}Run;", "HCIRVM0012", "");
      ( "I64 Run(){I64 (*p)(I64 n);p=0;return (*p)(Arg());}Run;",
        "HCIRVM0024",
        "A" );
      ( "I64 Target(I64 n,I64 m){PutChars('B');return n+m;}"
        ^ "I64 Run(){I64 (*p)(I64 n);p=&Target;return (*p)(Arg());}Run;",
        "HCIRVM0014",
        "A" );
      ( "I64 Target(I64 n){PutChars('B');return 1/0;}"
        ^ "I64 Run(){I64 (*p)(I64 n);p=&Target;return (*p)(Arg());}Run;",
        "HCIRVM0009",
        "AB" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (source, code, output) ->
          let source = prefix ^ source in
          let public = ir mode source in
          expect_error code (integer_program_report_outcome public);
          Alcotest.(check string)
            "public IR reached output" output
            (integer_program_report_output_bytes public);
          let report = native mode source in
          expect_error code (Native_program.outcome report);
          Alcotest.(check string)
            "native reached output" output
            (Native_program.output_bytes report);
          Alcotest.(check bool)
            "fault happened after native entry" true
            (match Native_program.native_outcome report with
            | Some (Program.Fault _) -> true
            | _ -> false))
        rows)
    modes

let indexed_faults_keep_reached_effects () =
  let prefix =
    "extern U0 PutChars(U64 ch);I64 N;I64 Index(I64 \
     i){N++;PutChars('I');return i;}I64 Arg(){PutChars('A');return 40;}"
  in
  let rows =
    [
      ( "I64 Run(){I64 (*p)(I64 n)[2];N=0;return (*p[Index(2)])(Arg());}Run();",
        "HCIRVM0019",
        "I" );
      ( "I64 Run(){I64 (*p)(I64 n)[2];N=0;return (*p[Index(1)])(Arg());}Run();",
        "HCIRVM0012",
        "I" );
      ( "I64 Run(){I64 (*p)(I64 n)[2];p[1]=123;N=0;return \
         (*p[Index(1)])(Arg());}Run();",
        "HCIRVM0024",
        "IA" );
      ( "I64 Target(I64 n){return n;}I64 Run(){I64 (*p)(I64 \
         n)[2];p[1]=&Target;N=0;*p[Index(1)]+=Arg();return 42;}Run();",
        "HCIRVM0024",
        "IA" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (source, code, output) ->
          let source = prefix ^ source in
          let public = ir mode source in
          expect_error code (integer_program_report_outcome public);
          Alcotest.(check string)
            "indexed public fault keeps reached effects" output
            (integer_program_report_output_bytes public);
          let report = native mode source in
          expect_error code (Native_program.outcome report);
          Alcotest.(check string)
            "indexed native fault keeps reached effects" output
            (Native_program.output_bytes report);
          Alcotest.(check bool)
            "indexed callback fault happens after native entry" true
            (match Native_program.native_outcome report with
            | Some (Program.Fault _) -> true
            | _ -> false))
        rows)
    modes

let indexed_remaining_shapes_reject_before_native_entry () =
  let rows =
    [
      ( "group below the star",
        target
        ^ "I64 Run(){I64 (*p)(I64 n)[2];p[1]=&Target;return \
           (*(p[1]))(40);}Run();" );
      ( "remaining dereference",
        target
        ^ "I64 Run(){I64 (*p)(I64 n)[2];p[1]=&Target;return \
           (**p[1])(40);}Run();" );
      ( "partial callback-array rank",
        target
        ^ "I64 Run(){I64 (*p)(I64 n)[2][2];p[1][1]=&Target;return \
           (*p[1])(40);}Run();" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source) ->
          let report = native mode source in
          (match Native_program.outcome report with
          | Error errors ->
              Alcotest.(check bool)
                (label ^ " reports a public diagnostic")
                true (errors <> [])
          | Ok _ -> Alcotest.failf "%s unexpectedly executed" label);
          Alcotest.(check bool)
            (label ^ " never enters native code")
            true
            (Option.is_none (Native_program.native_outcome report)))
        rows)
    modes

let exact_work_and_retained_calls () =
  List.iter
    (fun mode ->
      let make callee =
        target ^ "I64 Run(){I64 (*p)(I64 n);p=&Target;return " ^ callee
        ^ "(40);}Run;"
      in
      let plain = compare mode (make "p") 42L "" in
      let source = make "(*p)" in
      let canceled = compare mode source 42L "" in
      Alcotest.(check int)
        "canceled star adds no instruction work" plain canceled;
      ignore (native ~max_steps:canceled mode source |> native_value 42L);
      expect_error "HCIRVM0007"
        (native ~max_steps:(canceled - 1) mode source |> Native_program.outcome);
      let session, config, source = inputs mode source in
      let image =
        Native_program.compile session ~config ~source
        |> Result.map_error diagnostics
        |> checked
        |> fun checked -> checked.value
      in
      let retained = Runtime.retain image |> checked in
      Fun.protect
        ~finally:(fun () -> Runtime.release retained |> checked)
        (fun () ->
          let budget =
            Runtime.create_budget ~max_steps:(2 * canceled) () |> checked
          in
          for activation = 1 to 2 do
            match
              Runtime.execute_retained_budget_report budget retained
              |> Runtime.outcome |> checked
            with
            | Program.Completed result ->
                Alcotest.(check int)
                  "cumulative native work" (activation * canceled)
                  result.executed_steps;
                Alcotest.(check int64)
                  "retained owned callee" 42L
                  (Option.get result.final_value).bits
            | Program.Fault _ -> Alcotest.fail "retained callback faulted"
          done;
          match
            Runtime.execute_retained_budget_report budget retained
            |> Runtime.outcome |> checked
          with
          | Program.Fault { kind = Program.Step_limit_exceeded; _ } -> ()
          | _ -> Alcotest.fail "retained callback bypassed the cumulative quota"))
    modes

let remaining_dereferences_reject () =
  List.iter
    (fun mode ->
      List.iter
        (fun callee ->
          let source =
            target ^ "I64 Run(){I64 (*p)(I64 n);p=&Target;return " ^ callee
            ^ "(40);}Run;"
          in
          let report = native mode source in
          expect_error "HCRUN0003" (Native_program.outcome report);
          Alcotest.(check bool)
            "unsupported dereference did not enter native code" true
            (Option.is_none (Native_program.native_outcome report)))
        [ "(**p)"; "(*(p))" ];
      List.iter
        (fun (update, expected) ->
          let source =
            "I64 Run(){I64 (*p)(I64 n);p=40;" ^ update ^ ";return p=="
            ^ string_of_int expected ^ ";}Run;"
          in
          ignore (compare mode source 1L ""))
        [ ("++*p", 48); ("(*p)--", 32); ("*p+=1", 48) ];
      ignore
        (compare mode "I64 Run(){I64 n=42;I64 *p=&n;return *p;}Run;" 42L ""))
    modes

let () =
  match Runtime.platform () with
  | Runtime.Unsupported -> Alcotest.fail "native callback tests require x86-64"
  | Runtime.Windows_x86_64 | Runtime.Linux_x86_64 ->
      Alcotest.run "holyc native callback dereference"
        [
          ( "callback dereference",
            [
              Alcotest.test_case "storage and original signatures" `Quick
                storage_and_signatures;
              Alcotest.test_case
                "callee capture precedes right-to-left arguments" `Quick
                callee_capture_and_argument_order;
              Alcotest.test_case "indexed reads stores and updates" `Quick
                indexed_reads_stores_and_updates;
              Alcotest.test_case "indexed calls capture defaults and exact work"
                `Quick indexed_calls_keep_capture_defaults_and_work;
              Alcotest.test_case "faults preserve reached effects" `Quick
                faults_keep_reached_effects;
              Alcotest.test_case "indexed faults preserve reached effects"
                `Quick indexed_faults_keep_reached_effects;
              Alcotest.test_case "indexed remaining shapes reject before entry"
                `Quick indexed_remaining_shapes_reject_before_native_entry;
              Alcotest.test_case "exact work and retained execution" `Quick
                exact_work_and_retained_calls;
              Alcotest.test_case "remaining dereferences stay separate" `Quick
                remaining_dereferences_reject;
            ] );
        ]
