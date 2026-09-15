open Holyc_lib
module F = Test_integer_functions
module G = Test_integer_globals
module VM = Ir_integer_interpreter

let values () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, expected) ->
          ignore (G.run ~mode source |> F.expect expected))
        [
          ("I64 F(...){return argc+argv[0];}F(41);", 42L);
          ("I64 F(...){return argc;}F();", 0L);
          ("I64 F(I64 a,...){return a;}(F(1));", 1L);
          ("I64 F(I64 n,...){return n;}(1+F(2,3));", 3L);
          ("I64 F(){I64 n=42;I64i *p=&n;return *p;}F();", 42L);
          ("I64 F(){I64i n=42;I64 *p=&n;return *p;}F();", 42L);
          ("I64 F(I64 n,...){return n+argc+argv[0]+argv[1];}F(10,10,20);", 42L);
          ( "I64 F(...){I64 i=0,n=0;while(i<argc){n+=argv[i];i++;}return \
             n;}F(10,12,20);",
            42L );
          ("I64 F(...){argv[0]+=argv[1];return argv[0];}F(40,2);", 42L);
          ( "U0 Set(I64 *p){p[0]+=2;}I64 F(...){Set(argv);return argv[0];}F(40);",
            42L );
          ("I64 F(...){I64 *p=argv;return p[0]+p[1];}F(40,2);", 42L);
          ( "I64 F(I64 n,...){if(n)return F(n-1,argv[0]+1,7);return \
             argc+argv[0];}F(2,38);",
            42L );
          ("I64 F(...){return argv[0]+argc;}I64 V=F(41);V;", 42L);
          ("I64 F(I8 n,...){return n+argv[0];}F(257,41);", 42L);
          ("I64 F(...){return argv[0]+argv[1];}I8 n=-1;U8 b=43;F(n,b);", 42L);
          ("I64 F(...){return sizeof argv;}F();", 1016L);
          ("I64 F(...){argc=40;return argc+argv[0];}F(2);", 42L);
          ("I64 F(...){I64 *p;p=argv;p[0]+=2;return argv[0];}F(40);", 42L);
          ("U0 Empty(I64 *p){}I64 F(...){Empty(argv);return argc;}F();", 0L);
          ( "I64 F(I64 *p,...){p[0]+=argv[0];return p[0];}I64 G(){I64 \
             n=40;return F(&n,2);}G();",
            42L );
        ])
    G.modes

let retained () =
  let module T = Test_integer_task in
  let session = Session.create () in
  let task = T.create session in
  ignore
    (T.run session task "I64 F(...){return argc+argv[0];}"
    |> Test_integer_program.checked);
  T.value 42L (T.run session task "F(41);");
  T.value 42L (T.run session task "F(40,7);");
  T.fault "HCIRVM0019" (T.run session task "F();");
  T.value 42L (T.run session task "F(41);")

let bounds () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let error = F.first_error (G.run ~mode source) in
          Alcotest.(check string) source "HCIRVM0019" error.code;
          Alcotest.(check bool)
            "fault identifies variadic body" true
            (List.mem "function=F" error.notes))
        [
          "I64 F(...){return argv[0];}F();";
          "I64 F(...){I64 neighbor=42;return argv[1];}F(7);";
          "I64 F(...){return argv[-1];}F(42);";
          "I64 F(...){argv[argc]=42;return 0;}F(7);";
          "I64 F(...){argc=127;return argv[126];}F(42);";
          "I64 F(...){I64 *p=argv;return p[0];}F();";
        ])
    G.modes

let quotas () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, bytes, expected) ->
          let execution =
            G.run ~mode ~max_frame_bytes:bytes source |> F.expect expected
          in
          Alcotest.(check string)
            "one below actual frame bytes" "HCIRVM0011"
            (F.first_error (G.run ~mode ~max_frame_bytes:(bytes - 1) source))
              .code;
          let steps = VM.executed_steps execution in
          ignore
            (G.run ~mode ~max_frame_bytes:bytes ~max_steps:steps source
            |> F.expect expected);
          Alcotest.(check string)
            "one below runtime steps" "HCIRVM0007"
            (F.first_error
               (G.run ~mode ~max_frame_bytes:bytes ~max_steps:(steps - 1) source))
              .code)
        [
          ("I64 F(...){return argc;}F();", 8, 0L);
          ("I64 F(...){return argc+argv[0];}F(41);", 16, 42L);
          ( "I64 F(I64 n,...){I64 a[2];a[0]=n;return a[0]+argv[0];}F(40,2);",
            40,
            42L );
          ( "I64 F(I64 n,...){if(n)return F(n-1,argv[0]+1,7);return \
             argc+argv[0];}F(2,38);",
            88,
            42L );
          ("I64 F(...){return argc+argv[0];}F(1,2,3);F(41);", 32, 42L);
        ];
      Alcotest.(check string)
        "variadic recursion depth" "HCIRVM0015"
        (F.first_error
           (G.run ~mode ~max_call_depth:2
              "I64 F(I64 n,...){if(n)return F(n-1,argv[0]);return \
               argv[0];}F(2,42);"))
          .code)
    G.modes

let large_tail () =
  let values =
    List.init 130 (fun i -> if i = 129 then "42" else "0") |> String.concat ","
  in
  List.iter
    (fun mode ->
      ignore
        (G.run ~mode ~max_frame_bytes:1048
           ("I64 F(...){return argv[129];}F(" ^ values ^ ");")
        |> F.expect 42L))
    G.modes

let standalone () =
  List.iter
    (fun mode ->
      let compiled =
        G.compile ~mode "I64 F(I64 n,...){I64 x=argv[0];return n+argc+x;}"
      in
      let definition = List.hd (integer_program_functions compiled) in
      let execute bytes arguments =
        VM.execute_function ~max_steps:1000 ~max_frame_bytes:bytes
          ~frame:definition.frame ~arguments definition.body
      in
      let result =
        execute 32 [ 39L; 2L ]
        |> Test_ir_integer_interpreter.require_ok (fun errors ->
            (List.hd errors).VM.message)
      in
      (match VM.termination result with
      | VM.Returned (Some word) ->
          Alcotest.(check int64) "standalone ABI words" 42L word.bits
      | _ -> Alcotest.fail "standalone function did not return its word");
      List.iter
        (fun (bytes, args) ->
          match execute bytes args with
          | Error (error :: _) ->
              Alcotest.(check string)
                "standalone frame bounds" "HCIRVM0011" error.code
          | _ -> Alcotest.fail "expected standalone preflight rejection")
        [ (31, [ 39L; 2L ]); (32, []) ])
    G.modes

let implicit =
  {|#exe {
 I64 Out=0;
 U0 Print(I64 a=40,...){Out=a+argc+argv[0];}
 ""(,1);
 I64 Top=Out;
 U0 Saved(){""(39,1,7);}
 Out=0;Saved;
 StreamPrint("%d;",Top+Out-42);
}|}

let unsupported () =
  List.iter
    (fun mode ->
      List.iter
        (fun source -> ignore (F.first_error (G.run ~mode source)))
        [
          "I64 F(...){return argc;}F(1.0);";
          "I64 F(...){return argc;}I64 G(){I64 n=42;return F(&n);}G();";
          "U0 Set(U64 *p){}I64 F(...){Set(argv);return 42;}F(42);";
        ])
    G.modes

let source_ownership () =
  let source = "I64 F(...){return argc+argv[0];}F(41);" in
  List.iter
    (fun mode ->
      let compiled = G.compile ~mode source in
      let foreign = G.compile ~mode source in
      let run ?runtime_calls () =
        VM.execute_program ?runtime_calls
          ~globals:(integer_program_globals compiled)
          ~initialization:(integer_program_initialization compiled)
          ~functions:(integer_program_functions compiled)
          ~max_steps:1000 ~max_frame_bytes:16 ~max_call_depth:1
          (integer_program_entry compiled)
      in
      ignore
        (run ~runtime_calls:(integer_program_runtime_calls compiled) ()
        |> Test_ir_integer_interpreter.require_ok (fun errors ->
            (List.hd errors).VM.message));
      List.iter
        (fun result ->
          match result with
          | Error errors ->
              Alcotest.(check bool)
                "unowned protocol fails before execution" true
                (List.for_all
                   (fun (error : VM.error) ->
                     error.stage = VM.Preflight && error.executed_steps = 0)
                   errors)
          | Ok _ -> Alcotest.fail "variadic protocol lost its source owner")
        [
          run (); run ~runtime_calls:(integer_program_runtime_calls foreign) ();
        ];
      let definition = List.hd (integer_program_functions compiled) in
      let other = List.hd (integer_program_functions foreign) in
      match
        VM.execute_function ~max_steps:1000 ~max_frame_bytes:16
          ~frame:other.frame ~arguments:[ 41L ] definition.body
      with
      | Error (error :: _) ->
          Alcotest.(check string)
            "foreign synthetic frame" "HCIRVM0011" error.code
      | _ -> Alcotest.fail "foreign synthetic bindings admitted")
    G.modes

let tests =
  [
    Alcotest.test_case "source integer tails and dynamic frames" `Quick values;
    Alcotest.test_case "retained variadic definition" `Quick retained;
    Alcotest.test_case "actual tail bounds and count mutation" `Quick bounds;
    Alcotest.test_case "dynamic frame and instruction quotas" `Quick quotas;
    Alcotest.test_case "tail beyond native placeholder" `Quick large_tail;
    Alcotest.test_case "standalone variadic invocation" `Quick standalone;
    Alcotest.test_case "implicit saved defaults and retained bodies" `Quick
      (Test_task_parser_executor.generates implicit);
    Alcotest.test_case "unsupported tail and pointer representations" `Quick
      unsupported;
    Alcotest.test_case "source call and synthetic frame ownership" `Quick
      source_ownership;
  ]
