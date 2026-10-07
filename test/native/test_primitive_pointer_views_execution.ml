open Holyc_lib
module P = X86_64_program
module VM = Ir_integer_interpreter

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let errors diagnostics =
  List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message) diagnostics
  |> String.concat "; "

let checked = function
  | Ok x -> x
  | Error ds -> Alcotest.fail (errors ds)

let inputs mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-primitive-views.hc" ~contents
  in
  let config =
    match Preprocessor.Config.create ~compilation_mode:mode () with
    | Ok c -> c
    | Error e -> Alcotest.fail e
  in
  (session, config, source)

let run ?(max_steps = 100_000) ?max_output_bytes ?max_output_work
    ?max_frame_bytes ?max_call_depth mode contents =
  let session, config, source = inputs mode contents in
  Native_program.evaluate ?max_output_bytes ?max_output_work ?max_frame_bytes
    ?max_call_depth session ~config ~source ~max_steps

let word expected report =
  let native = (Native_program.outcome report |> checked).value in
  (match native.execution.final_value with
  | Some w -> Alcotest.(check int64) "native word" expected w.bits
  | None -> Alcotest.fail "missing native word");
  native

let case ?(output = "") mode (source, expected) =
  let report = run mode source in
  let native = word expected report in
  Alcotest.(check string)
    "native bytes" output
    (Native_program.output_bytes report);
  let session, config, source = inputs mode source in
  let interpreted =
    (run_integer_program session ~config ~source ~max_steps:100_000 |> checked)
      .value
  in
  (match VM.final_value interpreted with
  | Some w ->
      Alcotest.(check int64) "independent interpreter word" expected w.bits
  | None -> Alcotest.fail "missing interpreter word");
  Alcotest.(check int)
    "same reached instruction work"
    (VM.executed_steps interpreted)
    native.execution.executed_steps

let cases fixtures () =
  List.iter (fun mode -> List.iter (case mode) fixtures) modes

let classes =
  [
    ("Bool", 1, false);
    ("I8", 1, false);
    ("U8", 1, true);
    ("I16", 2, false);
    ("U16", 2, true);
    ("I32", 4, false);
    ("U32", 4, true);
    ("I64", 8, false);
    ("U64", 8, true);
  ]

let read_matrix () =
  List.iter
    (fun mode ->
      List.iter
        (fun (original, _, _) ->
          List.iter
            (fun (view, width, unsigned) ->
              let source =
                Printf.sprintf
                  "I64 F(){%s a[8];I64 i;for(i=0;i<8;i++)a[i]=-1;%s \
                   *p=a(%s*);return *p;}F();"
                  original view view
              in
              let expected =
                if unsigned && width < 8 then
                  Int64.pred (Int64.shift_left 1L (width * 8))
                else -1L
              in
              case mode (source, expected))
            classes)
        classes)
    modes

let write_matrix () =
  List.iter
    (fun mode ->
      List.iter
        (fun (original, _, _) ->
          List.iter
            (fun (view, width, _) ->
              let bytes =
                List.init 8 (fun byte ->
                    Printf.sprintf "if(b[%d]!=%d)return %d;" byte
                      (if byte < width then byte + 1 else 255)
                      (byte + 1))
                |> String.concat ""
              in
              let source =
                Printf.sprintf
                  "I64 F(){%s a[8];I64 i;for(i=0;i<8;i++)a[i]=-1;%s \
                   *p=a(%s*);*p=0x0807060504030201;U8 *b=a(U8*);%s return \
                   42;}F();"
                  original view view bytes
              in
              case mode (source, 42L))
            classes)
        classes)
    modes

let parameter_views () =
  List.iter
    (fun mode ->
      List.iter
        (fun (view, _, _) ->
          let source =
            Printf.sprintf
              "I64 Set(%s *p){*p=42;return *p;}I64 F(){U64 n=0;return \
               Set((&n)(%s*))+n;}F();"
              view view
          in
          case mode (source, 84L))
        classes)
    modes

let aliases =
  [
    ( "I64 F(){U64 n=0;U8 *p=(&n)(U8*);U16 *q=(&p[1])(U16*);*q=0x1234;return \
       n;}F();",
      0x123400L );
    ( "I64 F(){U64 a[2];a[0]=0;a[1]=0;U8 *p=a(U8*);U64 \
       *q=(&p[7])(U64*);*q=0x0807060504030201;return \
       a[0]==0x0100000000000000&&a[1]==0x0008070605040302;}F();",
      1L );
    ("I64 F(){U64 n=0;I8 *p=(&n)(I8*);*p=-1;return *p;}F();", -1L);
    ( "I64 F(){U64 n=0;U8 *p=(&n)(U8*);*p=250;I64 v=(*p+=10);return \
       v*1000+n;}F();",
      260004L );
    ( "I64 F(){U64 n=20,m=22;U8 *p=(&n)(U8*);*p+=*(p=(&m)(U8*));return n;}F();",
      42L );
    ( "I64 F(){U64 a[3];a[0]=20;a[1]=21;a[2]=22;U8 *q=a(U8*),*p=q;I64 \
       i;for(i=0;i<3;i++){q=(&a[i])(U8*);if(i==1)p=q;}return *p;}F();",
      21L );
    ( "I64 F(){U64 a[2];a[0]=40;a[1]=2;U8 *p=a(U8*),*q=p;I64 \
       i;for(i=0;i<9;i++){p=p(U8*);p=&p[1];}return *q+p[-1];}F();",
      42L );
    ( "I64 Set(U16 *p){*p=42;return *p;}I64 F(){U64 n=0;Set((&n)(U16*));return \
       n;}F();",
      42L );
    ( "I64 Set(U8 *p,U8 *q){p=q;*p=42;return *q;}I64 F(){U64 \
       n=0,m=0;Set((&n)(U8*),(&m)(U8*));return n+m;}F();",
      42L );
    ( "I64 R(U16 *p,I64 n){if(n){(*p)++;return R(p,n-1);}return *p;}I64 \
       F(){U64 n=40;return R((&n)(U16*),2);}F();",
      42L );
    ("I64 V(...){U8 *p=argv(U8*);return p[0]+p[8];}V(20,22);", 42L);
    ("U64 G=0;I64 F(){U8 *p=(&G)(U8*);p[1]=42;return G;}F();", 10752L);
    ("I64 F(){static U64 n=0;U8 *p=(&n)(U8*);p[0]+=21;return n;}F();F();", 42L);
    ("I64 F(){U8 *p=\"AB\";U16 *q=p(U16*);*q=0x4243;return p[0];}F();", 67L);
    ( "I64 F(){U64 n=0;U8 *p=(&n)(U8*);U16 *q=p(U16*);return \
       q==p(U16*)&&(&q[1])>q&&(&q[1])-q==1;}F();",
      1L );
    ("I64 F(){U64 a[100];U8 *p=a(U8*);p[799]=42;return p[799];}F();", 42L);
  ]

let initialization =
  [
    ("I64 F(){U64 n;U8 *p=(&n)(U8*);p[3]=42;return p[3];}F();", 42L);
    ( "I64 F(){U64 n;U8 *p=(&n)(U8*);I64 \
       i;for(i=0;i<8;i++)p[i]=0;p[0]=42;return n;}F();",
      42L );
    ("I64 F(){U8 a[8];U64 *p=a(U64*);*p=42;return a[0]+a[7];}F();", 42L);
    ("I64 F(){U64 n;U8 *p=(&n)(U8*);p[0]=7;n=42;return *p;}F();", 42L);
    ( "I64 F(){U64 a[2];U8 *p=a(U8*);U16 *q=(&p[7])(U16*);*q=0x2a2a;return \
       p[7]+p[8];}F();",
      84L );
  ]

let fault ?(output = "") mode code source =
  let report = run mode source in
  (match Native_program.outcome report with
  | Ok _ -> Alcotest.fail "unknown bytes or bounds unexpectedly admitted"
  | Error ds ->
      Alcotest.(check bool)
        (errors ds) true
        (List.exists (fun (d : Diagnostic.t) -> d.code = code) ds));
  (match Native_program.native_outcome report with
  | Some (P.Fault _) -> ()
  | _ -> Alcotest.fail "fault did not reach native entry");
  Alcotest.(check string)
    "reached output retained" output
    (Native_program.output_bytes report)

let unknown_bytes () =
  List.iter
    (fun mode ->
      List.iter (fault mode "HCIRVM0012")
        [
          "I64 F(){U64 n;U8 *p=(&n)(U8*);p[0]=42;return n;}F();";
          "I64 F(){U64 n;U8 *p=(&n)(U8*);p[0]=42;return p[1];}F();";
          "I64 F(){U64 n;U8 *p=(&n)(U8*);p[0]=42;p[1]+=1;return 42;}F();";
          "I64 F(){U8 a[8];a[0]=42;U64 *p=a(U64*);return *p;}F();";
          "I64 F(){U64 a[2];U8 *p=a(U8*);U16 *q=(&p[7])(U16*);*q=42;return \
           a[0];}F();";
        ];
      List.iter (fault mode "HCIRVM0019")
        [
          "I64 F(){U8 a[2];U64 *p=a(U64*);*p=42;return 0;}F();";
          "I64 F(){U64 n=42;U8 *p=(&n)(U8*);return p[8];}F();";
          "I64 F(){U64 n=42;U8 *p=(&n)(U8*);return p[-1];}F();";
        ];
      fault ~output:"kept" mode "HCIRVM0012"
        "extern U0 Print(U8 *fmt,...);I64 F(){U64 n;U8 \
         *p=(&n)(U8*);p[0]=42;Print(\"kept\");return n;}F();")
    modes;
  let source = "U64 N;I64 F(){U8 *p=(&N)(U8*);*p=42;return N;}F();" in
  fault Preprocessor.Jit "HCIRVM0012" source;
  case Preprocessor.Aot (source, 42L)

let internals =
  [
    ( "public _intern 0x78 Bool Bts(U8 *p,I64 n);I64 F(){U64 n;U8 \
       *p=(&n)(U8*);p[1]=0;Bts(p,9);return p[1];}F();",
      2L );
    ( "public _intern 0x78 Bool Bts(U8 *p,I64 n);I64 F(){U64 n=0;U8 \
       *b=(&n)(U8*);U16 *p=(&b[1])(U16*);Bts(p,16);return n;}F();",
      0x01000000L );
    ( "public _intern 0xa6 U0 SwapU16(U16 *a,U16 *b);I64 F(){U64 n=0x030201;U8 \
       *p=(&n)(U8*);SwapU16(p(U16*),(&p[1])(U16*));return n;}F();",
      0x020302L );
    ( "public _intern 0xaf U64 ModU64(U64 *q,U64 d);I64 F(){U8 a[8];U64 \
       *p=a(U64*);*p=85;I64 r=ModU64(p,2);return r+*p;}F();",
      43L );
  ]

let print_header = "extern U0 Print(U8 *fmt,...);"

let output () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, output) -> case ~output mode (print_header ^ source, 42L))
        [
          ( "I64 F(){I8 a[2];a[0]=65;a[1]=0;Print(\"%s\",a(U8*));return 42;}F();",
            "A" );
          ( "I64 F(){U64 n;U8 \
             *p=(&n)(U8*);p[0]=65;p[1]=0;Print(\"%s\",p);return 42;}F();",
            "A" );
          ( "public _intern 0x84 I64 StrLen(U8 *p);I64 F(){U64 n=0x00434241;U8 \
             *p=(&n)(U8*);Print(\"%d\",StrLen(p));return 42;}F();",
            "3" );
        ])
    modes;
  let session, config, source =
    inputs Preprocessor.Jit
      (print_header
     ^ "U0 (*p)(U8 *fmt,...);p=&Print;I64 F(){U64 \
        n=0x00434241;p(\"%s\",(&n)(U8*));return 42;}F();")
  in
  let report =
    Native_source_execution.evaluate ~max_code_bytes:262144 session ~config
      ~source ~max_steps:100_000
  in
  let result = (Native_source_execution.outcome report |> checked).value in
  Alcotest.(check int64)
    "provider callback word" 42L (Option.get result.final_value).bits;
  Alcotest.(check string)
    "provider callback scans the owned view" "ABC"
    (Native_source_execution.output_bytes report)

let task () =
  let contents =
    "U64 N;U0 Set(U8 *p,U8 n){*p=n;}Set((&N)(U8*),42);I64 Read(U8 *p){return \
     *p;}Read((&N)(U8*));N;"
  in
  let session, config, source = inputs Preprocessor.Jit contents in
  let report =
    Native_source_execution.evaluate session ~config ~source ~max_steps:100_000
  in
  (match Native_source_execution.outcome report with
  | Error ds ->
      Alcotest.(check bool)
        (errors ds) true
        (List.exists (fun (d : Diagnostic.t) -> d.code = "HCIRVM0012") ds)
  | Ok _ -> Alcotest.fail "task promoted unknown bytes");
  let session, config, source =
    inputs Preprocessor.Jit
      "U64 N;U0 Set(U8 *p,U8 n){*p=n;}Set((&N)(U8*),42);I64 Read(U8 *p){return \
       *p;}Read((&N)(U8*));N=0;Set((&N)(U8*),42);N;"
  in
  let report =
    Native_source_execution.evaluate session ~config ~source ~max_steps:100_000
  in
  let result = (Native_source_execution.outcome report |> checked).value in
  Alcotest.(check int64)
    "retained body reads the same original object" 42L
    (Option.get result.final_value).bits;
  let session, config, source =
    inputs Preprocessor.Jit
      "U0 Set(U8 *p,I64 i){p[i]=42;}U8 A[1];Set(A,0);U64 \
       B[20];Set(B(U8*),159);I64 Read(U8 *p){return p[159];}Read(B(U8*));"
  in
  let report =
    Native_source_execution.evaluate session ~config ~source ~max_steps:100_000
  in
  let result = (Native_source_execution.outcome report |> checked).value in
  Alcotest.(check int64)
    "retained callee accepts a later larger object" 42L
    (Option.get result.final_value).bits

let runtime_limits () =
  let contents =
    print_header
    ^ "U0 Write(U16 *p){*p=0x4241;}I64 Demo(){U64 word;U8 \
       *bytes=(&word)(U8*);Write(bytes(U16*));bytes[2]=0;Print(\"%s\",bytes);I64 \
       i;for(i=3;i<8;i++)bytes[i]=0;U64 *again=bytes(U64*);return \
       *again-0x4241+42;}Demo();"
  in
  List.iter
    (fun mode ->
      let report =
        run ~max_steps:233 ~max_output_bytes:2 ~max_output_work:8
          ~max_frame_bytes:56 ~max_call_depth:2 mode contents
      in
      let native = word 42L report in
      Alcotest.(check int)
        "exact original work" 233 native.execution.executed_steps;
      Alcotest.(check string)
        "exact byte window" "AB"
        (Native_program.output_bytes report);
      Alcotest.(check int)
        "exact scan work" 8
        (Native_program.output_work report);
      let rejected kind report =
        match Native_program.native_outcome report with
        | Some (P.Fault f) ->
            Alcotest.(check bool) "reached one-below fault" true (f.kind = kind)
        | _ -> Alcotest.fail "one-below runtime quota did not fault"
      in
      rejected P.Step_limit_exceeded (run ~max_steps:232 mode contents);
      rejected P.Frame_limit_exceeded (run ~max_frame_bytes:55 mode contents);
      rejected P.Call_depth_exceeded (run ~max_call_depth:1 mode contents);
      let below = run ~max_output_bytes:1 mode contents in
      rejected P.Output_limit_exceeded below;
      Alcotest.(check string)
        "field admission is atomic" ""
        (Native_program.output_bytes below);
      rejected P.Output_work_limit_exceeded
        (run ~max_output_work:7 mode contents))
    modes

let image_limits () =
  let contents =
    "I64 F(){U64 a[100];U8 *p=a(U8*);p[799]=42;return p[799];}F();"
  in
  List.iter
    (fun mode ->
      List.iter
        (fun status_abi ->
          let compile ?max_stack_bytes ?max_code_bytes () =
            let session, config, source = inputs mode contents in
            Native_program.compile ?max_stack_bytes ?max_code_bytes ~status_abi
              session ~config ~source
          in
          let image = (compile () |> checked).value in
          let frame = P.frame_bytes image and code = P.code_bytes image in
          ignore
            (compile ~max_stack_bytes:frame ~max_code_bytes:code () |> checked);
          Alcotest.(check bool)
            "real descriptor frame one below" true
            (Result.is_error (compile ~max_stack_bytes:(frame - 1) ()));
          Alcotest.(check bool)
            "real encoded bytes one below" true
            (Result.is_error (compile ~max_code_bytes:(code - 1) ()));
          let host =
            match Native_program_execution.platform () with
            | Native_program_execution.Windows_x86_64 -> P.Windows_x64
            | _ -> P.System_v_x64
          in
          if status_abi = host then
            for _ = 1 to 2 do
              match
                Native_program_execution.execute ~max_steps:100_000 image
              with
              | Ok (P.Completed result) ->
                  Alcotest.(check int64)
                    "fresh image" 42L (Option.get result.final_value).bits
              | _ -> Alcotest.fail "fresh view image failed"
            done)
        [ P.Windows_x64; P.System_v_x64 ])
    modes

let () =
  match Native_program_execution.platform () with
  | Native_program_execution.Unsupported ->
      Alcotest.fail "native views require x86-64"
  | _ ->
      Alcotest.run "native primitive pointer views"
        [
          ( "views",
            [
              Alcotest.test_case "all scalar read classes" `Quick read_matrix;
              Alcotest.test_case "all scalar write classes and neighbors" `Quick
                write_matrix;
              Alcotest.test_case "every cast class through fixed parameters"
                `Quick parameter_views;
              Alcotest.test_case "aliases, loops, windows and calls" `Quick
                (cases aliases);
              Alcotest.test_case "partial and full initialization" `Quick
                (cases initialization);
              Alcotest.test_case "unknown bytes and reached bounds" `Quick
                unknown_bytes;
              Alcotest.test_case "view bit, swap and modulo operations" `Quick
                (cases internals);
              Alcotest.test_case "byte scans and provider callbacks" `Quick
                output;
              Alcotest.test_case "retained task storage and bodies" `Quick task;
              Alcotest.test_case "exact reached runtime quotas" `Quick
                runtime_limits;
              Alcotest.test_case
                "both ABIs, exact image quotas and fresh execution" `Quick
                image_limits;
            ] );
        ]
