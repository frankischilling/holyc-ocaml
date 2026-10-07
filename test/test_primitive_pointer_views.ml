open Holyc_lib
module F = Test_integer_functions
module G = Test_integer_globals
module S = Test_integer_statics
module Output = Test_integer_output
module VM = Ir_integer_interpreter
module Seq = Ir_instruction_sequence
module SI = Test_integer_static_initializers

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

let read_classes () =
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
          S.cases [ (source, expected) ])
        classes)
    classes

let write_classes () =
  List.iter
    (fun (original, _, _) ->
      List.iter
        (fun (view, width, _) ->
          let checks =
            List.init 8 (fun byte ->
                Printf.sprintf "if(b[%d]!=%d)return %d;" byte
                  (if byte < width then byte + 1 else 255)
                  (byte + 1))
            |> String.concat ""
          in
          let source =
            Printf.sprintf
              "I64 F(){%s a[8];I64 i;for(i=0;i<8;i++)a[i]=-1;%s \
               *p=a(%s*);*p=0x0807060504030201;U8 *b=a(U8*);%s return 42;}F();"
              original view view checks
          in
          S.cases [ (source, 42L) ])
        classes)
    classes

let aliases_and_windows () =
  S.cases
    [
      ("I64 F(){U64 n=0x0102030405060708;U8 *p=(&n)(U8*);return p[3];}F();", 5L);
      ("I64 F(){U64 n=0;U8 *p=(&n)(U8*);p[1]=42;return n;}F();", 10752L);
      ( "I64 F(){U64 n=0;U8 *p=(&n)(U8*);U16 *q=(&p[1])(U16*);*q=0x1234;return \
         n;}F();",
        0x123400L );
      ( "I64 F(){U64 a[2];a[0]=0;a[1]=0;U8 *p=a(U8*);U64 \
         *q=(&p[7])(U64*);*q=0x0807060504030201;return \
         a[0]==0x0100000000000000&&a[1]==0x0008070605040302;}F();",
        1L );
      ( "I64 F(){U64 n=0;U8 *p=(&n)(U8*);p[1]=41;I64 old=p[1]++;return \
         old*1000+n;}F();",
        51752L );
      ( "I64 F(){U64 n=0;U8 *p=(&n)(U8*);p[0]=250;I64 v=(p[0]+=10);return \
         v*1000+n;}F();",
        260004L );
      ("I64 F(){U64 n=0;I8 *p=(&n)(I8*);p[0]=-1;return *p;}F();", -1L);
      ( "I64 F(){U64 n=42;U8 *a=(&n)(U8*),*b=a;U64 \
         *q=b(U64*);*q=40;*b+=2;return n;}F();",
        42L );
      ( "I64 F(){U64 n=20,m=22;U8 *p=(&n)(U8*);*p+=*(p=(&m)(U8*));return n;}F();",
        42L );
      ( "I64 Set(U16 *p){*p=42;return *p;}I64 F(){U64 \
         n=0;Set((&n)(U16*));return n;}F();",
        42L );
      ( "I64 R(U8 *p,I64 n){if(n)R(p,n-1);else *p=42;return *p;}I64 F(){U64 \
         n=0;R((&n)(U8*),3);return n;}F();",
        42L );
      ("I64 F(...){U8 *p=argv(U8*);p[0]=42;return argv[0];}F(0);", 42L);
      ("U64 G=0;I64 F(){U8 *p=(&G)(U8*);p[0]=42;return G;}F();", 42L);
      ("I64 F(){static U64 n=0;U8 *p=(&n)(U8*);return ++p[0];}F();F();", 2L);
      ("I64 F(){U8 *p=\"*\";I8 *q=p(I8*);U8 *r=q(U8*);return *r;}F();", 42L);
      ("I64 F(){U64 n=42;U8 *p=(&n)(U8*);U64 *q=p(U64*);return q==&n;}F();", 1L);
      ("I64 F(){U64 n=0x5400;U8 *p=(&n)(U8*);return p[1]/2;}F();", 42L);
      ("I64 F(){U64 a[2];U8 *p=a(U8*);U64 *q=(&p[8])(U64*);return q-a;}F();", 1L);
    ]

let initialization () =
  S.cases
    [
      ("I64 F(){U64 n;U8 *p=(&n)(U8*);p[3]=42;return p[3];}F();", 42L);
      ( "I64 F(){U64 n;U8 *p=(&n)(U8*);I64 \
         i;for(i=0;i<8;i++)p[i]=0;p[0]=42;return n;}F();",
        42L );
      ("I64 F(){U8 a[8];U64 *p=a(U64*);*p=42;return a[0]+a[7];}F();", 42L);
      ("I64 F(){U64 n;U8 *p=(&n)(U8*);p[0]=7;n=42;return *p;}F();", 42L);
      ("I64 F(){U64 n;U8 *p=(&n)(U8*);U64 *q=p(U64*);*q=42;return n;}F();", 42L);
      ( "I64 F(){U64 a[2];U8 *p=a(U8*);U16 *q=(&p[7])(U16*);*q=0x2a2a;return \
         p[7];}F();",
        42L );
    ];
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let error = F.first_error (G.run ~mode source) in
          Alcotest.(check string) source "HCIRVM0012" error.code)
        [
          "I64 F(){U64 n;U8 *p=(&n)(U8*);p[0]=42;return n;}F();";
          "I64 F(){U64 n;U8 *p=(&n)(U8*);p[0]=42;return p[1];}F();";
          "I64 F(){U64 n;U8 *p=(&n)(U8*);p[0]=42;p[1]+=1;return 42;}F();";
          "I64 F(){U8 a[8];a[0]=42;U64 *p=a(U64*);return *p;}F();";
          "I64 F(){U64 a[2];U8 *p=a(U8*);U16 *q=(&p[7])(U16*);*q=42;return \
           a[0];}F();";
        ])
    G.modes;
  let global = "U64 G;I64 F(){U8 *p=(&G)(U8*);p[0]=42;return G;}F();" in
  Alcotest.(check string)
    "JIT global keeps its unknown bytes" "HCIRVM0012"
    (F.first_error (G.run global)).code;
  ignore (G.run ~mode:Preprocessor.Aot global |> F.expect 42L)

let internals () =
  S.cases
    [
      ( "public _intern 0x78 Bool Bts(U8 *p,I64 n);I64 F(){U64 n=0;U8 \
         *p=(&n)(U8*);Bts(p,9);return n;}F();",
        512L );
      ( "public _intern 0x78 Bool Bts(U8 *p,I64 n);I64 F(){U64 n;U8 \
         *p=(&n)(U8*);p[1]=0;Bts(p,9);return p[1];}F();",
        2L );
      ( "public _intern 0xa6 U0 SwapU16(U16 *a,U16 *b);I64 F(){U64 \
         n=0x0008002a;U8 *p=(&n)(U8*);SwapU16(p(U16*),(&p[2])(U16*));return \
         n;}F();",
        0x002a0008L );
      ( "public _intern 0xa6 U0 SwapU16(U16 *a,U16 *b);I64 F(){U64 \
         n=0x030201;U8 *p=(&n)(U8*);SwapU16(p(U16*),(&p[1])(U16*));return \
         n;}F();",
        0x020302L );
      ( "public _intern 0xaf U64 ModU64(U64 *q,U64 d);I64 F(){U8 a[8];U64 \
         *p=a(U64*);*p=85;I64 r=ModU64(p,2);return r+*p;}F();",
        43L );
      ( "public _intern 0x79 Bool Btr(U8 *p,I64 n);public _intern 0x7a Bool \
         Btc(U8 *p,I64 n);I64 F(){U64 n=0x0300;U8 *p=(&n)(U8*);I64 \
         old=Btr(p,8);Btc(p,9);return old*1000+n;}F();",
        1000L );
    ]

let output () =
  Output.cases
    [
      ( Output.print_header
        ^ "I64 F(){I8 a[2];a[0]=65;a[1]=0;Print(\"%s\",a(U8*));return 42;}F();",
        "A" );
      ( Output.print_header
        ^ "I64 F(){U64 n;U8 *p=(&n)(U8*);p[0]=65;p[1]=0;Print(\"%s\",p);return \
           42;}F();",
        "A" );
      ( Output.print_header
        ^ "I64 F(){U64 n=0x00420041;Print(\"%s\",(&n)(U8*));return 42;}F();",
        "A" );
      ( "public _intern 0x84 I64 StrLen(U8 *p);" ^ Output.print_header
        ^ "I64 F(){U64 n=0x00434241;U8 \
           *p=(&n)(U8*);Print(\"%d\",StrLen(p));return 42;}F();",
        "3" );
    ];
  ignore
    (Output.run
       (Output.print_header
      ^ "U0 (*p)(U8 *fmt,...);p=&Print;I64 F(){U64 \
         n=0x00434241;p(\"%s\",(&n)(U8*));return 42;}F();")
    |> Output.expect "ABC");
  ignore
    (Output.run
       (Output.print_header ^ Output.putchars_header
      ^ "I64 Mark(){PutChars('M');return 0;}U0 (*p)(I8 \
         *fmt,...);p=&Print;p(\"X\"(I8*),Mark());42;")
    |> Output.fault ~output:"M" "HCIRVM0014")

let retained_initialization () =
  let module Task = Test_integer_task in
  let session = Session.create () in
  let task = Task.create session in
  ignore
    (Task.run session task "U64 N;U0 Set(U8 *p,U8 n){*p=n;}Set((&N)(U8*),42);"
    |> Test_integer_program.checked);
  Task.value 42L
    (Task.run session task "I64 Read(U8 *p){return *p;}Read((&N)(U8*));");
  (match Task.run session task "N;" with
  | Error (diagnostic :: _) ->
      Alcotest.(check string)
        "separate command preserves unknown bytes" "HCIRVM0012" diagnostic.code
  | _ -> Alcotest.fail "partially initialized task word was readable");
  (match Task.run session task "Set((&N)(U8*),41);1/0;" with
  | Error (diagnostic :: _) ->
      Alcotest.(check string) "reached fault" "HCIRVM0009" diagnostic.code
  | _ -> Alcotest.fail "missing reached task fault");
  Task.value 41L (Task.run session task "Read((&N)(U8*));");
  ignore
    (Task.run session task
       "U0 Finish(U8 *p){I64 i;for(i=1;i<8;i++)p[i]=0;}Finish((&N)(U8*));"
    |> Test_integer_program.checked);
  Task.value 41L (Task.run session task "N;");
  ignore (Task.run session task "N=42;" |> Test_integer_program.checked);
  Task.value 42L (Task.run session task "Read((&N)(U8*));")

let stream_views () =
  List.iter
    (fun (provider, return_type) ->
      let session = Session.create () in
      let task = Test_integer_task.create session in
      ignore
        (Test_integer_task.run session task
           (Printf.sprintf
              "extern %s %s(U8 *fmt,...);U64 N=0x3b3234;%s (*p)(U8 \
               *fmt,...);p=&%s;"
              return_type provider return_type provider)
        |> Test_integer_program.checked);
      let stream = Integer_task.begin_stream task |> Result.get_ok in
      ignore
        (Test_integer_task.run session task "p(\"%s\",(&N)(U8*));"
        |> Test_integer_program.checked);
      Alcotest.(check string)
        "stream callback scans the same owned bytes" "42;"
        (Integer_task.finish_stream task stream |> Result.get_ok);
      Alcotest.(check string)
        "generated bytes stay in the active stream" ""
        (Integer_task.output_bytes task))
    [ ("StreamPrint", "U0") ];
  ignore
    (Output.run ~mode:Preprocessor.Aot
       {|#exe {U64 Text=0x3b3234;I64 (*p)(U8 *fmt,...)=&StreamExePrint;I64 n=p("%s",(&Text)(U8*));StreamPrint("%d;",n);}|}
    |> Output.expect "")

let bounds_and_quotas () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          Alcotest.(check string)
            source "HCIRVM0019" (F.first_error (G.run ~mode source)).code)
        [
          "I64 F(){U64 n=42;U8 *p=(&n)(U8*);return p[8];}F();";
          "I64 F(){U64 n=42;U8 *p=(&n)(U8*);U64 *q=(&p[1])(U64*);return \
           *q;}F();";
          "I64 F(){U8 a[7];U64 *p=a(U64*);*p=42;return 42;}F();";
        ];
      ignore
        (G.run ~mode
           "I64 F(){U64 n;U8 *p=(&n)(U8*);U64 *q=(&p[8])(U64*);return \
            q==&n;}F();"
        |> F.expect 0L);
      let source = "I64 F(){U64 n=0;U8 *p=(&n)(U8*);*p=42;return n;}F();" in
      let result = G.run ~mode ~max_frame_bytes:16 source |> F.expect 42L in
      let steps = VM.executed_steps result in
      ignore
        (G.run ~mode ~max_frame_bytes:16 ~max_steps:steps source |> F.expect 42L);
      Alcotest.(check string)
        "cast preserves frame accounting" "HCIRVM0011"
        (F.first_error (G.run ~mode ~max_frame_bytes:15 source)).code;
      Alcotest.(check string)
        "cast consumes a normal instruction" "HCIRVM0007"
        (F.first_error (G.run ~mode ~max_steps:(steps - 1) source)).code)
    G.modes

let preflight () =
  List.iter
    (fun mode ->
      let source = "I64 F(){U64 n=42;U8 *p=(&n)(U8*);return *p;}F();" in
      let compiled = G.compile ~mode source in
      let functions =
        integer_program_functions compiled
        |> List.map (fun (fn : VM.function_definition) ->
            let instructions =
              Ir_function_body.body fn.body
              |> Ir_x87_stack.verify
              |> Test_ir_integer_interpreter.require_ok (fun _ -> "x87")
              |> SI.code
            in
            let numeric =
              List.find
                (fun (d : Seq.description) -> d.opcode = Ir_opcode.Ic_imm_i64)
                instructions
            in
            {
              fn with
              body =
                S.rebuild
                  (fun (d : Seq.description) ->
                    if d.opcode = Ir_opcode.Ic_holyc_typecast then
                      {
                        d with
                        operands = [ (Option.get numeric.result).value_id ];
                      }
                    else d)
                  fn.body;
            })
      in
      S.require_preflight (SI.execute ~functions compiled);
      List.iter
        (fun rewrite ->
          let functions =
            integer_program_functions compiled
            |> List.map (fun (fn : VM.function_definition) ->
                {
                  fn with
                  body =
                    S.rebuild
                      (fun (d : Seq.description) ->
                        if d.opcode = Ir_opcode.Ic_holyc_typecast then rewrite d
                        else d)
                      fn.body;
                })
          in
          S.require_preflight (SI.execute ~functions compiled))
        [
          (fun d -> { d with Seq.payload = Some (Seq.Integer 2L) });
          (fun d -> { d with Seq.flags = 1L });
        ];
      List.iter
        (fun source ->
          Alcotest.(check string)
            "cast cannot manufacture data ownership" "HCRUN0003"
            (F.first_error (G.run ~mode source)).code)
        [
          "I64 F(){U8 *p=42(U8*);return *p;}F();";
          "I64 A(){return 42;}I64 F(){I64 (*p)();p=&A;U8 *q=p(U8*);return \
           *q;}F();";
        ])
    G.modes

let tests =
  [
    Alcotest.test_case "all scalar read views" `Quick read_classes;
    Alcotest.test_case "all scalar write views preserve neighboring bytes"
      `Quick write_classes;
    Alcotest.test_case "aliases and unaligned windows" `Quick
      aliases_and_windows;
    Alcotest.test_case "byte initialization belongs to the original object"
      `Quick initialization;
    Alcotest.test_case "pointed internals use the cast view" `Quick internals;
    Alcotest.test_case "owned byte scans through cast views" `Quick output;
    Alcotest.test_case
      "partial task objects survive commands and reached faults" `Quick
      retained_initialization;
    Alcotest.test_case "retained stream callbacks scan cast views" `Quick
      stream_views;
    Alcotest.test_case "original extents and exact quotas" `Quick
      bounds_and_quotas;
    Alcotest.test_case "raw words and callbacks cannot become data pointers"
      `Quick preflight;
  ]
