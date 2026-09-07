open Holyc_lib
module F = Test_integer_functions
module G = Test_integer_globals
module VM = Ir_integer_interpreter
module S = Test_integer_statics
module SI = Test_integer_static_initializers
module H = Test_ir_integer_interpreter
module Seq = Ir_instruction_sequence
module O = Ir_opcode
module T = Semantic_type

let gates =
  [
    ( "sum",
      "I64 Sum(U8 *p){return p[0]+p[1];}I64 F(){U8 \
       a[3];a[0]=40;a[1]=2;a[2]=0;return Sum(a);}F();" );
    ("scalar", "I64 F(){U8 n=298;return n;}F();");
    ("local-elements", "I64 F(){U8 a[3];a[0]=40;a[1]=2;return a[0]+a[1];}F();");
    ( "caller-scalar",
      "I64 Set(U8 *p){*p=298;return 0;}I64 F(){U8 n;Set(&n);return n;}F();" );
    ( "caller-element",
      "I64 Set(U8 *p){p[0]=298;return 0;}I64 F(){U8 a[3];Set(&a[1]);return \
       a[1];}F();" );
    ("row-major", "I64 F(){U8 a[2][3];a[1][2]=298;return a[1][2];}F();");
    ( "dynamic-loop",
      "I64 F(){U8 a[3];I64 i=0;while(i<3){a[i]=270;i++;}return \
       a[0]+a[1]+a[2];}F();" );
    ( "interior-pointer",
      "I64 F(){U8 a[3];a[0]=298;U8 *p=&a[1];return p[-1];}F();" );
  ]

let byte_storage () =
  S.cases
    [
      ("I64 F(){U8 n=128;return n;}F();", 128L);
      ("I64 F(){U8 n=255;return n;}F();", 255L);
      ("I64 F(){U8 n=256;return n;}F();", 0L);
      ("I64 F(){U8 n=-1;return n;}F();", 255L);
      ( "I64 F(){U8 a[5];a[0]=128;a[1]=255;a[2]=256;a[3]=298;a[4]=-1;return \
         a[0]*1000000000000+a[1]*1000000000+a[2]*1000000+a[3]*1000+a[4];}F();",
        128255000042255L );
      ( "I64 F(){U8 a[3];a[0]=17;a[1]=23;a[2]=31;U8 *p=&a[1];*p=298;return \
         a[0]*10000+a[1]*100+a[2];}F();",
        174231L );
      ( "I64 F(){U8 a=17,b=23,c=31;U8 *p=&b;*p=-1;return a*10000+b*100+c;}F();",
        195531L );
    ];
  List.iter
    (fun mode ->
      ignore
        (G.run ~mode "U64 F(){U8 a[2];a[0]=128;a[1]=-1;return a[0]+a[1];}F();"
        |> F.expect ~type_:VM.U64 383L))
    G.modes

let assignment_results () =
  (* BackC.HC's ordinary ICAssign path stores at destination width and
     independently forwards the RHS word to the expression result. *)
  S.cases
    [
      ("I64 F(){U8 n;return (n=298);}F();", 298L);
      ("I64 F(){U8 a[1];return (a[0]=298);}F();", 298L);
      ("I64 F(){U8 n;U8 *p=&n;return (*p=-1);}F();", -1L);
      ("I64 F(){U8 a[1];return (a[0]=298)+1;}F();", 299L);
      ("I64 F(){U8 a[1];I64 n=(a[0]=298);return n*1000+a[0];}F();", 298042L);
      ("I64 F(){U8 a,b;I64 n=(a=b=298);return n*10000+a*100+b;}F();", 2984242L);
      ("I64 F(){U8 n=255;return n+-1;}F();", 254L);
      ("I64 F(){U8 n=255;return -256/n;}F();", -1L);
      ("I64 F(){U8 n=255;return -256%n;}F();", -1L);
      ("I64 F(){U8 n=255;return n<-1;}F();", 1L);
      ("I64 F(){U8 n=255;return n+1;}F();", 256L);
      ("I64 F(){U8 n=255;return (n+n)/2;}F();", 255L);
    ]

let grouping_and_decay () =
  S.cases
    (List.map
       (fun source -> (source, 42L))
       [
         "I64 Set(U8 *p){p[5]=298;return 0;}I64 F(){U8 a[2][3];Set(a);return \
          a[1][2];}F();";
         "I64 Set(U8 *p){p[5]=298;return 0;}I64 F(){U8 a[2][3];Set((a));return \
          a[1][2];}F();";
         "I64 F(){U8 a[2][3];(a)[1]=298;return a[0][1];}F();";
         "I64 F(){U8 a[2][3];U8 *p=a;p[5]=298;return (a[1])[2];}F();";
         "I64 F(){U8 a[2][3];U8 *p=a[1];p[2]=298;return a[1][2];}F();";
         "I64 F(){U8 a[2][3];U8 *p=((a));p[5]=298;return ((a))[5];}F();";
         "I64 F(){U8 a[2][3];U8 *p;p=a[1];p[2]=298;return a[1][2];}F();";
         "I64 F(){U8 a[2][3];*a=298;return *a;}F();";
         "I64 F(){U8 a[2][3];a[0][3]=298;return a[1][0];}F();";
         "I64 F(){U8 a[2][3];a[2][-1]=298;return a[1][2];}F();";
         "I64 F(){U8 a[2][3];a[3][-4]=298;return a[1][2];}F();";
         "I64 Set(U8 *p){U8 *q=p;q[-1]=298;return 0;}I64 F(){U8 a[3];U8 \
          *p=&a[3];Set(p);return a[2];}F();";
         "I64 F(){U8 a[2][3][4];a[1][2][3]=298;U8 *p=a;return p[23];}F();";
       ])

let aliases_and_evaluation_order () =
  S.cases
    [
      ("I64 F(){U8 n=40,m=1;U8 *p=&n,*q=p;p=&m;*q=298;return n;}F();", 42L);
      ("I64 F(){U8 n;U8 *p,*q;q=p=&n;*q=298;return *p;}F();", 42L);
      ("I64 F(){U8 n;U8 *p=&n,*q=&*p;(*q)=298;return *(&n);}F();", 42L);
      ( "I64 Set(U8 *p){U8 n=100;p=&n;*p=99;return 0;}I64 F(){U8 n=42;U8 \
         *p=&n;Set(p);return *p;}F();",
        42L );
      ("I64 F(){U8 a[2];I64 i=0;a[i++]=i;return 40+a[0]+i;}F();", 42L);
      ("I64 F(){U8 a[2][3];I64 i=0;a[i++][i++]=40;return a[0][1]+i;}F();", 42L);
      ( "I64 F(){U8 a[2],b[2];a[0]=7;b[0]=9;U8 \
         *p=a;p[((p=b)[0]=0)+0]=42;return a[0]*100+b[0];}F();",
        4200L );
      ("I64 F(){U8 n=0,m=42;U8 *p=&n;*p=*(p=&m);return n;}F();", 42L);
      ( "I64 Set(U8 *p){*p=100;return 298;}I64 F(){U8 a[2];I64 \
         i=0;a[i++]=Set(&a[1]);return a[0]*10000+a[1]*10+i;}F();",
        421001L );
      ("I64 F(){U8 n=1,m=42;U8 *p=&n;if(n)p=&m;else p=&n;return *p;}F();", 42L);
    ]

let errors () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, expected) ->
          let error = F.first_error (G.run ~mode source) in
          Alcotest.(check string) source expected error.code;
          Alcotest.(check bool)
            "byte fault retains its function" true
            (List.mem "function=F" error.notes);
          Alcotest.(check bool)
            "byte fault retains its source span" true
            (error.primary.start >= 0
            && error.primary.stop > error.primary.start
            && error.primary.stop <= String.length source))
        [
          ("I64 F(){U8 n;return n;}F();", "HCIRVM0012");
          ("I64 F(){U8 a[3];a[0]=42;return a[1];}F();", "HCIRVM0012");
          ("I64 F(){U8 a[3];a[1]=42;return a[0];}F();", "HCIRVM0012");
          ("I64 F(){U8 n;U8 *p=&n;return *p;}F();", "HCIRVM0012");
          ("I64 F(){U8 *p;return p[0];}F();", "HCIRVM0012");
          ("I64 F(){U8 a[3],neighbor=42;return a[3];}F();", "HCIRVM0019");
          ( "I64 F(){U8 neighbor=42,a[3];a[-1]=7;return neighbor;}F();",
            "HCIRVM0019" );
          ("I64 F(){U8 a[3];U8 *p=&a[3];return *p;}F();", "HCIRVM0019");
          ("I64 F(){U8 a[3];U8 *p=&a[3];*p=42;return 42;}F();", "HCIRVM0019");
          ("I64 F(){U8 a[3];U8 *p=&a[4];return 42;}F();", "HCIRVM0019");
          ("I64 F(){U8 n=42,neighbor=7;U8 *p=&n;return p[1];}F();", "HCIRVM0019");
          ("I64 F(){U8 a[3];a[3]=1/0;return 42;}F();", "HCIRVM0009");
          ("I64 F(){U8 a[3];U64 i=-1;return a[i];}F();", "HCIRVM0020");
          ("I64 F(){U8 a[3];return a[0x7FFFFFFFFFFFFFFF];}F();", "HCIRVM0019");
          ( "I64 F(){U8 a[3];U8 *p=&a[1];return p[0x7FFFFFFFFFFFFFFF];}F();",
            "HCIRVM0020" );
          ( "I64 F(){U8 a[2][3];return a[4611686018427387904][0];}F();",
            "HCIRVM0020" );
          ( "I64 F(){U8 a[2][3];a[4611686018427387904][0]=1/0;return 42;}F();",
            "HCIRVM0020" );
        ])
    G.modes

let unsupported_domains () =
  List.iter
    (fun mode ->
      List.iter
        (fun source -> ignore (F.first_error (G.run ~mode source)))
        [
          "I64 F(){U8 a[2][3];return (a)[1][2];}F();";
          "I64 F(){U8 a[2];U8 *p=&a;return 42;}F();";
          "I64 F(){U8 a[2][3];U8 *p=&a[1];return 42;}F();";
          "I64 F(){U8 a[2];return a[1.0];}F();";
          "I64 F(){U8 a[0];return 42;}F();";
          "I64 F(){U8 a[2]={40,2};return a[0]+a[1];}F();";
          "U8 a[2];a[0]=42;a[0];";
          "U8 n=42;n;";
          "I64 F(){static U8 a[2];a[0]=42;return a[0];}F();";
          "I64 F(){static U8 n=42;return n;}F();";
          "I64 F(){U8 n=40;n+=2;return n;}F();";
          "I64 F(){U8 n=41;return ++n;}F();";
          "I64 F(){U8 n=42;return -n;}F();";
          "I64 F(){U8 n=42;return n(I64);}F();";
          "I64 F(){U8 n=42;return n(U64);}F();";
          "I64 F(){U8 a[2],n=1;a[1]=42;return a[n];}F();";
          "I64 F(){U8 a[1];a[0]=42;return a[0]++;}F();";
          "I64 F(){U8 n=40;U8 *p=&n;*p+=2;return n;}F();";
          "I64 F(U8 n){return n;}F(42);";
          "U8 F(){return 42;}F();";
          "I64 F(){I8 n=42;return n;}F();";
          "I64 F(){U16 n=42;return n;}F();";
          "I64 F(){U8 n=42;I64 *p=&n;return *p;}F();";
          "I64 F(){I64 n=42;U8 *p=&n;return *p;}F();";
          "I64 Take(U8 *p){return *p;}I64 F(){U64 n=42;return Take(&n);}F();";
          "I64 F(){U8 *p=0;return 42;}F();";
          "I64 F(){U8 n=42;U8 *p=&n;p++;return n;}F();";
          "I64 F(){U8 n=42;U8 *p=&n;return p(I64);}F();";
          "U8 *F(){U8 n=42;return &n;}42;";
          "I64 F(){U8 n=42;U8 *p=&n,**q=&p;return **q;}F();";
          "I64 F(){U8 *a[2];return a[0][0];}F();";
          "I64 F(){U8 *p=\"x\";return p[0];}F();";
        ])
    G.modes

let fresh_frames_and_images () =
  S.cases
    [
      ( "I64 R(I64 n,U8 *p){U8 a[2];a[0]=n;if(n)R(n-1,a);p[0]=p[0]+a[0];return \
         0;}I64 F(){U8 a[2];a[0]=39;R(2,a);return a[0];}F();",
        42L );
      ( "I64 Add(U8 *p,I64 n){*p=*p+n;return *p;}I64 F(){U8 n=20;return \
         Add(&n,Add(&n,1));}F();",
        42L );
      ( "I64 Seed(){U8 a[2];a[0]=40;a[1]=2;return a[0]+a[1];}I64 G=Seed();G;",
        42L );
    ];
  List.iter
    (fun mode ->
      let compiled =
        G.compile ~mode
          "I64 G=40;I64 F(){U8 a[1];a[0]=G;G=G+1;return a[0];}F();F();"
      in
      List.iter
        (fun () ->
          let result = SI.execute compiled |> H.require_ok H.show_vm_errors in
          Alcotest.(check int64)
            "each execution starts with a fresh image and byte frames" 41L
            (Option.get (VM.final_value result)).bits)
        [ (); () ];
      List.iter
        (fun source ->
          Alcotest.(check string)
            "previous activation cannot initialize this byte" "HCIRVM0012"
            (F.first_error (G.run ~mode source)).code)
        [
          "I64 F(I64 n){U8 a[1];if(n)a[0]=42;return a[0];}F(1);F(0);";
          "I64 F(I64 n){U8 a[1];if(n){a[0]=42;return F(0);}return a[0];}F(1);";
        ];
      let error =
        F.first_error
          (G.run ~mode "I64 F(){U8 a[2];a[0]=42;return a[1];}I64 G=F();42;")
      in
      Alcotest.(check string) "initializer byte fault" "HCIRVM0012" error.code;
      List.iter
        (fun note ->
          Alcotest.(check bool) note true (List.mem note error.notes))
        [ "function=F"; "initializer=G"; "initializer_phase=" ^ SI.phase mode ];
      let compiled =
        G.compile ~mode "I64 F(){U8 a[3];U8 *p=&a[1];*p=298;return a[1];}42;"
      in
      let fn = List.hd (integer_program_functions compiled) in
      List.iter
        (fun () ->
          let result =
            VM.execute_function ~max_steps:100 ~max_frame_bytes:16
              ~frame:fn.frame ~arguments:[] fn.body
            |> H.require_ok H.show_vm_errors
          in
          match VM.termination result with
          | VM.Returned (Some word) ->
              Alcotest.(check int64) "raw fresh byte frame" 42L word.bits
          | _ -> Alcotest.fail "raw byte body did not return its checked word")
        [ (); () ])
    G.modes

let resource_limits () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, bytes, depth) ->
          let result =
            G.run ~mode ~max_frame_bytes:bytes ~max_call_depth:depth source
            |> F.expect 42L
          in
          let steps = VM.executed_steps result in
          Alcotest.(check int)
            "byte frames add no initializer preparation" 0
            (VM.compiled_initializer_steps result);
          ignore
            (G.run ~mode ~max_frame_bytes:bytes ~max_call_depth:depth
               ~max_steps:steps source
            |> F.expect 42L);
          List.iter
            (fun (result, expected) ->
              Alcotest.(check string)
                "one below the exact byte budget" expected
                (F.first_error result).code)
            [
              (G.run ~mode ~max_frame_bytes:(bytes - 1) source, "HCIRVM0011");
              ( G.run ~mode ~max_call_depth:(depth - 1) source,
                if depth = 1 then "HCIRVM0001" else "HCIRVM0015" );
              (G.run ~mode ~max_steps:(steps - 1) source, "HCIRVM0007");
            ])
        [
          (List.assoc "scalar" gates, 8, 1);
          (List.assoc "local-elements" gates, 8, 1);
          (List.assoc "row-major" gates, 8, 1);
          (List.assoc "sum" gates, 16, 2);
          (List.assoc "caller-scalar" gates, 16, 2);
          (List.assoc "interior-pointer" gates, 16, 1);
          ("I64 F(){U8 a[9];a[8]=298;return a[8];}F();", 16, 1);
          ( "I64 R(I64 n,U8 *p){U8 \
             a[1];a[0]=n;if(n)R(n-1,a);p[0]=p[0]+a[0];return 0;}I64 F(){U8 \
             a[1];a[0]=39;R(2,a);return a[0];}F();",
            80,
            4 );
        ];
      let count = Int64.succ (Int64.of_int Sys.max_array_length) in
      let source = Printf.sprintf "I64 F(){U8 a[%Ld];return 42;}F();" count in
      Alcotest.(check string)
        "oversized host array rejected before byte expansion" "HCIRVM0011"
        (F.first_error (G.run ~mode ~max_frame_bytes:max_int source)).code)
    G.modes

let malformed_ir_and_owners () =
  let module Body = Ir_function_body in
  let public_u8 = H.primitive_type ~form:T.Public_spelling Primitive_type.U8 in
  let wrong_pointer = T.pointer_to H.public_i64 |> Result.get_ok in
  List.iter
    (fun mode ->
      let compiled = G.compile ~mode (List.assoc "row-major" gates) in
      let functions = integer_program_functions compiled in
      let fn = List.hd functions in
      let rewrite transform =
        let functions =
          List.map
            (fun (fn : VM.function_definition) ->
              { fn with body = S.rebuild transform fn.body })
            functions
        in
        S.require_preflight (SI.execute ~functions compiled)
      in
      List.iter
        (fun stride ->
          rewrite (fun (d : Seq.description) ->
              if d.opcode = O.Ic_imm_i64 && d.payload = Some (Seq.Integer 3L)
              then { d with payload = Some (Seq.Integer stride) }
              else d))
        [ 0L; -1L; 1L; 2L; 8L ];
      List.iter
        (fun opcode ->
          rewrite (fun (d : Seq.description) ->
              if d.opcode = opcode then
                { d with target_type = Some H.public_u64 }
              else d))
        [ O.Ic_deref; O.Ic_assign ];
      rewrite (fun (d : Seq.description) ->
          if d.opcode = O.Ic_mul then
            { d with target_type = Some wrong_pointer }
          else d);
      rewrite (fun (d : Seq.description) ->
          if d.opcode = O.Ic_deref then
            { d with target_type = Some H.public_i64 }
          else d);
      let foreign =
        G.compile ~mode (List.assoc "row-major" gates)
        |> integer_program_functions |> List.hd
      in
      S.require_preflight
        (SI.execute ~functions:[ { fn with frame = foreign.frame } ] compiled);
      S.require_preflight
        (VM.execute_function ~max_steps:100 ~max_frame_bytes:8
           ~frame:foreign.frame ~arguments:[] fn.body);
      let member member =
        Body.
          {
            position = member_position member;
            symbol = member_symbol member;
            type_ = H.public_i64;
            span = member_span member;
          }
      in
      let forged_body =
        Body.create
          Body.
            {
              function_id = function_id fn.body;
              symbol = symbol fn.body;
              function_scope = function_scope fn.body;
              return_type = return_type fn.body;
              parameters = [];
              locals = List.map member (locals fn.body);
              stored_flags = stored_flags fn.body;
              compiler_options = compiler_options fn.body;
              span = span fn.body;
              body = body fn.body;
            }
        |> H.require_ok (fun _ -> "forged member graph must be well formed")
      in
      S.require_preflight
        (SI.execute ~functions:[ { fn with body = forged_body } ] compiled);
      let compiled = G.compile ~mode (List.assoc "sum" gates) in
      let functions = integer_program_functions compiled in
      List.iter
        (fun target_type ->
          let functions =
            List.map
              (fun (fn : VM.function_definition) ->
                {
                  fn with
                  body =
                    S.rebuild
                      (fun (d : Seq.description) ->
                        if d.opcode = O.Ic_addr then
                          { d with target_type = Some target_type }
                        else d)
                      fn.body;
                })
              functions
          in
          S.require_preflight (SI.execute ~functions compiled))
        [ wrong_pointer; public_u8 ];
      let callee = List.hd functions in
      S.require_preflight
        (VM.execute_function ~max_steps:100 ~max_frame_bytes:8
           ~frame:callee.frame ~arguments:[ 42L ] callee.body);
      let compiled =
        G.compile ~mode "I64 F(){U8 n=42;U8 *p=&n;return n;}F();"
      in
      let functions =
        integer_program_functions compiled
        |> List.map (fun (fn : VM.function_definition) ->
            let address =
              Body.x87 fn.body |> SI.code
              |> List.find (fun (d : Seq.description) -> d.opcode = O.Ic_addr)
              |> fun d -> (Option.get d.result).value_id
            in
            {
              fn with
              body =
                S.rebuild
                  (fun (d : Seq.description) ->
                    if d.opcode = O.Ic_return_val then
                      { d with operands = [ address ] }
                    else d)
                  fn.body;
            })
      in
      (* References are private VM values. A forged return must fail before
         its reference could escape and outlive the owning activation. *)
      S.require_preflight (SI.execute ~functions compiled))
    G.modes

let narrow_operand_boundaries () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, index) ->
          ignore (G.run ~mode source |> F.expect 42L);
          let compiled = G.compile ~mode source in
          let functions =
            integer_program_functions compiled
            |> List.map (fun (fn : VM.function_definition) ->
                let descriptions = Ir_function_body.x87 fn.body |> SI.code in
                let byte =
                  List.find
                    (fun (d : Seq.description) ->
                      d.opcode = O.Ic_deref
                      && Option.fold ~none:false
                           ~some:(fun type_ ->
                             T.pointer_depth type_ = 0
                             && T.base type_
                                = T.Primitive
                                    (T.Public_spelling, Primitive_type.U8))
                           d.target_type)
                    descriptions
                  |> fun d -> (Option.get d.result).value_id
                in
                let consumer =
                  List.find
                    (fun (d : Seq.description) ->
                      if index then d.opcode = O.Ic_mul
                      else d.opcode = O.Ic_add && d.target_type = Some H.i64)
                    descriptions
                in
                {
                  fn with
                  body =
                    S.rebuild
                      (fun (d : Seq.description) ->
                        if
                          Seq.Instruction_id.equal d.instruction_id
                            consumer.instruction_id
                        then
                          if index then
                            { d with operands = [ List.hd d.operands; byte ] }
                          else
                            {
                              d with
                              opcode = O.Ic_holyc_typecast;
                              operands = [ byte ];
                              flags = 0L;
                              payload = Some (Seq.Integer 0L);
                            }
                        else d)
                      fn.body;
                })
          in
          S.require_preflight (SI.execute ~functions compiled))
        [
          ("I64 F(){U8 a[3],n=1;a[n+0]=298;return a[1];}F();", true);
          ("I64 F(){U8 n=42;return n+0;}F();", false);
        ])
    G.modes

let malformed_arithmetic_rank () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          ignore (G.run ~mode source |> F.expect 42L);
          let compiled = G.compile ~mode source in
          let functions =
            integer_program_functions compiled
            |> List.map (fun (fn : VM.function_definition) ->
                {
                  fn with
                  body =
                    S.rebuild
                      (fun (d : Seq.description) ->
                        if
                          d.opcode = O.Ic_add
                          && Option.fold ~none:false
                               ~some:(fun type_ -> T.pointer_depth type_ = 0)
                               d.target_type
                        then { d with target_type = Some H.u64 }
                        else d)
                      fn.body;
                })
          in
          S.require_preflight (SI.execute ~functions compiled))
        [
          "I64 F(){U8 n=40;return n+2;}F();"; "I64 F(){U8 n=21;return n+n;}F();";
        ])
    G.modes

let tests =
  List.map
    (fun (name, source) ->
      Alcotest.test_case name `Quick (fun () ->
          List.iter
            (fun mode -> ignore (G.run ~mode source |> F.expect 42L))
            G.modes))
    gates
  @ [
      Alcotest.test_case "byte narrowing zero extension and neighboring objects"
        `Quick byte_storage;
      Alcotest.test_case "assignment results retain the RHS word" `Quick
        assignment_results;
      Alcotest.test_case "byte strides grouping decay and one-past references"
        `Quick grouping_and_decay;
      Alcotest.test_case "exact aliases and evaluation order" `Quick
        aliases_and_evaluation_order;
      Alcotest.test_case "byte initialization bounds overflow and fault origins"
        `Quick errors;
      Alcotest.test_case "unsupported byte domains stay explicit" `Quick
        unsupported_domains;
      Alcotest.test_case
        "recursive activations fresh images and initializer faults" `Quick
        fresh_frames_and_images;
      Alcotest.test_case "checked frame byte and pointer-slot limits" `Quick
        resource_limits;
      Alcotest.test_case "malformed byte IR types strides layouts and owners"
        `Quick malformed_ir_and_owners;
      Alcotest.test_case "byte values cannot forge word indices or casts" `Quick
        narrow_operand_boundaries;
      Alcotest.test_case "byte arithmetic result rank is checked" `Quick
        malformed_arithmetic_rank;
    ]
