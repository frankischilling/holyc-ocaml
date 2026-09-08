open Holyc_lib
module G = Test_integer_globals
module Output = Test_integer_output
module Bytes = Test_integer_persistent_bytes
module VM = Ir_integer_interpreter
module F = Test_integer_functions
module H = Test_ir_integer_interpreter
module Seq = Ir_instruction_sequence
module O = Ir_opcode

let compound_rows =
  [
    ("255", "+=", "1", 256L, 0L, O.Ic_add_equ);
    ("0", "-=", "1", -1L, 255L, O.Ic_sub_equ);
    ("129", "*=", "2", 258L, 2L, O.Ic_mul_equ);
    ("255", "/=", "-1", 0L, 0L, O.Ic_div_equ);
    ("255", "%=", "-1", 255L, 255L, O.Ic_mod_equ);
    ("255", "&=", "511", 255L, 255L, O.Ic_and_equ);
    ("42", "|=", "256", 298L, 42L, O.Ic_or_equ);
    ("42", "^=", "256", 298L, 42L, O.Ic_xor_equ);
    ("1", "<<=", "63", Int64.min_int, 0L, O.Ic_shl_equ);
    ("255", ">>=", "63", 0L, 0L, O.Ic_shr_equ);
  ]

let increment_rows =
  [
    ("255", "++G", 0L, 0L, O.Ic_pp_);
    ("0", "--G", 255L, 255L, O.Ic_mm_);
    ("255", "G++", 255L, 0L, O.Ic__pp);
    ("0", "G--", 0L, 255L, O.Ic__mm);
  ]

let compound_results () =
  List.iter
    (fun (initial, operator, rhs, result, stored, _) ->
      List.iter
        (fun right ->
          let start =
            "I64 Id(I64 n){return n;}U8 G=" ^ initial ^ ";I64 D=" ^ rhs ^ ";"
          in
          let update = "G" ^ operator ^ right in
          Bytes.cases ~type_:VM.U64
            [
              (start ^ "(" ^ update ^ ");", result);
              (start ^ update ^ ";G;", stored);
            ])
        [ rhs; "D"; "Id(D)" ])
    compound_rows;
  Bytes.cases ~type_:VM.U64
    [
      ("U8 G=21;G<<=65;G;", 42L);
      ("U8 G=128;I64 n=1;(G>>=n);", 64L);
      ("U8 G=255;(G/=2);", 127L);
      ("U8 G=255;(G%=2);", 1L);
    ];
  Bytes.cases
    [
      ("I64 G=40;U8 D=2;(G+=D);", 42L);
      ("I64 G=-6;U8 D=2;(G/=D);", -3L);
      ("I64 G=-7;U8 D=3;(G%=D);", -1L);
    ];
  Bytes.cases ~type_:VM.U64
    [
      ("U64 G=-1;U8 D=2;(G/=D);", Int64.max_int); ("U64 G=-1;U8 D=2;(G%=D);", 1L);
    ]

let increment_results () =
  List.iter
    (fun (initial, update, result, stored, _) ->
      let start = "U8 G=" ^ initial ^ ";" in
      Bytes.cases ~type_:VM.U64
        [
          (start ^ "(" ^ update ^ ");", result); (start ^ update ^ ";G;", stored);
        ])
    increment_rows

let promoted_cases () =
  Bytes.cases
    [
      ("I64 F(){U8 n=40;n+=2;return n;}F();", 42L);
      ("I64 F(){U8 n=41;return ++n;}F();", 42L);
      ("I64 F(){U8 a[1];a[0]=42;return a[0]++;}F();", 42L);
      ("I64 F(){U8 n=40;U8 *p=&n;*p+=2;return n;}F();", 42L);
      ("I64 F(){static U8 n=41;n+=1;return n;}F();", 42L);
      ("I64 F(){U8 *s=\"*\";s[0]++;return s[0];}F();", 43L);
    ];
  Bytes.cases ~type_:VM.U64 [ ("U8 G=41;G++;G;", 42L) ];
  ignore
    (G.run ~mode:Preprocessor.Aot "U8 G;G++;G;" |> F.expect ~type_:VM.U64 1L);
  Alcotest.(check string)
    "JIT byte stays unknown" "HCIRVM0012"
    (F.first_error (G.run ~mode:Preprocessor.Jit "U8 G;G++;")).code

let instructions compiled =
  integer_program_entry compiled
  |> Ir_x87_stack.graph |> Ir_block_graph.blocks
  |> List.concat_map (fun block ->
      Ir_block_graph.instructions block |> Seq.instructions)
  |> List.map Seq.description

let canonical_preflight () =
  let examples =
    List.map
      (fun (_, operator, _, _, _, opcode) -> ("G" ^ operator ^ "1", opcode))
      compound_rows
    @ List.map
        (fun (_, update, _, _, opcode) -> (update, opcode))
        increment_rows
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (update, opcode) ->
          let source = "U8 G;G=42;" ^ update ^ ";G;" in
          let compiled = G.compile ~mode source in
          let entry = integer_program_entry compiled in
          let globals = integer_program_globals compiled in
          let code = instructions compiled in
          let instruction = List.find (fun d -> d.Seq.opcode = opcode) code in
          Alcotest.(check int64) "canonical zero flags" 0L instruction.flags;
          Alcotest.(check bool)
            "materialized result" true
            (Option.is_some instruction.result);
          Alcotest.(check bool) "no payload" true (instruction.payload = None);
          Alcotest.(check bool)
            "exact byte destination type" true
            (Semantic_type.equal
               (Option.get instruction.target_type)
               (H.primitive_type ~form:Semantic_type.Public_spelling
                  Primitive_type.U8));
          Alcotest.(check bool)
            "retains folding barrier" true
            (O.info opcode).prevents_constant_folding;
          let literal =
            List.find_map
              (fun d ->
                match (d.Seq.opcode, d.payload, d.result) with
                | O.Ic_imm_i64, Some (Seq.Integer 42L), Some result ->
                    Some result.value_id
                | _ -> None)
              code
            |> Option.get
          in
          let execute graph =
            VM.execute_program ~globals ~functions:[] ~max_steps:1000
              ~max_frame_bytes:8 ~max_call_depth:1 graph
          in
          ignore (execute entry |> H.require_ok H.show_vm_errors);
          List.iter
            (fun rewrite ->
              match
                Seq.create
                  (List.map
                     (fun d -> if d.Seq.opcode = opcode then rewrite d else d)
                     code)
              with
              | Ok _ -> Alcotest.fail "malformed update sequence accepted"
              | Error errors ->
                  Alcotest.(check bool)
                    "sequence rejects update instruction" true
                    (List.exists
                       (fun (error : Seq.error) ->
                         error.instruction_id
                         = Some
                             (Seq.Instruction_id.to_int
                                instruction.instruction_id))
                       errors))
            [
              (fun d -> { d with Seq.result = None });
              (fun d -> { d with Seq.flags = -1L });
            ];
          List.iter
            (fun rewrite ->
              let graph =
                F.rewrite_entry
                  (fun d -> if d.Seq.opcode = opcode then rewrite d else d)
                  entry
              in
              match execute graph with
              | Ok _ -> Alcotest.fail "malformed byte update executed"
              | Error errors ->
                  List.iter
                    (fun (error : VM.error) ->
                      Alcotest.(check bool)
                        "preflight rejection" true
                        (error.stage = VM.Preflight);
                      Alcotest.(check int) "no effects" 0 error.executed_steps)
                    errors)
            ([
               (fun d -> { d with Seq.target_type = Some H.public_u64 });
               (fun d -> { d with Seq.payload = Some (Seq.Integer 0L) });
               (fun d ->
                 { d with Seq.operands = literal :: List.tl d.operands });
             ]
            @ List.map
                (fun flags d -> { d with Seq.flags })
                [ 1L; 0x200L; 0x800L; 0x10000L; 0x010000000L ]);
          Alcotest.(check string)
            "deterministic update dump"
            (integer_program_human compiled)
            (integer_program_human (G.compile ~mode source)))
        examples)
    G.modes

let owner_matrix () =
  let rows =
    List.map
      (fun (initial, operator, rhs, result, stored, _) ->
        ( initial,
          (fun target -> "(" ^ target ^ operator ^ rhs ^ ")"),
          result,
          stored ))
      compound_rows
    @ List.map
        (fun (initial, expression, result, stored, _) ->
          let before = String.sub expression 0 2 in
          let update target =
            if before = "++" || before = "--" then before ^ "(" ^ target ^ ")"
            else "(" ^ target ^ ")" ^ String.sub expression 1 2
          in
          (initial, update, result, stored))
        increment_rows
  in
  List.iter
    (fun (initial, update, result, stored) ->
      let owners =
        [
          ("", "U8 n=" ^ initial ^ ";", "n", "0");
          ("", "static U8 n=" ^ initial ^ ";", "n", "0");
          ("", "U8 a[2];a[0]=" ^ initial ^ ";a[1]=85;", "a[0]", "a[1]*1000");
          ("U8 a[2]={" ^ initial ^ ",85};", "", "a[0]", "a[1]*1000");
          ("", "static U8 a[2]={" ^ initial ^ ",85};", "a[0]", "a[1]*1000");
          ("", "U8 *p=\"aU\";p[0]=" ^ initial ^ ";", "*p", "p[1]*1000");
          ("U8 n=" ^ initial ^ ";", "U8 *p=&n;", "p[0]", "0");
        ]
      in
      List.iter
        (fun (global, declarations, target, neighbor) ->
          let start =
            global ^ "I64 F(){" ^ declarations ^ "I64 result=" ^ update target
            ^ ";return "
          in
          let neighbor_value = if neighbor = "0" then 0L else 85000L in
          Bytes.cases
            [
              (start ^ "result;}F();", result);
              ( start ^ neighbor ^ "+" ^ target ^ ";}F();",
                Int64.add neighbor_value stored );
            ])
        owners)
    rows

let effects_and_calls () =
  Bytes.cases ~type_:VM.U64
    [
      ("U8 G=1;(G+=(G=2));", 4L);
      ("U8 G;G+=(G=2);G;", 4L);
      ("U8 G=1;G+=G++;G;", 3L);
      ("U8 G=1;G+=++G;G;", 4L);
      ("U8 G=1;I64 Set(){G=250;return 10;}(G+=Set());", 260L);
      ("U8 G=1;I64 Set(){G=250;return 10;}G+=Set();G;", 4L);
      ("U8 G=0;if(0&&G++){}if(1||G++){}G;", 0L);
      ("U8 G=0;(0&&G++);G;", 1L);
    ];
  Bytes.cases
    [
      ("I64 F(){U8 n=1;return n+=(n=2);}F();", 4L);
      ( "I64 F(){U8 a[2];a[0]=40;a[1]=85;I64 i=0;a[i++]+=i+1;return \
         a[0]+i*100+a[1]*1000;}F();",
        85142L );
      ( "I64 F(){U8 a[2][2];a[0][1]=40;I64 i=0;a[i++][i++]+=2;return \
         a[0][1]+i;}F();",
        44L );
      ( "I64 Set(U8 *p){*p=250;return 10;}I64 F(){U8 a[2];a[0]=1;I64 i=0;I64 \
         result=(a[i++]+=Set(a));return result*1000+a[0]*10+i;}F();",
        260041L );
      ("I64 F(){U8 a=40,b=10;U8 *p=&a;*p+=*(p=&b);return a*100+b;}F();", 5010L);
      ("U8 G=0;I64 Sub(I64 a,I64 b){return a-b;}Sub(G++,G++)*10+G;", 12L);
      ("I64 F(){U8 n=0;while(n<40)++n;do{n++;}while(n<42);return n;}F();", 42L);
      ("I64 F(){U8 n=0;for(;n<42;n++){}return n;}F();", 42L);
      ( "I64 F(I64 depth){static U8 n=38;++n;if(depth)return F(depth-1);return \
         n;}F(3);",
        42L );
      ( "U8 G=40;I64 F(I64 n,U8 *p){if(n)F(n-1,p);(*p)++;return *p;}F(1,&G);",
        42L );
      ("I64 F(I64 depth){U8 n=41;if(depth)F(depth-1);return ++n;}F(2);", 42L);
      ( "I64 F(I64 depth){U8 *p=\"(\";p[0]++;if(depth)F(depth-1);return \
         p[0];}F(1);",
        42L );
    ];
  List.iter
    (fun mode ->
      let compiled =
        G.compile ~mode
          "U8 G=19;I64 F(){static U8 n=18;U8 *p=\"\001\";G++;n++;p[0]++;return \
           G+n+p[0];}F();"
      in
      let initial =
        integer_program_globals compiled |> Ir_integer_globals.human
      in
      List.iter
        (fun () ->
          let result =
            Test_integer_static_initializers.execute compiled
            |> H.require_ok H.show_vm_errors
          in
          Alcotest.(check int64)
            "fresh globals statics and literal sites" 41L
            (Option.get (VM.final_value result)).bits)
        [ (); () ];
      Alcotest.(check string)
        "prepared images remain immutable" initial
        (integer_program_globals compiled |> Ir_integer_globals.human))
    G.modes

let runtime_faults () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, code) ->
          ignore (Output.run ~mode source |> Output.fault code))
        [
          ("I64 F(){U8 n;return n++;}F();", "HCIRVM0012");
          ("I64 F(){U8 n;return ++n;}F();", "HCIRVM0012");
          ("I64 F(){U8 a[2];a[0]=1;return a[1]+=2;}F();", "HCIRVM0012");
          ("I64 F(){U8 *p;return (*p)++;}F();", "HCIRVM0012");
          ("U8 G=42;G/=0;", "HCIRVM0009");
          ("U8 G=42;G%=0;", "HCIRVM0009");
          ("I64 F(){U8 a[2];a[0]=42;return a[2]++;}F();", "HCIRVM0019");
          ("I64 F(){U8 a=42,b=1;U8 *p=&a;return ++p[1];}F();", "HCIRVM0019");
          ( "I64 F(){U8 a[2];U8 *p=&a[1];return p[0x7FFFFFFFFFFFFFFF]++;}F();",
            "HCIRVM0020" );
          ( "I64 F(I64 n){U8 a[1];if(n)a[0]=41;return ++a[0];}F(1);F(0);",
            "HCIRVM0012" );
        ];
      List.iter
        (fun operator ->
          let source =
            Output.putchars_header
            ^ "U8 G=42;I64 R(){G=255;PutChars('A');return 0;}G" ^ operator
            ^ "R();PutChars('B');"
          in
          let report = Output.run ~mode source in
          let error = Output.fault ~output:"A" "HCIRVM0009" report in
          Alcotest.(check bool)
            "fault retains prior RHS output work" true
            (integer_program_report_output_work report > 0);
          Alcotest.(check bool)
            "fault names byte compound opcode" true
            (String.starts_with
               ~prefix:(if operator = "/=" then "IC_DIV_EQU" else "IC_MOD_EQU")
               error.message))
        [ "/="; "%=" ];
      ignore
        (Output.run ~mode
           (Output.putchars_header
          ^ "I64 R(){PutChars('A');return 2;}I64 F(){U8 \
             a[1];a[0]=42;a[1]+=R();return 42;}F();")
        |> Output.fault ~output:"A" "HCIRVM0019"))
    G.modes

let gates =
  [
    ("global compound", "U8 G=40;G+=2;G;", VM.U64);
    ("global prefix", "U8 G=41;++G;", VM.U64);
    ("local postfix", "I64 F(){U8 n=41;return n++;}F()+1;", VM.I64);
    ("static postfix", "I64 F(){static U8 n=40;n++;return n;}F();F();", VM.I64);
    ("global array", "U8 A[2]={40,0};A[0]+=2;A[0];", VM.U64);
    ("static array", "I64 F(){static U8 A[2]={41,0};return ++A[0];}F();", VM.I64);
    ("passed alias", "U8 G=41;U0 Bump(U8 *p){(*p)++;}Bump(&G);G;", VM.U64);
    ("prefix wrap", "U8 G=255;I64 N=++G;N+42;", VM.I64);
  ]

let initializer_boundaries =
  [
    ( "native wide local divisor",
      "U8 G=84;I64 F(){U8 d=554;return G/=d;}I64 N=F();N;" );
    ( "native wide local remainder divisor",
      "U8 G=85;I64 F(){U8 d=555;return G%=d;}I64 N=F();N;" );
    ( "native wide local in hidden callee result",
      "U8 G=84;I64 D(){U8 d=554;return d;}I64 F(){return G/=D();}I64 N=F();N;"
    );
    ( "native wide local changes helper control flow",
      "U8 G=41;I64 F(){U8 d=256;if(d)G++;return G;}I64 N=F();N;" );
    ( "later wide store invalidates local range",
      "U8 G=84;I64 F(){U8 d=42;d=554;return G/=d;}I64 N=F();N;" );
    ( "explicit byte register read is not bounded by its stores",
      "U8 G=84;I64 F(){U8 reg RAX d=42;return G/=d;}I64 N=F();N;" );
    ( "folded subtraction discards bit result",
      "U8 A[8]={42,1,0,0,0,0,0,0};I64 F(){(A[0]&=~256)-0;return A[1];}I64 \
       N=F();N;" );
    ( "folded multiplication discards bit result",
      "U8 A[8]={42,1,0,0,0,0,0,0};I64 F(){(A[0]&=~256)*0;return A[1];}I64 \
       N=F();N;" );
    ( "byte declaration does not erase callee intermediate",
      "U8 G=250;I64 D=2;I64 F(){return (G+=10)/D;}U8 N=F();N;" );
    ( "pushed argument retains exposed result",
      "U8 G=250;I64 Id(I64 n){return n;}I64 N=Id(G+=10);N;" );
    ( "explicit byte register address escapes",
      "I64 Bump(U8 *p){return ++*p;}I64 F(){U8 reg R15 n=255;return \
       Bump(&n);}I64 N=F();N;" );
    ( "explicit byte register array",
      "I64 F(){U8 reg R15 a[1];a[0]=255;return ++a[0];}I64 N=F();N;" );
    ("exposed compound sum", "U8 G=250;I64 N=(G+=10);N;");
    ("exposed compound difference", "U8 G=0;I64 N=(G-=1);N;");
    ("exposed compound product", "U8 G=129;I64 N=(G*=2);N;");
    ("dynamic exposed compound", "U8 G=250;I64 D=10;I64 N=(G+=D);N;");
    ("assignment forwards wide result", "U8 G=250,H;I64 N=(H=(G+=10));N;");
    ("byte type does not bound assignment", "U8 G=40,R;I64 N=(G|=(R=256));N;");
    ( "RHS effects invalidate old initializer",
      "U8 G=0;I64 R(){G=250;return 10;}I64 N=(G+=R());N;" );
    ( "automatic prefix register wrap",
      "I64 F(){U8 n=255;return ++n;}I64 N=F();N;" );
    ( "automatic discarded register wrap",
      "I64 F(){U8 n=255;n++;return n;}I64 N=F();N;" );
    ( "transitive automatic register wrap",
      "I64 F(){U8 n=255;return ++n;}I64 Outer(){return F();}I64 N=Outer();N;" );
    ( "cancelled address is not memory proof",
      "I64 F(){U8 n=255;return ++*(&n);}I64 N=F();N;" );
    ( "discarded or widens memory",
      "U8 A[8]={42,0,0,0,0,0,0,0};I64 F(){A[0]|=256;return A[1];}I64 N=F();N;"
    );
    ( "discarded xor widens memory",
      "U8 A[8]={42,1,0,0,0,0,0,0};I64 F(){A[0]^=256;return A[1];}I64 N=F();N;"
    );
    ( "discarded and widens memory",
      "U8 A[8]={42,1,0,0,0,0,0,0};I64 F(){A[0]&=~256;return A[1];}I64 N=F();N;"
    );
    ( "transitive discarded bit update",
      "U8 A[8]={42,0,0,0,0,0,0,0};I64 F(){A[0]|=256;return 42;}I64 \
       Outer(){return F();}I64 N=Outer();N;" );
  ]

let safe_initializers () =
  Bytes.cases
    [
      ("U8 G=40;I64 N=(G|=2);N;", 42L);
      ("U8 G=40,R=2;I64 N=(G|=R);N;", 42L);
      ("U8 G=43;I64 N=(G^=1);N;", 42L);
      ("U8 G=42;I64 N=(G&=511);N;", 42L);
      ("U8 G=42;I64 N=(G&=~256);N;", 42L);
      ("U8 G=42;I64 F(){return G&=~256;}I64 N=F();N;", 42L);
      ("U8 G=42;I64 Id(I64 n){return n;}I64 N=Id(G&=~256);N;", 42L);
      ("U8 G=42;I64 N=(G+=0);N;", 42L);
      ("U8 G=42;I64 N=(G-=0);N;", 42L);
      ("U8 G=42;I64 N=(G*=1);N;", 42L);
      ("U8 G=84;I64 D=2;I64 N=(G/=D);N;", 42L);
      ("U8 G=84;I64 F(){U8 d=2;return G/=d;}I64 N=F();N;", 42L);
      ("U8 G=84;I64 F(){U8 a=2,b=a;return G/=b;}I64 N=F();N;", 42L);
      ("U8 G=84;I64 F(){U8 noreg d=554;return G/=d;}I64 N=F();N;", 2L);
      ("U8 G=85;I64 D=43;I64 N=(G%=D);N;", 42L);
      ("U8 G=250;I64 F(){G+=10;return 42;}I64 N=F();N;", 42L);
      ( "U8 G=250;I64 F(){G+=10;return G;}I64 Outer(){return F();}I64 \
         N=Outer();N;",
        4L );
      ("I64 F(){static U8 n=255;return ++n;}I64 N=F();N+42;", 42L);
      ("I64 F(){U8 a[1];a[0]=255;return ++a[0];}I64 N=F();N+42;", 42L);
      ("I64 F(){U8 noreg n=255;return ++n;}I64 N=F();N+42;", 42L);
      ("U8 G=255;I64 Bump(U8 *p){return ++*p;}I64 N=Bump(&G);N+42;", 42L);
    ];
  Bytes.cases ~type_:VM.U64
    [
      ("U8 G=250;U8 H=(G+=10);H;", 4L);
      ("U8 G=250;U8 A[1]={(G+=10)};A[0];", 4L);
      ("U8 G=42;U8 H=(G|=256);H;", 42L);
    ]

let tests =
  List.map
    (fun (name, source, type_) ->
      Alcotest.test_case name `Quick (fun () ->
          List.iter
            (fun mode ->
              ignore (Output.run ~mode source |> Bytes.expect ~type_ ""))
            G.modes))
    gates
  @ List.map
      (fun (name, test) -> Alcotest.test_case name `Quick test)
      [
        ("compound full results and byte stores", compound_results);
        ("prefix postfix wraps and stored results", increment_results);
        ("promoted byte storage boundaries", promoted_cases);
        ("all canonical opcode and preflight contracts", canonical_preflight);
        ("proven native initializer update controls", safe_initializers);
        ("all update opcodes across storage owners and neighbors", owner_matrix);
        ("address RHS call loop and recursive effects", effects_and_calls);
        ("byte update faults and earlier effects", runtime_faults);
      ]
  @ List.map
      (fun (name, source) ->
        Alcotest.test_case name `Quick (fun () ->
            List.iter
              (fun mode ->
                let error = F.first_error (G.run ~mode source) in
                Alcotest.(check string)
                  "unproven native initializer update" "HCRUN0006" error.code;
                Alcotest.(check bool)
                  "update-specific proof diagnostic" true
                  (String.starts_with ~prefix:"initializer byte update"
                     error.message);
                Alcotest.(check bool)
                  "initializer owner retained" true
                  (List.mem "initializer=N" error.notes))
              G.modes))
      initializer_boundaries
