open Holyc_lib
module B = Test_integer_persistent_bytes
module VM = Ir_integer_interpreter
module F = Test_integer_functions
module G = Test_integer_globals
module H = Test_ir_integer_interpreter
module Output = Test_integer_output
module Frame = Semantic_function_frame_layout

let gates =
  [
    ("I8 local", "I64 F(){I8 n=42;return n;}F();");
    ("I16 local", "I64 F(){I16 n=42;return n;}F();");
    ("U16 local", "I64 F(){U16 n=42;return n;}F();");
    ("I32 local", "I64 F(){I32 n=42;return n;}F();");
    ("U32 local", "I64 F(){U32 n=42;return n;}F();");
    ("I16 persistent array", "I16 A[2]={40,2};I64 F(){return A[0]+A[1];}F();");
    ("I32 parameter", "I64 F(I32 n){return n;}F(42);");
    ("I8 return", "I8 F(){return 42;}F();");
  ]

let families =
  [
    ("I8", VM.I64, "255", -1L, "256");
    ("I16", VM.I64, "65535", -1L, "65536");
    ("U16", VM.U64, "65535", 65535L, "65536");
    ("I32", VM.I64, "4294967295", -1L, "4294967296");
    ("U32", VM.U64, "4294967295", 4294967295L, "4294967296");
  ]

let entry_values (name, _, maximum, expected, modulus) () =
  B.cases
    [
      ("I64 F(){" ^ name ^ " n=" ^ maximum ^ ";return n;}F();", expected);
      ("I64 F(" ^ name ^ " n){return n;}F(" ^ maximum ^ ");", expected);
      ("I64 F(){" ^ name ^ " n=" ^ modulus ^ "+42;return n;}F();", 42L);
      ("I64 F(" ^ name ^ " n){return n;}F(" ^ modulus ^ "+42);", 42L);
      ("I64 F(){" ^ name ^ " n=0x800000000000002A;return n;}F();", 42L);
      ( "I64 F(){" ^ name ^ " n=0;" ^ name ^ " *p=&n;*p=" ^ maximum
        ^ ";return n;}F();",
        expected );
    ]

let return_values (name, type_, _, _, modulus) () =
  B.cases ~type_
    [
      ( name ^ " F(){return " ^ modulus ^ "+42;}F();",
        Int64.add (Int64.of_string modulus) 42L );
      (name ^ " F(){return -1;}F();", -1L);
      (name ^ " F(){return 0x8000000000000000;}F();", Int64.min_int);
    ];
  B.cases
    [
      ( name ^ " F(){return " ^ modulus ^ "+42;}I64 G(){" ^ name
        ^ " n=F();return n;}G();",
        42L );
      ( name ^ " F(){return " ^ modulus ^ "+42;}I64 G(" ^ name
        ^ " n){return n;}G(F());",
        42L );
    ]

let persistent_values (name, type_, maximum, expected, modulus) () =
  B.cases ~type_
    [
      (name ^ " N=" ^ maximum ^ ";N;", expected);
      (name ^ " A[2]={" ^ maximum ^ ",42};A[0];", expected);
      ( name ^ " V(){return " ^ maximum ^ ";}" ^ name ^ " A[2]={V(),42};A[0];",
        expected );
      (name ^ " N=0;N=" ^ modulus ^ "+42;N;", 42L);
    ];
  B.cases
    [
      ("I64 F(){static " ^ name ^ " n=" ^ maximum ^ ";return n;}F();", expected);
      ( "I64 F(){static " ^ name ^ " a[2]={" ^ maximum ^ ",42};return a[0];}F();",
        expected );
      ( "I64 F(){" ^ name ^ " a[2];a[0]=" ^ maximum
        ^ ";a[1]=42;return a[0];}F();",
        expected );
    ]

let unsafe_initializers =
  [
    ("signed wide local", "I64 F(){I8 n=255;return n;}I64 N=F();N;");
    ("signed AND result", "I8 A=-1;I64 N=(A&=255);N;");
    ( "signed DIV result",
      "I8 A=-128;I64 D=-1;I64 F(){I8 n=(A/=D);return n;}I64 N=F();N;" );
    ("signed wider sink", "I8 A=-1;U16 N=(A&=255);N;");
    ("unsigned wider sink", "U8 A=42;U16 N=(A+=256);N;");
    ( "incompatible parameter range",
      "I64 F(U8 a){I8 n=a;return n;}I64 N=F(255);N;" );
    ( "later signed wide assignment",
      "I64 F(I16 n){I64 a=n;n=65578;return n+a;}I64 N=F(0);N;" );
    ( "transitive signed wide read",
      "I64 F(){I32 n=4294967338;return n;}I64 Outer(){return F();}I64 \
       N=Outer();N;" );
    ("explicit signed register", "I64 F(I16 reg RAX n){return n;}I64 N=F(42);N;");
    ( "unproved signed call result",
      "I8 V(){return 255;}I64 F(){I8 n=V();return n;}I64 N=F();N;" );
    ("unknown local cycle", "I64 F(){I16 a,b;a=b;b=a;return a;}I64 N=F();N;");
  ]

let safe_initializers () =
  B.cases ~type_:VM.U64 [ ("I8 A=-1;U8 N=(A&=255);N;", 255L) ];
  B.cases
    [
      ("I8 A=84;U8 D=2;I64 N=(A/=D);N;", 42L);
      ("I64 F(I8 a){I16 n=a;return n;}I64 N=F(255);N;", -1L);
      ("I64 F(){I8 noreg n=255;return n;}I64 N=F();N;", -1L);
      ("I64 F(I8 a){I16 b=a;I32 c=b;return c;}I64 N=F(255);N;", -1L);
      ("I64 F(I16 a){I8 n=a&127;return n;}I64 N=F(170);N;", 42L);
      ("I64 F(I8 a){I16 b=a|1;return b;}I64 N=F(255);N;", -1L);
    ]

let unary_producers () =
  List.iter
    (fun (unsigned, signed) ->
      B.cases
        [
          ( "I64 F(){" ^ unsigned ^ " n=84;" ^ signed ^ " d=2;return -n/d;}F();",
            -42L );
          ( "I64 F(){" ^ unsigned ^ " a=12,b=7;" ^ signed
            ^ " d=2;return -(a*b)/d;}F();",
            -42L );
          ( "I64 F(){" ^ unsigned ^ " n=84;" ^ signed
            ^ " d=2;return -(n+=0)/d;}F();",
            -42L );
          ( unsigned ^ "i V(){return 84;}" ^ signed
            ^ " D(){return 2;}I64 F(){return -V()/D();}F();",
            -42L );
          ( unsigned ^ " V(){return 0;}" ^ signed
            ^ " D(){return 2;}I64 F(){return -!V()/D();}F();",
            0L );
        ];
      let call_expected =
        if unsigned = "U8" then -42L else 9223372036854775766L
      in
      B.cases
        [
          ( unsigned ^ " V(){return 84;}" ^ signed
            ^ " D(){return 2;}I64 F(){return -V()/D();}F();",
            call_expected );
          ( unsigned ^ " V(){return 84;}" ^ signed
            ^ " D(){return 2;}I64 F(){return -(+V())/D();}F();",
            call_expected );
        ];
      B.cases ~type_:VM.U64
        [
          ( unsigned ^ " V(){return -42;}" ^ unsigned
            ^ " D(){return 2;}(-~V()/D());",
            9223372036854775787L );
        ];
      B.cases
        [
          ( "I64 F(){" ^ unsigned ^ " n=84,d=2;return (~n)/d;}F();",
            9223372036854775765L );
          ("I64 F(){" ^ unsigned ^ " n=84;return ~n>0;}F();", 1L);
          ("I64 F(){" ^ unsigned ^ " n=84;return -~n;}F();", 85L);
        ])
    [ ("U8", "I8"); ("U16", "I16"); ("U32", "I32"); ("U64", "I64") ]

let update_values (name, type_, maximum, old, modulus) () =
  let modulus_bits = Int64.of_string modulus in
  let unsigned = type_ = VM.U64 in
  let rows =
    [
      ("40", "+=", "2", 42L, 42L);
      ("0", "-=", "1", -1L, old);
      ( maximum,
        "*=",
        "2",
        Int64.mul old 2L,
        if unsigned then Int64.pred old else -2L );
      ( maximum,
        "/=",
        "-1",
        (if unsigned then 0L else 1L),
        if unsigned then 0L else 1L );
      ( maximum,
        "%=",
        "-1",
        (if unsigned then old else 0L),
        if unsigned then old else 0L );
      (maximum, "&=", maximum, Int64.of_string maximum, old);
      ("42", "|=", modulus, Int64.add modulus_bits 42L, 42L);
      ("42", "^=", modulus, Int64.add modulus_bits 42L, 42L);
      ("1", "<<=", "63", Int64.min_int, 0L);
      ( maximum,
        ">>=",
        "1",
        (if unsigned then Int64.div old 2L else -1L),
        if unsigned then Int64.div old 2L else -1L );
    ]
  in
  List.iter
    (fun (initial, operator, rhs, full, stored) ->
      let prefix = name ^ " G=" ^ initial ^ ";I64 R(){return " ^ rhs ^ ";}" in
      B.cases ~type_
        [
          (prefix ^ "(G" ^ operator ^ "R());", full);
          (prefix ^ "G" ^ operator ^ "R();G;", stored);
        ])
    rows;
  let low = if unsigned then 0L else Int64.neg (Int64.div modulus_bits 2L) in
  let high = if unsigned then old else Int64.pred (Int64.div modulus_bits 2L) in
  List.iter
    (fun (initial, update, result, stored) ->
      let prefix = name ^ " G=" ^ Int64.to_string initial ^ ";" in
      B.cases ~type_
        [
          (prefix ^ "(" ^ update ^ ");", result);
          (prefix ^ update ^ ";G;", stored);
        ])
    [
      (high, "++G", low, low);
      (high, "G++", high, low);
      (low, "--G", high, high);
      (low, "G--", low, high);
    ]

let owners_and_abi (name, type_, maximum, expected, _) () =
  B.cases
    [
      ("I64 F(){" ^ name ^ " n=41;return ++n;}F();", 42L);
      ("I64 F(){" ^ name ^ " a[2];a[0]=41;a[1]=0;return ++a[0]+a[1];}F();", 42L);
      (name ^ " n=41;I64 F(){return ++n;}F();", 42L);
      (name ^ " a[2]={41,0};I64 F(){return ++a[0]+a[1];}F();", 42L);
      ("I64 F(){static " ^ name ^ " n=41;return ++n;}F();", 42L);
      ("I64 F(){static " ^ name ^ " a[2]={41,0};return ++a[0]+a[1];}F();", 42L);
      ("I64 F(" ^ name ^ " n){return ++n;}F(41);", 42L);
      ( "U0 Set(" ^ name ^ " *p){*p=41;}I64 F(" ^ name
        ^ " n){Set(&n);return ++n;}F(0);",
        42L );
      (name ^ " G=0;I64 R(){G=40;return 2;}I64 F(){G+=R();return G;}F();", 42L);
    ];
  List.iter
    (fun mode ->
      let compiled =
        G.compile ~mode (name ^ " F(" ^ name ^ " n){return n;}42;")
      in
      let fn = List.hd (integer_program_functions compiled) in
      let result =
        VM.execute_function ~max_steps:100 ~max_frame_bytes:8 ~frame:fn.frame
          ~arguments:[ Int64.of_string maximum ]
          fn.body
        |> H.require_ok H.show_vm_errors
      in
      (match VM.termination result with
      | VM.Returned (Some word) ->
          Alcotest.(check int64) "standalone declared entry" expected word.bits;
          Alcotest.(check bool)
            "standalone return class" true (word.type_ = type_)
      | _ -> Alcotest.fail "missing standalone word");
      let location = List.hd (Frame.function_locations fn.frame) in
      let width =
        if name = "I8" then 1L
        else if name = "I16" || name = "U16" then 2L
        else 4L
      in
      Alcotest.(check int64)
        "declared parameter width" width
        (Frame.location_element_size location);
      Alcotest.(check int64)
        "eight-byte ABI allocation" 8L
        (Frame.location_allocated_size location);
      Test_integer_statics.require_preflight
        (VM.execute_function ~max_steps:100 ~max_frame_bytes:7 ~frame:fn.frame
           ~arguments:[ 42L ] fn.body);
      ignore
        (Output.run ~mode
           (Output.putchars_header ^ "I64 F(" ^ name ^ " n){PutChars(65);"
          ^ name ^ " *p=&n;return p[1];}F(42);")
        |> Output.fault ~output:"A" "HCIRVM0019"))
    G.modes

let signed_copies () =
  B.cases
    [
      ("I8 A[3]=\"\\xFF\\x80\";A[0];", -1L);
      ("I8 A[3]=\"\\xFF\\x80\";A[1];", -128L);
      ("I8 A[3]=\"\\xFF\\x80\";A[2];", 0L);
      ("I64 F(){static I8 A[3]=\"\\xFF\\x80\";return A[0]+A[1];}F();", -129L);
      ("I8 A[2][2]={\"\\xFF\",\"*\"};A[1][0];", 42L);
    ]

let physical_bit_bounds () =
  List.iter
    (fun (name, _, _, _, modulus) ->
      let inside = Int64.div (Int64.of_string modulus) 2L |> Int64.to_string in
      B.cases
        [
          ( name ^ " A[2]={42,1};I64 F(){A[0]|=" ^ inside
            ^ ";return A[1];}I64 N=F();N;",
            1L );
        ];
      List.iter
        (fun operation ->
          let source =
            name ^ " A[2]={42,1};I64 F(){" ^ operation
            ^ ";return A[1];}I64 N=F();N;"
          in
          List.iter
            (fun mode ->
              let error = F.first_error (G.run ~mode source) in
              Alcotest.(check string)
                "physical width boundary" "HCRUN0006" error.code;
              Alcotest.(check bool)
                "original initializer" true
                (List.mem "initializer=N" error.notes))
            G.modes)
        [
          "A[0]|=" ^ modulus;
          "A[0]^=" ^ modulus;
          "A[0]&=~" ^ modulus;
          "(A[0]&=~" ^ modulus ^ ")-0";
        ])
    families

let preflight_classes () =
  let module Seq = Ir_instruction_sequence in
  let module O = Ir_opcode in
  let module T = Semantic_type in
  let module S = Test_integer_statics in
  List.iter
    (fun (unsigned, signed) ->
      let name = Primitive_type.to_string unsigned in
      List.iter
        (fun mode ->
          let compiled =
            G.compile ~mode (name ^ " V(){return 42;}I64 F(){return -~V();}F();")
          in
          let functions = integer_program_functions compiled in
          let execute rewrite =
            let functions =
              List.map
                (fun (fn : VM.function_definition) ->
                  { fn with body = S.rebuild rewrite fn.body })
                functions
            in
            Test_integer_static_initializers.execute ~functions compiled
          in
          let unchanged = execute Fun.id |> H.require_ok H.show_vm_errors in
          Alcotest.(check int64)
            "unchanged reconstructed complement" 43L
            (Option.get (VM.final_value unchanged)).bits;
          let expected = H.primitive_type signed in
          let negate_count = ref 0 in
          ignore
            (execute (fun (d : Seq.description) ->
                 if d.opcode = O.Ic_unary_minus then (
                   incr negate_count;
                   Alcotest.(check bool)
                     "native signed partner target" true
                     (d.target_type = Some expected));
                 d)
            |> H.require_ok H.show_vm_errors);
          Alcotest.(check int) "one audited negation" 1 !negate_count;
          List.iter
            (fun wrong ->
              S.require_preflight
                (execute (fun (d : Seq.description) ->
                     if d.opcode = O.Ic_unary_minus then
                       { d with target_type = Some wrong }
                     else d)))
            [
              H.primitive_type unsigned;
              H.primitive_type ~form:T.Public_spelling signed;
              H.primitive_type
                (if signed = Primitive_type.I8 then Primitive_type.I16
                 else Primitive_type.I8);
            ];
          List.iter
            (fun opcode ->
              List.iter
                (fun rewrite ->
                  S.require_preflight
                    (execute (fun (d : Seq.description) ->
                         if d.opcode = opcode then rewrite d else d)))
                [
                  (fun d -> { d with Seq.flags = 1L });
                  (fun d -> { d with Seq.payload = Some (Seq.Integer 0L) });
                ])
            [ O.Ic_com; O.Ic_unary_minus ];
          S.require_preflight
            (execute (fun (d : Seq.description) ->
                 if d.opcode = O.Ic_com then { d with target_type = Some H.u64 }
                 else d)))
        G.modes)
    [
      (Primitive_type.U8, Primitive_type.I8); (U16, I16); (U32, I32); (U64, I64);
    ]

let preflight_storage () =
  let module Seq = Ir_instruction_sequence in
  let module O = Ir_opcode in
  let module T = Semantic_type in
  let module S = Test_integer_statics in
  List.iter
    (fun (name, _, _, _, _) ->
      List.iter
        (fun mode ->
          let compiled =
            G.compile ~mode
              (name ^ " Id(" ^ name ^ " n){return n;}" ^ name ^ " F(" ^ name
             ^ " n){n+=1;n-=1;n*=1;n/=1;n%=43;n&=255;"
             ^ "n|=0;n^=0;n<<=0;n>>=0;++n;--n;n++;n--;return Id(n);}F(42);")
          in
          let functions = integer_program_functions compiled in
          let execute rewrite =
            let functions =
              List.map
                (fun (fn : VM.function_definition) ->
                  { fn with body = S.rebuild rewrite fn.body })
                functions
            in
            Test_integer_static_initializers.execute ~functions compiled
          in
          let unchanged = execute Fun.id |> H.require_ok H.show_vm_errors in
          Alcotest.(check int64)
            "unchanged reconstructed storage" 42L
            (Option.get (VM.final_value unchanged)).bits;
          let primitive = Primitive_type.of_spelling name |> Option.get in
          let internal = H.primitive_type primitive in
          let same_width_other =
            match primitive with
            | Primitive_type.I8 -> Primitive_type.U8
            | I16 -> U16
            | U16 -> I16
            | I32 -> U32
            | U32 -> I32
            | _ -> assert false
          in
          List.iter
            (fun opcode ->
              List.iter
                (fun rewrite ->
                  S.require_preflight
                    (execute (fun (d : Seq.description) ->
                         if d.opcode = opcode then rewrite d else d)))
                [
                  (fun d -> { d with Seq.target_type = Some internal });
                  (fun d ->
                    {
                      d with
                      Seq.target_type =
                        Some
                          (H.primitive_type ~form:T.Public_spelling
                             same_width_other);
                    });
                  (fun d -> { d with Seq.flags = 1L });
                  (fun d -> { d with Seq.payload = Some (Seq.Integer 0L) });
                ])
            [
              O.Ic_deref;
              Ic_add_equ;
              Ic_sub_equ;
              Ic_mul_equ;
              Ic_div_equ;
              Ic_mod_equ;
              Ic_and_equ;
              Ic_or_equ;
              Ic_xor_equ;
              Ic_shl_equ;
              Ic_shr_equ;
              Ic_pp_;
              Ic_mm_;
              Ic__pp;
              Ic__mm;
              Ic_return_val;
              Ic_call_end;
            ])
        G.modes)
    families

let fixture_limits () =
  let source = H.read_file "../examples/integer-narrow.hc" in
  List.iter
    (fun mode ->
      let run ?(steps = 139) ?(prep = 9) ?(global = 16) ?(frame = 40)
          ?(depth = 2) ?(literal = 3) ?(output = 2) ?(work = 5) () =
        Output.run ~mode ~max_steps:steps ~max_initializer_steps:prep
          ~max_global_bytes:global ~max_frame_bytes:frame ~max_call_depth:depth
          ~max_literal_bytes:literal ~max_output_bytes:output
          ~max_output_work:work source
      in
      let report = run () in
      let result = Output.expect "42" report in
      Alcotest.(check int)
        "fixture runtime instructions" 139 (VM.executed_steps result);
      Alcotest.(check int)
        "fixture preparation work" 9
        (VM.compiled_initializer_steps result);
      Alcotest.(check int)
        "fixture output work" 5
        (integer_program_report_output_work report);
      List.iter
        (fun (report, code, capture) ->
          ignore (Output.fault ~output:capture code report))
        [
          (run ~steps:138 (), "HCIRVM0007", "42");
          (run ~prep:8 (), "HCIRVM0007", "");
          (run ~global:15 (), "HCIRVM0016", "");
          (run ~frame:39 (), "HCIRVM0011", "");
          (run ~depth:1 (), "HCIRVM0015", "");
          (run ~literal:2 (), "HCIRVM0021", "");
          (run ~output:1 (), "HCIRVM0022", "");
          (run ~work:4 (), "HCIRVM0023", "");
        ];
      let compiled = G.compile ~mode source in
      List.iter
        (fun () ->
          let report =
            VM.execute_program_report
              ~runtime_calls:(integer_program_runtime_calls compiled)
              ~globals:(integer_program_globals compiled)
              ~initialization:(integer_program_initialization compiled)
              ~functions:(integer_program_functions compiled)
              ~max_steps:139 ~max_frame_bytes:40 ~max_call_depth:2
              (integer_program_entry compiled)
          in
          let result =
            VM.report_outcome report |> H.require_ok H.show_vm_errors
          in
          Alcotest.(check string)
            "fresh capture" "42"
            (VM.report_output_bytes report);
          Alcotest.(check int64)
            "fresh signed cells and normalized calls" 42L
            (Option.get (VM.final_value result)).bits)
        [ (); () ])
    G.modes

let raw_class_matrix () =
  let rows =
    [
      ("I8", 4, -1L, -2L);
      ("U8", 5, 255L, 254L);
      ("I16", 6, -1L, -2L);
      ("U16", 7, 65535L, 65534L);
      ("I32", 8, -1L, -2L);
      ("U32", 9, 4294967295L, 4294967294L);
      ("I64", 10, -1L, -2L);
      ("U64", 11, -1L, -2L);
    ]
  in
  List.iter
    (fun (left, left_rank, left_bits, _) ->
      List.iter
        (fun (right, right_rank, _, right_bits) ->
          let unsigned = max left_rank right_rank mod 2 = 1 in
          let divide = if unsigned then Int64.unsigned_div else Int64.div in
          B.cases
            [
              ( "I64 F(){" ^ left ^ " a=-1;" ^ right ^ " b=-2;return a/b;}F();",
                divide left_bits right_bits );
            ])
        rows)
    rows

let unsupported_domains () =
  List.iter
    (fun mode ->
      List.iter
        (fun source -> ignore (F.first_error (G.run ~mode source)))
        [
          "Bool N=42;N;";
          "U0 N=42;N;";
          "I0 N=42;N;";
          "F64 N=42;N;";
          "I64 F(){I8 *p=\"42\";return p[0];}F();";
          "I64 F(){I16 n=42;U16 *p=&n;return *p;}F();";
          "class C{I8 n;};I64 F(){C c;c.n=42;return c.n;}F();";
        ];
      List.iter
        (fun (name, type_, _, _, _) ->
          List.iter
            (fun source -> ignore (F.first_error (G.run ~mode source)))
            [
              "I64 F(){" ^ name ^ " a[2]={40,2};return a[0]+a[1];}F();";
              "I64 F(){" ^ name ^ " a[2]=\"42\";return a[0];}F();";
              "I64 F(){" ^ name ^ " a[2],i=1;a[1]=42;return a[i];}F();";
              "I64 F(){" ^ name ^ " n=42;return n(I64);}F();";
            ];
          if name <> "I8" then
            ignore (F.first_error (G.run ~mode (name ^ " A[2]=\"42\";A[0];")));
          if mode = Preprocessor.Aot then
            ignore (G.run ~mode (name ^ " A;A;") |> F.expect ~type_ 0L)
          else
            Alcotest.(check string)
              "unknown JIT narrow cell" "HCIRVM0012"
              (F.first_error (G.run ~mode (name ^ " A;A;"))).code)
        families)
    G.modes

let tests =
  List.map
    (fun (name, source) ->
      Alcotest.test_case name `Quick (fun () -> B.cases [ (source, 42L) ]))
    gates
  @ List.concat_map
      (fun ((name, _, _, _, _) as family) ->
        [
          Alcotest.test_case
            (name ^ " storage and entry")
            `Quick (entry_values family);
          Alcotest.test_case (name ^ " full returns") `Quick
            (return_values family);
          Alcotest.test_case
            (name ^ " persistent and array storage")
            `Quick (persistent_values family);
          Alcotest.test_case
            (name ^ " all canonical updates")
            `Quick (update_values family);
          Alcotest.test_case
            (name ^ " owners and parameter ABI")
            `Quick (owners_and_abi family);
        ])
      families
  @ [
      Alcotest.test_case "safe native initializers" `Quick safe_initializers;
      Alcotest.test_case "unary storage arithmetic and call producers" `Quick
        unary_producers;
      Alcotest.test_case "signed persistent string copies" `Quick signed_copies;
      Alcotest.test_case "physical bit bounds and folded observers" `Quick
        physical_bit_bounds;
      Alcotest.test_case "forged computation classes" `Quick preflight_classes;
      Alcotest.test_case "forged narrow storage and update joins" `Quick
        preflight_storage;
      Alcotest.test_case "fixture limits and fresh images" `Quick fixture_limits;
      Alcotest.test_case "interleaved binary raw classes" `Quick
        raw_class_matrix;
      Alcotest.test_case "other domains and unknown cells" `Quick
        unsupported_domains;
    ]
  @ List.map
      (fun (name, source) ->
        Alcotest.test_case name `Quick (fun () ->
            List.iter
              (fun mode ->
                let error = F.first_error (G.run ~mode source) in
                Alcotest.(check string) "native boundary" "HCRUN0006" error.code;
                Alcotest.(check bool)
                  "original owner" true
                  (List.mem "initializer=N" error.notes))
              G.modes))
      unsafe_initializers
