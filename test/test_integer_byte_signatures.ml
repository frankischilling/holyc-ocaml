open Holyc_lib
module F = Test_integer_functions
module G = Test_integer_globals
module B = Test_integer_persistent_bytes
module Output = Test_integer_output
module H = Test_ir_integer_interpreter
module VM = Ir_integer_interpreter
module Frame = Semantic_function_frame_layout

let gates =
  [
    ("parameter", "I64 Echo(U8 n){return n;}Echo(42);", VM.I64);
    ("two parameters", "I64 Add(U8 a,U8 b){return a+b;}Add(20,22);", VM.I64);
    ("return", "U8 Answer(){return 42;}Answer();", VM.U64);
    ( "parameters and return",
      "U8 Add(U8 a,U8 b){return a+b;}Add(20,22);",
      VM.U64 );
    ("parameter update", "I64 Bump(U8 n){return ++n;}Bump(41);", VM.I64);
    ( "nested return",
      "U8 Inner(){return 40;}I64 Outer(U8 n){return n+Inner();}Outer(2);",
      VM.I64 );
    ( "parameter address",
      "U0 Set(U8 *p){*p=42;}I64 F(U8 n){Set(&n);return n;}F(0);",
      VM.I64 );
    ("initializer", "I64 Id(U8 n){return n;}U8 G=Id(42);G;", VM.U64);
  ]

let entry_values () =
  List.iter
    (fun (source, value) ->
      B.cases [ ("I64 F(U8 n){return n;}F(" ^ source ^ ");", value) ];
      B.cases ~type_:VM.U64
        [ ("U8 F(U8 n){return n;}F(" ^ source ^ ");", value) ])
    [
      ("554", 42L);
      ("-1", 255L);
      ("256", 0L);
      ("128", 128L);
      ("0x800000000000002A", 42L);
    ];
  B.cases
    [
      ("I64 F(I64 a,U8 b,U64 c){return a+b+c;}F(10,276,12);", 42L);
      ("I64 F(U8 a,U8 b,U8 c){return a*10000+b*100+c;}F(257,514,771);", 10203L);
      ("U8 G=40;I64 F(U8 a,U8 *p,U8 b){*p+=a;return *p+b;}F(257,&G,257);", 42L);
    ]

let return_values () =
  List.iter
    (fun (source, value) ->
      B.cases ~type_:VM.U64 [ ("U8 F(){return " ^ source ^ ";}F();", value) ];
      B.cases
        [ ("U8 F(){return " ^ source ^ ";}I64 G(){return F();}G();", value) ])
    [ ("554", 554L); ("-1", -1L); ("0x8000000000000000", Int64.min_int) ];
  B.cases
    [
      ("U8 F(){return 554;}I64 G(){I64 n=F();return n;}G();", 554L);
      ("U8 F(){return 554;}I64 G(){U8 n=F();return n;}G();", 42L);
      ("U8 F(){return 554;}I64 Id(U8 n){return n;}Id(F());", 42L);
      ("U8 F(U8 n){return n+512;}I64 G(){return F(554);}G();", 554L);
      ("U8 F(){return 40;}F()+2;", 42L);
      ("U8 F(){return 554;}I64 G(){F();return 42;}G();", 42L);
      ( "extern U8 Id(U8 n);U8 Id(U8 n){return n;}I64 F(){return Id(554);}F();",
        42L );
    ]

let standalone () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, arguments, expected, bytes) ->
          let compiled = G.compile ~mode (source ^ "42;") in
          let fn = List.hd (integer_program_functions compiled) in
          let result =
            VM.execute_function ~max_steps:100 ~max_frame_bytes:bytes
              ~frame:fn.frame ~arguments fn.body
            |> H.require_ok H.show_vm_errors
          in
          let word =
            match VM.termination result with
            | VM.Returned (Some word) -> word
            | _ -> Alcotest.fail "expected a returned word"
          in
          Alcotest.(check int64) "standalone return bits" expected word.bits;
          Alcotest.(check bool)
            "standalone U8 return is U64" true (word.type_ = VM.U64);
          List.iter
            (fun location ->
              Alcotest.(check int64)
                "byte element" 1L
                (Frame.location_element_size location);
              Alcotest.(check int64)
                "eight-byte ABI allocation" 8L
                (Frame.location_allocated_size location);
              Alcotest.(check int64)
                "eight-byte ABI slot" 8L
                (Frame.frame_slot_size
                   (Option.get (Frame.location_frame_slot location))))
            (Frame.function_locations fn.frame);
          if arguments <> [] then
            Test_integer_statics.require_preflight
              (VM.execute_function ~max_steps:100 ~max_frame_bytes:(bytes - 1)
                 ~frame:fn.frame ~arguments fn.body))
        [
          ("U8 F(U8 n){return n;}", [ 554L ], 42L, 8);
          ("U8 F(U8 a,U8 b){return a+b;}", [ 276L; 278L ], 42L, 16);
          ("U8 F(){return -1;}", [], -1L, 1);
        ])
    G.modes

let effects () =
  B.cases
    [
      ( "I64 G=0;U8 F(U8 a,U8 b){return a-b;}I64 R(){I64 n=F(G++,G++);return \
         n*10+G;}R();",
        12L );
      ( "U8 F(U8 n){if(n)return F(n-1)+1;return 40;}I64 R(){return F(258);}R();",
        42L );
      ("I64 F(U8 n){if(n==41)F(0);return n;}F(297);", 41L);
      ("I64 F(U8 a,U8 b){U8 *p=&a;*p=554;return a+b;}F(0,0);", 42L);
      ("I64 F(U8 n){U8 *p=&n;return ++*p;}F(255);", 0L);
      ("I64 F(U8 n){return n+=256;}F(42);", 298L);
      ("I64 F(U8 n){n+=256;return n;}F(42);", 42L);
      ("I64 F(U8 n){return n++;}F(255);", 255L);
    ];
  List.iter
    (fun mode ->
      ignore
        (Output.run ~mode
           (Output.print_header ^ "U8 F(){return 554;}Print(\"%d\",F());42;")
        |> Output.expect "554");
      List.iter
        (fun (source, code) ->
          ignore
            (Output.run ~mode (Output.putchars_header ^ source)
            |> Output.fault ~output:"A" code))
        [
          ( "I64 F(U8 a,U8 b){PutChars(65);U8 *p=&a;return p[1];}F(42,85);",
            "HCIRVM0019" );
          ("U8 Bad(){PutChars(65);return;}Bad();", "HCIRVM0013");
          ("U8 Bad(){PutChars(65);}Bad();", "HCIRVM0013");
          ("U8 Bad(U8 n){PutChars(65);return n/0;}Bad(42);", "HCIRVM0009");
        ])
    G.modes

let safe_initializers () =
  B.cases
    [
      ("I64 Id(U8 n){return n;}I64 N=Id(554);N;", 42L);
      ("I64 Id(U8 n){return n;}I64 N=Id(-1);N;", 255L);
      ( "U8 Id(U8 n){return n;}I64 Outer(U8 n){return Id(n);}I64 N=Outer(554);N;",
        42L );
      ("I64 Id(U8 n){n=42;return n;}I64 N=Id(0);N;", 42L);
      ("I64 Id(U8 n){U8 local=n;return local;}I64 N=Id(554);N;", 42L);
      ("U8 G=84;I64 Div(U8 n){return G/=n;}I64 N=Div(258);N;", 42L);
      ("U8 F(){return 554;}I64 N=F();N;", 554L);
      ("I64 Id(U8 noreg n){n=554;return n;}I64 N=Id(0);N;", 42L);
      ("U0 Set(U8 *p){*p=554;}I64 F(U8 n){Set(&n);return n;}I64 N=F(0);N;", 42L);
    ];
  B.cases ~type_:VM.U64 [ ("U8 F(){return 554;}U8 G=F();G;", 42L) ]

let preflight_joins () =
  let module Seq = Ir_instruction_sequence in
  let module O = Ir_opcode in
  let module T = Semantic_type in
  let public_u8 = H.primitive_type ~form:T.Public_spelling Primitive_type.U8 in
  let internal_u8 = H.primitive_type Primitive_type.U8 in
  List.iter
    (fun mode ->
      let compiled =
        G.compile ~mode "U8 Id(U8 n){return n;}I64 F(){return Id(554);}F();"
      in
      let functions = integer_program_functions compiled in
      let rebuilt =
        List.map
          (fun (fn : VM.function_definition) ->
            { fn with body = Test_integer_statics.rebuild Fun.id fn.body })
          functions
      in
      let unchanged =
        Test_integer_static_initializers.execute ~functions:rebuilt compiled
        |> H.require_ok H.show_vm_errors
      in
      Alcotest.(check int64)
        "unchanged rebuilt bodies execute" 42L
        (Option.get (VM.final_value unchanged)).bits;
      let rewrite apply =
        let functions =
          List.map
            (fun (fn : VM.function_definition) ->
              { fn with body = Test_integer_statics.rebuild apply fn.body })
            functions
        in
        Test_integer_static_initializers.execute ~functions compiled
        |> Test_integer_statics.require_preflight
      in
      List.iter
        (fun opcode ->
          List.iter
            (fun transform ->
              rewrite (fun (d : Seq.description) ->
                  if d.opcode = opcode && d.target_type = Some public_u8 then
                    transform d
                  else d))
            [
              (fun d -> { d with Seq.target_type = Some H.public_u64 });
              (fun d -> { d with Seq.target_type = Some internal_u8 });
              (fun d -> { d with Seq.flags = 1L });
              (fun d -> { d with Seq.payload = Some (Seq.Integer 0L) });
            ])
        [ O.Ic_call_end; O.Ic_return_val ];
      rewrite (fun (d : Seq.description) ->
          if
            List.mem d.opcode [ O.Ic_add_rsp; O.Ic_add_rsp1 ]
            && d.target_type = Some public_u8
          then { d with payload = Some (Seq.Integer 1L) }
          else d);
      let first = integer_program_human compiled in
      Alcotest.(check string)
        "deterministic typed signature dump" first
        (G.compile ~mode "U8 Id(U8 n){return n;}I64 F(){return Id(554);}F();"
        |> integer_program_human))
    G.modes

let unsupported_signatures () =
  List.iter
    (fun mode ->
      List.iter
        (fun source -> ignore (F.first_error (G.run ~mode source)))
        [
          "I16 F(){return 42;}F();";
          "I64 F(I8 n){return n;}F(42);";
          "I64 F(U16 n){return n;}F(42);";
          "I64 F(U8 n){return -n;}F(42);";
          "U8 Wide(){return 1;}I64 F(){U8 a[2];a[1]=42;return a[Wide()];}F();";
          "U8 *F(){return 0;}F();";
        ])
    G.modes

let fixture_limits () =
  let source = H.read_file "../examples/integer-byte-signatures.hc" in
  List.iter
    (fun mode ->
      let run ?(steps = 110) ?(prep = 7) ?(global = 12) ?(frame = 32)
          ?(depth = 2) ?(literal = 3) ?(output = 2) ?(work = 8) () =
        Output.run ~mode ~max_steps:steps ~max_initializer_steps:prep
          ~max_global_bytes:global ~max_frame_bytes:frame ~max_call_depth:depth
          ~max_literal_bytes:literal ~max_output_bytes:output
          ~max_output_work:work source
      in
      let report = run () in
      let result = Output.expect "42" report in
      Alcotest.(check int)
        "fixture runtime instructions" 110 (VM.executed_steps result);
      Alcotest.(check int)
        "fixture preparation work" 7
        (VM.compiled_initializer_steps result);
      Alcotest.(check int)
        "fixture output work" 8
        (integer_program_report_output_work report);
      List.iter
        (fun (report, code, capture) ->
          ignore (Output.fault ~output:capture code report))
        [
          (run ~steps:109 (), "HCIRVM0007", "42");
          (run ~prep:6 (), "HCIRVM0007", "");
          (run ~global:11 (), "HCIRVM0016", "");
          (run ~frame:31 (), "HCIRVM0011", "");
          (run ~depth:1 (), "HCIRVM0015", "");
          (run ~literal:2 (), "HCIRVM0021", "");
          (run ~output:1 (), "HCIRVM0022", "");
          (run ~work:7 (), "HCIRVM0023", "");
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
              ~max_steps:110 ~max_frame_bytes:32 ~max_call_depth:2
              (integer_program_entry compiled)
          in
          let result =
            VM.report_outcome report |> H.require_ok H.show_vm_errors
          in
          Alcotest.(check string)
            "fresh fixture capture" "42"
            (VM.report_output_bytes report);
          Alcotest.(check int64)
            "fresh parameter persistent and literal cells" 42L
            (Option.get (VM.final_value result)).bits)
        [ (); () ])
    G.modes

let recursive_limits () =
  let source =
    Output.putchars_header
    ^ "U8 F(U8 n){if(n)return F(n-1);return 42;}PutChars(65);F(257);"
  in
  List.iter
    (fun mode ->
      ignore
        (Output.run ~mode ~max_frame_bytes:16 ~max_call_depth:2 source
        |> B.expect ~type_:VM.U64 "A");
      ignore
        (Output.run ~mode ~max_frame_bytes:15 ~max_call_depth:2 source
        |> Output.fault ~output:"A" "HCIRVM0011");
      ignore
        (Output.run ~mode ~max_frame_bytes:16 ~max_call_depth:1 source
        |> Output.fault ~output:"A" "HCIRVM0015"))
    G.modes

let initializer_boundaries =
  [
    ("wide parameter assignment", "I64 F(U8 n){n=554;return n;}I64 N=F(0);N;");
    ( "later wide assignment",
      "I64 F(U8 n){I64 a=n;n=554;return n+a;}I64 N=F(0);N;" );
    ("parameter update", "I64 F(U8 n){return ++n;}I64 N=F(255);N;");
    ("parameter compound", "I64 F(U8 n){n+=256;return n;}I64 N=F(42);N;");
    ( "explicit parameter register",
      "I64 F(U8 reg RAX n){return n;}I64 N=F(42);N;" );
    ( "wide byte return operand",
      "U8 G=84;U8 Wide(){return 554;}I64 F(){U8 n=Wide();return G/=n;}I64 \
       N=F();N;" );
    ( "transitive wide parameter",
      "I64 F(U8 n){n=554;return n;}I64 Outer(){return F(0);}I64 N=Outer();N;" );
    ( "direct parameter address write",
      "I64 F(U8 n){*(&n)=554;return n;}I64 N=F(0);N;" );
    ( "explicit parameter escape",
      "U0 Set(U8 *p){*p=554;}I64 F(U8 reg RAX n){Set(&n);return n;}I64 \
       N=F(0);N;" );
  ]

let tests =
  List.map
    (fun (name, source, type_) ->
      Alcotest.test_case name `Quick (fun () ->
          List.iter
            (fun mode -> ignore (Output.run ~mode source |> B.expect ~type_ ""))
            G.modes))
    gates
  @ List.map
      (fun (name, test) -> Alcotest.test_case name `Quick test)
      [
        ("entry narrowing and mixed slots", entry_values);
        ("full returned register bits", return_values);
        ("standalone parameter ABI and returns", standalone);
        ("effects recursion aliases and fault output", effects);
        ("exact typed return call and cleanup joins", preflight_joins);
        ( "other signature and expression domains stay explicit",
          unsupported_signatures );
        ("fixture exact and one-below limits and fresh cells", fixture_limits);
        ("recursive ABI frame and depth limits", recursive_limits);
        ("native parameter entry proof", safe_initializers);
      ]
  @ List.map
      (fun (name, source) ->
        Alcotest.test_case name `Quick (fun () ->
            List.iter
              (fun mode ->
                let error = F.first_error (G.run ~mode source) in
                Alcotest.(check string)
                  "native compatibility boundary" "HCRUN0006" error.code;
                Alcotest.(check bool)
                  "original initializer owner" true
                  (List.mem "initializer=N" error.notes))
              G.modes))
      initializer_boundaries
