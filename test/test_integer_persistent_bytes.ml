open Holyc_lib
module F = Test_integer_functions
module G = Test_integer_globals
module Output = Test_integer_output
module VM = Ir_integer_interpreter
module Globals = Ir_integer_globals
module SI = Test_integer_static_initializers
module S = Test_integer_statics
module H = Test_ir_integer_interpreter
module Seq = Ir_instruction_sequence
module O = Ir_opcode

let cases ?(type_ = VM.I64) examples =
  List.iter
    (fun mode ->
      List.iter
        (fun (text, bits) -> ignore (G.run ~mode text |> F.expect ~type_ bits))
        examples)
    G.modes

let expect ?(type_ = VM.I64) ?(bits = 42L) output report =
  let result = (F.checked (integer_program_report_outcome report)).value in
  let word = Option.get (VM.final_value result) in
  Alcotest.(check int64) "final bits" bits word.bits;
  Alcotest.(check bool) "final word class" true (word.type_ = type_);
  Alcotest.(check string)
    "captured bytes" output
    (integer_program_report_output_bytes report);
  result

let gates =
  [
    ("global constant", "U8 G=298;G;", VM.U64, "");
    ("global pointer", "U8 G;U0 Set(U8 *p){*p=298;}Set(&G);G;", VM.U64, "");
    ( "static constant",
      "I64 Next(){static U8 n=40;n=n+1;return n;}Next();Next();",
      VM.I64,
      "" );
    ( "global scheduled",
      "extern U0 PutChars(U64 ch);I64 Seed(I64 n){PutChars(65);return n;}U8 \
       G=Seed(298);G;",
      VM.U64,
      "A" );
    ( "static scheduled",
      "extern U0 PutChars(U64 ch);I64 Seed(I64 n){PutChars(65);return n;}I64 \
       Read(){static U8 n=Seed(298);return n;}Read();",
      VM.I64,
      "A" );
    ( "static pointer",
      "U0 Set(U8 *p){*p=298;}I64 Read(){static U8 n=0;Set(&n);return n;}Read();",
      VM.I64,
      "" );
  ]

let narrowing () =
  List.iter
    (fun (literal, bits) ->
      cases ~type_:VM.U64
        [
          ("U8 G=" ^ literal ^ ";G;", bits);
          ("U8 G;G=" ^ literal ^ ";G;", bits);
          ("I64 Id(I64 n){return n;}U8 G=Id(" ^ literal ^ ");G;", bits);
        ];
      cases
        [
          ("I64 F(){static U8 n=" ^ literal ^ ";return n;}F();", bits);
          ("I64 F(){static U8 n;n=" ^ literal ^ ";return n;}F();", bits);
          ( "I64 Id(I64 n){return n;}I64 F(){static U8 n=Id(" ^ literal
            ^ ");return n;}F();",
            bits );
        ])
    [
      ("128", 128L);
      ("255", 255L);
      ("256", 0L);
      ("298", 42L);
      ("-1", 255L);
      ("0xFFFFFFFFFFFFFFFF", 255L);
      ("0x100000000000012A", 42L);
    ];
  cases ~type_:VM.U64 [ ("U8 G;(G=298);", 298L); ("U8 G;(G=-1);", -1L) ];
  cases
    [
      ("I64 F(){static U8 n;return (n=298);}F();", 298L);
      ("I64 F(){static U8 n;return (n=-1);}F();", -1L);
      ("U8 G;I64 F(){I64 result=(G=298);return result+G;}F();", 340L);
    ];
  List.iter
    (fun mode ->
      let globals =
        G.compile ~mode "U8 G=298;I64 F(){static U8 n=-1;return n;}G;"
        |> integer_program_globals
      in
      Alcotest.(check (list (option int64)))
        "stored initial images narrow" [ Some 42L; Some 255L ]
        (Globals.storage_slots globals |> List.map Globals.storage_initial_bits))
    G.modes

let byte_initializer_sources () =
  cases ~type_:VM.U64
    [
      ("U8 A=298;U8 B=A;B;", 42L);
      ("U8 A=298;U64 B=A;B;", 42L);
      ("U8 A=298;U8 B=(A=554);B;", 42L);
      ("U8 A=0;U64 B=(A=-1);B;", -1L);
      ("U64 F(){static U8 a=0;static U64 b=(a=-1);return b;}F();", -1L);
    ];
  cases
    [
      ("U8 A=298;I64 B=A;B;", 42L);
      ("U8 A=298;I64 F(){static U8 n=A;return n;}F();", 42L);
      ("U8 A=298;I64 F(){static I64 n=A;return n;}F();", 42L);
      ("I64 F(){static U8 a=298,b=a;return b;}F();", 42L);
      ("I64 F(){static U8 a=298;static I64 b=a;return b;}F();", 42L);
      ("U8 A=0;I64 B=(A=554);B;", 554L);
      ("I64 F(){static U8 a=0;static I64 b=(a=554);return b;}F();", 554L);
    ]

let lifetime_and_aliases () =
  cases
    [
      ("U8 A=1;I64 W=40;U64 Z=1;I64 F(){return A+W+Z;}F();", 42L);
      ( "I64 F(I64 depth){static U8 n=38;n=n+1;if(depth)return \
         F(depth-1);return n;}F(3);",
        42L );
      ( "extern I64 Next();I64 Next(){static U8 n=40;n=n+1;return \
         n;}Next();Next();",
        42L );
      ( "U8 n=2;I64 A(){static U8 n=18;n=n+1;return n;}I64 B(){static U8 \
         n=20;n=n+1;return n;}I64 Sum(){return A()+B()+n;}Sum();",
        42L );
      ( "U0 Set(U8 *p){*p=298;}U8 G=0;I64 F(){U8 *p=&G;Set(p);return *p;}F();",
        42L );
      ( "U0 Set(U8 *p){*p=298;}I64 F(){static U8 n=0;U8 *p=&n;Set(p);return \
         *p;}F();",
        42L );
      ( "U8 G=40;I64 F(I64 depth,U8 *p){if(depth)F(depth-1,p);*p=*p+1;return \
         *p;}F(1,&G);",
        42L );
    ];
  List.iter
    (fun mode ->
      let compiled =
        G.compile ~mode
          "U8 G=20;I64 Next(){static U8 n=19;n=n+1;G=G+1;return \
           G+n;}Next();Next();"
      in
      let initial = Globals.human (integer_program_globals compiled) in
      List.iter
        (fun () ->
          let result =
            SI.execute compiled
            |> H.require_ok (fun (errors : VM.error list) ->
                (List.hd errors).message)
          in
          Alcotest.(check int64)
            "fresh global/static cells" 43L
            (Option.get (VM.final_value result)).bits)
        [ (); () ];
      Alcotest.(check string)
        "immutable prepared image" initial
        (Globals.human (integer_program_globals compiled)))
    G.modes

let initialization_order_and_faults () =
  cases
    [
      ( "U8 G=0;I64 Never(){static U8 n=(G=298);return n;}I64 Read(){return \
         G;}Read();",
        42L );
      ( "U8 G=0;I64 Never(){if(0){static U8 n=(G=298);}return 0;}I64 \
         Read(){return G;}Read();",
        42L );
      ("U8 G=1;I64 F(){static U8 n=G;return n;}G=7;F();", 1L);
      ("U8 G=1;G=7;I64 F(){static U8 n=G;return n;}F();", 7L);
      ( "U8 G=0;I64 F(){static U8 a=(G=1),b=(G=G*10+2);return b;}I64 \
         Read(){return G;}Read();",
        12L );
    ];
  List.iter
    (fun mode ->
      List.iter
        (fun tail ->
          let source =
            "extern U0 PutChars(U64 ch);I64 Seed(I64 n){PutChars(65);return \
             42/n;}" ^ tail
          in
          let error =
            Output.run ~mode source |> Output.fault ~output:"A" "HCIRVM0009"
          in
          Alcotest.(check bool)
            "initializer phase through byte callee" true
            (List.mem ("initializer_phase=" ^ SI.phase mode) error.notes);
          Alcotest.(check bool)
            "initializer owner retained" true
            (List.mem "initializer=b" error.notes);
          Alcotest.(check bool)
            "callee retained" true
            (List.mem "function=Seed" error.notes))
        [ "U8 b=Seed(0);b;"; "I64 Never(){static U8 b=Seed(0);return b;}42;" ];
      let error =
        Output.run ~mode "I64 Never(){static U8 b=42/0;return b;}42;"
        |> Output.fault "HCIRVM0009"
      in
      Alcotest.(check bool)
        "constant preparation fault" true
        (List.mem "initializer_phase=constant-preparation" error.notes);
      List.iter
        (fun text -> ignore (Output.run ~mode text |> Output.fault "HCRUN0006"))
        [
          "I64 Seed(I64 n){return n/3;}U8 b=Seed(126);b;";
          "extern I64 Seed(I64 n);I64 Seed(I64 n){return n/3;}I64 F(){static \
           U8 b=Seed(126);return b;}F();";
          "I64 F(I64 n){static U8 b=n;return b;}F(42);";
        ])
    G.modes

let zero_and_unknown () =
  List.iter
    (fun mode -> ignore (Output.run ~mode "U8 G;42;" |> expect ""))
    G.modes;
  List.iter
    (fun (source, type_) ->
      ignore
        (Output.run ~mode:Preprocessor.Aot source |> expect ~type_ ~bits:0L "");
      let error =
        Output.run ~mode:Preprocessor.Jit source |> Output.fault "HCIRVM0012"
      in
      Alcotest.(check bool)
        "unknown persistent read is labeled" true
        (Test_function_frame_layout.contains_substring error.message
           "uninitialized JIT persistent"))
    [ ("U8 G;G;", VM.U64); ("I64 F(){static U8 n;return n;}F();", VM.I64) ];
  List.iter
    (fun mode ->
      ignore (Output.run ~mode "U8 G;G=298;G;" |> expect ~type_:VM.U64 ""))
    G.modes

let quotas () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, bytes, type_) ->
          let compiled = G.compile ~mode source in
          Alcotest.(check int)
            "declared globals plus padded statics" bytes
            (Globals.byte_size (integer_program_globals compiled));
          ignore
            (Output.run ~mode ~max_global_bytes:bytes source |> expect ~type_ "");
          if bytes > 1 then
            ignore
              (Output.run ~mode ~max_global_bytes:(bytes - 1) source
              |> Output.fault "HCIRVM0016"))
        [
          ("U8 G=298;G;", 1, VM.U64);
          ("U8 G=40,H=2;I64 F(){return G+H;}F();", 2, VM.I64);
          ("I64 F(){static U8 n=298;return n;}F();", 8, VM.I64);
          ( "U8 G=2;I64 W=20;I64 F(){static U8 n=20;return G+W+n;}F();",
            17,
            VM.I64 );
          ("U8 G=298;I64 Never(){static U8 n=0;return n;}G;", 9, VM.U64);
        ];
      let source =
        "I64 F(I64 depth){static U8 n=40;n=n+1;if(depth)return \
         F(depth-1);return n;}F(1);"
      in
      let result =
        Output.run ~mode ~max_global_bytes:8 ~max_frame_bytes:16
          ~max_call_depth:2 source
        |> expect ""
      in
      let steps = VM.executed_steps result in
      let preparation = VM.compiled_initializer_steps result in
      ignore
        (Output.run ~mode ~max_steps:steps ~max_initializer_steps:preparation
           ~max_global_bytes:8 ~max_frame_bytes:16 ~max_call_depth:2 source
        |> expect "");
      ignore
        (Output.run ~mode ~max_steps:(steps - 1) source
        |> Output.fault "HCIRVM0007");
      ignore
        (Output.run ~mode ~max_initializer_steps:(preparation - 1) source
        |> Output.fault "HCIRVM0007");
      ignore
        (Output.run ~mode ~max_frame_bytes:15 source
        |> Output.fault "HCIRVM0011");
      ignore
        (Output.run ~mode ~max_call_depth:1 source |> Output.fault "HCIRVM0015");
      let byte =
        G.run ~mode "I64 F(){static U8 n=40;n=n+1;return n;}F();" |> F.checked
      in
      let word =
        G.run ~mode "I64 F(){static I64 n=40;n=n+1;return n;}F();" |> F.checked
      in
      Alcotest.(check int)
        "byte storage adds no instructions"
        (VM.executed_steps word.value)
        (VM.executed_steps byte.value);
      Alcotest.(check int)
        "byte storage adds no preparation"
        (VM.compiled_initializer_steps word.value)
        (VM.compiled_initializer_steps byte.value))
    G.modes

let bounds_and_output_scans () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          ignore (Output.run ~mode source |> Output.fault "HCIRVM0019"))
        [
          "U8 A=17,B=42;I64 F(U8 *p){return p[1];}F(&A);";
          "U8 A=42,B=17;I64 F(U8 *p){return p[-1];}F(&B);";
          "I64 F(){static U8 a=17,b=42;U8 *p=&a;return p[1];}F();";
          "U0 Set(U8 *p){p[1]=42;}I64 F(){static U8 a=0;Set(&a);return a;}F();";
        ];
      List.iter
        (fun source ->
          ignore (Output.run ~mode source |> Output.fault "HCIRVM0019"))
        [
          "extern U0 Print(U8 *fmt,...);U8 A=65,Z=0;Print(&A);";
          "extern U0 Print(U8 *fmt,...);I64 F(){static U8 \
           a=65;Print(&a);return 42;}F();";
        ];
      ignore
        (Output.run ~mode "extern U0 Print(U8 *fmt,...);U8 Z=0;Print(&Z);42;"
        |> expect "");
      ignore
        (Output.run ~mode
           "extern U0 Print(U8 *fmt,...);I64 F(){static U8 \
            z=0;Print(&z);return 42;}F();"
        |> expect ""))
    G.modes

let malformed_owners_and_types () =
  List.iter
    (fun mode ->
      let source = "U8 G=298;I64 F(){static U8 n=G;return n;}F();" in
      let compiled = G.compile ~mode source in
      let foreign = G.compile ~mode source in
      let functions = integer_program_functions compiled in
      let fn = List.hd functions in
      let foreign_fn = List.hd (integer_program_functions foreign) in
      S.require_preflight
        (SI.execute
           ~functions:[ { fn with frame = foreign_fn.frame } ]
           compiled);
      let execute body = SI.execute ~functions:[ { fn with body } ] compiled in
      ignore
        (execute (S.rebuild Fun.id fn.body)
        |> H.require_ok (fun (errors : VM.error list) ->
            (List.hd errors).message));
      let static_options =
        S.rebuild
          ~compiler_options:
            (Int64.logxor (Ir_function_body.compiler_options fn.body) 1L)
          Fun.id fn.body
        |> execute
      in
      S.require_preflight static_options;
      (match static_options with
      | Error (error :: _) ->
          Alcotest.(check string)
            "static-specific options guard" "HCIRVM0011" error.code;
          Alcotest.(check bool)
            "exact persistent static association" true
            (Test_function_frame_layout.contains_substring error.message
               "exact function-owned persistent statics")
      | _ -> Alcotest.fail "changed static options executed");
      List.iter
        (fun opcode ->
          let original =
            Ir_function_body.x87 fn.body
            |> SI.code
            |> List.find (fun (d : Seq.description) -> d.opcode = opcode)
          in
          let body =
            S.rebuild
              (fun (d : Seq.description) ->
                if d.opcode = opcode then
                  { d with target_type = Some H.public_i64 }
                else d)
              fn.body
          in
          let outcome = execute body in
          S.require_preflight outcome;
          match outcome with
          | Error (error :: _) ->
              Alcotest.(check string) "load type guard" "HCIRVM0006" error.code;
              Alcotest.(check (option int))
                "exact invalid load instruction"
                (Some (Seq.Instruction_id.to_int original.instruction_id))
                error.instruction_id
          | _ -> Alcotest.fail "changed byte load executed")
        [ O.Ic_deref ];
      S.require_preflight
        (VM.execute_program
           ~globals:(integer_program_globals foreign)
           ~initialization:(integer_program_initialization compiled)
           ~functions ~max_steps:1000 ~max_frame_bytes:1024 ~max_call_depth:16
           (integer_program_entry compiled));
      let regions = SI.descriptions compiled in
      let first = List.hd regions in
      let foreign_region = List.hd (SI.descriptions foreign) in
      let forged = { first with static_slot = foreign_region.static_slot } in
      ignore
        (Ir_global_initialization.create
           ~globals:(integer_program_globals compiled)
           ~static_descriptions:[ first ] ~span:(SI.span first)
           ~entry:(integer_program_entry compiled)
           []
        |> F.checked);
      match
        Ir_global_initialization.create
          ~globals:(integer_program_globals compiled)
          ~static_descriptions:[ forged ] ~span:(SI.span first)
          ~entry:(integer_program_entry compiled)
          []
      with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "foreign byte initializer slot was accepted")
    G.modes

let boundaries_and_dumps () =
  List.iter
    (fun mode ->
      List.iter
        (fun source -> ignore (F.first_error (G.run ~mode source)))
        [
          "U8 *G;42;";
          "I64 F(){static U8 *p;return 42;}F();";
          "U8 G=41;G++;G;";
          "I64 F(){static U8 n=41;n+=1;return n;}F();";
        ];
      let text = "U8 G=298;I64 F(){static U8 n=298;return n;}F();" in
      let dump () = G.compile ~mode text |> integer_program_human in
      let first = dump () in
      Alcotest.(check string) "deterministic byte image dump" first (dump ());
      List.iter
        (fun part ->
          Alcotest.(check bool)
            part true
            (Test_function_frame_layout.contains_substring first part))
        [
          "holyc-integer-globals-v1 bytes=1";
          "holyc-integer-statics-v1 bytes=8";
          "initial=0x000000000000002a";
        ])
    G.modes

let tests =
  List.map
    (fun (name, source, type_, output) ->
      Alcotest.test_case name `Quick (fun () ->
          List.iter
            (fun mode ->
              ignore (Output.run ~mode source |> expect ~type_ output))
            G.modes))
    gates
  @ [
      Alcotest.test_case "initial and reached narrowing" `Quick narrowing;
      Alcotest.test_case "byte-valued initializer sources" `Quick
        byte_initializer_sources;
      Alcotest.test_case "persistent lifetime and aliases" `Quick
        lifetime_and_aliases;
      Alcotest.test_case "initializer order and fault phases" `Quick
        initialization_order_and_faults;
      Alcotest.test_case "zero images and unknown reads" `Quick zero_and_unknown;
      Alcotest.test_case "declared width and padded quotas" `Quick quotas;
      Alcotest.test_case "scalar bounds and output scans" `Quick
        bounds_and_output_scans;
      Alcotest.test_case "foreign owners and malformed types" `Quick
        malformed_owners_and_types;
      Alcotest.test_case "remaining boundaries and dumps" `Quick
        boundaries_and_dumps;
    ]
