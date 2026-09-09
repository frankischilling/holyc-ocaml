open Holyc_lib
module G = Test_integer_globals
module Output = Test_integer_output
module Bytes = Test_integer_persistent_bytes
module VM = Ir_integer_interpreter
module F = Test_integer_functions
module Globals = Ir_integer_globals

let cases = Bytes.cases

let declared_size_queries () =
  cases
    [
      ("U8 A[3];sizeof A+39;", 42L);
      ("U16 A[2][3];sizeof A+30;", 42L);
      ("I64 A[1+2];sizeof A+18;", 42L);
      ("U8 A[3],B[sizeof A];sizeof B+39;", 42L);
      ("U8 A[3]={sizeof A,39,0};I64 F(){return A[0]+A[1];}F();", 42L);
      ("I64 F(){U8 A[3];return sizeof A+39;}F();", 42L);
      ("I64 F(){static U8 A[3];return sizeof A+39;}F();", 42L);
      ("I64 F(){U8 A[sizeof U8*];return sizeof A+34;}F();", 42L);
    ]

let evaluated_unbraced_bounds () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, literal, work) ->
          let literal_result = (G.run ~mode literal |> F.checked).value in
          let type_ = (Option.get (VM.final_value literal_result)).type_ in
          ignore (G.run ~mode source |> F.expect ~type_ 42L);
          let session, config, input = F.inputs ~mode source in
          let compiled =
            compile_integer_program ~max_dimension_work:work session ~config
              ~source:input
            |> F.checked
          in
          Alcotest.(check int)
            "grammar and layout reuse original numeric work" work
            (integer_program_dimension_preparation_work compiled.value);
          let instructions program =
            integer_program_initializer_preparation program
            |> Integer_initializer_preparation.executed_steps
          in
          Alcotest.(check int)
            "evaluated bounds preserve literal initializer work"
            (instructions (G.compile ~mode literal))
            (instructions compiled.value))
        [
          ("I64 A[1+1]=40,2;A[0]+A[1];", "I64 A[2]=40,2;A[0]+A[1];", 3);
          ("U8 A[sizeof U8+1]=40,2;A[0]+A[1];", "U8 A[2]=40,2;A[0]+A[1];", 3);
          ( "U8 A[2]=40,2,B[sizeof A]=20,22;A[0]+B[1]-20;",
            "U8 A[2]=40,2,B[2]=20,22;A[0]+B[1]-20;",
            2 );
          ( "I64 A[1+1][sizeof \
             U8+1]=10,10,20,2,B[1]=0;A[0][0]+A[0][1]+A[1][0]+A[1][1]+B[0];",
            "I64 \
             A[2][2]=10,10,20,2,B[1]=0;A[0][0]+A[0][1]+A[1][0]+A[1][1]+B[0];",
            7 );
          ( "U8 A[1+2]=sizeof A,39,0;A[0]+A[1];",
            "U8 A[3]=sizeof A,39,0;A[0]+A[1];",
            3 );
        ])
    G.modes

let shapes_and_aliases () =
  cases
    [
      ("I64 G[1];42;", 42L);
      ("U8 G[2];42;", 42L);
      ("I64 F(){static I64 n[2];return 0;}42;", 42L);
      ("I64 F(){static U8 n[2];return 42;}F();", 42L);
      ("I64 a[2];a[0]=42;a[0];", 42L);
      ("I64 F(){static I64 a[2];a[0]=42;return a[0];}F();", 42L);
      ("I64 F(){static U8 a[2];a[0]=42;return a[0];}F();", 42L);
      ("U64 A[2];A[0]=40;A[1]=2;I64 F(){return A[0]+A[1];}F();", 42L);
      ("I64 A[2][3];A[1][2]=42;A[1][2];", 42L);
      ("I64 A[2][3][4];A[1][2][3]=42;A[1][2][3];", 42L);
      ("I64 F(){static I64 A[2][3];A[0][3]=42;return A[1][0];}F();", 42L);
      ( "I64 F(){static U64 A[2][3][4];A[1][2][3]=42;return A[1][2][3];}F();",
        42L );
      ("I64 A[2][3];U0 Set(I64 *p){p[5]=42;}Set(A);A[1][2];", 42L);
      ("I64 A[2][3];U0 Set(I64 *p){p[2]=42;}Set(A[1]);A[1][2];", 42L);
      ("I64 A[2];U0 Set(I64 *p){p[-1]=42;}Set(&A[1]);A[0];", 42L);
      ("I64 A[2];I64 F(){I64 *p=A;p[1]=42;return A[1];}F();", 42L);
      ("I64 F(){static I64 A[2];I64 *p=A;p[1]=42;return A[1];}F();", 42L);
      ( "I64 A[2];I64 R(I64 n){if(n){A[0]+=n;R(n-1);}return A[0];}A[0]=36;R(3);",
        42L );
      ( "extern I64 F(I64 n);I64 F(I64 n){static I64 \
         A[2];if(n==3)A[0]=36;if(n){A[0]+=n;F(n-1);}return A[0];}F(3);",
        42L );
      ("U8 B=1;I64 A[2];U8 C=2;A[0]=39;A[1]=B+C;A[0]+A[1];", 42L);
      ( "I64 F(){static U8 A[2];static I64 B[2];A[0]=40;B[1]=2;return \
         A[0]+B[1];}F();",
        42L );
    ];
  cases ~type_:VM.U64
    [
      ("U8 a[2];a[0]=42;a[0];", 42L);
      ("U8 A[2][3];A[0][3]=298;A[1][0];", 42L);
      ("U8 A[2];U0 Set(U8 *p){p[1]=298;}Set(A);A[1];", 42L);
      ("U8 A[2];A[0]=554;(A[1]=A[0]);", 42L);
      ("U8 A[2];(A[1]=554);", 554L);
    ]

let quotas_and_bounds () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, bytes) ->
          let compiled = G.compile ~mode source in
          Alcotest.(check int)
            "declared/padded array bytes" bytes
            (compiled |> integer_program_globals |> Globals.byte_size);
          ignore
            (Output.run ~mode ~max_global_bytes:bytes source |> Bytes.expect "");
          ignore
            (Output.run ~mode ~max_global_bytes:(bytes - 1) source
            |> Output.fault "HCIRVM0016"))
        [
          ("U8 A[9];A[8]=42;I64 F(){return A[8];}F();", 9);
          ("I64 F(){static U8 A[9];A[8]=42;return A[8];}F();", 16);
          ("I64 A[2];A[1]=42;A[1];", 16);
          ("U8 A[9];I64 F(){static U8 B[9];B[8]=42;return B[8];}F();", 25);
        ];
      List.iter
        (fun source ->
          ignore (Output.run ~mode source |> Output.fault "HCIRVM0019"))
        [
          "I64 A[2];I64 neighbor=42;A[2];";
          "I64 neighbor=42;I64 A[2];A[-1];";
          "U8 A[9];U8 neighbor=42;A[9];";
          "I64 F(){static U8 A[9];A[0]=42;return A[9];}F();";
          "I64 F(){static I64 A[2];static I64 neighbor=42;return A[2];}F();";
        ];
      let untouched = "I64 A[2];A[0]=42;A[1];" in
      match mode with
      | Preprocessor.Jit ->
          ignore (Output.run ~mode untouched |> Output.fault "HCIRVM0012")
      | Preprocessor.Aot ->
          ignore (Output.run ~mode untouched |> Bytes.expect ~bits:0L ""))
    G.modes

let invalid_extents () =
  List.iter
    (fun mode ->
      List.iter
        (fun source -> ignore (F.first_error (G.run ~mode source)))
        [
          "I64 A[0];42;";
          "I64 A[-1];42;";
          "I64 A[];42;";
          "I64 A[9223372036854775807][2];42;";
          "U8 A[9223372036854775807];42;";
          "I64 A[1152921504606846976];42;";
          "I64 F(){static U8 A[9223372036854775807];return 42;}F();";
          "I64 n=2;I64 A[n];42;";
        ])
    G.modes

let fresh_images () =
  List.iter
    (fun mode ->
      let compiled =
        G.compile ~mode
          "U8 A[2]={19,1};I64 Next(){static U8 \
           B[2]={19,1};A[0]=A[0]+1;B[0]=B[0]+1;return \
           A[0]+B[0];}Next();Next();"
      in
      let initial = Globals.human (integer_program_globals compiled) in
      List.iter
        (fun () ->
          let result =
            Test_integer_static_initializers.execute compiled
            |> Test_ir_integer_interpreter.require_ok
                 (fun (errors : VM.error list) -> (List.hd errors).message)
          in
          Alcotest.(check int64)
            "fresh compiled array cells" 42L
            (Option.get (VM.final_value result)).bits)
        [ (); () ];
      Alcotest.(check string)
        "immutable compiled array image" initial
        (Globals.human (integer_program_globals compiled)))
    G.modes

let initializer_forms () =
  cases
    [
      ("I64 A[1]={42};A[0];", 42L);
      ("I64 A[1][1]={{42}};A[0][0];", 42L);
      ("I64 A[1]={39};while(A[0]<42){A[0]+=1;}A[0];", 42L);
      ("I64 A[1]={40};I64 F(){A[0]+=1;return A[0];}F();F();", 42L);
      ("I64 A[2][2]={{10,10},{20,2}};A[0][0]+A[0][1]+A[1][0]+A[1][1];", 42L);
      ("I64 A[2][2]={10,10,20,2};A[0][0]+A[0][1]+A[1][0]+A[1][1];", 42L);
      ("I64 A[2][2]=10,10,20,2;A[0][0]+A[0][1]+A[1][0]+A[1][1];", 42L);
      ("I64 A[2]=40,2};A[0]+A[1];", 42L);
      ("I64 A[2]={40,2,};A[0]+A[1];", 42L);
      ( "I64 F(){static I64 A[2][2]={{10,10},{20,2}};return \
         A[0][0]+A[0][1]+A[1][0]+A[1][1];}F();",
        42L );
      ("U8 B[2]={0,0};I64 A[2]={(B[0]=554),-512};A[0]+A[1];", 42L);
      ("I64 F(){static U8 A[2]={296,258};return A[0]+A[1];}F();", 42L);
    ]

let fresh_copies () =
  List.iter
    (fun mode ->
      let compiled =
        G.compile ~mode
          "U8 A[2]=\")\";I64 F(){static U8 \
           B[2]=\")\";A[0]=A[0]+1;B[0]=B[0]+1;return A[0]+B[0]-42;}F();"
      in
      let image = Globals.human (integer_program_globals compiled) in
      List.iter
        (fun () ->
          let result =
            Test_integer_static_initializers.execute compiled
            |> Test_ir_integer_interpreter.require_ok
                 (fun (errors : VM.error list) -> (List.hd errors).message)
          in
          Alcotest.(check int64)
            "fresh copied destinations" 42L
            (Option.get (VM.final_value result)).bits)
        [ (); () ];
      Alcotest.(check string)
        "immutable copied image" image
        (Globals.human (integer_program_globals compiled)))
    G.modes

let initializer_boundaries () =
  List.iter
    (fun mode ->
      List.iter
        (fun source -> ignore (F.first_error (G.run ~mode source)))
        [
          "I64 A={42};A;";
          "I64 F(){I64 A={42};return A;}F();";
          "I64 F(){static I64 A={42};return A;}F();";
          "I64 A[2]={42};42;";
          "I64 A[2]={40,2,3};42;";
          "I64 A[1]={42,};42;";
          "I64 A[1]={{42}};42;";
          "I64 F(){static U8 A[2]={42};return 42;}F();";
          "U8 A[4]=\"42\";42;";
          "U8 A[2][3]=\"42\";42;";
          "U8 A[3]=(\"42\");42;";
          "U8 A[3]={\"42\"};42;";
        ])
    G.modes

let per_leaf_phases () =
  List.iter
    (fun source ->
      ignore (Output.run ~mode:Preprocessor.Aot source |> Bytes.expect "");
      ignore
        (Output.run ~mode:Preprocessor.Jit source |> Output.fault "HCIRVM0012"))
    [
      "I64 A[2]={A[1],42};A[0];";
      "I64 F(){static I64 A[2]={A[1],42};return A[0];}F();";
    ];
  cases
    [
      ( "I64 G=0;I64 Never(){static I64 A[2]={(G=40),(G=G+2)};return A[0];}G;",
        42L );
      ("I64 G=0;I64 A[2]={(G=40),(G=G+2)};G;", 42L);
    ]

let copied_strings () =
  List.iter
    (fun mode ->
      List.iter
        (fun source -> ignore (Output.run ~mode source |> Bytes.expect "42"))
        [
          "extern U0 Print(U8 *fmt,...);U8 A[3]=\"4\" \"2\";Print(\"%s\",A);42;";
          "extern U0 Print(U8 *fmt,...);U8 \
           A[2][3]={\"42\",\"ab\"};Print(\"%s\",A[0]);42;";
          "extern U0 Print(U8 *fmt,...);U8 \
           A[3]=\"42\",B[3]=\"42\";A[0]='X';Print(\"%s\",B);42;";
          "extern U0 Print(U8 *fmt,...);I64 F(){static U8 \
           A[2][3]={\"42\",\"ab\"};Print(\"%s\",A[0]);return 42;}F();";
          "extern U0 Print(U8 *fmt,...);U8 A[1]=\"\";Print(\"%s42\",A);42;";
        ];
      ignore
        (Output.run ~mode
           "extern U0 Print(U8 *fmt,...);U8 A[2]=\"42\";Print(\"%s\",A);42;"
        |> Output.fault "HCIRVM0019"))
    G.modes

let array_dumps () =
  List.iter
    (fun mode ->
      let source =
        "U8 A[3]=\"42\";I64 F(){static I64 B[2][2]={{40,2},{0,0}};return \
         B[0][0]+B[0][1];}F();"
      in
      let dump () = G.compile ~mode source |> integer_program_human in
      let first = dump () in
      Alcotest.(check string) "deterministic array metadata" first (dump ());
      List.iter
        (fun part ->
          Alcotest.(check bool)
            part true
            (Test_function_frame_layout.contains_substring first part))
        [
          "holyc-persistent-arrays-v1";
          "dimensions=[2,2] strides=[16,8] cells=4 bytes=32";
          "cell=0 byte=0 state=prepared-bytes:343200 preparation-steps=3";
          "cell=1 byte=8 state=prepared-word:0x0000000000000002 \
           preparation-steps=4";
        ];
      Alcotest.(check bool)
        "JIT publication markers only" (mode = Preprocessor.Jit)
        (Test_function_frame_layout.contains_substring first
           "holyc-array-publications-v1"))
    G.modes

let gates =
  [
    ("global byte", "U8 A[2];A[0]=40;A[1]=2;A[0]+A[1];", VM.U64, "");
    ("global word", "I64 A[2];A[0]=40;A[1]=2;A[0]+A[1];", VM.I64, "");
    ( "static byte",
      "I64 F(){static U8 A[2];A[0]=40;A[1]=2;return A[0]+A[1];}F();",
      VM.I64,
      "" );
    ( "static word",
      "I64 F(){static I64 A[2];A[0]=40;A[1]=2;return A[0]+A[1];}F();",
      VM.I64,
      "" );
    ("global braced", "I64 A[2]={40,2};A[0]+A[1];", VM.I64, "");
    ( "static braced",
      "I64 F(){static U8 A[2]={40,2};return A[0]+A[1];}F();",
      VM.I64,
      "" );
    ( "global string",
      "extern U0 Print(U8 *fmt,...);U8 Msg[3]=\"42\";Print(\"%s\",Msg);42;",
      VM.I64,
      "42" );
    ( "static string",
      "extern U0 Print(U8 *fmt,...);I64 F(){static U8 \
       Msg[3]=\"42\";Print(\"%s\",Msg);return 42;}F();",
      VM.I64,
      "42" );
  ]

let tests =
  List.map
    (fun (name, source, type_, output) ->
      Alcotest.test_case name `Quick (fun () ->
          List.iter
            (fun mode ->
              ignore (Output.run ~mode source |> Bytes.expect ~type_ output))
            G.modes))
    gates
  @ [
      Alcotest.test_case "sizeof consumes declared array extents" `Quick
        declared_size_queries;
      Alcotest.test_case "unbraced initializers reuse checked array counts"
        `Quick evaluated_unbraced_bounds;
      Alcotest.test_case "shapes and persistent aliases" `Quick
        shapes_and_aliases;
      Alcotest.test_case "array quotas and object bounds" `Quick
        quotas_and_bounds;
      Alcotest.test_case "invalid fixed extents" `Quick invalid_extents;
      Alcotest.test_case "fresh compiled array images" `Quick fresh_images;
      Alcotest.test_case "fresh copied array images" `Quick fresh_copies;
      Alcotest.test_case "fixed initializer forms" `Quick initializer_forms;
      Alcotest.test_case "native initializer boundaries" `Quick
        initializer_boundaries;
      Alcotest.test_case "per-leaf JIT and AOT phases" `Quick per_leaf_phases;
      Alcotest.test_case "owned direct string copies" `Quick copied_strings;
      Alcotest.test_case "array shapes images and publication dumps" `Quick
        array_dumps;
    ]
