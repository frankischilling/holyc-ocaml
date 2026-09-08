open Holyc_lib
module F = Test_integer_functions
module G = Test_integer_globals
module VM = Ir_integer_interpreter
module H = Test_ir_integer_interpreter
module S = Test_integer_statics
module SI = Test_integer_static_initializers
module Seq = Ir_instruction_sequence
module O = Ir_opcode
module R = Ir_runtime_call_context

let print_header = "extern U0 Print(U8 *fmt,...);"
let putchars_header = "extern U0 PutChars(U64 ch);"

let gates =
  [
    ("implicit-Print", print_header ^ "\"42\\n\";42;");
    ("explicit-Print", print_header ^ "Print(\"42\\n\");42;");
    ("explicit-PutChars", putchars_header ^ "PutChars('42\\n');42;");
    ("implicit-PutChars", putchars_header ^ "'42\\n';42;");
  ]

let run ?(mode = Preprocessor.Jit) ?max_output_bytes ?max_output_work
    ?max_initializer_steps ?max_global_bytes ?max_literal_bytes ?max_frame_bytes
    ?max_call_depth ?(max_steps = 10000) text =
  let session, config, source = F.inputs ~mode text in
  run_integer_program_report ?max_output_bytes ?max_output_work
    ?max_initializer_steps ?max_global_bytes ?max_literal_bytes ?max_frame_bytes
    ?max_call_depth ~max_steps session ~config ~source

let expect ?(value = Some 42L) output report =
  let result = (F.checked (integer_program_report_outcome report)).value in
  Alcotest.(check string)
    "exact captured bytes" output
    (integer_program_report_output_bytes report);
  Alcotest.(check bool)
    "successful report reaches stream end" true
    (VM.termination result = VM.Stream_end);
  (match value with
  | None ->
      Alcotest.(check bool)
        "successful report has no final word" true
        (VM.final_value result = None)
  | Some bits ->
      let word = Option.get (VM.final_value result) in
      Alcotest.(check int64) "final word" bits word.bits;
      Alcotest.(check bool) "final class" true (word.type_ = VM.I64));
  result

let fault ?(output = "") code report =
  let error = F.first_error (integer_program_report_outcome report) in
  Alcotest.(check string)
    ("output failure code: " ^ error.message)
    code error.code;
  Alcotest.(check string)
    "capture survives independently of failure" output
    (integer_program_report_output_bytes report);
  error

let cases examples =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, output) -> ignore (run ~mode source |> expect output))
        examples)
    G.modes

let formats_and_binary_bytes () =
  cases
    [
      (print_header ^ "Print(\"\");42;", "");
      (print_header ^ "Print(\"%%\");42;", "%");
      (print_header ^ "Print(\"%d\",0);42;", "0");
      (print_header ^ "Print(\"%d\",-1);42;", "-1");
      (print_header ^ "Print(\"%d\",(+(-1)));42;", "-1");
      ( print_header
        ^ "Print(\"%d,%d\",-9223372036854775807-1,9223372036854775807);42;",
        "-9223372036854775808,9223372036854775807" );
      (print_header ^ "U64 n=-1;Print(\"%d\",n);42;", "-1");
      (print_header ^ "Print(\"%s\",\"42\\n\");42;", "42\n");
      (print_header ^ "Print(\"%s\",\"A\\0B\");42;", "A");
      (print_header ^ "Print(\"A\\0B\");42;", "A");
      (print_header ^ "Print(\"%c\",0x00420041);42;", "A");
      (putchars_header ^ "PutChars(0x00420041);42;", "AB");
      (print_header ^ "Print(\"%c\",'ABCDEFGH');42;", "ABCDEFGH");
      (putchars_header ^ "PutChars('ABCDEFGH');42;", "ABCDEFGH");
      (putchars_header ^ "PutChars(0x008000FF);42;", "\255\128");
      (putchars_header ^ "PutChars(0x0100000000000000);42;", "\001");
      (print_header ^ "Print(\"%c\",0);42;", "");
      (putchars_header ^ "PutChars(0);42;", "");
      ( print_header ^ Printf.sprintf "Print(\"%c%c\");42;" '\128' '\255',
        "\128\255" );
    ]

let function_contexts_and_arguments () =
  cases
    [
      ( print_header
        ^ "I64 F(){U8 a[2];a[0]=42;a[1]=0;Print(\"%s\",(a));return 42;}F();",
        "*" );
      ( print_header
        ^ "I64 F(){U8 *fmt=\"%q\";fmt[1]=100;Print(fmt,42);return 42;}F();",
        "42" );
      (print_header ^ "I64 F(){U8 *fmt=\"%d\";\"\" fmt,42;return 42;}F();", "42");
      (putchars_header ^ "I64 F(){U64 ch='42\\n';'' ch;return 42;}F();", "42\n");
      (print_header ^ "I64 F(){I64 n=42;Print(\"x\",&n);return n;}F();", "x");
      (print_header ^ "I64 F(){U64 n=42;Print(\"x\",&n);return 42;}F();", "x");
      (print_header ^ "I64 F(){\"42\\n\";return 42;}F();", "42\n");
      (putchars_header ^ "I64 F(){'42\\n';return 42;}F();", "42\n");
      ( print_header
        ^ "I64 F(){I64 i=0;while(i<3){Print(\"%d\",i);i++;}return 42;}F();",
        "012" );
      ( print_header ^ "I64 F(){U8 *fmt=\"%d\";Print(fmt,42);return 42;}F();",
        "42" );
      ( print_header
        ^ "I64 F(){U8i *fmt=\"%s\";U8 *s=\"42\\n\";Print(fmt,s);return 42;}F();",
        "42\n" );
      ( print_header
        ^ "I64 G=0;I64 Next(){G++;return G;}Print(\"%d%d\",Next(),Next());G+40;",
        "21" );
      ( print_header
        ^ "I64 G=0;I64 Next(){G++;return G;}\"%d%d\",Next(),Next();G+40;",
        "21" );
      (print_header ^ "I64 G=0;Print(\"42\\n\",G=1,G=2);G+41;", "42\n");
      ( print_header
        ^ "I64 Change(U8 *p){p[0]=42;return 5;}I64 F(){U8 \
           a[2];a[1]=0;Print(\"%s%d\",a,Change(a));return a[0];}F();",
        "*5" );
      ( print_header
        ^ "I64 F(){U8 *s=\"*\",*p=&s[1];Print(\"%s\",p);return s[0];}F();",
        "" );
      (print_header ^ "U0 R(I64 n){if(n)R(n-1);Print(\"%d\",n);}R(2);42;", "012");
    ]

let statement_origins_and_source_definitions () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, output, value) ->
          ignore (run ~mode source |> expect ~value output))
        [
          (print_header ^ "42;\"x\";", "x", Some 42L);
          (print_header ^ "42;Print(\"x\");", "x", None);
          (putchars_header ^ "42;'x';", "x", Some 42L);
          (putchars_header ^ "42;PutChars('x');", "x", None);
          ("I64 Print(U8 *fmt){return 7;}42;\"x\";", "", Some 42L);
          ("I64 Print(U8 *fmt){return 7;}42;Print(\"x\");", "", Some 7L);
          ( "I64 G=0;I64 Print(U8 *fmt){G=42;return 7;}42;\"x\";if(G!=42)1/0;",
            "",
            Some 42L );
          ("I64 G=0;U0 PutChars(U64 ch){G=42;}PutChars('x');G;", "", Some 42L);
          ("I64 PutChars(U64 ch){return 7;}42;'x';", "", Some 42L);
        ])
    G.modes

let initializer_output_and_fresh_reports () =
  cases
    [
      ( print_header
        ^ "I64 Seed(I64 x){Print(\"%d\",x);return x;}I64 Join(I64 x){return \
           x+1;}I64 G=Join(Seed(41));G;",
        "41" );
      ( print_header
        ^ "I64 Seed(I64 x){Print(\"%d\",x);return x;}I64 Join(I64 x){return \
           x+1;}I64 F(){static I64 n=Join(Seed(41));return n;}F();",
        "41" );
      (print_header ^ "I64 Seed(){Print(\"A\");return 42;}I64 G=Seed();G;", "A");
      ( print_header
        ^ "I64 Seed(){Print(\"A\");return 42;}I64 F(){static I64 \
           n=Seed();return n;}F();",
        "A" );
    ];
  List.iter
    (fun mode ->
      ignore
        (run ~mode
           (print_header ^ "I64 Seed(){Print(\"A\");return 42;}7;I64 G=Seed();")
        |> expect ~value:(Some 7L) "A");
      let text =
        print_header
        ^ "I64 F(){U8 *s=\"(\";s[0]=s[0]+1;Print(\"%c\",s[0]+0);return \
           s[0];}F();F();"
      in
      let session, config, source = F.inputs ~mode text in
      List.iter
        (fun () ->
          ignore
            (run_integer_program_report ~max_steps:10000 session ~config ~source
            |> expect ")*"))
        [ (); () ];
      let text =
        print_header ^ "I64 Seed(){Print(\"%q\");return 42;}I64 G=Seed();42;"
      in
      ignore (G.compile ~mode text);
      let error = run ~mode text |> fault "HCIRVM0024" in
      List.iter
        (fun note ->
          Alcotest.(check bool) note true (List.mem note error.notes))
        [
          "function=Seed";
          "initializer=G";
          "initializer_phase=" ^ Test_integer_static_initializers.phase mode;
        ])
    G.modes

let faults_preserve_prior_capture () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, code) ->
          let error = run ~mode source |> fault ~output:"ok" code in
          Alcotest.(check bool)
            "reached output failure retains source" true
            (error.primary.stop > error.primary.start);
          Alcotest.(check bool)
            "reached output failure stage" true
            (List.mem "stage=execution" error.notes))
        [
          (print_header ^ "Print(\"ok\");1/0;", "HCIRVM0009");
          ( print_header
            ^ "I64 F(){I64 n=42;Print(\"ok\");Print(\"%s\",&n);return n;}F();",
            "HCIRVM0018" );
          ( print_header
            ^ "I64 F(){U64 n=42;Print(\"ok\");Print(\"%s\",&n);return 42;}F();",
            "HCIRVM0018" );
          (print_header ^ "Print(\"ok\");Print(\"prefix%q\");42;", "HCIRVM0024");
          ( print_header ^ "Print(\"ok\");Print(\"prefix%08d\",42);42;",
            "HCIRVM0024" );
          (print_header ^ "Print(\"ok\");Print(\"prefix%\");42;", "HCIRVM0024");
          (print_header ^ "Print(\"ok\");Print(\"prefix%d\");42;", "HCIRVM0025");
          ( print_header ^ "Print(\"ok\");Print(\"prefix%s\",42);42;",
            "HCIRVM0025" );
          ( print_header ^ "Print(\"ok\");Print(\"prefix%d\",\"x\");42;",
            "HCIRVM0025" );
          ( print_header ^ "Print(\"ok\");Print(\"prefix%c\",\"x\");42;",
            "HCIRVM0025" );
          ( print_header
            ^ "I64 F(){U8 a[1];a[0]=65;Print(\"ok\");Print(\"%s\",a);return \
               42;}F();",
            "HCIRVM0019" );
          ( print_header
            ^ "I64 F(){U8 *fmt=\"A\";fmt[1]=66;Print(\"ok\");Print(fmt);return \
               42;}F();",
            "HCIRVM0019" );
          ( print_header
            ^ "I64 F(){U8 a[1];Print(\"ok\");Print(\"%s\",a);return 42;}F();",
            "HCIRVM0012" );
        ])
    G.modes

let output_and_work_limits () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, output, work) ->
          let report = run ~mode ~max_output_work:work source in
          ignore (expect output report);
          Alcotest.(check int)
            "exact formatting work" work
            (integer_program_report_output_work report);
          if work > 1 then (
            let report = run ~mode ~max_output_work:(work - 1) source in
            ignore (fault "HCIRVM0023" report);
            Alcotest.(check int)
              "failed charge stays at limit" (work - 1)
              (integer_program_report_output_work report)))
        [
          (print_header ^ "Print(\"42\\n\");42;", "42\n", 7);
          (print_header ^ "Print(\"\");42;", "", 1);
          (print_header ^ "Print(\"%%\");42;", "%", 4);
          (print_header ^ "Print(\"%d\",42);42;", "42", 5);
          ( print_header ^ "Print(\"%d\",-9223372036854775807-1);42;",
            "-9223372036854775808",
            23 );
          (print_header ^ "Print(\"%s\",\"AB\");42;", "AB", 8);
          (print_header ^ "Print(\"%s\",\"\");42;", "", 4);
          (print_header ^ "Print(\"%c\",0);42;", "", 4);
          (print_header ^ "Print(\"%c\",0x00420041);42;", "A", 6);
        ];
      let source = print_header ^ "Print(\"42\\n\");42;" in
      ignore
        (run ~mode ~max_output_bytes:3 ~max_output_work:7 source
        |> expect "42\n");
      let report = run ~mode ~max_output_bytes:2 source in
      ignore (fault "HCIRVM0022" report);
      Alcotest.(check int)
        "capacity charges candidate append first" 6
        (integer_program_report_output_work report);
      let report = run ~mode ~max_output_bytes:2 ~max_output_work:5 source in
      ignore (fault "HCIRVM0023" report);
      Alcotest.(check int)
        "work exhausts before capacity check" 5
        (integer_program_report_output_work report);
      List.iter
        (fun (body, output, work, prefix) ->
          let source = putchars_header ^ body ^ "42;" in
          let report = run ~mode ~max_output_work:work source in
          ignore (expect output report);
          Alcotest.(check int)
            "packed byte work" work
            (integer_program_report_output_work report);
          let report = run ~mode ~max_output_work:(work - 1) source in
          ignore (fault ~output:prefix "HCIRVM0023" report);
          Alcotest.(check int)
            "streaming failure work" (work - 1)
            (integer_program_report_output_work report))
        [
          ("PutChars('42\\n');", "42\n", 6, "42");
          ("PutChars(0x00420041);", "AB", 5, "A");
          ("PutChars(0x0100000000000000);", "\001", 9, "");
        ];
      let report =
        run ~mode ~max_output_bytes:2 (putchars_header ^ "PutChars('42\\n');42;")
      in
      ignore (fault ~output:"42" "HCIRVM0022" report);
      Alcotest.(check int)
        "streaming capacity failure work" 6
        (integer_program_report_output_work report);
      let report =
        run ~mode ~max_output_work:1 (putchars_header ^ "PutChars(0);42;")
      in
      ignore (expect "" report);
      Alcotest.(check int)
        "zero packed word needs no inspection" 0
        (integer_program_report_output_work report);
      let report =
        run ~mode (print_header ^ "Print(\"A\");Print(\"B%q\");42;")
      in
      ignore (fault ~output:"A" "HCIRVM0024" report);
      Alcotest.(check int)
        "failed atomic drafts still charge cumulative work" 7
        (integer_program_report_output_work report))
    G.modes

let reached_formatting_and_failed_read_work () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let report = run ~mode ~max_output_work:1 source in
          ignore (expect "" report);
          Alcotest.(check int)
            "unreached formatting charges no work" 0
            (integer_program_report_output_work report))
        [
          print_header ^ "U0 Never(){Print(\"%q\");}42;";
          print_header ^ "if(0)Print(\"%d\");42;";
          print_header ^ "if(0)Print(\"%s\",42);42;";
        ];
      List.iter
        (fun (call, work) ->
          let source =
            print_header ^ "I64 F(){U8 a[1];a[0]=65;" ^ call ^ "return 42;}F();"
          in
          let report = run ~mode ~max_output_work:work source in
          let error = fault "HCIRVM0019" report in
          Alcotest.(check bool)
            "pointer fault names its selected provider" true
            (String.starts_with ~prefix:"Print" error.message);
          Alcotest.(check int)
            "failed bounds read is charged" work
            (integer_program_report_output_work report);
          let report = run ~mode ~max_output_work:(work - 1) source in
          ignore (fault "HCIRVM0023" report);
          Alcotest.(check int)
            "exhaustion prevents the failed read" (work - 1)
            (integer_program_report_output_work report))
        [ ("Print(\"%s\",a);", 5); ("Print(a);", 3) ];
      let source =
        print_header
        ^ "I64 F(){I64 n=0;while(n<3){Print(\"\");n++;}return 42;}F();"
      in
      let report = run ~mode ~max_output_work:3 source in
      ignore (expect "" report);
      Alcotest.(check int)
        "empty output still accumulates scanning work" 3
        (integer_program_report_output_work report);
      ignore (run ~mode ~max_output_work:2 source |> fault "HCIRVM0023");
      let report =
        run ~mode ~max_output_work:1 ~max_steps:40
          (putchars_header ^ "while(1)PutChars(0);")
      in
      ignore (fault "HCIRVM0007" report);
      Alcotest.(check int)
        "IR steps independently bound zero-work providers" 0
        (integer_program_report_output_work report);
      let source = print_header ^ "Print(\"A\");Print(\"BC\");42;" in
      ignore
        (run ~mode ~max_output_bytes:3 ~max_output_work:8 source |> expect "ABC");
      let report = run ~mode ~max_output_bytes:2 source in
      ignore (fault ~output:"A" "HCIRVM0022" report);
      Alcotest.(check int)
        "capacity includes committed bytes plus the current draft" 7
        (integer_program_report_output_work report);
      ignore
        (run ~mode ~max_output_work:7 source |> fault ~output:"A" "HCIRVM0023"))
    G.modes

let provider_resources_and_configuration () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, bytes, depth) ->
          ignore
            (run ~mode ~max_frame_bytes:bytes ~max_call_depth:depth source
            |> expect "42\n");
          let error =
            run ~mode ~max_frame_bytes:(bytes - 1) source |> fault "HCIRVM0011"
          in
          Alcotest.(check bool)
            "provider slots charged at reached call" true
            (List.mem "stage=execution" error.notes);
          if depth > 1 then
            ignore
              (run ~mode ~max_call_depth:(depth - 1) source
              |> fault "HCIRVM0015"))
        [
          (print_header ^ "Print(\"42\\n\");42;", 16, 1);
          (putchars_header ^ "PutChars('42\\n');42;", 8, 1);
          (print_header ^ "Print(\"%s\",\"42\\n\");42;", 24, 1);
          ( print_header ^ "I64 F(){I64 n=42;Print(\"%d\\n\",n);return n;}F();",
            32,
            2 );
          ( putchars_header ^ "I64 F(){I64 n=42;PutChars('42\\n');return n;}F();",
            16,
            2 );
        ];
      let source = print_header ^ "Print(\"42\\n\");42;" in
      ignore (run ~mode ~max_literal_bytes:4 source |> expect "42\n");
      let report = run ~mode ~max_literal_bytes:3 source in
      ignore (fault "HCIRVM0021" report);
      Alcotest.(check int)
        "preflight charges no output work" 0
        (integer_program_report_output_work report);
      List.iter
        (fun report ->
          ignore (fault "HCIRVM0001" report);
          Alcotest.(check int)
            "configuration precedes parsing and capture" 0
            (integer_program_report_output_work report))
        [
          run ~mode ~max_output_bytes:0 "this cannot parse (";
          run ~mode ~max_output_bytes:(-1) "this cannot parse (";
          run ~mode
            ~max_output_bytes:(Sys.max_string_length + 1)
            "this cannot parse (";
          run ~mode ~max_output_work:0 "this cannot parse (";
          run ~mode ~max_output_work:(-1) "this cannot parse (";
        ];
      ignore
        (run ~mode ~max_output_bytes:Sys.max_string_length
           ~max_output_work:max_int source
        |> expect "42\n"))
    G.modes

let exact_ir_step_boundaries () =
  List.iter
    (fun mode ->
      List.iter
        (fun (name, steps, work) ->
          let source = List.assoc name gates in
          let report = run ~mode ~max_steps:steps source in
          let result = expect "42\n" report in
          Alcotest.(check int)
            "exact provider IR steps" steps (VM.executed_steps result);
          Alcotest.(check int)
            "provider does not fabricate preparation steps" 0
            (VM.compiled_initializer_steps result);
          Alcotest.(check int)
            "formatting work stays separate from IR steps" work
            (integer_program_report_output_work report);
          let report = run ~mode ~max_steps:(steps - 1) source in
          let error = fault ~output:"42\n" "HCIRVM0007" report in
          Alcotest.(check bool)
            "one below consumes the exact IR allowance" true
            (List.mem
               ("executed_steps=" ^ string_of_int (steps - 1))
               error.notes);
          Alcotest.(check int)
            "later step exhaustion retains completed output work" work
            (integer_program_report_output_work report))
        [
          ("implicit-Print", 10, 7);
          ("explicit-Print", 10, 7);
          ("explicit-PutChars", 9, 6);
          ("implicit-PutChars", 9, 6);
        ])
    G.modes

let preparation_and_storage_limits () =
  List.iter
    (fun mode ->
      let source =
        print_header ^ "I64 F(){static I64 n=40;Print(\"A\");return n+2;}F();"
      in
      let report =
        run ~mode ~max_initializer_steps:4 ~max_global_bytes:8
          ~max_literal_bytes:2 ~max_frame_bytes:16 ~max_call_depth:2
          ~max_output_bytes:1 ~max_output_work:3 source
      in
      let result = expect "A" report in
      Alcotest.(check int)
        "constant preparation has its own exact allowance" 4
        (VM.compiled_initializer_steps result);
      Alcotest.(check int)
        "preparation does not execute the output provider" 3
        (integer_program_report_output_work report);
      let report = run ~mode ~max_initializer_steps:3 source in
      let error = fault "HCIRVM0007" report in
      Alcotest.(check int)
        "preparation failure precedes output work" 0
        (integer_program_report_output_work report);
      List.iter
        (fun note ->
          Alcotest.(check bool) note true (List.mem note error.notes))
        [
          "initializer=n";
          "initializer_phase=constant-preparation";
          "compiled_initializer_steps=3";
        ];
      List.iter
        (fun (report, code) ->
          let error = fault code report in
          List.iter
            (fun note ->
              Alcotest.(check bool) note true (List.mem note error.notes))
            [ "stage=preflight"; "executed_steps=0" ];
          Alcotest.(check int)
            "storage preflight precedes output work" 0
            (integer_program_report_output_work report))
        [
          (run ~mode ~max_global_bytes:7 source, "HCIRVM0016");
          (run ~mode ~max_literal_bytes:1 source, "HCIRVM0021");
        ])
    G.modes

let declaration_snapshots_and_provider_boundaries () =
  cases
    [
      ( "extern U0 Print(U8 *pattern,...);\"A\";extern U0 Print(U8 \
         *text,...);\"B\";42;",
        "AB" );
      (print_header ^ "\"A\";extern U0 Print(I64 fmt,...);42;", "A");
      ( "extern I64 Unknown(I64 n);" ^ print_header ^ "Print(\"42\\n\");42;",
        "42\n" );
      ("extern U0 PutChars(U64 packed);PutChars('42\\n');42;", "42\n");
    ];
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let report = run ~mode source in
          ignore (F.first_error (integer_program_report_outcome report));
          Alcotest.(check string)
            "unsupported provider has no capture" ""
            (integer_program_report_output_bytes report);
          Alcotest.(check int)
            "unsupported provider has no work" 0
            (integer_program_report_output_work report))
        [
          "extern I64 Print(U8 *fmt,...);\"A\";extern U0 Print(U8 *fmt,...);42;";
          "extern U0 Print(U8 *fmt);Print(\"A\");42;";
          "extern U0 Print(U8 *fmt,I64 n,...);Print(\"A\",1);42;";
          "extern I64 PutChars(U64 ch);PutChars('A');42;";
          "extern U0 PutChars(I64 ch);PutChars('A');42;";
          "extern U0 PutChars(U64 ch,...);PutChars('A');42;";
          "extern U0 Unknown(U64 ch);Unknown(42);42;";
        ])
    G.modes

let checked_call_context_construction () =
  let module D = Test_ir_direct_call_lowering in
  let module B = Test_top_level_implicit_output_argument_binding in
  let module Typed = Semantic_function_call_expression_result in
  let module Lower = Ir_integer_program_lowering in
  List.iter
    (fun mode ->
      List.iter
        (fun implicit ->
          let source =
            print_header
            ^ if implicit then "\"%d\",42;42;" else "Print(\"%d\",42);42;"
          in
          let prepared = D.prepared mode source in
          let function_sources, records = D.analyze prepared in
          let inputs = B.analyze prepared in
          let top_level = inputs.expressions in
          let top_calls =
            Typed.top_level_direct_calls top_level
            |> List.map (D.top_level_target records)
          in
          let statements =
            if implicit then
              let outputs =
                B.bind inputs |> B.checked_binding
                |> Semantic_top_level_implicit_output_argument_binding.outputs
              in
              List.map
                (fun output -> Lower.Top_level_output (B.bound output))
                outputs
              @ [
                  Lower.Expression
                    (List.hd
                       (List.rev
                          (Test_top_level_expression_result.root_values
                             top_level)));
                ]
            else
              List.map
                (fun value -> Lower.Expression value)
                (Test_top_level_expression_result.root_values top_level)
          in
          let resolution =
            resolve_global_records prepared.session
              ~declarations:prepared.declarations ~globals:prepared.global_types
              ~compilation_mode:mode prepared.ast
            |> B.checked
          in
          let global_records =
            classify_global_records prepared.session ~resolution prepared.ast
            |> B.checked
          in
          let globals =
            Ir_integer_globals.create ~span:prepared.ast.span global_records
            |> F.checked
          in
          let lowered =
            Lower.lower_complete ~globals ~records ~top_calls
              ~span:prepared.ast.span statements
            |> F.checked
          in
          let entry = Lower.graph lowered
          and descriptions = Lower.runtime_calls lowered in
          let seal ?(records = records) ?(top_level = top_level) entry
              entry_calls =
            let initialization =
              Ir_global_initialization.create ~span:prepared.ast.span ~globals
                ~entry []
              |> F.checked
            in
            R.create ~records ~function_sources ~top_level ~initialization
              ~entry ~entry_calls ~functions:[]
          in
          let context = seal entry descriptions |> F.checked in
          let description = List.hd descriptions in
          let call =
            R.find_start context ~owner:R.Entry description.first |> Option.get
          in
          Alcotest.(check bool)
            "checked provider" true
            (R.provider call = Some R.Print);
          Alcotest.(check int64)
            "all three ABI slots retained" 24L (R.cleanup_bytes call);
          Alcotest.(check (option int64))
            "one supplied variadic value" (Some 1L) (R.variadic_count call);
          Alcotest.(check bool)
            "mode-specific selected opcode" true
            (R.call_opcode call
            =
            if mode = Preprocessor.Jit then O.Ic_call_indirect2
            else O.Ic_call_extern);
          let reject result =
            Alcotest.(check string)
              "malformed call context rejected at construction" "HCIRVM0014"
              (F.first_error result).code
          in
          reject (seal entry []);
          reject (seal entry (descriptions @ descriptions));
          reject
            (seal entry
               [ { description with last = R.cleanup_instruction call } ]);
          reject
            (seal entry
               [
                 {
                   description with
                   discard =
                     (if implicit then None
                      else
                        Some
                          (List.find
                             (fun (d : Seq.description) ->
                               d.opcode = O.Ic_end_exp)
                             (SI.code entry))
                            .instruction_id);
                 };
               ]);
          let foreign = D.prepared mode source in
          let _, foreign_records = D.analyze foreign in
          let foreign_inputs = B.analyze foreign in
          reject (seal ~records:foreign_records entry descriptions);
          reject (seal ~top_level:foreign_inputs.expressions entry descriptions);
          let mutate id transform =
            let found = ref false in
            let entry =
              F.rewrite_entry
                (fun (d : Seq.description) ->
                  if Seq.Instruction_id.equal id d.instruction_id then (
                    found := true;
                    transform d)
                  else d)
                entry
            in
            Alcotest.(check bool)
              "mutation reaches its checked instruction" true !found;
            reject (seal entry descriptions)
          in
          List.iter
            (mutate (R.call_instruction call))
            [
              (fun d -> { d with Seq.opcode = O.Ic_call });
              (fun d -> { d with Seq.target_type = Some H.public_i64 });
              (fun d -> { d with Seq.payload = Some (Seq.Integer 0L) });
              (fun d -> { d with Seq.flags = 0x2000L });
            ];
          List.iter
            (mutate (R.cleanup_instruction call))
            [
              (fun d -> { d with Seq.payload = Some (Seq.Integer 16L) });
              (fun d ->
                {
                  d with
                  Seq.opcode =
                    (if d.opcode = O.Ic_add_rsp then O.Ic_add_rsp1
                     else O.Ic_add_rsp);
                });
              (fun d -> { d with Seq.target_type = Some H.public_i64 });
            ];
          List.iter
            (mutate (R.last call))
            [
              (fun d -> { d with Seq.target_type = Some H.public_i64 });
              (fun d -> { d with Seq.payload = None });
              (fun d -> { d with Seq.flags = 1L });
            ];
          let count =
            List.find
              (fun arg -> R.argument_role arg = R.Variadic_count)
              (R.arguments call)
          in
          List.iter
            (mutate (R.argument_producer count))
            [
              (fun d -> { d with Seq.payload = Some (Seq.Integer 0L) });
              (fun d -> { d with Seq.payload = Some (Seq.Integer 2L) });
              (fun d -> { d with Seq.target_type = Some H.u64 });
              (fun d -> { d with Seq.flags = 0L });
            ];
          let fixed =
            List.find
              (fun arg -> R.argument_role arg = R.Fixed 0)
              (R.arguments call)
          in
          mutate (R.argument_producer fixed) (fun d ->
              { d with Seq.span = None });
          mutate (R.argument_producer fixed) (fun d ->
              { d with Seq.flags = 0L });
          Option.iter
            (fun discard ->
              mutate discard (fun d -> { d with Seq.flags = 0L });
              mutate discard (fun d ->
                  { d with Seq.operands = [ R.argument_value count ] }))
            description.discard;
          let items = SI.code entry in
          let malformed =
            List.map
              (fun (d : Seq.description) ->
                if Seq.Instruction_id.equal d.instruction_id (R.last call) then
                  { d with result = None }
                else d)
              items
          in
          match Seq.create malformed with
          | Ok _ ->
              Alcotest.fail
                "call end without a result passed sequence construction"
          | Error errors ->
              Alcotest.(check bool)
                "sequence shape checks remain authoritative" true
                (List.exists
                   (fun (e : Seq.error) -> e.code = "HCIR0005")
                   errors))
        [ false; true ])
    G.modes

let initializer_calls_require_their_exact_expression () =
  let module I = Test_integer_global_initializers in
  let module T = Test_top_level_expression_result in
  let module D = Test_ir_direct_call_lowering in
  let module Typed = Semantic_function_call_expression_result in
  let module Lower = Ir_integer_program_lowering in
  let module Initial = Ir_global_initialization in
  List.iter
    (fun mode ->
      let prepared, _, initializers, _, top_level =
        I.typed_inputs mode
          "I64 Seed(I64 n){return n;}I64 G=Seed(1);Seed(2);42;"
      in
      let function_sources, records = D.analyze prepared in
      let global_records =
        classify_global_records prepared.session
          ~resolution:
            (Semantic_global_initializer_binding.source_globals initializers)
          prepared.ast
        |> T.checked
      in
      let globals =
        Ir_integer_globals.create ~initializers:top_level
          ~span:prepared.ast.span global_records
        |> F.checked
      in
      let top_calls =
        Typed.top_level_direct_calls top_level
        |> List.map (D.top_level_target records)
      in
      let statements =
        T.roots top_level
        |> List.map (fun root ->
            match
              root |> Typed.top_level_root_source
              |> Semantic_top_level_expression_tree.root_role
            with
            | Semantic_top_level_expression_tree.Global_initializer _ ->
                Lower.Initialize_global root
            | _ -> Lower.Expression (Typed.top_level_root_value root))
      in
      let lowered =
        Lower.lower_complete ~globals ~records ~top_calls
          ~span:prepared.ast.span statements
        |> F.checked
      in
      let entry = Lower.graph lowered in
      let regions = Lower.initializer_regions lowered in
      let descriptions = Lower.runtime_calls lowered in
      let seal entry entry_calls =
        let initialization =
          Initial.create ~span:prepared.ast.span ~globals ~entry regions
          |> F.checked
        in
        R.create ~records ~function_sources ~top_level ~initialization ~entry
          ~entry_calls ~functions:[]
      in
      ignore (seal entry descriptions |> F.checked);
      let region = List.hd regions in
      let inside (description : R.description) =
        Seq.Instruction_id.compare description.first region.first >= 0
        && Seq.Instruction_id.compare description.last region.last <= 0
      in
      let original = List.find inside descriptions in
      let borrowed =
        List.find (fun description -> not (inside description)) descriptions
      in
      let call_items (description : R.description) =
        SI.code entry
        |> List.filter (fun (item : Seq.description) ->
            Seq.Instruction_id.compare item.instruction_id description.first
            >= 0
            && Seq.Instruction_id.compare item.instruction_id description.last
               <= 0)
      in
      let original_items = call_items original
      and borrowed_items = call_items borrowed in
      Alcotest.(check int)
        "same canonical call shape"
        (List.length original_items)
        (List.length borrowed_items);
      let value_map =
        List.filter_map
          (fun ((original : Seq.description), (borrowed : Seq.description)) ->
            match (original.result, borrowed.result) with
            | Some original, Some borrowed ->
                Some (borrowed.value_id, original.value_id)
            | None, None -> None
            | _ -> Alcotest.fail "replacement changed the call producer shape")
          (List.combine original_items borrowed_items)
      in
      let value id = List.assoc id value_map in
      let replacements =
        List.map2
          (fun (original : Seq.description) (borrowed : Seq.description) ->
            {
              borrowed with
              instruction_id = original.instruction_id;
              operands = List.map value borrowed.operands;
              result =
                Option.map
                  (fun (result : Seq.value_definition) ->
                    { Seq.value_id = value result.value_id })
                  borrowed.result;
            })
          original_items borrowed_items
      in
      let entry =
        F.rewrite_entry
          (fun (item : Seq.description) ->
            Option.value
              (List.find_opt
                 (fun (replacement : Seq.description) ->
                   Seq.Instruction_id.equal replacement.instruction_id
                     item.instruction_id)
                 replacements)
              ~default:item)
          entry
      in
      let descriptions =
        List.map
          (fun (description : R.description) ->
            if Seq.Instruction_id.equal description.first original.first then
              { description with source = borrowed.source }
            else description)
          descriptions
      in
      let error = F.first_error (seal entry descriptions) in
      Alcotest.(check string)
        "a same-batch call cannot replace an initializer subtree" "HCIRVM0014"
        error.code)
    G.modes

let reports_require_owned_contexts () =
  List.iter
    (fun mode ->
      let source = print_header ^ "U0 F(){Print(\"A\");}F();Print(\"B\");42;" in
      let compiled = G.compile ~mode source in
      let entry = integer_program_entry compiled in
      let functions = integer_program_functions compiled in
      let globals = integer_program_globals compiled in
      let initialization = integer_program_initialization compiled in
      let runtime_calls = integer_program_runtime_calls compiled in
      let execute ?runtime_calls ?(functions = functions)
          ?(initialization = Some initialization) entry =
        VM.execute_program_report ?runtime_calls ~globals ?initialization
          ~functions ~max_steps:10000 ~max_frame_bytes:1024 ~max_call_depth:16
          entry
      in
      let first = execute ~runtime_calls entry in
      ignore (VM.report_outcome first |> H.require_ok H.show_vm_errors);
      Alcotest.(check string)
        "raw report executes both owners" "AB"
        (VM.report_output_bytes first);
      let second = execute ~runtime_calls entry in
      ignore (VM.report_outcome second |> H.require_ok H.show_vm_errors);
      Alcotest.(check string)
        "new execution starts fresh capture" "AB"
        (VM.report_output_bytes second);
      let reject report =
        S.require_preflight (VM.report_outcome report);
        Alcotest.(check string)
          "preflight produces no capture" ""
          (VM.report_output_bytes report);
        Alcotest.(check int)
          "preflight charges no output work" 0
          (VM.report_output_work report)
      in
      reject (execute entry);
      reject (execute ~runtime_calls ~initialization:None entry);
      reject (execute ~runtime_calls (F.rewrite_entry Fun.id entry));
      let foreign = G.compile ~mode source in
      reject
        (execute ~runtime_calls:(integer_program_runtime_calls foreign) entry);
      reject
        (execute ~runtime_calls
           ~functions:(integer_program_functions foreign)
           entry);
      let functions =
        List.map
          (fun (fn : VM.function_definition) ->
            { fn with body = S.rebuild Fun.id fn.body })
          functions
      in
      reject (execute ~runtime_calls ~functions entry);
      Alcotest.(check string)
        "later reports cannot mutate prior capture" "AB"
        (VM.report_output_bytes first))
    G.modes

let tests =
  List.map
    (fun (name, source) ->
      Alcotest.test_case name `Quick (fun () ->
          List.iter
            (fun mode ->
              ignore (G.run ~mode source |> F.expect 42L);
              ignore (run ~mode source |> expect "42\n"))
            G.modes))
    gates
  @ [
      Alcotest.test_case "format grammar signed words and binary bytes" `Quick
        formats_and_binary_bytes;
      Alcotest.test_case "function output loops and right-to-left arguments"
        `Quick function_contexts_and_arguments;
      Alcotest.test_case "statement origins and source-defined output functions"
        `Quick statement_origins_and_source_definitions;
      Alcotest.test_case "initializer output and fresh report images" `Quick
        initializer_output_and_fresh_reports;
      Alcotest.test_case "earlier capture survives faults and Print is atomic"
        `Quick faults_preserve_prior_capture;
      Alcotest.test_case "exact output and cumulative work limits" `Quick
        output_and_work_limits;
      Alcotest.test_case
        "only reached formats charge work including failed reads" `Quick
        reached_formatting_and_failed_read_work;
      Alcotest.test_case "provider ABI resources and configuration limits"
        `Quick provider_resources_and_configuration;
      Alcotest.test_case "exact IR step boundaries retain completed capture"
        `Quick exact_ir_step_boundaries;
      Alcotest.test_case
        "output keeps preparation and storage budgets independent" `Quick
        preparation_and_storage_limits;
      Alcotest.test_case "declaration snapshots and provider boundaries" `Quick
        declaration_snapshots_and_provider_boundaries;
      Alcotest.test_case
        "checked call context construction rejects forged joins" `Quick
        checked_call_context_construction;
      Alcotest.test_case "reports require exact graph and function owners"
        `Quick reports_require_owned_contexts;
      Alcotest.test_case "initializer calls retain their exact semantic subtree"
        `Quick initializer_calls_require_their_exact_expression;
    ]
