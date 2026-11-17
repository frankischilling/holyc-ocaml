open Holyc_lib
open Yojson.Safe.Util
module Native = Native_source_execution
module Image = X86_64_program

let checked = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let diagnostics errors =
  errors
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let run ?(mode = Preprocessor.Jit) ?max_steps ?max_initializer_steps
    ?max_default_bytes text =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-f64-defaults.hc" ~contents:text
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> checked
  in
  Native.evaluate ?max_initializer_steps ?max_default_bytes session ~config
    ~source
    ~max_steps:(Option.value max_steps ~default:100_000)

let value ~type_ bits report =
  let result =
    Native.outcome report |> Result.map_error diagnostics |> checked
  in
  let observed = Option.get result.value.final_value in
  Alcotest.(check bool)
    "source result retains original public value class" true
    (observed.type_ = type_);
  Alcotest.(check int64) "source result exact raw word" bits observed.bits;
  Alcotest.(check string)
    "no unexpected ordinary output" ""
    (Native.output_bytes report);
  let progress = Option.get (Native.source_progress report) in
  Alcotest.(check int)
    "native source task does not substitute interpreted execution" 0
    progress.runtime.executed_steps;
  Alcotest.(check bool)
    "native source executed physical instructions" true
    (Native.executed_steps report > 0);
  observed

let reject report =
  match Native.outcome report with
  | Error errors when errors <> [] -> errors
  | _ -> Alcotest.fail "unsupported native source unexpectedly completed"

let diagnostic code report =
  let errors = reject report in
  Alcotest.(check bool)
    (diagnostics errors) true
    (List.exists (fun (item : Diagnostic.t) -> item.code = code) errors)

let scalar_defaults () =
  List.iter
    (fun (text, bits) ->
      let report = run text in
      ignore (value ~type_:Image.F64 bits report);
      Alcotest.(check int)
        "one saved F64 word uses eight payload bytes" 8
        (Native.default_bytes report))
    [
      ("F64 F(F64 n=1.5){return n;}F();", 0x3ff8_0000_0000_0000L);
      ("F64 F(F64 n=-0.0){return n;}F();", Int64.min_int);
      ( "I64 Seed(){return 41;}F64 F(F64 n=Seed()){return n;}F();",
        0x4044_8000_0000_0000L );
      ( "I64 Seed(){return 9007199254740993;}F64 F(F64 n=Seed()){return n;}F();",
        0x4340_0000_0000_0000L );
      ( "I64 Seed(){return 9007199254740995;}F64 F(F64 n=Seed()){return n;}F();",
        0x4340_0000_0000_0002L );
      ( "I64 Seed(){return -9007199254740995;}F64 F(F64 n=Seed()){return n;}F();",
        0xc340_0000_0000_0002L );
      ( "U64 Seed(){return 0xffffffffffffffff;}F64 F(F64 n=Seed()){return \
         n;}F();",
        0xbff0_0000_0000_0000L );
      ( "U64 Seed(){return 0x8000000000000000;}F64 F(F64 n=Seed()){return \
         n;}F();",
        0xc3e0_0000_0000_0000L );
      ( "U64 Seed(){return 0x7fffffffffffffff;}F64 F(F64 n=Seed()){return \
         n;}F();",
        0x43e0_0000_0000_0000L );
      ( "F64 Bits(){F64 x;(&x)(U64*)[0]=0x7ff0000000000042;return x;}F64 F(F64 \
         n=Bits()){return n;}F();",
        0x7ff0_0000_0000_0042L );
    ];
  let supplied = run "F64 F(F64 n=1.5){return n;}F(2.25);" in
  ignore (value ~type_:Image.F64 0x4002_0000_0000_0000L supplied);
  Alcotest.(check int)
    "explicit argument does not skip original default preparation" 8
    (Native.default_bytes supplied);
  let unused = run "F64 F(F64 n=1.5){return n;}42;" in
  ignore (value ~type_:Image.I64 42L unused);
  Alcotest.(check int)
    "unused function default still prepares at declaration" 8
    (Native.default_bytes unused);
  ignore
    (value ~type_:Image.F64 0x3ff8_0000_0000_0000L
       (run "F64 F(F64 n=1.5){return n;}F;"))

let conversion_defaults () =
  List.iter
    (fun (text, bits) ->
      let report = run text in
      ignore (value ~type_:Image.I64 bits report);
      Alcotest.(check int)
        "converted numeric default retains full saved word" 8
        (Native.default_bytes report))
    [
      ("I64 F(I64 n=42.75){return n;}F();", 42L);
      ("I64 F(I64 n=-2.75){return n;}F();", -2L);
      ("I64 F(I8 n=257.75){return n;}F();", 1L);
      ("F64 Seed(){return 41.75;}I64 F(I64 n=Seed()){return n+1;}F();", 42L);
      ( "F64 Bits(){F64 x;(&x)(U64*)[0]=0x7ff0000000000042;return x;}I64 F(I64 \
         n=Bits()){return n;}F();",
        Int64.min_int );
      ( "F64 Bits(){F64 x;(&x)(U64*)[0]=0x7ff0000000000000;return x;}I64 F(I64 \
         n=Bits()){return n;}F();",
        Int64.min_int );
      ( "F64 Bits(){F64 x;(&x)(U64*)[0]=0xfff0000000000000;return x;}I64 F(I64 \
         n=Bits()){return n;}F();",
        Int64.min_int );
      ( "F64 Bits(){F64 x;(&x)(U64*)[0]=0x43e0000000000001;return x;}I64 F(I64 \
         n=Bits()){return n;}F();",
        Int64.min_int );
    ]

let original_effects_and_callback () =
  let source =
    "I64 Counter=40;I64 Seed(){return ++Counter;}F64 F(F64 n=Seed()){return \
     n;}Counter=100;F();F();"
  in
  let report = run source in
  ignore (value ~type_:Image.F64 0x4044_8000_0000_0000L report);
  Alcotest.(check int)
    "default executes once before both calls" 8
    (Native.default_bytes report);
  let callback =
    "F64 Target(F64 n=2.25){return n;}F64 Run(){F64 (*p)(F64 \
     n=1.5);p=&Target;return p();}Run();"
  in
  let report = run callback in
  ignore (value ~type_:Image.F64 0x3ff8_0000_0000_0000L report);
  Alcotest.(check int)
    "distinct target and anonymous headers each prepare a default" 16
    (Native.default_bytes report);
  let nontrailing =
    "F64 Pick(F64 a=1.5,F64 b,F64 c){return a+b;}F64 Run(){F64 n=0.0;return \
     Pick(,n,n=2.25);}Run();"
  in
  ignore (value ~type_:Image.F64 0x400e_0000_0000_0000L (run nontrailing));
  let selected_header =
    "F64 Target(F64 n=1.5){return n;}F64 Run(){F64 (*p)(F64 \
     n=2.25);p=&Target;return p();}Run();"
  in
  ignore (value ~type_:Image.F64 0x4002_0000_0000_0000L (run selected_header));
  let historical_header =
    "I64 Counter=40;I64 Seed(){return ++Counter;}F64 F(F64 n=Seed()){return \
     n;}F64 Old(){return F();}I64 Counter=1;F64 F(F64 n=2.25){return n;}Old();"
  in
  ignore (value ~type_:Image.F64 0x4044_8000_0000_0000L (run historical_header))

let failure_preserves_output () =
  let source =
    "extern U0 PutChars(U64 ch);F64 Good(){PutChars('A');return 1.5;}F64 A(F64 \
     n=Good()){return n;}F64 Bad(){PutChars('B');I64 z=0;I64 q=7/z;return \
     2.25;}F64 B(F64 n=Bad()){return n;}42;"
  in
  let report = run source in
  diagnostic "HCIRVM0009" report;
  Alcotest.(check string)
    "reached outputs from both original defaults survive later fault" "AB"
    (Native.output_bytes report);
  Alcotest.(check int)
    "the first floating default remains the only saved payload" 8
    (Native.default_bytes report);
  let last = List.hd (List.rev (Native.fragments report)) in
  Alcotest.(check bool)
    "failure reached its original native default image" true
    (last.kind = Native.Default)

let bounded_preparation () =
  let source =
    "F64 Seed(){return 1.5;}F64 A(F64 n=Seed()){return n;}F64 B(F64 \
     n=A()){return n;}B();"
  in
  let report = run source in
  ignore (value ~type_:Image.F64 0x3ff8_0000_0000_0000L report);
  let work = Native.preparation_steps report in
  let executed = Native.executed_steps report in
  Alcotest.(check bool) "both defaults consume preparation work" true (work > 1);
  Alcotest.(check int)
    "two saved scalar payloads" 16
    (Native.default_bytes report);
  ignore
    (value ~type_:Image.F64 0x3ff8_0000_0000_0000L
       (run ~max_initializer_steps:work ~max_steps:executed
          ~max_default_bytes:16 source));
  ignore (reject (run ~max_initializer_steps:(work - 1) source));
  let limited = run ~max_default_bytes:15 source in
  diagnostic "HCIRVM0011" limited;
  Alcotest.(check int)
    "first completed default survives second payload rejection" 8
    (Native.default_bytes limited);
  let after_gc () =
    Gc.full_major ();
    ignore (value ~type_:Image.F64 0x3ff8_0000_0000_0000L (run source))
  in
  after_gc ()

let distinct_aot_preparation () =
  let text = "F64 F(F64 n=1.5){return n;}F();" in
  ignore (reject (run ~mode:Preprocessor.Aot text))

let nested_jit_exe_defaults () =
  let source =
    {|#exe {F64 Nested(F64 value=1.5){return value;}Nested();StreamPrint("42;");}|}
  in
  List.iter
    (fun mode ->
      let exact = run ~mode ~max_default_bytes:8 source in
      ignore (value ~type_:Image.I64 42L exact);
      Alcotest.(check int)
        "nested JIT default saves one full scalar word" 8
        (Native.default_bytes exact);
      Alcotest.(check bool)
        "nested JIT entered a native default image" true
        (List.exists
           (fun (fragment : Native.fragment) -> fragment.kind = Native.Default)
           (Native.fragments exact));
      let bounded = run ~mode ~max_default_bytes:7 source in
      diagnostic "HCIRVM0011" bounded;
      Alcotest.(check int)
        "the source rejects the too-small bound before saving" 0
        (Native.default_bytes bounded))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_temp_file extension action =
  let path = Filename.temp_file "holyc-f64-defaults-" extension in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () -> action path)

let invoke compiler expected args =
  with_temp_file ".out" (fun stdout ->
      with_temp_file ".err" (fun stderr ->
          let out_fd =
            Unix.openfile stdout [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
          in
          let err_fd =
            Unix.openfile stderr [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
          in
          let pid =
            Fun.protect
              ~finally:(fun () ->
                Unix.close out_fd;
                Unix.close err_fd)
              (fun () ->
                Unix.create_process compiler
                  (Array.of_list (compiler :: args))
                  Unix.stdin out_fd err_fd)
          in
          let _, status = Unix.waitpid [] pid in
          let stdout = read_file stdout and stderr = read_file stderr in
          Alcotest.(check bool)
            ("CLI exit for " ^ String.concat " " args ^ ": " ^ stderr)
            true
            (status = Unix.WEXITED expected);
          Alcotest.(check string) "CLI leaves stderr empty" "" stderr;
          Yojson.Safe.from_string stdout))

let cli_example compiler example () =
  let run ?(status = 0) ?(options = []) ?(target = "host-jit-task")
      ?(mode = "jit") () =
    invoke compiler status
      ([ "run"; "--target=" ^ target; "--mode=" ^ mode; "--format=json" ]
      @ options @ [ example ])
  in
  let bits report =
    report |> member "final_value" |> member "bits" |> to_string
  in
  let baseline = run () in
  Alcotest.(check string)
    "source-task CLI schema" "holyc-integer-program-v2"
    (baseline |> member "schema" |> to_string);
  Alcotest.(check string)
    "original floating default result" "0x400e000000000000" (bits baseline);
  Alcotest.(check string)
    "CLI result type" "f64"
    (baseline |> member "final_value" |> member "type" |> to_string);
  Alcotest.(check int)
    "named and anonymous saved defaults" 16
    (baseline |> member "prepared_default_bytes" |> to_int);
  let preparation = baseline |> member "compiled_initializer_steps" |> to_int in
  Alcotest.(check bool)
    "actual declaration-time native work" true (preparation > 1);
  let exact =
    run
      ~options:
        [
          "--default-byte-limit=16";
          "--initializer-step-limit=" ^ string_of_int preparation;
        ]
      ()
  in
  Alcotest.(check string)
    "exact CLI preparation limits succeed" (bits baseline) (bits exact);
  let too_few_bytes = run ~status:1 ~options:[ "--default-byte-limit=15" ] () in
  Alcotest.(check string)
    "payload bound stops second default" "error"
    (too_few_bytes |> member "outcome" |> to_string);
  let too_few_steps =
    run ~status:1
      ~options:[ "--initializer-step-limit=" ^ string_of_int (preparation - 1) ]
      ()
  in
  Alcotest.(check string)
    "preparation bound stops original native work" "error"
    (too_few_steps |> member "outcome" |> to_string);
  let isolated = run ~status:1 ~target:"host-jit" () in
  Alcotest.(check string)
    "isolated target retains closed floating default boundary" "error"
    (isolated |> member "outcome" |> to_string);
  let aot = run ~status:1 ~mode:"aot" () in
  Alcotest.(check string)
    "outer AOT retains its floating default preparation boundary" "error"
    (aot |> member "outcome" |> to_string)

let () =
  if Array.length Sys.argv <> 3 then
    failwith
      "usage: test_native_f64_defaults.exe <holyc.exe> <native-f64-defaults.hc>"
  else if not (Native_execution.x87_supported ()) then
    print_endline
      "native F64 default tests: x87 unsupported on this host, skipped"
  else
    Alcotest.run
      ~argv:[| Sys.argv.(0) |]
      "Native F64 parameter defaults"
      [
        ( "source task",
          [
            Alcotest.test_case "F64 default bits and explicit arguments" `Quick
              scalar_defaults;
            Alcotest.test_case "original effects and anonymous F64 headers"
              `Quick original_effects_and_callback;
            Alcotest.test_case "F64 default failure retains reached output"
              `Quick failure_preserves_output;
            Alcotest.test_case "F64 preparation and saved byte limits" `Quick
              bounded_preparation;
            Alcotest.test_case "outer AOT keeps its preparation boundary" `Quick
              distinct_aot_preparation;
            Alcotest.test_case "JIT nested exe defaults in both outer modes"
              `Quick nested_jit_exe_defaults;
            Alcotest.test_case "F64 defaults converted to integer" `Quick
              conversion_defaults;
          ] );
        ( "public CLI",
          [
            Alcotest.test_case "source example, F64 bits and admission limits"
              `Quick
              (cli_example Sys.argv.(1) Sys.argv.(2));
          ] );
      ]
