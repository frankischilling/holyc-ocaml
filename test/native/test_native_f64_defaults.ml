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
  (match Native.source_progress report with
  | None -> ()
  | Some progress ->
      Alcotest.(check int)
        "native source task does not substitute interpreted execution" 0
        progress.runtime.executed_steps);
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
  List.iter
    (fun (text, type_, bits) ->
      let report = run ~mode:Preprocessor.Aot text in
      ignore (value ~type_ bits report);
      Alcotest.(check int)
        "ordinary AOT saves its original numeric word" 8
        (Native.default_bytes report);
      Alcotest.(check bool)
        "AOT default executes before its distinct module" true
        (match Native.fragments report with
        | [ { kind = Native.Default; _ }; { kind = Native.Aot_module; _ } ] ->
            true
        | _ -> false);
      let work = Native.preparation_steps report in
      let steps = Native.executed_steps report in
      ignore
        (value ~type_ bits
           (run ~mode:Preprocessor.Aot ~max_initializer_steps:work
              ~max_default_bytes:8 ~max_steps:steps text));
      diagnostic "HCIRVM0007"
        (run ~mode:Preprocessor.Aot ~max_initializer_steps:(work - 1) text);
      diagnostic "HCIRVM0011"
        (run ~mode:Preprocessor.Aot ~max_default_bytes:7 text))
    [
      ("F64 F(F64 n=1.5){return n;}F();", Image.F64, 0x3ff8_0000_0000_0000L);
      ("F64 F(F64 n=-0.0){return n;}F;", Image.F64, Int64.min_int);
      ( "F64 F(F64 n=9007199254740993){return n;}F();",
        Image.F64,
        0x4340_0000_0000_0000L );
      ( "F64 F(F64 n=0xffffffffffffffff){return n;}F();",
        Image.F64,
        0xbff0_0000_0000_0000L );
      ("I64 F(I64 n=42.75){return n;}F();", Image.I64, 42L);
      ("I64 F(I8 n=257.75){return n;}F();", Image.I64, 1L);
      (* PrsVar.HC:350-356,637-653 uses the callback member's RT_PTR word
         class for default conversion, independently of its F64 return type. *)
      ( "I64 F(F64 (*p)()=1.75){if(p==1)return 42;return -1;}F();",
        Image.I64,
        42L );
      ("I64 F(I64 n=1.5>1.0){return n;}F();", Image.I64, 1L);
      ("I64 F(I64 n=(1.5>1.0)){return n;}F();", Image.I64, 1L);
      ("I64 F(I64 n=(1.5>1.0)+41){return n;}F();", Image.I64, 42L);
      ( "F64 F(F64 n=sizeof(U16)){return n;}F();",
        Image.F64,
        0x4000_0000_0000_0000L );
      ("I64 class C {I64 word;};I64 F(C n=42.75){return n;}F();", Image.I64, 42L);
      ( "I8 class Tiny {I8 word;};I64 F(Tiny n=257.75){return n;}F();",
        Image.I64,
        1L );
      ("F64 Unused(F64 n=1.5){return n;}42;", Image.I64, 42L);
      ( "F64 Explicit(F64 n=1.5){return n;}Explicit(2.25);",
        Image.F64,
        0x4002_0000_0000_0000L );
      ("1;F64 F(F64 n=1.5){return n;}F();", Image.F64, 0x3ff8_0000_0000_0000L);
    ];
  let callback =
    run ~mode:Preprocessor.Aot
      "F64 Target(F64 n=2.25){return n;}F64 (*p)(F64 n=1.5)=&Target;p();"
  in
  ignore (value ~type_:Image.F64 0x3ff8_0000_0000_0000L callback);
  Alcotest.(check int)
    "AOT anonymous header retains its own saved default" 16
    (Native.default_bytes callback);
  let late =
    run ~mode:Preprocessor.Aot
      "F64 Earlier(F64 n=1.5){return n;}F64 Later(F64 n=2.25]"
  in
  ignore (reject late);
  Alcotest.(check bool)
    "later syntax failure retains earlier preparation" true
    (Native.preparation_steps late > 0 && Native.default_bytes late >= 8);
  let failed =
    run ~mode:Preprocessor.Aot
      "extern U0 PutChars(U64 c);PutChars('P');F64 Earlier(F64 n=1.5){return \
       n;}F64 Later(F64 n=1/0){return n;}42;"
  in
  diagnostic "HCIRVM0009" failed;
  Alcotest.(check string)
    "AOT runtime output waits for successful compilation" ""
    (Native.output_bytes failed);
  Alcotest.(check int)
    "failed native AOT default publishes no saved word" 8
    (Native.default_bytes failed);
  diagnostic "HCRUN0006"
    (run ~mode:Preprocessor.Aot
       "I64 Seed(){return 41;}F64 F(F64 n=Seed()){return n;}F();")

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

let isolated_numeric_defaults () =
  let run ?(mode = Preprocessor.Aot) ?(max_steps = 100_000)
      ?max_initializer_steps ?max_default_bytes ?max_ir_instructions
      ?max_code_bytes text =
    let session = Session.create () in
    let source =
      Session.add_source session ~path:"isolated-defaults.hc" ~contents:text
    in
    let config =
      Preprocessor.Config.create ~compilation_mode:mode () |> checked
    in
    Native_program.evaluate ?max_initializer_steps ?max_default_bytes
      ?max_ir_instructions ?max_code_bytes session ~config ~source ~max_steps
  in
  let value bits report =
    let result =
      Native_program.outcome report |> Result.map_error diagnostics |> checked
    in
    let value = Option.get result.value.execution.final_value in
    Alcotest.(check bool)
      "isolated default result keeps F64 type" true (value.type_ = Image.F64);
    Alcotest.(check int64)
      "isolated default result keeps exact bits" bits value.bits;
    result.value.image
  in
  let fault code report =
    match Native_program.outcome report with
    | Error errors ->
        Alcotest.(check bool)
          (diagnostics errors) true
          (List.exists (fun (error : Diagnostic.t) -> error.code = code) errors)
    | Ok _ -> Alcotest.fail "isolated default unexpectedly completed"
  in
  let text = "F64 F(F64 n=1.5){return n;}F();" in
  List.iter
    (fun mode ->
      let baseline = run ~mode text in
      let image = value 0x3ff8_0000_0000_0000L baseline in
      let work = Native_program.preparation_steps baseline in
      let steps = Option.get (Native_program.executed_steps baseline) in
      Alcotest.(check int)
        "isolated default saves one full word" 8
        (Native_program.default_bytes baseline);
      Alcotest.(check bool)
        "physical work includes default and final entry" true
        (steps > work && work > 0);
      ignore
        (value 0x3ff8_0000_0000_0000L
           (run ~mode ~max_steps:steps ~max_initializer_steps:work
              ~max_default_bytes:8 text));
      fault "HCIRVM0007" (run ~mode ~max_steps:(steps - 1) text);
      fault "HCIRVM0007" (run ~mode ~max_initializer_steps:(work - 1) text);
      fault "HCIRVM0011" (run ~mode ~max_default_bytes:7 text);
      fault "HCBACK0001"
        (run ~mode ~max_ir_instructions:(Image.ir_instructions image) text);
      fault "HCBACK0005"
        (run ~mode ~max_code_bytes:(Image.code_bytes image) text))
    [ Preprocessor.Jit; Preprocessor.Aot ];
  let late = run "F64 F(F64 n=1.5){return n;}F64 Broken(F64 n=2.25]" in
  (match Native_program.outcome late with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "later syntax failure unexpectedly succeeded");
  Alcotest.(check bool)
    "reached native preparation survives later syntax error" true
    (Native_program.preparation_steps late > 0
    && Option.value ~default:0 (Native_program.executed_steps late) > 0
    && Native_program.default_bytes late >= 8);
  let failed =
    run "F64 Good(F64 n=1.5){return n;}F64 Bad(F64 n=1/0){return n;}42;"
  in
  fault "HCIRVM0009" failed;
  Alcotest.(check int)
    "failed isolated preparation publishes no second word" 8
    (Native_program.default_bytes failed);
  if not (Native_execution.fisttp_supported ()) then (
    let rejected_image = run "I64 F(F64 n=1.5){return n;}F();" in
    fault "HCNATIVE0002" rejected_image;
    Alcotest.(check bool)
      "later host capability failure preserves verified default work" true
      (Native_program.preparation_steps rejected_image > 0
      && Native_program.executed_steps rejected_image
         = Some (Native_program.preparation_steps rejected_image));
    Alcotest.(check int)
      "later host rejection preserves saved default" 8
      (Native_program.default_bytes rejected_image))

let closed_capture_authority () =
  let module Preparation = Holyc_lib__Driver.Native_default_preparation in
  let module Runtime = Native_program_execution in
  let module Capture = Holyc_lib__Ir.Native_scalar_capture in
  let rejected label result =
    Alcotest.(check bool) label true (Result.is_error result)
  in
  List.iter
    (fun scenario ->
      let session = Session.create () in
      let source =
        Session.add_source session ~path:"closed-capture.hc"
          ~contents:"F64 F(F64 n=1.5){return n;}F();"
      in
      let config =
        Preprocessor.Config.create ~compilation_mode:Aot () |> checked
      in
      let preparation_steps = ref 0
      and switch_work = ref 0
      and dimension_work = ref 0
      and default_bytes = ref 0 in
      let saved_request = ref None in
      let native_default request =
        saved_request := Some request;
        let program = Preparation.native_program request in
        let image =
          Image.compile_source_default ~max_ir_instructions:4096
            ~max_code_bytes:65_536 request
          |> Result.map_error (fun errors ->
              String.concat "; "
                (List.map (fun (e : Image.error) -> e.message) errors))
          |> checked
        in
        let retained = Runtime.retain image |> checked in
        Fun.protect
          ~finally:(fun () -> Runtime.release retained |> checked)
          (fun () ->
            let budget =
              Runtime.create_budget ~max_steps:100_000 () |> checked
            in
            let report =
              Runtime.execute_retained_budget_report budget retained
            in
            ignore (Runtime.outcome report |> checked);
            let work = (Runtime.budget_progress budget).executed_steps in
            let capture = Option.get (Runtime.source_default_capture report) in
            rejected "closed native default cannot enter its request twice"
              (Preparation.claim_native request);
            let before = work in
            rejected "closed default image cannot execute again"
              (Runtime.outcome
                 (Runtime.execute_retained_budget_report budget retained));
            Alcotest.(check int)
              "rejected replay consumes no native work" before
              (Runtime.budget_progress budget).executed_steps;
            let copied_program = Obj.obj (Obj.dup (Obj.repr program)) in
            rejected "copied program cannot consume a private native capture"
              (Capture.consume capture ~program:copied_program ~work);
            rejected "wrong work cannot consume a private native capture"
              (Capture.consume capture ~program ~work:(work + 1));
            rejected "foreign domain cannot publish a private native capture"
              (Domain.join
                 (Domain.spawn (fun () ->
                      Preparation.finish_native request ~steps:work
                        (Some capture))));
            Gc.full_major ();
            Gc.compact ();
            if scenario = `Released then Runtime.release retained |> checked;
            let outcome =
              Preparation.finish_native request ~steps:work
                (if scenario = `Missing then None else Some capture)
            in
            (match scenario with
            | `Released ->
                rejected "unmapped private image cannot publish" outcome
            | `Missing | `Valid -> outcome |> checked);
            rejected "closed source completion cannot replay"
              (Preparation.finish_native request ~steps:work (Some capture));
            Ok ())
      in
      let compiled =
        Native_program.compile_with_preparation ~native_default
          ~preparation_steps ~switch_work ~dimension_work ~default_bytes session
          ~config ~source
      in
      rejected "native source request expires with its parser callback"
        (Preparation.check_native (Option.get !saved_request));
      Alcotest.(check bool)
        "entered native work remains charged" true (!preparation_steps > 0);
      match scenario with
      | `Valid ->
          ignore (compiled |> Result.map_error diagnostics |> checked);
          Alcotest.(check int)
            "exact live private capture saves one word" 8 !default_bytes
      | `Missing | `Released ->
          rejected "unproved private capture cannot complete compilation"
            compiled;
          Alcotest.(check int)
            "unproved private capture saves no word" 0 !default_bytes)
    [ `Valid; `Missing; `Released ]

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
    "isolated target rejects a default needing live prior declarations" "error"
    (isolated |> member "outcome" |> to_string);
  let aot = run ~status:1 ~mode:"aot" () in
  Alcotest.(check string)
    "outer AOT rejects a default needing unresolved output relocations" "error"
    (aot |> member "outcome" |> to_string)

let cli_aot_example compiler example () =
  List.iter
    (fun target ->
      List.iter
        (fun mode ->
          let run status options =
            invoke compiler status
              ([
                 "run"; "--target=" ^ target; "--mode=" ^ mode; "--format=json";
               ]
              @ options @ [ example ])
          in
          let report = run 0 [] in
          Alcotest.(check string)
            "maintained numeric defaults example bits" "0x4015000000000000"
            (report |> member "final_value" |> member "bits" |> to_string);
          Alcotest.(check int)
            "all original named and anonymous defaults saved" 32
            (report |> member "prepared_default_bytes" |> to_int);
          let work = report |> member "compiled_initializer_steps" |> to_int in
          let exact =
            run 0
              [
                "--default-byte-limit=32";
                "--initializer-step-limit=" ^ string_of_int work;
              ]
          in
          Alcotest.(check string)
            "exact default and work limits succeed" "success"
            (exact |> member "outcome" |> to_string);
          let bounded = run 1 [ "--default-byte-limit=31" ] in
          Alcotest.(check string)
            "one byte below saved defaults fails" "error"
            (bounded |> member "outcome" |> to_string);
          Alcotest.(check bool)
            "fourth default fails its saved-byte allowance" true
            (bounded |> member "diagnostics" |> to_list
            |> List.exists (fun diagnostic ->
                diagnostic |> member "code" |> to_string = "HCIRVM0011"));
          Alcotest.(check int)
            "byte failure retains the first three saved defaults" 24
            (bounded |> member "prepared_default_bytes" |> to_int);
          let bounded_work =
            run 1 [ "--initializer-step-limit=" ^ string_of_int (work - 1) ]
          in
          Alcotest.(check bool)
            "one preparation step below the example fails" true
            (bounded_work |> member "diagnostics" |> to_list
            |> List.exists (fun diagnostic ->
                diagnostic |> member "code" |> to_string = "HCIRVM0007"));
          Alcotest.(check int)
            "work failure retains its exact reached allowance" (work - 1)
            (bounded_work |> member "compiled_initializer_steps" |> to_int);
          Alcotest.(check int)
            "work failure retains the first three saved defaults" 24
            (bounded_work |> member "prepared_default_bytes" |> to_int))
        [ "jit"; "aot" ])
    [ "host-jit-task"; "host-jit" ]

let () =
  if Array.length Sys.argv <> 4 then
    failwith
      "usage: test_native_f64_defaults.exe <holyc.exe> \
       <native-f64-defaults.hc> <aot-native-defaults.hc>"
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
            Alcotest.test_case "ordinary AOT executes original numeric defaults"
              `Quick distinct_aot_preparation;
            Alcotest.test_case "JIT nested exe defaults in both outer modes"
              `Quick nested_jit_exe_defaults;
            Alcotest.test_case
              "isolated native numeric defaults and cumulative limits" `Quick
              isolated_numeric_defaults;
            Alcotest.test_case
              "closed native capture ownership, lifetime and GC" `Quick
              closed_capture_authority;
            Alcotest.test_case "F64 defaults converted to integer" `Quick
              conversion_defaults;
          ] );
        ( "public CLI",
          [
            Alcotest.test_case "source example, F64 bits and admission limits"
              `Quick
              (cli_example Sys.argv.(1) Sys.argv.(2));
            Alcotest.test_case "AOT and JIT numeric defaults examples" `Quick
              (cli_aot_example Sys.argv.(1) Sys.argv.(3));
          ] );
      ]
