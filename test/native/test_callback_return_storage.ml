open Holyc_lib
module Program = X86_64_program
module Runtime = Native_program_execution
module VM = Ir_integer_interpreter

let require_ok show = function
  | Ok value -> value
  | Error error -> Alcotest.fail (show error)

let diagnostics_text diagnostics =
  diagnostics
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let vm_errors_text errors =
  errors
  |> List.map (fun (error : VM.error) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let source_inputs ~mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"callback-return-storage.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id
  in
  (session, config, source)

let host_abi () =
  match Runtime.platform () with
  | Runtime.Windows_x86_64 -> Program.Windows_x64
  | Runtime.Linux_x86_64 -> Program.System_v_x64
  | Runtime.Unsupported -> Alcotest.fail "callback storage requires x86-64"

let check_vm_word label expected result =
  match VM.final_value result with
  | Some word ->
      Alcotest.(check bool) (label ^ " VM type") true (word.type_ = VM.I64);
      Alcotest.(check int64) (label ^ " VM bits") expected word.bits
  | None -> Alcotest.failf "%s: checked IR returned no word" label

let check_native_word label expected = function
  | Some (word : Program.word) ->
      Alcotest.(check bool)
        (label ^ " native type") true (word.type_ = Program.I64);
      Alcotest.(check int64) (label ^ " native bits") expected word.bits
  | None -> Alcotest.failf "%s: native execution returned no word" label

let batch_fixture ~mode contents =
  Native_scalar_fixture.compile ~mode ~path:"callback-return-storage-batch.hc"
    ~contents ()
  |> require_ok diagnostics_text

let batch_success ?max_frame_bytes ?max_global_bytes ~mode ~max_steps contents =
  let fixture = batch_fixture ~mode contents in
  let execution =
    Native_scalar_fixture.execute ?max_frame_bytes ?max_global_bytes ~max_steps
      fixture
    |> require_ok vm_errors_text
  in
  (fixture, execution)

let batch_failure ?max_frame_bytes ?max_global_bytes ~mode ~max_steps contents =
  let fixture = batch_fixture ~mode contents in
  match
    Native_scalar_fixture.execute ?max_frame_bytes ?max_global_bytes ~max_steps
      fixture
  with
  | Error (first :: _) -> (fixture, first)
  | Error [] -> Alcotest.fail "checked callback batch returned no error"
  | Ok _ -> Alcotest.fail "checked callback batch unexpectedly completed"

let public_success ?max_frame_bytes ?max_global_bytes ~mode ~max_steps contents
    =
  let session, config, source = source_inputs ~mode contents in
  run_integer_program ?max_frame_bytes ?max_global_bytes session ~config ~source
    ~max_steps
  |> require_ok diagnostics_text
  |> fun checked -> checked.value

let native_report ?max_frame_bytes ?max_global_bytes ?max_code_bytes ?status_abi
    ~mode ~max_steps contents =
  let session, config, source = source_inputs ~mode contents in
  Native_program.evaluate ?max_frame_bytes ?max_global_bytes ?max_code_bytes
    ?status_abi session ~config ~source ~max_steps

let native_success ?max_frame_bytes ?max_global_bytes ~mode ~max_steps contents
    =
  let report =
    native_report ?max_frame_bytes ?max_global_bytes ~status_abi:(host_abi ())
      ~mode ~max_steps contents
  in
  match Native_program.outcome report with
  | Ok checked -> (report, checked.value)
  | Error diagnostics -> Alcotest.fail (diagnostics_text diagnostics)

let compare_success ~mode ~label contents =
  let fixture, batch = batch_success ~mode ~max_steps:10_000 contents in
  let steps = VM.executed_steps batch in
  check_vm_word (label ^ " checked batch") 42L batch;
  let public = public_success ~mode ~max_steps:10_000 contents in
  check_vm_word (label ^ " fresh public IR") 42L public;
  let _, native = native_success ~mode ~max_steps:steps contents in
  check_native_word label 42L native.execution.final_value;
  Alcotest.(check int)
    (label ^ " checked/native steps")
    steps native.execution.executed_steps;
  fixture

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let return_headers_store_full_words () =
  let rows =
    [
      ( "F64 scalar return header",
        "I64 Run(){F64 (*p)(I64 n);p=0xffffffffffffffff;if(p==-1)return \
         42;return 0;}Run();" );
      ( "U0 return header",
        "I64 Run(){U0 (*p)(I64 \
         n);p=0x8000000000000001;if(p==0x8000000000000001)return 42;return \
         0;}Run();" );
      ( "integer scalar return header",
        "I64 Run(){I64 (*p)(I64 n);p=0xffffffffffffffff;if(p==-1)return \
         42;return 0;}Run();" );
      ( "one return pointer layer",
        "I64 Run(){I64 *(*p)(I64 n);p=0xffffffffffffffff;if(p==-1)return \
         42;return 0;}Run();" );
      ( "four return pointer layers",
        "I64 Run(){I64 ****(*p)(I64 \
         n);p=0x8000000000000001;if(p==0x8000000000000001)return 42;return \
         0;}Run();" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source) -> ignore (compare_success ~mode ~label source))
        rows)
    modes

let forwarded_numeric_source =
  "I64 Forward(F64 **(*incoming)(I64 n)){F64 **(*local)(I64 n);static F64 \
   **(*saved)(I64 \
   n)[2];local=incoming;saved[1]=local;if(saved[1]==0xffffffffffffffff)return \
   42;return 0;}I64 Run(){F64 (*cells)(I64 \
   n)[2][3];cells[1][2]=0xffffffffffffffff;return Forward(cells[1][2]);}Run();"

let owned_code_source =
  "I64 Answer(I64 n){return n+2;}I64 Forward(F64 **(*incoming)(I64 n)){F64 \
   **(*local)(I64 n);static F64 **(*saved)(I64 n)[2];I64 (*invoke)(I64 \
   n);local=incoming;saved[1]=local;invoke=saved[1];return invoke(40);}I64 \
   Run(){F64 (*cells)(I64 n)[2][3];cells[1][2]=&Answer;return \
   Forward(cells[1][2]);}Run();"

let copies_forward_numeric_and_owned_words () =
  List.iter
    (fun mode ->
      ignore
        (compare_success ~mode ~label:"forwarded full numeric callback word"
           forwarded_numeric_source);
      ignore
        (compare_success ~mode ~label:"F64 storage retains owned I64 code"
           owned_code_source))
    modes

let expect_fault ~mode ~label ~code ~output source =
  let _, batch = batch_failure ~mode ~max_steps:10_000 source in
  Alcotest.(check string) (label ^ " checked batch code") code batch.code;
  let session, config, input = source_inputs ~mode source in
  let public =
    run_integer_program_report session ~config ~source:input ~max_steps:10_000
  in
  let public_errors =
    match integer_program_report_outcome public with
    | Error errors -> errors
    | Ok _ -> Alcotest.fail (label ^ ": public IR unexpectedly completed")
  in
  Alcotest.(check string)
    (label ^ " public code") code (List.hd public_errors).code;
  Alcotest.(check string)
    (label ^ " public output") output
    (integer_program_report_output_bytes public);
  let report =
    native_report ~status_abi:(host_abi ()) ~mode ~max_steps:10_000 source
  in
  let diagnostics =
    match Native_program.outcome report with
    | Error diagnostics -> diagnostics
    | Ok _ -> Alcotest.fail (label ^ ": native execution unexpectedly completed")
  in
  let fault =
    match Native_program.native_outcome report with
    | Some (Program.Fault fault) -> fault
    | Some (Program.Completed _) ->
        Alcotest.fail (label ^ ": native report completed after an error")
    | None -> Alcotest.fail (label ^ ": native fault did not reach machine code")
  in
  Alcotest.(check string)
    (label ^ " native code") code (List.hd diagnostics).code;
  Alcotest.(check int)
    (label ^ " checked/native reached steps")
    batch.executed_steps fault.executed_steps;
  Alcotest.(check string)
    (label ^ " native output") output
    (Native_program.output_bytes report)

let faults_keep_storage_order () =
  let output =
    "extern U0 PutChars(U64 ch);I64 Arg(){PutChars('A');return 40;}"
  in
  let rows =
    [
      ( "signature mismatch after argument effects",
        "HCIRVM0014",
        "A",
        output
        ^ "U64 Bad(I64 n){return n;}I64 Run(){F64 (*stored)(I64 n);I64 \
           (*invoke)(I64 n);stored=&Bad;invoke=stored;return \
           invoke(Arg());}Run();" );
      ( "uninitialized F64 callback element",
        "HCIRVM0012",
        "",
        output
        ^ "I64 Run(){F64 (*cells)(I64 n)[2];I64 (*invoke)(I64 \
           n);invoke=cells[1];return invoke(Arg());}Run();" );
      ( "F64 callback index before argument effects",
        "HCIRVM0019",
        "",
        output
        ^ "I64 Run(){F64 (*cells)(I64 n)[2];I64 (*invoke)(I64 \
           n);invoke=cells[2];return invoke(Arg());}Run();" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, code, output, source) ->
          expect_fault ~mode ~label ~code ~output source)
        rows)
    modes

let quota_source =
  "I64 Answer(){return 42;}I64 Run(){F64 (*stored)();I64 (*invoke)();static \
   F64 (*saved)()[2];stored=&Answer;saved[1]=stored;invoke=saved[1];return \
   invoke();}Run();"

let expect_batch_code label code = function
  | Error ((first : VM.error) :: _) ->
      Alcotest.(check string) label code first.code
  | Error [] -> Alcotest.fail (label ^ ": no checked IR error")
  | Ok _ -> Alcotest.fail (label ^ ": checked IR unexpectedly completed")

let exact_storage_code_and_step_quotas () =
  List.iter
    (fun mode ->
      let fixture, batch = batch_success ~mode ~max_steps:10_000 quota_source in
      let steps = VM.executed_steps batch in
      check_vm_word "quota oracle" 42L batch;
      let exact_batch =
        Native_scalar_fixture.execute ~max_frame_bytes:16 ~max_global_bytes:16
          ~max_steps:steps fixture
        |> require_ok vm_errors_text
      in
      check_vm_word "exact checked callback quotas" 42L exact_batch;
      expect_batch_code "checked frame one below" "HCIRVM0011"
        (Native_scalar_fixture.execute ~max_frame_bytes:15 ~max_global_bytes:16
           ~max_steps:steps fixture);
      expect_batch_code "checked global one below" "HCIRVM0016"
        (Native_scalar_fixture.execute ~max_frame_bytes:16 ~max_global_bytes:15
           ~max_steps:steps fixture);
      expect_batch_code "checked steps one below" "HCIRVM0007"
        (Native_scalar_fixture.execute ~max_frame_bytes:16 ~max_global_bytes:16
           ~max_steps:(steps - 1) fixture);
      let public =
        public_success ~max_frame_bytes:16 ~max_global_bytes:16 ~mode
          ~max_steps:steps quota_source
      in
      check_vm_word "exact fresh public callback quotas" 42L public;
      let session, config, source = source_inputs ~mode quota_source in
      let images =
        [ Program.Windows_x64; Program.System_v_x64 ]
        |> List.map (fun abi ->
            let compile ?max_code_bytes ?max_global_bytes () =
              Native_program.compile ?max_code_bytes ?max_global_bytes
                ~status_abi:abi session ~config ~source
            in
            let image =
              compile () |> require_ok diagnostics_text |> fun c -> c.value
            in
            let code = Program.code_bytes image in
            Alcotest.(check int)
              "callback static storage bytes" 16
              (Program.global_bytes image);
            let exact =
              compile ~max_code_bytes:code ~max_global_bytes:16 ()
              |> require_ok diagnostics_text
              |> fun c -> c.value
            in
            Alcotest.(check bool)
              "requested callback image ABI" true
              (Program.status_abi exact = abi);
            Alcotest.(check int)
              "exact callback code bytes" code (Program.code_bytes exact);
            (match
               compile ~max_code_bytes:(code - 1) ~max_global_bytes:16 ()
             with
            | Error (error :: _) ->
                Alcotest.(check string)
                  "callback code one below" "HCBACK0005" error.code
            | Error [] -> Alcotest.fail "callback code one below had no error"
            | Ok _ -> Alcotest.fail "callback code one below compiled");
            (match compile ~max_global_bytes:15 () with
            | Error (_ :: _) -> ()
            | Error [] -> Alcotest.fail "callback global one below had no error"
            | Ok _ -> Alcotest.fail "callback global one below compiled");
            (abi, exact))
      in
      let image = List.assoc (host_abi ()) images in
      let exact =
        Runtime.execute ~max_frame_bytes:16 ~max_global_bytes:16
          ~max_steps:steps image
        |> require_ok Fun.id
      in
      (match exact with
      | Program.Completed execution ->
          check_native_word "exact host callback quotas" 42L
            execution.final_value;
          Alcotest.(check int)
            "exact host callback steps" steps execution.executed_steps
      | Program.Fault _ -> Alcotest.fail "exact host callback quotas faulted");
      (match
         Runtime.execute ~max_frame_bytes:15 ~max_global_bytes:16
           ~max_steps:steps image
         |> require_ok Fun.id
       with
      | Program.Fault fault ->
          Alcotest.(check bool)
            "host frame one below" true
            (fault.kind = Program.Frame_limit_exceeded)
      | Program.Completed _ -> Alcotest.fail "host frame one below completed");
      (match
         Runtime.execute ~max_frame_bytes:16 ~max_global_bytes:15
           ~max_steps:steps image
       with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "host global one below executed");
      match
        Runtime.execute ~max_frame_bytes:16 ~max_global_bytes:16
          ~max_steps:(steps - 1) image
        |> require_ok Fun.id
      with
      | Program.Fault fault ->
          Alcotest.(check bool)
            "host steps one below" true
            (fault.kind = Program.Step_limit_exceeded)
      | Program.Completed _ -> Alcotest.fail "host steps one below completed")
    modes

let compile_error ~mode source =
  let session, config, source = source_inputs ~mode source in
  match Native_program.compile session ~config ~source with
  | Error (first :: _) -> first
  | Error [] -> Alcotest.fail "native callback gate returned no diagnostic"
  | Ok _ -> Alcotest.fail "native callback gate unexpectedly compiled"

let unsupported_f64_execution_stays_gated () =
  List.iter
    (fun mode ->
      List.iter
        (fun contents ->
          Alcotest.(check string)
            "unsupported object or callback indirection stays gated" "HCRUN0001"
            (compile_error ~mode contents).code)
        [
          "I64 Run(){F64 value;return 42;}Run();";
          "I64 Run(){F64 (*p)(),value;return 42;}Run();";
          "I64 Run(){I64 (**p)();return 42;}Run();";
        ];
      let body = "F64 Value(){return 1.0;}42;" in
      let body_error = compile_error ~mode body in
      Alcotest.(check string) "F64 source body gate" "HCRUN0001" body_error.code;
      let call = "I64 Run(){F64 (*p)(I64 n);p=0;p(40);return 42;}Run();" in
      let _, batch = batch_failure ~mode ~max_steps:10_000 call in
      Alcotest.(check string)
        "checked F64 callback call gate" "HCIRVM0014" batch.code;
      Alcotest.(check int)
        "checked F64 callback call is preflight" 0 batch.executed_steps;
      let session, config, input = source_inputs ~mode call in
      let public =
        run_integer_program_report session ~config ~source:input
          ~max_steps:10_000
      in
      let public_errors =
        match integer_program_report_outcome public with
        | Error errors -> errors
        | Ok _ ->
            Alcotest.fail "public F64 callback call unexpectedly completed"
      in
      Alcotest.(check string)
        "public F64 callback call gate" "HCIRVM0014"
        (List.hd public_errors).code;
      let call_error = compile_error ~mode call in
      Alcotest.(check string)
        "native F64 callback call gate" "HCBACK0002" call_error.code)
    modes

let () =
  match Runtime.platform () with
  | Runtime.Unsupported ->
      Alcotest.fail
        "callback return storage tests require Windows x86-64 or Linux x86-64"
  | Runtime.Windows_x86_64 | Runtime.Linux_x86_64 ->
      Alcotest.run "holyc native callback return storage"
        [
          ( "native callback return storage",
            [
              Alcotest.test_case
                "return headers keep callback cells at full word width" `Quick
                return_headers_store_full_words;
              Alcotest.test_case
                "F64 callback storage forwards numeric and owned words" `Quick
                copies_forward_numeric_and_owned_words;
              Alcotest.test_case
                "F64 callback storage faults preserve effect order" `Quick
                faults_keep_storage_order;
              Alcotest.test_case
                "callback storage quotas are exact across IR and native" `Quick
                exact_storage_code_and_step_quotas;
              Alcotest.test_case
                "F64 callback execution and F64 bodies remain gated" `Quick
                unsupported_f64_execution_stays_gated;
            ] );
        ]
