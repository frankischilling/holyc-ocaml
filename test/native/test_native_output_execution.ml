open Holyc_lib
module Program = X86_64_program
module Runtime = Native_program_execution
module VM = Ir_integer_interpreter
module Unit = Holyc_lib__Driver.Integer_unit

let require_ok show = function
  | Ok value -> value
  | Error error -> Alcotest.fail (show error)

let diagnostics_text diagnostics =
  diagnostics
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]
let putchars = "extern U0 PutChars(U64 ch);"

let inputs mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-output-execution.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id
  in
  (session, config, source)

let interpreter_report ?max_output_bytes ?max_output_work ?(max_steps = 10_000)
    mode contents =
  let session, config, source = inputs mode contents in
  run_integer_program_report ?max_output_bytes ?max_output_work ~max_steps
    session ~config ~source

let interpreter_success ?max_output_bytes ?max_output_work ?max_steps mode
    contents =
  let report =
    interpreter_report ?max_output_bytes ?max_output_work ?max_steps mode
      contents
  in
  let checked =
    integer_program_report_outcome report |> require_ok diagnostics_text
  in
  (report, checked.value)

let interpreter_fault ?max_output_bytes ?max_output_work ?max_steps mode
    contents =
  let report =
    interpreter_report ?max_output_bytes ?max_output_work ?max_steps mode
      contents
  in
  let diagnostics =
    match integer_program_report_outcome report with
    | Ok _ -> Alcotest.fail "shared interpreter unexpectedly completed"
    | Error diagnostics -> diagnostics
  in
  (report, List.hd diagnostics)

let vm_errors_text errors =
  errors
  |> List.map (fun (error : VM.error) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let batch_fixture mode contents =
  Native_scalar_fixture.compile ~mode ~path:"native-output-batch.hc" ~contents
    ()
  |> require_ok diagnostics_text

let batch_report ?max_output_bytes ?max_output_work ?(max_steps = 10_000)
    ?(max_frame_bytes = 1_048_576) ?(max_call_depth = 128) fixture =
  let unit_ = fixture.Native_scalar_fixture.unit_ in
  VM.execute_program_report ?max_output_bytes ?max_output_work ~max_steps
    ~max_frame_bytes ~max_call_depth ~runtime_calls:(Unit.runtime_calls unit_)
    ~globals:(Unit.globals unit_)
    ~initialization:(Unit.initialization unit_)
    ~functions:(Unit.functions unit_) (Unit.entry unit_)

let batch_success ?max_output_bytes ?max_output_work ?max_steps ?max_frame_bytes
    ?max_call_depth fixture =
  let report =
    batch_report ?max_output_bytes ?max_output_work ?max_steps ?max_frame_bytes
      ?max_call_depth fixture
  in
  let execution = VM.report_outcome report |> require_ok vm_errors_text in
  (report, execution)

let batch_fault ?max_output_bytes ?max_output_work ?max_steps ?max_frame_bytes
    ?max_call_depth fixture =
  let report =
    batch_report ?max_output_bytes ?max_output_work ?max_steps ?max_frame_bytes
      ?max_call_depth fixture
  in
  match VM.report_outcome report with
  | Ok _ -> Alcotest.fail "checked batch unexpectedly completed"
  | Error [] -> Alcotest.fail "checked batch returned no fault"
  | Error (error :: _) -> (report, error)

let report ?max_output_bytes ?max_output_work ?max_frame_bytes ?max_call_depth
    ?max_initializer_steps ?(max_steps = 10_000) mode contents =
  let session, config, source = inputs mode contents in
  Native_program.evaluate ?max_output_bytes ?max_output_work ?max_frame_bytes
    ?max_call_depth ?max_initializer_steps session ~config ~source ~max_steps

let success ?max_output_bytes ?max_output_work ?max_frame_bytes ?max_call_depth
    ?max_initializer_steps ?max_steps mode contents =
  let report =
    report ?max_output_bytes ?max_output_work ?max_frame_bytes ?max_call_depth
      ?max_initializer_steps ?max_steps mode contents
  in
  let checked = Native_program.outcome report |> require_ok diagnostics_text in
  (report, checked.value)

let first_error report =
  match Native_program.outcome report with
  | Ok _ -> Alcotest.fail "native source unexpectedly completed"
  | Error [] -> Alcotest.fail "native source returned no diagnostic"
  | Error (error :: _) -> error

let fault ?max_output_bytes ?max_output_work ?max_frame_bytes ?max_call_depth
    ?max_initializer_steps ?max_steps mode contents =
  let report =
    report ?max_output_bytes ?max_output_work ?max_frame_bytes ?max_call_depth
      ?max_initializer_steps ?max_steps mode contents
  in
  (report, first_error report)

let check_word label expected = function
  | None -> Alcotest.fail (label ^ ": missing final value")
  | Some (word : Program.word) ->
      Alcotest.(check int64) label expected word.bits

let check_output label expected_bytes expected_work report =
  Alcotest.(check string)
    (label ^ " bytes") expected_bytes
    (Native_program.output_bytes report);
  Alcotest.(check int)
    (label ^ " work") expected_work
    (Native_program.output_work report)

let check_fault_kind expected report =
  match Native_program.native_outcome report with
  | Some (Program.Fault fault) ->
      Alcotest.(check bool) "native fault kind" true (fault.kind = expected)
  | Some (Program.Completed _) ->
      Alcotest.fail "diagnostic followed successful native completion"
  | None -> Alcotest.fail "expected a reached native fault"

let native_fault report =
  match Native_program.native_outcome report with
  | Some (Program.Fault fault) -> fault
  | Some (Program.Completed _) -> Alcotest.fail "expected native fault"
  | None -> Alcotest.fail "expected reached native fault"

let basic_provider_modes () =
  let explicit = putchars ^ "PutChars('42\\n');42;" in
  let implicit = putchars ^ "'42\\n';42;" in
  let implicit_expression =
    putchars ^ "I64 F(){U64 ch='42\\n';'' ch;return 42;}F();"
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, exact_steps) ->
          let public_report, public = interpreter_success mode source in
          Alcotest.(check string)
            (label ^ " shared bytes") "42\n"
            (integer_program_report_output_bytes public_report);
          Alcotest.(check int)
            (label ^ " shared work") 6
            (integer_program_report_output_work public_report);
          (match VM.final_value public with
          | Some word ->
              Alcotest.(check int64)
                (label ^ " shared final value")
                42L word.bits
          | None -> Alcotest.fail (label ^ ": shared final value absent"));
          let fixture = batch_fixture mode source in
          let batch_report, batch = batch_success fixture in
          Alcotest.(check string)
            (label ^ " checked-batch bytes")
            "42\n"
            (VM.report_output_bytes batch_report);
          Alcotest.(check int)
            (label ^ " checked-batch work")
            6
            (VM.report_output_work batch_report);
          let report, native = success mode source in
          check_output label "42\n" 6 report;
          Alcotest.(check int)
            (label ^ " checked batch/native steps")
            (VM.executed_steps batch) native.execution.executed_steps;
          Option.iter
            (fun expected ->
              Alcotest.(check int)
                (label ^ " maintained exact IR steps")
                expected native.execution.executed_steps)
            exact_steps;
          check_word (label ^ " final value") 42L native.execution.final_value;
          Alcotest.(check bool)
            (label ^ " image output marker")
            true
            (Program.has_output native.image))
        [
          ("explicit", explicit, Some 9);
          ("implicit", implicit, Some 9);
          ("implicit expression", implicit_expression, None);
        ])
    modes

let result_latch () =
  List.iter
    (fun mode ->
      let implicit_report, implicit = success mode (putchars ^ "42;'x';") in
      check_output "implicit latch" "x" 2 implicit_report;
      check_word "implicit output preserves prior result" 42L
        implicit.execution.final_value;
      let explicit_report, explicit =
        success mode (putchars ^ "42;PutChars('x');")
      in
      check_output "explicit latch" "x" 2 explicit_report;
      Alcotest.(check bool)
        "explicit U0 call clears the entry result latch" true
        (Option.is_none explicit.execution.final_value))
    modes

let packed_bytes () =
  let source =
    putchars ^ "PutChars(0);PutChars(0x00420041);PutChars(0x008000FF);42;"
  in
  List.iter
    (fun mode ->
      let shared_report, _ = interpreter_success mode source in
      Alcotest.(check string)
        "shared zero/interior-zero/high-bit bytes" "AB\255\128"
        (integer_program_report_output_bytes shared_report);
      Alcotest.(check int)
        "shared zero/interior-zero/high-bit work" 10
        (integer_program_report_output_work shared_report);
      let report, native = success mode source in
      check_output "zero/interior-zero/high-bit" "AB\255\128" 10 report;
      check_word "packed-byte final value" 42L native.execution.final_value;
      let full = putchars ^ "PutChars(0xffffffffffffffff);42;" in
      let shared_full, _ = interpreter_success mode full in
      let expected = String.make 8 '\255' in
      Alcotest.(check string)
        "shared full-width packed bytes" expected
        (integer_program_report_output_bytes shared_full);
      Alcotest.(check int)
        "shared full-width packed work" 16
        (integer_program_report_output_work shared_full);
      let full_report, full_native = success mode full in
      check_output "full-width packed word" expected 16 full_report;
      check_word "full-width packed final value" 42L
        full_native.execution.final_value)
    modes

let nested_calls_and_recursion () =
  let source = putchars ^ "U0 R(I64 n){if(n)R(n-1);PutChars(48+n);}R(2);42;" in
  List.iter
    (fun mode ->
      let report, native = success mode source in
      check_output "recursive provider" "012" 6 report;
      check_word "recursive final value" 42L native.execution.final_value)
    modes

let source_defined_and_mixed_provider () =
  let source_only = "I64 G=0;U0 PutChars(U64 ch){G=ch;}PutChars(42);G;" in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source) ->
          let shared_report, shared = interpreter_success mode source in
          Alcotest.(check string)
            (label ^ " shared output") ""
            (integer_program_report_output_bytes shared_report);
          Alcotest.(check int)
            (label ^ " shared output work")
            0
            (integer_program_report_output_work shared_report);
          (match VM.final_value shared with
          | Some word ->
              Alcotest.(check int64)
                (label ^ " shared final value")
                42L word.bits
          | None -> Alcotest.fail (label ^ ": shared final value absent"));
          let report, native = success mode source in
          check_output label "" 0 report;
          check_word (label ^ " final value") 42L native.execution.final_value;
          Alcotest.(check bool)
            (label ^ " is not native output")
            false
            (Program.has_output native.image))
        [
          ("source PutChars direct call", source_only);
          ( "source body selected after its extern declaration",
            "I64 G=0;extern U0 PutChars(U64 ch);U0 PutChars(U64 \
             ch){G=ch;}PutChars(42);G;" );
          ( "source U0 PutChars implicit discard",
            "I64 G=0;U0 PutChars(U64 ch){G=ch;}42;'x';" );
          ( "source word PutChars implicit discard",
            "I64 PutChars(U64 ch){return 7;}42;'x';" );
        ])
    modes;
  let mixed =
    putchars ^ "U0 Saved(){PutChars('A');}Saved();U0 PutChars(U64 ch){}42;"
  in
  List.iter
    (fun (mode, expected_output, expected_work) ->
      let shared_report, shared = interpreter_success mode mixed in
      Alcotest.(check string)
        "interpreted joined publication output" expected_output
        (integer_program_report_output_bytes shared_report);
      Alcotest.(check int)
        "interpreted joined publication work" expected_work
        (integer_program_report_output_work shared_report);
      (match VM.final_value shared with
      | Some word ->
          Alcotest.(check int64)
            "interpreted joined publication completes" 42L word.bits
      | None -> Alcotest.fail "interpreted joined publication lost its value");
      let native_report, native_error = fault mode mixed in
      Alcotest.(check string)
        "native joined publication remains an explicit boundary" "HCBACK0002"
        native_error.code;
      check_output "unadmitted native publication" "" 0 native_report;
      Alcotest.(check bool)
        "unadmitted publication has no image or execution" true
        (Option.is_none (Native_program.image native_report)
        && Option.is_none (Native_program.native_outcome native_report)))
    [ (Preprocessor.Jit, "A", 2); (Preprocessor.Aot, "", 0) ]

let provider_source_gate () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let report, native = success mode source in
          check_output "provider punctuation" "A" 2 report;
          check_word "provider punctuation final value" 42L
            native.execution.final_value;
          Alcotest.(check bool)
            "provider punctuation retains runtime authority" true
            (Program.has_output native.image))
        [
          "extern U0 PutChars(;U64 ch);PutChars('A');42;";
          "extern U0 PutChars(U64 ch;);PutChars('A');42;";
          "extern U0 PutChars(U64 ch) PutChars('A');42;";
        ];
      List.iter
        (fun source ->
          let report, error = fault mode source in
          Alcotest.(check string)
            "invalid provider prototype" "HCRUN0001" error.code;
          check_output "invalid provider prototype" "" 0 report;
          Alcotest.(check bool)
            "invalid provider prototype has no native image" true
            (Option.is_none (Native_program.image report)))
        [
          "extern I64 PutChars(U64 ch);PutChars('A');42;";
          "extern U0 PutChars(I64 ch);PutChars('A');42;";
          "extern U0 PutChars(U64 ch,...);PutChars('A');42;";
        ])
    modes

let argument_effects_and_fault_prefix () =
  List.iter
    (fun mode ->
      let effect_source =
        putchars ^ "I64 F(){I64 n=40;PutChars(n+=2);return n;}F();"
      in
      let shared_effect_report, shared_effect =
        interpreter_success mode effect_source
      in
      Alcotest.(check string)
        "shared argument side effect bytes" "*"
        (integer_program_report_output_bytes shared_effect_report);
      Alcotest.(check int)
        "shared argument side effect work" 2
        (integer_program_report_output_work shared_effect_report);
      (match VM.final_value shared_effect with
      | Some word ->
          Alcotest.(check int64)
            "shared argument side effect result" 42L word.bits
      | None -> Alcotest.fail "shared argument side effect lost its result");
      let effect_report, effect_result = success mode effect_source in
      check_output "argument side effect" "*" 2 effect_report;
      check_word "argument side effect result" 42L
        effect_result.execution.final_value;
      let source = putchars ^ "PutChars('A');PutChars(1/0);42;" in
      let shared_fault_report, shared_fault = interpreter_fault mode source in
      Alcotest.(check string)
        "shared argument fault" "HCIRVM0009" shared_fault.code;
      Alcotest.(check string)
        "shared argument fault prefix" "A"
        (integer_program_report_output_bytes shared_fault_report);
      Alcotest.(check int)
        "shared argument fault work" 2
        (integer_program_report_output_work shared_fault_report);
      let report, error = fault mode source in
      Alcotest.(check string) "argument fault" "HCIRVM0009" error.code;
      check_output "argument fault prefix" "A" 2 report;
      check_fault_kind Program.Division_by_zero report)
    modes

let initializer_certificates () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let report, native = success mode source in
          check_output "prepared output" "A" 2 report;
          check_word "prepared output final value" 42L
            native.execution.final_value;
          Alcotest.(check bool)
            "prepared callable image keeps provider authority" true
            (Program.has_output native.image);
          Alcotest.(check bool)
            "prepared initializer work is retained" true
            (Native_program.preparation_steps report > 0))
        [
          putchars ^ "I64 G=40;PutChars('A');G+2;";
          putchars ^ "I64 F(){static I64 n=40;PutChars('A');return n+2;}F();";
        ])
    modes

let exact_limits () =
  let source = putchars ^ "PutChars('42\\n');42;" in
  List.iter
    (fun mode ->
      let fixture = batch_fixture mode source in
      let batch_exact_report, batch_exact =
        batch_success ~max_steps:9 ~max_output_bytes:3 ~max_output_work:6
          fixture
      in
      Alcotest.(check int)
        "checked batch exact meter" 9
        (VM.executed_steps batch_exact);
      Alcotest.(check string)
        "checked batch exact bytes" "42\n"
        (VM.report_output_bytes batch_exact_report);
      Alcotest.(check int)
        "checked batch exact output work" 6
        (VM.report_output_work batch_exact_report);
      let exact, native =
        success ~max_steps:9 ~max_output_bytes:3 ~max_output_work:6 mode source
      in
      check_output "exact output limits" "42\n" 6 exact;
      Alcotest.(check int)
        "exact step allowance" 9 native.execution.executed_steps;
      let shared_bytes, shared_byte_error =
        interpreter_fault ~max_output_bytes:2 ~max_output_work:6 mode source
      in
      Alcotest.(check string)
        "shared byte one-below diagnostic" "HCIRVM0022" shared_byte_error.code;
      Alcotest.(check string)
        "shared byte one-below prefix" "42"
        (integer_program_report_output_bytes shared_bytes);
      Alcotest.(check int)
        "shared byte one-below work" 6
        (integer_program_report_output_work shared_bytes);
      let batch_bytes, batch_byte_error =
        batch_fault ~max_output_bytes:2 ~max_output_work:6 fixture
      in
      Alcotest.(check string)
        "checked batch byte one-below diagnostic" "HCIRVM0022"
        batch_byte_error.code;
      Alcotest.(check string)
        "checked batch byte one-below prefix" "42"
        (VM.report_output_bytes batch_bytes);
      Alcotest.(check int)
        "checked batch byte one-below work" 6
        (VM.report_output_work batch_bytes);
      let bytes, byte_error =
        fault ~max_output_bytes:2 ~max_output_work:6 mode source
      in
      Alcotest.(check string) "byte one-below" "HCIRVM0022" byte_error.code;
      check_output "byte one-below prefix" "42" 6 bytes;
      check_fault_kind Program.Output_limit_exceeded bytes;
      let native_byte = native_fault bytes in
      Alcotest.(check int)
        "byte fault executed-step site" batch_byte_error.executed_steps
        native_byte.executed_steps;
      Alcotest.(check (option int))
        "byte fault instruction site" batch_byte_error.instruction_id
        (Some native_byte.instruction_id);
      Alcotest.(check (option int))
        "byte fault block site" batch_byte_error.block_id
        (Some native_byte.block_id);
      Alcotest.(check bool)
        "byte fault source span" true
        (batch_byte_error.span = native_byte.span);
      let shared_work, shared_work_error =
        interpreter_fault ~max_output_bytes:3 ~max_output_work:5 mode source
      in
      Alcotest.(check string)
        "shared work one-below diagnostic" "HCIRVM0023" shared_work_error.code;
      Alcotest.(check string)
        "shared work one-below prefix" "42"
        (integer_program_report_output_bytes shared_work);
      Alcotest.(check int)
        "shared work one-below count" 5
        (integer_program_report_output_work shared_work);
      let batch_work, batch_work_error =
        batch_fault ~max_output_bytes:3 ~max_output_work:5 fixture
      in
      Alcotest.(check string)
        "checked batch work one-below diagnostic" "HCIRVM0023"
        batch_work_error.code;
      Alcotest.(check string)
        "checked batch work one-below prefix" "42"
        (VM.report_output_bytes batch_work);
      Alcotest.(check int)
        "checked batch work one-below count" 5
        (VM.report_output_work batch_work);
      let work, work_error =
        fault ~max_output_bytes:3 ~max_output_work:5 mode source
      in
      Alcotest.(check string) "work one-below" "HCIRVM0023" work_error.code;
      check_output "work one-below prefix" "42" 5 work;
      check_fault_kind Program.Output_work_limit_exceeded work;
      let native_work = native_fault work in
      Alcotest.(check int)
        "work fault executed-step site" batch_work_error.executed_steps
        native_work.executed_steps;
      Alcotest.(check (option int))
        "work fault instruction site" batch_work_error.instruction_id
        (Some native_work.instruction_id);
      Alcotest.(check (option int))
        "work fault block site" batch_work_error.block_id
        (Some native_work.block_id);
      Alcotest.(check bool)
        "work fault source span" true
        (batch_work_error.span = native_work.span);
      let steps, step_error = fault ~max_steps:8 mode source in
      Alcotest.(check string) "step one-below" "HCIRVM0007" step_error.code;
      check_output "step one-below retains output" "42\n" 6 steps;
      check_fault_kind Program.Step_limit_exceeded steps;
      let framed =
        putchars ^ "I64 F(){I64 n=42;PutChars('42\\n');return n;}F();"
      in
      let framed_report, framed_result =
        success ~max_frame_bytes:16 ~max_call_depth:2 mode framed
      in
      check_output "exact frame/depth" "42\n" 6 framed_report;
      check_word "exact frame/depth result" 42L
        framed_result.execution.final_value;
      let frame, frame_error = fault ~max_frame_bytes:15 mode framed in
      Alcotest.(check string) "frame one-below" "HCIRVM0011" frame_error.code;
      check_fault_kind Program.Frame_limit_exceeded frame;
      let depth, depth_error = fault ~max_call_depth:1 mode framed in
      Alcotest.(check string) "depth one-below" "HCIRVM0015" depth_error.code;
      check_fault_kind Program.Call_depth_exceeded depth;
      let zero = putchars ^ "I64 F(){PutChars(0);return 42;}F();" in
      let zero_frame, zero_frame_error = fault ~max_frame_bytes:7 mode zero in
      Alcotest.(check string)
        "zero provider frame quota" "HCIRVM0011" zero_frame_error.code;
      check_output "zero provider frame quota" "" 0 zero_frame;
      check_fault_kind Program.Frame_limit_exceeded zero_frame;
      let zero_depth, zero_depth_error = fault ~max_call_depth:1 mode zero in
      Alcotest.(check string)
        "zero provider depth quota" "HCIRVM0015" zero_depth_error.code;
      check_output "zero provider depth quota" "" 0 zero_depth;
      check_fault_kind Program.Call_depth_exceeded zero_depth)
    modes

let invalid_output_configuration_precedes_initializers () =
  let source = "I64 G=1/0;42;" in
  List.iter
    (fun mode ->
      List.iter
        (fun report ->
          let error = first_error report in
          Alcotest.(check string)
            "output configuration diagnostic" "HCIRVM0001" error.code;
          Alcotest.(check int)
            "invalid output configuration prepares nothing" 0
            (Native_program.preparation_steps report);
          Alcotest.(check bool)
            "invalid output configuration has no native image" true
            (Option.is_none (Native_program.image report));
          Alcotest.(check bool)
            "invalid output configuration executes nothing" true
            (Option.is_none (Native_program.executed_steps report));
          check_output "invalid output configuration" "" 0 report)
        [
          report ~max_output_bytes:0 mode source;
          report ~max_output_work:0 mode source;
          report
            ~max_output_bytes:(Runtime.hard_max_output_bytes + 1)
            mode source;
        ])
    modes

let fresh_execution_reports () =
  let source = putchars ^ "I64 G=65;PutChars(G);G++;G-24;" in
  List.iter
    (fun mode ->
      let session, config, source = inputs mode source in
      let image =
        Native_program.compile session ~config ~source
        |> require_ok diagnostics_text
        |> fun checked -> checked.value
      in
      Alcotest.(check bool)
        "compiled image has output" true (Program.has_output image);
      let execute () =
        Runtime.execute_report ~max_steps:1000 ~max_output_bytes:1
          ~max_output_work:2 image
      in
      let first = execute () and second = execute () in
      List.iter
        (fun runtime_report ->
          Alcotest.(check string)
            "fresh execution output" "A"
            (Runtime.output_bytes runtime_report);
          Alcotest.(check int)
            "fresh execution work" 2
            (Runtime.output_work runtime_report);
          match Runtime.outcome runtime_report with
          | Ok (Program.Completed execution) ->
              check_word "fresh execution value" 42L execution.final_value
          | Ok (Program.Fault _) -> Alcotest.fail "fresh execution faulted"
          | Error message -> Alcotest.fail message)
        [ first; second ])
    modes

let () =
  Alcotest.run "native output execution"
    [
      ( "output",
        [
          Alcotest.test_case "explicit and implicit provider modes" `Quick
            basic_provider_modes;
          Alcotest.test_case "explicit and implicit result latch" `Quick
            result_latch;
          Alcotest.test_case "zero interior-zero and high-bit bytes" `Quick
            packed_bytes;
          Alcotest.test_case "nested source calls and recursion" `Quick
            nested_calls_and_recursion;
          Alcotest.test_case "source-defined and mixed PutChars" `Quick
            source_defined_and_mixed_provider;
          Alcotest.test_case "provider source gate" `Quick provider_source_gate;
          Alcotest.test_case "argument effects and fault prefix" `Quick
            argument_effects_and_fault_prefix;
          Alcotest.test_case "initializer certificates with output" `Quick
            initializer_certificates;
          Alcotest.test_case "exact independent resource limits" `Quick
            exact_limits;
          Alcotest.test_case "output configuration precedes initializers" `Quick
            invalid_output_configuration_precedes_initializers;
          Alcotest.test_case "fresh execution reports" `Quick
            fresh_execution_reports;
        ] );
    ]
