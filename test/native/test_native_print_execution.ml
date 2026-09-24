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

let vm_errors_text errors =
  errors
  |> List.map (fun (error : VM.error) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]
let print = "extern U0 Print(U8 *fmt,...);"
let putchars = "extern U0 PutChars(U64 ch);"

let inputs mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-print-execution.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id
  in
  (session, config, source)

let interpreter_report ?max_output_bytes ?max_output_work ?max_frame_bytes
    ?max_call_depth ?(max_steps = 10_000) mode contents =
  let session, config, source = inputs mode contents in
  run_integer_program_report ?max_output_bytes ?max_output_work ?max_frame_bytes
    ?max_call_depth ~max_steps session ~config ~source

let interpreter_success ?max_output_bytes ?max_output_work ?max_frame_bytes
    ?max_call_depth ?max_steps mode contents =
  let report =
    interpreter_report ?max_output_bytes ?max_output_work ?max_frame_bytes
      ?max_call_depth ?max_steps mode contents
  in
  let checked =
    integer_program_report_outcome report |> require_ok diagnostics_text
  in
  (report, checked.value)

let interpreter_fault ?max_output_bytes ?max_output_work ?max_frame_bytes
    ?max_call_depth ?max_steps mode contents =
  let report =
    interpreter_report ?max_output_bytes ?max_output_work ?max_frame_bytes
      ?max_call_depth ?max_steps mode contents
  in
  match integer_program_report_outcome report with
  | Ok _ -> Alcotest.fail "shared interpreter unexpectedly completed"
  | Error [] -> Alcotest.fail "shared interpreter returned no diagnostic"
  | Error (error :: _) -> (report, error)

let batch_fixture mode contents =
  Native_scalar_fixture.compile ~mode ~path:"native-print-batch.hc" ~contents ()
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

let native_fault report =
  match Native_program.native_outcome report with
  | Some (Program.Fault fault) -> fault
  | Some (Program.Completed _) ->
      Alcotest.fail "expected a reached native fault"
  | None -> Alcotest.fail "expected native execution metadata"

let check_output label ~bytes ~work report =
  Alcotest.(check string)
    (label ^ " bytes") bytes
    (Native_program.output_bytes report);
  Alcotest.(check int)
    (label ^ " work") work
    (Native_program.output_work report)

let check_vm_output label ~bytes ~work report =
  Alcotest.(check string)
    (label ^ " bytes") bytes
    (VM.report_output_bytes report);
  Alcotest.(check int) (label ^ " work") work (VM.report_output_work report)

let check_native_word label expected = function
  | None -> Alcotest.fail (label ^ ": missing final value")
  | Some (word : Program.word) ->
      Alcotest.(check int64) label expected word.bits

let check_vm_word label expected = function
  | None -> Alcotest.fail (label ^ ": missing final value")
  | Some (word : VM.word) -> Alcotest.(check int64) label expected word.bits

let check_final label expected vm (native : Native_program.result) =
  match expected with
  | None ->
      Alcotest.(check bool)
        (label ^ " VM void result")
        true
        (Option.is_none (VM.final_value vm));
      Alcotest.(check bool)
        (label ^ " native void result")
        true
        (Option.is_none native.execution.final_value)
  | Some bits ->
      check_vm_word (label ^ " VM result") bits (VM.final_value vm);
      check_native_word (label ^ " native result") bits
        native.execution.final_value

let compare_success ?max_output_bytes ?max_output_work ?max_frame_bytes
    ?max_call_depth ?max_steps mode ~label ~contents ~bytes ~work ~value () =
  let public_report, public =
    interpreter_success ?max_output_bytes ?max_output_work ?max_frame_bytes
      ?max_call_depth ?max_steps mode contents
  in
  Alcotest.(check string)
    (label ^ " public bytes") bytes
    (integer_program_report_output_bytes public_report);
  Alcotest.(check int)
    (label ^ " public work") work
    (integer_program_report_output_work public_report);
  let fixture = batch_fixture mode contents in
  let batch_report, batch =
    batch_success ?max_output_bytes ?max_output_work ?max_frame_bytes
      ?max_call_depth ?max_steps fixture
  in
  check_vm_output (label ^ " checked batch") ~bytes ~work batch_report;
  let native_report, native =
    success ?max_output_bytes ?max_output_work ?max_frame_bytes ?max_call_depth
      ?max_steps mode contents
  in
  check_output (label ^ " native") ~bytes ~work native_report;
  Alcotest.(check int)
    (label ^ " exact runtime steps")
    (VM.executed_steps batch) native.execution.executed_steps;
  check_final (label ^ " public/native") value public native;
  check_final (label ^ " checked/native") value batch native;
  (native_report, native, fixture)

let compare_reached_fault ?max_output_bytes ?max_output_work ?max_frame_bytes
    ?max_call_depth ?max_steps mode ~label ~contents ~code ~kind ~bytes ~work ()
    =
  let public_report, public_error =
    interpreter_fault ?max_output_bytes ?max_output_work ?max_frame_bytes
      ?max_call_depth ?max_steps mode contents
  in
  Alcotest.(check string) (label ^ " public code") code public_error.code;
  Alcotest.(check string)
    (label ^ " public bytes") bytes
    (integer_program_report_output_bytes public_report);
  Alcotest.(check int)
    (label ^ " public work") work
    (integer_program_report_output_work public_report);
  let fixture = batch_fixture mode contents in
  let batch_report, batch_error =
    batch_fault ?max_output_bytes ?max_output_work ?max_frame_bytes
      ?max_call_depth ?max_steps fixture
  in
  Alcotest.(check string) (label ^ " checked code") code batch_error.code;
  check_vm_output (label ^ " checked batch") ~bytes ~work batch_report;
  let native_report, native_error =
    fault ?max_output_bytes ?max_output_work ?max_frame_bytes ?max_call_depth
      ?max_steps mode contents
  in
  Alcotest.(check string) (label ^ " native code") code native_error.code;
  check_output (label ^ " native") ~bytes ~work native_report;
  let native = native_fault native_report in
  Alcotest.(check bool) (label ^ " native kind") true (native.kind = kind);
  Alcotest.(check int)
    (label ^ " fault step") batch_error.executed_steps native.executed_steps;
  Alcotest.(check (option int))
    (label ^ " fault instruction")
    batch_error.instruction_id (Some native.instruction_id);
  Alcotest.(check (option int))
    (label ^ " fault block") batch_error.block_id (Some native.block_id);
  Alcotest.(check bool)
    (label ^ " fault source span")
    true
    (batch_error.span = native.span);
  native

let expanded_format_values () =
  List.iter
    (fun mode ->
      List.iter
        (fun (case : Integer_format_fixture.t) ->
          ignore
            (compare_success mode ~label:case.label
               ~contents:(Integer_format_fixture.source case)
               ~bytes:case.bytes ~work:case.work ~value:(Some 42L) ()))
        (Integer_format_fixture.all @ Quoted_format_fixture.all
       @ Aux_format_fixture.all);
      List.iter
        (fun (label, contents, bytes, work) ->
          ignore
            (compare_success mode ~label ~contents ~bytes ~work
               ~value:(Some 42L) ()))
        (Integer_format_fixture.argument_effects
       @ Aux_format_fixture.argument_effects))
    modes

let expanded_format_quotas () =
  List.iter
    (fun mode ->
      List.iter
        (fun (case : Integer_format_fixture.t) ->
          let contents = Integer_format_fixture.source case in
          let byte_count = String.length case.bytes in
          ignore
            (compare_success ~max_output_bytes:(max 1 byte_count)
               ~max_output_work:case.work mode ~label:(case.label ^ " exact")
               ~contents ~bytes:case.bytes ~work:case.work ~value:(Some 42L) ());
          let reached =
            compare_reached_fault ~max_output_work:(case.work - 1) mode
              ~label:(case.label ^ " work one below")
              ~contents ~code:"HCIRVM0023"
              ~kind:Program.Output_work_limit_exceeded ~bytes:""
              ~work:(case.work - 1) ()
          in
          Alcotest.(check bool)
            "bounded expanded format is atomic" true reached.atomic_output;
          if byte_count > 1 then
            ignore
              (compare_reached_fault ~max_output_bytes:(byte_count - 1) mode
                 ~label:(case.label ^ " bytes one below")
                 ~contents ~code:"HCIRVM0022"
                 ~kind:Program.Output_limit_exceeded ~bytes:""
                 ~work:(case.work - 1) ()))
        (Integer_format_fixture.quota_cases @ Quoted_format_fixture.quota_cases
       @ Aux_format_fixture.quota_cases);
      List.iter
        (fun (body, work) ->
          let contents = print ^ body ^ "42;" in
          ignore
            (compare_reached_fault ~max_output_bytes:1 mode
               ~label:"huge width reaches bounded append" ~contents
               ~code:"HCIRVM0022" ~kind:Program.Output_limit_exceeded ~bytes:""
               ~work ());
          ignore
            (compare_reached_fault ~max_output_bytes:1
               ~max_output_work:(work - 1) mode
               ~label:"huge width work precedes capacity" ~contents
               ~code:"HCIRVM0023" ~kind:Program.Output_work_limit_exceeded
               ~bytes:"" ~work:(work - 1) ()))
        [
          ("Print(\"%*d\",9223372036854775807,1);", 5);
          ("Print(\"%*s\",9223372036854775807,\"AB\");", 8);
        ];
      let contents = print ^ "Print(\"|\");Print(\"%5s\",\"AB\");42;" in
      ignore
        (compare_reached_fault ~max_output_bytes:5 mode
           ~label:"decorated draft retains only preceding output" ~contents
           ~code:"HCIRVM0022" ~kind:Program.Output_limit_exceeded ~bytes:"|"
           ~work:16 ());
      ignore
        (compare_reached_fault ~max_output_bytes:5 ~max_output_work:15 mode
           ~label:"decorated draft work precedes capacity" ~contents
           ~code:"HCIRVM0023" ~kind:Program.Output_work_limit_exceeded
           ~bytes:"|" ~work:15 ()))
    modes

let expanded_format_failures () =
  List.iter
    (fun mode ->
      List.iter
        (fun (label, body, work, code) ->
          let kind =
            if code = "HCIRVM0024" then Program.Output_invalid_format
            else Program.Output_invalid_argument
          in
          let contents = print ^ body ^ "42;" in
          ignore
            (compare_reached_fault mode ~label ~contents ~code ~kind ~bytes:""
               ~work ());
          ignore
            (compare_reached_fault ~max_output_work:(work - 1) mode
               ~label:(label ^ " work first") ~contents ~code:"HCIRVM0023"
               ~kind:Program.Output_work_limit_exceeded ~bytes:""
               ~work:(work - 1) ()))
        (Integer_format_fixture.invalid_fields
       @ Quoted_format_fixture.invalid_fields
       @ Aux_format_fixture.invalid_fields);
      List.iter
        (fun (body, code, kind, work) ->
          ignore
            (compare_reached_fault mode ~label:"truncation scans through NUL"
               ~contents:(print ^ body) ~code ~kind ~bytes:"" ~work ()))
        [
          ( "U8 Text[2]={'A','B'};Print(\"%1ts\",Text);42;",
            "HCIRVM0019",
            Program.Address_out_of_bounds,
            7 );
          ( "I64 F(){U8 Text[2];Print(\"%0ts\",Text);return 42;}F();",
            "HCIRVM0012",
            Program.Uninitialized_read,
            5 );
        ])
    modes

let interleaved_format_faults () =
  List.iter
    (fun mode ->
      List.iter
        (fun (label, format, arguments, work) ->
          let case =
            Integer_format_fixture.case label format arguments "" work
          in
          let contents = Integer_format_fixture.source case in
          ignore
            (compare_reached_fault ~max_output_bytes:1 mode ~label ~contents
               ~code:"HCIRVM0022" ~kind:Program.Output_limit_exceeded ~bytes:""
               ~work ());
          ignore
            (compare_reached_fault ~max_output_bytes:1
               ~max_output_work:(work - 1) mode
               ~label:(label ^ " work precedes capacity")
               ~contents ~code:"HCIRVM0023"
               ~kind:Program.Output_work_limit_exceeded ~bytes:""
               ~work:(work - 1) ()))
        (Integer_format_fixture.interleaved_faults
       @ Quoted_format_fixture.interleaved_faults))
    modes

let quoted_memory_failures () =
  List.iter
    (fun mode ->
      List.iter
        (fun (label, body, code, work) ->
          let kind =
            match code with
            | "HCIRVM0019" -> Program.Address_out_of_bounds
            | "HCIRVM0012" -> Program.Uninitialized_read
            | "HCIRVM0018" -> Program.Output_invalid_pointer
            | "HCIRVM0008" -> Program.Output_invalid_byte
            | "HCIRVM0025" -> Program.Output_invalid_argument
            | _ -> Alcotest.fail "unknown quoted-memory fixture diagnostic"
          in
          let contents = print ^ body in
          ignore
            (compare_reached_fault mode ~label ~contents ~code ~kind ~bytes:"|"
               ~work:(work + 3) ());
          ignore
            (compare_reached_fault ~max_output_work:(work + 2) mode
               ~label:(label ^ " work precedes failed read")
               ~contents ~code:"HCIRVM0023"
               ~kind:Program.Output_work_limit_exceeded ~bytes:"|"
               ~work:(work + 2) ()))
        (Quoted_format_fixture.memory_failures
       @ Aux_format_fixture.memory_failures);
      List.iter
        (fun (format, work) ->
          let contents =
            print ^ "Print(\"" ^ format ^ "\",9223372036854775807,\"A\");42;"
          in
          ignore
            (compare_reached_fault ~max_output_bytes:1 mode
               ~label:"quoted huge width uses bounded appends" ~contents
               ~code:"HCIRVM0022" ~kind:Program.Output_limit_exceeded ~bytes:""
               ~work ()))
        [ ("%*Q", 7); ("%*q", 8) ])
    modes

let auxiliary_repeat_limits () =
  List.iter
    (fun mode ->
      List.iter
        (fun (label, format, arguments) ->
          let case = Integer_format_fixture.case label format arguments "" 10 in
          ignore
            (compare_reached_fault ~max_output_work:10 mode ~label
               ~contents:(Integer_format_fixture.source case)
               ~code:"HCIRVM0023" ~kind:Program.Output_work_limit_exceeded
               ~bytes:"" ~work:10 ()))
        Aux_format_fixture.empty_repeats;
      let contents = print ^ {|Print("|");Print("%h3c",'A');42;|} in
      ignore
        (compare_success ~max_output_bytes:4 ~max_output_work:17 mode
           ~label:"repeated draft exact limits" ~contents ~bytes:"|AAA" ~work:17
           ~value:(Some 42L) ());
      List.iter
        (fun (byte_limit, work_limit, code, kind, work) ->
          let reached =
            compare_reached_fault ~max_output_bytes:byte_limit
              ~max_output_work:work_limit mode ~label:"repeated atomic failure"
              ~contents ~code ~kind ~bytes:"|" ~work ()
          in
          Alcotest.(check bool)
            "repeat fault belongs to atomic Print" true reached.atomic_output)
        [
          (3, 17, "HCIRVM0022", Program.Output_limit_exceeded, 15);
          (3, 14, "HCIRVM0023", Program.Output_work_limit_exceeded, 14);
          (4, 16, "HCIRVM0023", Program.Output_work_limit_exceeded, 16);
        ])
    modes

let dynamic_formats_and_pointer_offsets () =
  let mutable_arrays =
    print
    ^ "U8 Fmt[4]=\"x%q\";U8 Text[4]=\"x42\";I64 \
       F(){Fmt[2]='d';Print(&Fmt[1],42);Print(\"%s\",&Text[1]);return 42;}F();"
  in
  let signed_and_high =
    print
    ^ "U64 \
       High=0x8000000000000000;Print(\"%d,%d\",-9223372036854775807-1,High);42;"
  in
  List.iter
    (fun mode ->
      ignore
        (compare_success mode
           ~label:"mutable global format and interior strings"
           ~contents:mutable_arrays ~bytes:"4242" ~work:13 ~value:(Some 42L) ());
      ignore
        (compare_success mode ~label:"signed minimum and high U64 bits"
           ~contents:signed_and_high
           ~bytes:"-9223372036854775808,-9223372036854775808" ~work:47
           ~value:(Some 42L) ()))
    modes

let narrow_words_and_full_returns () =
  let contents =
    print
    ^ "U8 Wide(){return 554;}I64 F(){I8 a=255;U8 b=255;I16 c=65535;U16 \
       d=65535;I32 e=4294967295;U32 \
       f=4294967295;Print(\"%d,%d,%d,%d,%d,%d|%d\",a,b,c,d,e,f,Wide());return \
       42;}F();"
  in
  List.iter
    (fun mode ->
      ignore
        (compare_success mode
           ~label:"narrow variadic words and full return bits" ~contents
           ~bytes:"-1,255,-1,65535,-1,4294967295|554" ~work:54 ~value:(Some 42L)
           ()))
    modes

let packed_c_stops_at_first_zero () =
  let contents =
    print ^ putchars ^ "Print(\"%c\",0x00420041);PutChars(0x00420041);42;"
  in
  List.iter
    (fun mode ->
      ignore
        (compare_success mode ~label:"Print c versus PutChars zero handling"
           ~contents ~bytes:"AAB" ~work:11 ~value:(Some 42L) ()))
    modes

let nul_scans_and_failed_reads () =
  List.iter
    (fun mode ->
      ignore
        (compare_success mode ~label:"format and string NUL stop scans"
           ~contents:(print ^ "Print(\"A\\0B\");Print(\"%s\",\"A\\0B\");42;")
           ~bytes:"AA" ~work:9 ~value:(Some 42L) ());
      List.iter
        (fun (label, contents, work) ->
          let native =
            compare_reached_fault mode ~label ~contents ~code:"HCIRVM0019"
              ~kind:Program.Address_out_of_bounds ~bytes:"" ~work ()
          in
          Alcotest.(check bool)
            (label ^ " failed Print remains atomic")
            true native.atomic_output)
        [
          ( "unterminated format read",
            print ^ "U8 Fmt[1];I64 F(){Fmt[0]=65;Print(Fmt);return 42;}F();",
            3 );
          ( "unterminated string read",
            print
            ^ "U8 Text[1];I64 F(){Text[0]=65;Print(\"%s\",Text);return 42;}F();",
            5 );
        ])
    modes

let argument_effects_and_right_to_left () =
  List.iter
    (fun mode ->
      ignore
        (compare_success mode ~label:"right-to-left consumed arguments"
           ~contents:
             (print
            ^ "I64 G=0;I64 Next(){return \
               ++G;}Print(\"%d%d\",Next(),Next());G+40;")
           ~bytes:"21" ~work:7 ~value:(Some 42L) ());
      ignore
        (compare_success mode ~label:"unused tail still keeps source effects"
           ~contents:(print ^ "I64 G=0;Print(\"X\",G=1,G=2);G+41;")
           ~bytes:"X" ~work:3 ~value:(Some 42L) ()))
    modes

let captured_pointer_arguments () =
  List.iter
    (fun mode ->
      ignore
        (compare_success mode
           ~label:"later pointer assignment keeps captured tail"
           ~contents:
             (print
            ^ "U8 Text[3]=\"AB\";I64 F(){U8 \
               *p=Text;Print(\"%s%s\",p=&Text[1],p);return 42;}F();")
           ~bytes:"BAB" ~work:13 ~value:(Some 42L) ());
      ignore
        (compare_success mode ~label:"format assignment keeps captured string"
           ~contents:
             (print
            ^ "U8 Text[3]=\"AB\";U8 Fmt[3]=\"%s\";I64 F(){U8 \
               *p=Text;Print(p=Fmt,p);return 42;}F();")
           ~bytes:"AB" ~work:8 ~value:(Some 42L) ()))
    modes

let nested_output_arguments () =
  List.iter
    (fun mode ->
      ignore
        (compare_success mode
           ~label:"nested Print preserves outer argument staging"
           ~contents:
             (print
            ^ "I64 E(I64 n){Print(\"%d\",n);return \
               n;}Print(\"%d%d\",E(1),E(2));42;")
           ~bytes:"2112" ~work:15 ~value:(Some 42L) ()))
    modes

let non_utf8_bytes () =
  List.iter
    (fun mode ->
      ignore
        (compare_success mode ~label:"raw format string and packed bytes"
           ~contents:(print ^ "Print(\"\\x80%s%c%%\",\"\\xff\",0x81fe);42;")
           ~bytes:"\128\255\254\129%" ~work:18 ~value:(Some 42L) ()))
    modes

let zero_output_charges_work () =
  let contents = print ^ "Print(\"\");Print(\"%c\",0);Print(\"%s\",\"\");42;" in
  List.iter
    (fun mode ->
      ignore
        (compare_success ~max_output_bytes:1 ~max_output_work:9 mode
           ~label:"empty output keeps scan work" ~contents ~bytes:"" ~work:9
           ~value:(Some 42L) ());
      let reached =
        compare_reached_fault ~max_output_bytes:1 ~max_output_work:8 mode
          ~label:"empty output work one below" ~contents ~code:"HCIRVM0023"
          ~kind:Program.Output_work_limit_exceeded ~bytes:"" ~work:8 ()
      in
      Alcotest.(check bool)
        "empty output work failure keeps Print ownership" true
        reached.atomic_output)
    modes

let format_and_argument_faults () =
  let cases =
    [
      ( "unsupported directive",
        print ^ "Print(\"%j\");42;",
        "HCIRVM0024",
        Program.Output_invalid_format,
        2 );
      ( "unsupported plus modifier",
        print ^ "Print(\"%+d\",42);42;",
        "HCIRVM0024",
        Program.Output_invalid_format,
        2 );
      ( "trailing percent",
        print ^ "Print(\"%\");42;",
        "HCIRVM0024",
        Program.Output_invalid_format,
        2 );
      ( "missing argument",
        print ^ "Print(\"%d\");42;",
        "HCIRVM0025",
        Program.Output_invalid_argument,
        2 );
      ( "word where string is required",
        print ^ "Print(\"%s\",42);42;",
        "HCIRVM0025",
        Program.Output_invalid_argument,
        2 );
      ( "pointer where word is required",
        print ^ "Print(\"%d\",\"42\");42;",
        "HCIRVM0025",
        Program.Output_invalid_argument,
        2 );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, contents, code, kind, work) ->
          let fault =
            compare_reached_fault mode ~label ~contents ~code ~kind ~bytes:""
              ~work ()
          in
          Alcotest.(check bool) (label ^ " is atomic") true fault.atomic_output)
        cases)
    modes

let pointer_class_faults_match_interpreter () =
  List.iter
    (fun mode ->
      List.iter
        (fun (label, contents, code, kind) ->
          let fault =
            compare_reached_fault mode ~label ~contents ~code ~kind ~bytes:""
              ~work:3 ()
          in
          Alcotest.(check bool) (label ^ " is atomic") true fault.atomic_output)
        [
          ( "signed byte pointer has invalid byte cells",
            print
            ^ "I8 Text[2];I64 \
               F(){Text[0]=65;Text[1]=0;Print(\"%s\",Text);return 42;}F();",
            "HCIRVM0008",
            Program.Output_invalid_byte );
          ( "wider pointer is not a byte pointer",
            print
            ^ "I16 Text[2];I64 \
               F(){Text[0]=65;Text[1]=0;Print(\"%s\",Text);return 42;}F();",
            "HCIRVM0018",
            Program.Output_invalid_pointer );
          ( "uninitialized U8 byte remains an ordinary read fault",
            print ^ "I64 F(){U8 Text[2];Print(\"%s\",Text);return 42;}F();",
            "HCIRVM0012",
            Program.Uninitialized_read );
        ])
    modes

let atomic_drafts_keep_only_prior_output () =
  let invalid =
    print ^ putchars ^ "Print(\"A\");PutChars('B');Print(\"C%j\");42;"
  in
  let capacity =
    print ^ putchars ^ "Print(\"A\");PutChars('B');Print(\"CD\");42;"
  in
  List.iter
    (fun mode ->
      let invalid_fault =
        compare_reached_fault mode ~label:"invalid format after prior output"
          ~contents:invalid ~code:"HCIRVM0024"
          ~kind:Program.Output_invalid_format ~bytes:"AB" ~work:9 ()
      in
      Alcotest.(check bool)
        "invalid format fault marks an atomic output site" true
        invalid_fault.atomic_output;
      let capacity_fault =
        compare_reached_fault ~max_output_bytes:3 mode
          ~label:"capacity fault after prior output" ~contents:capacity
          ~code:"HCIRVM0022" ~kind:Program.Output_limit_exceeded ~bytes:"AB"
          ~work:9 ()
      in
      Alcotest.(check bool)
        "capacity fault keeps the failed Print draft private" true
        capacity_fault.atomic_output)
    modes

let skipped_and_reached_faults () =
  List.iter
    (fun mode ->
      ignore
        (compare_success mode ~label:"skipped invalid format"
           ~contents:(print ^ "if(0)Print(\"%j\");42;")
           ~bytes:"" ~work:0 ~value:(Some 42L) ());
      let reached =
        compare_reached_fault mode ~label:"reached invalid format"
          ~contents:(print ^ "if(1)Print(\"%j\");42;")
          ~code:"HCIRVM0024" ~kind:Program.Output_invalid_format ~bytes:""
          ~work:2 ()
      in
      Alcotest.(check bool)
        "reached format fault is atomic" true reached.atomic_output)
    modes

let recursive_provider_quotas () =
  let contents =
    print
    ^ "U0 R(I64 n){if(n){R(n-1);return;}Print(\"%d%s%c\",1,\"A\",66);}R(2);42;"
  in
  List.iter
    (fun mode ->
      ignore
        (compare_success ~max_frame_bytes:64 ~max_call_depth:4 mode
           ~label:"recursive exact provider slots" ~contents ~bytes:"1AB"
           ~work:14 ~value:(Some 42L) ());
      let frame =
        compare_reached_fault ~max_frame_bytes:63 ~max_call_depth:4 mode
          ~label:"recursive provider frame one below" ~contents
          ~code:"HCIRVM0011" ~kind:Program.Frame_limit_exceeded ~bytes:""
          ~work:0 ()
      in
      Alcotest.(check bool)
        "provider frame quota faults before formatter work" true
        frame.atomic_output;
      let depth =
        compare_reached_fault ~max_frame_bytes:64 ~max_call_depth:3 mode
          ~label:"recursive provider depth one below" ~contents
          ~code:"HCIRVM0015" ~kind:Program.Call_depth_exceeded ~bytes:"" ~work:0
          ()
      in
      Alcotest.(check bool)
        "provider depth quota faults at the Print site" true depth.atomic_output)
    modes

let result_latch () =
  List.iter
    (fun mode ->
      ignore
        (compare_success mode ~label:"implicit Print preserves the prior result"
           ~contents:(print ^ "42;\"x\";") ~bytes:"x" ~work:3 ~value:(Some 42L)
           ());
      ignore
        (compare_success mode ~label:"explicit U0 Print clears the result"
           ~contents:(print ^ "42;Print(\"x\");")
           ~bytes:"x" ~work:3 ~value:None ()))
    modes

let source_defined_and_mixed_boundaries () =
  List.iter
    (fun mode ->
      ignore
        (compare_success mode ~label:"source-defined Print is an ordinary call"
           ~contents:"I64 Print(U8 *fmt){return 7;}42;Print(\"x\");" ~bytes:""
           ~work:0 ~value:(Some 7L) ());
      ignore
        (compare_success mode ~label:"implicit output selects source Print body"
           ~contents:"I64 G=0;U0 Print(U8 *fmt){G=42;}42;\"x\";if(G!=42)1/0;42;"
           ~bytes:"" ~work:0 ~value:(Some 42L) ());
      let mixed =
        print ^ "U0 Saved(){Print(\"A\");}Saved();U0 Print(U8 *fmt){}42;"
      in
      let report, error = fault mode mixed in
      Alcotest.(check string)
        "mixed provider/source publication remains explicit" "HCBACK0002"
        error.code;
      check_output "mixed provider rejected before entry" ~bytes:"" ~work:0
        report;
      Alcotest.(check bool)
        "mixed provider has no executable image" true
        (Option.is_none (Native_program.image report)
        && Option.is_none (Native_program.native_outcome report)))
    modes

let provider_source_gate () =
  List.iter
    (fun mode ->
      List.iter
        (fun contents ->
          ignore
            (compare_success mode ~label:"Print provider punctuation" ~contents
               ~bytes:"A" ~work:3 ~value:(Some 42L) ()))
        [
          "extern U0 Print(;;U8 *fmt,...);Print(\"A\");42;";
          "extern U0 Print(U8 *fmt,;;...);Print(\"A\");42;";
          "extern U0 Print(U8 *fmt,...) Print(\"A\");42;";
        ];
      List.iter
        (fun contents ->
          let report, error = fault mode contents in
          Alcotest.(check string)
            "invalid Print provider prototype" "HCRUN0001" error.code;
          check_output "invalid Print provider prototype" ~bytes:"" ~work:0
            report;
          Alcotest.(check bool)
            "invalid Print provider has no native image" true
            (Option.is_none (Native_program.image report)))
        [
          "extern I64 Print(U8 *fmt,...);42;";
          "extern U0 Print(I8 *fmt,...);42;";
          "extern U0 Print(U8 *fmt);42;";
          "extern U0 Print(U8 **fmt,...);42;";
        ])
    modes

let exact_independent_limits () =
  let direct = print ^ "Print(\"42\\n\");42;" in
  let nested = print ^ "I64 F(){Print(\"A\");return 42;}F();" in
  List.iter
    (fun mode ->
      let exact, native, fixture =
        compare_success ~max_output_bytes:3 ~max_output_work:7
          ~max_frame_bytes:16 ~max_steps:10 mode ~label:"exact Print limits"
          ~contents:direct ~bytes:"42\n" ~work:7 ~value:(Some 42L) ()
      in
      Alcotest.(check int)
        "maintained explicit Print step count" 10
        native.execution.executed_steps;
      let byte_fault =
        compare_reached_fault ~max_output_bytes:2 ~max_output_work:7
          ~max_frame_bytes:16 ~max_steps:10 mode ~label:"byte one below"
          ~contents:direct ~code:"HCIRVM0022"
          ~kind:Program.Output_limit_exceeded ~bytes:"" ~work:6 ()
      in
      Alcotest.(check bool)
        "byte limit is an atomic Print fault" true byte_fault.atomic_output;
      let work_fault =
        compare_reached_fault ~max_output_bytes:3 ~max_output_work:6
          ~max_frame_bytes:16 ~max_steps:10 mode ~label:"work one below"
          ~contents:direct ~code:"HCIRVM0023"
          ~kind:Program.Output_work_limit_exceeded ~bytes:"" ~work:6 ()
      in
      Alcotest.(check bool)
        "work limit is an atomic Print fault" true work_fault.atomic_output;
      let step_fault =
        compare_reached_fault ~max_output_bytes:3 ~max_output_work:7
          ~max_frame_bytes:16 ~max_steps:9 mode ~label:"step one below"
          ~contents:direct ~code:"HCIRVM0007" ~kind:Program.Step_limit_exceeded
          ~bytes:"42\n" ~work:7 ()
      in
      Alcotest.(check bool)
        "later step limit is not an output fault" false step_fault.atomic_output;
      let frame_fault =
        compare_reached_fault ~max_frame_bytes:15 mode ~label:"frame one below"
          ~contents:direct ~code:"HCIRVM0011" ~kind:Program.Frame_limit_exceeded
          ~bytes:"" ~work:0 ()
      in
      Alcotest.(check bool)
        "Print frame fault retains its sealed site" true
        frame_fault.atomic_output;
      ignore
        (compare_success ~max_call_depth:2 mode ~label:"nested depth exact"
           ~contents:nested ~bytes:"A" ~work:3 ~value:(Some 42L) ());
      ignore
        (compare_reached_fault ~max_call_depth:1 mode
           ~label:"nested depth one below" ~contents:nested ~code:"HCIRVM0015"
           ~kind:Program.Call_depth_exceeded ~bytes:"" ~work:0 ());
      check_output "exact report remains independent" ~bytes:"42\n" ~work:7
        exact;
      let batch_exact, _ =
        batch_success ~max_output_bytes:3 ~max_output_work:7 ~max_frame_bytes:16
          ~max_steps:10 fixture
      in
      check_vm_output "exact checked report remains independent" ~bytes:"42\n"
        ~work:7 batch_exact)
    modes

let compile_result ?max_code_bytes ?max_stack_bytes ?status_abi mode contents =
  let session, config, source = inputs mode contents in
  Native_program.compile ?max_code_bytes ?max_stack_bytes ?status_abi session
    ~config ~source

let compile_image ?max_code_bytes ?max_stack_bytes ?status_abi mode contents =
  compile_result ?max_code_bytes ?max_stack_bytes ?status_abi mode contents
  |> require_ok diagnostics_text
  |> fun checked -> checked.value

let compile_abis_and_exact_private_quotas () =
  let contents = print ^ "Print(\"%d%s%c\",42,\"A\",66);42;" in
  List.iter
    (fun mode ->
      let image = compile_image mode contents in
      let frame = Program.frame_bytes image
      and code = Program.code_bytes image in
      Alcotest.(check bool) "Print formatter uses private frame" true (frame > 0);
      Alcotest.(check bool) "Print formatter emits machine code" true (code > 1);
      let exact =
        compile_image ~max_stack_bytes:frame ~max_code_bytes:code mode contents
      in
      Alcotest.(check int)
        "exact private frame quota" frame
        (Program.frame_bytes exact);
      Alcotest.(check int) "exact code quota" code (Program.code_bytes exact);
      (match compile_result ~max_stack_bytes:(frame - 1) mode contents with
      | Error (error :: _) ->
          Alcotest.(check string)
            "private frame one below" "HCBACK0004" error.code
      | Error [] ->
          Alcotest.fail "private frame one below returned no diagnostic"
      | Ok _ -> Alcotest.fail "private frame one below compiled");
      (match compile_result ~max_code_bytes:(code - 1) mode contents with
      | Error (error :: _) ->
          Alcotest.(check string) "code one below" "HCBACK0005" error.code
      | Error [] -> Alcotest.fail "code one below returned no diagnostic"
      | Ok _ -> Alcotest.fail "code one below compiled");
      List.iter
        (fun abi ->
          let foreign = compile_image ~status_abi:abi mode contents in
          Alcotest.(check bool)
            "compile-only Print ABI image" true
            (Program.status_abi foreign = abi))
        [ Program.Windows_x64; Program.System_v_x64 ])
    modes

let repeated_image_execution_is_fresh () =
  let contents =
    print ^ "I64 G=0;I64 F(){Print(\"%d\",++G);return G+41;}F();"
  in
  List.iter
    (fun mode ->
      let image = compile_image mode contents in
      Alcotest.(check bool)
        "repeated image keeps output metadata" true (Program.has_output image);
      let execute () =
        Runtime.execute_report ~max_steps:10_000 ~max_output_bytes:1
          ~max_output_work:4 image
      in
      List.iter
        (fun runtime_report ->
          Alcotest.(check string)
            "fresh Print output" "1"
            (Runtime.output_bytes runtime_report);
          Alcotest.(check int)
            "fresh Print work" 4
            (Runtime.output_work runtime_report);
          match Runtime.outcome runtime_report with
          | Ok (Program.Completed execution) ->
              check_native_word "fresh Print value" 42L execution.final_value
          | Ok (Program.Fault _) -> Alcotest.fail "fresh Print image faulted"
          | Error message -> Alcotest.fail message)
        [ execute (); execute (); execute () ])
    modes

let () =
  if Runtime.platform () = Runtime.Unsupported then
    failwith "native Print execution tests require x86-64";
  Alcotest.run "native Print execution"
    [
      ( "Print",
        [
          Alcotest.test_case "integer bases widths flags and byte padding"
            `Quick expanded_format_values;
          Alcotest.test_case "expanded format exact quotas and huge widths"
            `Quick expanded_format_quotas;
          Alcotest.test_case "format field faults and complete truncation scans"
            `Quick expanded_format_failures;
          Alcotest.test_case "unmeasured fields preserve interleaved fault work"
            `Quick interleaved_format_faults;
          Alcotest.test_case
            "quoted scans preserve late memory faults and bounds" `Quick
            quoted_memory_failures;
          Alcotest.test_case "dynamic formats and interior pointers" `Quick
            dynamic_formats_and_pointer_offsets;
          Alcotest.test_case
            "auxiliary repeats bound empty work and atomic drafts" `Quick
            auxiliary_repeat_limits;
          Alcotest.test_case "narrow words and full returned bits" `Quick
            narrow_words_and_full_returns;
          Alcotest.test_case "packed c first NUL versus PutChars" `Quick
            packed_c_stops_at_first_zero;
          Alcotest.test_case "format string NUL and charged failed reads" `Quick
            nul_scans_and_failed_reads;
          Alcotest.test_case "right-to-left argument effects and ignored extras"
            `Quick argument_effects_and_right_to_left;
          Alcotest.test_case
            "captured pointer arguments survive later assignment" `Quick
            captured_pointer_arguments;
          Alcotest.test_case "nested output preserves argument staging" `Quick
            nested_output_arguments;
          Alcotest.test_case "format string and packed non-UTF-8 bytes" `Quick
            non_utf8_bytes;
          Alcotest.test_case "zero output exact work and one below" `Quick
            zero_output_charges_work;
          Alcotest.test_case "invalid formats and argument classes" `Quick
            format_and_argument_faults;
          Alcotest.test_case "signed-byte and wider-pointer faults" `Quick
            pointer_class_faults_match_interpreter;
          Alcotest.test_case "atomic failed drafts retain prior output" `Quick
            atomic_drafts_keep_only_prior_output;
          Alcotest.test_case "skipped and reached Print faults" `Quick
            skipped_and_reached_faults;
          Alcotest.test_case "recursive Print frame and depth quotas" `Quick
            recursive_provider_quotas;
          Alcotest.test_case "implicit result latch and explicit U0 clear"
            `Quick result_latch;
          Alcotest.test_case "source-defined and mixed Print boundaries" `Quick
            source_defined_and_mixed_boundaries;
          Alcotest.test_case "Print provider source punctuation" `Quick
            provider_source_gate;
          Alcotest.test_case "exact bytes work steps frame and depth" `Quick
            exact_independent_limits;
          Alcotest.test_case "both ABIs and exact private compile quotas" `Quick
            compile_abis_and_exact_private_quotas;
          Alcotest.test_case "repeated images start with fresh output and data"
            `Quick repeated_image_execution_is_fresh;
        ] );
    ]
