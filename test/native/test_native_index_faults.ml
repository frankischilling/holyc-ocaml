open Holyc_lib
module Program = X86_64_program
module Runtime = Native_program_execution
module VM = Ir_integer_interpreter

let require_ok show = function
  | Ok value -> value
  | Error errors -> Alcotest.fail (show errors)

let diagnostics_text diagnostics =
  diagnostics
  |> List.map (fun (diagnostic : Diagnostic.t) ->
      diagnostic.code ^ ": " ^ diagnostic.message)
  |> String.concat "; "

let source_inputs ~mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-index-fault.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id
  in
  (session, config, source)

let batch_fault ~mode contents =
  let fixture =
    Native_scalar_fixture.compile ~mode ~path:"native-index-fault.hc" ~contents
      ()
    |> require_ok diagnostics_text
  in
  match Native_scalar_fixture.execute ~max_steps:10000 fixture with
  | Error (fault :: _) -> fault
  | Error [] -> Alcotest.fail "checked batch failed without a diagnostic"
  | Ok _ -> Alcotest.fail "checked batch unexpectedly accepted the fault case"

let first_diagnostic = function
  | Error (diagnostic :: _) -> diagnostic
  | Error [] -> Alcotest.fail "source execution failed without a diagnostic"
  | Ok _ -> Alcotest.fail "source execution unexpectedly completed"

let span_range = Option.map (fun (span : Span.t) -> (span.start, span.stop))

let status_kind = function
  | Program.Division_by_zero -> 1L
  | Program.Signed_division_overflow -> 2L
  | Program.Step_limit_exceeded -> 3L
  | Program.Call_depth_exceeded -> 4L
  | Program.Frame_limit_exceeded -> 5L
  | Program.Native_stack_limit_exceeded -> 6L
  | Program.Uninitialized_read -> 7L
  | Program.Index_scale_overflow -> 8L
  | Program.Index_addition_overflow -> 9L
  | Program.Address_out_of_bounds -> 10L
  | Program.Output_limit_exceeded -> 11L
  | Program.Output_work_limit_exceeded -> 12L

let check_fault_site label (expected : Program.fault) (actual : Program.fault) =
  Alcotest.(check int64)
    (label ^ " kind")
    (status_kind expected.kind)
    (status_kind actual.kind);
  Alcotest.(check int)
    (label ^ " consumed steps")
    expected.executed_steps actual.executed_steps;
  Alcotest.(check int)
    (label ^ " dense site") expected.global_position actual.global_position;
  Alcotest.(check int)
    (label ^ " instruction") expected.instruction_id actual.instruction_id;
  Alcotest.(check int) (label ^ " block") expected.block_id actual.block_id;
  Alcotest.(check (option string))
    (label ^ " function") expected.function_name actual.function_name;
  Alcotest.(check (option (pair int int)))
    (label ^ " span") (span_range expected.span) (span_range actual.span)

let fault_from_outcome = function
  | Program.Fault fault -> fault
  | Program.Completed _ ->
      Alcotest.fail "native fault case unexpectedly completed"

let compare_fault ~mode ~label ~code ~kind contents =
  let oracle = batch_fault ~mode contents in
  Alcotest.(check string) (label ^ " independent diagnostic") code oracle.code;
  let session, config, source = source_inputs ~mode contents in
  let public_error =
    run_integer_program session ~config ~source ~max_steps:10000
    |> first_diagnostic
  in
  Alcotest.(check string)
    (label ^ " fresh source diagnostic")
    code public_error.code;
  let session, config, source = source_inputs ~mode contents in
  let report =
    Native_program.evaluate session ~config ~source ~max_steps:10000
  in
  let diagnostic = Native_program.outcome report |> first_diagnostic in
  Alcotest.(check string) (label ^ " native diagnostic") code diagnostic.code;
  let fault =
    match Native_program.native_outcome report with
    | Some outcome -> fault_from_outcome outcome
    | None ->
        Alcotest.failf "%s failed before native entry: %s" label
          diagnostic.message
  in
  Alcotest.(check int64)
    (label ^ " exact native fault kind")
    (status_kind kind) (status_kind fault.kind);
  Alcotest.(check int)
    (label ^ " exact interpreter work")
    oracle.executed_steps fault.executed_steps;
  Alcotest.(check (option int))
    (label ^ " original instruction")
    oracle.instruction_id (Some fault.instruction_id);
  Alcotest.(check (option int))
    (label ^ " original block")
    oracle.block_id (Some fault.block_id);
  Alcotest.(check (option string))
    (label ^ " original function")
    oracle.function_name fault.function_name;
  Alcotest.(check (option (pair int int)))
    (label ^ " original span") (span_range oracle.span) (span_range fault.span);
  let image =
    match Native_program.image report with
    | Some image -> image
    | None -> Alcotest.fail "executed fault did not retain its compiled image"
  in
  let decode ~executed_steps =
    Program.decode_runtime_status image ~max_steps:10000
      ~kind:(status_kind kind)
      ~site:(Int64.of_int (fault.global_position + 1))
      ~executed_steps ~value_site:0L ~bits:0L
  in
  let decoded =
    decode ~executed_steps:(Int64.of_int fault.executed_steps)
    |> require_ok Fun.id |> fault_from_outcome
  in
  check_fault_site (label ^ " decoded status") fault decoded;
  Alcotest.(check bool)
    (label ^ " zero-step status rejected")
    true
    (decode ~executed_steps:0L |> Result.is_error);
  let exact =
    Runtime.execute ~max_steps:fault.executed_steps image
    |> require_ok Fun.id |> fault_from_outcome
  in
  check_fault_site (label ^ " exact fault budget") fault exact;
  if fault.executed_steps <= 1 then
    Alcotest.fail "array fault fixture did not reach its function body";
  let before =
    Runtime.execute ~max_steps:(fault.executed_steps - 1) image
    |> require_ok Fun.id |> fault_from_outcome
  in
  Alcotest.(check int64)
    (label ^ " one-below is the step fault")
    (status_kind Program.Step_limit_exceeded)
    (status_kind before.kind);
  Alcotest.(check int)
    (label ^ " one-below consumes exactly its budget")
    (fault.executed_steps - 1) before.executed_steps;
  let repeated =
    Runtime.execute ~max_steps:10000 image
    |> require_ok Fun.id |> fault_from_outcome
  in
  check_fault_site (label ^ " repeated image") fault repeated

let cases =
  [
    ( "unsigned index exceeds signed address range even with byte elements",
      "HCIRVM0020",
      Program.Index_scale_overflow,
      "U8 F(){U8 a[2];U64 i=-1;return a[i];}F();" );
    ( "positive signed scaling overflow",
      "HCIRVM0020",
      Program.Index_scale_overflow,
      "I64 F(){I64 a[2];return a[0x7fffffffffffffff];}F();" );
    ( "negative signed scaling overflow",
      "HCIRVM0020",
      Program.Index_scale_overflow,
      "I64 F(){I64 a[2];return a[-9223372036854775808];}F();" );
    ( "positive addition overflow after valid scaling",
      "HCIRVM0020",
      Program.Index_addition_overflow,
      "I64 F(){I64 a[2][3];return a[384307168202282325][2];}F();" );
    ( "negative addition overflow after valid scaling",
      "HCIRVM0020",
      Program.Index_addition_overflow,
      "U8 F(){U8 a[1][1];return a[-9223372036854775808][-1];}F();" );
    ( "bounds precede initialization at a final dereference",
      "HCIRVM0019",
      Program.Address_out_of_bounds,
      "I64 F(){I64 a[2],neighbor=42;return a[2];}F();" );
    ( "negative writes cannot reach a neighboring local",
      "HCIRVM0019",
      Program.Address_out_of_bounds,
      "I64 F(){I64 neighbor=42,a[2];a[-1]=7;return neighbor;}F();" );
    ( "materialization rejects past one-past",
      "HCIRVM0019",
      Program.Address_out_of_bounds,
      "I64 F(){I64 a[2];I64 *p=&a[3];return 42;}F();" );
    ( "one-past read faults after successful materialization",
      "HCIRVM0019",
      Program.Address_out_of_bounds,
      "I64 F(){I64 a[2];I64 *p=&a[2];return *p;}F();" );
    ( "one-past write faults after successful materialization",
      "HCIRVM0019",
      Program.Address_out_of_bounds,
      "I64 F(){I64 a[2];I64 *p=&a[2];*p=42;return 42;}F();" );
    ( "one-past compound update checks bounds before initialization",
      "HCIRVM0019",
      Program.Address_out_of_bounds,
      "I64 F(){I64 a[2];I64 *p=&a[2];return (*p)++;}F();" );
    ( "scalar references retain only their own object extent",
      "HCIRVM0019",
      Program.Address_out_of_bounds,
      "I64 F(){I64 n=42,neighbor=7;I64 *p=&n;return p[1];}F();" );
    ( "one initialized element does not initialize its sibling",
      "HCIRVM0012",
      Program.Uninitialized_read,
      "I64 F(){I64 a[2];a[0]=42;return a[1];}F();" );
    ( "RHS division fault precedes final destination bounds",
      "HCIRVM0009",
      Program.Division_by_zero,
      "I64 F(){I64 a[2];a[2]=1/0;return 42;}F();" );
    ( "RHS unknown read precedes final destination bounds",
      "HCIRVM0012",
      Program.Uninitialized_read,
      "I64 F(){I64 a[2],rhs;a[2]=rhs;return 42;}F();" );
    ( "index scaling overflow precedes RHS division",
      "HCIRVM0020",
      Program.Index_scale_overflow,
      "I64 F(){I64 a[2];a[0x7fffffffffffffff]=1/0;return 42;}F();" );
    ( "unknown base is read before the index expression",
      "HCIRVM0012",
      Program.Uninitialized_read,
      "I64 F(){I64 *p;return p[1/0];}F();" );
    ( "compound read follows an RHS write to a different element",
      "HCIRVM0012",
      Program.Uninitialized_read,
      "I64 Set(I64 *p){*p=40;return 2;}I64 F(){I64 \
       a[2];a[0]+=Set(&a[1]);return 42;}F();" );
  ]

let () =
  match Runtime.platform () with
  | Runtime.Unsupported ->
      Alcotest.fail "native index fault tests require Windows or Linux x86-64"
  | Runtime.Windows_x86_64 | Runtime.Linux_x86_64 ->
      Alcotest.run "holyc native index faults"
        [
          ( "fault phases and status",
            List.map
              (fun (label, code, kind, contents) ->
                Alcotest.test_case label `Quick (fun () ->
                    List.iter
                      (fun mode ->
                        compare_fault ~mode ~label ~code ~kind contents)
                      [ Preprocessor.Jit; Preprocessor.Aot ]))
              cases );
        ]
