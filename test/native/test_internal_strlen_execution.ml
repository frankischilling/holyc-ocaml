open Holyc_lib
module Program = X86_64_program
module Runtime = Native_program_execution
module VM = Ir_integer_interpreter
module Unit = Holyc_lib__Driver.Integer_unit
module Graph = Ir_block_graph
module Seq = Ir_instruction_sequence

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let internal ?(name = "StrLen") () =
  Printf.sprintf
    "#define IC_STRLEN 0x84\npublic _intern IC_STRLEN I64 %s(U8 *s);" name

let require_ok show = function
  | Ok value -> value
  | Error errors -> Alcotest.fail (show errors)

let diagnostics_text diagnostics =
  diagnostics
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let vm_errors_text errors =
  errors
  |> List.map (fun (error : VM.error) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let source_inputs mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"native-internal-strlen.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> require_ok Fun.id
  in
  (session, config, source)

let batch_fixture mode contents =
  Native_scalar_fixture.compile ~mode ~path:"native-internal-strlen-batch.hc"
    ~contents ()
  |> require_ok diagnostics_text

let batch_success ?(max_call_depth = 128) ~max_steps mode contents =
  let fixture = batch_fixture mode contents in
  let execution =
    Native_scalar_fixture.execute ~max_call_depth ~max_steps fixture
    |> require_ok vm_errors_text
  in
  (fixture, execution)

let batch_fault ~max_steps mode contents =
  let fixture = batch_fixture mode contents in
  match Native_scalar_fixture.execute ~max_steps fixture with
  | Ok _ -> Alcotest.fail "checked intrinsic batch unexpectedly completed"
  | Error [] -> Alcotest.fail "checked intrinsic batch returned no fault"
  | Error (first :: _) -> (fixture, first)

let native_report ?(max_call_depth = 128) ?status_abi ~max_steps mode contents =
  let session, config, source = source_inputs mode contents in
  Native_program.evaluate ~max_call_depth ?status_abi session ~config ~source
    ~max_steps

let native_success ?max_call_depth ?status_abi ~max_steps mode contents =
  let report =
    native_report ?max_call_depth ?status_abi ~max_steps mode contents
  in
  let checked =
    Native_program.outcome report |> require_ok diagnostics_text
    |> fun checked -> checked.value
  in
  Alcotest.(check string)
    "native StrLen emits no output" ""
    (Native_program.output_bytes report);
  Alcotest.(check int)
    "native StrLen charges no output work" 0
    (Native_program.output_work report);
  (report, checked)

let native_fault ~max_steps mode contents =
  let report = native_report ~max_steps mode contents in
  let diagnostics =
    match Native_program.outcome report with
    | Ok _ -> Alcotest.fail "native intrinsic source unexpectedly completed"
    | Error [] -> Alcotest.fail "native intrinsic source returned no diagnostic"
    | Error diagnostics -> diagnostics
  in
  let fault =
    match Native_program.native_outcome report with
    | Some (Program.Fault fault) -> fault
    | Some (Program.Completed _) ->
        Alcotest.fail "native intrinsic reported an error after completion"
    | None -> Alcotest.fail "native intrinsic fault did not reach machine code"
  in
  (report, fault, List.hd diagnostics)

let native_image ?status_abi mode contents =
  let session, config, source = source_inputs mode contents in
  Native_program.compile ?status_abi session ~config ~source
  |> require_ok diagnostics_text
  |> fun checked -> checked.value

let host_abi () =
  match Runtime.platform () with
  | Runtime.Windows_x86_64 -> Program.Windows_x64
  | Runtime.Linux_x86_64 -> Program.System_v_x64
  | Runtime.Unsupported -> Alcotest.fail "native StrLen requires x86-64"

let check_native_word label expected = function
  | Some (word : Program.word) ->
      Alcotest.(check bool) (label ^ " type") true (word.type_ = Program.I64);
      Alcotest.(check int64) (label ^ " bits") expected word.bits
  | None -> Alcotest.fail (label ^ ": missing final value")

let check_vm_word label expected execution =
  match VM.final_value execution with
  | Some word ->
      Alcotest.(check bool) (label ^ " type") true (word.type_ = VM.I64);
      Alcotest.(check int64) (label ^ " bits") expected word.bits
  | None -> Alcotest.fail (label ^ ": missing final value")

let compare ?(max_steps = 10_000) ?(max_call_depth = 128) mode label expected
    contents =
  let fixture, batch = batch_success ~max_call_depth ~max_steps mode contents in
  check_vm_word (label ^ " checked batch") expected batch;
  let report, native =
    native_success ~max_call_depth ~max_steps mode contents
  in
  check_native_word label expected native.execution.final_value;
  Alcotest.(check int)
    (label ^ " checked/native runtime steps")
    (VM.executed_steps batch) native.execution.executed_steps;
  (fixture, report, native)

let instruction_count fixture =
  Unit.entry fixture.Native_scalar_fixture.unit_
  |> Ir_x87_stack.graph |> Graph.blocks
  |> List.fold_left
       (fun count block -> count + Seq.length (Graph.instructions block))
       0

let intrinsic_position_and_site fixture =
  let graph =
    Unit.entry fixture.Native_scalar_fixture.unit_ |> Ir_x87_stack.graph
  in
  let rec search position = function
    | [] -> Alcotest.fail "checked entry has no IC_STRLEN"
    | block :: rest -> (
        let block_id = Graph.block_id block |> Seq.Block_id.to_int in
        let descriptions =
          Graph.instructions block |> Seq.instructions
          |> List.map Seq.description
        in
        let rec in_block offset = function
          | [] -> None
          | (description : Seq.description) :: tail ->
              if description.opcode = Ir_opcode.Ic_strlen then
                Some
                  ( position + offset,
                    block_id,
                    Seq.Instruction_id.to_int description.instruction_id )
              else in_block (offset + 1) tail
        in
        match in_block 0 descriptions with
        | Some found -> found
        | None -> search (position + List.length descriptions) rest)
  in
  search 0 (Graph.blocks graph)

let first_entry_description fixture =
  Unit.entry fixture.Native_scalar_fixture.unit_
  |> Ir_x87_stack.graph |> Graph.blocks
  |> List.find_map (fun block ->
      match Graph.instructions block |> Seq.instructions with
      | first :: _ -> Some (Seq.description first)
      | [] -> None)
  |> function
  | Some description -> description
  | None -> Alcotest.fail "checked entry has no instruction"

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
  | Program.Output_invalid_format -> 13L
  | Program.Output_invalid_argument -> 14L
  | Program.Output_invalid_pointer -> 15L
  | Program.Output_invalid_byte -> 16L

let fault_from_outcome = function
  | Program.Fault fault -> fault
  | Program.Completed _ ->
      Alcotest.fail "native StrLen fault status unexpectedly completed"

let check_fault_origin label (expected : VM.error) (actual : Program.fault) =
  Alcotest.(check int)
    (label ^ " work") expected.executed_steps actual.executed_steps;
  Alcotest.(check (option int))
    (label ^ " instruction") expected.instruction_id
    (Some actual.instruction_id);
  Alcotest.(check (option int))
    (label ^ " block") expected.block_id (Some actual.block_id);
  Alcotest.(check (option string))
    (label ^ " function") expected.function_name actual.function_name;
  Alcotest.(check (option (pair int int)))
    (label ^ " span") (span_range expected.span) (span_range actual.span)

let acceptance_and_call_depth () =
  let cases =
    [
      ("literal", internal () ^ "StrLen(\"abc\")+39;", 42L);
      ("embedded NUL", internal () ^ "StrLen(\"A\\0B\")+41;", 42L);
      ("non-ASCII bytes", internal () ^ "StrLen(\"\\xff\\x80\")+40;", 42L);
      ( "arbitrary internal name",
        internal ~name:"ByteCount" () ^ "ByteCount(\"abc\")+39;",
        42L );
      ( "later macro changes do not replace the retained declaration",
        internal () ^ "\n#define IC_STRLEN 0x85\nStrLen(\"abc\")+39;",
        42L );
      ( "source-defined same-name control",
        "I64 StrLen(U8 *s){return s[0]-23;}StrLen(\"A\");",
        42L );
      ( "automatic array offset and mutation",
        internal () ^ "I64 F(){U8 a[5];a[0]=65;a[1]=66;a[2]=67;a[3]=0;a[4]=68;"
        ^ "a[2]=0;return StrLen(&a[1])+41;}F();",
        42L );
      ( "NUL stops before an unknown local tail",
        internal () ^ "I64 F(){U8 a[3];a[0]=65;a[1]=0;return StrLen(a)+41;}F();",
        42L );
      ( "global array offset and mutation",
        internal () ^ "U8 G[5]={65,66,67,68,0};G[2]=0;StrLen(&G[1])+41;",
        42L );
      ( "nested ordinary calls preserve the result",
        internal () ^ "I64 Inner(U8 *s){return StrLen(s);}"
        ^ "I64 Outer(){return Inner(\"abc\")+39;}Outer();",
        42L );
      ( "saved pointer survives a nested index call",
        internal () ^ "I64 One(){return 1;}"
        ^ "I64 F(){U8 a[4];U8 *p=a;a[0]=65;a[1]=66;a[2]=0;a[3]=67;"
        ^ "return StrLen(&p[One()])+41;}F();",
        42L );
      ( "internal results survive sibling ordinary arguments",
        internal () ^ "I64 One(){return 1;}"
        ^ "I64 Sum(I64 a,I64 b,I64 c){return a+b+c;}"
        ^ "Sum(StrLen(\"abc\"),One(),StrLen(\"AB\")+36);",
        42L );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, expected) ->
          ignore (compare mode label expected source))
        cases;
      ignore
        (compare ~max_call_depth:1 mode
           "intrinsic does not create an ordinary activation" 42L
           (internal () ^ "I64 Only(){return StrLen(\"abc\")+39;}Only();")))
    modes

let memory_faults_match_checked_ir () =
  let cases =
    [
      ( "missing terminator",
        internal () ^ "I64 F(){U8 a[2];a[0]=65;a[1]=66;return StrLen(a);}F();",
        "HCIRVM0019",
        Program.Address_out_of_bounds );
      ( "uninitialized probed byte",
        internal () ^ "I64 F(){U8 a[2];a[0]=65;return StrLen(a);}F();",
        "HCIRVM0012",
        Program.Uninitialized_read );
      ( "one-past start",
        internal () ^ "I64 F(){U8 a[1];a[0]=0;return StrLen(&a[1]);}F();",
        "HCIRVM0019",
        Program.Address_out_of_bounds );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, expected_code, expected_kind) ->
          let _, batch = batch_fault ~max_steps:10_000 mode source in
          let report, native, diagnostic =
            native_fault ~max_steps:10_000 mode source
          in
          Alcotest.(check string)
            (label ^ " checked/native diagnostic")
            batch.code diagnostic.code;
          Alcotest.(check string)
            (label ^ " public fault code")
            expected_code diagnostic.code;
          Alcotest.(check int)
            (label ^ " exact fault work")
            batch.executed_steps native.executed_steps;
          Alcotest.(check bool)
            (label ^ " native fault kind")
            true
            (native.kind = expected_kind);
          Alcotest.(check (option int))
            (label ^ " original fault instruction")
            batch.instruction_id (Some native.instruction_id);
          Alcotest.(check int)
            (label ^ " fault charges no output work")
            0
            (Native_program.output_work report))
        cases)
    modes

let probe_ticks_and_step_limits () =
  let cases =
    [
      ("empty", "", 0L, 0);
      ("one byte", "A", 1L, 1);
      ("three bytes", "ABC", 3L, 3);
      ("embedded NUL", "A\\0BC", 1L, 1);
      ("non-ASCII", "\\xff\\x80", 2L, 2);
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, literal, expected, extra_ticks) ->
          let source =
            Printf.sprintf "%sStrLen(\"%s\");" (internal ()) literal
          in
          let fixture, _, native = compare mode label expected source in
          let base = instruction_count fixture in
          Alcotest.(check int)
            (label
           ^ " first probe shares the intrinsic tick and later probes each \
              tick once")
            (base + extra_ticks) native.execution.executed_steps)
        cases;

      let source = internal () ^ "StrLen(\"ABC\");" in
      let fixture, _, native = compare mode "meter boundary" 3L source in
      let steps = native.execution.executed_steps in
      let image = native.image in
      (match Runtime.execute ~max_steps:steps image |> require_ok Fun.id with
      | Program.Completed execution ->
          check_native_word "exact native StrLen budget" 3L
            execution.final_value;
          Alcotest.(check int)
            "exact native StrLen work" steps execution.executed_steps
      | Program.Fault _ -> Alcotest.fail "exact native StrLen budget faulted");
      (match
         Runtime.execute ~max_steps:(steps - 1) image |> require_ok Fun.id
       with
      | Program.Fault fault when fault.kind = Program.Step_limit_exceeded ->
          Alcotest.(check int)
            "one-below native StrLen stops at the exact limit" (steps - 1)
            fault.executed_steps
      | _ -> Alcotest.fail "one-below native StrLen budget completed");

      let position, block_id, instruction_id =
        intrinsic_position_and_site fixture
      in
      let scan_complete_steps = position + 1 + 3 in
      let in_scan_steps = scan_complete_steps - 1 in
      let _, batch_scan_fault =
        batch_fault ~max_steps:in_scan_steps mode source
      in
      Alcotest.(check string)
        "one-below scan budget faults on the step limit" "HCIRVM0007"
        batch_scan_fault.code;
      Alcotest.(check int)
        "one-below scan consumes exactly its supplied budget" in_scan_steps
        batch_scan_fault.executed_steps;
      Alcotest.(check (option int))
        "one-below scan remains at the IC_STRLEN block" (Some block_id)
        batch_scan_fault.block_id;
      Alcotest.(check (option int))
        "one-below scan remains at the IC_STRLEN instruction"
        (Some instruction_id) batch_scan_fault.instruction_id;
      let _, native_scan_fault, scan_diagnostic =
        native_fault ~max_steps:in_scan_steps mode source
      in
      Alcotest.(check string)
        "native one-below scan diagnostic" "HCIRVM0007" scan_diagnostic.code;
      Alcotest.(check bool)
        "native one-below scan kind" true
        (native_scan_fault.kind = Program.Step_limit_exceeded);
      Alcotest.(check int)
        "native one-below scan consumes exactly its budget" in_scan_steps
        native_scan_fault.executed_steps;
      Alcotest.(check int)
        "native one-below scan remains at the IC_STRLEN block" block_id
        native_scan_fault.block_id;
      Alcotest.(check int)
        "native one-below scan remains at the IC_STRLEN instruction"
        instruction_id native_scan_fault.instruction_id)
    modes

let fault_status_decode_and_site_authentication () =
  let storage_cases =
    [
      ( "bounds status",
        internal () ^ "I64 F(){U8 a[2];a[0]=65;a[1]=66;return StrLen(a);}F();",
        "HCIRVM0019",
        Program.Address_out_of_bounds );
      ( "uninitialized status",
        internal () ^ "I64 F(){U8 a[2];a[0]=65;return StrLen(a);}F();",
        "HCIRVM0012",
        Program.Uninitialized_read );
    ]
  in
  let check_decoded mode (label, source, code, kind) =
    let max_steps = 10_000 in
    let fixture, batch = batch_fault ~max_steps mode source in
    let report, fault, diagnostic = native_fault ~max_steps mode source in
    Alcotest.(check string) (label ^ " diagnostic") code diagnostic.code;
    Alcotest.(check int64)
      (label ^ " kind") (status_kind kind) (status_kind fault.kind);
    check_fault_origin (label ^ " original") batch fault;
    Alcotest.(check (option string))
      (label ^ " source function")
      (Some "F") fault.function_name;
    Alcotest.(check int)
      (label ^ " no output work")
      0
      (Native_program.output_work report);
    let image =
      match Native_program.image report with
      | Some image -> image
      | None -> Alcotest.fail (label ^ ": fault did not retain its native image")
    in
    let site = Int64.of_int (fault.global_position + 1) in
    let decode ?(fault_kind = status_kind kind) ?(fault_site = site)
        ?(executed_steps = Int64.of_int fault.executed_steps) () =
      Program.decode_runtime_status image ~max_steps ~kind:fault_kind
        ~site:fault_site ~executed_steps ~value_site:0L ~bits:0L
    in
    let decoded = decode () |> require_ok Fun.id |> fault_from_outcome in
    Alcotest.(check int64)
      (label ^ " decoded kind") (status_kind fault.kind)
      (status_kind decoded.kind);
    Alcotest.(check int)
      (label ^ " decoded dense site")
      fault.global_position decoded.global_position;
    Alcotest.(check int)
      (label ^ " decoded instruction")
      fault.instruction_id decoded.instruction_id;
    Alcotest.(check int)
      (label ^ " decoded block") fault.block_id decoded.block_id;
    Alcotest.(check (option string))
      (label ^ " decoded function")
      fault.function_name decoded.function_name;
    Alcotest.(check (option (pair int int)))
      (label ^ " decoded span") (span_range fault.span)
      (span_range decoded.span);
    Alcotest.(check int)
      (label ^ " decoded work") fault.executed_steps decoded.executed_steps;
    Alcotest.(check bool)
      (label ^ " zero counter rejected")
      true
      (decode ~executed_steps:0L () |> Result.is_error);
    Alcotest.(check bool)
      (label ^ " excess counter rejected")
      true
      (decode ~executed_steps:(Int64.of_int (max_steps + 1)) ()
      |> Result.is_error);
    if kind = Program.Uninitialized_read then (
      Alcotest.(check bool)
        (label ^ " first dense site is a non-load call start")
        true
        ((first_entry_description fixture).opcode = Ir_opcode.Ic_call_start);
      Alcotest.(check bool)
        (label ^ " non-load site rejected")
        true
        (decode ~fault_site:1L () |> Result.is_error));
    List.iter
      (fun (wrong_kind, wrong_label) ->
        Alcotest.(check bool)
          (label ^ " rejects " ^ wrong_label)
          true
          (decode ~fault_kind:wrong_kind () |> Result.is_error))
      [
        (4L, "call-depth status");
        (8L, "index-scale status");
        (9L, "index-addition status");
        (11L, "output-limit status");
        (12L, "output-work status");
      ]
  in
  List.iter
    (fun mode ->
      List.iter (check_decoded mode) storage_cases;
      let source = internal () ^ "StrLen(\"ABC\");" in
      let fixture = batch_fixture mode source in
      let position, block_id, instruction_id =
        intrinsic_position_and_site fixture
      in
      let max_steps = position + 3 in
      let _, batch = batch_fault ~max_steps mode source in
      let report, fault, diagnostic = native_fault ~max_steps mode source in
      Alcotest.(check string)
        "step status diagnostic" "HCIRVM0007" diagnostic.code;
      Alcotest.(check int64)
        "step status kind"
        (status_kind Program.Step_limit_exceeded)
        (status_kind fault.kind);
      Alcotest.(check int)
        "step status intrinsic instruction" instruction_id fault.instruction_id;
      Alcotest.(check int) "step status intrinsic block" block_id fault.block_id;
      check_fault_origin "step status original" batch fault;
      Alcotest.(check int)
        "step status no output work" 0
        (Native_program.output_work report);
      let image =
        match Native_program.image report with
        | Some image -> image
        | None -> Alcotest.fail "step fault did not retain its native image"
      in
      let site = Int64.of_int (fault.global_position + 1) in
      let decode ?(fault_kind = 3L) ?(fault_site = site)
          ?(executed_steps = Int64.of_int fault.executed_steps) () =
        Program.decode_runtime_status image ~max_steps ~kind:fault_kind
          ~site:fault_site ~executed_steps ~value_site:0L ~bits:0L
      in
      let decoded = decode () |> require_ok Fun.id |> fault_from_outcome in
      Alcotest.(check int64)
        "step decoded kind" (status_kind fault.kind) (status_kind decoded.kind);
      Alcotest.(check int)
        "step decoded dense site" fault.global_position decoded.global_position;
      Alcotest.(check int)
        "step decoded instruction" fault.instruction_id decoded.instruction_id;
      Alcotest.(check int) "step decoded block" fault.block_id decoded.block_id;
      Alcotest.(check (option string))
        "step decoded function" fault.function_name decoded.function_name;
      Alcotest.(check (option (pair int int)))
        "step decoded span" (span_range fault.span) (span_range decoded.span);
      Alcotest.(check int)
        "step decoded work" fault.executed_steps decoded.executed_steps;
      Alcotest.(check bool)
        "step zero counter rejected" true
        (decode ~executed_steps:0L () |> Result.is_error);
      Alcotest.(check bool)
        "step excess counter rejected" true
        (decode ~executed_steps:(Int64.of_int (max_steps + 1)) ()
        |> Result.is_error);
      List.iter
        (fun (wrong_kind, wrong_label) ->
          Alcotest.(check bool)
            ("step intrinsic rejects " ^ wrong_label)
            true
            (decode ~fault_kind:wrong_kind () |> Result.is_error))
        [
          (4L, "call-depth status");
          (8L, "index-scale status");
          (9L, "index-addition status");
          (11L, "output-limit status");
          (12L, "output-work status");
        ])
    modes

let abi_compile_and_fresh_images () =
  let source =
    internal () ^ "U8 G[4]={65,255,66,0};"
    ^ "I64 F(){G[1]++;return StrLen(G)+41;}F();"
  in
  List.iter
    (fun mode ->
      List.iter
        (fun abi ->
          let image = native_image ~status_abi:abi mode source in
          Alcotest.(check bool)
            "compile-only intrinsic ABI image" true
            (Program.status_abi image = abi))
        [ Program.Windows_x64; Program.System_v_x64 ];

      let fixture, batch = batch_success ~max_steps:10_000 mode source in
      let expected_steps = VM.executed_steps batch in
      ignore fixture;
      let image = native_image ~status_abi:(host_abi ()) mode source in
      Alcotest.(check bool)
        "executable intrinsic image uses the host ABI" true
        (Program.status_abi image = host_abi ());
      for run = 1 to 3 do
        match
          Runtime.execute ~max_steps:expected_steps image |> require_ok Fun.id
        with
        | Program.Completed execution ->
            check_native_word
              (Printf.sprintf "fresh intrinsic image run %d" run)
              42L execution.final_value;
            Alcotest.(check int)
              "fresh intrinsic image work" expected_steps
              execution.executed_steps
        | Program.Fault _ -> Alcotest.fail "fresh intrinsic image faulted"
      done)
    modes

let prior_output_and_probe_fault_priority () =
  List.iter
    (fun mode ->
      let declarations =
        internal () ^ "extern U0 Print(U8 *fmt,...);U8 G[1]={65};"
      in
      let prefix = declarations ^ "Print(\"kept\");" in
      let control = native_report ~max_steps:10_000 mode (prefix ^ "42;") in
      ignore (Native_program.outcome control |> require_ok diagnostics_text);
      Alcotest.(check string)
        "control publishes the earlier call" "kept"
        (Native_program.output_bytes control);
      let expected_work = Native_program.output_work control in
      let source = prefix ^ "StrLen(G);" in
      let _, bounds, _ = native_fault ~max_steps:10_000 mode source in
      List.iter
        (fun (limit, expected_code) ->
          let native, fault, diagnostic =
            native_fault ~max_steps:limit mode source
          in
          Alcotest.(check string)
            "budget exhaustion precedes an unreached bounds probe" expected_code
            diagnostic.code;
          Alcotest.(check int)
            "both faults retain the scan instruction" bounds.instruction_id
            fault.instruction_id;
          Alcotest.(check string)
            "native scan fault retains earlier output" "kept"
            (Native_program.output_bytes native);
          Alcotest.(check int)
            "native scan consumes no output work" expected_work
            (Native_program.output_work native);
          let session, config, source = source_inputs mode source in
          let interpreted =
            run_integer_program_report session ~config ~source ~max_steps:limit
          in
          (match integer_program_report_outcome interpreted with
          | Ok _ -> Alcotest.fail "public source scan unexpectedly completed"
          | Error [] -> Alcotest.fail "public source scan lost its fault"
          | Error (first :: _) ->
              Alcotest.(check string)
                "public source probe fault priority" expected_code first.code);
          Alcotest.(check string)
            "public source scan fault retains earlier output" "kept"
            (integer_program_report_output_bytes interpreted);
          Alcotest.(check int)
            "public source scan consumes no output work" expected_work
            (integer_program_report_output_work interpreted))
        [ (10_000, "HCIRVM0019"); (bounds.executed_steps - 1, "HCIRVM0007") ])
    modes

let () =
  match Runtime.platform () with
  | Runtime.Unsupported ->
      Alcotest.fail "native internal StrLen tests require x86-64"
  | Runtime.Windows_x86_64 | Runtime.Linux_x86_64 ->
      Alcotest.run "holyc native internal StrLen"
        [
          ( "internal StrLen",
            [
              Alcotest.test_case "public source, storage and call depth" `Quick
                acceptance_and_call_depth;
              Alcotest.test_case "owned memory faults match checked IR" `Quick
                memory_faults_match_checked_ir;
              Alcotest.test_case "byte probes and exact step limits" `Quick
                probe_ticks_and_step_limits;
              Alcotest.test_case "fault status decode and site authentication"
                `Quick fault_status_decode_and_site_authentication;
              Alcotest.test_case "ABI compilation and fresh images" `Quick
                abi_compile_and_fresh_images;
              Alcotest.test_case "prior output and probe fault priority" `Quick
                prior_output_and_probe_fault_priority;
            ] );
        ]
