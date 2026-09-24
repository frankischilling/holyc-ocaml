open Holyc_lib
module VM = Ir_integer_interpreter
module Graph = Ir_block_graph
module Seq = Ir_instruction_sequence

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let internal ?(name = "StrLen") () =
  Printf.sprintf
    "#define IC_STRLEN 0x84\npublic _intern IC_STRLEN I64 %s(U8 *s);" name

let inputs mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"internal-strlen.hc" ~contents
  in
  let config =
    match Preprocessor.Config.create ~compilation_mode:mode () with
    | Ok config -> config
    | Error message -> Alcotest.fail message
  in
  (session, config, source)

let run ?(max_steps = 10_000) ?(max_call_depth = 16) mode contents =
  let session, config, source = inputs mode contents in
  run_integer_program_report ~max_call_depth session ~config ~source ~max_steps

let diagnostics_text diagnostics =
  diagnostics
  |> List.map (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
  |> String.concat "; "

let success ?max_steps ?max_call_depth mode contents =
  let report = run ?max_steps ?max_call_depth mode contents in
  let execution =
    match integer_program_report_outcome report with
    | Ok checked -> checked.value
    | Error diagnostics -> Alcotest.fail (diagnostics_text diagnostics)
  in
  Alcotest.(check string)
    "internal StrLen emits no output" ""
    (integer_program_report_output_bytes report);
  Alcotest.(check int)
    "internal StrLen charges no output work" 0
    (integer_program_report_output_work report);
  (report, execution)

let first_error ?max_steps ?max_call_depth mode contents =
  let report = run ?max_steps ?max_call_depth mode contents in
  match integer_program_report_outcome report with
  | Ok _ -> Alcotest.fail "source unexpectedly completed"
  | Error [] -> Alcotest.fail "source failed without a diagnostic"
  | Error (first :: _) -> (report, first)

let check_word label expected execution =
  match VM.final_value execution with
  | Some word ->
      Alcotest.(check bool) (label ^ " type") true (word.type_ = VM.I64);
      Alcotest.(check int64) (label ^ " bits") expected word.bits
  | None -> Alcotest.fail (label ^ ": missing final value")

let check_value ?max_steps ?max_call_depth mode label expected contents =
  let report, execution = success ?max_steps ?max_call_depth mode contents in
  check_word label expected execution;
  (report, execution)

let contains text needle =
  let text_length = String.length text
  and needle_length = String.length needle in
  let rec loop index =
    index + needle_length <= text_length
    && (String.sub text index needle_length = needle || loop (index + 1))
  in
  needle_length = 0 || loop 0

let program report =
  match integer_program_report_program report with
  | Some program -> program
  | None -> Alcotest.fail "successful source did not retain its checked program"

let instruction_count report =
  program report |> integer_program_entry |> Ir_x87_stack.graph |> Graph.blocks
  |> List.fold_left
       (fun count block -> count + Seq.length (Graph.instructions block))
       0

let entry_opcodes report =
  program report |> integer_program_entry |> Ir_x87_stack.graph |> Graph.blocks
  |> List.concat_map (fun block ->
      Graph.instructions block |> Seq.instructions
      |> List.map (fun instruction -> (Seq.description instruction).opcode))

let entry_descriptions report =
  program report |> integer_program_entry |> Ir_x87_stack.graph |> Graph.blocks
  |> List.concat_map (fun block ->
      Graph.instructions block |> Seq.instructions |> List.map Seq.description)

let target_and_name_selection () =
  List.iter
    (fun mode ->
      let canonical = internal () ^ "StrLen(\"abc\");" in
      let report, execution =
        check_value mode "canonical internal target" 3L canonical
      in
      let lowered = integer_program_human (program report) in
      Alcotest.(check bool)
        "0x84 lowers to the pinned intrinsic" true
        (contains lowered "IC_STRLEN");
      let opcodes = entry_opcodes report in
      Alcotest.(check bool)
        "internal source retains its call-start scope" true
        (List.mem Ir_opcode.Ic_call_start opcodes);
      Alcotest.(check bool)
        "internal source retains its call-end scope" true
        (List.mem Ir_opcode.Ic_call_end opcodes);
      Alcotest.(check bool)
        "internal source emits no ordinary call opcode" false
        (List.mem Ir_opcode.Ic_call opcodes);
      Alcotest.(check bool)
        "internal source emits no JIT extern call opcode" false
        (List.mem Ir_opcode.Ic_call_indirect2 opcodes);
      Alcotest.(check bool)
        "internal source emits no AOT extern call opcode" false
        (List.mem Ir_opcode.Ic_call_extern opcodes);
      Alcotest.(check bool)
        "internal call has no ordinary argument cleanup" false
        (List.mem Ir_opcode.Ic_add_rsp opcodes
        || List.mem Ir_opcode.Ic_add_rsp1 opcodes);
      Alcotest.(check bool)
        "internal argument is never pushed through ICF_PUSH_RES" true
        (entry_descriptions report
        |> List.for_all (fun (description : Seq.description) ->
            Int64.logand description.flags 0x000002000L = 0L));
      check_word "canonical result remains available" 3L execution;

      ignore
        (check_value mode "arbitrary internal function name" 42L
           (internal ~name:"ByteCount" () ^ "ByteCount(\"abc\")+39;"));

      let source_defined = "I64 StrLen(U8 *s){return s[0]-23;}StrLen(\"A\");" in
      let report, _ =
        check_value mode "source-defined same-name control" 42L source_defined
      in
      Alcotest.(check bool)
        "ordinary same-name function is not rewritten to the intrinsic" false
        (contains (integer_program_human (program report)) "IC_STRLEN"))
    modes

let literals_storage_and_nested_calls () =
  let cases =
    [
      ("literal length", internal () ^ "StrLen(\"abc\")+39;", 42L);
      ("embedded NUL stops the scan", internal () ^ "StrLen(\"A\\0B\")+41;", 42L);
      ( "non-ASCII bytes count as bytes",
        internal () ^ "StrLen(\"\\xff\\x80\")+40;",
        42L );
      ( "interior literal pointer",
        internal () ^ "I64 F(){U8 *s=\"abcd\";return StrLen(&s[2])+40;}F();",
        42L );
      ( "automatic byte array offset and mutation",
        internal () ^ "I64 F(){U8 a[5];a[0]=65;a[1]=66;a[2]=67;a[3]=0;a[4]=68;"
        ^ "a[2]=0;return StrLen(&a[1])+41;}F();",
        42L );
      ( "NUL stops before an unknown local tail",
        internal () ^ "I64 F(){U8 a[3];a[0]=65;a[1]=0;return StrLen(a)+41;}F();",
        42L );
      ( "global byte array offset and mutation",
        internal () ^ "U8 G[5]={65,66,67,68,0};G[2]=0;StrLen(&G[1])+41;",
        42L );
      ( "nested ordinary calls preserve the intrinsic result",
        internal () ^ "I64 Inner(U8 *s){return StrLen(s);}"
        ^ "I64 Outer(){return Inner(\"abc\")+39;}Outer();",
        42L );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, expected) ->
          ignore (check_value mode label expected source))
        cases;
      ignore
        (check_value ~max_call_depth:1 mode
           "intrinsic consumes no additional call activation" 42L
           (internal () ^ "I64 Only(){return StrLen(\"abc\")+39;}Only();"));
      let _, depth_error =
        first_error ~max_call_depth:1 mode
          (internal () ^ "I64 Inner(){return StrLen(\"abc\");}"
         ^ "I64 Outer(){return Inner()+39;}Outer();")
      in
      Alcotest.(check string)
        "ordinary nested call still consumes the configured depth" "HCIRVM0015"
        depth_error.code)
    modes

let memory_faults () =
  let cases =
    [
      ( "missing terminator",
        internal () ^ "I64 F(){U8 a[2];a[0]=65;a[1]=66;return StrLen(a);}F();",
        "HCIRVM0019" );
      ( "uninitialized probed byte",
        internal () ^ "I64 F(){U8 a[2];a[0]=65;return StrLen(a);}F();",
        "HCIRVM0012" );
      ( "one-past start",
        internal () ^ "I64 F(){U8 a[1];a[0]=0;return StrLen(&a[1]);}F();",
        "HCIRVM0019" );
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, code) ->
          let report, error = first_error mode source in
          Alcotest.(check string) label code error.code;
          Alcotest.(check string)
            (label ^ " emits no bytes before the fault")
            ""
            (integer_program_report_output_bytes report);
          Alcotest.(check int)
            (label ^ " charges no output work before the fault")
            0
            (integer_program_report_output_work report))
        cases)
    modes

let rejected_targets_and_signatures () =
  let rejected =
    [
      ("unsupported internal target", "_intern 0x83 I64 Bad(U8 *s);Bad(\"A\");");
      ( "changed IC_STRLEN macro target",
        "#define IC_STRLEN 0x85\n\
         public _intern IC_STRLEN I64 Bad(U8 *s);Bad(\"A\");" );
      ( "unbound IC_STRLEN target",
        "public _intern IC_STRLEN I64 Bad(U8 *s);Bad(\"A\");" );
      ("wrong return type", "_intern 0x84 U64 Bad(U8 *s);Bad(\"A\");");
      ("wrong parameter type", "_intern 0x84 I64 Bad(I64 n);Bad(0);");
      ("wrong arity", "_intern 0x84 I64 Bad(U8 *s,I64 n);Bad(\"A\",0);");
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source) ->
          let report, _ = first_error mode source in
          Alcotest.(check bool)
            (label ^ " publishes no executable program")
            true
            (Option.is_none (integer_program_report_program report)))
        rejected)
    modes

let byte_probe_meter_and_step_limit () =
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
        (fun (label, literal, expected_value, extra_probe_ticks) ->
          let source =
            Printf.sprintf "%sStrLen(\"%s\");" (internal ()) literal
          in
          let report, execution =
            check_value mode label expected_value source
          in
          let base = instruction_count report in
          Alcotest.(check int)
            (label
           ^ " uses the normal intrinsic tick for the first probe and one tick \
              for each later probe")
            (base + extra_probe_ticks)
            (VM.executed_steps execution))
        cases;

      let source = internal () ^ "StrLen(\"ABC\");" in
      let _, measured = success mode source in
      let steps = VM.executed_steps measured in
      ignore
        (check_value ~max_steps:steps mode "exact StrLen step budget" 3L source);
      let _, error = first_error ~max_steps:(steps - 1) mode source in
      Alcotest.(check string)
        "one-below StrLen step budget" "HCIRVM0007" error.code)
    modes

let tests =
  [
    Alcotest.test_case "internal target and name selection" `Quick
      target_and_name_selection;
    Alcotest.test_case "literals storage and nested calls" `Quick
      literals_storage_and_nested_calls;
    Alcotest.test_case "owned memory faults" `Quick memory_faults;
    Alcotest.test_case "rejected target and signatures" `Quick
      rejected_targets_and_signatures;
    Alcotest.test_case "byte probes and exact step limits" `Quick
      byte_probe_meter_and_step_limit;
  ]
