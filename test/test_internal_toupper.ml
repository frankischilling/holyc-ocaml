open Holyc_lib
module VM = Ir_integer_interpreter

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let declaration =
  "#define IC_TOUPPER 0x1e\npublic _intern IC_TOUPPER I64 ToUpper(U8 ch);"

let run ?(max_steps = 100_000) mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"internal-toupper.hc" ~contents
  in
  let config =
    match Preprocessor.Config.create ~compilation_mode:mode () with
    | Ok config -> config
    | Error message -> Alcotest.fail message
  in
  run_integer_program_report session ~config ~source ~max_steps

let success mode source =
  let report = run mode source in
  match integer_program_report_outcome report with
  | Ok checked -> (report, checked.value)
  | Error diagnostics ->
      diagnostics
      |> List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
      |> String.concat "; " |> Alcotest.fail

let word label expected execution =
  match VM.final_value execution with
  | Some actual ->
      Alcotest.(check bool) (label ^ " I64") true (actual.type_ = VM.I64);
      Alcotest.(check int64) label expected actual.bits
  | None -> Alcotest.fail (label ^ " has no final value")

let all_bytes =
  declaration
  ^ "I64 Check(){I64 i,errors=0;U8 *lower=\"abcdefghijklmnopqrstuvwxyz\";U8 \
     *upper=\"ABCDEFGHIJKLMNOPQRSTUVWXYZ\";for(i=0;i<256;i++){if(i>=97&&i<=122){if(ToUpper(lower[i-97])!=upper[i-97])errors++;}else \
     if(ToUpper(i)!=i)errors++;}return errors;}Check();"

let cases =
  [
    ("all byte values", all_bytes, 0L);
    ("lower boundary", declaration ^ "ToUpper(97);", 65L);
    ("upper boundary", declaration ^ "ToUpper(122);", 90L);
    ("preceding punctuation", declaration ^ "ToUpper(96);", 96L);
    ("following punctuation", declaration ^ "ToUpper(123);", 123L);
    ("negative word", declaration ^ "ToUpper(-1);", -1L);
    ("high byte does not truncate", declaration ^ "ToUpper(0x161);", 353L);
    ( "signed minimum",
      declaration ^ "ToUpper(0x8000000000000061);",
      Int64.add Int64.min_int 97L );
    ( "unsigned high word",
      declaration ^ "U64 ch=0xffffffffffffffff;ToUpper(ch);",
      -1L );
    ( "computed narrow result",
      declaration ^ "U8 Bits(){return 0x161;}ToUpper(Bits());",
      353L );
    ("stored byte normalizes", declaration ^ "U8 ch=0x161;ToUpper(ch);", 65L);
    ("signed byte load", declaration ^ "I8 ch=-31;ToUpper(ch);", -31L);
    ("nested conversion", declaration ^ "ToUpper(ToUpper('a'));", 65L);
    ( "argument effects",
      declaration ^ "I64 G=0;I64 Input(){G++;return 'z';}ToUpper(Input())+G;",
      91L );
    ( "recursive caller",
      declaration ^ "I64 F(I64 n){if(n)return ToUpper(F(n-1));return 'a';}F(4);",
      65L );
    ("renamed internal", "_intern 0x1e I64 Convert(U8 ch);Convert('q');", 81L);
    ( "ordinary name keeps body",
      "I64 ToUpper(U8 ch){return ch+1;}ToUpper('a');",
      98L );
    ( "later macro keeps original target",
      declaration ^ "\n#define IC_TOUPPER 0x84\nToUpper('a');",
      65L );
  ]

let rejected =
  [
    "_intern 0x1e U64 Bad(U8 ch);Bad(97);";
    "_intern 0x1e I64 Bad(I64 ch);Bad(97);";
    "_intern 0x1e I64 Bad(U8 *ch);Bad(\"a\");";
    "_intern 0x1e I64 Bad(U8 ch,U8 other);Bad(97,98);";
    "_intern 0x1e I64 Bad(U8 ch,...);Bad(97,98);";
    "_intern 0x1e I64 Bad(U8 ch=97);Bad();";
    "_intern 0x1f I64 Bad(U8 ch);Bad(97);";
    "_intern IC_TOUPPER I64 Bad(U8 ch);Bad(97);";
    "_intern (0x1e) I64 Bad(U8 ch);Bad(97);";
  ]

let source_values () =
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, expected) ->
          let report, execution = success mode source in
          word label expected execution;
          Alcotest.(check string)
            "no intrinsic output" ""
            (integer_program_report_output_bytes report);
          Alcotest.(check int)
            "no intrinsic output work" 0
            (integer_program_report_output_work report))
        cases)
    modes

let source_rejections () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let report = run mode source in
          match integer_program_report_outcome report with
          | Error (_ :: _) -> ()
          | Error [] -> Alcotest.fail "missing rejection diagnostic"
          | Ok _ ->
              Alcotest.fail
                ("unsupported internal declaration executed: " ^ source))
        rejected)
    modes

let exact_steps_and_output () =
  List.iter
    (fun mode ->
      let source =
        declaration
        ^ "extern U0 Print(U8 *fmt,...);Print(\"kept\");ToUpper('a');"
      in
      let control, execution = success mode source in
      let steps = VM.executed_steps execution in
      let exact = run ~max_steps:steps mode source in
      (match integer_program_report_outcome exact with
      | Ok checked -> word "exact step budget" 65L checked.value
      | Error _ -> Alcotest.fail "exact step budget rejected");
      let below = run ~max_steps:(steps - 1) mode source in
      (match integer_program_report_outcome below with
      | Error ((first : Diagnostic.t) :: _) ->
          Alcotest.(check string) "one below" "HCIRVM0007" first.code
      | _ -> Alcotest.fail "one-below step limit did not fault");
      Alcotest.(check string)
        "prior output" "kept"
        (integer_program_report_output_bytes below);
      Alcotest.(check int)
        "prior output work"
        (integer_program_report_output_work control)
        (integer_program_report_output_work below))
    modes

let authority () =
  let module A = Test_internal_strlen_authority in
  let source = "_intern 0x1e I64 Convert(U8 ch);Convert(97);" in
  let mutations =
    [
      ( Ir_opcode.Ic_toupper,
        fun (d : A.Seq.description) -> { d with flags = 1L } );
      (Ir_opcode.Ic_toupper, fun d -> { d with operands = [] });
      ( Ir_opcode.Ic_toupper,
        fun d -> { d with payload = Some (A.Seq.Integer 0x1eL) } );
      (Ir_opcode.Ic_toupper, fun d -> { d with opcode = Ir_opcode.Ic_strlen });
      (Ir_opcode.Ic_toupper, fun d -> { d with target_type = None });
      (Ir_opcode.Ic_imm_i64, fun d -> { d with flags = 0x2000L });
      (Ir_opcode.Ic_call_start, fun d -> { d with payload = None });
      (Ir_opcode.Ic_call_end, fun d -> { d with result = None });
    ]
  in
  List.iter
    (fun mode ->
      let original = A.fixture ~source mode in
      let foreign = A.fixture ~source mode in
      A.valid_control ~expected:65L original;
      let context = A.Unit.runtime_calls foreign.unit_ in
      A.rejects "foreign native context"
        (A.compile ~runtime_calls:context original);
      A.rejects "foreign interpreter context"
        (A.execute ~runtime_calls:context original);
      List.iter
        (fun (opcode, transform) ->
          let original = A.fixture ~source mode in
          A.valid_control ~expected:65L original;
          let cell, description = A.find_cell original opcode in
          Obj.set_field (Obj.repr cell) 0 (Obj.repr (transform description));
          A.rejects "changed native intrinsic" (A.compile original);
          A.rejects "changed interpreter intrinsic" (A.execute original))
        mutations)
    modes

let tests =
  [
    Alcotest.test_case "bytes and full computed words" `Quick source_values;
    Alcotest.test_case "internal target and signature rejection" `Quick
      source_rejections;
    Alcotest.test_case "instruction limits preserve prior output" `Quick
      exact_steps_and_output;
    Alcotest.test_case "foreign and changed intrinsic authority" `Quick
      authority;
  ]
