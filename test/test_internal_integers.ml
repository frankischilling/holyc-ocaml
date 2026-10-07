open Holyc_lib
module VM = Ir_integer_interpreter
module A = Test_internal_strlen_authority

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let declaration =
  "public _intern 0xa9 I64 AbsI64(I64 i);public _intern 0xaa I64 SignI64(I64 i);\n\
   public _intern 0xb0 I64 SqrI64(I64 i);public _intern 0xb1 U64 SqrU64(U64 i);\n\
   public _intern 0x1d U8 ToBool(I64 i);"

let run ?(max_steps = 100_000) mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"internal-integers.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> function
    | Ok value -> value
    | Error message -> Alcotest.fail message
  in
  run_integer_program_report session ~config ~source ~max_steps

let success mode contents =
  let report = run mode contents in
  match integer_program_report_outcome report with
  | Ok checked -> (report, checked.value)
  | Error errors -> Alcotest.fail (A.diagnostics errors)

let word label type_ bits execution =
  match VM.final_value execution with
  | Some actual ->
      Alcotest.(check bool) (label ^ " class") true (actual.type_ = type_);
      Alcotest.(check int64) (label ^ " bits") bits actual.bits
  | None -> Alcotest.fail (label ^ " has no final value")

let cases =
  let case label expression type_ bits =
    (label, declaration ^ expression, type_, bits)
  in
  [
    case "absolute zero" "AbsI64(0);" VM.I64 0L;
    case "absolute negative" "AbsI64(-42);" VM.I64 42L;
    case "absolute positive" "AbsI64(42);" VM.I64 42L;
    case "absolute signed minimum" "AbsI64(0x8000000000000000);" VM.I64
      Int64.min_int;
    case "absolute signed maximum" "AbsI64(0x7fffffffffffffff);" VM.I64
      Int64.max_int;
    case "sign zero" "SignI64(0);" VM.I64 0L;
    case "sign negative" "SignI64(-42);" VM.I64 (-1L);
    case "sign positive" "SignI64(42);" VM.I64 1L;
    case "sign high unsigned word" "SignI64(0xffffffffffffffff);" VM.I64 (-1L);
    case "sign signed minimum" "SignI64(0x8000000000000000);" VM.I64 (-1L);
    case "signed square negative" "SqrI64(-6);" VM.I64 36L;
    case "signed square zero" "SqrI64(0);" VM.I64 0L;
    case "signed square minimum wraps" "SqrI64(0x8000000000000000);" VM.I64 0L;
    case "signed square maximum wraps" "SqrI64(0x7fffffffffffffff);" VM.I64 1L;
    case "signed square carries low word" "SqrI64(0x100000001);" VM.I64
      8589934593L;
    case "unsigned square" "SqrU64(6);" VM.U64 36L;
    case "unsigned square maximum wraps" "SqrU64(0xffffffffffffffff);" VM.U64 1L;
    case "unsigned square high bit wraps" "SqrU64(0x8000000000000001);" VM.U64
      1L;
    case "unsigned square 32-bit boundary" "SqrU64(0xffffffff);" VM.U64
      0xfffffffe00000001L;
    case "Boolean zero" "ToBool(0);" VM.U64 0L;
    case "Boolean negative" "ToBool(-1);" VM.U64 1L;
    case "Boolean full word" "ToBool(0x100);" VM.U64 1L;
    case "Boolean high bit" "ToBool(0x8000000000000000);" VM.U64 1L;
    case "all Boolean input bits"
      "I64 Check(){I64 \
       i,errors=0;for(i=0;i<64;i++){if(ToBool(1<<i)!=1)errors++;}if(ToBool(0))errors++;return \
       errors;}Check();"
      VM.I64 0L;
    case "computed narrow word" "U8 Bits(){return 0x100000001;}SqrU64(Bits());"
      VM.U64 8589934593L;
    case "stored narrow word" "U8 bits=0x100;ToBool(bits);" VM.U64 0L;
    case "Boolean computed narrow word"
      "U8 Bits(){return 0x100;}ToBool(Bits());" VM.U64 1L;
    case "nested arithmetic"
      "I64 F(){return \
       AbsI64(-32)+SignI64(-1)+SqrI64(-3)+SqrU64(1)+ToBool(7);}F();"
      VM.I64 42L;
    case "effectful argument"
      "I64 N=0;I64 Input(){N++;return -6;}AbsI64(Input())+N;" VM.I64 7L;
    case "recursive source caller"
      "I64 F(I64 n){if(n)return AbsI64(-F(n-1));return 42;}F(4);" VM.I64 42L;
    ( "renamed numeric operation",
      "_intern 0xa9 I64 Positive(I64 i);Positive(-42);",
      VM.I64,
      42L );
    ( "ordinary spelling uses body",
      "I64 AbsI64(I64 i){return i+1;}AbsI64(41);",
      VM.I64,
      42L );
    ( "original macro target",
      "#define OP 0xaa\n\
       _intern OP I64 Sign(I64 i);\n\
       #define OP 0xa9\n\
       Sign(-42);",
      VM.I64,
      -1L );
  ]

let rejected =
  [
    "_intern 0xa9 U64 Bad(I64 i);Bad(-42);";
    "_intern 0xaa I64 Bad(U64 i);Bad(42);";
    "_intern 0xb0 I64 Bad(U8 i);Bad(6);";
    "_intern 0xb1 I64 Bad(U64 i);Bad(6);";
    "_intern 0x1d I64 Bad(I64 i);Bad(42);";
    "_intern 0xa9 I64 Bad(I64 *i);I64 n=42;Bad(&n);";
    "_intern 0xa9 I64 Bad(I64 i,I64 j);Bad(42,1);";
    "_intern 0xa9 I64 Bad(I64 i,...);Bad(42,1);";
    "_intern 0xa9 I64 Bad(I64 i=42);Bad();";
    "_intern (0xa9) I64 Bad(I64 i);Bad(42);";
    "_intern 0xab I64 Bad(I64 i);Bad(42);";
  ]

let values () =
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, type_, bits) ->
          let report, execution = success mode source in
          word label type_ bits execution;
          Alcotest.(check string)
            "no output" ""
            (integer_program_report_output_bytes report))
        cases)
    modes

let signatures () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          match integer_program_report_outcome (run mode source) with
          | Error (_ :: _) -> ()
          | _ -> Alcotest.fail ("unsupported signature executed: " ^ source))
        (if mode = Preprocessor.Jit then
           List.filter
             (fun source ->
               not (Internal_binding_cases.is_parenthesized source))
             rejected
         else rejected))
    modes

let fault_source =
  declaration ^ "extern U0 Print(U8 *fmt,...);Print(\"kept\");AbsI64(-42);"

let limits () =
  List.iter
    (fun mode ->
      let control, execution = success mode fault_source in
      let steps = VM.executed_steps execution in
      (match
         integer_program_report_outcome (run ~max_steps:steps mode fault_source)
       with
      | Ok checked -> word "exact quota" VM.I64 42L checked.value
      | Error errors -> Alcotest.fail (A.diagnostics errors));
      let below = run ~max_steps:(steps - 1) mode fault_source in
      (match integer_program_report_outcome below with
      | Error (first :: _) ->
          Alcotest.(check string) "one below" "HCIRVM0007" first.code
      | _ -> Alcotest.fail "one-below quota admitted");
      Alcotest.(check string)
        "reached output" "kept"
        (integer_program_report_output_bytes below);
      Alcotest.(check int)
        "reached output work"
        (integer_program_report_output_work control)
        (integer_program_report_output_work below))
    modes

let authority () =
  let operations =
    [
      (Ir_opcode.Ic_abs_i64, "_intern 0xa9 I64 F(I64 i);F(-42);", 42L);
      (Ir_opcode.Ic_sign_i64, "_intern 0xaa I64 F(I64 i);F(-42);", -1L);
      (Ir_opcode.Ic_sqr_i64, "_intern 0xb0 I64 F(I64 i);F(-6);", 36L);
      (Ir_opcode.Ic_sqr_u64, "_intern 0xb1 U64 F(U64 i);F(6);", 36L);
      (Ir_opcode.Ic_to_bool, "_intern 0x1d U8 F(I64 i);F(0x100);", 1L);
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (opcode, source, expected) ->
          let original = A.fixture ~source mode
          and foreign = A.fixture ~source mode in
          A.valid_control ~expected original;
          let context = A.Unit.runtime_calls foreign.unit_ in
          A.rejects "foreign native context"
            (A.compile ~runtime_calls:context original);
          A.rejects "foreign VM context"
            (A.execute ~runtime_calls:context original);
          let mutations =
            [
              (fun (d : A.Seq.description) -> { d with flags = 1L });
              (fun d -> { d with operands = [] });
              (fun d -> { d with payload = Some (A.Seq.Integer 42L) });
              (fun d -> { d with target_type = None });
              (fun d -> { d with opcode = Ir_opcode.Ic_toupper });
            ]
          in
          List.iter
            (fun transform ->
              let original = A.fixture ~source mode in
              let cell, description = A.find_cell original opcode in
              Obj.set_field (Obj.repr cell) 0 (Obj.repr (transform description));
              A.rejects "changed native operation" (A.compile original);
              A.rejects "changed VM operation" (A.execute original))
            mutations)
        operations)
    modes

let retained =
  "#exe {" ^ declaration
  ^ "I64 N=0;I64 Input(){N++;return -6;}I64 Saved(I64 \
     x=AbsI64(Input())){return x;}N=0;if(SqrI64(Saved())+ToBool(N)!=36) \
     Print(\"bad\");StreamPrint(\"%d;\",AbsI64(-32)+SignI64(-1)+SqrI64(-3)+SqrU64(1)+ToBool(7));}"

let retained_values () =
  List.iter
    (fun mode ->
      let report, execution = success mode retained in
      word "retained result" VM.I64 42L execution;
      Alcotest.(check int)
        "original preparation work" 18
        (Option.get (integer_program_report_preparation_work report));
      Alcotest.(check int)
        "original runtime work" 113
        (VM.executed_steps execution);
      Alcotest.(check string)
        "default ran once" ""
        (integer_program_report_output_bytes report);
      let steps = VM.executed_steps execution in
      (match
         integer_program_report_outcome (run ~max_steps:steps mode retained)
       with
      | Ok checked -> word "exact retained work" VM.I64 42L checked.value
      | Error errors -> Alcotest.fail (A.diagnostics errors));
      match
        integer_program_report_outcome
          (run ~max_steps:(steps - 1) mode retained)
      with
      | Error (_ :: _) -> ()
      | _ -> Alcotest.fail "retained one-below quota admitted")
    modes

let default_cases =
  [
    ("I64", "AbsI64(-42)", 42L);
    ("I64", "SignI64(-42)", -1L);
    ("I64", "SqrI64(-6)", 36L);
    ("U64", "SqrU64(6)", 36L);
    ("U8", "ToBool(0x100)", 1L);
  ]

let retained_defaults () =
  List.iter
    (fun mode ->
      List.iter
        (fun (type_, expression, expected) ->
          let source =
            "#exe {" ^ declaration ^ type_ ^ " Saved(" ^ type_ ^ " x="
            ^ expression ^ "){return x;}StreamPrint(\"%d;\",Saved());}"
          in
          let _, execution = success mode source in
          word "original retained default" VM.I64 expected execution)
        default_cases)
    modes

let tests =
  [
    Alcotest.test_case "independent full-word values and callers" `Quick values;
    Alcotest.test_case "original numeric signatures" `Quick signatures;
    Alcotest.test_case "exact runtime limits and reached output" `Quick limits;
    Alcotest.test_case "foreign and changed internal authority" `Quick authority;
    Alcotest.test_case "all unary operations in original defaults" `Quick
      retained_defaults;
    Alcotest.test_case "retained defaults and generated source" `Quick
      retained_values;
  ]
