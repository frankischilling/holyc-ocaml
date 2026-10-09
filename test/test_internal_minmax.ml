open Holyc_lib
module VM = Ir_integer_interpreter
module A = Test_internal_strlen_authority

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let declaration =
  "public _intern 0xab I64 MinI64(I64 a,I64 b);\n\
   public _intern 0xac U64 MinU64(U64 a,U64 b);\n\
   public _intern 0xad I64 MaxI64(I64 a,I64 b);\n\
   public _intern 0xae U64 MaxU64(U64 a,U64 b);"

let run ?(max_steps = 100_000) ?(max_initializer_steps = 100_000) mode contents
    =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"internal-minmax.hc" ~contents
  in
  let config =
    match Preprocessor.Config.create ~compilation_mode:mode () with
    | Ok value -> value
    | Error message -> Alcotest.fail message
  in
  run_integer_program_report ~max_initializer_steps session ~config ~source
    ~max_steps

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
    case "signed minimum zero" "MinI64(0,0);" VM.I64 0L;
    case "signed minimum negative" "MinI64(-1,42);" VM.I64 (-1L);
    case "signed minimum reversed" "MinI64(42,-1);" VM.I64 (-1L);
    case "signed minimum extremes"
      "MinI64(0x8000000000000000,0x7fffffffffffffff);" VM.I64 Int64.min_int;
    case "signed minimum reversed extremes"
      "MinI64(0x7fffffffffffffff,0x8000000000000000);" VM.I64 Int64.min_int;
    case "signed minimum converted full word" "MinI64(0xffffffffffffffff,42);"
      VM.I64 (-1L);
    case "signed minimum equal" "MinI64(42,42);" VM.I64 42L;
    case "signed maximum zero" "MaxI64(0,0);" VM.I64 0L;
    case "signed maximum negative" "MaxI64(-42,-1);" VM.I64 (-1L);
    case "signed maximum reversed" "MaxI64(-1,-42);" VM.I64 (-1L);
    case "signed maximum extremes"
      "MaxI64(0x8000000000000000,0x7fffffffffffffff);" VM.I64 Int64.max_int;
    case "signed maximum reversed extremes"
      "MaxI64(0x7fffffffffffffff,0x8000000000000000);" VM.I64 Int64.max_int;
    case "signed maximum equal" "MaxI64(42,42);" VM.I64 42L;
    case "unsigned minimum zero" "MinU64(0,0xffffffffffffffff);" VM.U64 0L;
    case "unsigned minimum reversed zero" "MinU64(0xffffffffffffffff,0);" VM.U64
      0L;
    case "unsigned minimum high bit" "MinU64(0x8000000000000000,1);" VM.U64 1L;
    case "unsigned minimum two high words"
      "MinU64(0xffffffffffffffff,0x8000000000000000);" VM.U64 Int64.min_int;
    case "unsigned minimum equal"
      "MinU64(0xffffffffffffffff,0xffffffffffffffff);" VM.U64 (-1L);
    case "unsigned maximum zero" "MaxU64(0,0xffffffffffffffff);" VM.U64 (-1L);
    case "unsigned maximum reversed zero" "MaxU64(0xffffffffffffffff,0);" VM.U64
      (-1L);
    case "unsigned maximum high bit" "MaxU64(0x8000000000000000,1);" VM.U64
      Int64.min_int;
    case "unsigned maximum reversed high bit" "MaxU64(1,0x8000000000000000);"
      VM.U64 Int64.min_int;
    case "unsigned maximum two high words"
      "MaxU64(0x8000000000000000,0xffffffffffffffff);" VM.U64 (-1L);
    case "unsigned maximum equal" "MaxU64(42,42);" VM.U64 42L;
    case "computed narrow word" "U8 Input(){return 0x100;}MinU64(Input(),257);"
      VM.U64 256L;
    case "stored narrow word" "U8 bits=0x100;MaxU64(bits,1);" VM.U64 1L;
    case "same object has two original loads" "I64 bits=42;MinI64(bits,bits);"
      VM.I64 42L;
    case "nested arithmetic"
      "I64 F(){return MinI64(-3,4)+MaxI64(40,42)+MinU64(3,5)+MaxU64(0,0);}F();"
      VM.I64 42L;
    case "right to left minimum effects"
      "I64 N=0;I64 Input(I64 n){N=N*10+n;return \
       n;}MinI64(Input(1),Input(2))*100+N;"
      VM.I64 121L;
    case "right to left maximum effects"
      "I64 N=0;I64 Input(I64 n){N=N*10+n;return \
       n;}MaxI64(Input(1),Input(2))*100+N;"
      VM.I64 221L;
    case "nested right to left effects"
      "I64 N=0;I64 Input(I64 n){N=N*10+n;return \
       n;}MinI64(MaxI64(Input(1),Input(2)),Input(3))+N;"
      VM.I64 323L;
    case "recursive source caller"
      "I64 F(I64 n){if(n)return MaxI64(MinI64(F(n-1),42),0);return 42;}F(4);"
      VM.I64 42L;
    ( "renamed numeric operation",
      "_intern 0xad I64 Larger(I64 a,I64 b);Larger(41,42);",
      VM.I64,
      42L );
    ( "ordinary spelling uses body",
      "I64 MinI64(I64 a,I64 b){return a+b;}MinI64(40,2);",
      VM.I64,
      42L );
    ( "original macro target",
      "#define OP 0xab\n\
       _intern OP I64 Smaller(I64 a,I64 b);\n\
       #define OP 0xad\n\
       Smaller(-1,42);",
      VM.I64,
      -1L );
  ]

let rejected =
  [
    "_intern 0xab U64 Bad(I64 a,I64 b);Bad(1,2);";
    "_intern 0xad I64 Bad(U64 a,I64 b);Bad(1,2);";
    "_intern 0xac U64 Bad(U64 a,I64 b);Bad(1,2);";
    "_intern 0xae I64 Bad(U64 a,U64 b);Bad(1,2);";
    "_intern 0xab I64 Bad(I64 a);Bad(1);";
    "_intern 0xad I64 Bad(I64 a,I64 b,I64 c);Bad(1,2,3);";
    "_intern 0xab I64 Bad(I64 a,I64 b,...);Bad(1,2,3);";
    "_intern 0xab I64 Bad(I64 a,I64 b=42);Bad(1);";
    "_intern 0xac U64 Bad(U64 *a,U64 b);U64 n=1;Bad(&n,2);";
    "_intern (0xab) I64 Bad(I64 a,I64 b);Bad(1,2);";
    "_intern 0xaf U64 Bad(U64 a,U64 b);Bad(1,2);";
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

let default_cases =
  [
    ("I64", "MinI64(42,50)", 42L);
    ("I64", "MaxI64(41,42)", 42L);
    ("U64", "MinU64(42,0xffffffffffffffff)", 42L);
    ("U64", "MaxU64(41,42)", 42L);
  ]

let limits () =
  List.iter
    (fun mode ->
      List.iter
        (fun (type_, expression, expected) ->
          let source =
            declaration ^ "extern U0 Print(U8 *fmt,...);Print(\"kept\");"
            ^ expression ^ ";"
          in
          let control, execution = success mode source in
          let steps = VM.executed_steps execution in
          let type_ = if type_ = "I64" then VM.I64 else VM.U64 in
          (match
             integer_program_report_outcome (run ~max_steps:steps mode source)
           with
          | Ok checked -> word "exact quota" type_ expected checked.value
          | Error errors -> Alcotest.fail (A.diagnostics errors));
          let below = run ~max_steps:(steps - 1) mode source in
          (match integer_program_report_outcome below with
          | Error (first :: _) ->
              Alcotest.(check string) "one below" "HCIRVM0007" first.code
          | _ -> Alcotest.fail "one-below quota admitted");
          Alcotest.(check string)
            "reached output" "kept"
            (integer_program_report_output_bytes below);
          Alcotest.(check int)
            "reached work"
            (integer_program_report_output_work control)
            (integer_program_report_output_work below))
        default_cases)
    modes

let authority () =
  let operations =
    [
      (Ir_opcode.Ic_min_i64, "_intern 0xab I64 F(I64 a,I64 b);F(42,50);");
      (Ir_opcode.Ic_max_i64, "_intern 0xad I64 F(I64 a,I64 b);F(41,42);");
      (Ir_opcode.Ic_min_u64, "_intern 0xac U64 F(U64 a,U64 b);F(42,50);");
      (Ir_opcode.Ic_max_u64, "_intern 0xae U64 F(U64 a,U64 b);F(41,42);");
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun (opcode, source) ->
          let original = A.fixture ~source mode
          and foreign = A.fixture ~source mode in
          A.valid_control ~expected:42L original;
          let context = A.Unit.runtime_calls foreign.unit_ in
          A.rejects "foreign native context"
            (A.compile ~runtime_calls:context original);
          A.rejects "foreign VM context"
            (A.execute ~runtime_calls:context original);
          let mutations =
            [
              (fun (d : A.Seq.description) -> { d with flags = 1L });
              (fun d -> { d with operands = [] });
              (fun d -> { d with operands = List.rev d.operands });
              (fun d ->
                { d with operands = [ List.hd d.operands; List.hd d.operands ] });
              (fun d ->
                { d with result = Some { A.Seq.value_id = List.hd d.operands } });
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
  ^ "I64 N=0;I64 Input(I64 n){N=N*10+n;return n;}I64 Saved(I64 \
     x=MinI64(Input(4),Input(2))){return \
     x;}N=0;if(Saved()!=2||N)Print(\"bad\");StreamPrint(\"%d;\",MaxI64(Saved(),MinI64(50,42)));}"

let retained_values () =
  List.iter
    (fun mode ->
      let report, execution = success mode retained in
      word "retained result" VM.I64 42L execution;
      Alcotest.(check int)
        "original runtime work" 134
        (VM.executed_steps execution);
      Alcotest.(check int)
        "original preparation work" 15
        (Option.get (integer_program_report_preparation_work report));
      Alcotest.(check string)
        "default ran once" ""
        (integer_program_report_output_bytes report);
      let steps = VM.executed_steps execution in
      (match
         integer_program_report_outcome (run ~max_steps:steps mode retained)
       with
      | Ok checked -> word "exact retained work" VM.I64 42L checked.value
      | Error errors -> Alcotest.fail (A.diagnostics errors));
      (match
         integer_program_report_outcome
           (run ~max_steps:(steps - 1) mode retained)
       with
      | Error (_ :: _) -> ()
      | _ -> Alcotest.fail "retained one-below quota admitted");
      (match
         integer_program_report_outcome
           (run ~max_initializer_steps:15 mode retained)
       with
      | Ok checked -> word "exact preparation work" VM.I64 42L checked.value
      | Error errors -> Alcotest.fail (A.diagnostics errors));
      let below = run ~max_initializer_steps:14 mode retained in
      (match integer_program_report_outcome below with
      | Error (_ :: _) -> ()
      | _ -> Alcotest.fail "retained one-below preparation admitted");
      Alcotest.(check int)
        "failed original preparation work" 14
        (Option.get (integer_program_report_preparation_work below));
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

let fault_cases =
  [
    ("MinI64(Unknown(),Input());", "keptright");
    ("MinI64(Input(),Unknown());", "kept");
  ]

let fault_source expression =
  declaration
  ^ "extern U0 Print(U8 *fmt,...);I64 Unknown(){I64 n;return n;}I64 \
     Input(){Print(\"right\");return 42;}Print(\"kept\");" ^ expression

let faults () =
  List.iter
    (fun mode ->
      List.iter
        (fun (expression, expected) ->
          let report = run mode (fault_source expression) in
          (match integer_program_report_outcome report with
          | Error (_ :: _) -> ()
          | _ -> Alcotest.fail "unknown argument did not fault");
          Alcotest.(check string)
            "reached argument output" expected
            (integer_program_report_output_bytes report))
        fault_cases)
    modes

let tests =
  [
    Alcotest.test_case "independent full-word values and argument order" `Quick
      values;
    Alcotest.test_case "original numeric signatures" `Quick signatures;
    Alcotest.test_case "exact runtime limits and reached output" `Quick limits;
    Alcotest.test_case "foreign and changed two-argument authority" `Quick
      authority;
    Alcotest.test_case "argument faults retain source order and output" `Quick
      faults;
    Alcotest.test_case "retained defaults and generated source" `Quick
      retained_values;
  ]
