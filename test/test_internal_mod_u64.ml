open Holyc_lib
module VM = Ir_integer_interpreter
module A = Test_internal_strlen_authority

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]
let declaration = "public _intern 0xaf U64 ModU64(U64 *q,U64 d);"

let run ?(max_steps = 100_000) ?(max_initializer_steps = 100_000) mode contents
    =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"internal-mod-u64.hc" ~contents
  in
  let config =
    match Preprocessor.Config.create ~compilation_mode:mode () with
    | Ok config -> config
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

let ordinary_cases =
  let quotient label object_type numerator divisor quotient remainder =
    let source =
      Printf.sprintf
        "%sU64 F(){%s q=%s;U64 r;r=ModU64(&q,%s);if(q!=%s)return %Ld;return \
         r;}F();"
        declaration object_type numerator divisor quotient
        (Int64.logxor remainder 1L)
    in
    (label, source, VM.U64, remainder)
  in
  let case label expression type_ bits =
    (label, declaration ^ expression, type_, bits)
  in
  [
    quotient "zero dividend" "U64" "0" "10" "0" 0L;
    quotient "smaller dividend" "U64" "2" "10" "0" 2L;
    quotient "quotient and remainder" "U64" "442" "10" "44" 2L;
    quotient "divisor one" "U64" "0xffffffffffffffff" "1" "0xffffffffffffffff"
      0L;
    quotient "unsigned high dividend" "U64" "0x8000000000000000" "3"
      "0x2aaaaaaaaaaaaaaa" 2L;
    quotient "all ones divided by two" "U64" "0xffffffffffffffff" "2"
      "0x7fffffffffffffff" 1L;
    quotient "all ones divided by three" "U64" "0xffffffffffffffff" "3"
      "0x5555555555555555" 0L;
    quotient "high divisor" "U64" "0xffffffffffffffff" "0x8000000000000000" "1"
      Int64.max_int;
    quotient "equal high words" "U64" "0x8000000000000000" "0x8000000000000000"
      "1" 0L;
    quotient "larger high divisor" "U64" "0x8000000000000000"
      "0xffffffffffffffff" "0" Int64.min_int;
    quotient "signed object uses unsigned bits" "I64" "-1" "3"
      "0x5555555555555555" 0L;
    quotient "signed high object" "I64" "0x8000000000000000" "3"
      "0x2aaaaaaaaaaaaaaa" 2L;
    case "signed object retains its type"
      "I64 F(){I64 q=-1;ModU64(&q,1);return q;}F();" VM.I64 (-1L);
    case "computed narrow divisor retains full word"
      "U8 D(){return 0x100;}U64 F(){U64 q=258;return ModU64(&q,D());}F();"
      VM.U64 2L;
    case "stored narrow divisor narrows"
      "U64 F(){U8 d=0x101;U64 q=258;return ModU64(&q,d);}F();" VM.U64 0L;
    case "array interior updates original object"
      "U64 F(){U64 q[3];q[0]=9;q[1]=442;q[2]=8;U64 r;r=ModU64(&q[1],10);return \
       q[0]+q[1]+q[2]+r;}F();"
      VM.U64 63L;
    case "copied pointer aliases caller"
      "U64 F(){U64 q=442;U64 *p=&q,*s=p;U64 r;r=ModU64(s,10);return *p+r;}F();"
      VM.U64 46L;
    case "nested remainder is right argument"
      "U64 F(){U64 a=442,b=22;U64 r;r=ModU64(&a,ModU64(&b,10));return \
       a+b+r;}F();"
      VM.U64 223L;
    case "right argument writes pointed word first"
      "U64 F(){U64 q=999;U64 r;r=ModU64(&q,q=442);return q*100+r;}F();" VM.U64
      100L;
    case "right to left index effects"
      "I64 N=0;I64 Index(){N=N*10+1;return 0;}U64 D(){N=N*10+2;return 10;}U64 \
       F(){U64 q[1];q[0]=442;U64 r;r=ModU64(&q[Index()],D());return \
       q[0]*10000+r*100+N;}F();"
      VM.U64 440221L;
    case "left rebinding retains earlier right alias"
      "U64 D(U64 *p){*p=442;return 10;}U64 F(){U64 a[2];a[0]=123;a[1]=1234;U64 \
       *p=&a[0];U64 r;r=ModU64(p=&a[1],D(p));return a[0]+a[1]+r;}F();"
      VM.U64 569L;
    case "right rebinding precedes left load"
      "U64 D(U64 *p){*p=442;return 10;}U64 F(){U64 a[2];a[0]=123;a[1]=1234;U64 \
       *p=&a[0];U64 r;r=ModU64(p,D(p=&a[1]));return a[0]+a[1]+r;}F();"
      VM.U64 169L;
    case "recursive aliases retain caller storage"
      "U64 Divide(U64 *p,I64 n){if(n)return Divide(p,n-1);return \
       ModU64(p,10);}U64 F(){U64 q=442;U64 r;r=Divide(&q,4);return q+r;}F();"
      VM.U64 46L;
    case "decimal extraction chain"
      "U64 F(){U64 q=442;U64 \
       a,b,c;a=ModU64(&q,10);b=ModU64(&q,10);c=ModU64(&q,10);return \
       a+10*b+100*c+q;}F();"
      VM.U64 442L;
    case "date extraction signed object"
      "I64 F(){I64 q=123456789;U64 \
       a,b,c,d;a=ModU64(&q,100);b=ModU64(&q,100);c=ModU64(&q,60);d=ModU64(&q,60);return \
       q*1000000+d*10000+c*100+b+a;}F();"
      VM.I64 3254656L;
    ( "renamed numeric binding",
      "_intern 0xaf U64 Extract(U64 *q,U64 d);U64 F(){U64 q=442;return \
       Extract(&q,10);}F();",
      VM.U64,
      2L );
    ( "ordinary name uses source body",
      "U64 ModU64(U64 *q,U64 d){*q=40;return d;}U64 F(){U64 q=0;U64 \
       r;r=ModU64(&q,2);return q+r;}F();",
      VM.U64,
      42L );
    ( "original macro target",
      "#define OP 0xaf\n\
       _intern OP U64 Extract(U64 *q,U64 d);U64 F(){U64 q=442;return \
       Extract(&q,10);}F();",
      VM.U64,
      2L );
  ]

let cases =
  ordinary_cases
  @ List.init 64 (fun index ->
      let divisor = Int64.shift_left 1L index in
      let quotient = Int64.shift_right_logical (-1L) index in
      let remainder = Int64.pred divisor in
      let source =
        Printf.sprintf
          "%sU64 F(){%s q=-1;U64 r;r=ModU64(&q,0x%Lx);if(q!=0x%Lx)return \
           %Ld;return r;}F();"
          declaration
          (if index mod 2 = 0 then "U64" else "I64")
          divisor quotient
          (Int64.logxor remainder 1L)
      in
      ("each divisor bit " ^ string_of_int index, source, VM.U64, remainder))

let values () =
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, type_, expected) ->
          let _, execution = success mode source in
          word label type_ expected execution)
        cases)
    modes

let rejected =
  [
    "_intern 0xaf I64 F(U64 *q,U64 d);U64 q=42;F(&q,2);";
    "_intern 0xaf U64 F(I64 *q,U64 d);I64 q=42;F(&q,2);";
    "_intern 0xaf U64 F(U64 q,U64 d);F(42,2);";
    "_intern 0xaf U64 F(U64 *q,I64 d);U64 q=42;F(&q,2);";
    "_intern 0xaf U64 F(U64 *q);U64 q=42;F(&q);";
    "_intern 0xaf U64 F(U64 *q,U64 d=2);U64 q=42;F(&q);";
    "_intern 0xaf U64 F(U64 *q,U64 d,...);U64 q=42;F(&q,2);";
    "_intern 0xaf U64 F(U64 **q,U64 d);U64 q=42;U64 *p=&q;F(&p,2);";
    "_intern (0xaf) U64 F(U64 *q,U64 d);U64 q=42;F(&q,2);";
    "_intern 0xae U64 F(U64 *q,U64 d);U64 q=42;F(&q,2);";
    declaration ^ "U8 q=42;ModU64(&q,2);";
    declaration ^ "U32 q=42;ModU64(&q,2);";
    declaration ^ "ModU64(0,2);";
  ]

let signatures () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          match integer_program_report_outcome (run mode source) with
          | Error (_ :: _) -> ()
          | _ -> Alcotest.fail ("invalid call executed: " ^ source))
        (if mode = Preprocessor.Jit then
           List.filter
             (fun source ->
               not (Internal_binding_cases.is_parenthesized source))
             rejected
         else rejected))
    modes

let authority () =
  let source = declaration ^ "U64 q;q=442;ModU64(&q,10);" in
  List.iter
    (fun mode ->
      let original = A.fixture ~source mode
      and foreign = A.fixture ~source mode in
      A.valid_control ~expected:2L original;
      let context = A.Unit.runtime_calls foreign.unit_ in
      A.rejects "foreign native context"
        (A.compile ~runtime_calls:context original);
      A.rejects "foreign VM context" (A.execute ~runtime_calls:context original);
      let mutations =
        [
          (fun (d : A.Seq.description) -> { d with flags = 1L });
          (fun d -> { d with operands = [] });
          (fun d -> { d with operands = List.rev d.operands });
          (fun d ->
            { d with operands = [ List.hd d.operands; List.hd d.operands ] });
          (fun d -> { d with operands = d.operands @ d.operands });
          (fun d ->
            { d with result = Some { A.Seq.value_id = List.hd d.operands } });
          (fun d -> { d with payload = Some (A.Seq.Integer 42L) });
          (fun d -> { d with target_type = None });
          (fun d -> { d with opcode = Ir_opcode.Ic_max_u64 });
        ]
      in
      List.iter
        (fun transform ->
          let original = A.fixture ~source mode in
          let cell, description = A.find_cell original Ir_opcode.Ic_mod_u64 in
          Obj.set_field (Obj.repr cell) 0 (Obj.repr (transform description));
          A.rejects "changed native operation" (A.compile original);
          A.rejects "changed VM operation" (A.execute original))
        mutations;
      List.iteri
        (fun index transform ->
          let original = A.fixture ~source mode in
          let _, operation = A.find_cell original Ir_opcode.Ic_mod_u64 in
          let intrinsic =
            A.Calls.find_intrinsic_instruction
              (A.Unit.runtime_calls original.unit_)
              ~owner:A.Calls.Entry operation.instruction_id
            |> Option.get
          in
          let producer =
            A.Calls.intrinsic_arguments intrinsic
            |> List.hd |> A.Calls.argument_producer
          in
          let rec find = function
            | [] -> None
            | instruction :: rest as cell ->
                let description = A.Seq.description instruction in
                if description.instruction_id = producer then
                  Some (cell, description)
                else find rest
          in
          let cell, description =
            A.Unit.entry original.unit_
            |> Ir_x87_stack.graph |> A.Graph.blocks
            |> List.find_map (fun block ->
                find (A.Graph.instructions block |> A.Seq.instructions))
            |> Option.get
          in
          Obj.set_field (Obj.repr cell) 0 (Obj.repr (transform description));
          A.rejects
            ("changed native pointer producer " ^ string_of_int index)
            (A.compile original);
          A.rejects
            ("changed VM pointer producer " ^ string_of_int index)
            (A.execute original))
        [
          (fun (d : A.Seq.description) -> { d with flags = 0x2000L });
          (fun d -> { d with target_type = None });
          (fun d -> { d with span = None });
        ])
    modes

let fault_cases =
  [
    ("U64 q=442;return ModU64(&q,0);", "HCIRVM0009", "kept");
    ("U64 q;return ModU64(&q,10);", "HCIRVM0012", "kept");
    ("U64 q;return ModU64(&q,0);", "HCIRVM0012", "kept");
    ("U64 q[1];q[0]=442;return ModU64(&q[1],10);", "HCIRVM0019", "kept");
    ("U64 q[1];q[0]=442;return ModU64(&q[2],10);", "HCIRVM0019", "kept");
    ("U64 q=442;return ModU64(&q,Unknown());", "HCIRVM0012", "keptright");
  ]

let fault_source body =
  declaration
  ^ "extern U0 Print(U8 *fmt,...);U64 Unknown(){U64 n;Print(\"right\");return \
     n;}U64 F(){Print(\"kept\");" ^ body ^ "}F();"

let faults () =
  List.iter
    (fun mode ->
      List.iter
        (fun (body, code, output) ->
          let report = run mode (fault_source body) in
          (match integer_program_report_outcome report with
          | Error (first :: _) ->
              Alcotest.(check string) "reached fault" code first.code
          | _ -> Alcotest.fail "invalid operation completed");
          Alcotest.(check string)
            "reached output" output
            (integer_program_report_output_bytes report))
        fault_cases)
    modes

let limit_source =
  declaration
  ^ "extern U0 Print(U8 *fmt,...);U64 F(){U64 q=442;U64 \
     r;Print(\"kept\");r=ModU64(&q,10);return q+r;}F();"

let limits () =
  List.iter
    (fun mode ->
      let control, execution = success mode limit_source in
      let steps = VM.executed_steps execution in
      (match
         integer_program_report_outcome (run ~max_steps:steps mode limit_source)
       with
      | Ok checked -> word "exact quota" VM.U64 46L checked.value
      | Error errors -> Alcotest.fail (A.diagnostics errors));
      let below = run ~max_steps:(steps - 1) mode limit_source in
      (match integer_program_report_outcome below with
      | Error (first :: _) ->
          Alcotest.(check string) "one below" "HCIRVM0007" first.code
      | _ -> Alcotest.fail "one-below quota admitted");
      Alcotest.(check string)
        "reached bytes" "kept"
        (integer_program_report_output_bytes below);
      Alcotest.(check int)
        "reached work"
        (integer_program_report_output_work control)
        (integer_program_report_output_work below))
    modes

let retained =
  "#exe {" ^ declaration
  ^ "U64 Q=442;I64 N=0;U64 D(){N++;return 10;}U64 Saved(U64 \
     x=ModU64(&Q,D())){return \
     x;}if(Q!=44||N!=1)Print(\"bad\");Q=999;N=0;if(Saved()!=2||Q!=999||N)Print(\"bad\");StreamPrint(\"%d;\",Saved()+40);}"

let retained_values () =
  List.iter
    (fun mode ->
      let report, execution = success mode retained in
      word "retained source" VM.I64 42L execution;
      Alcotest.(check string)
        "saved once" ""
        (integer_program_report_output_bytes report);
      let prep = Option.get (integer_program_report_preparation_work report) in
      (match
         integer_program_report_outcome
           (run ~max_initializer_steps:prep mode retained)
       with
      | Ok checked -> word "exact prep" VM.I64 42L checked.value
      | Error errors -> Alcotest.fail (A.diagnostics errors));
      let below = run ~max_initializer_steps:(prep - 1) mode retained in
      (match integer_program_report_outcome below with
      | Error (_ :: _) -> ()
      | _ -> Alcotest.fail "one-below preparation admitted");
      Alcotest.(check int)
        "failed preparation retains work" (prep - 1)
        (Option.get (integer_program_report_preparation_work below));
      let failed =
        run mode
          ("#exe {" ^ declaration
         ^ "U64 Q=442;U64 D(){Print(\"divisor\");return 0;}U64 Saved(U64 \
            x=ModU64(&Q,D())){return x;}StreamPrint(\"bad;\");}")
      in
      (match integer_program_report_outcome failed with
      | Error (first :: _) ->
          Alcotest.(check string)
            "original default fault" "HCIRVM0009" first.code
      | _ -> Alcotest.fail "zero-divisor default completed");
      Alcotest.(check string)
        "failed default ran once" "divisor"
        (integer_program_report_output_bytes failed);
      Alcotest.(check bool)
        "failed default keeps attempted preparation" true
        (Option.get (integer_program_report_preparation_work failed) > 0))
    modes

let tests =
  [
    Alcotest.test_case "unsigned words, mutation, aliases and actual callers"
      `Quick values;
    Alcotest.test_case "exact original signature and pointer domain" `Quick
      signatures;
    Alcotest.test_case "foreign and changed call authority" `Quick authority;
    Alcotest.test_case "fault phases and reached effects" `Quick faults;
    Alcotest.test_case "exact runtime limits" `Quick limits;
    Alcotest.test_case "original retained preparation and mutation" `Quick
      retained_values;
  ]
