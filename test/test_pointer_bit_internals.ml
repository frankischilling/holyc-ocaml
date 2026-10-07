open Holyc_lib
module VM = Ir_integer_interpreter
module A = Test_internal_strlen_authority
module E = X86_64_encoder

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let declarations =
  "public _intern 0x77 Bool Bt(U8 *p,I64 n);public _intern 0x78 Bool Bts(U8 \
   *p,I64 n);public _intern 0x79 Bool Btr(U8 *p,I64 n);public _intern 0x7a \
   Bool Btc(U8 *p,I64 n);"

let run ?(max_steps = 100_000) ?(max_initializer_steps = 100_000) mode contents
    =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"pointer-bit-internals.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
  in
  run_integer_program_report ~max_initializer_steps session ~config ~source
    ~max_steps

let success mode contents =
  let report = run mode contents in
  match integer_program_report_outcome report with
  | Ok checked -> (report, checked.value)
  | Error ds -> Alcotest.fail (A.diagnostics ds)

let word label type_ expected execution =
  match VM.final_value execution with
  | Some word ->
      Alcotest.(check bool) (label ^ " class") true (word.type_ = type_);
      Alcotest.(check int64) (label ^ " bits") expected word.bits
  | None -> Alcotest.fail (label ^ " has no final word")

let case label contents expected =
  (label, declarations ^ contents, VM.I64, expected)

let ordinary_cases =
  [
    case "test leaves original word"
      "I64 F(){I64 q=2;return Bt(&q,1)*100+q;}F();" 102L;
    case "set returns prior zero" "I64 F(){I64 q=0;return Bts(&q,1)*100+q;}F();"
      2L;
    case "set returns prior one" "I64 F(){I64 q=2;return Bts(&q,1)*100+q;}F();"
      102L;
    case "reset returns prior one"
      "I64 F(){I64 q=2;return Btr(&q,1)*100+q;}F();" 100L;
    case "reset returns prior zero"
      "I64 F(){I64 q=0;return Btr(&q,1)*100+q;}F();" 0L;
    case "complement returns prior one"
      "I64 F(){I64 q=2;return Btc(&q,1)*100+q;}F();" 100L;
    case "complement returns prior zero"
      "I64 F(){I64 q=0;return Btc(&q,1)*100+q;}F();" 2L;
    case "mixed bits preserve neighbors"
      "I64 F(){I64 q=0x55;Btc(&q,1);Btr(&q,4);return q;}F();" 71L;
    case "test an all-ones word" "I64 F(){I64 q=-1;return Bt(&q,63);}F();" 1L;
    case "set preserves an all-ones word"
      "I64 F(){I64 q=-1;I64 old=Bts(&q,63);return old*100+(q==-1);}F();" 101L;
    case "reset preserves every other full-word bit"
      "I64 F(){I64 q=-1;Btr(&q,31);return q;}F();"
      (Int64.logxor (-1L) (Int64.shift_left 1L 31));
    case "complement the all-ones high bit"
      "I64 F(){I64 q=-1;Btc(&q,63);return q;}F();" Int64.max_int;
    case "compiler argument-mask caller pattern"
      "I64 F(){I64 arg1mask=5,arg2mask=0x50;return \
       Bt(&arg1mask,2)*10+Bt(&arg2mask,4);}F();"
      11L;
    case "wide array selects original second cell"
      "I64 q[2];q[0]=0;q[1]=2;Bts(q,70);q[1];" 66L;
    case "narrow array selects signed high bit"
      "I16 q[2];q[0]=0;q[1]=2;Bts(q,31);q[1];" (-32766L);
    case "byte bitmap crosses a word boundary"
      "I64 F(){U8 q[9];I64 i;for(i=0;i<9;i++)q[i]=0;Bts(q,64);return q[8];}F();"
      1L;
    case "interior original word reference"
      "I64 F(){I64 q[2];q[0]=0;q[1]=2;I64 *p=&q[1];Bts(p,63);return q[1];}F();"
      (Int64.add Int64.min_int 2L);
    case "copied caller alias"
      "I64 Set(I64 *p){I64 *alias=p;return Bts(alias,5);}I64 F(){I64 \
       q=0;return Set(&q)+q;}F();"
      32L;
    case "recursive borrowed reference"
      "Bool Rec(I64 *p,I64 n){if(n)return Rec(p,n-1);return Btc(p,63);}I64 \
       F(){I64 q=0;return Rec(&q,4)+Bt(&q,63);}F();"
      1L;
    case "nested original bit calls"
      "I64 F(){I64 q=1;return Btc(&q,Bt(&q,0))*100+q;}F();" 3L;
    case "right argument precedes address selection"
      "I64 N=0;I64 Index(){N++;return 1;}I64 F(){I64 \
       q[3];q[0]=0;q[1]=2;q[2]=0;return \
       Btc(&q[N++],Index())*100+N*10+q[1];}F();"
      120L;
    case "right argument changes original pointed value"
      "I64 Index(I64 *p){*p=2;return 1;}I64 F(){I64 q=0;return \
       Btc(&q,Index(&q))*100+q;}F();"
      100L;
    case "computed narrow index keeps full word"
      "I64 q[5]={0,0,0,0,1};I8 Index(){return 0x100;}Bt(q,Index());" 1L;
    case "stored narrow index truncates"
      "I64 q[5]={0,0,0,0,1};I8 index=0x100;Bt(q,index);" 0L;
    case "owned literal mutation"
      "I64 F(){U8 *p=\"A\";return Btr(p,6)*100+p[0];}F();" 101L;
    case "owned literal terminator bit"
      "I64 F(){U8 *p=\"A\";Bts(p,15);return p[1];}F();" 128L;
    case "persistent static word"
      "I64 F(){static I64 q=0;return Btc(&q,0);}F()+F();" 1L;
    case "prepared persistent byte array" "U8 q[2]={0,0x80};Bt(q,15);" 1L;
    ( "byte storage keeps unsigned class",
      declarations ^ "U8 q=0;Bts(&q,7);q;",
      VM.U64,
      128L );
    case "renamed original numeric binding"
      "public _intern 0x7a Bool Toggle(U8 *p,I64 n);I64 F(){I64 q=2;return \
       Toggle(&q,1);}F();"
      1L;
    ( "ordinary spelling executes its body",
      "Bool Btc(I64 *p,I64 n){*p=42;return 7;}I64 F(){I64 q=0;return \
       Btc(&q,1)*100+q;}F();",
      VM.I64,
      742L );
    ( "original macro target",
      "#define OP 0x77\n\
       public _intern OP Bool Test(U8 *p,I64 n);I64 F(){I64 q=2;return \
       Test(&q,1);}F();",
      VM.I64,
      1L );
  ]

let storage_types =
  [
    ("Bool", 8, true);
    ("I8", 8, true);
    ("U8", 8, false);
    ("I16", 16, true);
    ("U16", 16, false);
    ("I32", 32, true);
    ("U32", 32, false);
    ("I64", 64, true);
    ("U64", 64, false);
  ]

let storage_cases =
  storage_types
  |> List.concat_map (fun (name, width, signed) ->
      let bits = Int64.shift_left 1L (width - 1) in
      let expected = if signed || width = 64 then Int64.neg bits else bits in
      [
        case (name ^ " high bit store")
          (Printf.sprintf "I64 F(){%s q=0;Bts(&q,%d);return q;}F();" name
             (width - 1))
          expected;
        case (name ^ " high bit reset")
          (Printf.sprintf "I64 F(){%s q=0x%Lx;return Btr(&q,%d)*100+q;}F();"
             name bits (width - 1))
          100L;
      ])

let array_cases =
  storage_types
  |> List.concat_map (fun (name, width, signed) ->
      let high = Int64.shift_left 1L (width - 1) in
      let normalize bits =
        if signed && width < 64 then Int64.sub bits (Int64.shift_left 1L width)
        else bits
      in
      [
        case
          (name ^ " array selects the second cell without changing neighbors")
          (Printf.sprintf
             "I64 F(){%s q[3];q[0]=5;q[1]=2;q[2]=7;I64 prior=Bts(q,%d);return \
              prior*100+(q[0]==5)*10+(q[1]==%Ld)*(q[2]==7);}F();"
             name
             ((2 * width) - 1)
             (normalize (Int64.logor high 2L)))
          11L;
        case
          (name ^ " array complements only its selected cell")
          (Printf.sprintf
             "I64 F(){%s q[3];q[0]=5;q[1]=0x%Lx;q[2]=7;I64 \
              prior=Btc(q,%d);return \
              prior*100+(q[1]==0)*10+(q[0]==5)*(q[2]==7);}F();"
             name high
             ((2 * width) - 1))
          111L;
        case
          (name ^ " interior pointer retains its containing array")
          (Printf.sprintf
             "I64 F(){%s q[3];q[0]=5;q[1]=2;q[2]=7;I64 \
              prior=Bts(&q[1],%d);return \
              prior*100+(q[0]==5)*10+(q[1]==2)*(q[2]==%Ld);}F();"
             name
             ((2 * width) - 1)
             (normalize (Int64.logor high 7L)))
          11L;
      ])

let bit_cases =
  List.init 64 (fun bit ->
      let mask = Int64.shift_left 1L bit in
      [
        case
          ("test bit " ^ string_of_int bit)
          (Printf.sprintf "I64 F(){U64 q=0x%Lx;return Bt(&q,%d);}F();" mask bit)
          1L;
        case
          ("set bit " ^ string_of_int bit)
          (Printf.sprintf
             "I64 F(){U64 q=0;I64 old=Bts(&q,%d);if(old)return 0;return \
              q==0x%Lx;}F();"
             bit mask)
          1L;
        case
          ("reset bit " ^ string_of_int bit)
          (Printf.sprintf "I64 F(){U64 q=0x%Lx;return Btr(&q,%d)*100+q;}F();"
             mask bit)
          100L;
        case
          ("complement bit twice " ^ string_of_int bit)
          (Printf.sprintf
             "I64 F(){U64 q=0;I64 a=Btc(&q,%d),b=Btc(&q,%d);return \
              a*1000+b*100+q;}F();"
             bit bit)
          100L;
      ])
  |> List.concat

let cases = ordinary_cases @ storage_cases @ array_cases @ bit_cases

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
    "_intern 0x77 I64 F(U8 *p,I64 n);I64 q=0;F(&q,0);";
    "_intern 0x77 U8 F(U8 *p,I64 n);I64 q=0;F(&q,0);";
    "_intern 0x77 Bool F(I64 *p,I64 n);I64 q=0;F(&q,0);";
    "_intern 0x77 Bool F(U8 *p,U64 n);I64 q=0;F(&q,0);";
    "_intern 0x77 Bool F(U8 **p,I64 n);I64 q=0;F(&q,0);";
    "_intern 0x77 Bool F(U8 *p,I64 n=0);I64 q=0;F(&q);";
    "_intern 0x77 Bool F(U8 *p,I64 n,...);I64 q=0;F(&q,0);";
    "_intern (0x77) Bool F(U8 *p,I64 n);I64 q=0;F(&q,0);";
    declarations ^ "Bt(0,0);";
    declarations ^ "I64 F(){F64 q=0;return Bt(&q,0);}F();";
    declarations ^ "I64 F(I64 **p){return Bt(p,0);}42;";
    "_intern 0x7b Bool F(U8 *p,I64 n);I64 q=0;F(&q,0);";
    "_intern 0x7c Bool F(U8 *p,I64 n);I64 q=0;F(&q,0);";
    "_intern 0x7d Bool F(U8 *p,I64 n);I64 q=0;F(&q,0);";
  ]

let signatures () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          match integer_program_report_outcome (run mode source) with
          | Error (_ :: _) -> ()
          | _ -> Alcotest.fail "unsupported pointed bit source executed")
        (if mode = Preprocessor.Jit then
           List.filter
             (fun source ->
               not (Internal_binding_cases.is_parenthesized source))
             rejected
         else rejected))
    modes

let authority () =
  List.iter
    (fun mode ->
      let source = declarations ^ "I64 q;q=2;Btc(&q,1);" in
      let original = A.fixture ~source mode
      and foreign = A.fixture ~source mode in
      A.valid_control ~expected:1L original;
      let context = A.Unit.runtime_calls foreign.unit_ in
      A.rejects "foreign native bit context"
        (A.compile ~runtime_calls:context original);
      A.rejects "foreign VM bit context"
        (A.execute ~runtime_calls:context original);
      List.iter
        (fun transform ->
          let original = A.fixture ~source mode in
          let cell, description = A.find_cell original Ir_opcode.Ic_btc in
          Obj.set_field (Obj.repr cell) 0 (Obj.repr (transform description));
          A.rejects "changed native bit operation" (A.compile original);
          A.rejects "changed VM bit operation" (A.execute original))
        [
          (fun (d : A.Seq.description) -> { d with flags = 1L });
          (fun d -> { d with operands = List.rev d.operands });
          (fun d -> { d with operands = [] });
          (fun d ->
            { d with result = Some { A.Seq.value_id = List.hd d.operands } });
          (fun d -> { d with payload = Some (A.Seq.Integer 1L) });
          (fun d -> { d with target_type = None });
          (fun d -> { d with opcode = Ir_opcode.Ic_btr });
        ];
      List.iter
        (fun argument_index ->
          List.iter
            (fun transform ->
              let original = A.fixture ~source mode in
              let _, call = A.find_cell original Ir_opcode.Ic_btc in
              let intrinsic =
                A.Calls.find_intrinsic_instruction
                  (A.Unit.runtime_calls original.unit_)
                  ~owner:A.Calls.Entry call.instruction_id
                |> Option.get
              in
              let producer =
                List.nth (A.Calls.intrinsic_arguments intrinsic) argument_index
                |> A.Calls.argument_producer
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
                original.unit_ |> A.Unit.entry |> Ir_x87_stack.graph
                |> A.Graph.blocks
                |> List.find_map (fun block ->
                    find (A.Graph.instructions block |> A.Seq.instructions))
                |> Option.get
              in
              Obj.set_field (Obj.repr cell) 0 (Obj.repr (transform description));
              A.rejects "changed native bit argument producer"
                (A.compile original);
              A.rejects "changed VM bit argument producer" (A.execute original))
            [
              (fun (d : A.Seq.description) -> { d with span = None });
              (fun d -> (Obj.obj (Obj.dup (Obj.repr d)) : A.Seq.description));
            ])
        [ 0; 1 ])
    modes

let fault_cases =
  let base = declarations ^ "extern U0 Print(U8 *fmt,...);" in
  [
    ( base ^ "I64 F(){I64 q;Print(\"kept\");return Bt(&q,0);}F();",
      "HCIRVM0012",
      "kept" );
    ( base ^ "I64 F(){I64 q;Print(\"kept\");return Bts(&q,64);}F();",
      "HCIRVM0019",
      "kept" );
    ( base ^ "I64 F(){I64 q=0;Print(\"kept\");return Bts(&q,-1);}F();",
      "HCIRVM0019",
      "kept" );
    ( base ^ "I64 F(){Bool q=0;Print(\"kept\");return Btc(&q,8);}F();",
      "HCIRVM0019",
      "kept" );
    ( base ^ "I64 F(Bool q){Print(\"kept\");return Bt(&q,8);}F(42);",
      "HCIRVM0019",
      "kept" );
    ( base ^ "I64 F(){I64 q[2];q[0]=0;Print(\"kept\");return Bt(q,64);}F();",
      "HCIRVM0012",
      "kept" );
    ( base ^ "I64 F(){U8 q[2];q[0]=0;Print(\"kept\");return Btr(q,8);}F();",
      "HCIRVM0012",
      "kept" );
    ( base
      ^ "I64 F(){I64 q[1];q[0]=0;I64 *p=&q[1];Print(\"kept\");return \
         Bt(p,0);}F();",
      "HCIRVM0019",
      "kept" );
    ( base
      ^ "I64 Index(){Print(\"right\");return 0;}I64 F(){I64 \
         q;Print(\"kept\");return Btc(&q,Index());}F();",
      "HCIRVM0012",
      "keptright" );
    ( base ^ "I64 F(){U8 *p=\"A\";Print(\"kept\");return Bts(p,16);}F();",
      "HCIRVM0019",
      "kept" );
    ( base
      ^ "I64 F(){I64 q=0;Print(\"kept\");return Bt(&q,0x7fffffffffffffff);}F();",
      "HCIRVM0019",
      "kept" );
  ]

let faults () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, code, output) ->
          let report = run mode source in
          (match integer_program_report_outcome report with
          | Error (first :: _) ->
              Alcotest.(check string) "reached original fault" code first.code
          | _ -> Alcotest.fail "invalid bit access completed");
          Alcotest.(check string)
            "prior output" output
            (integer_program_report_output_bytes report))
        fault_cases)
    modes

let limit_source =
  declarations
  ^ "extern U0 Print(U8 *fmt,...);I64 q;q=2;Print(\"kept\");Btc(&q,1);"

let limits () =
  List.iter
    (fun mode ->
      let control, execution = success mode limit_source in
      let steps = VM.executed_steps execution in
      (match
         integer_program_report_outcome (run ~max_steps:steps mode limit_source)
       with
      | Ok checked -> word "exact bit quota" VM.I64 1L checked.value
      | Error ds -> Alcotest.fail (A.diagnostics ds));
      let below = run ~max_steps:(steps - 1) mode limit_source in
      (match integer_program_report_outcome below with
      | Error (first :: _) ->
          Alcotest.(check string) "one below" "HCIRVM0007" first.code
      | _ -> Alcotest.fail "one-below bit quota admitted");
      Alcotest.(check string)
        "reached output" "kept"
        (integer_program_report_output_bytes below);
      Alcotest.(check int)
        "reached work"
        (integer_program_report_output_work control)
        (integer_program_report_output_work below))
    modes

let retained =
  "#exe {" ^ declarations
  ^ "I64 Q=2;I64 N=0;I64 Index(){N++;return 1;}Bool Saved(Bool \
     x=Btc(&Q,Index())){return \
     x;}if(Q!=0||N!=1)Print(\"bad\");Q=999;N=0;if(Saved()!=1||Q!=999||N)Print(\"bad\");StreamPrint(\"%d;\",Saved()*42);}"

let retained_values () =
  List.iter
    (fun mode ->
      let report, execution = success mode retained in
      word "original retained mutation" VM.I64 42L execution;
      Alcotest.(check string)
        "once-only default mutation" ""
        (integer_program_report_output_bytes report);
      let work = Option.get (integer_program_report_preparation_work report) in
      (match
         integer_program_report_outcome
           (run ~max_initializer_steps:work mode retained)
       with
      | Ok checked -> word "exact original preparation" VM.I64 42L checked.value
      | Error ds -> Alcotest.fail (A.diagnostics ds));
      let below = run ~max_initializer_steps:(work - 1) mode retained in
      (match integer_program_report_outcome below with
      | Error (_ :: _) -> ()
      | _ -> Alcotest.fail "one-below preparation admitted");
      Alcotest.(check int)
        "failed work retained" (work - 1)
        (Option.get (integer_program_report_preparation_work below)))
    modes

let encoder_bytes () =
  List.iter
    (fun (operation, opcode) ->
      List.iter
        (fun (field, index, prefix, modrm) ->
          let instruction = E.Bit (operation, field, index) in
          let expected =
            String.init 4 (function
              | 0 -> Char.chr prefix
              | 1 -> '\x0f'
              | 2 -> Char.chr opcode
              | _ -> Char.chr modrm)
          in
          Alcotest.(check int) "qword bit size" 4 (E.size instruction);
          Alcotest.(check string)
            "independent bit encoding" expected (E.encode instruction))
        [
          (E.Rax, E.Rcx, 0x48, 0xc8);
          (E.Rdx, E.Rcx, 0x48, 0xca);
          (E.R8, E.R9, 0x4d, 0xc8);
          (E.R9, E.Rax, 0x49, 0xc1);
        ])
    [ (E.Bt, 0xa3); (E.Bts, 0xab); (E.Btr, 0xb3); (E.Btc, 0xbb) ]

let tests =
  [
    Alcotest.test_case "original pointed bits and storage" `Quick values;
    Alcotest.test_case "exact signatures and neighboring domains" `Quick
      signatures;
    Alcotest.test_case "original, foreign and changed authority" `Quick
      authority;
    Alcotest.test_case "original bounds, initialization and effects" `Quick
      faults;
    Alcotest.test_case "exact runtime limits" `Quick limits;
    Alcotest.test_case "original retained default mutation" `Quick
      retained_values;
    Alcotest.test_case "pinned qword bit encoder forms" `Quick encoder_bytes;
  ]
