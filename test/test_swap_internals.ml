open Holyc_lib
module T = Test_pointer_bit_internals
module A = Test_internal_strlen_authority
module VM = Ir_integer_interpreter

let modes = T.modes

let operations =
  [
    ("SwapU8", 0xa5, "U8");
    ("SwapU16", 0xa6, "U16");
    ("SwapU32", 0xa7, "U32");
    ("SwapI64", 0xa8, "I64");
  ]

let declarations =
  operations
  |> List.map (fun (name, opcode, type_) ->
      Printf.sprintf "public _intern 0x%x U0 %s(%s *a,%s *b);" opcode name type_
        type_)
  |> String.concat ""

let run ?max_steps ?max_initializer_steps mode source =
  T.run ?max_steps ?max_initializer_steps mode source

let success mode source =
  let report = run mode source in
  match integer_program_report_outcome report with
  | Ok checked -> (report, checked.value)
  | Error ds -> Alcotest.fail (A.diagnostics ds)

let case label source expected = (label, declarations ^ source, expected)

let ordinary_cases =
  [
    case "compiler three-word operand swap pattern"
      "I64 F(){I64 \
       t2=8,t3=42,r2=7,r3=6,d2=5,d3=4;SwapI64(&t2,&t3);SwapI64(&r2,&r3);SwapI64(&d2,&d3);return \
       t2+(t3==8)+(r2==6)+(r3==7)+(d2==4)+(d3==5);}F();"
      47L;
    case "right pointer selection precedes left"
      "I64 N=0;I64 F(){I64 \
       q[3];q[0]=11;q[1]=22;q[2]=33;SwapI64(&q[N++],&q[N++]);return \
       q[0]*100+q[1]+N;}F();"
      2213L;
    case "captured right pointer survives left assignment"
      "I64 F(){I64 q[2];q[0]=11;q[1]=22;I64 *p=&q[0];SwapI64(p=&q[1],p);return \
       q[0]*100+q[1];}F();"
      2211L;
    case "right assignment determines subsequent left selection"
      "I64 F(){I64 q[2];q[0]=11;q[1]=22;I64 *p=&q[0];SwapI64(p,p=&q[1]);return \
       q[0]*100+q[1];}F();"
      1122L;
    case "recursive borrowed objects and void calls"
      "U0 Rec(I64 *a,I64 *b,I64 n){if(n)Rec(a,b,n-1);else SwapI64(a,b);}I64 \
       F(){I64 a=11,b=22;Rec(&a,&b,3);return a*100+b;}F();"
      2211L;
    case "nested index effects precede original reads"
      "I64 N=0;I64 Index(I64 *p){*p=33;N++;return 0;}I64 F(){I64 \
       q[2];q[0]=11;q[1]=22;SwapI64(&q[1],&q[Index(&q[1])]);return \
       q[0]*100+q[1]+N;}F();"
      3312L;
    case "persistent static swap is shared"
      "I64 F(){static I64 a=8,b=42;SwapI64(&a,&b);return a;}F()+F();" 50L;
    case "owned literal byte exchange"
      "I64 F(){U8 *p=\"AB\";SwapU8(&p[0],&p[1]);return p[0]*100+p[1];}F();"
      6665L;
    case "signed Bool and unsigned byte retain destination types"
      "I64 F(){Bool a=-128;U8 b=42;SwapU8(&a,&b);return a*100+b;}F();" 4328L;
    case "renamed original binding"
      "public _intern 0xa8 U0 Exchange(I64 *a,I64 *b);I64 F(){I64 \
       a=8,b=42;Exchange(&a,&b);return a*100+b;}F();"
      4208L;
    ( "ordinary spelling executes its body",
      "U0 SwapI64(I64 *a,I64 *b){*a=77;}I64 F(){I64 \
       a=8,b=42;SwapI64(&a,&b);return a;}F();",
      77L );
    ( "original macro survives later replacement",
      "#define OP 0xa8\n\
       public _intern OP U0 Exchange(I64 *a,I64 *b);\n\
       #define OP 0xa5\n\
       I64 F(){I64 a=8,b=42;Exchange(&a,&b);return a;}F();",
      42L );
  ]

let storage_cases =
  T.storage_types
  |> List.concat_map (fun (type_, width, signed) ->
      let operation =
        match width with
        | 8 -> "SwapU8"
        | 16 -> "SwapU16"
        | 32 -> "SwapU32"
        | _ -> "SwapI64"
      in
      let normalize bits =
        if
          signed && width < 64
          && Int64.logand bits (Int64.shift_left 1L (width - 1)) <> 0L
        then Int64.sub bits (Int64.shift_left 1L width)
        else bits
      in
      let fixed =
        [
          case
            (type_ ^ " same-location alias")
            (Printf.sprintf "I64 F(){%s a=42;%s(&a,&a);return a;}F();" type_
               operation)
            42L;
          case
            (type_ ^ " array neighbors")
            (Printf.sprintf
               "I64 F(){%s q[3];q[0]=8;q[1]=16;q[2]=42;%s(&q[0],&q[2]);return \
                q[0]*100+q[1]+q[2];}F();"
               type_ operation)
            4224L;
          case
            (type_ ^ " interior saved alias")
            (Printf.sprintf
               "I64 F(){%s q[3];q[0]=8;q[1]=16;q[2]=42;%s \
                *p=&q[1];%s(p,&q[2]);return q[0]*100+q[1]+q[2];}F();"
               type_ type_ operation)
            858L;
        ]
      in
      let bits =
        List.init width (fun bit ->
            let mask = Int64.shift_left 1L bit in
            case
              (type_ ^ " exchange bit " ^ string_of_int bit)
              (Printf.sprintf
                 "I64 F(){%s a=0x%Lx,b=42;%s(&a,&b);return a*257+b;}F();" type_
                 mask operation)
              (Int64.add (Int64.mul 42L 257L) (normalize mask)))
      in
      fixed @ bits)

let cases = ordinary_cases @ storage_cases

let values () =
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, expected) ->
          let _, execution = success mode source in
          T.word label VM.I64 expected execution)
        cases)
    modes

let void_cases =
  [
    declarations ^ "I64 a=8,b=42;42;SwapI64(&a,&b);";
    declarations ^ "I64 a=8,b=42;42;SwapI64(&a,&b);I64 later;";
    declarations ^ "U0 F(){I64 a=8,b=42;SwapI64(&a,&b);}42;F();";
  ]

let void_completion () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let _, execution = success mode source in
          Alcotest.(check bool)
            "real void clears final latch" true
            (Option.is_none (VM.final_value execution)))
        void_cases;
      let _, skipped =
        success mode (declarations ^ "I64 a=8,b=42;42;if(0)SwapI64(&a,&b);")
      in
      T.word "unreached void preserves latch" VM.I64 42L skipped)
    modes

let rejected =
  [
    "_intern 0xa8 I64 F(I64 *a,I64 *b);I64 a=8,b=42;F(&a,&b);";
    "_intern 0xa8 U0 F(U64 *a,U64 *b);U64 a=8,b=42;F(&a,&b);";
    "_intern 0xa5 U0 F(I8 *a,I8 *b);I8 a=8,b=42;F(&a,&b);";
    "_intern 0xa8 U0 F(I64 *a,I64 b);I64 a=8;F(&a,42);";
    "_intern 0xa8 U0 F(I64 *a,I64 *b,...);I64 a=8,b=42;F(&a,&b);";
    declarations ^ "SwapI64(0,0);";
    declarations ^ "U8 a=8,b=42;SwapI64(&a,&b);";
    declarations ^ "I64 a=8,b=42;SwapU8(&a,&b);";
    declarations ^ "I64 **a;SwapI64(a,a);";
    declarations ^ "I64 *Bad(){I64 a=8;return &a;}I64 b=42;SwapI64(Bad(),&b);";
    declarations ^ "I64 a=8,b=42;SwapI64(&a+1,&b);";
    declarations ^ "class C{I64 value;};C a,b;SwapI64(&a,&b);";
  ]

let signatures () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          match integer_program_report_outcome (run mode source) with
          | Error (_ :: _) -> ()
          | _ -> Alcotest.fail "unsupported swap executed")
        rejected)
    modes

let authority () =
  List.iter
    (fun mode ->
      let source = declarations ^ "I64 a,b;a=8;b=42;SwapI64(&a,&b);a;" in
      let original = A.fixture ~source mode
      and foreign = A.fixture ~source mode in
      A.valid_control ~expected:42L original;
      let context = A.Unit.runtime_calls foreign.unit_ in
      A.rejects "foreign native swap context"
        (A.compile ~runtime_calls:context original);
      A.rejects "foreign VM swap context"
        (A.execute ~runtime_calls:context original);
      List.iter
        (fun transform ->
          let original = A.fixture ~source mode in
          let cell, description = A.find_cell original Ir_opcode.Ic_swap_i64 in
          Obj.set_field (Obj.repr cell) 0 (Obj.repr (transform description));
          A.rejects "changed native swap" (A.compile original);
          A.rejects "changed VM swap" (A.execute original))
        [
          (fun (d : A.Seq.description) -> { d with flags = 1L });
          (fun d -> { d with operands = List.rev d.operands });
          (fun d -> { d with operands = [] });
          (fun d -> { d with payload = Some (A.Seq.Integer 1L) });
          (fun d -> { d with target_type = None });
          (fun d -> { d with opcode = Ir_opcode.Ic_swap_u8 });
        ];
      List.iter
        (fun opcode ->
          let original = A.fixture ~source mode in
          let cell, description = A.find_cell original opcode in
          Obj.set_field (Obj.repr cell) 0
            (Obj.repr { description with flags = 1L });
          A.rejects "changed native void call phase" (A.compile original);
          A.rejects "changed VM void call phase" (A.execute original))
        [ Ir_opcode.Ic_call_start; Ir_opcode.Ic_call_end ];
      List.iter
        (fun argument_index ->
          List.iter
            (fun copy ->
              let original = A.fixture ~source mode in
              let _, call = A.find_cell original Ir_opcode.Ic_swap_i64 in
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
                    let d = A.Seq.description instruction in
                    if d.instruction_id = producer then Some (cell, d)
                    else find rest
              in
              let cell, d =
                original.unit_ |> A.Unit.entry |> Ir_x87_stack.graph
                |> A.Graph.blocks
                |> List.find_map (fun block ->
                    find (A.Graph.instructions block |> A.Seq.instructions))
                |> Option.get
              in
              let replacement =
                if copy then
                  (Obj.obj (Obj.dup (Obj.repr d)) : A.Seq.description)
                else { d with span = None }
              in
              Obj.set_field (Obj.repr cell) 0 (Obj.repr replacement);
              A.rejects "changed native swap producer" (A.compile original);
              A.rejects "changed VM swap producer" (A.execute original))
            [ false; true ])
        [ 0; 1 ])
    modes

let fault_cases =
  let base = declarations ^ "extern U0 Print(U8 *fmt,...);" in
  [
    ( base ^ "U0 F(){I64 *a;I64 b=42;Print(\"kept\");SwapI64(a,&b);}F();",
      "HCIRVM0012",
      "kept" );
    ( base ^ "U0 F(){I64 a=8;I64 *b;Print(\"kept\");SwapI64(&a,b);}F();",
      "HCIRVM0012",
      "kept" );
    ( base ^ "U0 F(){I64 a,b=42;Print(\"kept\");SwapI64(&a,&b);}F();",
      "HCIRVM0012",
      "kept" );
    ( base ^ "U0 F(){I64 a=8,b;Print(\"kept\");SwapI64(&a,&b);}F();",
      "HCIRVM0012",
      "kept" );
    ( base
      ^ "U0 F(){I64 \
         q[2];q[0]=8;q[1]=42;Print(\"kept\");SwapI64(&q[2],&q[0]);}F();",
      "HCIRVM0019",
      "kept" );
    ( base
      ^ "U0 F(){I64 \
         q[2];q[0]=8;q[1]=42;Print(\"kept\");SwapI64(&q[0],&q[2]);}F();",
      "HCIRVM0019",
      "kept" );
    ( base ^ "U0 F(){I64 q[1];Print(\"kept\");SwapI64(&q[0],&q[1]);}F();",
      "HCIRVM0012",
      "kept" );
    ( base ^ "U0 F(){I64 q[1];Print(\"kept\");SwapI64(&q[1],&q[0]);}F();",
      "HCIRVM0019",
      "kept" );
    ( base
      ^ "I64 Index(){Print(\"right\");return 0;}U0 F(){I64 \
         q[2];q[0]=8;Print(\"kept\");SwapI64(&q[1],&q[Index()]);}F();",
      "HCIRVM0012",
      "keptright" );
  ]

let faults () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, code, output) ->
          let report = run mode source in
          (match integer_program_report_outcome report with
          | Error (first :: _) ->
              Alcotest.(check string) "original swap fault" code first.code
          | _ -> Alcotest.fail "invalid swap completed");
          Alcotest.(check string)
            "earlier output" output
            (integer_program_report_output_bytes report))
        fault_cases)
    modes

let limit_source =
  declarations
  ^ "extern U0 Print(U8 *fmt,...);I64 \
     a,b;a=8;b=42;Print(\"kept\");SwapI64(&a,&b);a;"

let limits () =
  List.iter
    (fun mode ->
      let _, control = success mode limit_source in
      let steps = VM.executed_steps control in
      (match
         integer_program_report_outcome (run ~max_steps:steps mode limit_source)
       with
      | Ok checked -> T.word "exact swap work" VM.I64 42L checked.value
      | Error ds -> Alcotest.fail (A.diagnostics ds));
      let below = run ~max_steps:(steps - 1) mode limit_source in
      (match integer_program_report_outcome below with
      | Error (first :: _) ->
          Alcotest.(check string) "one below" "HCIRVM0007" first.code
      | _ -> Alcotest.fail "one-below work admitted");
      Alcotest.(check string)
        "prior bytes" "kept"
        (integer_program_report_output_bytes below))
    modes

let retained =
  "#exe {" ^ declarations
  ^ "I64 A=8,B=42,N=0;I64 Init(){N++;SwapI64(&A,&B);return A;}I64 Saved(I64 \
     x=Init()){return \
     x;}if(A!=42||B!=8||N!=1)Print(\"bad\");A=99;B=77;N=0;if(Saved()!=42||A!=99||B!=77||N)Print(\"bad\");StreamPrint(\"%d;\",Saved());}"

let retained_values () =
  List.iter
    (fun mode ->
      let report, execution = success mode retained in
      T.word "saved result of original void mutation" VM.I64 42L execution;
      Alcotest.(check string)
        "once-only mutation" ""
        (integer_program_report_output_bytes report);
      let work = Option.get (integer_program_report_preparation_work report) in
      (match
         integer_program_report_outcome
           (run ~max_initializer_steps:work mode retained)
       with
      | Ok checked -> T.word "exact preparation" VM.I64 42L checked.value
      | Error ds -> Alcotest.fail (A.diagnostics ds));
      match
        integer_program_report_outcome
          (run ~max_initializer_steps:(work - 1) mode retained)
      with
      | Error (_ :: _) -> ()
      | _ -> Alcotest.fail "one-below preparation admitted")
    modes

let tests =
  [
    Alcotest.test_case "original objects and independent swaps" `Quick values;
    Alcotest.test_case "actual void completion and latch" `Quick void_completion;
    Alcotest.test_case "exact signatures and neighboring domains" `Quick
      signatures;
    Alcotest.test_case "original, foreign and changed authority" `Quick
      authority;
    Alcotest.test_case "read order, bounds and initialization" `Quick faults;
    Alcotest.test_case "exact runtime limits" `Quick limits;
    Alcotest.test_case "original retained default mutation" `Quick
      retained_values;
  ]
