open Holyc_lib
module T = Test_pointer_bit_internals
module A = Test_internal_strlen_authority
module VM = Ir_integer_interpreter

let modes = T.modes

let run ?(max_steps = 100_000) ?(max_initializer_steps = 100_000) mode contents
    =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"pointer-subtraction.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
  in
  run_integer_program_report ~max_initializer_steps session ~config ~source
    ~max_steps

let success mode source =
  let report = run mode source in
  match integer_program_report_outcome report with
  | Ok checked -> (report, checked.value)
  | Error ds -> Alcotest.fail (A.diagnostics ds)

let ordinary_cases =
  [
    ( "caller offset writes original object",
      "U0 Set(I64 *p){*(p-1)=42;}I64 F(){I64 q[2];q[1]=8;Set(&q[1]);return \
       q[0]+q[1]-8;}F();",
      42L );
    ( "captured base survives right rebinding",
      "I64 F(){I64 q[3];q[0]=11;q[1]=22;q[2]=33;I64 *p=&q[2];I64 *saved=p+0; \
       saved=p-(*(p=&q[0])-10);return *saved*100+*p;}F();",
      2211L );
    ( "subtraction result survives later assignment",
      "I64 F(){I64 q[3];q[0]=11;q[1]=22;q[2]=33;I64 \
       *p=&q[2],*saved;saved=p-1;p=&q[0];return *saved*100+*p;}F();",
      2211L );
    ( "repeated subtraction preserves earlier alias",
      "I64 F(){I64 q[3];q[0]=11;q[1]=22;q[2]=33;I64 *p=&q[2],*a,*b;I64 \
       i=0;while(i<2){if(i)a=p-i;else b=p-i;i++;}return *b*100+*a;}F();",
      3322L );
    ( "pointer reassignment retreats canonical offsets",
      "I64 F(){I64 q[3];q[0]=14;q[1]=14;q[2]=14;I64 *p=q+3;I64 \
       i=0,sum=0;while(i<3){p=p-1;sum+=*p;i++;}return sum;}F();",
      42L );
    ( "nested subtraction retains original extent",
      "I64 F(){I64 q[3];q[0]=42;I64 *p=&q[2];return *((p-1)-1);}F();",
      42L );
    ( "one-past can move back before reading",
      "I64 F(){I64 q[2];q[1]=42;I64 *p=q+2;return *(p-1);}F();",
      42L );
    ( "recursive caller borrowing",
      "I64 Rec(I64 *p,I64 n){if(n)return Rec(p-1,n-1);return *p;}I64 F(){I64 \
       q[3];q[0]=42;return Rec(&q[2],2);}F();",
      42L );
    ( "static object keeps its offset",
      "I64 F(){static I64 q[2];q[0]=42;I64 *p=&q[1];return *(p-1);}F();",
      42L );
    ( "global array mutation",
      "I64 q[2];I64 F(){I64 *p=&q[1];*(p-1)=42;return q[0];}F();",
      42L );
    ( "literal offset writes original byte",
      "I64 F(){U8 *p=\"AB\";p=p+1;*(p-1)=42;return p[-1];}F();",
      42L );
    ( "array row retains original full extent",
      "I64 F(){I64 q[2][2];q[0][1]=42;I64 *p=q[1];return *(p-1);}F();",
      42L );
    ( "flattened array reference",
      "I64 F(){I64 q[2][2];q[0][0]=42;I64 *p=q;p=p+3;return *(p-3);}F();",
      42L );
    ( "stored byte offset narrows",
      "I64 F(){I64 q[2];q[0]=42;I64 *p=&q[1];U8 n=257;return *(p-n);}F();",
      42L );
    ( "stored signed byte offset extends",
      "I64 F(){I64 q[2];q[1]=42;I64 *p=q;I8 n=255;return *(p-n);}F();",
      42L );
    ( "taking offset address does not read unknown cell",
      "I64 F(){I64 q[2];I64 *p=&q[1];*(p-1)=42;return q[0];}F();",
      42L );
    ( "effects precede selected object read",
      "I64 Index(I64 *p){*p=42;return 1;}I64 F(){I64 q[2];I64 *p=&q[1];return \
       *(p-Index(&q[0]));}F();",
      42L );
  ]

let storage_cases =
  T.storage_types
  |> List.concat_map (fun (type_, width, signed) ->
      let normalize bits =
        if
          signed && width < 64
          && Int64.logand bits (Int64.shift_left 1L (width - 1)) <> 0L
        then Int64.sub bits (Int64.shift_left 1L width)
        else bits
      in
      let mask = Int64.shift_left 1L (width - 1) in
      let all =
        if width = 64 then -1L else Int64.sub (Int64.shift_left 1L width) 1L
      in
      List.init 5 (fun i ->
          ( type_ ^ " relative offset " ^ string_of_int (i - 2),
            Printf.sprintf
              "I64 F(){%s q[5];q[0]=40;q[1]=41;q[2]=42;q[3]=43;q[4]=44;%s \
               *p=&q[2];return *(p-(%d));}F();"
              type_ type_ (i - 2),
            Int64.of_int (44 - i) ))
      @ [
          ( type_ ^ " direct array subtraction",
            Printf.sprintf "I64 F(){%s q[2];q[1]=42;return *(q-(-1));}F();"
              type_,
            42L );
          ( type_ ^ " zero scalar subtraction",
            Printf.sprintf "I64 F(){%s a=42;return *(&a-0);}F();" type_,
            42L );
          ( type_ ^ " high bit keeps pointee",
            Printf.sprintf
              "I64 F(){%s q[2];q[0]=0x%Lx;%s *p=&q[1];return *(p-1);}F();" type_
              mask type_,
            normalize mask );
          ( type_ ^ " all bits keep pointee",
            Printf.sprintf
              "I64 F(){%s q[2];q[0]=-1;%s *p=&q[1];return *(p-1);}F();" type_
              type_,
            normalize all );
        ])

let offset_type_cases =
  T.storage_types
  |> List.concat_map (fun (type_, _, _) ->
      [
        ( type_ ^ " stored offset type",
          Printf.sprintf
            "I64 F(){I64 q[2];q[0]=42;%s n=1;return *(&q[1]-n);}F();" type_,
          42L );
        ( type_ ^ " computed offset type",
          Printf.sprintf
            "%s Step(){return 1;}I64 F(){I64 q[2];q[0]=42;return \
             *(&q[1]-Step());}F();"
            type_,
          42L );
      ])

let cases = ordinary_cases @ storage_cases @ offset_type_cases

let values () =
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, expected) ->
          let _, execution = success mode source in
          T.word label VM.I64 expected execution)
        cases)
    modes

let address_cases =
  [
    "I64 q[2];40;q-(-2);";
    "U0 F(){I64 q[2];I64 *p=q-(-2);}42;F();";
    "I64 q[2];40;q-(-2);I64 later;";
  ]

let address_results () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let _, execution = success mode source in
          Alcotest.(check bool)
            "owned address exposes no word" true
            (Option.is_none (VM.final_value execution)))
        address_cases;
      let _, execution = success mode "I64 q[2];42;if(0)q-(-2);" in
      T.word "unreached address preserves final value" VM.I64 42L execution)
    modes

let rejected =
  [
    "I64 F(){I64 a=42;return *(&a-&a);}F();";
    "I64 F(){I64 a=42;return *(1+&a);}F();";
    "I64 F(){I64 a=42;return *(&a+&a);}F();";
    "I64 F(){I64 a=42;return *(&a-1.0);}F();";
    "I64 F(){I64 a=42;I64 *p=&a;p-=1;return a;}F();";
    "I64 F(){I64 a=42;I64 *p=&a;p--;return a;}F();";
    "I64 *Bad(){I64 a=42;return &a-0;}Bad();";
    "I64 a=42;I64 *p=&a-0;42;";
    "class C{I64 a;};I64 F(){C c;return *(&c-1);}F();";
    "I64 F(){I64 a=42;I64 *p=&a;I64 **q=&p;return **(q-0);}F();";
    "I64 F(){I64 q[2][2];return *(q-1);}F();";
  ]

let boundaries () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          match integer_program_report_outcome (run mode source) with
          | Error (_ :: _) -> ()
          | _ -> Alcotest.fail "unsupported pointer form executed")
        rejected)
    modes

type expected_fault =
  | Unknown
  | Out_of_bounds
  | Scale_overflow
  | Offset_overflow

let fault_code = function
  | Unknown -> "HCIRVM0012"
  | Out_of_bounds -> "HCIRVM0019"
  | Scale_overflow | Offset_overflow -> "HCIRVM0020"

let fault_cases =
  let base = "extern U0 Print(U8 *fmt,...);" in
  [
    ( base
      ^ "I64 Index(){Print(\"right\");return 0;}U0 F(){I64 \
         *p;Print(\"kept\");*(p-Index());}F();",
      Unknown,
      "kept" );
    ( base ^ "U0 F(){I64 q[2];I64 n;I64 *p=&q[1];Print(\"kept\");*(p-n);}F();",
      Unknown,
      "kept" );
    (base ^ "U0 F(){I64 a;Print(\"kept\");*(&a-0);}F();", Unknown, "kept");
    ( base ^ "U0 F(){I64 q[2];I64 *p=q;Print(\"kept\");*(p-(-2));}F();",
      Out_of_bounds,
      "kept" );
    ( base ^ "U0 F(){I64 q[2];I64 *p=q;Print(\"kept\");*(p-(-3));}F();",
      Out_of_bounds,
      "kept" );
    ( base ^ "U0 F(){I64 q[2];I64 *p=q;Print(\"kept\");*(p-1);}F();",
      Out_of_bounds,
      "kept" );
    ( base
      ^ "U0 F(){I64 q[2];I64 *p=q;Print(\"kept\");*(p-0x4000000000000000);}F();",
      Scale_overflow,
      "kept" );
    ( base
      ^ "I64 Index(){return -9223372036854775808;}U0 F(){U8 q[2];U8 \
         *p=q;Print(\"kept\");*(p-Index());}F();",
      Offset_overflow,
      "kept" );
    ( base
      ^ "U64 Index(){return 0x8000000000000000;}U0 F(){U8 q[2];U8 \
         *p=q;Print(\"kept\");*(p-Index());}F();",
      Scale_overflow,
      "kept" );
    ( base
      ^ "U8 Index(){Print(\"right\");return 257;}U0 F(){I64 q[2];q[0]=42;I64 \
         *p=&q[1];Print(\"kept\");*(p-Index());}F();",
      Out_of_bounds,
      "keptright" );
    ( base
      ^ "I8 Index(){Print(\"right\");return -255;}U0 F(){I64 q[2];q[0]=42;I64 \
         *p=&q[1];Print(\"kept\");*(p-Index());}F();",
      Out_of_bounds,
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
              Alcotest.(check string)
                "original reached fault" (fault_code code) first.code
          | _ -> Alcotest.fail "invalid pointer subtraction completed");
          Alcotest.(check string)
            "reached bytes" output
            (integer_program_report_output_bytes report))
        fault_cases)
    modes

let authority () =
  List.iter
    (fun mode ->
      let source = "I64 q[2];q[0]=42;*(&q[1]-1);" in
      let original = A.fixture ~source mode
      and foreign = A.fixture ~source mode in
      A.valid_control ~expected:42L original;
      let runtime_calls = A.Unit.runtime_calls foreign.unit_ in
      A.rejects "foreign native offset context"
        (A.compile ~runtime_calls original);
      A.rejects "foreign VM offset context" (A.execute ~runtime_calls original);
      List.iter
        (fun transform ->
          let original = A.fixture ~source mode in
          let cell, description = A.find_cell original Ir_opcode.Ic_mul in
          Obj.set_field (Obj.repr cell) 0 (Obj.repr (transform description));
          A.rejects "changed native offset metadata" (A.compile original);
          A.rejects "changed VM offset metadata" (A.execute original))
        [
          (fun (d : A.Seq.description) -> { d with flags = 1L });
          (fun d -> { d with operands = [] });
          (fun d -> { d with target_type = None });
          (fun d -> { d with payload = Some (A.Seq.Integer 1L) });
          (fun d -> { d with operands = List.rev d.operands });
          (fun d -> { d with operands = List.map Fun.id d.operands });
        ];
      let source = "I64 q[2];q[0]=42;U8 n;n=0;*(&q[1]-(n+1));" in
      let original = A.fixture ~source mode in
      A.valid_control ~expected:42L original;
      let _, byte = A.find_cell original Ir_opcode.Ic_deref in
      let selected = ref None in
      let rec visit = function
        | [] -> ()
        | instruction :: rest as cell ->
            let description = A.Seq.description instruction in
            if description.opcode = Ir_opcode.Ic_mul then
              selected := Some (cell, description);
            visit rest
      in
      A.Unit.entry original.unit_
      |> Ir_x87_stack.graph |> A.Graph.blocks
      |> List.iter (fun block ->
          A.Graph.instructions block |> A.Seq.instructions |> visit);
      let cell, scale = Option.get !selected in
      let byte_id = (Option.get byte.result).value_id in
      (* A byte is a valid offset class. Replacing the original computed
         offset with another byte producer must not borrow source authority. *)
      Obj.set_field (Obj.repr cell) 0
        (Obj.repr { scale with operands = [ List.hd scale.operands; byte_id ] });
      A.rejects "native type-compatible offset substitution"
        (A.compile original);
      A.rejects "VM type-compatible offset substitution" (A.execute original);
      let original = A.fixture ~source mode in
      A.valid_control ~expected:42L original;
      let cell, byte = A.find_cell original Ir_opcode.Ic_deref in
      Obj.set_field (Obj.repr cell) 0
        (Obj.repr { byte with operands = List.map Fun.id byte.operands });
      A.rejects "native copied transitive offset producer" (A.compile original);
      A.rejects "VM copied transitive offset producer" (A.execute original);
      List.iter
        (fun transform ->
          let original = A.fixture ~source:"I64 q[1];q[0]=42;*(q-0);" mode in
          A.valid_control ~expected:42L original;
          let cell, operation = A.find_cell original Ir_opcode.Ic_sub in
          Obj.set_field (Obj.repr cell) 0 (Obj.repr (transform operation));
          A.rejects "native original subtraction record" (A.compile original);
          A.rejects "VM original subtraction record" (A.execute original))
        [
          (fun (d : A.Seq.description) -> { d with opcode = Ir_opcode.Ic_add });
          (fun d -> { d with operands = List.map Fun.id d.operands });
        ])
    modes

let limit_source =
  "extern U0 Print(U8 *fmt,...);I64 F(){I64 q[2];q[0]=42;I64 \
   *p=&q[1];Print(\"kept\");return *(p-1);}F();"

let limits () =
  List.iter
    (fun mode ->
      let _, control = success mode limit_source in
      let steps = VM.executed_steps control in
      (match
         integer_program_report_outcome (run ~max_steps:steps mode limit_source)
       with
      | Ok checked -> T.word "exact offset work" VM.I64 42L checked.value
      | Error ds -> Alcotest.fail (A.diagnostics ds));
      let below = run ~max_steps:(steps - 1) mode limit_source in
      (match integer_program_report_outcome below with
      | Error (first :: _) ->
          Alcotest.(check string) "one below runtime" "HCIRVM0007" first.code
      | _ -> Alcotest.fail "one-below offset budget admitted");
      Alcotest.(check string)
        "reached bytes" "kept"
        (integer_program_report_output_bytes below))
    modes

let retained =
  "#exe {I64 Q[2];Q[0]=42;I64 N=0;I64 Init(){N++;I64 *p=&Q[1];return \
   *(p-1);}I64 Saved(I64 x=Init()){return \
   x;}if(N!=1||Saved()!=42)Print(\"bad\");Q[0]=99;N=0;if(Saved()!=42||Q[0]!=99||N)Print(\"bad\");StreamPrint(\"%d;\",Saved());}"

let retained_values () =
  List.iter
    (fun mode ->
      let report, execution = success mode retained in
      T.word "retained original offset" VM.I64 42L execution;
      Alcotest.(check string)
        "once-only original reads" ""
        (integer_program_report_output_bytes report);
      let work = Option.get (integer_program_report_preparation_work report) in
      (match
         integer_program_report_outcome
           (run ~max_initializer_steps:work mode retained)
       with
      | Ok checked -> T.word "exact offset preparation" VM.I64 42L checked.value
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
    Alcotest.test_case "original types, offsets and alias effects" `Quick values;
    Alcotest.test_case "one-past address and outer result latch" `Quick
      address_results;
    Alcotest.test_case "explicit neighboring pointer domains" `Quick boundaries;
    Alcotest.test_case "original faults, order and full offset words" `Quick
      faults;
    Alcotest.test_case "foreign and changed offset metadata" `Quick authority;
    Alcotest.test_case "exact runtime limits" `Quick limits;
    Alcotest.test_case "retained defaults read original object once" `Quick
      retained_values;
  ]
