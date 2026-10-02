open Holyc_lib
module T = Test_pointer_bit_internals
module A = Test_internal_strlen_authority
module VM = Ir_integer_interpreter

let modes = T.modes

let run ?(max_steps = 100_000) ?(max_initializer_steps = 100_000) mode contents
    =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"pointer-equality.hc" ~contents
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
    ( "pointee mutation preserves identity",
      "I64 F(){I64 q[2];I64 *p=q,*r=q;*p=40;*(r+0)=42;return p==r;}F();",
      1L );
    ( "captured equality precedes right rebinding",
      "I64 F(){I64 q[2];I64 *p=q;return p==(p=&q[1]);}F();",
      0L );
    ( "captured inequality precedes right rebinding",
      "I64 F(){I64 q[2];I64 *p=q;return p!=(p=&q[1]);}F();",
      1L );
    ( "comparison result and changed pointer remain distinct",
      "I64 F(){I64 q[2];I64 *p=q;I64 e=p==(p=&q[1]);return \
       e*100+(p==&q[1]);}F();",
      1L );
    ( "repeated sites retain earlier aliases",
      "I64 F(){I64 q[3];I64 *p=q,*saved=q;I64 \
       i=0,n=0;while(i<3){p=q+i;if(p==saved)n++;i++;}return n;}F();",
      1L );
    ( "one-past alias survives repeated sites",
      "I64 F(){I64 q[2];I64 *p=q,*saved=q;I64 i=0;while(i<3){p=q+i;i++;}return \
       (saved==q)&&(p==q+2);}F();",
      1L );
    ( "recursive borrowed aliases",
      "I64 Rec(I64 *p,I64 n){I64 *r=p;if(n)return Rec(p,n-1);return r==p;}I64 \
       F(){I64 q[2];return Rec(q,3);}F();",
      1L );
    ( "distinct live recursive owners",
      "I64 Rec(I64 *p,I64 n){I64 x;if(n)return Rec(&x,n-1);return p!=&x;}I64 \
       F(){I64 x;return Rec(&x,1);}F();",
      1L );
    ("global aliases", "I64 q[2];I64 F(){I64 *p=&q[1];return p==q+1;}F();", 1L);
    ( "different persistent objects",
      "I64 q[2],r[2];I64 F(){I64 *p=q;return p!=r;}F();",
      1L );
    ( "static aliases",
      "I64 F(){static I64 q[2];I64 *p=q;return p==&q[0];}F();",
      1L );
    ("literal byte aliases", "I64 F(){U8 *p=\"AB\";return p==&p[0];}F();", 1L);
    ( "compatible literal spelling and distinct original regions",
      "I64 F(){U8 *p=\"AB\";return p==\"AB\";}F();",
      0L );
    ( "one original literal site keeps its identity",
      "I64 F(){U8 *p,*saved;I64 \
       i=0;while(i<3){p=\"AB\";if(!i)saved=p;i++;}return p==saved;}F();",
      1L );
    ( "literal interior aliases",
      "I64 F(){U8 *p=\"AB\";return p+1==&p[1];}F();",
      1L );
    ( "row and flattened aliases",
      "I64 F(){I64 q[2][2];I64 *p=q;return q[1]==p+2;}F();",
      1L );
    ( "distinct rows retain full original object",
      "I64 F(){I64 q[2][2];return q[0]!=q[1];}F();",
      1L );
    ("multi-rank address equality", "I64 F(){I64 q[2][2];return q==q;}F();", 1L);
    ( "ordinary numeric comparisons",
      "I64 F(){I64 q[1];return (41==41)+(41!=42);}F();",
      2L );
    ( "comparison conditions keep source effects",
      "I64 F(){I64 q[2];I64 *p=q;if(p==q)p=q+1;return p!=q;}F();",
      1L );
  ]

let storage_cases =
  T.storage_types
  |> List.concat_map (fun (type_, _, _) ->
      let local label body expected =
        (type_ ^ " " ^ label, "I64 F(){" ^ body ^ "}F();", expected)
      in
      [
        local "same unknown scalar"
          (Printf.sprintf "%s x;return &x==&x;" type_)
          1L;
        local "same scalar inequality"
          (Printf.sprintf "%s x;return &x!=&x;" type_)
          0L;
        local "different scalars"
          (Printf.sprintf "%s x,y;return &x==&y;" type_)
          0L;
        local "different scalar inequality"
          (Printf.sprintf "%s x,y;return &x!=&y;" type_)
          1L;
        local "array start aliases"
          (Printf.sprintf "%s q[2];%s *p=q;return p==q;" type_ type_)
          1L;
        local "different offsets"
          (Printf.sprintf "%s q[2];%s *p=q;return p==&q[1];" type_ type_)
          0L;
        local "different offset inequality"
          (Printf.sprintf "%s q[2];%s *p=q;return p!=&q[1];" type_ type_)
          1L;
        local "interior sites"
          (Printf.sprintf "%s q[2];return &q[1]==q+1;" type_)
          1L;
        local "one-past sites"
          (Printf.sprintf "%s q[2];return q+2==q-(-2);" type_)
          1L;
        local "one-past inequality"
          (Printf.sprintf "%s q[2];%s *p=q+2;return p!=(q+2);" type_ type_)
          0L;
        local "one-past differs from last element"
          (Printf.sprintf "%s q[2];return q+2==&q[1];" type_)
          0L;
        local "different array owners"
          (Printf.sprintf "%s q[2],r[2];%s *p=q;return p==r;" type_ type_)
          0L;
        ( type_ ^ " caller aliases",
          Printf.sprintf
            "I64 Same(%s *p,%s *r){return p==r;}I64 F(){%s q[2];return \
             Same(q,&q[0]);}F();"
            type_ type_ type_,
          1L );
        local "retreat alias"
          (Printf.sprintf "%s q[2];%s *p=q+2;return p-1==&q[1];" type_ type_)
          1L;
      ])

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

let result_cases =
  [
    ("I64 q[2];q==&q[0];", 1L);
    ("I64 q[2];q!=&q[1];", 1L);
    ("I64 q[2];40;(q+2)==(q+2);", 1L);
    ("I64 q[2];42;if(0)q==q;", 42L);
  ]

let results () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, expected) ->
          let _, execution = success mode source in
          T.word "original I64 comparison result" VM.I64 expected execution)
        result_cases)
    modes

let rejected =
  [
    "I64 F(){I64 x;return &x==0;}F();";
    "I64 F(){I64 x;return 0!=&x;}F();";
    "I64 F(){I64 x;U64 y;return &x==&y;}F();";
    "I64 F(){I8 x;U8 y;return &x!=&y;}F();";
    "I64 F(){I64 x;F64 y;return &x==&y;}F();";
    "I64 F(){I64 x;return &x<&x;}F();";
    "I64 F(){I64 x;return &x-&x;}F();";
    "I64 F(){I64 x;I64 *p=&x;I64 **q=&p;return q==q;}F();";
    "class C{I64 x;};I64 F(){C c;return &c==&c;}F();";
    "I64 F(){I64 x,y;return &x==&y==&x;}F();";
    "I64 x;I64 *p=&x;42;";
    "I64 *F(){I64 x;return &x;}F();";
  ]

let boundaries () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          match integer_program_report_outcome (run mode source) with
          | Error (_ :: _) -> ()
          | _ -> Alcotest.fail "unsupported pointer comparison executed")
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
      ^ "I64 Index(){Print(\"right\");return 0;}U0 F(){I64 q[2];I64 \
         *p;Print(\"kept\");p==&q[Index()];}F();",
      Unknown,
      "kept" );
    ( base
      ^ "I64 Index(){Print(\"left\");return 0;}U0 F(){I64 q[2];I64 \
         *p;Print(\"kept\");&q[Index()]!=p;}F();",
      Unknown,
      "keptleft" );
    ( base
      ^ "I64 Index(){Print(\"right\");return 0;}U0 F(){I64 q[2];I64 \
         n;Print(\"kept\");(q+n)==&q[Index()];}F();",
      Unknown,
      "kept" );
    ( base
      ^ "I64 Index(){Print(\"right\");return 0;}U0 F(){I64 \
         q[2];Print(\"kept\");(q-1)==&q[Index()];}F();",
      Out_of_bounds,
      "kept" );
    ( base ^ "U0 F(){I64 q[2];Print(\"kept\");q==(q+3);}F();",
      Out_of_bounds,
      "kept" );
    ( base ^ "U0 F(){I64 q[2];Print(\"kept\");q==(q+0x4000000000000000);}F();",
      Scale_overflow,
      "kept" );
    ( base
      ^ "I64 Index(){return -9223372036854775808;}U0 F(){U8 \
         q[2];Print(\"kept\");q==(q-Index());}F();",
      Offset_overflow,
      "kept" );
    ( base
      ^ "I64 Index(I64 *p){return *p;}U0 F(){I64 \
         q[2];Print(\"kept\");q==&q[Index(q)];}F();",
      Unknown,
      "kept" );
    ( base
      ^ "U8 Index(){Print(\"right\");return 257;}U0 F(){I64 \
         q[2];Print(\"kept\");q==(&q[1]-Index());}F();",
      Out_of_bounds,
      "keptright" );
    ( base
      ^ "U64 Index(){return 0x8000000000000000;}U0 F(){U8 \
         q[2];Print(\"kept\");q==(q+Index());}F();",
      Scale_overflow,
      "kept" );
    ( base ^ "U0 F(){I64 q[2];Print(\"kept\");(q+2)==(q+2);*(q+2);}F();",
      Out_of_bounds,
      "kept" );
  ]

let faults () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, kind, output) ->
          let report = run mode source in
          (match integer_program_report_outcome report with
          | Error (first :: _) ->
              Alcotest.(check string)
                "original reached fault" (fault_code kind) first.code
          | _ -> Alcotest.fail "invalid pointer comparison completed");
          Alcotest.(check string)
            "reached bytes" output
            (integer_program_report_output_bytes report))
        fault_cases)
    modes

let authority () =
  List.iter
    (fun mode ->
      let source = "I64 q[2];(q+0)==&q[0];" in
      let original = A.fixture ~source mode
      and foreign = A.fixture ~source mode in
      A.valid_control ~expected:1L original;
      let runtime_calls = A.Unit.runtime_calls foreign.unit_ in
      A.rejects "foreign native equality context"
        (A.compile ~runtime_calls original);
      A.rejects "foreign VM equality context"
        (A.execute ~runtime_calls original);
      List.iter
        (fun transform ->
          let original = A.fixture ~source mode in
          let cell, comparison = A.find_cell original Ir_opcode.Ic_equ_equ in
          Obj.set_field (Obj.repr cell) 0 (Obj.repr (transform comparison));
          A.rejects "changed native equality producer" (A.compile original);
          A.rejects "changed VM equality producer" (A.execute original))
        [
          (fun (d : A.Seq.description) -> { d with flags = 1L });
          (fun d -> { d with operands = [] });
          (fun d -> { d with target_type = None });
          (fun d -> { d with payload = Some (A.Seq.Integer 1L) });
          (fun d -> { d with operands = List.rev d.operands });
          (fun d -> { d with operands = List.map Fun.id d.operands });
          (fun d -> { d with opcode = Ir_opcode.Ic_not_equ });
        ];
      let source = "I64 q[2];(q+0)==(q+1);" in
      let original = A.fixture ~source mode in
      A.valid_control ~expected:0L original;
      let cell, comparison = A.find_cell original Ir_opcode.Ic_equ_equ in
      let left = List.hd comparison.operands in
      Obj.set_field (Obj.repr cell) 0
        (Obj.repr { comparison with operands = [ left; left ] });
      A.rejects "native type-compatible pointer substitution"
        (A.compile original);
      A.rejects "VM type-compatible pointer substitution" (A.execute original);
      let original = A.fixture ~source mode in
      A.valid_control ~expected:0L original;
      let cell, address = A.find_cell original Ir_opcode.Ic_addr in
      Obj.set_field (Obj.repr cell) 0
        (Obj.repr { address with operands = List.map Fun.id address.operands });
      A.rejects "native copied transitive address producer" (A.compile original);
      A.rejects "VM copied transitive address producer" (A.execute original);
      List.iter
        (fun opcode ->
          let original = A.fixture ~source:"I64 q[2];q+0;q+0;40+2;" mode in
          A.valid_control ~expected:42L original;
          let pointers = ref [] and numeric = ref None in
          let rec visit = function
            | [] -> ()
            | instruction :: rest as cell ->
                let d = A.Seq.description instruction in
                if d.opcode = Ir_opcode.Ic_addr then
                  pointers := (Option.get d.result).value_id :: !pointers;
                if
                  d.opcode = Ir_opcode.Ic_add
                  && Option.fold ~none:false
                       ~some:(fun t -> Semantic_type.pointer_depth t = 0)
                       d.target_type
                then numeric := Some (cell, d);
                visit rest
          in
          A.Unit.entry original.unit_
          |> Ir_x87_stack.graph |> A.Graph.blocks
          |> List.iter (fun b ->
              A.Graph.instructions b |> A.Seq.instructions |> visit);
          let pointers =
            match List.rev !pointers with
            | left :: right :: _ -> [ left; right ]
            | _ -> Alcotest.fail "missing genuine original reference operands"
          in
          let cell, numeric = Option.get !numeric in
          (* The result type and references are valid, but the source expression
         was numeric addition. It cannot acquire a pointer-comparison receipt. *)
          Obj.set_field (Obj.repr cell) 0
            (Obj.repr { numeric with opcode; operands = pointers });
          A.rejects "native numeric producer cannot become pointer equality"
            (A.compile original);
          A.rejects "VM numeric producer cannot become pointer equality"
            (A.execute original))
        [ Ir_opcode.Ic_equ_equ; Ir_opcode.Ic_not_equ ])
    modes

let limit_source =
  "extern U0 Print(U8 *fmt,...);I64 F(){I64 q[2];I64 \
   *p=q;Print(\"kept\");return 41+(p==&q[0]);}F();"

let limits () =
  List.iter
    (fun mode ->
      let _, control = success mode limit_source in
      let steps = VM.executed_steps control in
      (match
         integer_program_report_outcome (run ~max_steps:steps mode limit_source)
       with
      | Ok checked -> T.word "exact comparison work" VM.I64 42L checked.value
      | Error ds -> Alcotest.fail (A.diagnostics ds));
      let below = run ~max_steps:(steps - 1) mode limit_source in
      (match integer_program_report_outcome below with
      | Error (first :: _) ->
          Alcotest.(check string) "one below runtime" "HCIRVM0007" first.code
      | _ -> Alcotest.fail "one-below comparison budget admitted");
      Alcotest.(check string)
        "reached bytes" "kept"
        (integer_program_report_output_bytes below))
    modes

let retained =
  "#exe {I64 Q[2];I64 Offset=0,N=0;I64 Init(){N++;I64 *p=Q+Offset;return \
   41+(p==Q);}I64 Saved(I64 x=Init()){return \
   x;}if(N!=1||Saved()!=42)Print(\"bad\");Offset=1;N=0;if(Saved()!=42||Offset!=1||N)Print(\"bad\");StreamPrint(\"%d;\",Saved());}"

let retained_values () =
  List.iter
    (fun mode ->
      let report, execution = success mode retained in
      T.word "saved original comparison" VM.I64 42L execution;
      Alcotest.(check string)
        "once-only original operands" ""
        (integer_program_report_output_bytes report);
      let work = Option.get (integer_program_report_preparation_work report) in
      (match
         integer_program_report_outcome
           (run ~max_initializer_steps:work mode retained)
       with
      | Ok checked ->
          T.word "exact equality preparation" VM.I64 42L checked.value
      | Error ds -> Alcotest.fail (A.diagnostics ds));
      match
        integer_program_report_outcome
          (run ~max_initializer_steps:(work - 1) mode retained)
      with
      | Error (_ :: _) -> ()
      | _ -> Alcotest.fail "one-below equality preparation admitted")
    modes

let tests =
  [
    Alcotest.test_case "original object identity, offsets and effects" `Quick
      values;
    Alcotest.test_case "original I64 results and outer latch" `Quick results;
    Alcotest.test_case "explicit neighboring pointer domains" `Quick boundaries;
    Alcotest.test_case "original faults, work and operand order" `Quick faults;
    Alcotest.test_case "original comparisons and transitive authority" `Quick
      authority;
    Alcotest.test_case "exact runtime limits" `Quick limits;
    Alcotest.test_case "retained comparison defaults execute once" `Quick
      retained_values;
  ]
