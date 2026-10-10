open Holyc_lib
module T = Test_pointer_bit_internals
module A = Test_internal_strlen_authority
module E = Test_pointer_equality
module O = Test_pointer_ordering
module VM = Ir_integer_interpreter

let modes = T.modes
let run = E.run
let success = E.success

let storage_cases =
  T.storage_types
  |> List.concat_map (fun (type_, _, _) ->
      [
        ("equal unknown", "q-q", 0L);
        ("forward", "&q[2]-q", 2L);
        ("backward", "q-&q[2]", -2L);
        ("interior unknown", "&q[3]-&q[1]", 2L);
        ("one-past", "(q+4)-q", 4L);
        ("one-past backward", "q-(q+4)", -4L);
        ("retreat then difference", "(q+4-1)-q", 3L);
      ]
      |> List.map (fun (label, expression, expected) ->
          ( type_ ^ " " ^ label,
            Printf.sprintf "I64 F(){%s q[4];return %s;}F();" type_ expression,
            expected ))
      |> fun array ->
      array
      @ [
          ( type_ ^ " scalar one-past",
            Printf.sprintf "I64 F(){%s x;return (&x+1)-&x;}F();" type_,
            1L );
        ])

let ordinary_cases =
  [
    ( "distinct address sites preserve object",
      "I64 F(){I64 q[3];I64 *p=q,*r=&q[2];return r-p;}F();",
      2L );
    ( "left snapshot before right assignment",
      "I64 F(){I64 q[2];I64 *p=q;I64 r=p-(p=&q[1]);return r*100+(p-&q[1]);}F();",
      -100L );
    ( "left snapshot before right index effect",
      "I64 Index(I64 *n){*n=1;return 1;}I64 F(){I64 q[2],n=0;return \
       (q+n)-&q[Index(&n)];}F();",
      -1L );
    ( "loop offsets",
      "I64 F(){I64 q[4];I64 *p=q;I64 \
       i=0,n=0;while(i<4){p=q+i;n+=p-q;i++;}return n;}F();",
      6L );
    ( "one-past saved alias",
      "I64 F(){I64 q[3];I64 *p=q,*saved=q+3;I64 \
       i=0;while(i<4){p=q+i;i++;}return saved-p;}F();",
      0L );
    ( "recursive borrowed references",
      "I64 Rec(I64 *p,I64 n){I64 *r=p;if(n)return Rec(p,n-1);return \
       r-(p+1);}I64 F(){I64 q[2];return Rec(q,3);}F();",
      -1L );
    ("global aliases", "I64 q[3];I64 F(){I64 *p=&q[2];return p-q;}F();", 2L);
    ( "static aliases",
      "I64 F(){static I64 q[2];I64 *p=q+2;return p-&q[1];}F();",
      1L );
    ( "compatible literal spelling",
      "I64 F(){U8 *p=\"AB\";return &p[2]-p;}F();",
      2L );
    ("literal one-past", "I64 F(){U8 *p=\"AB\";return (p+3)-p;}F();", 3L);
    ( "repeated literal producer",
      "I64 F(){U8 *p,*saved;I64 \
       i=0;while(i<3){p=\"AB\";if(!i)saved=p;i++;}return p-saved;}F();",
      0L );
    ("rows retain full object", "I64 F(){I64 q[2][2];return q[1]-q[0];}F();", 2L);
    ( "row and flattened alias",
      "I64 F(){I64 q[2][2];I64 *p=q;return (p+1)-q[1];}F();",
      -1L );
    ( "effectful indexes left to right",
      "I64 Index(I64 *n){(*n)++;return *n;}I64 F(){I64 q[3],n=0;I64 \
       r=(q+Index(&n))-(q+Index(&n));return r+n*10;}F();",
      19L );
    ( "canonical reference reuse",
      "I64 F(){I64 q[2];I64 *p=q,*saved=q+1;I64 \
       i=0,n=0;while(i<30){p=q+1;n+=p-saved;i++;}return n;}F();",
      0L );
    ( "right-to-left call changes earlier argument",
      "I64 Pair(I64 *a,I64 *b){return a-b;}I64 F(){I64 q[2];I64 *p=q;return \
       Pair(p,p=&q[1]);}F();",
      0L );
    ( "right argument keeps old reference",
      "I64 Pair(I64 *a,I64 *b){return a-b;}I64 F(){I64 q[2];I64 *p=q;return \
       Pair(p=&q[1],p);}F();",
      1L );
    ( "difference result stays numeric",
      "I64 F(){I64 q[2];return ((q+1)-q)*42;}F();",
      42L );
    ( "stored narrowing stays separate",
      "I64 F(){U64 q[2];U8 n=q-(q+1);return n;}F();",
      255L );
    ("ordinary numeric subtraction", "I64 F(){return 80-38;}F();", 42L);
    ( "pointer minus integer remains original",
      "I64 F(){I64 q[2];q[0]=40;q[1]=42;I64 *p=q+2;return *(p-1);}F();",
      42L );
  ]

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
    ("I64 q[2];q-&q[1];", -1L);
    ("U8 q[2];q-&q[1];", -1L);
    ("I64 q[2];40;(q+2)-(q+2);", 0L);
    ("I64 q[2];42;if(0)q-q;", 42L);
  ]

let results () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, expected) ->
          let _, execution = success mode source in
          T.word "signed I64 result" VM.I64 expected execution)
        result_cases)
    modes

let rejected =
  [
    "I64 F(){I64 x;return 0-&x;}F();";
    "I64 F(){I64 x;U64 y;return &x-&y;}F();";
    "I64 F(){I8 x;U8 y;return &x-&y;}F();";
    "I64 F(){I64 x;F64 y;return &x-&y;}F();";
    "I64 F(){I64 x;I64 *p=&x;I64 **q=&p;return q-q;}F();";
    "I64 F(){I64 q[2];I64 *p=q-(q+1);return *p;}F();";
    "I64 F(){I64 x;return &x(I64)-&x;}F();";
    "I64 x;I64 *p=&x;42;";
    "I64 *F(){I64 x;return &x;}F();";
  ]

let boundaries () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let _, execution = success mode source in
          T.word "class and multidimensional root difference" VM.I64 0L
            execution)
        [
          "class C{I64 x;};I64 F(){C c;return &c-&c;}F();";
          "I64 F(){I64 q[2][2];return q-q;}F();";
        ];
      List.iter
        (fun source ->
          match integer_program_report_outcome (run mode source) with
          | Error (_ :: _) -> ()
          | _ -> Alcotest.fail "unsupported difference domain admitted")
        rejected)
    modes

type expected_fault = Existing of E.expected_fault | Mismatch

let fault_code = function
  | Existing kind -> E.fault_code kind
  | Mismatch -> "HCIRVM0018"

let replace = O.replace

let fault_cases =
  List.map
    (fun (source, kind, output) ->
      (replace (replace source "==" "-") "!=" "-", Existing kind, output))
    E.fault_cases
  @ [
      ( "extern U0 Print(U8 *fmt,...);I64 Left(){Print(\"left\");return 0;}I64 \
         Right(){Print(\"right\");return 0;}U0 F(){I64 \
         q[2],r[2];Print(\"kept\");&q[Left()]-&r[Right()];}F();",
        Mismatch,
        "keptleftright" );
      ( "extern U0 Print(U8 *fmt,...);U0 F(){I64 x,y;Print(\"kept\");&x-&y;}F();",
        Mismatch,
        "kept" );
      ( "extern U0 Print(U8 *fmt,...);I64 q[1],r[1];U0 \
         F(){Print(\"kept\");(q+1)-r;}F();",
        Mismatch,
        "kept" );
      ( "extern U0 Print(U8 *fmt,...);U0 F(){U8 \
         *p=\"AB\";Print(\"kept\");p-\"AB\";}F();",
        Mismatch,
        "kept" );
      ( "extern U0 Print(U8 *fmt,...);U0 Rec(I64 *p,I64 n){I64 \
         x;if(n)Rec(&x,n-1);else{Print(\"kept\");p-&x;}}U0 F(){I64 \
         x;Rec(&x,1);}F();",
        Mismatch,
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
                "original fault" (fault_code kind) first.code
          | _ -> Alcotest.fail "invalid difference completed");
          Alcotest.(check string)
            "reached output" output
            (integer_program_report_output_bytes report))
        fault_cases)
    modes

let shape_and_authority () =
  List.iter
    (fun mode ->
      List.iter
        (fun (type_, _, _) ->
          let source = type_ ^ " q[4];(q+3)-q;" in
          let original = A.fixture ~source mode
          and foreign = A.fixture ~source mode in
          A.valid_control ~expected:3L original;
          let runtime_calls = A.Unit.runtime_calls foreign.unit_ in
          A.rejects "foreign native difference context"
            (A.compile ~runtime_calls original);
          A.rejects "foreign VM difference context"
            (A.execute ~runtime_calls original);
          let descriptions =
            A.Unit.entry original.unit_
            |> Ir_x87_stack.graph |> A.Graph.blocks
            |> List.concat_map (fun b ->
                A.Graph.instructions b |> A.Seq.instructions
                |> List.map A.Seq.description)
          in
          let _, sub = A.find_cell original Ir_opcode.Ic_sub in
          Alcotest.(check bool)
            "difference is numeric I64" true
            (Option.fold ~none:false
               ~some:(fun t ->
                 Semantic_type.pointer_depth t = 0
                 && Semantic_type.base t
                    = Semantic_type.Primitive
                        (Semantic_type.Internal_storage, Primitive_type.I64))
               sub.target_type);
          let divides =
            List.filter
              (fun (d : A.Seq.description) -> d.opcode = Ir_opcode.Ic_div)
              descriptions
          in
          let width =
            match type_ with
            | "I8" | "U8" | "Bool" -> 1L
            | "I16" | "U16" -> 2L
            | "I32" | "U32" -> 4L
            | _ -> 8L
          in
          Alcotest.(check int)
            "original optional division"
            (if width = 1L then 0 else 1)
            (List.length divides);
          (match divides with
          | [ divide ] ->
              Alcotest.(check bool)
                "original byte subtraction feeds division" true
                (List.hd divide.operands = (Option.get sub.result).value_id);
              let size_id = List.nth divide.operands 1 in
              let size =
                List.find
                  (fun (d : A.Seq.description) ->
                    Option.fold ~none:false
                      ~some:(fun v -> v.A.Seq.value_id = size_id)
                      d.result)
                  descriptions
              in
              Alcotest.(check bool)
                "original pointee-size constant" true
                (size.opcode = Ir_opcode.Ic_imm_i64
                && size.payload = Some (A.Seq.Integer width));
              let size_cell = ref None in
              let rec find = function
                | [] -> ()
                | instruction :: rest as cell ->
                    let d = A.Seq.description instruction in
                    if
                      Option.fold ~none:false
                        ~some:(fun v -> v.A.Seq.value_id = size_id)
                        d.result
                    then size_cell := Some cell;
                    find rest
              in
              A.Unit.entry original.unit_
              |> Ir_x87_stack.graph |> A.Graph.blocks
              |> List.iter (fun block ->
                  A.Graph.instructions block |> A.Seq.instructions |> find);
              Obj.set_field
                (Obj.repr (Option.get !size_cell))
                0
                (Obj.repr
                   { size with operands = List.map Fun.id size.operands });
              A.rejects "copied exact pointee-size constant native"
                (A.compile original);
              A.rejects "copied exact pointee-size constant VM"
                (A.execute original);
              List.iter
                (fun opcode ->
                  let changed = A.fixture ~source mode in
                  let cell, d = A.find_cell changed opcode in
                  Obj.set_field (Obj.repr cell) 0
                    (Obj.repr { d with operands = List.map Fun.id d.operands });
                  A.rejects "copied original difference tail native"
                    (A.compile changed);
                  A.rejects "copied original difference tail VM"
                    (A.execute changed))
                [ Ir_opcode.Ic_div; Ir_opcode.Ic_imm_i64 ]
          | [] -> ()
          | _ -> Alcotest.fail "unexpected difference divisions");
          List.iter
            (fun transform ->
              let changed = A.fixture ~source mode in
              let cell, d = A.find_cell changed Ir_opcode.Ic_sub in
              Obj.set_field (Obj.repr cell) 0 (Obj.repr (transform d));
              A.rejects "changed native difference producer" (A.compile changed);
              A.rejects "changed VM difference producer" (A.execute changed))
            [
              (fun (d : A.Seq.description) -> { d with flags = 1L });
              (fun d -> { d with operands = [] });
              (fun d -> { d with target_type = None });
              (fun d -> { d with payload = Some (A.Seq.Integer 1L) });
              (fun d -> { d with operands = List.rev d.operands });
              (fun d -> { d with operands = List.map Fun.id d.operands });
              (fun d -> { d with opcode = Ir_opcode.Ic_equ_equ });
              (fun d ->
                {
                  d with
                  operands = List.map (fun _ -> List.hd d.operands) d.operands;
                });
            ];
          let changed = A.fixture ~source mode in
          let cell, d = A.find_cell changed Ir_opcode.Ic_addr in
          Obj.set_field (Obj.repr cell) 0
            (Obj.repr { d with operands = List.map Fun.id d.operands });
          A.rejects "copied transitive native address" (A.compile changed);
          A.rejects "copied transitive VM address" (A.execute changed))
        T.storage_types)
    modes

let numeric_forgery () =
  List.iter
    (fun mode ->
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
      let operands =
        match List.rev !pointers with
        | left :: right :: _ -> [ left; right ]
        | _ -> Alcotest.fail "missing original reference operands"
      in
      let cell, d = Option.get !numeric in
      Obj.set_field (Obj.repr cell) 0
        (Obj.repr { d with opcode = Ir_opcode.Ic_sub; operands });
      A.rejects "numeric cannot acquire native difference authority"
        (A.compile original);
      A.rejects "numeric cannot acquire VM difference authority"
        (A.execute original))
    modes

let limit_source =
  "extern U0 Print(U8 *fmt,...);I64 F(){I64 q[2];I64 \
   *p=q;Print(\"kept\");return 41+(&q[1]-p);}F();"

let limits () =
  List.iter
    (fun mode ->
      let _, control = success mode limit_source in
      let steps = VM.executed_steps control in
      (match
         integer_program_report_outcome (run ~max_steps:steps mode limit_source)
       with
      | Ok checked -> T.word "exact difference work" VM.I64 42L checked.value
      | Error ds -> Alcotest.fail (A.diagnostics ds));
      let below = run ~max_steps:(steps - 1) mode limit_source in
      (match integer_program_report_outcome below with
      | Error (first :: _) ->
          Alcotest.(check string) "one below runtime" "HCIRVM0007" first.code
      | _ -> Alcotest.fail "one-below difference budget admitted");
      Alcotest.(check string)
        "reached bytes" "kept"
        (integer_program_report_output_bytes below))
    modes

let retained_wide = replace E.retained "41+(p==Q)" "42+(p-Q)"

let retained =
  replace (replace retained_wide "I64 Q[2]" "U8 Q[2]") "I64 *p=Q" "U8 *p=Q"

let retained_values () =
  List.iter
    (fun mode ->
      let _, wide = success mode retained_wide in
      T.word "wide original size division is prepared" VM.I64 42L wide;
      let report, execution = success mode retained in
      T.word "saved original difference" VM.I64 42L execution;
      Alcotest.(check string)
        "once-only operands" ""
        (integer_program_report_output_bytes report);
      let work = Option.get (integer_program_report_preparation_work report) in
      (match
         integer_program_report_outcome
           (run ~max_initializer_steps:work mode retained)
       with
      | Ok checked ->
          T.word "exact difference preparation" VM.I64 42L checked.value
      | Error ds -> Alcotest.fail (A.diagnostics ds));
      match
        integer_program_report_outcome
          (run ~max_initializer_steps:(work - 1) mode retained)
      with
      | Error (_ :: _) -> ()
      | _ -> Alcotest.fail "one-below difference preparation admitted")
    modes

let tests =
  [
    Alcotest.test_case "all types, signed offsets, aliases and effects" `Quick
      values;
    Alcotest.test_case "original I64 result and outer latch" `Quick results;
    Alcotest.test_case "explicit neighboring domains" `Quick boundaries;
    Alcotest.test_case "original faults, work and operand order" `Quick faults;
    Alcotest.test_case "byte/size IR shape and original authority" `Quick
      shape_and_authority;
    Alcotest.test_case "numeric producers cannot gain authority" `Quick
      numeric_forgery;
    Alcotest.test_case "exact runtime limits" `Quick limits;
    Alcotest.test_case "once-only retained defaults and limits" `Quick
      retained_values;
  ]
