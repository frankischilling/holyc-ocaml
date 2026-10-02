open Holyc_lib
module T = Test_pointer_bit_internals
module A = Test_internal_strlen_authority
module E = Test_pointer_equality
module VM = Ir_integer_interpreter

let modes = T.modes
let run = E.run
let success = E.success

let operators =
  [
    ("<", Ir_opcode.Ic_less, [ 0L; 1L; 0L ]);
    ("<=", Ir_opcode.Ic_less_equ, [ 1L; 1L; 0L ]);
    (">", Ir_opcode.Ic_greater, [ 0L; 0L; 1L ]);
    (">=", Ir_opcode.Ic_greater_equ, [ 1L; 0L; 1L ]);
  ]

let storage_cases =
  T.storage_types
  |> List.concat_map (fun (type_, _, _) ->
      let per_operator =
        operators
        |> List.concat_map (fun (operator, _, expected) ->
            let equal = List.nth expected 0 and forward = List.nth expected 1 in
            let array =
              [ (1, 1); (0, 2); (3, 1) ]
              |> List.map2
                   (fun expected (left, right) ->
                     ( type_ ^ " offsets " ^ operator,
                       Printf.sprintf
                         "I64 F(){%s q[3];%s *p=q+%d;return p%s(q+%d);}F();"
                         type_ type_ left operator right,
                       expected ))
                   expected
            in
            array
            @ [
                ( type_ ^ " unknown scalar " ^ operator,
                  Printf.sprintf "I64 F(){%s x;return &x%s&x;}F();" type_
                    operator,
                  equal );
                ( type_ ^ " one-past sites " ^ operator,
                  Printf.sprintf
                    "I64 F(){%s q[3];%s *p=q+3;return p%s(q-(-3));}F();" type_
                    type_ operator,
                  equal );
                ( type_ ^ " caller borrows " ^ operator,
                  Printf.sprintf
                    "I64 Rel(%s *p,%s *r){return p%sr;}I64 F(){%s q[3];return \
                     Rel(q+1,&q[2]);}F();"
                    type_ type_ operator type_,
                  forward );
              ])
      in
      per_operator
      @ [
          ( type_ ^ " retreat aliases",
            Printf.sprintf "I64 F(){%s q[3];%s *p=q+3;return p-1>=&q[2];}F();"
              type_ type_,
            1L );
        ])

let ordinary_cases =
  [
    ( "pointee mutation preserves order",
      "I64 F(){I64 q[2];I64 *p=q,*r=q;*p=40;*(r+0)=42;return p<=r;}F();",
      1L );
    ( "captured left precedes forward rebinding",
      "I64 F(){I64 q[2];I64 *p=q;return p<(p=&q[1]);}F();",
      1L );
    ( "captured greater precedes rebinding",
      "I64 F(){I64 q[2];I64 *p=q;return p>(p=&q[1]);}F();",
      0L );
    ( "captured left precedes backward rebinding",
      "I64 F(){I64 q[3];I64 *p=q+2;return p>(p=&q[1]);}F();",
      1L );
    ( "stored comparison and changed pointer",
      "I64 F(){I64 q[2];I64 *p=q;I64 r=p<(p=&q[1]);return \
       r*100+(p==&q[1]);}F();",
      101L );
    ( "loop offsets cross the bound",
      "I64 F(){I64 q[3];I64 *p=q;I64 \
       i=0,n=0;while(i<4){p=q+i;n+=p<&q[2];i++;}return n;}F();",
      2L );
    ( "one-past alias survives repeated sites",
      "I64 F(){I64 q[3];I64 *p=q,*saved=q+3;I64 \
       i=0;while(i<4){p=q+i;i++;}return p>=saved;}F();",
      1L );
    ( "recursive borrowed references",
      "I64 Rec(I64 *p,I64 n){I64 *r=p;if(n)return Rec(p,n-1);return r<p+1;}I64 \
       F(){I64 q[2];return Rec(q,3);}F();",
      1L );
    ( "global interior aliases",
      "I64 q[2];I64 F(){I64 *p=&q[1];return p>=q;}F();",
      1L );
    ( "static one-past aliases",
      "I64 F(){static I64 q[2];I64 *p=q+2;return p>&q[1];}F();",
      1L );
    ( "literal byte aliases and compatible spelling",
      "I64 F(){U8 *p=\"AB\";return p<&p[2];}F();",
      1L );
    ( "one original literal producer keeps its identity",
      "I64 F(){U8 *p,*saved;I64 \
       i=0;while(i<3){p=\"AB\";if(!i)saved=p;i++;}return p<=saved;}F();",
      1L );
    ( "literal one-past address",
      "I64 F(){U8 *p=\"AB\";return p+3>&p[2];}F();",
      1L );
    ( "rows retain the original object extent",
      "I64 F(){I64 q[2][2];return q[0]<q[1];}F();",
      1L );
    ( "row and flattened aliases",
      "I64 F(){I64 q[2][2];I64 *p=q;return q[1]>=p+2;}F();",
      1L );
    ( "direct multi-rank address order",
      "I64 F(){I64 q[2][2];return q<q;}F();",
      0L );
    ( "ordinary numeric order remains signed",
      "I64 F(){I64 q[1];return (-1<0)+(2>=3);}F();",
      1L );
    ( "ordered conditions retain pointer changes",
      "I64 F(){I64 q[2];I64 *p=q;if(p<&q[1])p=q+1;return p>=&q[1];}F();",
      1L );
    ( "effectful indexes retain left-right order",
      "I64 Index(I64 *n){(*n)++;return *n;}I64 F(){I64 q[3],n=0;I64 \
       r=(q+Index(&n))<(q+Index(&n));return r+n*10;}F();",
      21L );
    ( "canonical references survive repeated evaluations",
      "I64 F(){I64 q[2];I64 *p=q,*saved=q+1;I64 \
       i=0,n=0;while(i<30){p=q+1;n+=p<=saved;i++;}return n;}F();",
      30L );
    ( "call arguments preserve right-to-left comparison effects",
      "I64 Pair(I64 a,I64 b){return a*10+b;}I64 F(){I64 q[2];I64 \
       *p=q,*alias=q+1;return Pair(p<alias,p<(p=&q[1]));}F();",
      1L );
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
    ("I64 q[2];q<&q[1];", 1L);
    ("I64 q[2];q>=&q[1];", 0L);
    ("I64 q[2];40;(q+2)<=(q+2);", 1L);
    ("I64 q[2];42;if(0)q<q;", 42L);
  ]

let results () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, expected) ->
          let _, execution = success mode source in
          T.word "original I64 ordering result" VM.I64 expected execution)
        result_cases)
    modes

let rejected =
  [
    "I64 F(){I64 x;return &x<0;}F();";
    "I64 F(){I64 x;return 0>=&x;}F();";
    "I64 F(){I64 x;U64 y;return &x<&y;}F();";
    "I64 F(){I8 x;U8 y;return &x>=&y;}F();";
    "I64 F(){I64 x;F64 y;return &x<&y;}F();";
    "I64 F(){I64 x;return &x-&x;}F();";
    "I64 F(){I64 x;I64 *p=&x;I64 **q=&p;return q<=q;}F();";
    "class C{I64 x;};I64 F(){C c;return &c<=&c;}F();";
    "I64 F(){I64 q[3];return q<&q[1]<&q[2];}F();";
    "I64 F(){I64 x;return &x(I64)<&x;}F();";
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
          | _ -> Alcotest.fail "unsupported pointer ordering executed")
        rejected)
    modes

type expected_fault = Existing of E.expected_fault | Mismatch

let fault_code = function
  | Existing kind -> E.fault_code kind
  | Mismatch -> "HCIRVM0018"

let replace text pattern replacement =
  let result = Buffer.create (String.length text) in
  let rec visit index =
    if index < String.length text then
      if
        index + String.length pattern <= String.length text
        && String.sub text index (String.length pattern) = pattern
      then (
        Buffer.add_string result replacement;
        visit (index + String.length pattern))
      else (
        Buffer.add_char result text.[index];
        visit (index + 1))
  in
  visit 0;
  Buffer.contents result

let fault_cases =
  List.map
    (fun (source, kind, output) ->
      (replace (replace source "==" "<") "!=" ">=", Existing kind, output))
    E.fault_cases
  @ (operators
    |> List.map (fun (operator, _, _) ->
        ( "extern U0 Print(U8 *fmt,...);I64 Left(){Print(\"left\");return \
           0;}I64 Right(){Print(\"right\");return 0;}U0 F(){I64 \
           q[2],r[2];Print(\"kept\");&q[Left()]" ^ operator
          ^ "&r[Right()];}F();",
          Mismatch,
          "keptleftright" )))
  @ [
      ( "extern U0 Print(U8 *fmt,...);U0 F(){I64 \
         x,y;Print(\"kept\");&x<=&y;}F();",
        Mismatch,
        "kept" );
      ( "extern U0 Print(U8 *fmt,...);I64 q[1],r[1];U0 \
         F(){Print(\"kept\");q+1<r;}F();",
        Mismatch,
        "kept" );
      ( "extern U0 Print(U8 *fmt,...);U0 F(){U8 \
         *p=\"AB\";Print(\"kept\");p<\"AB\";}F();",
        Mismatch,
        "kept" );
      ( "extern U0 Print(U8 *fmt,...);U0 Rec(I64 *p,I64 n){I64 \
         x;if(n)Rec(&x,n-1);else{Print(\"kept\");p>=&x;}}U0 F(){I64 \
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
          | _ -> Alcotest.fail "invalid pointer ordering completed");
          Alcotest.(check string)
            "reached bytes" output
            (integer_program_report_output_bytes report))
        fault_cases)
    modes

let authority () =
  List.iter
    (fun mode ->
      List.iter
        (fun (operator, opcode, expected) ->
          let source = "I64 q[2];(q+0)" ^ operator ^ "(q+1);" in
          let original = A.fixture ~source mode
          and foreign = A.fixture ~source mode in
          A.valid_control ~expected:(List.nth expected 1) original;
          let runtime_calls = A.Unit.runtime_calls foreign.unit_ in
          A.rejects "foreign native ordering context"
            (A.compile ~runtime_calls original);
          A.rejects "foreign VM ordering context"
            (A.execute ~runtime_calls original);
          List.iter
            (fun transform ->
              let original = A.fixture ~source mode in
              let cell, comparison = A.find_cell original opcode in
              Obj.set_field (Obj.repr cell) 0 (Obj.repr (transform comparison));
              A.rejects "changed native ordering producer" (A.compile original);
              A.rejects "changed VM ordering producer" (A.execute original))
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
          let original = A.fixture ~source mode in
          let cell, address = A.find_cell original Ir_opcode.Ic_addr in
          Obj.set_field (Obj.repr cell) 0
            (Obj.repr
               { address with operands = List.map Fun.id address.operands });
          A.rejects "native copied transitive producer" (A.compile original);
          A.rejects "VM copied transitive producer" (A.execute original);
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
            | _ -> Alcotest.fail "missing original pointer operands"
          in
          let cell, numeric = Option.get !numeric in
          Obj.set_field (Obj.repr cell) 0
            (Obj.repr { numeric with opcode; operands });
          A.rejects "numeric producer cannot gain native ordering authority"
            (A.compile original);
          A.rejects "numeric producer cannot gain VM ordering authority"
            (A.execute original))
        operators)
    modes

let limit_source =
  "extern U0 Print(U8 *fmt,...);I64 F(){I64 q[2];I64 \
   *p=q;Print(\"kept\");return 41+(p<&q[1]);}F();"

let limits () =
  List.iter
    (fun mode ->
      let _, control = success mode limit_source in
      let steps = VM.executed_steps control in
      (match
         integer_program_report_outcome (run ~max_steps:steps mode limit_source)
       with
      | Ok checked -> T.word "exact ordering work" VM.I64 42L checked.value
      | Error ds -> Alcotest.fail (A.diagnostics ds));
      let below = run ~max_steps:(steps - 1) mode limit_source in
      (match integer_program_report_outcome below with
      | Error (first :: _) ->
          Alcotest.(check string) "one below runtime" "HCIRVM0007" first.code
      | _ -> Alcotest.fail "one-below ordering budget admitted");
      Alcotest.(check string)
        "reached bytes" "kept"
        (integer_program_report_output_bytes below))
    modes

let retained = replace E.retained "p==Q" "p<=Q"

let retained_values () =
  List.iter
    (fun mode ->
      let report, execution = success mode retained in
      T.word "saved original ordering" VM.I64 42L execution;
      Alcotest.(check string)
        "once-only operands" ""
        (integer_program_report_output_bytes report);
      let work = Option.get (integer_program_report_preparation_work report) in
      (match
         integer_program_report_outcome
           (run ~max_initializer_steps:work mode retained)
       with
      | Ok checked ->
          T.word "exact ordering preparation" VM.I64 42L checked.value
      | Error ds -> Alcotest.fail (A.diagnostics ds));
      match
        integer_program_report_outcome
          (run ~max_initializer_steps:(work - 1) mode retained)
      with
      | Error (_ :: _) -> ()
      | _ -> Alcotest.fail "one-below ordering preparation admitted")
    modes

let tests =
  [
    Alcotest.test_case "same object, all scalar widths, operators and effects"
      `Quick values;
    Alcotest.test_case "original I64 results and outer latch" `Quick results;
    Alcotest.test_case "explicit neighboring pointer domains" `Quick boundaries;
    Alcotest.test_case "object mismatches, original faults and work" `Quick
      faults;
    Alcotest.test_case "original comparisons and transitive authority" `Quick
      authority;
    Alcotest.test_case "exact runtime limits" `Quick limits;
    Alcotest.test_case "retained ordering defaults execute once" `Quick
      retained_values;
  ]
