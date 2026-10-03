open Holyc_lib
module A = Test_internal_strlen_authority
module D = Test_pointer_difference
module T = Test_pointer_bit_internals
module Calls = Ir_runtime_call_context
module VM = Ir_integer_interpreter

let wrap ?(declarations = "I64 Q[4];") ?(offset = 3) ?(count = 1)
    ?(helpers = "") body =
  Printf.sprintf
    "#exe {%s I64 Offset=%d,N=0;%s I64 Init(){%s}I64 Saved(I64 \
     x=Init()){return \
     x;}if(N!=%d||Saved()!=42)Print(\"bad\");Offset=0;N=0;if(Saved()!=42||N)Print(\"bad\");StreamPrint(\"%%d;\",Saved());}"
    declarations offset helpers body count

let type_cases =
  T.storage_types
  |> List.concat_map (fun (type_, _, _) ->
      [
        (3, "39+(p-Q)");
        (3, "45+(Q-p)");
        (3, "42+(p-p)");
        (4, "38+(p-Q)");
        (4, "46+(Q-p)");
      ]
      |> List.map (fun (offset, expression) ->
          ( type_,
            wrap ~declarations:(type_ ^ " Q[4];") ~offset
              ("N++;" ^ type_ ^ " *p=Q+Offset;return " ^ expression ^ ";") )))

let call_cases =
  [
    wrap ~offset:0 "N++;I64 *p=Q;return 43+(p-(p=Q+1));";
    wrap ~helpers:"I64 Diff(I64 *p,I64 *q){return p-q;}"
      "N++;return 39+Diff(Q+3,Q);";
    wrap
      ~helpers:
        "I64 Rec(I64 *p,I64 *q,I64 n){if(n)return Rec(p,q,n-1);return p-q;}"
      "N++;return 45+Rec(Q,Q+3,3);";
    wrap ~count:2 ~helpers:"I64 Index(){N++;return N;}"
      "I64 *p=Q+Index(),*q=Q+Index();return 43+(p-q);";
    wrap ~declarations:"I64 Q[2][2];" "N++;return 40+(Q[1]-Q[0]);";
    wrap ~declarations:"I64 Q[2][2];" "N++;I64 *p=Q;return 43+((p+1)-Q[1]);";
    wrap
      "N++;I64 *p=Q,*saved=Q+3;I64 i=0,n=0;while(i<4){p=Q+i;n+=p-Q;i++;}return \
       36+n+(p-saved);";
    wrap "N++;I64 x;return 41+((&x+1)-&x);";
  ]

let values () =
  List.iter
    (fun mode ->
      List.iter
        (fun (_, source) ->
          let report, execution = D.success mode source in
          T.word "original saved I64 difference" VM.I64 42L execution;
          Alcotest.(check string)
            "once-only operands and no pointee reads" ""
            (integer_program_report_output_bytes report))
        (type_cases @ List.map (fun source -> ("callee", source)) call_cases))
    T.modes

let descriptions fixture =
  A.Unit.entry fixture.Native_scalar_fixture.unit_
  |> Ir_x87_stack.graph |> A.Graph.blocks
  |> List.concat_map (fun b -> A.Graph.instructions b |> A.Seq.instructions)
  |> List.map A.Seq.description

let divisions fixture =
  Calls.original_pointer_difference_divisions
    (A.Unit.runtime_calls fixture.Native_scalar_fixture.unit_)
    ~owner:Calls.Entry

let collected fixture =
  match divisions fixture with
  | Some collected -> collected
  | None -> Alcotest.fail "original graph did not yield its division collection"

let find_id fixture id =
  let rec find = function
    | [] -> None
    | instruction :: rest as cell ->
        let description = A.Seq.description instruction in
        if description.instruction_id = id then Some (cell, description)
        else find rest
  in
  A.Unit.entry fixture.Native_scalar_fixture.unit_
  |> Ir_x87_stack.graph |> A.Graph.blocks
  |> List.find_map (fun block ->
      find (A.Graph.instructions block |> A.Seq.instructions))
  |> Option.get

let changed_width_proofs mode =
  let mutations =
    [
      ( "division flags",
        `Divide,
        fun (d : A.Seq.description) -> { d with flags = 1L } );
      ( "division payload",
        `Divide,
        fun d -> { d with payload = Some (A.Seq.Integer 8L) } );
      ("division target", `Divide, fun d -> { d with target_type = None });
      ( "division operand order",
        `Divide,
        fun d -> { d with operands = List.rev d.operands } );
      ( "compatible numeric operand",
        `Divide,
        fun d ->
          { d with operands = [ List.hd d.operands; List.hd d.operands ] } );
      ("subtraction flags", `Subtract, fun d -> { d with flags = 1L });
      ( "subtraction operand order",
        `Subtract,
        fun d -> { d with operands = List.rev d.operands } );
      ( "compatible pointer operand",
        `Subtract,
        fun d ->
          { d with operands = [ List.hd d.operands; List.hd d.operands ] } );
      ("subtraction target", `Subtract, fun d -> { d with target_type = None });
      ( "different scalar size",
        `Size,
        fun d -> { d with payload = Some (A.Seq.Integer 2L) } );
      ("size flags", `Size, fun d -> { d with flags = 1L });
      ( "copied exact size",
        `Size,
        fun d -> { d with operands = List.map Fun.id d.operands } );
    ]
  in
  List.iter
    (fun (label, selected, transform) ->
      let changed = A.fixture ~source:"I64 q[4];(q+3)-q;" mode in
      let before = collected changed in
      let _, divide = A.find_cell changed Ir_opcode.Ic_div in
      Alcotest.(check bool)
        (label ^ " original control")
        true
        (Calls.is_original_pointer_difference_division before divide);
      let bytes, size =
        match divide.operands with
        | [ bytes; size ] -> (bytes, size)
        | _ -> Alcotest.fail "difference omitted size division"
      in
      let id =
        match selected with
        | `Divide -> divide.instruction_id
        | `Subtract | `Size ->
            let value_id = if selected = `Subtract then bytes else size in
            descriptions changed
            |> List.find (fun (d : A.Seq.description) ->
                Option.fold ~none:false
                  ~some:(fun result -> result.A.Seq.value_id = value_id)
                  d.result)
            |> fun d -> d.instruction_id
      in
      let cell, original = find_id changed id in
      Obj.set_field (Obj.repr cell) 0 (Obj.repr (transform original));
      Alcotest.(check bool)
        (label ^ " cannot collect new authority")
        true
        (Option.is_none (divisions changed));
      A.rejects ("native " ^ label) (A.compile changed);
      A.rejects ("VM " ^ label) (A.execute changed))
    mutations;
  let foreign =
    A.fixture
      ~source:
        "I64 Difference(I64 *p,I64 *q){return p-q;}I64 q[4];Difference(q+3,q);"
      mode
  in
  let original = A.fixture ~source:"I64 q[4];(q+3)-q;" mode in
  let body = List.hd (A.Unit.functions foreign.unit_) in
  Alcotest.(check bool)
    "foreign function owner cannot collect entry proof" true
    (Option.is_none
       (Calls.original_pointer_difference_divisions
          (A.Unit.runtime_calls original.unit_)
          ~owner:(Calls.Function body.VM.body)))

let authority () =
  List.iter
    (fun mode ->
      changed_width_proofs mode;
      List.iter
        (fun (type_, _, _) ->
          let source = type_ ^ " q[4];(q+3)-q;" in
          let original = A.fixture ~source mode
          and foreign = A.fixture ~source mode in
          A.valid_control ~expected:3L original;
          let received = collected original in
          let ds =
            descriptions original
            |> List.filter (fun (d : A.Seq.description) ->
                d.opcode = Ir_opcode.Ic_div)
          in
          let expected = not (List.mem type_ [ "I8"; "U8"; "Bool" ]) in
          Alcotest.(check int)
            "optional original size division"
            (if expected then 1 else 0)
            (List.length ds);
          List.iter
            (fun d ->
              Alcotest.(check bool)
                "original record" true
                (Calls.is_original_pointer_difference_division received d);
              Alcotest.(check bool)
                "copied record cannot qualify" false
                (Calls.is_original_pointer_difference_division received
                   { d with operands = List.map Fun.id d.operands });
              Alcotest.(check bool)
                "foreign collection cannot qualify" false
                (Calls.is_original_pointer_difference_division
                   (collected foreign) d))
            ds;
          if expected then
            List.iter
              (fun opcode ->
                let changed = A.fixture ~source mode in
                let before = collected changed in
                let _, divide = A.find_cell changed Ir_opcode.Ic_div in
                let cell, d = A.find_cell changed opcode in
                Obj.set_field (Obj.repr cell) 0
                  (Obj.repr { d with operands = List.map Fun.id d.operands });
                (match divisions changed with
                | None -> ()
                | Some _ ->
                    Alcotest.fail "changed transitive graph yielded a new proof");
                if opcode <> Ir_opcode.Ic_div then
                  Alcotest.(check bool)
                    "unchanged original record remains in snapshot" true
                    (Calls.is_original_pointer_difference_division before divide);
                A.rejects
                  "native entry rechecks the source bundle after collection"
                  (A.compile changed);
                A.rejects "VM entry rechecks the source bundle after collection"
                  (A.execute changed))
              [ Ir_opcode.Ic_div; Ir_opcode.Ic_sub; Ir_opcode.Ic_addr ])
        T.storage_types)
    T.modes

let user_divisions () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, expected, authorized) ->
          let original = A.fixture ~source mode in
          A.valid_control ~expected original;
          let proof = collected original in
          let ds =
            descriptions original
            |> List.filter (fun (d : A.Seq.description) ->
                d.opcode = Ir_opcode.Ic_div)
          in
          Alcotest.(check int)
            "only original matching size divisions qualify" authorized
            (List.length
               (List.filter
                  (Calls.is_original_pointer_difference_division proof)
                  ds)))
        [
          ("84/2;", 42L, 0);
          ("I8 q[3];(q-(q+1))/2;", 0L, 0);
          ("I64 q[3];((q+3)-q)/2;", 1L, 1);
        ])
    T.modes

let rejected =
  [
    wrap "N++;return 84/2;";
    wrap "N++;return -1/2;";
    wrap "N++;return 9%2;";
    wrap "N++;return N<<2;";
    wrap "N++;return -N>>1;";
    wrap "N++;I64 *p=Q;return (p-(Q+1))/2;";
    wrap ~declarations:"U8 Q[4];" "N++;U8 *p=Q;return (p-(Q+1))/2;";
    "#exe {I64 Q[4];I64 Saved(I64 x=(Q-Q)){return \
     x;}StreamPrint(\"%d;\",Saved());}";
  ]

let gates () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          match integer_program_report_outcome (D.run mode source) with
          | Error (first :: _) ->
              Alcotest.(check string)
                "existing initializer optimizer gate" "HCRUN0006" first.code
          | _ -> Alcotest.fail "unrelated initializer arithmetic admitted")
        rejected)
    T.modes

let fault_cases =
  [
    ("I64 *p;return p-Q;", "HCIRVM0012", 4, 9, 6);
    ("return (Q+5)-Q;", "HCIRVM0019", 4, 12, 9);
    ("I64 r[4];return Q-r;", "HCIRVM0018", 5, 12, 9);
    ("return (Q+0x4000000000000000)-Q;", "HCIRVM0020", 4, 10, 7);
  ]

let faults () =
  List.iter
    (fun mode ->
      List.iter
        (fun (body, code, prep, steps, instruction) ->
          let source =
            "#exe {I64 Q[4],N=0;I64 Init(){N++;" ^ body
            ^ "}I64 Saved(I64 x=Init()){return \
               x;}StreamPrint(\"%d;\",Saved());}"
          in
          let report = D.run mode source in
          (match integer_program_report_outcome report with
          | Error (first :: _) ->
              Alcotest.(check string) "original reached fault" code first.code;
              List.iter
                (fun note ->
                  Alcotest.(check bool)
                    ("retained " ^ note) true
                    (List.mem note first.notes))
                [
                  "stage=execution";
                  "function=Init";
                  "executed_steps=" ^ string_of_int steps;
                  "instruction_id=" ^ string_of_int instruction;
                ]
          | _ -> Alcotest.fail "invalid prepared reference executed");
          Alcotest.(check (option int))
            "earlier preparation work survives" (Some prep)
            (integer_program_report_preparation_work report);
          Alcotest.(check string)
            "no generated success or pointee bytes" ""
            (integer_program_report_output_bytes report))
        fault_cases)
    T.modes

let limits () =
  List.iter
    (fun mode ->
      let source = D.retained_wide in
      let report, execution = D.success mode source in
      T.word "wide original default" VM.I64 42L execution;
      let prep = Option.get (integer_program_report_preparation_work report) in
      let runtime = VM.executed_steps execution in
      (match
         integer_program_report_outcome
           (D.run ~max_initializer_steps:prep ~max_steps:runtime mode source)
       with
      | Ok checked ->
          T.word "exact preparation and runtime" VM.I64 42L checked.value
      | Error ds -> Alcotest.fail (A.diagnostics ds));
      List.iter
        (fun report ->
          match integer_program_report_outcome report with
          | Error (first :: _) ->
              Alcotest.(check string)
                "one below work limit" "HCIRVM0007" first.code
          | _ -> Alcotest.fail "one-below preparation/runtime limit admitted")
        [
          D.run ~max_initializer_steps:(prep - 1) mode source;
          D.run ~max_steps:(runtime - 1) mode source;
        ])
    T.modes

let output_report ?max_output_bytes ?max_output_work mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"prepared-difference-output.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
  in
  run_integer_program_report ?max_output_bytes ?max_output_work session ~config
    ~source ~max_steps:100_000

let effects_and_output_limits () =
  List.iter
    (fun mode ->
      let source =
        D.replace D.retained_wide "N++;I64 *p" "Print(\"kept\");N++;I64 *p"
      in
      let control = output_report mode source in
      (match integer_program_report_outcome control with
      | Ok checked ->
          T.word "prepared output retains saved word" VM.I64 42L checked.value
      | Error ds -> Alcotest.fail (A.diagnostics ds));
      Alcotest.(check string)
        "prepared captured bytes" "kept"
        (integer_program_report_output_bytes control);
      let bytes = String.length (integer_program_report_output_bytes control)
      and work = integer_program_report_output_work control in
      (match
         integer_program_report_outcome
           (output_report ~max_output_bytes:bytes ~max_output_work:work mode
              source)
       with
      | Ok checked -> T.word "exact output limits" VM.I64 42L checked.value
      | Error ds -> Alcotest.fail (A.diagnostics ds));
      List.iter
        (fun (report, code) ->
          match integer_program_report_outcome report with
          | Error (first :: _) ->
              Alcotest.(check string) "one below output limit" code first.code
          | _ -> Alcotest.fail "one-below prepared output limit admitted")
        [
          (output_report ~max_output_bytes:(bytes - 1) mode source, "HCIRVM0022");
          (output_report ~max_output_work:(work - 1) mode source, "HCIRVM0023");
        ];
      let source =
        "#exe {I64 Q[4],N=0;I64 Right(){Print(\"right\");return 0;}I64 \
         Init(){Print(\"kept\");N++;I64 *p;return p-&Q[Right()];}I64 Saved(I64 \
         x=Init()){return x;}StreamPrint(\"%d;\",Saved());}"
      in
      let failed = output_report mode source in
      (match integer_program_report_outcome failed with
      | Error (first :: _) ->
          Alcotest.(check string)
            "left failure prevents right call" "HCIRVM0012" first.code
      | _ -> Alcotest.fail "unknown prepared left reference executed");
      Alcotest.(check string)
        "reached capture survives failure" "kept"
        (integer_program_report_output_bytes failed);
      Alcotest.(check int)
        "reached formatting work survives failure" 9
        (integer_program_report_output_work failed))
    T.modes

let tests =
  [
    Alcotest.test_case "all widths, signed offsets and retained call paths"
      `Quick values;
    Alcotest.test_case "original collection and current/transitive authority"
      `Quick authority;
    Alcotest.test_case "user divisions do not qualify" `Quick user_divisions;
    Alcotest.test_case "generic optimizer and direct-expression gates" `Quick
      gates;
    Alcotest.test_case "original preparation fault phases and charged work"
      `Quick faults;
    Alcotest.test_case "exact preparation and runtime limits" `Quick limits;
    Alcotest.test_case "reached output and exact output limits" `Quick
      effects_and_output_limits;
  ]
