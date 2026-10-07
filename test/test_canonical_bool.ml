open Holyc_lib
module VM = Ir_integer_interpreter
module A = Test_internal_strlen_authority
module Primitive = Primitive_type
module Type = Semantic_type

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]
let declaration = "public _intern 0x1d Bool ToBool(I64 i);"

let run ?(max_steps = 100_000) ?(max_initializer_steps = 100_000) mode contents
    =
  let session = Session.create () in
  let source = Session.add_source session ~path:"canonical-bool.hc" ~contents in
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
  let truth label expression expected =
    (label, declaration ^ expression, VM.I64, expected)
  in
  let scalar label source expected = (label, source, VM.I64, expected) in
  [
    truth "canonical zero" "ToBool(0);" 0L;
    truth "canonical positive" "ToBool(42);" 1L;
    truth "canonical negative" "ToBool(-1);" 1L;
    truth "canonical high bit" "ToBool(0x8000000000000000);" 1L;
    truth "canonical mixed bits" "ToBool(0x123400000000);" 1L;
    truth "computed Bool input retains full word"
      "Bool Input(){return 0x100;}ToBool(Input());" 1L;
    truth "stored Bool input narrows" "Bool input=0x100;ToBool(input);" 0L;
    truth "stored negative Bool is true" "Bool input=0x80;ToBool(input);" 1L;
    truth "computed U8 input retains full word"
      "U8 Input(){return 0x100;}ToBool(Input());" 1L;
    truth "stored U8 input narrows" "U8 input=0x100;ToBool(input);" 0L;
    truth "nested original calls" "ToBool(ToBool(0x100));" 1L;
    truth "recursive original caller"
      "Bool F(I64 n){if(n)return ToBool(F(n-1));return ToBool(42);}F(4);" 1L;
    truth "effectful argument runs once"
      "I64 N=0;I64 Input(){N++;return 42;}ToBool(Input())+N;" 2L;
    scalar "computed Bool high byte" "Bool F(){return 0x180;}F();" 384L;
    scalar "computed Bool full word" "Bool F(){return 0x8000000000000000;}F();"
      Int64.min_int;
    scalar "stored Bool sign extension" "Bool q=0x180;q;" (-128L);
    scalar "stored Bool all byte bits" "Bool q=0xff;q;" (-1L);
    scalar "stored Bool zero low byte" "Bool q=0x100;q;" 0L;
    scalar "Bool parameter entry" "Bool F(Bool q){return q;}F(0x180);" (-128L);
    scalar "computed return narrowed at parameter entry"
      "Bool Input(){return 0x180;}Bool F(Bool q){return q;}F(Input());" (-128L);
    scalar "Bool arrays use signed byte leaves"
      "Bool q[2];q[0]=0x80;q[1]=0x7f;q[0]+q[1];" (-1L);
    scalar "prepared Bool array leaves" "Bool q[2]={0x80,0x7f};q[0]+q[1];" (-1L);
    scalar "automatic Bool storage" "I64 F(){Bool q=0x180;return q;}F();"
      (-128L);
    scalar "Bool statics persist"
      "Bool F(){static Bool q=0x80;return q++;}F()+F();" (-255L);
    scalar "Bool static arrays"
      "I64 F(){static Bool q[2];q[0]=1;q[1]=0x80;return q[0]+q[1];}F()+F();"
      (-254L);
    scalar "prepared Bool static array leaves"
      "I64 F(){static Bool q[2]={0x80,0x7f};return q[0]+q[1];}F()+F();" (-2L);
    scalar "Bool signed computation class" "Bool q=0x80;q/2;" (-64L);
    scalar "Bool signed right shift" "Bool q=0x80;q>>1;" (-64L);
    scalar "Bool signed comparison" "Bool q=0x80;q<0;" 1L;
    scalar "Bool full returned unary bits" "Bool F(){return 0x180;}-F();"
      (-384L);
    scalar "Bool full returned complement bits" "Bool F(){return 84;}(~F())/2;"
      (-42L);
    scalar "assignment returns full computed bits" "Bool q;q=0x180;" 384L;
    scalar "later load narrows assigned bits" "Bool q;q=0x180;q;" (-128L);
    scalar "compound returns full computed bits" "Bool q=0x7f;q+=1;" 128L;
    scalar "compound store narrows" "Bool q=0x7f;q+=1;q;" (-128L);
    scalar "prefix update returns stored byte" "Bool q=0x7f;++q;" (-128L);
    scalar "postfix update returns old byte" "Bool q=0x7f;q++;" 127L;
    scalar "Bool aliases preserve caller storage"
      "I64 Write(Bool *p){*p=0x180;return *p;}I64 F(){Bool q=0;Bool \
       *p=&q;return Write(p)+q;}F();"
      (-256L);
    scalar "Bool parameter exposes one byte"
      "I64 F(Bool q){return sizeof(q);}F(42);" 1L;
    scalar "Bool primitive size stays one" "sizeof(Bool);" 1L;
    scalar "Bool constant default entry" "Bool F(Bool q=0x180){return q;}F();"
      (-128L);
    scalar "Bool omitted positions"
      "I64 F(Bool a=0x80,I64 b=42,Bool c=0x7f){return a+b+c;}F(,43,);" 42L;
    ( "supported U8 form remains distinct",
      "public _intern 0x1d U8 Truth(I64 i);Truth(42);",
      VM.U64,
      1L );
    scalar "renamed canonical numeric binding"
      "public _intern 0x1d Bool Truth(I64 i);Truth(42);" 1L;
    scalar "ordinary spelling uses body"
      "Bool ToBool(I64 i){return i+1;}ToBool(0x180);" 385L;
    scalar "original macro target"
      "#define OP 0x1d\npublic _intern OP Bool Truth(I64 i);Truth(42);" 1L;
  ]

let cases =
  ordinary_cases
  @ List.init 64 (fun index ->
      let source =
        Printf.sprintf "%sToBool(0x%Lx);" declaration
          (Int64.shift_left 1L index)
      in
      ("each truth input bit " ^ string_of_int index, source, VM.I64, 1L))

let values () =
  List.iter
    (fun mode ->
      List.iter
        (fun (label, source, type_, expected) ->
          let _, execution = success mode source in
          word label type_ expected execution)
        cases)
    modes

let backing_and_identity () =
  let info = Primitive.info Primitive.Bool in
  Alcotest.(check bool)
    "distinct Boolean category" true
    (info.category = Primitive.Boolean);
  Alcotest.(check int) "audited byte extent" 1 info.byte_size;
  Alcotest.(check string) "audited raw class" "RT_I8" info.raw_name;
  let backing = Primitive.integer_storage_info Primitive.Bool |> Option.get in
  Alcotest.(check bool)
    "I8 backing primitive" true
    (backing.primitive = Primitive.I8);
  Alcotest.(check bool) "signed backing" false backing.raw_is_unsigned;
  List.iter
    (fun primitive ->
      Alcotest.(check bool)
        "unsupported scalar backing" true
        (Option.is_none (Primitive.integer_storage_info primitive)))
    [ Primitive.I0; Primitive.U0; Primitive.F64 ];
  let public primitive =
    Type.make_primitive ~form:Type.Public_spelling ~primitive ~pointer_depth:0
    |> Result.get_ok
  in
  let bool = public Primitive.Bool in
  List.iter
    (fun primitive ->
      Alcotest.(check bool)
        "public Bool is not an integer alias" false
        (Type.equal bool (public primitive)))
    [ Primitive.I8; Primitive.U8 ];
  match
    Type.make_primitive ~form:Type.Internal_storage ~primitive:Primitive.Bool
      ~pointer_depth:0
  with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "manufactured an internal Bool spelling"

let rejected =
  [
    "_intern 0x1d I8 F(I64 i);F(42);";
    "_intern 0x1d I64 F(I64 i);F(42);";
    "_intern 0x1d Bool F(Bool i);F(42);";
    "_intern 0x1d Bool F(U64 i);F(42);";
    "_intern 0x1d Bool F(I64 i=42);F();";
    "_intern 0x1d Bool F(I64 i,...);F(42);";
    "_intern 0x1d Bool *F(I64 i);F(42);";
    "_intern 0x1d Bool F(I64 *i);I64 i=42;F(&i);";
    "_intern (0x1d) Bool F(I64 i);F(42);";
    "_intern 0x6d Bool F(I64 i);F(42);";
  ]

let signatures () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          match integer_program_report_outcome (run mode source) with
          | Error (_ :: _) -> ()
          | _ -> Alcotest.fail ("invalid canonical call executed: " ^ source))
        (if mode = Preprocessor.Jit then
           List.filter
             (fun source ->
               not (Internal_binding_cases.is_parenthesized source))
             rejected
         else rejected))
    modes

let authority () =
  let source = declaration ^ "ToBool(42);" in
  List.iter
    (fun mode ->
      let original = A.fixture ~source mode
      and foreign = A.fixture ~source mode in
      A.valid_control ~expected:1L original;
      let context = A.Unit.runtime_calls foreign.unit_ in
      A.rejects "foreign native context"
        (A.compile ~runtime_calls:context original);
      A.rejects "foreign VM context" (A.execute ~runtime_calls:context original);
      List.iter
        (fun transform ->
          let original = A.fixture ~source mode in
          let cell, description = A.find_cell original Ir_opcode.Ic_to_bool in
          Obj.set_field (Obj.repr cell) 0 (Obj.repr (transform description));
          A.rejects "changed native canonical operation" (A.compile original);
          A.rejects "changed VM canonical operation" (A.execute original))
        [
          (fun (d : A.Seq.description) -> { d with flags = 1L });
          (fun d -> { d with operands = [] });
          (fun d -> { d with operands = d.operands @ d.operands });
          (fun d ->
            { d with result = Some { A.Seq.value_id = List.hd d.operands } });
          (fun d -> { d with payload = Some (A.Seq.Integer 42L) });
          (fun d -> { d with target_type = None });
          (fun d -> { d with opcode = Ir_opcode.Ic_toupper });
        ];
      List.iter
        (fun transform ->
          let original = A.fixture ~source mode in
          let cell, description = A.find_cell original Ir_opcode.Ic_imm_i64 in
          Obj.set_field (Obj.repr cell) 0 (Obj.repr (transform description));
          A.rejects "changed native canonical producer" (A.compile original);
          A.rejects "changed VM canonical producer" (A.execute original))
        [
          (fun (d : A.Seq.description) -> { d with span = None });
          (fun d -> (Obj.obj (Obj.dup (Obj.repr d)) : A.Seq.description));
        ])
    modes

let fault_cases =
  [
    ( declaration
      ^ "extern U0 Print(U8 *fmt,...);Bool F(){Bool q;Print(\"kept\");return \
         ToBool(q);}F();",
      "HCIRVM0012",
      "kept" );
    ( declaration
      ^ "extern U0 Print(U8 *fmt,...);Bool Input(){Bool \
         q;Print(\"right\");return q;}Print(\"kept\");ToBool(Input());",
      "HCIRVM0012",
      "keptright" );
    ( "extern U0 Print(U8 *fmt,...);I64 F(Bool q){Bool \
       *p=&q;Print(\"kept\");return p[1];}F(42);",
      "HCIRVM0019",
      "kept" );
    ( "extern U0 Print(U8 *fmt,...);Print(\"kept\");Bool q[1];q[0]=42;q[1];",
      "HCIRVM0019",
      "kept" );
  ]

let faults () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, code, output) ->
          let failed = run mode source in
          (match integer_program_report_outcome failed with
          | Error (first :: _) ->
              Alcotest.(check string) "original reached fault" code first.code
          | _ -> Alcotest.fail "invalid Bool access completed");
          Alcotest.(check string)
            "original effects" output
            (integer_program_report_output_bytes failed))
        fault_cases)
    modes

let limit_source =
  declaration ^ "extern U0 Print(U8 *fmt,...);Print(\"kept\");ToBool(0x100);"

let limits () =
  List.iter
    (fun mode ->
      let control, execution = success mode limit_source in
      let steps = VM.executed_steps execution in
      (match
         integer_program_report_outcome (run ~max_steps:steps mode limit_source)
       with
      | Ok checked -> word "exact canonical quota" VM.I64 1L checked.value
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
  ^ "I64 N=0;I64 Input(){N++;return 0x100;}Bool Saved(Bool \
     x=ToBool(Input())){return \
     x;}N=0;if(Saved()!=1||N)Print(\"bad\");StreamPrint(\"%d;\",Saved()*42);}"

let retained_values () =
  List.iter
    (fun mode ->
      let report, execution = success mode retained in
      word "retained canonical default" VM.I64 42L execution;
      Alcotest.(check string)
        "owning default runs once" ""
        (integer_program_report_output_bytes report);
      let prep = Option.get (integer_program_report_preparation_work report) in
      (match
         integer_program_report_outcome
           (run ~max_initializer_steps:prep mode retained)
       with
      | Ok checked -> word "exact original prep" VM.I64 42L checked.value
      | Error errors -> Alcotest.fail (A.diagnostics errors));
      let below = run ~max_initializer_steps:(prep - 1) mode retained in
      (match integer_program_report_outcome below with
      | Error (_ :: _) -> ()
      | _ -> Alcotest.fail "one-below prep admitted");
      Alcotest.(check int)
        "failed prep keeps work" (prep - 1)
        (Option.get (integer_program_report_preparation_work below)))
    modes

let tests =
  [
    Alcotest.test_case "distinct Bool identity and audited integer backing"
      `Quick backing_and_identity;
    Alcotest.test_case "canonical calls and complete Bool scalar paths" `Quick
      values;
    Alcotest.test_case "original canonical signatures" `Quick signatures;
    Alcotest.test_case "foreign, changed and copied call authority" `Quick
      authority;
    Alcotest.test_case "Bool storage faults and original effects" `Quick faults;
    Alcotest.test_case "exact canonical runtime quotas" `Quick limits;
    Alcotest.test_case "original retained Bool defaults" `Quick retained_values;
  ]
