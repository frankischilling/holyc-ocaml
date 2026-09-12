open Holyc_lib
module F = Test_integer_functions
module G = Test_integer_globals
module VM = Ir_integer_interpreter
module S = Test_integer_statics
module SI = Test_integer_static_initializers
module H = Test_ir_integer_interpreter
module Seq = Ir_instruction_sequence
module O = Ir_opcode
module T = Semantic_type

let gates =
  [
    ( "local-pointer-call",
      "I64 Read(U8 *p){return p[0]+p[1];}I64 F(){U8 *s=\"*\";return \
       Read(s);}F();" );
    ("direct-index", "I64 F(){return \"*\"[0];}F();");
    ("direct-argument", "I64 Read(U8 *p){return p[0]+p[1];}Read(\"*\");");
    ( "pointer-assignment-and-copy",
      "I64 F(){U8 *s,*p;s=\"*\";p=s;return p[0]+p[1];}F();" );
    ( "site-survives-calls",
      "I64 F(){U8 *s=\"(\";s[0]=s[0]+1;return s[0]+s[1];}F();F();" );
    ( "same-text-distinct-sites",
      "I64 F(){U8 *a=\"*\",*b=\"*\";a[0]=0;return b[0]+a[0];}F();" );
  ]

let payloads_and_terminators () =
  S.cases
    [
      ("I64 F(){return \"\"[0];}F();", 0L);
      ("I64 F(){return \"*\"[1];}F();", 0L);
      ( "I64 F(){U8 *s=\"A\\0B\";return s[0]*10000+s[1]*100+s[2]+s[3];}F();",
        650066L );
      ( "I64 F(){U8 *s=\"A\" \"\\0B\";return s[0]*10000+s[1]*100+s[2]+s[3];}F();",
        650066L );
      ("I64 F(){U8 *s=\"*\" \"\";return s[0]+s[1];}F();", 42L);
      ( Printf.sprintf "I64 F(){U8 *s=\"%c%c\";return s[0]*1000+s[1]+s[2];}F();"
          '\128' '\255',
        128255L );
      ("I64 F(){U8 *s=\"\";s[0]=298;return s[0];}F();", 42L);
      ("I64 F(){U8 *s=\"*\";s[1]=298;return s[1];}F();", 42L);
      ( "I64 F(){U8 *s=\"AB\";s[0]=256;s[1]=298;s[2]=-1;return \
         s[0]*1000000+s[1]*1000+s[2];}F();",
        42255L );
    ]

let aliases_classes_and_order () =
  S.cases
    [
      ("(\"*\"[0]+0);", 42L);
      ("I64 F(){return *\"*\";}F();", 42L);
      ("I64 F(){U8 *p;return (p=\"*\")[0];}F();", 42L);
      ("I64 F(){U8 *p,*q;q=p=\"*\";q[0]=298;return p[0];}F();", 42L);
      ("I64 F(){U8i *p=\"*\";U8 *q=p;return q[0]+q[1];}F();", 42L);
      ("I64 F(){U8 *p=\"*\";U8i *q=p;return q[0]+q[1];}F();", 42L);
      ( "I64 Read(U8i *p){return p[0]+p[1];}I64 F(){U8 *p=\"*\";return \
         Read(p);}F();",
        42L );
      ( "I64 Read(U8 *p){return p[0]+p[1];}I64 F(){U8i *p=\"*\";return \
         Read(p);}F();",
        42L );
      ( "I64 F(){U8 *a=\"A\",*b=\"B\",*p=a;p[((p=b)[0]=0)+0]=42;return \
         a[0]*100+b[0];}F();",
        4200L );
      ("I64 F(){U8 *s=\"A\",*p=s;*p=*(p=\"*\");return s[0];}F();", 42L);
      ("I64 F(){U8 *s=\"*\";I64 i=0;s[i++]=i;return s[0]+i+40;}F();", 42L);
      ("I64 F(){U8 *s=\"*\";I64 n=(s[0]=298);return n*1000+s[0];}F();", 298042L);
      ("I64 F(){U8 *s=\"*\";return (s[0]=-1);}F();", -1L);
      ( "I64 Set(U8 *p){p[0]=298;return 0;}I64 F(){U8 *s=\"A\";Set(s);return \
         s[0];}F();",
        42L );
      ("I64 F(){U8 *s=\"*\",*p=&s[1];return p[-1];}F();", 42L);
      ("I64 F(){U8 *s=\"*\",*p=&s[2];return p[-2]+p[-1];}F();", 42L);
    ]

let lifetime_and_fresh_images () =
  S.cases
    [
      ( "I64 A(){U8 *s=\"*\";s[0]=0;return 0;}I64 B(){U8 *s=\"*\";return \
         s[0];}A();B();",
        42L );
      ( "I64 Set(U8 *p){*p=0;return 0;}I64 F(){U8 \
         *a=\"*\",*b=\"*\";Set(a);return b[0]+a[0];}F();",
        42L );
      ( "I64 R(I64 n){U8 *s=\"(\";s[0]=s[0]+1;if(n)R(n-1);return s[0];}R(1);",
        42L );
      ( "I64 R(I64 n,U8 *p){if(n)return R(n-1,p);p[0]=298;return 0;}I64 F(){U8 \
         *s=\"A\";R(2,s);return s[0];}F();",
        42L );
      ("I64 B(){U8 *s=\"(\";s[0]=s[0]+1;return s[0];}I64 G=B();B();", 42L);
    ];
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let compiled = G.compile ~mode source in
          List.iter
            (fun () ->
              let result =
                SI.execute compiled |> H.require_ok H.show_vm_errors
              in
              Alcotest.(check int64)
                "a fresh image restores literal bytes" 42L
                (Option.get (VM.final_value result)).bits)
            [ (); () ])
        [
          List.assoc "site-survives-calls" gates;
          "I64 B(){U8 *s=\"(\";s[0]=s[0]+1;return s[0];}I64 G=B();B();";
        ])
    G.modes

let access_faults () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, code) ->
          let error = F.first_error (G.run ~mode source) in
          Alcotest.(check string) source code error.code;
          Alcotest.(check bool)
            "literal access fault retains the function" true
            (List.mem "function=F" error.notes);
          Alcotest.(check bool)
            "literal access fault retains a source span" true
            (error.primary.stop > error.primary.start))
        [
          ("I64 F(){U8 *s=\"\";return s[1];}F();", "HCIRVM0019");
          ("I64 F(){U8 *s=\"*\",*neighbor=\"*\";return s[2];}F();", "HCIRVM0019");
          ("I64 F(){U8 *s=\"*\";s[-1]=42;return 42;}F();", "HCIRVM0019");
          ("I64 F(){U8 *s=\"*\",*p=&s[2];return *p;}F();", "HCIRVM0019");
          ("I64 F(){U8 *s=\"*\",*p=&s[3];return 42;}F();", "HCIRVM0019");
          ("I64 F(){U8 *s=\"*\";U64 n=-1;return s[n];}F();", "HCIRVM0020");
          ( "I64 F(){U8 *s=\"*\",*p=&s[1];return p[0x7FFFFFFFFFFFFFFF];}F();",
            "HCIRVM0020" );
          ("I64 F(){U8 *s=\"*\";s[2]=1/0;return 42;}F();", "HCIRVM0009");
        ])
    G.modes

let explicit_boundaries () =
  List.iter
    (fun mode ->
      List.iter
        (fun source -> ignore (F.first_error (G.run ~mode source)))
        [
          "I64 F(){I64 *s=\"*\";return *s;}F();";
          "I64 F(){U64 *s=\"*\";return *s;}F();";
          "I64 F(){U8 **s=\"*\";return 42;}F();";
          "I64 F(){U8 *s=\"*\";return s(I64);}F();";
          "U8 *F(){return \"*\";}42;";
          "I64 F(){U8 *s=\"*\";return s;}F();";
          "I64 F(){U8 *s=\"*\";s++;return *s;}F();";
          "I64 F(){I64 n=42;U64i *p=&n;return *p;}F();";
        ];
      List.iter
        (fun source ->
          ignore (F.first_error (Test_integer_expression.evaluate ~mode source)))
        [ "(\"*\");"; "(\"*\"[0]+0);" ])
    G.modes

let run ?(mode = Preprocessor.Jit) ?(max_literal_bytes = 1_048_576)
    ?(max_frame_bytes = 1024) ?(max_call_depth = 16) ?(max_steps = 10000) source
    =
  let session, config, source = F.inputs ~mode source in
  run_integer_program ~max_literal_bytes ~max_frame_bytes ~max_call_depth
    ~max_steps session ~config ~source

let execute ?functions ?(max_literal_bytes = 1_048_576) compiled =
  VM.execute_program ~max_literal_bytes
    ~globals:(integer_program_globals compiled)
    ~initialization:(integer_program_initialization compiled)
    ~functions:
      (Option.value functions ~default:(integer_program_functions compiled))
    ~max_steps:10000 ~max_frame_bytes:1024 ~max_call_depth:16
    (integer_program_entry compiled)

let resource_limits () =
  List.iter
    (fun mode ->
      let string_initializer = "I64 G=\"*\"[0]+0;G;" in
      ignore (run ~mode ~max_literal_bytes:2 string_initializer |> F.expect 42L);
      List.iter
        (fun (result, code, stage) ->
          let error = F.first_error result in
          Alcotest.(check string)
            "entry initializer literal fault" code error.code;
          List.iter
            (fun note ->
              Alcotest.(check bool) note true (List.mem note error.notes))
            [
              "initializer=G";
              "stage=" ^ stage;
              ("initializer_phase="
              ^
              match mode with
              | Preprocessor.Jit -> "compile-initializer"
              | Aot -> "load-initializer");
            ])
        [
          ( run ~mode ~max_literal_bytes:1 string_initializer,
            "HCIRVM0021",
            "preflight" );
          (run ~mode "I64 G=\"*\"[2]+0;G;", "HCIRVM0019", "execution");
        ];
      List.iter
        (fun (source, bytes, frame, depth) ->
          let result =
            run ~mode ~max_literal_bytes:bytes ~max_frame_bytes:frame
              ~max_call_depth:depth source
            |> F.expect 42L
          in
          let steps = VM.executed_steps result in
          ignore
            (run ~mode ~max_literal_bytes:bytes ~max_frame_bytes:frame
               ~max_call_depth:depth ~max_steps:steps source
            |> F.expect 42L);
          Alcotest.(check string)
            "one byte below the literal image limit"
            (if bytes = 1 then "HCIRVM0001" else "HCIRVM0021")
            (F.first_error (run ~mode ~max_literal_bytes:(bytes - 1) source))
              .code;
          Alcotest.(check string)
            "one step below the reached literal path" "HCIRVM0007"
            (F.first_error (run ~mode ~max_steps:(steps - 1) source)).code)
        [
          ("I64 F(){return \"\"[0]+42;}F();", 1, 1, 1);
          (List.assoc "local-pointer-call" gates, 2, 16, 2);
          (List.assoc "site-survives-calls" gates, 2, 8, 1);
          (List.assoc "same-text-distinct-sites" gates, 4, 16, 1);
          ("I64 F(){U8 *s=\"A\\0B\";return s[1]+s[3]+42;}F();", 4, 8, 1);
          ("I64 F(){U8 *s=\"*\" \"\";return s[0]+s[1];}F();", 2, 8, 1);
          ( "I64 R(I64 n){U8 *s=\"(\";s[0]=s[0]+1;if(n)R(n-1);return s[0];}R(1);",
            2,
            32,
            2 );
        ];
      ignore
        (run ~mode ~max_literal_bytes:max_int
           (List.assoc "local-pointer-call" gates)
        |> F.expect 42L);
      List.iter
        (fun limit ->
          Alcotest.(check string)
            "invalid literal limit precedes parsing" "HCIRVM0001"
            (F.first_error (run ~mode ~max_literal_bytes:limit "@invalid")).code)
        [ 0; -1 ];
      List.iter
        (fun (source, owner, bytes) ->
          let compiled = G.compile ~mode source in
          ignore
            (execute ~max_literal_bytes:bytes compiled
            |> H.require_ok H.show_vm_errors);
          let errors =
            match execute ~max_literal_bytes:(bytes - 1) compiled with
            | Ok _ ->
                Alcotest.fail "an unreachable literal escaped its image budget"
            | Error errors -> errors
          in
          S.require_preflight (Error errors);
          let error =
            List.find (fun (e : VM.error) -> e.code = "HCIRVM0021") errors
          in
          Alcotest.(check (option string))
            "capacity fault owner" (Some owner) error.function_name;
          List.iter
            (fun (label, present) -> Alcotest.(check bool) label true present)
            [
              ("capacity instruction", Option.is_some error.instruction_id);
              ("capacity block", Option.is_some error.block_id);
              ("capacity source", Option.is_some error.span);
            ])
        [
          ("I64 Never(){return \"abc\"[0];}42;", "Never", 4);
          ("I64 F(){if(0)return \"abc\"[0];return 42;}F();", "F", 4);
          ("I64 A(){return \"*\"[0];}I64 B(){return \"*\"[0];}42;", "B", 4);
        ];
      List.iter
        (fun (result, expected) ->
          Alcotest.(check string)
            "literal storage has a separate budget" expected
            (F.first_error result).code)
        [
          ( run ~mode ~max_literal_bytes:2 ~max_frame_bytes:15
              (List.assoc "local-pointer-call" gates),
            "HCIRVM0011" );
          ( run ~mode ~max_literal_bytes:2 ~max_call_depth:1
              (List.assoc "local-pointer-call" gates),
            "HCIRVM0015" );
        ])
    G.modes

let raw_execution_and_graph_boundary () =
  List.iter
    (fun mode ->
      let compiled =
        G.compile ~mode "I64 F(){U8 *s=\"*\";s[0]=s[0]+1;return s[0];}42;"
      in
      let fn = List.hd (integer_program_functions compiled) in
      List.iter
        (fun () ->
          let result =
            VM.execute_function ~max_literal_bytes:2 ~max_steps:100
              ~max_frame_bytes:8 ~frame:fn.frame ~arguments:[] fn.body
            |> H.require_ok H.show_vm_errors
          in
          match VM.termination result with
          | VM.Returned (Some word) ->
              Alcotest.(check int64)
                "raw executions start fresh literal images" 43L word.bits
          | _ -> Alcotest.fail "raw literal function did not return")
        [ (); () ];
      S.require_preflight
        (VM.execute_function ~max_literal_bytes:1 ~max_steps:100
           ~max_frame_bytes:8 ~frame:fn.frame ~arguments:[] fn.body);
      let compiled = G.compile ~mode (List.assoc "direct-index" gates) in
      let fn = List.hd (integer_program_functions compiled) in
      let literal =
        Ir_function_body.x87 fn.body
        |> SI.code
        |> List.find (fun (d : Seq.description) -> d.opcode = O.Ic_str_const)
      in
      match VM.execute ~max_steps:100 (Ir_function_body.x87 fn.body) with
      | Ok _ -> Alcotest.fail "graph-only execution acquired a literal image"
      | Error errors ->
          Alcotest.(check bool)
            "graph-only execution rejects the literal producer itself" true
            (List.exists
               (fun (e : VM.error) ->
                 e.code = "HCIRVM0002"
                 && e.instruction_id
                    = Some (Seq.Instruction_id.to_int literal.instruction_id))
               errors))
    G.modes

let malformed_producers_and_owners () =
  let public_u8 = H.primitive_type ~form:T.Public_spelling Primitive_type.U8 in
  let public_pointer = T.pointer_to public_u8 |> Result.get_ok in
  let wrong_pointer = T.pointer_to H.public_u64 |> Result.get_ok in
  List.iter
    (fun mode ->
      let source = "I64 F(){I64 n=1;return \"*\"[0]+n-1;}F();" in
      let compiled = G.compile ~mode source in
      ignore (G.run ~mode source |> F.expect 42L);
      let functions = integer_program_functions compiled in
      let rewrite transform =
        let functions =
          List.map
            (fun (fn : VM.function_definition) ->
              let descriptions = Ir_function_body.x87 fn.body |> SI.code in
              { fn with body = S.rebuild (transform descriptions) fn.body })
            functions
        in
        S.require_preflight (execute ~functions compiled)
      in
      List.iter
        (fun transform ->
          rewrite (fun _ (d : Seq.description) ->
              if d.opcode = O.Ic_str_const then transform d else d))
        [
          (fun d -> { d with Seq.payload = None });
          (fun d -> { d with Seq.payload = Some (Seq.Integer 42L) });
          (fun d -> { d with Seq.target_type = Some public_pointer });
          (fun d -> { d with Seq.target_type = Some wrong_pointer });
          (fun d -> { d with Seq.flags = 1L });
          (fun d -> { d with Seq.flags = 0x2000L });
        ];
      let descriptions =
        Ir_function_body.x87 (List.hd functions).body |> SI.code
      in
      let word =
        List.find
          (fun (d : Seq.description) ->
            d.opcode = O.Ic_imm_i64 && d.target_type = Some H.i64)
          descriptions
      in
      let literal =
        List.find
          (fun (d : Seq.description) -> d.opcode = O.Ic_str_const)
          descriptions
      in
      (match
         Seq.create
           [
             word;
             { literal with operands = [ (Option.get word.result).value_id ] };
           ]
       with
      | Ok _ -> Alcotest.fail "a string producer accepted an operand"
      | Error errors ->
          Alcotest.(check bool)
            "the sequence constructor rejects extra literal operands" true
            (List.exists
               (fun (error : Seq.error) ->
                 error.code = "HCIR0004"
                 && error.instruction_id
                    = Some (Seq.Instruction_id.to_int literal.instruction_id))
               errors));
      rewrite (fun _ (d : Seq.description) ->
          if d.opcode = O.Ic_deref && d.target_type = Some H.u8 then
            { d with target_type = Some public_u8 }
          else d);
      let foreign =
        G.compile ~mode source |> integer_program_functions |> List.hd
      in
      let fn = List.hd functions in
      S.require_preflight
        (execute ~functions:[ { fn with frame = foreign.frame } ] compiled);
      let source =
        "I64 Read(U8 *p){return p[0]+p[1];}I64 F(){return Read(\"*\");}F();"
      in
      let compiled = G.compile ~mode source in
      ignore (G.run ~mode source |> F.expect 42L);
      let functions =
        integer_program_functions compiled
        |> List.map (fun (fn : VM.function_definition) ->
            {
              fn with
              body =
                S.rebuild
                  (fun (d : Seq.description) ->
                    if d.opcode = O.Ic_str_const then { d with flags = 0L }
                    else d)
                  fn.body;
            })
      in
      S.require_preflight (execute ~functions compiled))
    G.modes

let tests =
  List.map
    (fun (name, source) ->
      Alcotest.test_case name `Quick (fun () ->
          List.iter
            (fun mode -> ignore (G.run ~mode source |> F.expect 42L))
            G.modes))
    gates
  @ [
      Alcotest.test_case "literal payloads and initialized terminators" `Quick
        payloads_and_terminators;
      Alcotest.test_case "literal aliases source classes and evaluation order"
        `Quick aliases_classes_and_order;
      Alcotest.test_case "persistent sites recursion and fresh execution images"
        `Quick lifetime_and_fresh_images;
      Alcotest.test_case "literal object bounds and overflow" `Quick
        access_faults;
      Alcotest.test_case "pointer conversions escapes and graph-only boundaries"
        `Quick explicit_boundaries;
      Alcotest.test_case "exact literal budgets and unreachable image planning"
        `Quick resource_limits;
      Alcotest.test_case "raw function images and graph-only rejection" `Quick
        raw_execution_and_graph_boundary;
      Alcotest.test_case "malformed string producers pointees flags and owners"
        `Quick malformed_producers_and_owners;
    ]
