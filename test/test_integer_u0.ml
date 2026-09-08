open Holyc_lib
module F = Test_integer_functions
module G = Test_integer_globals
module VM = Ir_integer_interpreter
module S = Test_integer_statics
module SI = Test_integer_static_initializers
module H = Test_ir_integer_interpreter
module Seq = Ir_instruction_sequence
module O = Ir_opcode

let gates =
  [
    ("source-gate", "I64 G;U0 Set(I64 n){G=n;return;}Set(42);G;");
    ("fallthrough", "I64 G;U0 Set(I64 n){G=n;}Set(42);G;");
    ( "nested-void-and-word-callers",
      "I64 G;U0 Set(I64 n){G=n;return;}U0 Outer(){Set(40);return;}I64 \
       F(){Outer();return G+2;}F();" );
    ( "caller-pointer-writeback",
      "U0 Set(U8 *p){*p=298;return;}I64 F(){U8 n;Set(&n);return n;}F();" );
  ]

let control_flow_and_arguments () =
  S.cases
    [
      ("I64 G;U0 Set(I64 n){if(n){G=42;return;}G=7;}Set(1);G;", 42L);
      ("I64 G;U0 Set(I64 n){if(n)G=42;else G=7;}Set(1);G;", 42L);
      ("I64 G;U0 Set(){G=42;return;G=7;1/0;}Set();G;", 42L);
      ( "I64 G;U0 Set(){I64 \
         i=0;while(i<5){i++;if(i==3){G=42;return;}}G=7;}Set();G;",
        42L );
      ("I64 G=40;U0 Tick(){G++;}U0 Twice(){Tick();Tick();}Twice();G;", 42L);
      ( "I64 G;U0 Save(I64 a,I64 b){G=a*10+b;}I64 F(){I64 \
         n=0;Save(n=1,n=2);return G*10+n;}F();",
        121L );
      ( "I64 G;U0 Save(I64 a,I64 b){G=a*10+b;}I64 Put(I64 *p,I64 \
         n){*p=n;return n;}I64 F(){I64 n;Save(Put(&n,1),Put(&n,2));return \
         G*10+n;}F();",
        121L );
      ( "U0 Set(I64 *p){*p=42;}I64 F(){I64 a[2];Set(&a[1]);return a[1];}F();",
        42L );
      ( "U0 Set(U8 *p){p[0]=298;}I64 F(){U8 *s=\"A\";Set(s);return s[0];}F();",
        42L );
      ("I64 G;U0 Tick(){U8 *s=\"(\";s[0]=s[0]+1;G=s[0];}Tick();Tick();G;", 42L);
    ];
  List.iter
    (fun mode ->
      ignore
        (G.run ~mode
           "U0 Set(U64 *p){*p=-1;}U64 F(){U64 n;Set(&n);return n;}F();"
        |> F.expect ~type_:VM.U64 (-1L)))
    G.modes

let no_value_reporting () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let result = (F.checked (G.run ~mode source)).value in
          Alcotest.(check bool)
            "void calls resume to stream end" true
            (VM.termination result = VM.Stream_end);
          Alcotest.(check bool)
            "void expression clears the preceding word" true
            (VM.final_value result = None))
        [
          "U0 V(){return;}V();";
          "U0 V(){}42;V();";
          "I64 G;U0 Set(I64 n){G=n;}42;Set(1);";
          "I64 Good(){return 42;}U0 V(){Good();}42;V();";
          "U0 V(){}U0 Outer(){V();}42;Outer();";
        ];
      ignore (G.run ~mode "U0 V(){}V();42;" |> F.expect 42L);
      ignore (G.run ~mode "U0 V(){}I64 F(){V();return 42;}F();" |> F.expect 42L))
    G.modes

let initializers_and_fresh_images () =
  S.cases
    [
      ("I64 G=0;U0 Set(){G=42;}I64 Seed(){Set();return G;}I64 H=Seed();H;", 42L);
      ( "I64 G=0;U0 Set(){G=42;}I64 Seed(){Set();return G;}I64 F(){static I64 \
         n=Seed();return n;}F();",
        42L );
      ("I64 G;U0 Set(){G=42;}I64 Seed(){Set();return G;}7;I64 H=Seed();", 7L);
      ("I64 G;U0 Tick(){static I64 n=40;n++;G=n;}Tick();Tick();G;", 42L);
    ];
  List.iter
    (fun mode ->
      let compiled =
        G.compile ~mode
          "I64 G;U0 Tick(){static I64 n=40;n++;G=n;}Tick();Tick();G;"
      in
      List.iter
        (fun () ->
          let result = SI.execute compiled |> H.require_ok H.show_vm_errors in
          Alcotest.(check int64)
            "void calls use each fresh persistent image" 42L
            (Option.get (VM.final_value result)).bits)
        [ (); () ])
    G.modes

let raw_returns () =
  List.iter
    (fun mode ->
      List.iter
        (fun source ->
          let compiled = G.compile ~mode source in
          let fn = List.hd (integer_program_functions compiled) in
          let result =
            VM.execute_function ~max_steps:100 ~max_frame_bytes:8
              ~frame:fn.frame ~arguments:[] fn.body
            |> H.require_ok H.show_vm_errors
          in
          Alcotest.(check bool)
            "a void function completes without a word" true
            (VM.termination result = VM.Returned None);
          Alcotest.(check bool)
            "void completion has no final word" true
            (VM.final_value result = None))
        [ "U0 V(){return;}42;"; "U0 V(){}42;"; "U0 V(){I64 n=42;n;}42;" ])
    G.modes

let numeric_and_missing_return_boundaries () =
  List.iter
    (fun mode ->
      List.iter
        (fun source -> ignore (F.first_error (G.run ~mode source)))
        [
          "U0 V(){}(V()+1);";
          "U0 V(){}if(V())42;";
          "U0 V(){}I64 F(){I64 n;n=V();return n;}F();";
          "U0 V(){}I64 F(){I64 n=V();return n;}F();";
          "U0 V(){}I64 Take(I64 n){return n;}Take(V());";
          "U0 V(){}I64 F(){return V();}F();";
          "U0 V(){}I64 G=V();G;";
          "U0 V(){return 42;}V();";
          "U0 V(){}U0 F(){return V();}F();";
        ];
      List.iter
        (fun source ->
          let error = F.first_error (G.run ~mode source) in
          Alcotest.(check string)
            "missing word return retains its hosted fault" "HCIRVM0013"
            error.code;
          Alcotest.(check bool)
            "missing return identifies the active callee" true
            (List.mem "function=Bad" error.notes))
        [
          "I64 Bad(){return;}Bad();";
          "U64 Bad(){}Bad();";
          "I64 Good(){return 42;}I64 Bad(){Good();}Bad();";
          "I64 Good(){return 42;}U0 V(){Good();}I64 Bad(){V();}Bad();";
        ])
    G.modes

let reached_faults () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, is_initializer) ->
          let error = F.first_error (G.run ~mode source) in
          Alcotest.(check string)
            "void callee faults after earlier writes" "HCIRVM0009" error.code;
          List.iter
            (fun note ->
              Alcotest.(check bool) note true (List.mem note error.notes))
            ([ "function=Fail"; "stage=execution" ]
            @
            if is_initializer then
              [ "initializer=H"; "initializer_phase=" ^ SI.phase mode ]
            else []);
          Alcotest.(check bool)
            "void fault retains its source" true
            (error.primary.stop > error.primary.start))
        [
          ("I64 G=0;U0 Set(){G=42;}U0 Fail(){Set();1/(G-42);}Fail();", false);
          ( "I64 G=0;U0 Set(){G=42;}U0 Fail(){Set();1/(G-42);}I64 \
             Seed(){Fail();return 0;}I64 H=Seed();42;",
            true );
        ])
    G.modes

let resource_limits () =
  List.iter
    (fun mode ->
      List.iter
        (fun (source, bytes, depth, global_bytes) ->
          let result =
            G.run ~mode ~max_frame_bytes:bytes ~max_call_depth:depth
              ~max_global_bytes:global_bytes source
            |> F.expect 42L
          in
          let steps = VM.executed_steps result in
          ignore
            (G.run ~mode ~max_frame_bytes:bytes ~max_call_depth:depth
               ~max_global_bytes:global_bytes ~max_steps:steps source
            |> F.expect 42L);
          List.iter
            (fun (result, code) ->
              Alcotest.(check string)
                "one below an exact void-call budget" code
                (F.first_error result).code)
            [
              (G.run ~mode ~max_frame_bytes:(bytes - 1) source, "HCIRVM0011");
              ( G.run ~mode ~max_call_depth:(depth - 1) source,
                if depth = 1 then "HCIRVM0001" else "HCIRVM0015" );
              (G.run ~mode ~max_steps:(steps - 1) source, "HCIRVM0007");
            ])
        [
          (List.assoc "source-gate" gates, 8, 1, 8);
          (List.assoc "caller-pointer-writeback" gates, 16, 2, 1);
          ("I64 G=39;U0 R(I64 n){if(n){R(n-1);G++;}}R(3);G;", 32, 4, 8);
          ( "I64 G=0;U0 R(I64 n){I64 saved=n;if(n)R(n-1);G+=saved;}R(3);G+36;",
            64,
            4,
            8 );
          ( "U0 R(I64 n,I64 *p){if(n){R(n-1,p);*p+=1;}}I64 F(){I64 \
             n=39;R(3,&n);return n;}F();",
            72,
            5,
            1 );
        ];
      Alcotest.(check string)
        "global storage remains separately bounded" "HCIRVM0016"
        (F.first_error
           (G.run ~mode ~max_global_bytes:7 (List.assoc "source-gate" gates)))
          .code)
    G.modes

let malformed_call_and_discard_joins () =
  let module T = Semantic_type in
  let public_u0 = H.primitive_type ~form:T.Public_spelling Primitive_type.U0 in
  let internal_u0 = H.primitive_type Primitive_type.U0 in
  List.iter
    (fun mode ->
      let source =
        "U0 V(I64 n){return;}I64 F(){I64 n=7;V(42);return n+35;}F();"
      in
      let compiled = G.compile ~mode source in
      let functions = integer_program_functions compiled in
      ignore (G.run ~mode source |> F.expect 42L);
      let rewrite transform =
        let functions =
          List.map
            (fun (fn : VM.function_definition) ->
              let descriptions = Ir_function_body.x87 fn.body |> SI.code in
              { fn with body = S.rebuild (transform descriptions) fn.body })
            functions
        in
        S.require_preflight (SI.execute ~functions compiled)
      in
      let void_end (d : Seq.description) =
        d.opcode = O.Ic_call_end && d.target_type = Some public_u0
      in
      List.iter
        (fun transform ->
          rewrite (fun _ d -> if void_end d then transform d else d))
        [
          (fun d -> { d with Seq.target_type = Some H.public_i64 });
          (fun d -> { d with Seq.target_type = Some internal_u0 });
          (fun d -> { d with Seq.payload = None });
          (fun d -> { d with Seq.payload = Some (Seq.Integer 0L) });
          (fun d -> { d with Seq.flags = 1L });
          (fun d -> { d with Seq.flags = 0x2000L });
        ];
      List.iter
        (fun transform ->
          rewrite (fun _ (d : Seq.description) ->
              if
                d.target_type = Some public_u0
                && List.mem d.opcode [ O.Ic_add_rsp; O.Ic_add_rsp1 ]
              then transform d
              else d))
        [
          (fun d -> { d with Seq.payload = Some (Seq.Integer 16L) });
          (fun d -> { d with Seq.target_type = Some H.public_i64 });
          (fun d ->
            {
              d with
              Seq.opcode =
                (if d.opcode = O.Ic_add_rsp then O.Ic_add_rsp1 else O.Ic_add_rsp);
            });
        ];
      List.iter
        (fun transform ->
          rewrite (fun descriptions (d : Seq.description) ->
              if
                d.opcode = O.Ic_end_exp
                && List.exists
                     (fun producer ->
                       void_end producer
                       && d.operands = [ (Option.get producer.result).value_id ])
                     descriptions
              then transform d
              else d))
        [
          (fun d -> { d with Seq.target_type = Some H.i64 });
          (fun d -> { d with Seq.flags = Int64.logor d.flags 1L });
          (fun d -> { d with Seq.payload = Some (Seq.Integer 0L) });
        ];
      rewrite (fun _ (d : Seq.description) ->
          if d.opcode = O.Ic_imm_i64 && d.target_type = Some H.i64 then
            { d with target_type = Some public_u0 }
          else d);
      let caller =
        List.find
          (fun (fn : VM.function_definition) ->
            Ir_function_body.x87 fn.body |> SI.code |> List.exists void_end)
          functions
      in
      let descriptions = Ir_function_body.x87 caller.body |> SI.code in
      let call_end = List.find void_end descriptions in
      List.iter
        (fun (description, code) ->
          match Seq.create [ description ] with
          | Ok _ -> Alcotest.fail "malformed call-end shape passed construction"
          | Error errors ->
              Alcotest.(check bool)
                "call-end shape rejected before graph publication" true
                (List.exists (fun (e : Seq.error) -> e.code = code) errors))
        [
          ({ call_end with result = None }, "HCIR0005");
          ( {
              call_end with
              operands = [ (Option.get call_end.result).value_id ];
            },
            "HCIR0004" );
        ];
      let foreign = G.compile ~mode source |> integer_program_functions in
      S.require_preflight
        (SI.execute
           ~functions:
             (List.map2
                (fun (fn : VM.function_definition)
                     (other : VM.function_definition) ->
                  { fn with frame = other.frame })
                functions foreign)
           compiled))
    G.modes

let literal_and_preparation_limits () =
  List.iter
    (fun mode ->
      let run ?(max_literal_bytes = 1_048_576) ?(max_initializer_steps = 10000)
          source =
        let session, config, source = F.inputs ~mode source in
        run_integer_program ~max_literal_bytes ~max_initializer_steps
          ~max_steps:10000 session ~config ~source
      in
      List.iter
        (fun (source, owner) ->
          ignore (run ~max_literal_bytes:2 source |> F.expect 42L);
          let error = F.first_error (run ~max_literal_bytes:1 source) in
          Alcotest.(check string)
            "void literal image has an exact independent byte limit"
            "HCIRVM0021" error.code;
          List.iter
            (fun note ->
              Alcotest.(check bool) note true (List.mem note error.notes))
            [ "function=" ^ owner; "stage=preflight"; "executed_steps=0" ];
          Alcotest.(check bool)
            "void literal capacity retains its source" true
            (error.primary.stop > error.primary.start))
        [
          ( "I64 G;U0 Tick(){U8 *s=\"(\";s[0]=s[0]+1;G=s[0];}Tick();Tick();G;",
            "Tick" );
          ("U0 Never(){U8 *s=\"*\";}42;", "Never");
        ];
      let source =
        "I64 G;U0 Tick(){static I64 n=40;n++;G=n;}Tick();Tick();G;"
      in
      let result = run ~max_initializer_steps:4 source |> F.expect 42L in
      Alcotest.(check int)
        "void static initializer retains its separate preparation count" 4
        (VM.compiled_initializer_steps result);
      let error = F.first_error (run ~max_initializer_steps:3 source) in
      Alcotest.(check string)
        "one below the static preparation budget" "HCIRVM0007" error.code;
      List.iter
        (fun note ->
          Alcotest.(check bool) note true (List.mem note error.notes))
        [
          "initializer=n";
          "initializer_phase=constant-preparation";
          "compiled_initializer_steps=3";
        ])
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
      Alcotest.test_case "void control flow pointer effects and argument order"
        `Quick control_flow_and_arguments;
      Alcotest.test_case "void results clear preceding expression words" `Quick
        no_value_reporting;
      Alcotest.test_case "void calls inside initializers and fresh images"
        `Quick initializers_and_fresh_images;
      Alcotest.test_case "standalone void functions return no value" `Quick
        raw_returns;
      Alcotest.test_case "numeric void use and missing word return boundaries"
        `Quick numeric_and_missing_return_boundaries;
      Alcotest.test_case "void faults retain effects and initializer provenance"
        `Quick reached_faults;
      Alcotest.test_case "exact void-call frame depth and step budgets" `Quick
        resource_limits;
      Alcotest.test_case
        "malformed void producers call cleanup discard and owners" `Quick
        malformed_call_and_discard_joins;
      Alcotest.test_case "void literal and static preparation resource limits"
        `Quick literal_and_preparation_limits;
    ]
