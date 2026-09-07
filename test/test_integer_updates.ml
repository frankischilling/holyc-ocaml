open Holyc_lib
module F = Test_integer_functions
module G = Test_integer_globals
module VM = Ir_integer_interpreter
module Seq = Ir_instruction_sequence

let source =
  "I64 Total=0;I64 AddTo(I64 n){Total+=n;return \
   Total;}AddTo(20);AddTo(22);Total;"

let cases examples =
  List.iter
    (fun mode ->
      List.iter
        (fun (text, expected) -> ignore (G.run ~mode text |> F.expect expected))
        examples)
    G.modes

let source_gate () =
  cases
    [
      (source, 42L);
      ("I64 G=0;I64 Next(){return G++;}(Next()*100+Next()*10+G);", 12L);
      ("I64 Count(){I64 n=0;for(;n<42;n++){}return n;}Count();", 42L);
    ]

let compound () =
  cases
    [
      ("I64 G=1;(G+=(G=2));", 4L);
      ("I64 G;G+=(G=2);G;", 4L);
      ("I64 G=1;G+=G++;G;", 3L);
      ("I64 G=1;G+=++G;G;", 4L);
      ("I64 G=1;I64 Set(){G=20;return 22;}(G+=Set());", 42L);
      ("I64 F(){I64 n=1;return (n+=(n=2));}F();", 4L);
      ("I64 F(){I64 n;n+=(n=2);return n;}F();", 4L);
      ("I64 G=3;(G*=14);", 42L);
      ("I64 G=84;(G/=2);", 42L);
      ("I64 G=85;(G%=43);", 42L);
      ("I64 G=44;(G-=2);", 42L);
      ("I64 G=63;(G&=42);", 42L);
      ("I64 G=40;(G|=2);", 42L);
      ("I64 G=43;(G^=1);", 42L);
      ("I64 G=21;(G<<=1);", 42L);
      ("I64 G=84;(G>>=1);", 42L);
      ("I64 G=20;((G)+=22);", 42L);
    ]

let old_new () =
  cases
    [
      ("I64 G=41;(++G);", 42L);
      ("I64 G=43;(--G);", 42L);
      ("I64 G=42;(G++);", 42L);
      ("I64 G=42;(G--);", 42L);
      ("I64 G=41;((G)++);G;", 42L);
      ("I64 G=0;I64 Sub(I64 a,I64 b){return a-b;}(Sub(G++,G++)*10+G);", 12L);
      ("I64 G=41;I64 F(){I64 G=0;return ++G;}(F()+G);", 42L);
      ("I64 F(){I64 n=0;while(n<40)++n;do{n++;}while(n<42);return n;}F();", 42L);
      ("I64 G=0;if(0&&G++){}if(1||G++){}G;", 0L);
      ("I64 G=0;(0&&G++);G;", 1L);
      ("I64 G=1;(0<G++<3);G;", 2L);
    ]

let signedness () =
  List.iter
    (fun mode ->
      List.iter
        (fun (text, type_, expected) ->
          ignore (G.run ~mode text |> F.expect ~type_ expected))
        [
          ("I64 G=-6;U64 D=2;(G/=D);", VM.I64, -3L);
          ("I64 G=-7;U64 D=3;(G%=D);", VM.I64, -1L);
          ("U64 G=-1;I64 D=2;(G/=D);", VM.U64, Int64.max_int);
          ("U64 G=-1;I64 D=2;(G%=D);", VM.U64, 1L);
          ("I64 G=-4;U64 D=1;(G>>=D);", VM.I64, -2L);
          ("U64 G=-1;I64 D=1;(G>>=D);", VM.U64, Int64.max_int);
          ("I64 G=21;(G<<=65);", VM.I64, 42L);
          ("U64 G=-1;(++G);", VM.U64, 0L);
          ("U64 G=0;(--G);", VM.U64, -1L);
          ("U64 G=-1;(G++);", VM.U64, -1L);
          ("I64 G=0x7FFFFFFFFFFFFFFF;(++G);", VM.I64, Int64.min_int);
          ("I64 G=0x8000000000000000;(--G);", VM.I64, Int64.max_int);
          ("I64 G=0x7FFFFFFFFFFFFFFF;(G+=1);", VM.I64, Int64.min_int);
          ("I64 F(){U64 n=-1;return n++;}F();", VM.I64, -1L);
        ])
    G.modes

let initializers () =
  cases
    [
      ("I64 G=1;I64 H=G++;(H*10+G);", 12L);
      ("I64 G=1;I64 H=++G;(H*10+G);", 22L);
      ("I64 G=1;I64 H=(G+=2);(H*10+G);", 33L);
      ("I64 G=1;I64 Next(){return G++;}I64 H=Next();(H*10+G);", 12L);
      ("I64 G=84,D=2;I64 H=(G/=D);(H);", 42L);
      ("I64 G=-3,D=2;I64 H=(G%=D);H;", -1L);
      ("7;I64 G=1;I64 H=G++;", 7L);
    ];
  List.iter
    (fun mode ->
      let compiled = G.compile ~mode "I64 G=1;I64 H=G++;H;" in
      let slots = Ir_integer_globals.slots (integer_program_globals compiled) in
      Alcotest.(check (list bool))
        "only the pure initializer is materialized" [ true; false ]
        (List.map Ir_integer_globals.slot_initializer_materialized slots);
      Alcotest.(check int)
        "one scheduled update initializer" 1
        (List.length
           (Ir_global_initialization.regions
              (integer_program_initialization compiled)));
      Alcotest.(check int)
        "update is not part of constant preparation" 3
        (Integer_initializer_preparation.executed_steps
           (integer_program_initializer_preparation compiled));
      let error =
        F.first_error
          (G.run ~mode "I64 G=1;I64 Fail(I64 d){return G/=d;}I64 H=Fail(0);")
      in
      Alcotest.(check string) "initializer update fault" "HCIRVM0009" error.code;
      List.iter
        (fun note ->
          Alcotest.(check bool) note true (List.mem note error.notes))
        [
          "function=Fail";
          "initializer=H";
          ("initializer_phase="
          ^
          if mode = Preprocessor.Jit then "compile-initializer"
          else "load-initializer");
        ])
    G.modes

let initializer_optimizer_boundary () =
  List.iter
    (fun mode ->
      List.iter
        (fun text ->
          Alcotest.(check string)
            "compound optimizer boundary" "HCRUN0006"
            (F.first_error (G.run ~mode text)).code)
        [
          "I64 G=-3;I64 H=(G/=2);H;";
          "I64 G=-3;I64 H=(G%=(1+1));H;";
          "I64 G=1,D=2;I64 H=(G<<=D);H;";
          "I64 G=1;I64 H=(G>>=1);H;";
          "I64 Half(I64 n){return n/=2;}I64 G=Half(-3);G;";
          "I64 Mod(I64 n){return n%=2;}I64 Outer(I64 n){return Mod(n);}I64 \
           G=Outer(-3);G;";
        ])
    G.modes

let faults_and_limits () =
  List.iter
    (fun text ->
      Alcotest.(check string)
        "unknown update read" "HCIRVM0012" (F.first_error (G.run text)).code)
    [
      "I64 G;(G++);";
      "I64 G;(++G);";
      "I64 G;(G+=1);";
      "I64 F(){I64 n;return n++;}F();";
    ];
  ignore (G.run ~mode:Preprocessor.Aot "I64 G;(G++);G;" |> F.expect 1L);
  List.iter
    (fun mode ->
      List.iter
        (fun (text, code) ->
          let error = F.first_error (G.run ~mode text) in
          Alcotest.(check string) "faulting update" code error.code;
          Alcotest.(check bool)
            "fault names its compound opcode" true
            (String.starts_with
               ~prefix:
                 (if String.contains text '/' then "IC_DIV_EQU"
                  else "IC_MOD_EQU")
               error.message);
          Alcotest.(check int)
            "update operator span"
            (String.index text (if String.contains text '/' then '/' else '%'))
            error.primary.start)
        [
          ("I64 G=42;(G/=0);", "HCIRVM0009");
          ("I64 G=42;(G%=0);", "HCIRVM0009");
          ("I64 G=0x8000000000000000;(G/=(-1));", "HCIRVM0010");
          ("I64 G=0x8000000000000000;(G%=(-1));", "HCIRVM0010");
        ];
      let result =
        G.run ~mode ~max_global_bytes:8 ~max_frame_bytes:8 ~max_call_depth:1
          source
        |> F.expect 42L
      in
      let steps = VM.executed_steps result in
      ignore (G.run ~mode ~max_steps:steps source |> F.expect 42L);
      List.iter
        (fun (run, code) ->
          Alcotest.(check string)
            "bounded update program" code (F.first_error run).code)
        [
          (G.run ~mode ~max_steps:(steps - 1) source, "HCIRVM0007");
          (G.run ~mode ~max_global_bytes:7 source, "HCIRVM0016");
          (G.run ~mode ~max_frame_bytes:7 source, "HCIRVM0011");
        ])
    G.modes

let preflight () =
  let module H = Test_ir_integer_interpreter in
  List.iter
    (fun mode ->
      List.iter
        (fun text ->
          let compiled = G.compile ~mode text in
          let globals = integer_program_globals compiled in
          let entry = integer_program_entry compiled in
          let execute ?(globals = globals) graph =
            VM.execute_program ~globals ~functions:[] ~max_steps:1000
              ~max_frame_bytes:8 ~max_call_depth:1 graph
          in
          let rejects = function
            | Ok _ -> Alcotest.fail "malformed update executed"
            | Error errors ->
                List.iter
                  (fun (e : VM.error) ->
                    Alcotest.(check bool)
                      "preflight" true (e.stage = VM.Preflight);
                    Alcotest.(check int) "no effects" 0 e.executed_steps)
                  errors
          in
          let is_update d =
            match d.Seq.opcode with
            | Ir_opcode.Ic_add_equ | Ic__pp | Ic_pp_ -> true
            | _ -> false
          in
          let literal = ref None in
          ignore
            (F.rewrite_entry
               (fun d ->
                 (match (d.Seq.payload, d.result) with
                 | Some (Seq.Integer 42L), Some result ->
                     literal := Some result.value_id
                 | _ -> ());
                 d)
               entry);
          List.iter
            (fun rewrite ->
              rejects
                (execute
                   (F.rewrite_entry
                      (fun d -> if is_update d then rewrite d else d)
                      entry)))
            [
              (fun d -> { d with Seq.target_type = Some H.public_u64 });
              (fun d -> { d with Seq.flags = 1L });
              (fun d -> { d with Seq.payload = Some (Seq.Integer 0L) });
              (fun d ->
                {
                  d with
                  Seq.operands = Option.get !literal :: List.tl d.operands;
                });
            ];
          let foreign = G.compile ~mode text |> integer_program_globals in
          rejects (execute ~globals:foreign entry);
          List.iter
            (fun _ ->
              match execute entry with
              | Ok result ->
                  Alcotest.(check int64)
                    "fresh execution" 43L
                    (Option.get (VM.final_value result)).bits
              | Error errors -> Alcotest.fail (List.hd errors).message)
            [ (); () ];
          Alcotest.(check string)
            "deterministic update dump"
            (integer_program_human compiled)
            (integer_program_human (G.compile ~mode text)))
        [ "I64 G;G=42;G+=1;G;"; "I64 G;G=42;G++;G;"; "I64 G;G=42;++G;G;" ])
    G.modes

let unsupported () =
  List.iter
    (fun mode ->
      List.iter
        (fun text -> ignore (F.first_error (G.run ~mode text)))
        [
          "I64 *G;G++;";
          "U8 G;G++;";
          "F64 G;G++;";
          "I64 G[2];G++;";
          "I64 G=1;G+=1.0;";
          "I64 G=1;G<<=1.0;";
          "I64 F(){static I64 n;n++;return n;}F();";
          "I64 Bad(){F64 n;n++;return 42;}42;";
          "I64 G=1;(G++)++;";
          "I64 G=1;(++G)++;";
          "I64 G=1;(G+=1)++;";
          "I64 G=1;(&G)[0]++;";
          "I64 G=1;lock{G++;}";
        ])
    G.modes

let canonical_opcodes () =
  let expected =
    [
      Ir_opcode.Ic_add_equ;
      Ic_sub_equ;
      Ic_mul_equ;
      Ic_div_equ;
      Ic_mod_equ;
      Ic_and_equ;
      Ic_or_equ;
      Ic_xor_equ;
      Ic_shl_equ;
      Ic_shr_equ;
      Ic_pp_;
      Ic_mm_;
      Ic__pp;
      Ic__mm;
    ]
  in
  List.iter
    (fun mode ->
      let compiled =
        G.compile ~mode
          "I64 \
           G=42;G+=1;G-=1;G*=2;G/=2;G%=100;G&=63;G|=2;G^=1;G<<=1;G>>=1;++G;--G;G++;G--;"
      in
      let updates =
        integer_program_entry compiled
        |> Ir_x87_stack.graph |> Ir_block_graph.blocks
        |> List.concat_map (fun block ->
            Ir_block_graph.instructions block |> Seq.instructions)
        |> List.map Seq.description
        |> List.filter (fun d -> List.mem d.Seq.opcode expected)
      in
      Alcotest.(check (list string))
        "canonical source updates in order"
        (List.map Ir_opcode.to_source_name expected)
        (List.map (fun d -> Ir_opcode.to_source_name d.Seq.opcode) updates);
      Alcotest.(check bool)
        "all updates retain constant barriers" true
        (List.for_all
           (fun d -> (Ir_opcode.info d.Seq.opcode).prevents_constant_folding)
           updates))
    G.modes

let tests =
  List.map
    (fun (name, test) -> Alcotest.test_case name `Quick test)
    [
      ("real compound accumulator and postfix loop", source_gate);
      ("compound read modify write and all operators", compound);
      ("prefix postfix results and source contexts", old_new);
      ("destination signedness and wrapping", signedness);
      ("initializer barriers phase and owner", initializers);
      ( "transitive compound initializer optimizer boundary",
        initializer_optimizer_boundary );
      ("update faults and exact resource limits", faults_and_limits);
      ("canonical preflight and fresh storage", preflight);
      ("unsupported update forms", unsupported);
      ("original update opcodes and constant barriers", canonical_opcodes);
    ]
