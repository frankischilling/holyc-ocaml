open Holyc_lib
module F = Test_integer_functions
module H = Test_ir_integer_interpreter
module VM = Ir_integer_interpreter
module Globals = Ir_integer_globals
module Seq = Ir_instruction_sequence

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let run ?(mode = Preprocessor.Jit) ?(max_global_bytes = 1024)
    ?(max_frame_bytes = 1024) ?(max_call_depth = 16) ?(max_steps = 10000) text =
  let session, config, source = F.inputs ~mode text in
  run_integer_program ~max_global_bytes ~max_frame_bytes ~max_call_depth
    ~max_steps session ~config ~source

let compile ?(mode = Preprocessor.Jit) text =
  let session, config, source = F.inputs ~mode text in
  (F.checked (compile_integer_program session ~config ~source)).value

let source =
  "I64 Total;I64 AddTo(I64 n){Total=Total+n;return \
   Total;}Total=0;AddTo(20);AddTo(22);Total;"

let source_gate () =
  List.iter
    (fun mode ->
      ignore (run ~mode source |> F.expect 42L);
      let result = F.checked (run ~mode "I64 G;") in
      Alcotest.(check bool)
        "declaration alone supplies no expression value" true
        (VM.final_value result.value = None))
    modes

let source_contexts () =
  List.iter
    (fun mode ->
      List.iter
        (fun (text, expected) -> ignore (run ~mode text |> F.expect expected))
        [
          ("I64 G;G=42;G;", 42L);
          ("I64 G;I64 F(){I64 G=7;return G;}G=35;(G+F());", 42L);
          ("I64 G;I64 F(I64 G){return G+1;}G=41;(F(G));", 42L);
          ("I64 G,H;I64 F(){I64 n=G;H=n+2;return H;}G=40;F();H;", 42L);
          ("I64 G;I64 Id(I64 n){return n;}G=0;(Id((G)=42));", 42L);
          ( "I64 G;I64 Sub(I64 a,I64 b){return a-b;}G=0;(Sub(G=1,G=2)*10+G);",
            -9L );
          ( "I64 \
             G;G=0;while(G<3){G=G+1;}for(;G<7;G=G+1){}do{G=G-1;}while(G>5);G;",
            5L );
          ("I64 G;G=0;if(0&&(G=99)){}if(1||(G=88)){}G;", 0L);
          ("I64 G;G=0;(0&&(G=1));G;", 1L);
          ("I64 G;G=0;(0<(G=G+1)<3);G;", 1L);
          ( "I64 G;I64 F(I64 n){if(n){G=G+1;return F(n-1);}return G;}G=0;(F(5));",
            5L );
          ("I64 G;I64 Set(){G=20;return 2;}G=40;(G+Set());", 42L);
          ( "I64 G;I64 Inner(){G=G+1;return G;}I64 Outer(){I64 \
             n=Inner();Inner();return n+G;}G=19;(Outer());",
            41L );
          ("I64 G;I64 F(){G=G+1;return G;G=99;}G=40;F();F();G;", 42L);
        ])
    modes;
  let redefined = "I64 G;I64 First(){return G;}G=20;I64 G;G=22;(First()+G);" in
  ignore (run redefined |> F.expect 42L);
  Alcotest.(check string)
    "AOT redefinition retains its alias boundary" "HCRUN0001"
    (F.first_error (run ~mode:Preprocessor.Aot redefined)).code

let signedness () =
  List.iter
    (fun mode ->
      ignore (run ~mode "U64 G;G=-1;G;" |> F.expect ~type_:VM.U64 (-1L));
      ignore (run ~mode "I64 G;G=0xFFFFFFFFFFFFFFFF;G;" |> F.expect (-1L));
      ignore (run ~mode "U64 G;G=7;(-G);" |> F.expect (-7L));
      ignore (run ~mode "U64 G;I64 F(){return G;}G=-1;(F());" |> F.expect (-1L));
      ignore
        (run ~mode "I64 G;U64 F(){return (G=0xFFFFFFFFFFFFFFFF);} (F());"
        |> F.expect ~type_:VM.U64 (-1L));
      ignore
        (run ~mode "I64 S;U64 U;U=-1;S=U;U=S;U;" |> F.expect ~type_:VM.U64 (-1L)))
    modes

let initialization_and_faults () =
  ignore (run ~mode:Preprocessor.Aot "I64 G;G;" |> F.expect 0L);
  ignore (run ~mode:Preprocessor.Aot "U64 G;G;" |> F.expect ~type_:VM.U64 0L);
  let error = F.first_error (run "I64 G;I64 Read(){return G;}(Read());") in
  Alcotest.(check string)
    "unknown JIT global diagnostic" "HCIRVM0012" error.code;
  Alcotest.(check bool)
    "hosted label" true
    (String.starts_with ~prefix:"hosted execution" error.message);
  Alcotest.(check bool)
    "callee provenance" true
    (List.mem "function=Read" error.notes);
  Alcotest.(check bool)
    "execution phase" true
    (List.mem "stage=execution" error.notes);
  List.iter
    (fun mode ->
      ignore (run ~mode "I64 G;if(0)G;42;" |> F.expect 42L);
      let text = "I64 G;I64 Fail(){return 1/G;}G=0;(Fail());" in
      let error = F.first_error (run ~mode text) in
      Alcotest.(check string) "global operand fault" "HCIRVM0009" error.code;
      Alcotest.(check int)
        "exact operator" (String.index text '/') error.primary.start;
      Alcotest.(check bool)
        "fault owner" true
        (List.mem "function=Fail" error.notes);
      let error =
        F.first_error (run ~mode "I64 G;I64 Bad(){1.0;return G;}G=42;G;")
      in
      Alcotest.(check bool)
        "unused definition preflight" true
        (List.mem "executed_steps=0" error.notes))
    modes

let limits () =
  List.iter
    (fun mode ->
      let result =
        run ~mode ~max_global_bytes:8 ~max_frame_bytes:8 ~max_call_depth:1
          source
        |> F.expect 42L
      in
      let steps = VM.executed_steps result in
      ignore (run ~mode ~max_steps:steps source |> F.expect 42L);
      List.iter
        (fun (result, code) ->
          Alcotest.(check string)
            "resource diagnostic" code (F.first_error result).code)
        [
          (run ~mode ~max_global_bytes:7 source, "HCIRVM0016");
          (run ~mode ~max_global_bytes:8 "I64 A,B;42;", "HCIRVM0016");
          (run ~mode ~max_frame_bytes:7 source, "HCIRVM0011");
          (run ~mode ~max_steps:(steps - 1) source, "HCIRVM0007");
          (run ~mode ~max_global_bytes:0 "@invalid", "HCIRVM0001");
          (run ~mode ~max_global_bytes:(-1) "@invalid", "HCIRVM0001");
        ];
      ignore
        (run ~mode ~max_global_bytes:16 "I64 A,B;A=20;B=22;(A+B);"
        |> F.expect 42L))
    modes

let unsupported () =
  List.iter
    (fun mode ->
      List.iter
        (fun text -> ignore (F.first_error (run ~mode text)))
        [
          "I64 G={0};42;";
          "I64 G=Missing();42;";
          "I64 G=1/0;42;";
          "I0 G;42;";
          "F64 G;42;";
          "I64 *G;42;";
          "extern I64 G;42;";
          "import I64 G;42;";
          "extern I64 G;I64 G;42;";
          "I64 G;I64 F(){static I0 n;return 1;}42;";
          "I64 F(){return G;}I64 G;G=42;(F());";
          "G=42;I64 G;G;";
        ];
      List.iter
        (fun text ->
          Alcotest.(check string)
            "explicit binding boundary" "HCRUN0001"
            (F.first_error (run ~mode text)).code)
        [ "_intern 42 I64 G;42;"; "_extern REMOTE I64 G;42;" ];
      let module D = Test_globals_on_data_heap in
      let prepared = D.prepare ~mode ~path:"global-data-heap.hc" "I64 G;" in
      let records =
        D.resolved ~compiler_option_mask:D.data_heap_mask prepared mode
      in
      let classified = D.classification prepared records in
      let error =
        F.first_error (Globals.create ~span:prepared.ast.span classified)
      in
      Alcotest.(check string)
        "checked data-heap context is unsupported" "HCRUN0001" error.code)
    modes

let context_and_preflight () =
  List.iter
    (fun mode ->
      let compiled = compile ~mode "I64 G;G=42;G;" in
      let globals = integer_program_globals compiled in
      let entry = integer_program_entry compiled in
      let execute ?(globals = globals) entry =
        VM.execute_program ~globals ~max_global_bytes:8 ~max_steps:1000
          ~max_frame_bytes:1 ~max_call_depth:1 ~functions:[] entry
      in
      let require_preflight = function
        | Ok _ -> Alcotest.fail "malformed global IR executed"
        | Error errors ->
            List.iter
              (fun (e : VM.error) ->
                Alcotest.(check bool)
                  "preflight stage" true (e.stage = VM.Preflight);
                Alcotest.(check int) "no effects" 0 e.executed_steps)
              errors
      in
      let foreign = compile ~mode "I64 G;G=42;G;" |> integer_program_globals in
      require_preflight (execute ~globals:foreign entry);
      let hidden_source = "I64 G;I64 Hidden(){return G;}42;" in
      let hidden = compile ~mode hidden_source in
      let foreign = compile ~mode hidden_source |> integer_program_globals in
      require_preflight
        (VM.execute_program ~globals:foreign ~max_global_bytes:8 ~max_steps:1000
           ~max_frame_bytes:1 ~max_call_depth:1
           ~functions:(integer_program_functions hidden)
           (integer_program_entry hidden));
      let slot = List.hd (Globals.slots globals) in
      Alcotest.(check int) "exact global bytes" 8 (Globals.byte_size globals);
      Alcotest.(check bool)
        "exact own symbol" true
        (Option.is_some (Globals.find globals (Globals.slot_symbol slot)));
      Alcotest.(check bool)
        "foreign table-local ID" true
        (Globals.find globals
           (Globals.slot_symbol (List.hd (Globals.slots foreign)))
        = None);
      let address_opcode =
        if mode = Preprocessor.Jit then Ir_opcode.Ic_imm_i64
        else Ir_opcode.Ic_abs_addr
      in
      Alcotest.(check bool)
        "mode address intent" true
        (Globals.slot_opcode slot = address_opcode);
      let address (d : Seq.description) =
        match d.payload with
        | Some (Seq.Symbol _) -> true
        | _ -> false
      in
      List.iter
        (fun rewrite ->
          require_preflight (execute (F.rewrite_entry rewrite entry)))
        [
          (fun d -> if address d then { d with Seq.flags = 1L } else d);
          (fun d ->
            if address d then { d with Seq.target_type = Some H.i64 } else d);
          (fun d ->
            if address d then { d with Seq.payload = Some (Seq.Integer 0L) }
            else d);
          (fun d ->
            if address d then
              {
                d with
                Seq.opcode =
                  (if mode = Preprocessor.Jit then Ir_opcode.Ic_abs_addr
                   else Ir_opcode.Ic_imm_i64);
              }
            else d);
          (fun d ->
            if d.Seq.opcode = Ir_opcode.Ic_deref then
              { d with Seq.target_type = Some H.public_u64 }
            else d);
          (fun d ->
            if d.Seq.opcode = Ir_opcode.Ic_assign then
              { d with Seq.target_type = Some H.public_u64 }
            else d);
          (fun d ->
            if d.Seq.opcode = Ir_opcode.Ic_assign then { d with Seq.flags = 1L }
            else d);
          (fun d ->
            if d.Seq.opcode = Ir_opcode.Ic_assign then
              { d with Seq.payload = Some (Seq.Integer 0L) }
            else d);
        ];
      Alcotest.(check string)
        "deterministic storage and graph"
        (integer_program_human compiled)
        (integer_program_human (compile ~mode "I64 G;G=42;G;"));
      let uninitialized = compile ~mode "I64 G;G=G+1;G;" in
      let entry = integer_program_entry uninitialized
      and globals = integer_program_globals uninitialized in
      List.iter
        (fun _ ->
          match (execute ~globals entry, mode) with
          | Ok result, Preprocessor.Aot ->
              Alcotest.(check int64)
                "fresh AOT storage" 1L (Option.get (VM.final_value result)).bits
          | Error (e :: _), Preprocessor.Jit ->
              Alcotest.(check string) "fresh JIT storage" "HCIRVM0012" e.code
          | _ -> Alcotest.fail "global initial state leaked between runs")
        [ (); () ])
    modes

let legacy_graph_boundary () =
  let session, config, source = F.inputs "I64 G;G=42;G;" in
  let error = F.first_error (lower_integer_program session ~config ~source) in
  Alcotest.(check string)
    "graph-only API must retain the storage boundary" "HCRUN0001" error.code

let tests =
  List.map
    (fun (name, test) -> Alcotest.test_case name `Quick test)
    [
      ("shared source accumulator", source_gate);
      ("global expression and caller contexts", source_contexts);
      ("public signedness and assignment results", signedness);
      ("initial state and fault provenance", initialization_and_faults);
      ("independent persistent allocation bounds", limits);
      ("unsupported declaration boundaries", unsupported);
      ("exact storage identity and canonical preflight", context_and_preflight);
      ("graph-only API cannot discard global storage", legacy_graph_boundary);
    ]
