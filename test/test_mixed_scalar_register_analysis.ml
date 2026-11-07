open Holyc_lib
module Analysis = Holyc_lib__Ir.Register_variable_analysis
module Frame = Semantic_function_frame_layout
module Body = Ir_function_body
module Graph = Ir_block_graph
module Seq = Ir_instruction_sequence
module O = Ir_opcode
module Symbol = Semantic_symbol
module Typed = Holyc_lib__Sema.Function_call_expression_result
module Source = Holyc_lib__Sema.Function_call_resolution

let lowered mode contents =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"mixed-register-proof.hc" ~contents
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
  in
  compile_integer_program session ~config ~source

let compile_with_runtime mode contents =
  match lowered mode contents with
  | Ok program ->
      let fn =
        integer_program_functions program.value
        |> List.find (fun (fn : Ir_integer_interpreter.function_definition) ->
            Symbol.name (Body.symbol fn.body) = "F")
      in
      (fn, integer_program_runtime_calls program.value)
  | Error errors ->
      Alcotest.fail
        (contents ^ ": "
        ^ String.concat "; "
            (List.map
               (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
               errors))

let compile mode contents = fst (compile_with_runtime mode contents)

let location fn name =
  Frame.function_locations fn.Ir_integer_interpreter.frame
  |> List.find (fun member -> Symbol.name (Frame.location_symbol member) = name)

let proof fn =
  match
    Analysis.analyze ~frame:fn.Ir_integer_interpreter.frame ~body:fn.body
  with
  | Some proof -> proof
  | None -> Alcotest.fail "missing mixed scalar allocation proof"

let check fn proof name selection register =
  let symbol = Frame.location_symbol (location fn name) in
  Alcotest.(check bool)
    (name ^ " source selection")
    true
    (Analysis.selection proof ~frame:fn.frame ~body:fn.body ~symbol
    = Some selection);
  Alcotest.(check (option int))
    (name ^ " exact physical register")
    register
    (Analysis.physical_register proof ~frame:fn.frame ~body:fn.body ~symbol)

let modes test = List.iter test [ Preprocessor.Jit; Aot ]

let placement () =
  modes (fun mode ->
      List.iter
        (fun (source, expected) ->
          let fn = compile mode source in
          let evidence = proof fn in
          List.iter
            (fun (name, selection, register) ->
              check fn evidence name selection register)
            expected)
        [
          ( "F64 F(reg R10 F64 x){return x;}42;",
            [ ("x", Analysis.Register_candidate, Some 10) ] );
          ( "F64 F(reg R10 F64 x){F64 y;return x;}42;",
            [
              ("x", Analysis.Register_candidate, Some 10);
              ("y", Analysis.Stack, None);
            ] );
          ( "F64 F(reg R10 F64 x,I64 y){return x+y;}42;",
            [ ("x", Analysis.Register_candidate, Some 10) ] );
          ( "I64 F(reg R10 F64 x){return x;}42;",
            [ ("x", Analysis.Register_candidate, Some 10) ] );
          ( "F64 F(F64 reg R12 x){return x;}42;",
            [ ("x", Analysis.Register_candidate, Some 12) ] );
          ("F64 F(F64 x){return x;}42;", [ ("x", Analysis.Stack, None) ]);
          ("F64 F(noreg F64 x){return x;}42;", [ ("x", Analysis.Stack, None) ]);
          ( "F64 F(reg F64 x){return x;}42;",
            [ ("x", Analysis.Register_candidate, Some 6) ] );
          ( "I64 F(I8 x,I64 step,F64 ignored){x+=step;return x;}42;",
            [
              ("x", Analysis.Register_candidate, Some 6);
              ("ignored", Analysis.Stack, None);
            ] );
          ( "I64 F(noreg I8 x,I64 step,F64 ignored){x+=step;return x;}42;",
            [
              ("x", Analysis.Stack, None);
              ("step", Analysis.Register_candidate, Some 6);
            ] );
          ( "F64 F(reg F64 x,F64 a,F64 b,F64 c,F64 d,F64 e,F64 f,F64 g){return \
             x;}42;",
            [
              ("x", Analysis.Register_candidate, Some 6);
              ("g", Analysis.Stack, None);
            ] );
          ( "F64 F(reg F64 x,reg RSI F64 unused){return x;}42;",
            [
              ("x", Analysis.Register_candidate, Some 7);
              ("unused", Analysis.Register_candidate, Some 6);
            ] );
          ( "I64 F(I64 x,reg F64 unused){return x;}42;",
            [
              ("x", Analysis.Register_candidate, Some 7);
              ("unused", Analysis.Register_candidate, Some 6);
            ] );
          ( "I64 F(I64 x,reg RSI F64 unused){return x;}42;",
            [
              ("x", Analysis.Register_candidate, Some 7);
              ("unused", Analysis.Register_candidate, Some 6);
            ] );
          ( "F64 F(F64 x){F64 reg R11 y=x+1.5;y=y*2.5;return y;}42;",
            [
              ("x", Analysis.Stack, None);
              ("y", Analysis.Register_candidate, Some 11);
            ] );
          ( "F64 F(reg R10 F64 x){return (x+1.5)*(x-2.5);}42;",
            [ ("x", Analysis.Register_candidate, Some 10) ] );
        ])

let capacity () =
  modes (fun mode ->
      List.iter
        (fun count ->
          let names = List.init count (fun i -> "f" ^ string_of_int i) in
          let source =
            "F64 F("
            ^ String.concat "," (List.map (fun name -> "reg F64 " ^ name) names)
            ^ "){return " ^ String.concat "+" names ^ ";}42;"
          in
          let fn = compile mode source in
          (* Literal source deductions from pinned QSort2b, not allocator
             output: all forced members have seed+2 and original source order. *)
          let expected =
            match count with
            | 6 -> [ Some 11; Some 14; Some 15; Some 6; Some 7; Some 10 ]
            | 7 ->
                [ Some 10; Some 11; Some 14; Some 7; Some 12; Some 13; Some 6 ]
            | 8 ->
                [
                  Some 11;
                  Some 10;
                  Some 15;
                  Some 14;
                  Some 13;
                  Some 12;
                  Some 7;
                  Some 6;
                ]
            | 9 ->
                [
                  Some 14;
                  Some 11;
                  None;
                  Some 15;
                  Some 10;
                  Some 13;
                  Some 12;
                  Some 7;
                  Some 6;
                ]
            | _ -> assert false
          in
          let evidence = proof fn in
          List.iter2
            (fun name register ->
              check fn evidence name
                (if Option.is_some register then Analysis.Register_candidate
                 else Analysis.Stack)
                register)
            names expected)
        [ 6; 7; 8; 9 ];
      let blocked =
        compile mode
          "F64 F(reg F64 x,reg RSI F64 a,reg RDI F64 b,reg R10 F64 c,reg R11 \
           F64 d,reg R14 F64 e,reg R15 F64 f){return x;}42;"
      in
      let evidence = proof blocked in
      check blocked evidence "x" Analysis.Stack None;
      List.iter
        (fun (name, register) ->
          check blocked evidence name Analysis.Register_candidate
            (Some register))
        [ ("a", 6); ("b", 7); ("c", 10); ("d", 11); ("e", 14); ("f", 15) ];
      let ordinary =
        compile mode
          "I64 F(I8 a,I8 b,I8 c,I8 d,I8 e,I8 f,F64 ignored){return \
           a+b+c+d+e+f;}42;"
      in
      let evidence = proof ordinary in
      check ordinary evidence "ignored" Analysis.Stack None;
      List.iter
        (fun (name, register) ->
          check ordinary evidence name Analysis.Register_candidate
            (Some register))
        [ ("a", 11); ("b", 14); ("c", 15); ("d", 6); ("e", 7); ("f", 10) ])

let exclusions () =
  modes (fun mode ->
      List.iter
        (fun source ->
          match lowered mode source with
          | Error _ -> ()
          | Ok program ->
              let fn =
                List.find
                  (fun (fn : Ir_integer_interpreter.function_definition) ->
                    Symbol.name (Body.symbol fn.body) = "F")
                  (integer_program_functions program.value)
              in
              Alcotest.(check bool)
                ("outside mixed capacity proof: " ^ source)
                true
                (Option.is_none
                   (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          "F64 F(reg RSI F64 x,reg RSI F64 y){return x+y;}42;";
          "F64 F(I64 reg R10 n,F64 reg R10 x){return x;}42;";
          "F64 F(reg RAX F64 x){return x;}42;";
          "F64 F(reg R10 F64 x){x+=1.5;return x;}42;";
          "F64 F(reg R10 F64 x){x++;return x;}42;";
          "F64 F(reg R10 F64 x,F64){return x;}42;";
        ])

let descriptions fn =
  Body.body fn.Ir_integer_interpreter.body
  |> Graph.blocks
  |> List.concat_map (fun block ->
      Graph.instructions block |> Seq.instructions |> List.map Seq.description)

let calls_and_control_flow () =
  modes (fun mode ->
      List.iter
        (fun (contents, name, register) ->
          let fn, runtime = compile_with_runtime mode contents in
          let evidence =
            match
              Analysis.analyze_with_runtime ~runtime ~frame:fn.frame
                ~body:fn.body
            with
            | Some evidence -> evidence
            | None ->
                Alcotest.fail
                  ("missing canonical call/control proof: " ^ contents)
          in
          check fn evidence name Analysis.Register_candidate register)
        [
          ( "F64 G(F64 x){return x+1.5;}F64 F(reg R10 F64 x){G(x);return x;}42;",
            "x",
            Some 10 );
          ( "F64 G(F64 x){return x+1.5;}F64 F(reg R11 F64 x){return G(x)+x;}42;",
            "x",
            Some 11 );
          ( "F64 G(F64 x){return x+1.5;}F64 F(reg R10 F64 x){F64 reg R11 \
             y=G(x);return y+x;}42;",
            "y",
            Some 11 );
          ( "F64 G(F64 x){return x+1.5;}F64 F(reg R10 F64 x){x=G(x);return \
             x;}42;",
            "x",
            Some 10 );
          ( "F64 F(I64 n,reg R10 F64 x){if(n)return F(n-1,x+1.5)+x;return x;}42;",
            "x",
            Some 10 );
          ( "F64 F(I64 n,reg R11 F64 x){while(n){x=x+1.5;n--;}return x;}42;",
            "x",
            Some 11 );
          ( "F64 F(I64 n,reg R10 F64 x){do{x=x+1.5;n--;}while(n);return x;}42;",
            "x",
            Some 10 );
          ( "F64 F(I64 n,reg R10 F64 x){for(n=2;n;n--){x=x+1.5;}return x;}42;",
            "x",
            Some 10 );
          ( "F64 F(I64 n,reg R10 F64 x){if(n){x=x+1.5;}else{x=x-2.5;}return \
             x;}42;",
            "x",
            Some 10 );
        ];
      let contents =
        "F64 G(F64 x){return x;}F64 F(reg R10 F64 x){return G(x);}42;"
      in
      let fn, runtime = compile_with_runtime mode contents in
      Alcotest.(check bool)
        "mixed calls require original target context" true
        (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body));
      let foreign, foreign_runtime = compile_with_runtime mode contents in
      Alcotest.(check bool)
        "foreign runtime cannot classify mixed calls" true
        (Option.is_none
           (Analysis.analyze_with_runtime ~runtime:foreign_runtime
              ~frame:fn.frame ~body:fn.body));
      Alcotest.(check bool)
        "foreign body cannot reuse mixed call context" true
        (Option.is_none
           (Analysis.analyze_with_runtime ~runtime ~frame:foreign.frame
              ~body:foreign.body));
      let evidence =
        Option.get
          (Analysis.analyze_with_runtime ~runtime ~frame:fn.frame ~body:fn.body)
      in
      let call =
        descriptions fn
        |> List.find (fun item ->
            item.Seq.opcode = O.Ic_call || item.opcode = O.Ic_call_indirect2)
      in
      Obj.set_field (Obj.repr call) 6 (Obj.repr 0x2000L);
      let symbol = Frame.location_symbol (location fn "x") in
      Alcotest.(check bool)
        "mutated call primitive revokes saved proof" true
        (Option.is_none
           (Analysis.selection evidence ~frame:fn.frame ~body:fn.body ~symbol));
      Alcotest.(check bool)
        "runtime graph identities cannot launder changed call flags" true
        (Option.is_none
           (Analysis.analyze_with_runtime ~runtime ~frame:fn.frame ~body:fn.body)))

let canonical_mutations () =
  let contents =
    "F64 G(F64 a,F64 b){return a-b;}F64 F(I64 n,reg R10 F64 x,F64 \
     y){if(n)return G(x,y);return x-y;}42;"
  in
  let floating item =
    Option.fold ~none:false
      ~some:(fun type_ ->
        match Semantic_type.base type_ with
        | Semantic_type.Primitive (_, Primitive_type.F64) -> true
        | _ -> false)
      item.Seq.target_type
  in
  let edits =
    [
      ( "ordered F64 subtraction operands",
        fun fn ->
          let item =
            descriptions fn
            |> List.find (fun d -> d.Seq.opcode = O.Ic_sub && floating d)
          in
          Obj.set_field (Obj.repr item) 2 (Obj.repr (List.rev item.operands)) );
      ( "F64 subtraction opcode",
        fun fn ->
          let item =
            descriptions fn
            |> List.find (fun d -> d.Seq.opcode = O.Ic_sub && floating d)
          in
          Obj.set_field (Obj.repr item) 1 (Obj.repr O.Ic_add) );
      ( "call argument frame loads",
        fun fn ->
          let loads =
            descriptions fn
            |> List.filter (fun d -> d.Seq.opcode = O.Ic_deref && floating d)
          in
          let first = List.nth loads 0 and second = List.nth loads 1 in
          let operands = first.operands in
          Obj.set_field (Obj.repr first) 2 (Obj.repr second.operands);
          Obj.set_field (Obj.repr second) 2 (Obj.repr operands) );
      ( "named call target",
        fun fn ->
          let item =
            descriptions fn
            |> List.find (fun d ->
                d.Seq.opcode = O.Ic_call || d.opcode = O.Ic_call_indirect2)
          in
          Obj.set_field (Obj.repr item) 5
            (Obj.repr (Some (Seq.Symbol (Body.callable_symbol fn.body)))) );
      ( "branch target",
        fun fn ->
          let item =
            descriptions fn |> List.find (fun d -> d.Seq.opcode = O.Ic_br_zero)
          in
          let original =
            match item.payload with
            | Some (Seq.Block b) -> b
            | _ -> assert false
          in
          let other =
            Body.body fn.body |> Graph.blocks
            |> List.find (fun b ->
                not (Seq.Block_id.equal (Graph.block_id b) original))
          in
          Obj.set_field (Obj.repr item) 5
            (Obj.repr (Some (Seq.Block (Graph.block_id other)))) );
      ( "branch successor order",
        fun fn ->
          let block =
            Body.body fn.Ir_integer_interpreter.body
            |> Graph.blocks
            |> List.find (fun b -> List.length (Graph.successors b) = 2)
          in
          Obj.set_field (Obj.repr block) 2
            (Obj.repr (List.rev (Graph.successors block))) );
      ( "completed compiler options",
        fun fn ->
          Obj.set_field
            (Obj.repr fn.Ir_integer_interpreter.body)
            8
            (Obj.repr
               (Int64.logxor
                  (Body.compiler_options fn.body)
                  (Compiler_option.mask Compiler_option.No_reg_var))) );
    ]
  in
  modes (fun mode ->
      List.iter
        (fun (label, edit) ->
          let fn, runtime = compile_with_runtime mode contents in
          let evidence =
            Option.get
              (Analysis.analyze_with_runtime ~runtime ~frame:fn.frame
                 ~body:fn.body)
          in
          let symbol = Frame.location_symbol (location fn "x") in
          edit fn;
          Alcotest.(check bool)
            (label ^ " revokes saved proof")
            true
            (Option.is_none
               (Analysis.selection evidence ~frame:fn.frame ~body:fn.body
                  ~symbol));
          Alcotest.(check bool)
            (label ^ " rejects fresh proof")
            true
            (Option.is_none
               (Analysis.analyze_with_runtime ~runtime ~frame:fn.frame
                  ~body:fn.body)))
        edits;
      let fn, runtime =
        compile_with_runtime mode
          "F64 G(F64 x){return x;}F64 F(reg R10 F64 x){F64 y=G(y);return x;}42;"
      in
      Alcotest.(check bool)
        "call initializer cannot read its own uninitialized local" true
        (Option.is_none
           (Analysis.analyze_with_runtime ~runtime ~frame:fn.frame ~body:fn.body)))

let authority () =
  modes (fun mode ->
      let source = "F64 F(reg R10 F64 x){return x+1.5;}42;" in
      let original = compile mode source in
      let evidence = proof original in
      let symbol = Frame.location_symbol (location original "x") in
      let foreign = compile mode source in
      Alcotest.(check bool)
        "foreign body revokes mixed evidence" true
        (Option.is_none
           (Analysis.selection evidence ~frame:original.frame ~body:foreign.body
              ~symbol));
      Alcotest.(check bool)
        "foreign frame revokes mixed evidence" true
        (Option.is_none
           (Analysis.selection evidence ~frame:foreign.frame ~body:original.body
              ~symbol));
      List.iter
        (fun bits ->
          let fn = compile mode source in
          let evidence = proof fn in
          let symbol = Frame.location_symbol (location fn "x") in
          let literal =
            descriptions fn |> List.find (fun d -> d.Seq.opcode = O.Ic_imm_f64)
          in
          Obj.set_field (Obj.repr literal) 5
            (Obj.repr (Some (Seq.Float_bits bits)));
          Alcotest.(check bool)
            "changed floating payload revokes saved witness" true
            (Option.is_none
               (Analysis.selection evidence ~frame:fn.frame ~body:fn.body
                  ~symbol));
          Alcotest.(check bool)
            "changed floating payload rejects fresh proof" true
            (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body)))
        [
          0L;
          Int64.min_int;
          0x7ff0000000000000L;
          0x7ff0000000000001L;
          0x7ff8000000000042L;
        ];
      let fn = compile mode "F64 F(reg R10 F64 x,F64 y){return x-y;}42;" in
      let evidence = proof fn in
      let symbol = Frame.location_symbol (location fn "x") in
      let typed = Option.get (Body.definition_source fn.body) in
      let returned = List.hd (Typed.function_returns typed) in
      let value = Option.get (Typed.return_value returned) in
      let left, right = Option.get (Typed.result_binary_operands value) in
      let binary =
        match Source.argument_expression_kind (Typed.result_source value) with
        | Source.Binary_expression binary -> binary
        | _ -> assert false
      in
      Obj.set_field (Obj.repr value) 4 (Obj.repr (Some (right, left)));
      Obj.set_field (Obj.repr binary) 2 (Obj.repr (Typed.result_source right));
      Obj.set_field (Obj.repr binary) 3 (Obj.repr (Typed.result_source left));
      let entry = Body.body fn.body |> Graph.entry in
      let code =
        Graph.instructions entry |> Seq.instructions |> List.map Seq.description
      in
      let first = List.hd code in
      let leave =
        match (List.hd (List.rev code)).payload with
        | Some (Seq.Block leave) -> leave
        | _ -> assert false
      in
      let module Return = Holyc_lib__Ir.Return_lowering in
      let lowered =
        Return.lower_function_return ~frame:fn.frame ~optimize_shifts:true
          ~optimize_division:true ~instruction_id:first.instruction_id
          ~value_id:(Option.get first.result).value_id ~leave returned
      in
      let sequence =
        match lowered with
        | Ok (Return.Lowered result) -> Return.sequence result
        | _ -> Alcotest.fail "forged typed operands still lower"
      in
      Obj.set_field (Obj.repr entry) 1 (Obj.repr sequence);
      Alcotest.(check bool)
        "source operand reversal rejects saved proof" true
        (Option.is_none
           (Analysis.selection evidence ~frame:fn.frame ~body:fn.body ~symbol));
      Alcotest.(check bool)
        "canonical relowering cannot launder reversed original operands" true
        (Option.is_none (Analysis.analyze ~frame:fn.frame ~body:fn.body)))

let () =
  Alcotest.run "mixed scalar register authority"
    [
      ( "proof",
        [
          Alcotest.test_case "source scalar selection and exact homes" `Quick
            placement;
          Alcotest.test_case "scalar-aware bounded capacity" `Quick capacity;
          Alcotest.test_case "unsupported sources retain rejection" `Quick
            exclusions;
          Alcotest.test_case "source and raw-bit ownership" `Quick authority;
          Alcotest.test_case "canonical calls and control flow" `Quick
            calls_and_control_flow;
          Alcotest.test_case "canonical graph mutation refusal" `Quick
            canonical_mutations;
        ] );
    ]
