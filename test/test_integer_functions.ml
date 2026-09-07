open Holyc_lib
module A = Test_ir_frame_address_lowering
module H = Test_ir_integer_interpreter
module F = Test_ir_integer_frames
module Typed = Semantic_function_call_expression_result
module Expr = Ir_expression_lowering
module Lower = Ir_integer_program_lowering
module Frame = Semantic_function_frame_layout
module VM = Ir_integer_interpreter
module Seq = Ir_instruction_sequence
module Graph = Ir_block_graph

let source = "I64 Add(I64 a,I64 b){I64 c=a+b; return c;}"

let initializer_storage () =
  List.iter
    (fun mode ->
      let frames, typed = A.analyze ~compilation_mode:mode source in
      let function_ = A.function_named typed "Add" in
      let frame = A.frame_for frames function_ in
      let initializers = Typed.function_initializers function_ in
      Alcotest.(check int) "retained initializer" 1 (List.length initializers);
      let initial = List.hd initializers in
      let lowered =
        Expr.lower_initializer ~frame ~instruction_id:(H.instruction_id 0)
          ~value_id:(H.value_id 0) initial
        |> H.require_ok (fun errors ->
            String.concat "; " (List.map H.show_sequence_error errors))
        |> Test_ir_expression_lowering.require_lowered
      in
      let operations =
        Expr.sequence lowered |> Ir_instruction_sequence.instructions
        |> List.map (fun instruction ->
            (Ir_instruction_sequence.description instruction).opcode)
      in
      Alcotest.(check int)
        "reads only the two parameters" 2
        (List.filter (( = ) Ir_opcode.Ic_deref) operations |> List.length);
      Alcotest.(check bool)
        "initializer stores" true
        (List.hd (List.rev operations) = Ir_opcode.Ic_assign);
      let graph =
        Lower.lower ~frame ~span:(H.span 0 42)
          (List.map (fun initial -> Lower.Initialize initial) initializers
          @ List.map
              (fun returned -> Lower.Return returned)
              (Typed.function_returns function_))
        |> H.require_ok (fun errors ->
            String.concat "; "
              (List.map (fun (e : Diagnostic.t) -> e.message) errors))
      in
      let body =
        F.body frame H.public_i64
          (Ir_x87_stack.graph graph |> Ir_block_graph.blocks
          |> List.map (fun block ->
              Ir_block_graph.
                {
                  block_id = block_id block;
                  instructions =
                    instructions block |> Ir_instruction_sequence.instructions
                    |> List.map Ir_instruction_sequence.description;
                }))
      in
      ignore (F.execute frame [ 20L; 22L ] body |> F.expect_word 42L))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let inputs = Test_integer_program.inputs

let checked result =
  result
  |> H.require_ok (fun errors ->
      String.concat "; "
        (List.map
           (fun (error : Diagnostic.t) -> error.code ^ ": " ^ error.message)
           errors))

let run ?(mode = Preprocessor.Jit) ?(max_steps = 10000)
    ?(max_frame_bytes = 1024) ?(max_call_depth = 16) text =
  let session, config, source = inputs ~mode text in
  run_integer_program ~max_frame_bytes ~max_call_depth session ~config ~source
    ~max_steps

let expect ?(type_ = VM.I64) expected result =
  let result = (checked result).value in
  Alcotest.(check bool)
    "stream resumes to its end" true
    (VM.termination result = VM.Stream_end);
  (match VM.final_value result with
  | Some word ->
      Alcotest.(check int64) "actual final word" expected word.bits;
      Alcotest.(check bool) "actual final class" true (word.type_ = type_)
  | None -> Alcotest.fail "program has no final expression value");
  result

let examples () =
  List.iter
    (fun mode ->
      List.iter
        (fun (text, expected) -> ignore (run ~mode text |> expect expected))
        [
          (source ^ "(Add(20,22));", 42L);
          (source ^ "(Add(0,0));", 0L);
          (source ^ "(Add(-7,2));", -5L);
          ("I64 Sub(I64 a,I64 b){I64 c=a-b;return c;}(Sub(20,22));", -2L);
          ("I64 F(){I64 a=20,b=a+2;return a+b;}(F());", 42L);
          ("I64 F(I64 a){I64 c=(a=a+1);return c+a;}(F(20));", 42L);
          ("I64 F(I64 a){I64 c=0;while(a){c=c+1;a=a-1;}return c;}(F(42));", 42L);
          ("I64 F(I64 a){if(a)return a;else return 42;}(F(0));", 42L);
        ])
    [ Preprocessor.Jit; Preprocessor.Aot ]

let returns () =
  List.iter
    (fun mode ->
      ignore
        (run ~mode "U64 F(I64 a){return a;}(F(-1));"
        |> expect ~type_:VM.U64 (-1L));
      ignore
        (run ~mode "I64 F(U64 a){return a;}(F(0xFFFFFFFFFFFFFFFF));"
        |> expect (-1L));
      ignore
        (run ~mode "U64 F(U64 a){return -a;}(F(5));"
        |> expect ~type_:VM.U64 (-5L)))
    [ Preprocessor.Jit; Preprocessor.Aot ]

let continuation () =
  List.iter
    (fun mode ->
      ignore
        (run ~mode
           "I64 Take(I64 a,I64 b){return a;} I64 F(){I64 \
            n=0;(Take(n=1,n=2));return n;}(F());"
        |> expect 1L))
    [ Preprocessor.Jit; Preprocessor.Aot ];
  ignore (run (source ^ "(Add(1,2));(Add(20,22));") |> expect 42L);
  ignore (run (source ^ "(Add(20,22));7;") |> expect 7L);
  ignore
    (run
       "I64 Inner(I64 a){I64 b=a+1;return b;} I64 Outer(I64 \
        a){(Inner(a));return a+2;}(Outer(40));"
    |> expect 42L);
  let result = run "I64 F(){return 42;}" |> checked in
  Alcotest.(check bool)
    "uncalled definition supplies no expression value" true
    (VM.final_value result.value = None)

let first_error result =
  match result with
  | Ok _ -> Alcotest.fail "expected a program diagnostic"
  | Error errors ->
      List.find
        (fun (error : Diagnostic.t) -> error.severity = Diagnostic.Error)
        errors

let limits () =
  let text = source ^ "(Add(20,22));" in
  let result =
    run ~max_steps:29 ~max_frame_bytes:24 ~max_call_depth:1 text |> expect 42L
  in
  Alcotest.(check int)
    "entire call and resumed stream budget" 29 (VM.executed_steps result);
  List.iter
    (fun (result, code) ->
      Alcotest.(check string) "bounded rejection" code (first_error result).code)
    [
      (run ~max_steps:28 text, "HCIRVM0007");
      (run ~max_frame_bytes:23 text, "HCIRVM0011");
      (run ~max_steps:0 "@invalid", "HCIRVM0001");
      (run ~max_frame_bytes:0 "@invalid", "HCIRVM0001");
      (run ~max_call_depth:0 "@invalid", "HCIRVM0001");
    ];
  let error = first_error (run ~max_steps:10 text) in
  Alcotest.(check bool)
    "callee fault retains its function" true
    (List.mem "function=Add" error.notes);
  Alcotest.(check bool)
    "callee fault retains shared steps" true
    (List.mem "executed_steps=10" error.notes)

let recursion () =
  let text = "I64 F(I64 a){if(a>0){F(a-1);}return a;}(F(3));" in
  ignore (run ~max_call_depth:4 ~max_frame_bytes:32 text |> expect 3L);
  Alcotest.(check string)
    "shared depth bound" "HCIRVM0015"
    (first_error (run ~max_call_depth:3 text)).code;
  let error = first_error (run ~max_frame_bytes:24 text) in
  Alcotest.(check string) "simultaneous frame bytes" "HCIRVM0011" error.code;
  Alcotest.(check bool)
    "allocation fails on a reached nested call" true
    (List.mem "stage=execution" error.notes)

let faults () =
  let text =
    "I64 F(I64 initialize){I64 c;if(initialize)c=42;return c;}(F(1));(F(0));"
  in
  let error = first_error (run text) in
  Alcotest.(check string)
    "second invocation starts uninitialized" "HCIRVM0012" error.code;
  Alcotest.(check bool)
    "uninitialized read identifies callee" true
    (List.mem "function=F" error.notes);
  let error = first_error (run "I64 F(I64 a){I64 c=7/a;return c;}(F(0));") in
  Alcotest.(check string)
    "reached initializer arithmetic fault" "HCIRVM0009" error.code;
  Alcotest.(check int)
    "initializer fault source operator" 20 error.primary.start;
  Alcotest.(check string)
    "missing return" "HCIRVM0013"
    (first_error (run "I64 F(){return;}(F());")).code;
  let error =
    first_error (run "I64 Good(){return 42;}I64 Bad(){1.0;return 1;}(Good());")
  in
  Alcotest.(check bool)
    "unused body is preflighted before entry execution" true
    (List.mem "executed_steps=0" error.notes);
  Alcotest.(check bool)
    "unused bad definition retains its name" true
    (List.mem "function=Bad" error.notes)

let unsupported () =
  List.iter
    (fun text -> ignore (first_error (run text)))
    [
      "I64 F(){I32 a;return 1;}(F());";
      "I64 F(){I64 a[1];return 1;}(F());";
      "I64 F(){static I64 a;return 1;}(F());";
      "I64 F(I64 *a){return 1;}(F(0));";
      "I64 F(I64 a=1){return a;}(F());";
      "I64 F(I64 a,...){return a;}(F(1));";
      "extern I64 F();(F());";
    ]

let compile text =
  let session, config, source = inputs text in
  compile_integer_program session ~config ~source |> checked |> fun result ->
  result.value

let rewrite_entry apply graph =
  let blocks = Ir_x87_stack.graph graph |> Graph.blocks in
  Graph.create
    ~entry:(Graph.entry (Ir_x87_stack.graph graph) |> Graph.block_id)
    (List.map
       (fun block ->
         Graph.
           {
             block_id = block_id block;
             instructions =
               instructions block |> Seq.instructions
               |> List.map (fun instruction ->
                   apply (Seq.description instruction));
           })
       blocks)
  |> H.require_ok (fun errors ->
      String.concat "; " (List.map H.show_graph_error errors))
  |> Ir_x87_stack.verify
  |> H.require_ok (fun errors ->
      String.concat "; " (List.map H.show_x87_error errors))

let call_preflight () =
  let compiled = compile (source ^ "(Add(20,22));") in
  let entry = integer_program_entry compiled
  and functions = integer_program_functions compiled in
  let execute ?(functions = functions) entry =
    VM.execute_program ~max_steps:1000 ~max_frame_bytes:1024 ~max_call_depth:8
      ~functions entry
  in
  List.iter
    (fun apply ->
      let graph = rewrite_entry apply entry in
      F.expect_error "HCIRVM0014" (execute graph) |> ignore)
    [
      (fun (d : Seq.description) ->
        if d.opcode = Ir_opcode.Ic_imm_i64 then { d with flags = 0L } else d);
      (fun d ->
        if d.Seq.opcode = Ir_opcode.Ic_add_rsp1 then
          { d with payload = Some (Seq.Integer 24L) }
        else d);
      (fun d ->
        if d.Seq.opcode = Ir_opcode.Ic_add_rsp1 then
          { d with opcode = Ir_opcode.Ic_add_rsp }
        else d);
    ];
  F.expect_error "HCIRVM0014" (execute ~functions:(functions @ functions) entry)
  |> ignore;
  let foreign =
    compile (source ^ "(Add(1,2));") |> integer_program_functions |> List.hd
  in
  let definition = List.hd functions in
  F.expect_error "HCIRVM0011"
    (execute ~functions:[ { definition with frame = foreign.frame } ] entry)
  |> ignore

let tests =
  List.map
    (fun (name, test) -> Alcotest.test_case name `Quick test)
    [
      ("typed initializer reaches checked storage", initializer_storage);
      ("source parameters, initializer order and mutable bodies", examples);
      ("declared return bits and signedness", returns);
      ("caller continuation and independent invocations", continuation);
      ("whole-program resource limits", limits);
      ("explicit recursive call stack", recursion);
      ("reached faults and complete definition preflight", faults);
      ("unsupported source boundaries", unsupported);
      ("canonical call protocol and ownership", call_preflight);
    ]
