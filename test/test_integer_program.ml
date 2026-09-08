open Holyc_lib
module VM = Ir_integer_interpreter

let inputs ?(mode = Preprocessor.Jit) text =
  let session = Session.create () in
  let source = Session.add_source session ~path:"program.hc" ~contents:text in
  let config =
    match Preprocessor.Config.create ~compilation_mode:mode () with
    | Ok config -> config
    | Error message -> Alcotest.fail message
  in
  (session, config, source)

let run ?mode ?(max_steps = 200) text =
  let session, config, source = inputs ?mode text in
  run_integer_program session ~config ~source ~max_steps
  |> Result.map (fun result -> result.value)

let checked = function
  | Ok value -> value
  | Error diagnostics ->
      diagnostics
      |> List.map (fun (d : Diagnostic.t) -> d.code ^ ": " ^ d.message)
      |> String.concat "; " |> Alcotest.fail

let succeeds text () =
  let result = run text |> checked in
  Alcotest.(check bool)
    "reaches stream end" true
    (VM.termination result = VM.Stream_end)

let diagnostic ?max_steps text =
  match run ?max_steps text with
  | Ok _ -> Alcotest.fail "expected failure without an execution result"
  | Error [] -> Alcotest.fail "missing diagnostic"
  | Error (first :: _) -> first

let faults () =
  List.iter
    (fun text ->
      Alcotest.(check string) text "HCIRVM0009" (diagnostic text).code)
    [
      "if(1) 1/0;";
      "if(0) 2; else 1/0;";
      "if(1 && (1/0)) ;";
      "if(0 || (1/0)) ;";
      "if(0 ^^ (1/0)) ;";
      "0 && (1/0);";
      "1 || (1/0);";
      "do {1/0;} while(0);";
      "for(1/0;0;0);";
      "for(0;1;1/0) 42;";
    ];
  let fault = diagnostic "if(1) {\n  7/0;\n}" in
  Alcotest.(check int) "fault operator position" 11 fault.primary.start

let budget () =
  let result = run ~max_steps:5 "6*7;" |> checked in
  Alcotest.(check int)
    "arithmetic, discard and stream end" 5 (VM.executed_steps result);
  Alcotest.(check string)
    "exact exhaustion" "HCIRVM0007" (diagnostic ~max_steps:4 "6*7;").code;
  List.iter
    (fun text ->
      Alcotest.(check string)
        text "HCIRVM0007" (diagnostic ~max_steps:17 text).code)
    [ "while(1);"; "do ; while(1);"; "for(0;1;0);" ];
  Alcotest.(check string)
    "invalid budget before parse" "HCIRVM0001"
    (diagnostic ~max_steps:0 "@ invalid").code

let unsupported () =
  List.iter
    (fun text ->
      Alcotest.(check string) text "HCRUN0001" (diagnostic text).code)
    [ "U16 x=0;"; "return;"; "lock {1;}"; "switch(1){case 1:break;}" ];
  List.iter
    (fun text ->
      Alcotest.(check string)
        "output requires a checked visible target" "HCSEMA0059"
        (diagnostic text).code)
    [ "\"hello\";"; "if(0) {\"hidden\";}" ];
  Alcotest.(check string) "unbound break" "HCRUN0002" (diagnostic "break;").code;
  Alcotest.(check string)
    "break cannot escape for initializer" "HCRUN0002"
    (diagnostic "while(1) for(break;1;0) ;").code;
  Alcotest.(check string)
    "chained comparison boundary" "HCRUN0003" (diagnostic "if(3<2<1) ;").code;
  List.iter
    (fun text ->
      Alcotest.(check string) text "HCRUN0003" (diagnostic text).code)
    [ "if(2==2==2) ;"; "if(1<2==2) ;"; "if(1!=2!=3) ;" ];
  Alcotest.(check bool)
    "unreachable float is preflighted" true
    (String.starts_with ~prefix:"HCIRVM" (diagnostic "if(0) 1.0;").code)

let replay () =
  let text = "#define ZERO 0\nif(ZERO) 1/0; else {42;}" in
  let lower mode =
    let session, config, source = inputs ~mode text in
    lower_integer_program session ~config ~source |> checked |> fun result ->
    Ir_x87_stack.graph result.value |> Ir_block_graph.human
  in
  Alcotest.(check string)
    "repeatable graph" (lower Preprocessor.Jit) (lower Preprocessor.Jit);
  Alcotest.(check string)
    "same graph for mode-independent input" (lower Preprocessor.Jit)
    (lower Preprocessor.Aot);
  let result =
    run ~mode:Preprocessor.Aot "#ifaot\nif(0) 1/0;\n#else\n1/0;\n#endif\n"
    |> checked
  in
  Alcotest.(check bool)
    "AOT-selected branch" true
    (VM.termination result = VM.Stream_end)

let tests =
  List.map
    (fun (name, text) -> Alcotest.test_case name `Quick (succeeds text))
    [
      ("empty stream", "");
      ("empty statements", ";;{}");
      ("statement batch", "6*7; {1;2;}");
      ("false branch", "if(0) 1/0; else 42;");
      ("true branch", "if(1) 42; else 1/0;");
      ("conditional AND", "if(0 && (1/0)) 1/0;");
      ("conditional OR", "if(1 || (1/0)) 42; else 1/0;");
      ("conditional NOT", "if(!(0 && (1/0))) 42; else 1/0;");
      ("nested conditions", "if((0&&(1/0)) || (1||(1/0))) {if(0) 1/0;}");
      ("while exit", "while(0) 1/0;");
      ("loop break", "while(1) {break; 1/0;}");
      ("nested loop break", "while(1) {while(1) break; break; 1/0;}");
      ("break bypasses do condition", "do {break;} while(1/0);");
      ("do executes once", "do {42;} while(0);");
      ("for skips body and update", "for(0;0;1/0) 1/0;");
      ("break bypasses for update", "for(0;1;1/0) {break;}");
      ("parenthesized comparison value", "if((3<2)<1) 42; else 1/0;");
      ("comparison right operand", "if(0==1<2) 1/0; else 42;");
    ]
  @ [
      Alcotest.test_case "reached faults" `Quick faults;
      Alcotest.test_case "instruction budgets" `Quick budget;
      Alcotest.test_case "unsupported source and preflight" `Quick unsupported;
      Alcotest.test_case "mode and replay" `Quick replay;
    ]
