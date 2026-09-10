open Holyc_lib
module Task = Integer_task
module VM = Ir_integer_interpreter

let create session =
  match Task.create session with
  | Ok task -> task
  | Error message -> Alcotest.fail message

let source_symbols session name =
  Semantic_symbol_table.all_symbols (Session.semantic_symbols session)
  |> List.filter (fun symbol -> Semantic_symbol.name symbol = name)

let run session task text =
  let source = Session.add_source session ~path:"task.hc" ~contents:text in
  Task.run task ~source

let value expected result =
  let execution = Test_integer_program.checked result in
  match VM.final_value execution with
  | Some word -> Alcotest.(check int64) "reached task word" expected word.bits
  | None -> Alcotest.fail "task command produced no word"

let stream_provider_requires_active_buffer () =
  let session = Session.create () in
  let task = create session in
  match
    run session task {|extern U0 StreamPrint(U8 *fmt,...);StreamPrint("42;");|}
  with
  | Error (diagnostic :: _) ->
      Alcotest.(check string)
        diagnostic.Diagnostic.message "HCIRVM0027" diagnostic.code;
      Alcotest.(check int)
        "formatting precedes inactive buffer diagnostic" 7
        (Task.output_work task);
      Alcotest.(check string)
        "inactive service has no ordinary capture" "" (Task.output_bytes task)
  | _ -> Alcotest.fail "StreamPrint needs an active generation buffer"

let persistent_scalar () =
  let session = Session.create () in
  let task = create session in
  ignore (run session task "I64 N=40;" |> Test_integer_program.checked);
  value 42L (run session task "N+=2;");
  value 42L (run session task "N;");
  value 43L (run session task "++N;")

let persistent_array () =
  let session = Session.create () in
  let task = create session in
  ignore (run session task "I64 A[2]={40,2};" |> Test_integer_program.checked);
  value 42L (run session task "A[0]+=A[1];");
  value 44L (run session task "A[0]+A[1];")

let persistent_unbraced_array () =
  let session = Session.create () in
  let task = create session in
  ignore (run session task "I64 A[1+1]=40,2;" |> Test_integer_program.checked);
  Alcotest.(check int)
    "task grammar preserves checked dimension work" 3 (Task.dimension_work task);
  let preparation = Task.initializer_steps task in
  value 42L (run session task "A[0]+A[1];");
  Alcotest.(check int)
    "later task execution does not repeat extent preparation" preparation
    (Task.initializer_steps task)

let reached_fault () =
  let session = Session.create () in
  let task = create session in
  ignore (run session task "I64 N=40;" |> Test_integer_program.checked);
  (match run session task "N+=2;1/0;" with
  | Error (diagnostic :: _) ->
      Alcotest.(check string)
        "reached division fault" "HCIRVM0009" diagnostic.code
  | _ -> Alcotest.fail "expected the reached arithmetic fault");
  value 42L (run session task "N;")

let parse session text =
  let source = Session.add_source session ~path:"pending.hc" ~contents:text in
  let config =
    match Preprocessor.Config.create () with
    | Ok config -> config
    | Error message -> Alcotest.fail message
  in
  let parsed =
    Parser.parse ~sources:(Session.sources session)
      ~definitions:(Session.definitions session)
      ~symbols:(Session.symbols session) ~config source
  in
  match parsed.ast with
  | Some ast -> ast
  | None -> Alcotest.fail "test command did not parse"

let compile session task text =
  Task.compile_ast task (parse session text) |> Test_integer_program.checked

let retained_array_sizeof () =
  List.iter
    (fun legacy ->
      List.iter
        (fun (declaration, size, work) ->
          let session = Session.create () in
          let task = create session in
          if legacy then
            let command =
              compile (Session.fork_frontend session) task declaration
            in
            ignore (Task.execute task command |> Test_integer_program.checked)
          else
            ignore (run session task declaration |> Test_integer_program.checked);
          let preparation = Task.initializer_steps task in
          let dimension_work = if legacy then 0 else work in
          Alcotest.(check int)
            "original source preparation policy" dimension_work
            (Task.dimension_work task);
          value size (run session task "sizeof A;");
          value size (run session task "sizeof A;");
          Alcotest.(check int)
            "retained metadata reads do not prepare again" preparation
            (Task.initializer_steps task);
          value size (run session task "U8 B[sizeof A];sizeof B;");
          value size (run session task "sizeof B;");
          Alcotest.(check int)
            "query-based new extent adds only its own visit"
            (dimension_work + 1) (Task.dimension_work task))
        [
          ("U8 A[1+2]={40,2,0};", 3L, 3);
          ("U16 A[2][3]={{1,2,3},{4,5,6}};", 12L, 2);
          ("I64 A[2][3];", 48L, 2);
        ])
    [ false; true ]

let retained_array_sizeof_shadow () =
  List.iter
    (fun legacy ->
      let session = Session.create () in
      let task = create session in
      let admit text =
        if legacy then
          compile (Session.fork_frontend session) task text
          |> Task.execute task |> Test_integer_program.checked |> ignore
        else run session task text |> Test_integer_program.checked |> ignore
      in
      admit "U8 A[3];";
      ignore
        (run session task "I64 Saved(){return sizeof A;}"
        |> Test_integer_program.checked);
      admit "U8 A[5];";
      value 3L (run session task "Saved();");
      value 5L (run session task "sizeof A;"))
    [ false; true ]

let legacy_admission_frontend_visibility () =
  let session = Session.create () in
  let task = create session in
  let frontend = Session.fork_frontend session in
  let command = compile frontend task "I64 N=42;" in
  value 0L (run session task "#ifdef N\n1;\n#else\n0;\n#endif");
  ignore (Task.execute task command |> Test_integer_program.checked);
  value 42L (run session task "#ifdef N\nN;\n#else\n0;\n#endif")

let legacy_admission_function_shape () =
  let session = Session.create () in
  let task = create session in
  let frontend = Session.fork_frontend session in
  let command =
    compile frontend task
      "I64 Answer(){return 42;}I64 Defaulted(I64 n=42){return n;}"
  in
  ignore (Task.execute task command |> Test_integer_program.checked);
  value 42L (run session task "Answer;");
  let ast = parse session "Defaulted;" in
  match ast.items with
  | [
   Ast.Top_level_statement
     (Ast.Expression_statement
        { expression_statement_expression = Ast.Call_expression call; _ });
  ] ->
      Alcotest.(check bool)
        "defaulted header supplies an omitted parser argument" true
        (match call.call_arguments with
        | [ { call_argument_value = Ast.Omitted_call_argument; _ } ] -> true
        | _ -> false)
  | _ -> Alcotest.fail "expected a defaulted parenthesis-free call"

let fault code = function
  | Error (diagnostic :: _) ->
      Alcotest.(check string) diagnostic.Diagnostic.message code diagnostic.code
  | _ -> Alcotest.fail "expected task diagnostic"

let retained_array_extent_admission_boundaries () =
  let session = Session.create () in
  let task = create session in
  let command =
    compile (Session.fork_frontend session) task "U8 A[3]={40,2,0};1/0;"
  in
  value 0L (run session task "defined A;");
  fault "HCIRVM0009" (Task.execute task command);
  value 3L (run session task "sizeof A;");
  value 42L (run session task "A[0]+A[1];");
  fault "HCIRVM0026" (Task.execute task command);
  value 3L (run session task "sizeof A;");
  let session = Session.create () in
  let task =
    Task.create ~max_global_bytes:2 session
    |> Test_global_dimension_binding.checked
  in
  let command =
    compile (Session.fork_frontend session) task "U8 Rejected[3];"
  in
  fault "HCIRVM0016" (Task.execute task command);
  value 0L (run session task "defined Rejected;");
  let command =
    compile (Session.fork_frontend session) task "U8 Accepted[2];"
  in
  ignore (Task.execute task command |> Test_integer_program.checked);
  value 2L (run session task "sizeof Accepted;")

let legacy_fault_publication () =
  let session = Session.create () in
  let task = create session in
  let frontend = Session.fork_frontend session in
  let command =
    compile frontend task "I64 N=40;I64 F(I64 n){return n;}N=42;1/0;"
  in
  fault "HCIRVM0009" (Task.execute task command);
  value 42L (run session task "#ifdef N\nF(N);\n#else\n0;\n#endif");
  fault "HCIRVM0026" (Task.execute task command);
  value 42L (run session task "F(N);")

let legacy_preflight_and_shadow () =
  let session = Session.create () in
  let task =
    Task.create ~max_global_bytes:1 session |> function
    | Ok task -> task
    | Error message -> Alcotest.fail message
  in
  let command =
    compile (Session.fork_frontend session) task "I64 Rejected=42;"
  in
  fault "HCIRVM0016" (Task.execute task command);
  value 0L (run session task "#ifdef Rejected\n1;\n#else\n0;\n#endif");
  let session = Session.create () in
  let task = create session in
  ignore (run session task "I64 N=1;" |> Test_integer_program.checked);
  let entries () =
    Symbol_visibility.Environment.all (Session.symbols session)
    |> List.filter (fun entry -> Symbol_visibility.name entry = "N")
  in
  Alcotest.(check int)
    "source admission creates no duplicate frontend entry" 1
    (List.length (entries ()));
  let command = compile (Session.fork_frontend session) task "I64 N=40;" in
  ignore (Task.execute task command |> Test_integer_program.checked);
  Alcotest.(check int)
    "legacy admission publishes once" 2
    (List.length (entries ()));
  value 40L (run session task "N;");
  ignore (run session task "I64 N=42;" |> Test_integer_program.checked);
  Alcotest.(check int)
    "later source publication remains distinct" 3
    (List.length (entries ()));
  let newest =
    Symbol_visibility.Environment.find_preprocessor (Session.symbols session)
      "N"
  in
  fault "HCIRVM0026" (Task.execute task command);
  Alcotest.(check bool)
    "legacy replay cannot replace newer source entry" true
    (match
       ( newest,
         Symbol_visibility.Environment.find_preprocessor
           (Session.symbols session) "N" )
     with
    | Symbol_visibility.Present left, Symbol_visibility.Present right ->
        left == right
    | _ -> false);
  value 42L (run session task "N;")

let selected_before_shadow () =
  let session = Session.create () in
  let task = create session in
  ignore (run session task "I64 N=40;" |> Test_integer_program.checked);
  let pending = compile session task "N+=2;" in
  ignore
    (run session task "I64 N=100; I64 Other[4]={1,2,3,4};"
    |> Test_integer_program.checked);
  value 42L (Task.execute task pending);
  value 100L (run session task "N;");
  value 10L (run session task "Other[0]+Other[1]+Other[2]+Other[3];")

let replay_and_foreign_owner () =
  let session = Session.create () in
  let task = create session in
  let other = create session in
  let ast = parse session "40+2;" in
  let command = Task.compile_ast task ast |> Test_integer_program.checked in
  fault "HCIRVM0026" (Task.execute other command);
  value 42L (Task.execute task command);
  let before = Task.executed_steps task in
  fault "HCIRVM0026" (Task.execute task command);
  let same_ast = Task.compile_ast task ast |> Test_integer_program.checked in
  fault "HCIRVM0026" (Task.execute task same_ast);
  Alcotest.(check int)
    "replay charges no instructions" before (Task.executed_steps task);
  value 42L (run session task "40+2;")

let failed_command_cannot_replay () =
  let session = Session.create () in
  let task = create session in
  ignore (run session task "I64 N=40;" |> Test_integer_program.checked);
  let command = compile session task "N+=2;1/0;" in
  fault "HCIRVM0009" (Task.execute task command);
  fault "HCIRVM0026" (Task.execute task command);
  value 42L (run session task "N;")

let rejected_command_has_no_effects () =
  let session = Session.create () in
  let task = create session in
  ignore (run session task "I64 N=40;" |> Test_integer_program.checked);
  let command =
    compile session task "extern I64 Missing();I64 M=1;N=99;if(0)Missing();"
  in
  let before = Task.executed_steps task in
  fault "HCIRVM0014" (Task.execute task command);
  fault "HCIRVM0014" (Task.execute task command);
  Alcotest.(check int)
    "preflight charges no instructions" before (Task.executed_steps task);
  value 40L (run session task "N;");
  fault "HCSEMA0054" (run session task "M;")

let new_function_reads_old_global () =
  let session = Session.create () in
  let task = create session in
  ignore (run session task "I64 N=40;" |> Test_integer_program.checked);
  value 42L (run session task "I64 Add(){return N+2;}Add();")

let initializer_reads_old_global () =
  let session = Session.create () in
  let task = create session in
  ignore (run session task "I64 N=40;" |> Test_integer_program.checked);
  value 42L (run session task "I64 M=N+2;M;");
  value 42L (run session task "M;")

let narrow_storage () =
  let session = Session.create () in
  let task = create session in
  ignore
    (run session task "I8 N=127;U16 A[2]={65535,42};"
    |> Test_integer_program.checked);
  value (-128L) (run session task "++N;");
  value 0L (run session task "++A[0];");
  value 42L (run session task "A[1];")

let cumulative_global_limit () =
  let session = Session.create () in
  let task =
    match Task.create ~max_global_bytes:16 session with
    | Ok task -> task
    | Error message -> Alcotest.fail message
  in
  ignore (run session task "I64 N=40;" |> Test_integer_program.checked);
  ignore (run session task "I64 M=2;" |> Test_integer_program.checked);
  fault "HCIRVM0016" (run session task "I8 Excess=1;N=0;");
  value 42L (run session task "N+M;");
  fault "HCSEMA0054" (run session task "Excess;")

let cumulative_output () =
  let session = Session.create () in
  let task =
    match Task.create ~max_output_bytes:2 session with
    | Ok task -> task
    | Error message -> Alcotest.fail message
  in
  ignore
    (run session task "extern U0 PutChars(U64 ch);PutChars('4');"
    |> Test_integer_program.checked);
  ignore
    (run session task "extern U0 PutChars(U64 ch);PutChars('2');"
    |> Test_integer_program.checked);
  Alcotest.(check string) "capture joins commands" "42" (Task.output_bytes task);
  fault "HCIRVM0022"
    (run session task "extern U0 PutChars(U64 ch);PutChars('!');");
  Alcotest.(check string)
    "failed append retains prior capture" "42" (Task.output_bytes task)

let retained_array_argument () =
  let session = Session.create () in
  let task = create session in
  ignore (run session task "I64 A[2]={40,2};" |> Test_integer_program.checked);
  value 42L (run session task "I64 Sum(I64 *p){return p[0]+p[1];}Sum(A);");
  value 42L
    (run session task
       "I64 Sum(I64 *p){return p[0]+p[1];}I64 F(){return Sum(A);}F();")

let retained_array_output () =
  let session = Session.create () in
  let task = create session in
  ignore (run session task "U8 Text[3]=\"42\";" |> Test_integer_program.checked);
  ignore
    (run session task "extern U0 Print(U8 *fmt,...);Print(\"%s\",Text);"
    |> Test_integer_program.checked);
  Alcotest.(check string)
    "retained bytes passed to formatter" "42" (Task.output_bytes task)

let compilation_budget () =
  let session = Session.create () in
  let task =
    match Task.create ~max_initializer_steps:1 session with
    | Ok task -> task
    | Error message -> Alcotest.fail message
  in
  fault "HCIRVM0007" (Task.compile_ast task (parse session "I64 N=40;"));
  value 42L (run session task "42;");
  fault "HCSEMA0054" (run session task "N;")

let shared_compilation_budget () =
  let session = Session.create () in
  let task =
    match Task.create ~max_initializer_steps:6 session with
    | Ok task -> task
    | Error message -> Alcotest.fail message
  in
  let first = compile session task "I64 N=40;" in
  Alcotest.(check int)
    "literal preparation precedes admission" 3
    (Task.initializer_steps task);
  let second = compile session task "I64 M=2;" in
  Alcotest.(check int)
    "pending commands share preparation work" 6
    (Task.initializer_steps task);
  fault "HCIRVM0007" (Task.compile_ast task (parse session "I64 Excess=1;"));
  Alcotest.(check int)
    "exhaustion does not exceed preparation bound" 6
    (Task.initializer_steps task);
  ignore (Task.execute task first |> Test_integer_program.checked);
  ignore (Task.execute task second |> Test_integer_program.checked);
  value 42L (run session task "N+M;")

let failed_preparation_charges_work () =
  let session = Session.create () in
  let task =
    match Task.create ~max_initializer_steps:6 session with
    | Ok task -> task
    | Error message -> Alcotest.fail message
  in
  fault "HCIRVM0009" (Task.compile_ast task (parse session "I64 Failed=1/0;"));
  Alcotest.(check int)
    "faulting preparation consumes reached instructions" 3
    (Task.initializer_steps task);
  ignore (run session task "I64 N=42;" |> Test_integer_program.checked);
  Alcotest.(check int)
    "successful preparation consumes remaining budget" 6
    (Task.initializer_steps task);
  fault "HCIRVM0007" (Task.compile_ast task (parse session "I64 M=1;"));
  value 42L (run session task "N;")

let larger_preparation_limit () =
  let session = Session.create () in
  let task =
    match Task.create ~max_initializer_steps:100_002 session with
    | Ok task -> task
    | Error message -> Alcotest.fail message
  in
  let source = "U8 Bytes[100001]=\"" ^ String.make 100_000 '*' ^ "\";" in
  ignore (run session task source |> Test_integer_program.checked);
  Alcotest.(check int)
    "configured task limit includes copy and dimension work" 100_002
    (Task.initializer_steps task);
  Alcotest.(check int)
    "task retains the separate numeric visit tally" 1 (Task.dimension_work task);
  value 42L (run session task "Bytes[99999];")

let reconstructed_module_cannot_replay () =
  let session = Session.create () in
  let task = create session in
  let ast = parse session "40+2;" in
  let first = Task.compile_ast task ast |> Test_integer_program.checked in
  value 42L (Task.execute task first);
  let wrapper =
    Ast.make_module ~source:ast.source ~span:ast.span
      ~items:(List.map Fun.id ast.items)
  in
  let repeated =
    Task.compile_ast task wrapper |> Test_integer_program.checked
  in
  fault "HCIRVM0026" (Task.execute task repeated)

let retained_item_cannot_be_recompiled () =
  let session = Session.create () in
  let task = create session in
  let ast = parse session "40;42;" in
  let first = Task.compile_ast task ast |> Test_integer_program.checked in
  value 42L (Task.execute task first);
  let item =
    match List.hd ast.items with
    | Ast.Top_level_statement statement -> Ast.Top_level_statement statement
    | _ -> Alcotest.fail "expected statement source"
  in
  let subset =
    Ast.make_module ~source:ast.source ~span:ast.span ~items:[ item ]
  in
  fault "HCIRVM0026" (Task.compile_ast task subset)

let reconstructed_statement_cannot_replay () =
  let session = Session.create () in
  let task = create session in
  ignore (run session task "I64 N=40;" |> Test_integer_program.checked);
  let ast = parse session "++N;" in
  let command = Task.compile_ast task ast |> Test_integer_program.checked in
  value 41L (Task.execute task command);
  let item =
    match List.hd ast.items with
    | Ast.Top_level_statement (Ast.Expression_statement statement) ->
        Ast.Top_level_statement (Ast.Expression_statement statement)
    | _ -> Alcotest.fail "expected expression statement source"
  in
  let wrapper =
    Ast.make_module ~source:ast.source ~span:ast.span ~items:[ item ]
  in
  let repeated =
    Task.compile_ast task wrapper |> Test_integer_program.checked
  in
  fault "HCIRVM0026" (Task.execute task repeated);
  value 41L (run session task "N;")

let independent_tasks_and_unknown_cells () =
  let session = Session.create () in
  let task = create session and other = create session in
  ignore
    (run session task "I64 N=40;I64 Unknown;" |> Test_integer_program.checked);
  ignore (run session other "I64 N=2;" |> Test_integer_program.checked);
  value 42L (run session task "N+=2;");
  value 2L (run session other "N;");
  fault "HCIRVM0012" (run session task "Unknown;");
  value 42L (run session task "Unknown=42;");
  value 42L (run session task "Unknown;")

let independent_function_headers () =
  let session = Session.create () in
  let task = create session in
  ignore (run session task "I64 F(){return 42;}" |> Test_integer_program.checked);
  let other = create session in
  value 0L (run session other "#ifdef F\n1;\n#else\n0;\n#endif");
  ignore
    (run session other "I64 F(I64 n){return n+1;}"
    |> Test_integer_program.checked);
  value 42L (run session task "F;");
  value 3L (run session other "F(2);");
  let entries =
    Symbol_visibility.Environment.all (Session.symbols session)
    |> List.filter (fun entry -> Symbol_visibility.name entry = "F")
  in
  Alcotest.(check int)
    "root sees each completed task header exactly once" 2 (List.length entries)

let independent_definitions () =
  let session = Session.create () in
  ignore (parse session "#define Shared 40\n");
  let task = create session and other = create session in
  ignore (run session task "#define N 40\n0;" |> Test_integer_program.checked);
  ignore (run session other "#define N 2\n0;" |> Test_integer_program.checked);
  value 42L (run session task "N+2;");
  value 42L (run session other "Shared+N;");
  let copied = Session.fork_frontend (Task.frontend task) in
  ignore (run session task "#define N 100\n0;" |> Test_integer_program.checked);
  Alcotest.(check string)
    "detached frontend keeps the task's original definition" "40"
    (Definition.Environment.find (Session.definitions copied) "N"
    |> Option.get |> Definition.replacement);
  let later = create session in
  value 0L (run session later "#ifdef N\n1;\n#else\n0;\n#endif");
  value 42L (run session later "Shared+2;");
  let definitions = Definition.Environment.all (Session.definitions session) in
  Alcotest.(check int)
    "root sees each task definition exactly once" 4 (List.length definitions)

let cumulative_instruction_limit () =
  List.iter
    (fun (limit, succeeds) ->
      let session = Session.create () in
      let task =
        match Task.create ~max_steps:limit session with
        | Ok task -> task
        | Error message -> Alcotest.fail message
      in
      value 40L (run session task "40;");
      if succeeds then value 42L (run session task "42;")
      else fault "HCIRVM0007" (run session task "42;");
      Alcotest.(check int)
        "commands share exact execution budget" limit (Task.executed_steps task);
      fault "HCIRVM0007" (run session task "1;"))
    [ (6, true); (5, false) ]

let cumulative_literal_limit () =
  let session = Session.create () in
  let task =
    match Task.create ~max_literal_bytes:4 session with
    | Ok task -> task
    | Error message -> Alcotest.fail message
  in
  value 42L (run session task "(\"*\")[0];");
  value 42L (run session task "(\"*\")[0];");
  let before = Task.executed_steps task in
  fault "HCIRVM0021" (run session task "(\"!\")[0];");
  Alcotest.(check int)
    "literal rejection precedes execution" before (Task.executed_steps task);
  value 42L (run session task "42;")

let cumulative_output_work () =
  let session = Session.create () in
  let task =
    match Task.create ~max_output_work:4 session with
    | Ok task -> task
    | Error message -> Alcotest.fail message
  in
  List.iter
    (fun byte ->
      ignore
        (run session task
           ("extern U0 PutChars(U64 ch);PutChars('" ^ byte ^ "');")
        |> Test_integer_program.checked))
    [ "4"; "2" ];
  Alcotest.(check int)
    "packed-byte scans and appends share work" 4 (Task.output_work task);
  fault "HCIRVM0023"
    (run session task "extern U0 PutChars(U64 ch);PutChars('!');");
  Alcotest.(check string)
    "work exhaustion retains capture" "42" (Task.output_bytes task);
  Alcotest.(check int) "work never exceeds bound" 4 (Task.output_work task)

let retained_function () =
  let session = Session.create () in
  let task = create session in
  ignore
    (run session task "I64 Add(I64 a,I64 b){return a+b;}"
    |> Test_integer_program.checked);
  value 42L (run session task "Add(20,22);");
  ignore
    (run session task "I64 Answer(){return Add(19,23);}"
    |> Test_integer_program.checked);
  value 42L (run session task "Answer();")

let retained_function_global_owner () =
  let session = Session.create () in
  let task = create session in
  ignore
    (run session task "I64 N=40;I64 Next(){return ++N;}"
    |> Test_integer_program.checked);
  value 41L (run session task "Next();");
  ignore
    (run session task "I64 N=100;I64 Extra[8]={1,2,3,4,5,6,7,8};"
    |> Test_integer_program.checked);
  value 42L (run session task "Next();");
  value 100L (run session task "N;")

let retained_function_static_owner () =
  let session = Session.create () in
  let task = create session in
  ignore
    (run session task "I64 Next(){static I64 N=40;return ++N;}"
    |> Test_integer_program.checked);
  value 41L (run session task "Next();");
  ignore
    (run session task "I64 Extra[8]={1,2,3,4,5,6,7,8};I64 F(){return 0;}"
    |> Test_integer_program.checked);
  value 42L (run session task "Next();")

let retained_function_literal_owner () =
  let session = Session.create () in
  let task = create session in
  ignore
    (run session task "I64 Next(){U8 *s=\"(\";return ++s[0];}"
    |> Test_integer_program.checked);
  value 41L (run session task "Next();");
  value 42L (run session task "(\"*\")[0];");
  value 42L (run session task "Next();")

let retained_function_pending_selection () =
  let session = Session.create () in
  let task = create session in
  ignore
    (run session task "I64 F(I64 n){return n+1;}"
    |> Test_integer_program.checked);
  let pending = compile session task "F(41);" in
  ignore
    (run session task "I64 F(I64 a,I64 b){return a+b;}"
    |> Test_integer_program.checked);
  value 42L (Task.execute task pending);
  value 42L (run session task "F(20,22);")

let retained_function_dependencies () =
  let session = Session.create () in
  let task = create session in
  ignore
    (run session task "I64 Base(){return 33;}" |> Test_integer_program.checked);
  ignore
    (run session task "I64 Wrap(){return Base()+9;}"
    |> Test_integer_program.checked);
  ignore
    (run session task "I64 Base(){return 100;}" |> Test_integer_program.checked);
  value 42L (run session task "Wrap();");
  value 100L (run session task "Base();")

let retained_function_restores_literal_owner () =
  let session = Session.create () in
  let task = create session in
  ignore
    (run session task "I64 Old(){return (\" \" )[0];}"
    |> Test_integer_program.checked);
  ignore
    (run session task "I64 Wrap(){return Old()+(\"*\")[0];}"
    |> Test_integer_program.checked);
  value 74L (run session task "Wrap();");
  value 74L (run session task "Old()+(\"*\")[0];")

let retained_function_recursion () =
  let session = Session.create () in
  let task = create session in
  ignore
    (run session task "I64 Recur(I64 n){if(n)return 1+Recur(n-1);return 40;}"
    |> Test_integer_program.checked);
  ignore
    (run session task "I64 Other(){return 0;}" |> Test_integer_program.checked);
  value 42L (run session task "Recur(2);")

let retained_function_initializers () =
  let session = Session.create () in
  let task = create session in
  ignore
    (run session task "I64 Seed(){return 40;}" |> Test_integer_program.checked);
  ignore
    (run session task "I64 N=Seed();I64 Next(){static I64 S=Seed();return ++S;}"
    |> Test_integer_program.checked);
  value 40L (run session task "N;");
  value 41L (run session task "Next();");
  value 42L (run session task "Next();")

let retained_function_pointer_owner () =
  let session = Session.create () in
  let task = create session in
  ignore
    (run session task
       "I64 A[2]={40,2};I64 Sum(){I64 *p=&A[0];return p[0]+p[1];}"
    |> Test_integer_program.checked);
  ignore (run session task "I64 A[2]={100,200};" |> Test_integer_program.checked);
  value 42L (run session task "Sum();");
  value 300L (run session task "A[0]+A[1];")

let retained_function_providers () =
  let session = Session.create () in
  let task = create session in
  ignore
    (run session task "extern U0 Print(U8 *fmt,...);extern U0 PutChars(U64 ch);"
    |> Test_integer_program.checked);
  ignore
    (run session task "Print(\"%d\",4);PutChars('2');"
    |> Test_integer_program.checked);
  Alcotest.(check string)
    "retained provider declarations keep checked signatures" "42"
    (Task.output_bytes task)

let retained_function_rejections () =
  let session = Session.create () in
  let task = create session in
  ignore
    (run session task "I64 N=40;I64 F(I64 n){return N+n;}"
    |> Test_integer_program.checked);
  List.iter
    (fun source ->
      let before = Task.executed_steps task in
      (match run session task source with
      | Error (_ :: _) -> ()
      | _ -> Alcotest.fail "invalid retained argument list executed");
      Alcotest.(check int)
        "rejected call consumes no runtime work" before
        (Task.executed_steps task);
      value 40L (run session task "N;"))
    [ "N=100;F();"; "N=100;F(1,2);"; "N=100;F(\"bad\");" ];
  value 42L (run session task "F(2);")

let retained_function_literal_capacity () =
  let session = Session.create () in
  let task =
    match Task.create ~max_literal_bytes:2 session with
    | Ok task -> task
    | Error message -> Alcotest.fail message
  in
  ignore
    (run session task "I64 Next(){U8 *s=\"(\";return ++s[0];}"
    |> Test_integer_program.checked);
  value 41L (run session task "Next();");
  value 42L (run session task "Next();");
  fault "HCIRVM0021" (run session task "(\"!\")[0];");
  value 43L (run session task "Next();")

let retained_function_fault_and_join () =
  let session = Session.create () in
  let task = create session in
  ignore
    (run session task "extern I64 Next();I64 N=40;I64 Next(){return ++N;}"
    |> Test_integer_program.checked);
  fault "HCIRVM0009" (run session task "Next();1/0;");
  value 42L (run session task "Next();")

let retained_function_publication_boundary () =
  let session = Session.create () in
  let task = create session in
  fault "HCIRVM0014"
    (run session task
       "I64 Rejected(){return 100;}extern I64 Missing();Missing();");
  fault "HCSEMA0054" (run session task "Rejected();");
  fault "HCIRVM0009" (run session task "I64 Admitted(){return 42;}1/0;");
  value 42L (run session task "Admitted();")

let retained_initializer_guard () =
  let session = Session.create () in
  let task = create session in
  ignore
    (run session task "I64 Shift(I64 n){return n<<1;}"
    |> Test_integer_program.checked);
  ignore
    (run session task "I64 Wrap(){return Shift(21);}"
    |> Test_integer_program.checked);
  value 42L (run session task "Wrap();");
  List.iter
    (fun text ->
      let before = Task.executed_steps task in
      fault "HCRUN0006" (run session task text);
      Alcotest.(check int)
        "initializer guard runs before execution" before
        (Task.executed_steps task))
    [
      "I64 N=Shift(21);";
      "I64 N=Wrap();";
      "I64 F(){static I64 N=Wrap();return N;}";
    ]

let retained_function_depth_limit () =
  let session = Session.create () in
  let task =
    match Task.create ~max_call_depth:2 session with
    | Ok task -> task
    | Error message -> Alcotest.fail message
  in
  ignore
    (run session task "I64 Recur(I64 n){if(n)return 1+Recur(n-1);return 40;}"
    |> Test_integer_program.checked);
  value 41L (run session task "Recur(1);");
  fault "HCIRVM0015" (run session task "Recur(2);");
  value 40L (run session task "Recur(0);")

let retained_unfinished_selection () =
  let session = Session.create () in
  let task = create session in
  ignore
    (run session task "I64 N=0;extern I64 F(I64 n);"
    |> Test_integer_program.checked);
  let pending = compile session task "F(++N);" in
  ignore
    (run session task "I64 F(I64 n){N=100;return n+2;}"
    |> Test_integer_program.checked);
  fault "HCIRVM0014" (Task.execute task pending);
  value 0L (run session task "N;");
  value 42L (run session task "F(40);")

let retained_cross_command_join () =
  let session = Session.create () in
  let task = create session in
  ignore
    (run session task "extern I64 Joined(I64 x);"
    |> Test_integer_program.checked);
  let original = List.hd (source_symbols session "Joined") in
  ignore
    (run session task "I64 Joined(I64 value){I64 local=value+2;return local;}"
    |> Test_integer_program.checked);
  let definitions =
    Task.compiled_units task
    |> List.concat_map Holyc_lib__Driver.Integer_unit.functions
  in
  let definition =
    List.find
      (fun (definition : VM.function_definition) ->
        Semantic_symbol.name (Ir_function_body.symbol definition.body)
        = "Joined")
      definitions
  in
  Alcotest.(check bool)
    "extern keeps its canonical callable identity" true
    (Ir_function_body.callable_symbol definition.body == original);
  Alcotest.(check bool)
    "definition retains its own source and frame identity" true
    (Ir_function_body.symbol definition.body != original);
  value 42L (run session task "Joined(40);")

let retained_join_shadow () =
  let session = Session.create () in
  let task = create session in
  List.iter
    (fun source ->
      ignore (run session task source |> Test_integer_program.checked))
    [
      "extern I64 Joined();"; "extern I64 Joined();"; "I64 Joined(){return 42;}";
    ];
  ignore
    (run session task "I64 Earlier(){return Joined();}"
    |> Test_integer_program.checked);
  ignore
    (run session task "I64 Joined(){return 99;}" |> Test_integer_program.checked);
  value 42L (run session task "Earlier();");
  value 99L (run session task "Joined();");
  ignore
    (run session task "extern I64 Joined();" |> Test_integer_program.checked);
  fault "HCIRVM0014" (run session task "Joined();");
  value 42L (run session task "Earlier();")

let retained_join_dimension () =
  ignore
    (Test_integer_output.run
       "I64 N=1;extern I64 Extent();I64 Extent(){return ++N;}I64 \
        A[Extent()];A[1]=24;A[1]+sizeof A+N;"
    |> Test_integer_output.expect "")

let retained_declaration_scope () =
  let session = Session.create () in
  let task = create session in
  ignore (run session task "I64 A=40;" |> Test_integer_program.checked);
  value 42L (run session task "I64 F(){return A+2;}F();");
  let a = List.hd (source_symbols session "A") in
  let f = List.hd (source_symbols session "F") in
  Alcotest.(check bool)
    "separate task declarations retain one module scope" true
    (Semantic_symbol.Scope_id.equal
       (Semantic_symbol.scope_id a)
       (Semantic_symbol.scope_id f));
  Alcotest.(check int)
    "completion does not allocate another global symbol" 1
    (List.length (source_symbols session "A"));
  Alcotest.(check int)
    "completion does not allocate another function symbol" 1
    (List.length (source_symbols session "F"))

let reached_semantic_publication () =
  List.iter
    (fun (name, source) ->
      let session = Session.create () in
      let task = create session in
      Alcotest.(check bool)
        "source fails after declaration publication" true
        (Result.is_error (run session task source));
      Alcotest.(check int)
        "reached provisional symbol remains assigned" 1
        (List.length (source_symbols session name));
      value 42L (run session task "I64 Good=42;Good;");
      Alcotest.(check bool)
        "unfinished declaration has no runtime binding" true
        (Result.is_error (run session task (name ^ ";"))))
    [ ("Broken", "I64 Broken=;"); ("Unfinished", "I64 Unfinished(I64 n=);") ]

let reconstructed_source_is_rejected () =
  let session = Session.create () in
  let task = create session in
  let original = Session.add_source session ~path:"owned.hc" ~contents:"42;" in
  let substitute =
    Source_file.create ~id:(Source_file.id original)
      ~path:(Source_file.path original)
      ~display_path:(Source_file.display_path original)
      ~contents:(Source_file.contents original)
  in
  fault "HCRUN0004" (Task.run task ~source:substitute);
  Alcotest.(check int)
    "foreign source executes nothing" 0 (Task.executed_steps task);
  value 42L (Task.run task ~source:original)

let uninitialized_seed_keeps_storage_identity () =
  let session = Session.create () in
  let task = create session in
  ignore (run session task "I64 N;" |> Test_integer_program.checked);
  let symbol = List.hd (source_symbols session "N") in
  fault "HCIRVM0012" (run session task "N;");
  value 42L (run session task "N=42;");
  value 42L (run session task "N;");
  Alcotest.(check bool)
    "uninitialized declaration keeps its single assigned symbol" true
    (match source_symbols session "N" with
    | [ current ] -> current == symbol
    | _ -> false)

let query_presence_without_admission () =
  let session = Session.create () in
  let task = create session in
  Alcotest.(check bool)
    "declaration fails after parser publication" true
    (run session task "I64 Broken=;" |> Result.is_error);
  value 1L (run session task "defined Broken;");
  value 1L (run session task "defined return;");
  value 0L (run session task "defined Missing;");
  value 0L (run session task "#define NUMBER 42\ndefined NUMBER;");
  value 1L (run session task "I64 Self=defined Self;Self;");
  value 1L (run session task "I64 F(I64 n){return defined n;}F(0);")

let query_builtin_metadata () =
  List.iter
    (fun (source, expected) ->
      let session = Session.create () in
      let task = create session in
      value expected (run session task source))
    [ ("sizeof U8;", 1L); ("sizeof I64i;", 8L); ("sizeof U8*;", 8L) ]

let query_self_metadata () =
  let session = Session.create () in
  let task = create session in
  value 8L (run session task "I64 N=sizeof N;N;")

let tests =
  [
    Alcotest.test_case
      "retained array extents follow preflight and reached admission" `Quick
      retained_array_extent_admission_boundaries;
    Alcotest.test_case "retained arrays preserve original declared sizeof"
      `Quick retained_array_sizeof;
    Alcotest.test_case "retained sizeof survives later array shadow" `Quick
      retained_array_sizeof_shadow;
    Alcotest.test_case "unbraced checked arrays persist across task commands"
      `Quick persistent_unbraced_array;
    Alcotest.test_case "sizeof uses the exact seeded primitive record" `Quick
      query_builtin_metadata;
    Alcotest.test_case
      "sizeof sees its published global before initializer admission" `Quick
      query_self_metadata;
    Alcotest.test_case
      "defined sees published records without runtime admission" `Quick
      query_presence_without_admission;
    Alcotest.test_case "independent tasks retain their function headers" `Quick
      independent_function_headers;
    Alcotest.test_case "independent tasks retain their definitions" `Quick
      independent_definitions;
    Alcotest.test_case "legacy reached faults retain frontend publications"
      `Quick legacy_fault_publication;
    Alcotest.test_case
      "legacy preflight and replay preserve frontend publication order" `Quick
      legacy_preflight_and_shadow;
    Alcotest.test_case "legacy AST admission publishes frontend globals" `Quick
      legacy_admission_frontend_visibility;
    Alcotest.test_case "legacy AST admission publishes exact function shape"
      `Quick legacy_admission_function_shape;
    Alcotest.test_case "StreamPrint requires an active task buffer" `Quick
      stream_provider_requires_active_buffer;
    Alcotest.test_case
      "uninitialized seed retains unknown storage and exact symbol" `Quick
      uninitialized_seed_keeps_storage_identity;
    Alcotest.test_case "task source requires exact registered object" `Quick
      reconstructed_source_is_rejected;
    Alcotest.test_case "task declarations retain one semantic scope" `Quick
      retained_declaration_scope;
    Alcotest.test_case "parse failure retains reached semantic publication"
      `Quick reached_semantic_publication;
    Alcotest.test_case "separate commands retain scalar writes" `Quick
      persistent_scalar;
    Alcotest.test_case "separate commands retain array cells" `Quick
      persistent_array;
    Alcotest.test_case "writes before a fault remain reached" `Quick
      reached_fault;
    Alcotest.test_case "pending reference survives later shadow and allocations"
      `Quick selected_before_shadow;
    Alcotest.test_case "command owner and exact AST replay" `Quick
      replay_and_foreign_owner;
    Alcotest.test_case "fault consumes command" `Quick
      failed_command_cannot_replay;
    Alcotest.test_case "late preflight preserves cells and namespace" `Quick
      rejected_command_has_no_effects;
    Alcotest.test_case "new function reads retained global" `Quick
      new_function_reads_old_global;
    Alcotest.test_case "new initializer reads retained global" `Quick
      initializer_reads_old_global;
    Alcotest.test_case "retained narrow scalar and array storage" `Quick
      narrow_storage;
    Alcotest.test_case "cumulative global allocation limit" `Quick
      cumulative_global_limit;
    Alcotest.test_case "cumulative ordinary output limit" `Quick
      cumulative_output;
    Alcotest.test_case "retained arrays are checked call arguments" `Quick
      retained_array_argument;
    Alcotest.test_case "retained bytes feed checked formatter" `Quick
      retained_array_output;
    Alcotest.test_case "preparation limit applies during compilation" `Quick
      compilation_budget;
    Alcotest.test_case "pending commands share preparation budget" `Quick
      shared_compilation_budget;
    Alcotest.test_case "failed preparation retains its work charge" `Quick
      failed_preparation_charges_work;
    Alcotest.test_case "configured preparation exceeds default" `Quick
      larger_preparation_limit;
    Alcotest.test_case "module wrapper cannot replay retained syntax" `Quick
      reconstructed_module_cannot_replay;
    Alcotest.test_case "item wrapper cannot replay retained syntax" `Quick
      retained_item_cannot_be_recompiled;
    Alcotest.test_case "statement wrapper cannot repeat a reached update" `Quick
      reconstructed_statement_cannot_replay;
    Alcotest.test_case "task cells are independent and retain unknown state"
      `Quick independent_tasks_and_unknown_cells;
    Alcotest.test_case "cumulative instruction exact and one below" `Quick
      cumulative_instruction_limit;
    Alcotest.test_case "cumulative literal capacity" `Quick
      cumulative_literal_limit;
    Alcotest.test_case "cumulative formatter work" `Quick cumulative_output_work;
    Alcotest.test_case "earlier function used by later commands and functions"
      `Quick retained_function;
    Alcotest.test_case "old function retains global owner across shadowing"
      `Quick retained_function_global_owner;
    Alcotest.test_case "old function retains static owner after allocations"
      `Quick retained_function_static_owner;
    Alcotest.test_case "old function retains mutated literal site" `Quick
      retained_function_literal_owner;
    Alcotest.test_case "pending call retains prior header after shadow" `Quick
      retained_function_pending_selection;
    Alcotest.test_case "old dependencies survive function shadowing" `Quick
      retained_function_dependencies;
    Alcotest.test_case "returns restore caller literal owner" `Quick
      retained_function_restores_literal_owner;
    Alcotest.test_case "retained recursive calls use original table" `Quick
      retained_function_recursion;
    Alcotest.test_case "retained calls initialize later globals and statics"
      `Quick retained_function_initializers;
    Alcotest.test_case "retained pointer local preserves object owner" `Quick
      retained_function_pointer_owner;
    Alcotest.test_case "retained providers keep checked signatures" `Quick
      retained_function_providers;
    Alcotest.test_case "invalid retained calls have no effects" `Quick
      retained_function_rejections;
    Alcotest.test_case "old literal image is charged only once" `Quick
      retained_function_literal_capacity;
    Alcotest.test_case
      "joined retained function preserves reached fault effects" `Quick
      retained_function_fault_and_join;
    Alcotest.test_case
      "functions publish on admission and survive reached faults" `Quick
      retained_function_publication_boundary;
    Alcotest.test_case "retained initializer callees preserve optimizer guards"
      `Quick retained_initializer_guard;
    Alcotest.test_case "retained recursive calls obey active depth bound" `Quick
      retained_function_depth_limit;
    Alcotest.test_case
      "earlier extern selection cannot acquire later executable" `Quick
      retained_unfinished_selection;
    Alcotest.test_case "separate commands join the current extern identity"
      `Quick retained_cross_command_join;
    Alcotest.test_case
      "completed joined definitions shadow and freeze earlier calls" `Quick
      retained_join_shadow;
    Alcotest.test_case "joined function supplies one runtime dimension" `Quick
      retained_join_dimension;
  ]
