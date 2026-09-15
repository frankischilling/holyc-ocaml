open Holyc_lib
module D = Task_declarations
module Task = Integer_task
module VM = Ir_integer_interpreter

let checked = Test_declaration_collection.checked
let expect = Test_integer_program.checked

let run_result ?max_steps ?max_initializer_steps ?max_global_bytes
    ?max_literal_bytes ?(observe_runtime = fun _ -> true)
    ?(on_observed = fun _ _ -> ()) text =
  let session, source, ledger = Test_source_promotion.inputs text in
  let retained = ref None in
  let task () = Option.get !retained in
  let declaration event =
    Result.bind (D.observe ledger event) (fun () ->
        if observe_runtime event then
          Task.observe_initializer (task ()) event
          |> Result.map (fun () -> on_observed (task ()) event)
        else Ok ())
  in
  let providers = ref false in
  let enter span =
    if not !providers then (
      let detached = Session.fork_frontend session in
      let headers = Test_integer_task.parse detached Test_task_stream.headers in
      ignore
        (Task.compile_ast (task ()) headers
        |> expect
        |> Task.execute (task ())
        |> expect);
      providers := true);
    Task.stream_executor (task ()) span
  in
  let checkpoint event =
    Result.bind (D.observe_command ledger event) (fun () ->
        match event with
        | Parser.Sequence_started _ when Option.is_none !retained ->
            retained :=
              Some
                (Task.adopt_source ?max_steps ?max_initializer_steps
                   ?max_global_bytes ?max_literal_bytes session ~source ~ledger
                |> checked);
            Ok ()
        | Parser.Command_resumed completed ->
            Result.bind
              (Task.compile_source_ast (task ()) completed.command_ast)
              (fun command ->
                Result.map ignore (Task.execute (task ()) command))
        | _ -> Ok ())
  in
  let parsed =
    Test_source_promotion.parse ~declaration ~checkpoint ~execute_stream:enter
      session source ledger
  in
  (parsed, task ())

let run text =
  let parsed, task = run_result text in
  ignore (Test_parser.expect_ast parsed);
  (Task.progress task).runtime

let returns_42 text () =
  let progress = run text in
  Alcotest.(check (option int64))
    "original live initializer reaches 42" (Some 42L)
    (Option.map (fun word -> word.VM.bits) progress.final_value)

let short_generated_copy () =
  let parsed, task =
    run_result
      {|U8 A[2][3]={"AB",#exe {StreamPrint("\"%c\"",A[0][0]-23);}};A[1][0];|}
  in
  Alcotest.(check bool)
    "generated copy cannot read beyond its owned terminator" true
    (List.exists
       (fun (diagnostic : Diagnostic.t) -> diagnostic.code = "HCRUN0006")
       parsed.diagnostics);
  Alcotest.(check int)
    "first row preparation remains charged" 5
    (Task.initializer_steps task)

let incomplete_runtime () =
  List.iter
    (fun skip_leaf ->
      let observe_runtime = function
        | Parser.Global_initializer_leaf_completed receipt when skip_leaf ->
            receipt.leaf_index = 0
        | Parser.Global_initializer_delimiter_completed receipt -> (
            match receipt.delimiter_value with
            | Parser.Initializer_close _ -> false
            | _ -> true)
        | Parser.Global_completed _ -> false
        | _ -> true
      in
      let parsed, _ = run_result ~observe_runtime {|I64 A[2]={40,2};A[0];|} in
      Alcotest.(check bool)
        "completion cannot replace missing live leaves or delimiters" true
        (Parser.has_errors parsed))
    [ false; true ]

let replay () =
  let saved = ref [] in
  let on_observed task event =
    match event with
    | Parser.Global_initializer_started _
    | Parser.Global_initializer_leaf_completed _
    | Parser.Global_initializer_delimiter_completed _ ->
        saved := event :: !saved;
        let before = Task.progress task in
        Alcotest.(check bool)
          "current original event cannot execute twice" true
          (Result.is_error (Task.observe_initializer task event));
        Alcotest.(check bool)
          "replay changes no progress" true
          (before = Task.progress task)
    | _ -> ()
  in
  let parsed, task = run_result ~on_observed {|I64 A[2]={40,2};A[0]+A[1];|} in
  ignore (Test_parser.expect_ast parsed);
  let foreign = Task.create (Task.frontend task) |> checked in
  List.iter
    (fun runtime ->
      let before = Task.progress runtime in
      List.iter
        (fun event ->
          Alcotest.(check bool)
            "stale and foreign events rejected" true
            (Result.is_error (Task.observe_initializer runtime event)))
        !saved;
      Alcotest.(check bool)
        "rejected callbacks leave task unchanged" true
        (before = Task.progress runtime))
    [ task; foreign ]

let expect_error code parsed =
  Alcotest.(check bool)
    ("reached " ^ code) true
    (List.exists
       (fun (d : Diagnostic.t) -> d.code = code)
       parsed.Parser.diagnostics)

let read task text =
  Test_integer_task.compile (Task.frontend task) task text
  |> Task.execute task |> expect |> VM.final_value |> Option.get
  |> fun word -> word.VM.bits

let failure_effects () =
  let parsed, task =
    run_result {|I64 N=0,Z=0;I64 Touch(){N=42;return 1/Z;};77;I64 A=Touch();|}
  in
  expect_error "HCIRVM0009" parsed;
  Alcotest.(check (option int64))
    "failed initializer preserves outer capture" (Some 77L)
    (Option.map
       (fun word -> word.VM.bits)
       (Task.progress task).runtime.final_value);
  Alcotest.(check int64)
    "write before initializer runtime failure survives" 42L (read task "N;");
  let parsed, task =
    run_result
      {|I64 N=0;I64 Touch(){N=42;return 40;};I64 A[2]={Touch(),Missing};|}
  in
  Alcotest.(check bool)
    "later leaf fails typing" true (Parser.has_errors parsed);
  Alcotest.(check int64)
    "earlier leaf store and call effects survive typing failure" 82L
    (read task "A[0]+N;")

let copy_budget () =
  let text = {|U8 A[2][3]={"AB","CD"};A[1][0]-25;|} in
  let visits = ref [] in
  let on_observed task = function
    | Parser.Global_initializer_leaf_completed _ ->
        visits := Task.initializer_steps task :: !visits
    | _ -> ()
  in
  let parsed, task =
    run_result ~max_initializer_steps:8 ~max_global_bytes:6 ~on_observed text
  in
  ignore (Test_parser.expect_ast parsed);
  Alcotest.(check (list int))
    "copy preparation charged at each original row" [ 5; 8 ] (List.rev !visits);
  Alcotest.(check int)
    "completion does not repeat copies or preparation" 8
    (Task.initializer_steps task);
  Alcotest.(check int)
    "completion does not duplicate allocation" 6
    (Task.progress task).runtime.global_bytes;
  let parsed, task = run_result ~max_initializer_steps:7 text in
  expect_error "HCIRVM0007" parsed;
  Alcotest.(check int64)
    "previous copy survives exhausted preparation" 65L (read task "A[0][0];")

let literal_owner () =
  let parsed, task =
    run_result ~max_literal_bytes:2
      {|I64 First(U8 *p){p[0]=42;return p[0];};I64 N=First("a");N;|}
  in
  ignore (Test_parser.expect_ast parsed);
  Alcotest.(check int)
    "original initializer literal charged once" 2
    (Task.progress task).runtime.literal_bytes;
  Alcotest.(check int64)
    "retained call mutates original initializer literal" 42L (read task "N;")

let optimizer_guards () =
  List.iter
    (fun body ->
      let parsed, task =
        run_result
          ("I64 N=0;I64 Touch(){N=42;" ^ body ^ "};I64 A[2]={40,Touch()};")
      in
      expect_error "HCRUN0006" parsed;
      Alcotest.(check int64)
        "transitive optimizer guard preserves the prior leaf" 40L
        (read task "A[0];");
      Alcotest.(check int64)
        "guard runs before the rejected call has effects" 0L (read task "N;"))
    [ "return 1<<2;"; "return N/2;" ]

let instruction_budget () =
  let text = {|I64 Add(I64 a,I64 b){return a+b;};77;I64 N=Add(20,22);N;|} in
  let leaf_steps = ref 0 in
  let on_observed task = function
    | Parser.Global_initializer_leaf_completed _ ->
        leaf_steps := Task.executed_steps task
    | _ -> ()
  in
  let parsed, task = run_result ~on_observed text in
  ignore (Test_parser.expect_ast parsed);
  let total = Task.executed_steps task in
  let parsed, task = run_result ~max_steps:total text in
  ignore (Test_parser.expect_ast parsed);
  Alcotest.(check int)
    "exact cumulative instruction allowance succeeds" total
    (Task.executed_steps task);
  let limit = !leaf_steps - 1 in
  let parsed, task = run_result ~max_steps:limit text in
  expect_error "HCIRVM0007" parsed;
  Alcotest.(check int)
    "reached initializer instructions stay charged" limit
    (Task.executed_steps task);
  Alcotest.(check (option int64))
    "failed initializer does not capture a value" (Some 77L)
    (Option.map
       (fun word -> word.VM.bits)
       (Task.progress task).runtime.final_value)

let tests =
  [
    Alcotest.test_case "default effects precede the next parameter directive"
      `Quick
      (returns_42
         {|I64 N=0;I64 Touch(){return ++N;};I64 F(I64 a=Touch(),I64 b=#exe {StreamPrint("%d",N+40);}){return a+b;};F();|});
    Alcotest.test_case "default values survive later writes and repeated calls"
      `Quick
      (returns_42
         {|I64 N=20;I64 Touch(){return ++N;};I64 F(I64 a=Touch()){return a;};N=0;F()+F();|});
    Alcotest.test_case "default preparation runs even without a call" `Quick
      (returns_42
         {|I64 N=41;I64 Touch(){return ++N;};I64 F(I64 a=Touch()){return a;};N;|});
    Alcotest.test_case "live calls retain transitive optimizer guards" `Quick
      optimizer_guards;
    Alcotest.test_case "live calls share cumulative instruction allowance"
      `Quick instruction_budget;
    Alcotest.test_case "current stale and foreign replay rejection" `Quick
      replay;
    Alcotest.test_case "reached failure preserves writes and capture" `Quick
      failure_effects;
    Alcotest.test_case "copied rows share exact preparation allowance" `Quick
      copy_budget;
    Alcotest.test_case "initializer calls retain literal storage" `Quick
      literal_owner;
    Alcotest.test_case "completed call effects execute once" `Quick
      (returns_42
         {|I64 N=0;I64 Bump(){return ++N;};I64 A[2]={Bump(),Bump()};N+40;|});
    Alcotest.test_case "nested calls retain omitted defaults" `Quick
      (returns_42
         {|I64 Seed(I64 n=21){return n;};I64 Sum(I64 a,I64 b){return a+b;};I64 N=Sum(Seed(),Seed());N;|});
    Alcotest.test_case "nested calls retain explicit arguments" `Quick
      (returns_42
         {|I64 Seed(I64 n){return n;};I64 Sum(I64 a,I64 b){return a+b;};I64 N=Sum(Seed(21),Seed(21));N;|});
    Alcotest.test_case "next original leaf reads earlier storage" `Quick
      (returns_42 {|I64 A[2]={40,A[0]+2};A[1];|});
    Alcotest.test_case "completion requires finished live runtime" `Quick
      incomplete_runtime;
    Alcotest.test_case "scalar before following directive" `Quick
      (returns_42 {|I64 N=40;#exe {StreamPrint("%d;",N+2);}|});
    Alcotest.test_case "array leaf before next leaf reads" `Quick
      (returns_42 {|I64 A[2]={40,#exe {StreamPrint("%d",A[0]+2);}};A[1];|});
    Alcotest.test_case "copied row before next leaf reads" `Quick
      (returns_42
         {|U8 A[2][3]={"AB",#exe {StreamPrint("\"%cB\"",A[0][0]-23);}};A[1][0];|});
    Alcotest.test_case "short generated copy preserves earlier preparation"
      `Quick short_generated_copy;
    Alcotest.test_case "retained function initializer before directive" `Quick
      (returns_42
         {|I64 Add(I64 a,I64 b){return a+b;};I64 N=Add(20,22);#exe {StreamPrint("%d;",N);}|});
  ]
