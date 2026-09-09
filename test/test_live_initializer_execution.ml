open Holyc_lib
module D = Task_declarations
module Task = Integer_task
module VM = Ir_integer_interpreter

let checked = Test_declaration_collection.checked
let expect = Test_integer_program.checked

let run text =
  let session, source, ledger = Test_source_promotion.inputs text in
  let retained = ref None in
  let task () = Option.get !retained in
  let declaration event =
    Result.bind (D.observe ledger event) (fun () ->
        match event with
        | Parser.Global_declared publication ->
            Task.admit_global (task ()) publication
        | _ -> Ok ())
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
              Some (Task.adopt_source session ~source ~ledger |> checked);
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
  ignore (Test_parser.expect_ast parsed);
  (Task.progress (task ())).runtime

let returns_42 text () =
  let progress = run text in
  Alcotest.(check (option int64))
    "original live initializer reaches 42" (Some 42L)
    (Option.map (fun word -> word.VM.bits) progress.final_value)

let tests =
  [
    Alcotest.test_case "scalar before following directive" `Quick
      (returns_42 {|I64 N=40;#exe {StreamPrint("%d;",N+2);}|});
    Alcotest.test_case "array leaf before next leaf reads" `Quick
      (returns_42 {|I64 A[2]={40,#exe {StreamPrint("%d",A[0]+2);}};A[1];|});
    Alcotest.test_case "copied row before next leaf reads" `Quick
      (returns_42
         {|U8 A[2][3]={"AB",#exe {StreamPrint("\"%c\"",A[0][0]-23);}};A[1][0];|});
    Alcotest.test_case "retained function initializer before directive" `Quick
      (returns_42
         {|I64 Add(I64 a,I64 b){return a+b;};I64 N=Add(20,22);#exe {StreamPrint("%d;",N);}|});
  ]
