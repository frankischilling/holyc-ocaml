open Holyc_lib
module Live = Test_live_initializer_execution
module Task = Integer_task
module VM = Ir_integer_interpreter

let replay () =
  let saved = ref [] in
  let on_observed task = function
    | Parser.Parameter_default_completed _ as event ->
        saved := event :: !saved;
        let before = Task.progress task in
        Alcotest.(check bool)
          "current default cannot execute twice" true
          (Result.is_error (Task.observe_initializer task event));
        Alcotest.(check bool)
          "replay leaves work and effects unchanged" true
          (before = Task.progress task)
    | _ -> ()
  in
  let parsed, task =
    Live.run_result ~on_observed {|I64 N=0;I64 F(I64 a=++N){return a;};F()+N;|}
  in
  ignore (Test_parser.expect_ast parsed);
  let foreign = Task.create (Task.frontend task) |> Live.checked in
  List.iter
    (fun event ->
      List.iter
        (fun task ->
          let before = Task.progress task in
          Alcotest.(check bool)
            "delayed and foreign default execution rejected" true
            (Result.is_error (Task.observe_initializer task event));
          Alcotest.(check bool)
            "rejected execution changes no progress" true
            (before = Task.progress task))
        [ task; foreign ])
    !saved

let missing () =
  let observe_runtime = function
    | Parser.Parameter_default_completed _ -> false
    | _ -> true
  in
  let parsed, _ =
    Live.run_result ~observe_runtime {|I64 F(I64 a=42){return a;};F(1);|}
  in
  Alcotest.(check bool)
    "header cannot skip original default evaluation" true
    (Parser.has_errors parsed)

let skipped_predecessor () =
  let observe_runtime = function
    | Parser.Parameter_default_completed receipt
      when receipt.default_parameter_index = 0 -> false
    | _ -> true
  in
  let parsed, task =
    Live.run_result ~observe_runtime
      {|I64 N=0;I64 F(I64 a=++N,I64 b=++N){return a+b;};|}
  in
  Alcotest.(check bool)
    "later default rejects its missing predecessor" true
    (Parser.has_errors parsed);
  Alcotest.(check int64)
    "later default runs no effects" 0L (Live.read task "N;")

let skipped_family () =
  let observe_runtime = function
    | Parser.Parameter_default_completed _ | Parser.Function_header_completed _
      -> false
    | _ -> true
  in
  let parsed, task =
    Live.run_result ~observe_runtime
      {|I64 N=0;I64 F(I64 a=++N){return a;};F(7);|}
  in
  Alcotest.(check bool)
    "command cannot skip its entire default family" true
    (Parser.has_errors parsed);
  Alcotest.(check int64)
    "skipped family has no reached effects" 0L (Live.read task "N;")

let capture () =
  let progress =
    Live.run
      {|I64 N=0;I64 Touch(){return ++N;};77;I64 F(I64 a=Touch()){return a;};|}
  in
  Alcotest.(check (option int64))
    "default result leaves last outer value intact" (Some 77L)
    (Option.map (fun word -> word.VM.bits) progress.final_value)

let failure () =
  let parsed, task =
    Live.run_result
      {|I64 N=0;I64 Z=0;I64 Fail(){N=42;return N/Z;};77;I64 F(I64 a=Fail(),I64 b=++N){return a;};|}
  in
  Live.expect_error "HCIRVM0009" parsed;
  Alcotest.(check (option int64))
    "fault preserves outer capture" (Some 77L)
    (Option.map
       (fun word -> word.VM.bits)
       (Task.progress task).runtime.final_value);
  Alcotest.(check int64)
    "fault preserves reached write and stops later default" 42L
    (Live.read task "N;")

let preparation_budget () =
  let text = {|77;I64 F(I64 a=20+22){return a;};F();|} in
  let parsed, task = Live.run_result text in
  ignore (Test_parser.expect_ast parsed);
  let steps = Task.initializer_steps task in
  let parsed, task = Live.run_result ~max_initializer_steps:steps text in
  ignore (Test_parser.expect_ast parsed);
  Alcotest.(check int)
    "exact preparation allowance succeeds" steps
    (Task.initializer_steps task);
  let parsed, task = Live.run_result ~max_initializer_steps:(steps - 1) text in
  Live.expect_error "HCIRVM0007" parsed;
  Alcotest.(check int)
    "failed default preparation remains charged" (steps - 1)
    (Task.initializer_steps task)

let runtime_budget () =
  let text =
    {|I64 Add(I64 a,I64 b){return a+b;};77;I64 F(I64 a=Add(20,22)){return a;};F();|}
  in
  let reached = ref 0 in
  let on_observed task = function
    | Parser.Parameter_default_completed _ ->
        reached := Task.executed_steps task
    | _ -> ()
  in
  let parsed, task = Live.run_result ~on_observed text in
  ignore (Test_parser.expect_ast parsed);
  let total = Task.executed_steps task in
  let parsed, _ = Live.run_result ~max_steps:total text in
  ignore (Test_parser.expect_ast parsed);
  let parsed, task = Live.run_result ~max_steps:(!reached - 1) text in
  Live.expect_error "HCIRVM0007" parsed;
  Alcotest.(check int)
    "default instructions share the retained task allowance" (!reached - 1)
    (Task.executed_steps task)

let string_storage_boundary () =
  let parsed, task =
    Live.run_result
      {|I64 N=0;I64 First(U8 *p){N=42;return p[0];};I64 F(I64 n=First("a")){return n;};|}
  in
  Live.expect_error "HCRUN0006" parsed;
  Alcotest.(check int64)
    "unprepared string-default ownership cannot run effects" 0L
    (Live.read task "N;")

let tests =
  [
    Alcotest.test_case "string-containing defaults require owned preparation"
      `Quick string_storage_boundary;
    Alcotest.test_case "later default requires prior runtime completion" `Quick
      skipped_predecessor;
    Alcotest.test_case "function admission requires its original defaults"
      `Quick skipped_family;
    Alcotest.test_case "current stale and foreign default replay" `Quick replay;
    Alcotest.test_case "header requires default runtime completion" `Quick
      missing;
    Alcotest.test_case "default return does not replace outer capture" `Quick
      capture;
    Alcotest.test_case "default faults preserve reached state" `Quick failure;
    Alcotest.test_case "constant defaults share preparation allowance" `Quick
      preparation_budget;
    Alcotest.test_case "scheduled defaults share execution allowance" `Quick
      runtime_budget;
    Alcotest.test_case "retained body calls materialize defaults" `Quick
      (Live.returns_42
         {|I64 F(I64 a=21){return a;};I64 G(){return F()+F();};G();|});
    Alcotest.test_case
      "default expressions can call earlier defaulted functions" `Quick
      (Live.returns_42
         {|I64 F(I64 a=21){return a;};I64 G(I64 a=F()+F()){return a;};G();|});
    Alcotest.test_case "narrow integer defaults narrow only at entry" `Quick
      (Live.returns_42 {|I64 F(U8 a=298){return a;};F();|});
    Alcotest.test_case "signed narrow defaults retain their declared class"
      `Quick
      (Live.returns_42 {|I64 F(I8 a=255){return a+43;};F();|});
    Alcotest.test_case "unsigned full-width defaults retain register bits"
      `Quick
      (Live.returns_42
         {|I64 F(U64 a=0xffffffffffffffff){return (a==0xffffffffffffffff)+41;};F();|});
    Alcotest.test_case "explicit arguments do not suppress declaration effects"
      `Quick
      (Live.returns_42 {|I64 N=0;I64 F(I64 a=++N){return a;};F(41)+N;|});
    Alcotest.test_case "default effects execute for prototypes" `Quick
      (Live.returns_42 {|I64 N=41;I64 F(I64 a=++N);N;|});
    Alcotest.test_case "distinct declarations retain distinct defaults" `Quick
      (Live.returns_42
         {|I64 N=20;I64 F(I64 a=N){return a;};N=22;I64 G(I64 a=N){return a;};N=0;F()+G();|});
  ]
