open Holyc_lib
module Output = Test_integer_output
module VM = Ir_integer_interpreter
module Cases = Provider_callback_cases

let sources () =
  List.iter
    (fun (_, text, output) -> ignore (Output.run text |> Output.expect output))
    Cases.cases

let signatures () =
  ignore (Output.run Cases.self_placeholder |> Output.fault "HCIRVM0030");
  List.iter
    (fun (_, text) ->
      ignore (Output.run text |> Output.fault ~output:"M" "HCIRVM0014"))
    Cases.mismatches;
  ignore
    (Output.run "extern U0 PutChars(I64 ch);U0 (*p)(I64 ch)=&PutChars;p('A');"
    |> Output.fault "HCIRVM0030")

let quotas () =
  let text = Cases.header ^ "U0 (*p)(U64 ch)=&PutChars;p('AB');42;" in
  let result = Output.run text |> Output.expect "AB" in
  let steps = VM.executed_steps result in
  ignore
    (Output.run ~max_steps:steps ~max_output_bytes:2 ~max_output_work:4 text
    |> Output.expect "AB");
  ignore
    (Output.run ~max_output_bytes:1 text
    |> Output.fault ~output:"A" "HCIRVM0022");
  ignore
    (Output.run ~max_output_work:3 text |> Output.fault ~output:"A" "HCIRVM0023");
  ignore
    (Output.run ~max_steps:(steps - 1) text
    |> Output.fault ~output:"AB" "HCIRVM0007");
  let nested =
    Cases.header ^ "I64 Run(U0 (*p)(U64 ch)){p('A');return 42;}Run(&PutChars);"
  in
  ignore (Output.run ~max_call_depth:1 nested |> Output.fault "HCIRVM0015");
  ignore (Output.run ~max_frame_bytes:15 nested |> Output.fault "HCIRVM0011")

let owners () =
  let text = Cases.header ^ "U0 (*p)(U64 ch)=&PutChars;PutChars('B');p+0;" in
  ignore (Output.run text |> Output.fault ~output:"B" "HCIRVM0024");
  let report = Output.run (Cases.header ^ "U0 (*p)(U64 ch)=&PutChars;p;") in
  ignore (report |> Output.expect ~value:None "");
  let report =
    Output.run ~mode:Preprocessor.Aot
      (Cases.header ^ "U0 (*p)(U64 ch)=&PutChars;p('A');42;")
  in
  let diagnostics =
    Test_integer_functions.first_error (integer_program_report_outcome report)
  in
  Alcotest.(check string)
    "AOT extern address remains rejected" "HCSEMA0046" diagnostics.code

let print_sources () =
  List.iter
    (fun (_, text, output) -> ignore (Output.run text |> Output.expect output))
    Cases.print_cases

let print_guards () =
  List.iter
    (fun (_, text) ->
      ignore (Output.run text |> Output.fault ~output:"M" "HCIRVM0014"))
    Cases.print_mismatches;
  List.iter
    (fun (_, text, code) ->
      ignore (Output.run text |> Output.fault ~output:"B" code))
    Cases.print_faults

let print_formats () =
  List.iter
    (fun (case : Integer_format_fixture.t) ->
      let report =
        Output.run
          (Cases.print_header ^ Cases.print_capture
          ^ Integer_format_fixture.call "p" case
          ^ "42;")
      in
      ignore (report |> Output.expect case.bytes);
      Alcotest.(check int)
        case.label case.work
        (integer_program_report_output_work report))
    Integer_format_fixture.all

let print_quotas () =
  let text = Cases.print_header ^ Cases.print_capture ^ "p(\"AB\");42;" in
  let result = Output.run text |> Output.expect "AB" in
  ignore
    (Output.run ~max_steps:(VM.executed_steps result) ~max_output_bytes:2
       ~max_output_work:5 text
    |> Output.expect "AB");
  ignore (Output.run ~max_output_bytes:1 text |> Output.fault "HCIRVM0022");
  ignore (Output.run ~max_output_work:4 text |> Output.fault "HCIRVM0023");
  ignore (Output.run ~max_frame_bytes:15 text |> Output.fault "HCIRVM0011");
  let nested =
    Cases.print_header
    ^ "I64 Run(U0 (*p)(U8 *fmt,...)){p(\"A%d\",42);return 42;}Run(&Print);"
  in
  ignore (Output.run ~max_call_depth:1 nested |> Output.fault "HCIRVM0015")

let stream_contexts () =
  List.iter
    (fun (header, capture) ->
      let session = Session.create () in
      let task = Test_integer_task.create session in
      let outcome =
        Test_integer_task.run session task (header ^ capture ^ "p(\"42;\");")
      in
      let fault = Test_integer_functions.first_error outcome in
      Alcotest.(check string)
        "formatting precedes inactive context" "HCIRVM0027" fault.code;
      Alcotest.(check int)
        "original format work" 7
        (Integer_task.output_work task);
      Alcotest.(check string)
        "no ordinary stream capture" ""
        (Integer_task.output_bytes task))
    [
      ( "extern U0 StreamPrint(U8 *fmt,...);",
        "U0 (*p)(U8 *fmt,...)=&StreamPrint;" );
      ( "extern I64 StreamExePrint(U8 *fmt,...);",
        "I64 (*p)(U8 *fmt,...)=&StreamExePrint;" );
    ]

let installed_provider_signatures () =
  List.iter
    (fun mode ->
      ignore
        (Output.run ~mode
           {|#exe {U0 (*p)(U8 *fmt,...)=&Print;p("A%d",42);StreamPrint("42;");}|}
        |> Output.expect "A42");
      ignore
        (Output.run ~mode
           {|#exe {U0 (*p)(U64 ch)=&PutChars;p('A');StreamPrint("42;");}|}
        |> Output.expect "A");
      ignore
        (Output.run ~mode {|#exe {U0 (*p)(I64 ch)=&PutChars;p(42);}|}
        |> Output.fault "HCIRVM0014");
      ignore
        (Output.run ~mode {|#exe {I64 (*p)(U8 *fmt,...)=&Print;p("A");}|}
        |> Output.fault "HCIRVM0014"))
    Test_integer_globals.modes

let stream_generation () =
  List.iter
    (fun mode ->
      List.iter
        (fun source -> ignore (Output.run ~mode source |> Output.expect ""))
        [
          {|#exe {U0 (*p)(U8 *fmt,...)=&StreamPrint;p("%s;","42");}|};
          {|#exe {U0 (*p)(U8 *fmt,...)=&StreamPrint;}#exe {p("42;");}|};
          {|#exe {U0 Run(U0 (*p)(U8 *fmt,...)=&StreamPrint){p("42;");}Run();}|};
          {|#exe {U0 (*p)(U8 *fmt,...)[2]={&StreamPrint,0};p[1]=p[0];p[1]("42;");}|};
          {|#exe {U0 (*p)(U8 *fmt,...)=&StreamPrint;U0 StreamPrint(U8 *fmt,...){Print("A");}p("42;");}|};
        ])
    Test_integer_globals.modes;
  let session = Session.create () in
  let task =
    Integer_task.create ~max_generated_bytes:3 session |> Result.get_ok
  in
  let run text =
    Test_integer_task.run session task text
    |> Test_integer_program.checked |> ignore
  in
  run {|extern U0 StreamPrint(U8 *fmt,...);U0 (*p)(U8 *fmt,...)=&StreamPrint;|};
  let outer = Integer_task.begin_stream task |> Result.get_ok in
  run {|p("4");|};
  let inner = Integer_task.begin_stream task |> Result.get_ok in
  run {|p("2");|};
  Alcotest.(check string)
    "callback uses active inner buffer" "2"
    (Integer_task.finish_stream task inner |> Result.get_ok);
  run {|p(";");|};
  Alcotest.(check string)
    "callback resumes original outer buffer" "4;"
    (Integer_task.finish_stream task outer |> Result.get_ok);
  Alcotest.(check string)
    "stream bytes remain separate" ""
    (Integer_task.output_bytes task);
  let stream = Integer_task.begin_stream task |> Result.get_ok in
  let failed = Test_integer_task.run session task {|p("X");|} in
  Alcotest.(check string)
    "cumulative generated limit" "HCIRVM0028"
    (Test_integer_functions.first_error failed).code;
  Alcotest.(check string)
    "failed stream draft remains unpublished" ""
    (Integer_task.finish_stream task stream |> Result.get_ok)

let stream_execution () =
  let sources =
    [
      {|#exe {I64 (*p)(U8 *fmt,...)=&StreamExePrint;I64 N=p("40+2;");StreamPrint("%d;",N);}|};
      {|#exe {I64 (*p)(U8 *fmt,...)=&StreamExePrint;}#exe {I64 N=p("40+2;");StreamPrint("%d;",N);}|};
      {|#exe {I64 Run(I64 (*p)(U8 *fmt,...)=&StreamExePrint){return p("40+2;");}StreamPrint("%d;",Run());}|};
      {|#exe {I64 (*p)(U8 *fmt,...)=&StreamExePrint;I64 N=p("p(\"40+2;\");");StreamPrint("%d;",N);}|};
    ]
  in
  List.iter
    (fun source ->
      ignore (Output.run ~mode:Preprocessor.Aot source |> Output.expect "");
      let rejected = Output.run ~mode:Preprocessor.Jit source in
      ignore (rejected |> Output.fault "HCIRVM0027");
      Alcotest.(check bool)
        "JIT context check follows formatting" true
        (integer_program_report_output_work rejected > 0))
    sources;
  let source = List.hd sources in
  let measured = Output.run ~mode:Preprocessor.Aot source in
  let progress = Option.get (integer_program_report_progress measured) in
  let steps = progress.runtime.executed_steps in
  let work = integer_program_report_output_work measured in
  ignore
    (Output.run ~mode:Preprocessor.Aot ~max_steps:steps ~max_output_work:work
       source
    |> Output.expect "");
  ignore
    (Output.run ~mode:Preprocessor.Aot ~max_steps:(steps - 1) source
    |> Output.fault "HCIRVM0007");
  ignore
    (Output.run ~mode:Preprocessor.Aot ~max_output_work:(work - 1) source
    |> Output.fault "HCIRVM0023");
  ignore
    (Output.run ~mode:Preprocessor.Aot
       {|#exe {I64 (*p)(U8 *fmt,...)=&StreamExePrint;p("Print(\"A\");1/0;");}|}
    |> Output.fault ~output:"A" "HCIRVM0009");
  ignore
    (Output.run ~mode:Preprocessor.Aot
       {|#exe {I64 (*p)(U8 *fmt,...)=&StreamExePrint;p("%d");}|}
    |> Output.fault "HCIRVM0025")

let tests =
  [
    Alcotest.test_case
      "original captures, storage, defaults and joined definitions" `Quick
      sources;
    Alcotest.test_case "reached provider signatures and declaration contract"
      `Quick signatures;
    Alcotest.test_case "provider effects and exact quotas" `Quick quotas;
    Alcotest.test_case "owned words and AOT boundary" `Quick owners;
    Alcotest.test_case
      "Print captures, pointer tails, defaults and replacements" `Quick
      print_sources;
    Alcotest.test_case "Print signatures, argument kinds and atomic drafts"
      `Quick print_guards;
    Alcotest.test_case "Print callback shared format grammar and exact work"
      `Quick print_formats;
    Alcotest.test_case "Print exact output, work, frame and depth limits" `Quick
      print_quotas;
    Alcotest.test_case "stream entries retain inactive-context phases" `Quick
      stream_contexts;
    Alcotest.test_case "installed provider primitive ABI classes" `Quick
      installed_provider_signatures;
    Alcotest.test_case
      "stream callback generation, history and buffer ownership" `Quick
      stream_generation;
    Alcotest.test_case "stream callback AOT execution and nested limits" `Quick
      stream_execution;
  ]
