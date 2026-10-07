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

let tests =
  [
    Alcotest.test_case
      "original captures, storage, defaults and joined definitions" `Quick
      sources;
    Alcotest.test_case "reached provider signatures and declaration contract"
      `Quick signatures;
    Alcotest.test_case "provider effects and exact quotas" `Quick quotas;
    Alcotest.test_case "owned words and AOT boundary" `Quick owners;
  ]
