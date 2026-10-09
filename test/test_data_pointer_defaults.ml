open Holyc_lib
module Cases = Data_pointer_default_cases
module Live = Test_live_initializer_execution
module VM = Ir_integer_interpreter

let cases fixtures () =
  List.iter
    (fun (source, expected) ->
      let session = Session.create () in
      let source_ =
        Session.add_source session ~path:"data-defaults.hc" ~contents:source
      in
      let config =
        Preprocessor.Config.create ~compilation_mode:Jit ()
        |> Test_live_initializer_execution.checked
      in
      let report =
        run_integer_program_report session ~source:source_ ~config
          ~max_steps:100_000
      in
      let result =
        integer_program_report_outcome report |> Test_integer_program.checked
      in
      Alcotest.(check (option int64))
        source (Some expected)
        (Option.map (fun word -> word.VM.bits) (VM.final_value result.value)))
    fixtures

let faults () =
  List.iter
    (fun (source, code) ->
      let parsed, _ = Live.run_result source in
      Live.expect_error code parsed)
    Cases.faults

let string_limits () =
  let source = "I64 F(U8 *p=\"AB\"){return *p;}F();" in
  let parsed, task = Live.run_result source in
  ignore (Test_parser.expect_ast parsed);
  let limit = Integer_task.initializer_steps task in
  let exact_literal, _ = Live.run_result ~max_literal_bytes:6 source in
  ignore (Test_parser.expect_ast exact_literal);
  let insufficient_literal, _ = Live.run_result ~max_literal_bytes:5 source in
  Live.expect_error "HCIRVM0011" insufficient_literal;
  let exact, _ = Live.run_result ~max_initializer_steps:limit source in
  ignore (Test_parser.expect_ast exact);
  let parsed, task =
    Live.run_result ~max_initializer_steps:(limit - 1) source
  in
  Live.expect_error "HCIRVM0007" parsed;
  Alcotest.(check int)
    "attempted copy work remains charged" (limit - 1)
    (Integer_task.initializer_steps task);
  let source =
    "U8 A[2];I64 N=0;I64 F(U8 *p=A+(\"x\"[0]-120+N++)){return *p;}"
  in
  let parsed, task = Live.run_result source in
  Live.expect_error "HCIRVM0012" parsed;
  Alcotest.(check int64)
    "copy fault retains earlier expression effects" 1L (Live.read task "N;")

let tests =
  [
    Alcotest.test_case "all original/view scalar read classes" `Quick
      (cases Cases.read_matrix);
    Alcotest.test_case "all original/view scalar write classes" `Quick
      (cases Cases.write_matrix);
    Alcotest.test_case "original objects, headers and activations" `Quick
      (cases Cases.ownership);
    Alcotest.test_case "copied strings, views and providers" `Quick
      (cases Cases.strings);
    Alcotest.test_case "reached bounds and unknown bytes" `Quick faults;
    Alcotest.test_case "copy work, limits and fault effects" `Quick
      string_limits;
  ]
