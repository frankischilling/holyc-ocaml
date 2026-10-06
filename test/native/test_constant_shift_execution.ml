open Holyc_lib
module F = Test_constant_shifts
module Native = X86_64_expression

let native_fields () =
  let fixture = F.fixture () in
  List.iter
    (fun projection ->
      let graph, _, count = F.projected_graph projection in
      let image = Test_native_expression.image graph in
      let program = F.program_image (F.program_graph graph) in
      let expected = F.observed fixture projection "case_id" in
      for run = 1 to 2 do
        let bits =
          match Native_execution.execute image with
          | Ok bits -> bits
          | Error message -> Alcotest.fail message
        in
        Alcotest.(check int64)
          (F.field projection "field" ^ " native run " ^ string_of_int run)
          expected bits;
        match Native_program_execution.execute ~max_steps:count program with
        | Ok
            (X86_64_program.Completed
               {
                 final_value = Some word;
                 executed_steps;
                 captured_callback = None;
               }) ->
            Alcotest.(check int64) "native program bits" expected word.bits;
            Alcotest.(check int)
              "native program exact work" count executed_steps
        | Ok _ -> Alcotest.fail "native program did not complete with its value"
        | Error message -> Alcotest.fail message
      done;
      match Native_program_execution.execute ~max_steps:(count - 1) program with
      | Ok (X86_64_program.Fault fault) ->
          Alcotest.(check bool)
            "one below program budget" true
            (fault.kind = X86_64_program.Step_limit_exceeded);
          Alcotest.(check int)
            "retained work before next instruction" (count - 1)
            fault.executed_steps
      | _ -> Alcotest.fail "one below program budget published a value")
    (F.projections fixture)

let () =
  Alcotest.run "constant shift native execution"
    [
      ( "native oracle projections",
        [ Alcotest.test_case "49 fields, twice" `Quick native_fields ] );
    ]
