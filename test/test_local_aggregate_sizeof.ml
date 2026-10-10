open Holyc_lib
module Cases = Local_aggregate_sizeof_cases
module VM = Ir_integer_interpreter

let modes = [ Preprocessor.Jit; Preprocessor.Aot ]

let run ?max_steps ?max_initializer_steps mode source =
  let session = Session.create () in
  let source =
    Session.add_source session ~path:"local-aggregate-sizeof.hc"
      ~contents:(Cases.headers ^ source)
  in
  let config =
    Preprocessor.Config.create ~compilation_mode:mode () |> Result.get_ok
  in
  run_integer_program_report ?max_initializer_steps
    ~max_steps:(Option.value ~default:100_000 max_steps)
    session ~source ~config

let describe errors =
  String.concat "; "
    (List.map
       (fun (diagnostic : Diagnostic.t) ->
         diagnostic.code ^ ": " ^ diagnostic.message)
       errors)

let expect (case : Cases.case) report =
  let result =
    match integer_program_report_outcome report with
    | Ok result -> result
    | Error errors -> Alcotest.fail (describe errors)
  in
  Alcotest.(check string)
    "original output" case.output
    (integer_program_report_output_bytes report);
  Alcotest.(check bool)
    "successful execution reaches stream end" true
    (VM.termination result.value = VM.Stream_end);
  Alcotest.(check (option int64))
    (case.label ^ " original sizeof word")
    (Some case.expected)
    (VM.final_value result.value
    |> Option.map (fun (word : VM.word) -> word.bits));
  Alcotest.(check bool)
    "function result has I64 class" true
    (match VM.final_value result.value with
    | Some word -> word.type_ = VM.I64
    | None -> false)

let value mode (case : Cases.case) () = expect case (run mode case.source)

let failure code report =
  match integer_program_report_outcome report with
  | Ok _ -> Alcotest.fail ("expected " ^ code)
  | Error errors ->
      Alcotest.(check bool)
        (describe errors) true
        (List.exists
           (fun (diagnostic : Diagnostic.t) -> diagnostic.code = code)
           errors)

let runtime_layout_effects_and_limits () =
  let case = Cases.runtime_layout in
  let baseline = run Preprocessor.Jit case.source in
  expect case baseline;
  let preparation =
    integer_program_report_preparation_work baseline |> Option.get
  in
  let steps =
    match integer_program_report_outcome baseline with
    | Error _ -> Alcotest.fail "successful source report lost its result"
    | Ok result -> VM.executed_steps result.value
  in
  Alcotest.(check bool) "runtime preparation is reached" true (preparation > 0);
  Alcotest.(check bool) "runtime program is reached" true (steps > 0);
  expect case
    (run ~max_steps:steps ~max_initializer_steps:preparation Preprocessor.Jit
       case.source);
  failure "HCIRVM0007"
    (run ~max_initializer_steps:(preparation - 1) Preprocessor.Jit case.source);
  failure "HCIRVM0007" (run ~max_steps:(steps - 1) Preprocessor.Jit case.source)

let () =
  Alcotest.run "Original local aggregate sizeof"
    [
      ( "IR",
        List.concat_map
          (fun mode ->
            List.map
              (fun (case : Cases.case) ->
                Alcotest.test_case
                  ((if mode = Preprocessor.Jit then "JIT " else "AOT ")
                  ^ case.label)
                  `Quick (value mode case))
              Cases.closed)
          modes
        @ List.map
            (fun (case : Cases.case) ->
              Alcotest.test_case ("JIT " ^ case.label) `Quick
                (value Preprocessor.Jit case))
            Cases.jit_only
        @ [
            Alcotest.test_case "JIT runtime layout and exact budgets" `Quick
              runtime_layout_effects_and_limits;
          ] );
    ]
