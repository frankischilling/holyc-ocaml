module VM = Ir.Integer_interpreter

type t = {
  outcome_ : (VM.t Integer_program.checked, Common.Diagnostic.t list) result;
  output_bytes_ : string;
  output_work_ : int;
}

let outcome report = report.outcome_
let output_bytes report = report.output_bytes_
let output_work report = report.output_work_

let run ?(max_initializer_steps = 100_000) ?(max_global_bytes = 1_048_576)
    ?(max_literal_bytes = 1_048_576) ?(max_frame_bytes = 1_048_576)
    ?(max_call_depth = 128) ?(max_output_bytes = 1_048_576)
    ?(max_output_work = 1_048_576) session ~config ~source ~max_steps =
  let empty diagnostics =
    { outcome_ = Error diagnostics; output_bytes_ = ""; output_work_ = 0 }
  in
  let span = Integer_source.source_span source in
  match
    Integer_execution_diagnostics.validate_limits ~span ~max_steps
      ~max_initializer_steps ~max_global_bytes ~max_literal_bytes
      ~max_frame_bytes ~max_call_depth ~max_output_bytes ~max_output_work
  with
  | Error diagnostics -> empty diagnostics
  | Ok () -> (
      match
        Integer_program.compile ~max_initializer_steps session ~config ~source
      with
      | Error diagnostics -> empty diagnostics
      | Ok compiled ->
          let program = compiled.value in
          let report =
            VM.execute_program_report
              ~runtime_calls:(Integer_program.runtime_calls program)
              ~globals:(Integer_program.globals program)
              ~initialization:(Integer_program.initialization program)
              ~max_global_bytes ~max_literal_bytes ~max_steps ~max_frame_bytes
              ~max_call_depth ~max_output_bytes ~max_output_work
              ~functions:(Integer_program.functions program)
              (Integer_program.entry program)
          in
          {
            outcome_ =
              VM.report_outcome report
              |> Result.map (fun value ->
                  { Integer_program.value; diagnostics = compiled.diagnostics })
              |> Result.map_error (fun errors ->
                  compiled.diagnostics
                  @ Integer_execution_diagnostics.of_errors ~span errors);
            output_bytes_ = VM.report_output_bytes report;
            output_work_ = VM.report_output_work report;
          })
