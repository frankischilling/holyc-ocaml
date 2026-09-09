include Integer_unit

let ( let* ) = Result.bind

type compilation_report = Integer_source_execution.compilation_report

let compilation_outcome = Integer_source_execution.compilation_outcome

let compilation_dimension_work =
  Integer_source_execution.compilation_dimension_work

let compilation_progress = Integer_source_execution.compilation_progress
let compilation_task_units = Integer_source_execution.compilation_task_units

let compile_report ?max_dimension_work ?max_initializer_steps session ~config
    ~source =
  Integer_source_execution.compile_report ?max_dimension_work
    ?max_initializer_steps session ~config ~source

let compile ?max_dimension_work ?max_initializer_steps session ~config ~source =
  compile_report ?max_dimension_work ?max_initializer_steps session ~config
    ~source
  |> compilation_outcome

let lower session ~config ~source =
  let* compiled = compile session ~config ~source in
  match functions compiled.value with
  | []
    when Ir.Integer_globals.byte_size (globals compiled.value) = 0
         && not (has_entry_calls compiled.value) ->
      Ok { value = entry compiled.value; diagnostics = compiled.diagnostics }
  | _ ->
      Error
        (compiled.diagnostics
        @ [
            Integer_source.diagnostic
              ~span:(Integer_source.source_span source)
              "HCRUN0001"
              "named functions, calls and global storage require the \
               compiled-program API";
          ])

let run ?max_dimension_work ?max_initializer_steps ?max_global_bytes
    ?max_literal_bytes ?max_frame_bytes ?max_call_depth ?max_output_bytes
    ?max_output_work session ~config ~source ~max_steps =
  Integer_source_execution.run ?max_dimension_work ?max_initializer_steps
    ?max_global_bytes ?max_literal_bytes ?max_frame_bytes ?max_call_depth
    ?max_output_bytes ?max_output_work session ~config ~source ~max_steps
  |> Integer_source_execution.outcome
