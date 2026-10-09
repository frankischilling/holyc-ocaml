type t = Integer_source_execution.report

let run ?max_dimension_work ?max_switch_work ?max_initializer_steps
    ?max_global_bytes ?max_literal_bytes ?max_frame_bytes ?max_call_depth
    ?max_output_bytes ?max_output_work session ~config ~source ~max_steps =
  Integer_source_execution.run ?max_dimension_work ?max_switch_work
    ?max_initializer_steps ?max_global_bytes ?max_literal_bytes ?max_frame_bytes
    ?max_call_depth ?max_output_bytes ?max_output_work session ~config ~source
    ~max_steps

let outcome = Integer_source_execution.outcome
let output_bytes = Integer_source_execution.output_bytes
let output_work = Integer_source_execution.output_work
let dimension_work = Integer_source_execution.dimension_work
let switch_work = Integer_source_execution.switch_work
let preparation_work = Integer_source_execution.preparation_work
let progress = Integer_source_execution.progress
let program = Integer_source_execution.program
let task_units = Integer_source_execution.task_units
let compiler_exceptions = Integer_source_execution.compiler_exceptions
