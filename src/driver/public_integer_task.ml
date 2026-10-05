type t = Integer_task.t
type command = Integer_task.command
type stream = Integer_task.stream

type progress = Integer_task.progress = private {
  runtime : Ir.Integer_interpreter.task_progress;
  dimension_work : int;
  switch_work : int;
}

let create ?compiler_positions ?max_switch_work ?switch_budget ?max_steps
    ?max_initializer_steps ?max_global_bytes ?max_literal_bytes ?max_frame_bytes
    ?max_call_depth ?max_output_bytes ?max_output_work ?max_generated_bytes
    ?max_stream_depth session =
  Integer_task.create ?compiler_positions ?max_switch_work ?switch_budget
    ?max_steps ?max_initializer_steps ?max_global_bytes ?max_literal_bytes
    ?max_frame_bytes ?max_call_depth ?max_output_bytes ?max_output_work
    ?max_generated_bytes ?max_stream_depth session

let adopt_source ?max_steps ?max_initializer_steps ?max_global_bytes
    ?max_literal_bytes ?max_frame_bytes ?max_call_depth ?max_output_bytes
    ?max_output_work ?max_generated_bytes ?max_stream_depth session ~source
    ~ledger =
  Integer_task.adopt_source ?max_steps ?max_initializer_steps ?max_global_bytes
    ?max_literal_bytes ?max_frame_bytes ?max_call_depth ?max_output_bytes
    ?max_output_work ?max_generated_bytes ?max_stream_depth session ~source
    ~ledger

let adopt_source_for_activation ?max_steps ?max_initializer_steps
    ?max_global_bytes ?max_literal_bytes ?max_frame_bytes ?max_call_depth
    ?max_output_bytes ?max_output_work ?max_generated_bytes ?max_stream_depth
    session ~source ~ledger =
  Integer_task.adopt_source_for_activation ?max_steps ?max_initializer_steps
    ?max_global_bytes ?max_literal_bytes ?max_frame_bytes ?max_call_depth
    ?max_output_bytes ?max_output_work ?max_generated_bytes ?max_stream_depth
    session ~source ~ledger

let observe_source_offset = Integer_task.observe_source_offset
let prepare_source_default = Integer_task.prepare_source_default
let activate_source = Integer_task.activate_source
let result = Integer_task.result
let progress = Integer_task.progress
let compiled_units = Integer_task.compiled_units
let compile_isolated = Integer_task.compile_isolated
let execute_isolated = Integer_task.execute_isolated
let frontend = Integer_task.frontend
let admit_global = Integer_task.admit_global
let prepare_initializer = Integer_task.prepare_initializer
let prepare_parameter_default = Integer_task.prepare_parameter_default

let prepare_initializer_destination =
  Integer_task.prepare_initializer_destination

let lower_initializer_fragment = Integer_task.lower_initializer_fragment
let observe_initializer = Integer_task.observe_initializer
let compile_source_ast = Integer_task.compile_source_ast
let output_bytes = Integer_task.output_bytes
let output_work = Integer_task.output_work
let generated_bytes = Integer_task.generated_bytes
let executed_steps = Integer_task.executed_steps
let switch_work = Integer_task.switch_work
let initializer_steps = Integer_task.initializer_steps
let dimension_work = Integer_task.dimension_work
let begin_stream = Integer_task.begin_stream
let finish_stream = Integer_task.finish_stream
let abort_stream = Integer_task.abort_stream
let compile_ast = Integer_task.compile_ast
let execute = Integer_task.execute
let run = Integer_task.run
let stream_executor = Integer_task.stream_executor
let run_suspended = Integer_task.run_suspended

let prepare_source_callback_default =
  Integer_task.prepare_source_callback_default
