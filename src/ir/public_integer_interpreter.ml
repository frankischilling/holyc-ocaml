include Integer_interpreter

let create_task_state ?max_steps ?max_initializer_steps ?max_global_bytes
    ?max_literal_bytes ?max_frame_bytes ?max_call_depth ?max_output_bytes
    ?max_output_work ?max_generated_bytes ?max_stream_depth ~table () =
  Integer_interpreter.create_task_state ?max_steps ?max_initializer_steps
    ?max_global_bytes ?max_literal_bytes ?max_frame_bytes ?max_call_depth
    ?max_output_bytes ?max_output_work ?max_generated_bytes ?max_stream_depth
    ~table ()
