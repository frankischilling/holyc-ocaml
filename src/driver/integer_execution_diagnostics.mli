val of_errors :
  span:Common.Span.t ->
  Ir.Integer_interpreter.error list ->
  Common.Diagnostic.t list

val validate_limits :
  span:Common.Span.t ->
  max_steps:int ->
  max_initializer_steps:int ->
  max_global_bytes:int ->
  max_literal_bytes:int ->
  max_frame_bytes:int ->
  max_call_depth:int ->
  max_output_bytes:int ->
  max_output_work:int ->
  (unit, Common.Diagnostic.t list) result
