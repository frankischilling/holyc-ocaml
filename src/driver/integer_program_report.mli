type t

val run :
  ?max_dimension_work:int ->
  ?max_initializer_steps:int ->
  ?max_global_bytes:int ->
  ?max_literal_bytes:int ->
  ?max_frame_bytes:int ->
  ?max_call_depth:int ->
  ?max_output_bytes:int ->
  ?max_output_work:int ->
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  source:Common.Source_file.t ->
  max_steps:int ->
  t

val outcome :
  t ->
  ( Ir.Integer_interpreter.t Integer_program.checked,
    Common.Diagnostic.t list )
  result

val output_bytes : t -> string
val output_work : t -> int
val dimension_work : t -> int
val progress : t -> Integer_task.progress option
val program : t -> Integer_unit.compiled option
val task_units : t -> Integer_unit.compiled list
