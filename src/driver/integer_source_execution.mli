type compilation_report

type compilation =
  | Isolated of Integer_unit.compiled
  | Stateful of Ir.Integer_interpreter.t

val compilation_result :
  compilation_report ->
  (compilation Integer_unit.checked, Common.Diagnostic.t list) result

type report

val compile_report :
  ?max_dimension_work:int ->
  ?max_initializer_steps:int ->
  ?max_steps:int ->
  ?max_global_bytes:int ->
  ?max_literal_bytes:int ->
  ?max_frame_bytes:int ->
  ?max_call_depth:int ->
  ?max_output_bytes:int ->
  ?max_output_work:int ->
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  source:Common.Source_file.t ->
  compilation_report

val compilation_outcome :
  compilation_report ->
  (Integer_unit.compiled Integer_unit.checked, Common.Diagnostic.t list) result

val compilation_dimension_work : compilation_report -> int
val compilation_progress : compilation_report -> Integer_task.progress option
val compilation_task_units : compilation_report -> Integer_unit.compiled list

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
  report

val outcome :
  report ->
  ( Ir.Integer_interpreter.t Integer_unit.checked,
    Common.Diagnostic.t list )
  result

val output_bytes : report -> string
val output_work : report -> int
val dimension_work : report -> int
val progress : report -> Integer_task.progress option
val program : report -> Integer_unit.compiled option
val task_units : report -> Integer_unit.compiled list
