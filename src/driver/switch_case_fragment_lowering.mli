val lower_native :
  context:Initializer_fragment_typing.context ->
  authority:Sema.Switch_case_fragment.authority ->
  Ir.Switch_case_fragment_destination.t ->
  (Ir.Switch_case_fragment_program.t, Common.Diagnostic.t list) result

val prepare :
  context:Initializer_fragment_typing.context ->
  authority:Sema.Switch_case_fragment.authority ->
  runtime:Ir.Integer_interpreter.task_state ->
  attempt:Ir.Integer_interpreter.switch_case_attempt ->
  Ir.Switch_case_fragment_destination.t ->
  (Ir.Switch_case_fragment_program.execution, Common.Diagnostic.t list) result
