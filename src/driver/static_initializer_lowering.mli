val lower :
  ?runtime:Ir.Integer_interpreter.task_state ->
  context:Initializer_fragment_typing.context ->
  Ir.Static_initializer_destination.t ->
  (Ir.Static_initializer_program.t, Common.Diagnostic.t list) result
