val prepare :
  context:Initializer_fragment_typing.context ->
  authority:Sema.Default_fragment.authority ->
  runtime:Ir.Integer_interpreter.task_state ->
  Ir.Default_fragment_destination.t ->
  (Ir.Default_fragment_program.execution, Common.Diagnostic.t list) result
