val prepare :
  context:Initializer_fragment_typing.context ->
  authority:Sema.Offset_fragment.authority ->
  runtime:Ir.Integer_interpreter.task_state ->
  Ir.Offset_fragment_destination.t ->
  (Ir.Offset_fragment_program.execution, Common.Diagnostic.t list) result
