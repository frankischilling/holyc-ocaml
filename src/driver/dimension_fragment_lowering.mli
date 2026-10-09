val lower_native :
  context:Initializer_fragment_typing.context ->
  authority:Sema.Dimension_fragment.authority ->
  Ir.Dimension_fragment_destination.t ->
  (Ir.Dimension_fragment_program.t, Common.Diagnostic.t list) result

val prepare :
  context:Initializer_fragment_typing.context ->
  authority:Sema.Dimension_fragment.authority ->
  runtime:Ir.Integer_interpreter.task_state ->
  Ir.Dimension_fragment_destination.t ->
  (Ir.Dimension_fragment_program.execution, Common.Diagnostic.t list) result
