val lower_native :
  context:Initializer_fragment_typing.context ->
  authority:Sema.Internal_binding_fragment.authority ->
  Ir.Internal_binding_fragment_destination.t ->
  (Ir.Internal_binding_fragment_program.t, Common.Diagnostic.t list) result

val prepare :
  context:Initializer_fragment_typing.context ->
  authority:Sema.Internal_binding_fragment.authority ->
  runtime:Ir.Integer_interpreter.task_state ->
  Ir.Internal_binding_fragment_destination.t ->
  ( Ir.Internal_binding_fragment_program.execution,
    Common.Diagnostic.t list )
  result
