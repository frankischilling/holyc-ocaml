val lower_native :
  context:Initializer_fragment_typing.context ->
  authority:Sema.Member_metadata_fragment.authority ->
  Ir.Member_metadata_fragment_destination.t ->
  (Ir.Member_metadata_fragment_program.t, Common.Diagnostic.t list) result

val prepare :
  context:Initializer_fragment_typing.context ->
  authority:Sema.Member_metadata_fragment.authority ->
  runtime:Ir.Integer_interpreter.task_state ->
  Ir.Member_metadata_fragment_destination.t ->
  ( Ir.Member_metadata_fragment_program.execution,
    Common.Diagnostic.t list )
  result
