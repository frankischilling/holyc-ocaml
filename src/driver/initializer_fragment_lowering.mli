val lower :
  context:Initializer_fragment_typing.context ->
  authority:Sema.Initializer_fragment.authority ->
  Ir.Initializer_fragment_destination.t ->
  (Ir.Initializer_fragment_program.t, Common.Diagnostic.t list) result
(** Lower original scalar stores through the shared expression/call path and
    seal their matching initialization and call contexts. No runtime effects. *)

val prepare :
  context:Initializer_fragment_typing.context ->
  authority:Sema.Initializer_fragment.authority ->
  runtime:Ir.Integer_interpreter.task_state ->
  Ir.Initializer_fragment_destination.t ->
  (Ir.Initializer_fragment_program.execution, Common.Diagnostic.t list) result
