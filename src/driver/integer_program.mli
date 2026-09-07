type 'a checked = { value : 'a; diagnostics : Common.Diagnostic.t list }

val lower :
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  source:Common.Source_file.t ->
  (Ir.X87_stack.t checked, Common.Diagnostic.t list) result

val run :
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  source:Common.Source_file.t ->
  max_steps:int ->
  (Ir.Integer_interpreter.t checked, Common.Diagnostic.t list) result
