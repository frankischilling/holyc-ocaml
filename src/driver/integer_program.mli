type 'a checked = { value : 'a; diagnostics : Common.Diagnostic.t list }
type compiled

val compile :
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  source:Common.Source_file.t ->
  (compiled checked, Common.Diagnostic.t list) result

val entry : compiled -> Ir.X87_stack.t
val functions : compiled -> Ir.Integer_interpreter.function_definition list
val human : compiled -> string

val lower :
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  source:Common.Source_file.t ->
  (Ir.X87_stack.t checked, Common.Diagnostic.t list) result

val run :
  ?max_frame_bytes:int ->
  ?max_call_depth:int ->
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  source:Common.Source_file.t ->
  max_steps:int ->
  (Ir.Integer_interpreter.t checked, Common.Diagnostic.t list) result
