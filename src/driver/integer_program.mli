type 'a checked = { value : 'a; diagnostics : Common.Diagnostic.t list }
type compiled

val compile_task_ast :
  task_view:Ir.Integer_globals.task_view ->
  ?initializer_progress:(int -> unit) ->
  ?max_initializer_steps:int ->
  ?declaration_command:Task_declarations.command ->
  ?retained_function_source:
    (Ir.Retained_function.t ->
    Ir.Integer_interpreter.task_function_source option) ->
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  Frontend.Ast.module_ ->
  (compiled checked, Common.Diagnostic.t list) result

val compile_ast :
  ?max_initializer_steps:int ->
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  Frontend.Ast.module_ ->
  (compiled checked, Common.Diagnostic.t list) result
(** Lower already parsed syntax through the ordinary semantic and checked IR
    pipeline. Does not consume source or invoke preprocessing again. This is an
    independent compilation unit and does not provide persistent task linking.
*)

val compile :
  ?max_initializer_steps:int ->
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  source:Common.Source_file.t ->
  (compiled checked, Common.Diagnostic.t list) result

val entry : compiled -> Ir.X87_stack.t
val globals : compiled -> Ir.Integer_globals.t
val initialization : compiled -> Ir.Global_initialization.t
val initializer_preparation : compiled -> Integer_initializers.t
val functions : compiled -> Ir.Integer_interpreter.function_definition list
val runtime_calls : compiled -> Ir.Runtime_call_context.t

val human : compiled -> string
(** Render the entry, storage, initialization and named bodies. A distinct
    callable/definition pair adds a versioned function-binding component with
    both symbol IDs and the actual definition item index. *)

val lower :
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  source:Common.Source_file.t ->
  (Ir.X87_stack.t checked, Common.Diagnostic.t list) result

val run :
  ?max_initializer_steps:int ->
  ?max_global_bytes:int ->
  ?max_literal_bytes:int ->
  ?max_frame_bytes:int ->
  ?max_call_depth:int ->
  ?max_output_bytes:int ->
  ?max_output_work:int ->
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  source:Common.Source_file.t ->
  max_steps:int ->
  (Ir.Integer_interpreter.t checked, Common.Diagnostic.t list) result
