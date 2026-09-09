type 'a checked = 'a Integer_unit.checked = {
  value : 'a;
  diagnostics : Common.Diagnostic.t list;
}

type compiled = Integer_unit.compiled

val compile_task_ast :
  task:Ir.Integer_interpreter.task_state ->
  ?declaration_command:Task_declarations.command ->
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  Frontend.Ast.module_ ->
  (compiled checked, Common.Diagnostic.t list) result
(** Compile against an owning JIT task snapshot and charge its cumulative
    preparation budget, including reached work on failure. Foreign semantic
    tables and AOT mode are rejected before semantic collection. *)

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
  ?max_dimension_work:int ->
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
  ?max_dimension_work:int ->
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

val dimension_preparation_work : compiled -> int
(** Evaluated numeric node visits in original source dimensions, separately from
    initializer VM instructions. Ordinary source uses its own dimension-work
    allowance; runtime-bound commands share their owning task preparation limit.
*)

type compilation_report

val compile_report :
  ?max_dimension_work:int ->
  ?max_initializer_steps:int ->
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  source:Common.Source_file.t ->
  compilation_report

val compilation_outcome :
  compilation_report -> (compiled checked, Common.Diagnostic.t list) result

val compilation_dimension_work : compilation_report -> int
