type 'a checked = { value : 'a; diagnostics : Common.Diagnostic.t list }
type compiled

val compile_source_output :
  source_command:Task_declarations.source_command ->
  max_initializer_steps:int ->
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  Frontend.Parser.output ->
  (compiled checked, Common.Diagnostic.t list) result
(** Internal ordinary-source compilation entry. The parser output and required
    source witness keep their original ownership and diagnostic order. Task
    compilation must use [compile_task_ast] and its runtime ownership checks.
    This function never reads source or creates a task. *)

val compile_task_ast :
  task:Ir.Integer_interpreter.task_state ->
  ?declaration_command:Task_declarations.command ->
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

val entry : compiled -> Ir.X87_stack.t
val globals : compiled -> Ir.Integer_globals.t
val initialization : compiled -> Ir.Global_initialization.t
val initializer_preparation : compiled -> Integer_initializers.t
val dimension_preparation_work : compiled -> int
val functions : compiled -> Ir.Integer_interpreter.function_definition list
val runtime_calls : compiled -> Ir.Runtime_call_context.t
val has_entry_calls : compiled -> bool
val human : compiled -> string
