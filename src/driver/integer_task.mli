type t
type command

(** Incremental JIT execution with retained globals and functions. Calls
    preserve each body's original storage, literals and callees. [run] retains
    assigned declaration symbols across parsing and compilation; partial runtime
    publication and #exe integration remain separate work. *)

val create :
  ?max_steps:int ->
  ?max_initializer_steps:int ->
  ?max_global_bytes:int ->
  ?max_literal_bytes:int ->
  ?max_frame_bytes:int ->
  ?max_call_depth:int ->
  ?max_output_bytes:int ->
  ?max_output_work:int ->
  Session.t ->
  (t, string) result
(** Limits belong to the task. Preparation is charged during compilation,
    including reached failures; runtime instructions, allocations and ordinary
    output are cumulative across admitted commands. Frame bytes and call depth
    bound simultaneously active calls. *)

val output_bytes : t -> string
val output_work : t -> int
val executed_steps : t -> int
val initializer_steps : t -> int

val compile_ast :
  t -> Frontend.Ast.module_ -> (command, Common.Diagnostic.t list) result
(** Compile only this syntax through the ordinary pipeline. Reusing the same
    parsed command returns its existing receipt; overlapping command items are
    rejected. Pending commands retain earlier bindings across later shadow
    publications. Parser-aware predecessor authority remains separate work. *)

val execute :
  t -> command -> (Ir.Integer_interpreter.t, Common.Diagnostic.t list) result
(** Preflight before admitting storage or consuming the receipt. Success and
    reached faults consume it; earlier writes survive a reached fault. Foreign
    commands and replay report HCIRVM0026 without effects. *)

val run :
  t ->
  source:Common.Source_file.t ->
  (Ir.Integer_interpreter.t, Common.Diagnostic.t list) result
(** Parse the exact registered source with declaration observation, then compile
    using those assigned symbols in the task's shared module scope. Reached
    semantic publications survive parse errors; failed declarations have no
    runtime binding. [compile_ast] retains its callback-free collection path. *)
