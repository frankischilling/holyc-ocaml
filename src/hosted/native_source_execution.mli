type word = private { type_ : Backend.X86_64_program.word_type; bits : int64 }
type result = private { final_value : word option }
type 'a checked = { value : 'a; diagnostics : Common.Diagnostic.t list }
type fragment_kind = Initializer | Command

type image = private {
  status_abi : Backend.X86_64_program.status_abi;
  ir_instructions : int;
  code_bytes : int;
  global_bytes : int;
  global_arena_bytes : int;
  entry_stack_bytes : int;
  function_count : int;
}
(** Detached immutable metadata from the exact compiled fragment. This is not an
    executable image and retains no live request, task arena or source task. *)

type fragment = private {
  kind : fragment_kind;
  image : image;
  native_outcome : (Backend.X86_64_program.outcome, string) Stdlib.result option;
}

type report

val evaluate :
  ?max_ir_instructions:int ->
  ?max_code_bytes:int ->
  ?max_stack_bytes:int ->
  ?max_blocks:int ->
  ?max_initializer_steps:int ->
  ?max_default_bytes:int ->
  ?max_switch_work:int ->
  ?max_dimension_work:int ->
  ?max_global_bytes:int ->
  ?max_literal_bytes:int ->
  ?max_frame_bytes:int ->
  ?max_call_depth:int ->
  ?max_output_bytes:int ->
  ?max_output_work:int ->
  ?max_active_stack_bytes:int ->
  ?status_abi:Backend.X86_64_program.status_abi ->
  Driver.Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  source:Common.Source_file.t ->
  max_steps:int ->
  report
(** Execute original JIT scalar initializer leaves and resumed source commands
    through one shared native task arena. Each fragment retains its original
    checked source request and claims its live entry once. Earlier data and
    initialization flags remain in that arena when later fragments execute.
    Source admission retains declaration and command metadata without an IR
    storage copy. The report records actual native outcomes and cumulative
    native work separately from closed declaration preparation. Fragment
    observations retain detached image metadata after code and arena release.

    Code bytes and verified IR instructions are bounded cumulatively across
    emitted fragments. Runtime steps and output share one native allowance;
    frame, call-depth and active native stack limits apply to each activation.
    Unsupported native task declarations, functions, literals and AOT mode
    return diagnostics. There is no isolated program or interpreter fallback. *)

val outcome : report -> (result checked, Common.Diagnostic.t list) Stdlib.result
val fragments : report -> fragment list
val platform : report -> Runtime.Native_program_execution.platform
val executed_steps : report -> int
val preparation_steps : report -> int
val dimension_work : report -> int
val switch_work : report -> int
val output_bytes : report -> string
val output_work : report -> int

val source_progress : report -> Driver.Integer_task.progress option
(** Original source admission and preparation progress. Its interpreter runtime
    counters do not stand in for native execution; use [executed_steps]. *)
