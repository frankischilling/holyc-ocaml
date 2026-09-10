type t
type command
type stream

val prepare_source_default :
  t ->
  session:Session.t ->
  ledger:Task_declarations.t ->
  Frontend.Parser.completed_parameter_default ->
  (unit, Common.Diagnostic.t list) result

val activate_source :
  t -> span:Common.Span.t -> (unit, Common.Diagnostic.t list) result

val result :
  t ->
  sequence:Frontend.Parser.completed_sequence ->
  (Ir.Integer_interpreter.t, Common.Diagnostic.t list) result

type progress = private {
  runtime : Ir.Integer_interpreter.task_progress;
  dimension_work : int;
}

val progress : t -> progress
(** Immutable cumulative task observation, including dimension preparation and
    effects reached before a later parse, compile or execution failure. This is
    neither a successful program outcome nor an executable artifact. *)

val compiled_units : t -> Integer_unit.compiled list
(** Immutable collection in compilation order, including checked units whose
    later execution failed. The collection is not one isolated program. *)

val compile_isolated :
  t ->
  source_command:Task_declarations.source_command ->
  Session.t ->
  config:Frontend.Preprocessor.Config.t ->
  Frontend.Parser.output ->
  (Integer_unit.compiled Integer_unit.checked, Common.Diagnostic.t list) result

val execute_isolated :
  t ->
  Integer_unit.compiled ->
  (Ir.Integer_interpreter.t, Ir.Integer_interpreter.error list) result
(** Isolated output compilation/execution shares remaining invocation allowances
    without importing or publishing retained task bindings. *)

(** Incremental JIT execution with retained globals and functions. Calls
    preserve each body's original storage, literals and callees. [run] retains
    assigned declaration symbols across parsing and compilation. The synchronous
    initializer adapter publishes partial storage and executes original leaves;
    the public source driver activates and resumes outer JIT commands. *)

val create :
  ?max_steps:int ->
  ?max_initializer_steps:int ->
  ?max_global_bytes:int ->
  ?max_literal_bytes:int ->
  ?max_frame_bytes:int ->
  ?max_call_depth:int ->
  ?max_output_bytes:int ->
  ?max_output_work:int ->
  ?max_generated_bytes:int ->
  ?max_stream_depth:int ->
  Session.t ->
  (t, string) result
(** Limits belong to the task. Preparation is charged during compilation,
    including reached failures; runtime instructions, allocations and ordinary
    output are cumulative across admitted commands. Frame bytes and call depth
    bound simultaneously active calls. *)

val frontend : t -> Session.t
(** The task's persistent frontend view, also usable for callback-free parsing.
    Sources and semantic table are shared with the caller session; declarations,
    definitions and local contexts have this task's visibility owner. *)

val admit_global :
  t ->
  Frontend.Parser.global_publication ->
  (unit, Common.Diagnostic.t list) result
(** Admit one original observed integer global while its parser context is live.
    Checked dimensions, namespace and predecessor evidence authorize unknown
    storage before initialization. Completion reuses that same object. This
    operation neither admits a command nor executes initializer leaves. *)

val prepare_initializer :
  t ->
  Frontend.Parser.completed_initializer_leaf ->
  ( Sema.Function_call_expression_result.top_level_t,
    Common.Diagnostic.t list )
  result
(** Type one current observed initializer leaf against its original admitted
    storage and selected retained bindings. This performs no runtime effects. *)

val prepare_parameter_default :
  t ->
  Frontend.Parser.completed_parameter_default ->
  ( Sema.Function_call_expression_result.top_level_t,
    Common.Diagnostic.t list )
  result
(** Type the original current default through a distinct expression root without
    allocating declarations, evaluating it or materializing an argument. *)

val prepare_initializer_destination :
  t ->
  destination:Ir.Integer_initializer_layout.entry ->
  Frontend.Parser.completed_initializer_leaf ->
  (Ir.Initializer_fragment_destination.t, Common.Diagnostic.t list) result
(** Bind an original live layout entry to its typed fragment and exact retained
    object without allocating storage or authorizing execution. *)

val lower_initializer_fragment :
  t ->
  destination:Ir.Integer_initializer_layout.entry ->
  Frontend.Parser.completed_initializer_leaf ->
  (Ir.Initializer_fragment_program.t, Common.Diagnostic.t list) result
(** Lower one current original fragment into an internally sealed program.
    Numeric preparation, runtime admission and execution remain separate. *)

val observe_initializer :
  t ->
  Frontend.Parser.declaration_event ->
  (unit, Common.Diagnostic.t list) result
(** After the source ledger observes the original event, admit declared storage,
    consume initializer boundaries, and prepare/execute each original leaf and
    integer expression default once. Global completion reuses successful stores;
    header completion binds saved defaults to their original parameters. *)

val adopt_source :
  ?max_steps:int ->
  ?max_initializer_steps:int ->
  ?max_global_bytes:int ->
  ?max_literal_bytes:int ->
  ?max_frame_bytes:int ->
  ?max_call_depth:int ->
  ?max_output_bytes:int ->
  ?max_output_work:int ->
  ?max_generated_bytes:int ->
  ?max_stream_depth:int ->
  Session.t ->
  source:Common.Source_file.t ->
  ledger:Task_declarations.t ->
  (t, string) result
(** Adopt the exact frontend and active source ledger without taking another
    task view. Requires its original unsealed JIT input. Earlier source
    dimensions enter this task's preparation allowance once; failed adoption
    leaves the ledger unchanged. No source commands execute during adoption. *)

val adopt_source_for_activation :
  ?max_steps:int ->
  ?max_initializer_steps:int ->
  ?max_global_bytes:int ->
  ?max_literal_bytes:int ->
  ?max_frame_bytes:int ->
  ?max_call_depth:int ->
  ?max_output_bytes:int ->
  ?max_output_work:int ->
  ?max_generated_bytes:int ->
  ?max_stream_depth:int ->
  Session.t ->
  source:Common.Source_file.t ->
  ledger:Task_declarations.t ->
  (t, string) result
(** Prepare original source activation with deferred dimension charges. The
    complete checked journal is bound before any source effects. *)

val compile_source_ast :
  t -> Frontend.Ast.module_ -> (command, Common.Diagnostic.t list) result
(** Compile this ledger's exact completed parser command or accepted sequence.
    Its original resume/predecessor evidence still controls runtime admission;
    incomplete or reconstructed syntax cannot obtain a command. *)

val output_bytes : t -> string
val output_work : t -> int
val generated_bytes : t -> int
val executed_steps : t -> int

val initializer_steps : t -> int
(** Cumulative task preparation, including numeric dimension visits. *)

val dimension_work : t -> int
(** Numeric dimension visits, also included in the shared task preparation
    tally. *)

val begin_stream : t -> (stream, string) result
val finish_stream : t -> stream -> (string, string) result

val abort_stream : t -> stream -> (unit, string) result
(** Opaque LIFO generation buffers for the parser executor. StreamPrint needs a
    checked extern header and an active buffer. It shares ordinary formatting
    work; generated bytes have a separate cumulative bound (default 16 MiB).
    Active depth defaults to 64. Finish returns that buffer's text; abort
    returns none. Reached work and generated bytes remain charged on both paths.
*)

val compile_ast :
  t -> Frontend.Ast.module_ -> (command, Common.Diagnostic.t list) result
(** Compile only this syntax through the ordinary pipeline. Reusing the same
    parsed command returns its existing receipt; overlapping command items are
    rejected. Pending commands retain earlier bindings across later shadow
    publications. This callback-free interface retains its separate admission
    contract; parser-aware source commands carry resume-order authority. *)

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

val stream_executor :
  t ->
  Common.Span.t ->
  (Frontend.Parser.stream_execution, Common.Diagnostic.t list) result
(** Retained task adapter for [Parser.parse ~execute_stream]. It executes each
    stream command at its original resume boundary and returns the accepted
    block's generated buffer. Earlier ordinary effects and resource charges
    survive faults; abort injects no partial buffer.

    The task must already own checked provider declarations. Its ledger observes
    only stream commands; an unobserved outer parser must use a distinct
    frontend environment. Within the stream, original initializer leaves finish
    before later leaves and reuse their retained storage at command completion.
    This does not execute the outer unit or provide a whole-invocation report.
*)
