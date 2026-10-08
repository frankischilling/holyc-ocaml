type t
type command
type stream
type saved_compiler

type native_source_callback =
  Ir.Native_source_suspension.t ->
  Ir.Native_source_suspension.request ->
  ((int64, Common.Diagnostic.t list) result, string) result
(** Original parser handler of an entered native request. The outer error is a
    request or physical suspension rejection; the inner result retains child
    source diagnostics. The C scope reserves the caller's frame and depth but
    does not grant machine, arena or budget entry. *)

module Native_dispatch : sig
  type word = I64 of int64 | U64 of int64
  type capture = Unchanged | Captured of word option
  type initializer_request
  type command_request

  type t = {
    execute_initializer :
      initializer_request -> (unit, Common.Diagnostic.t list) result;
    execute_command :
      command_request -> (capture, Common.Diagnostic.t list) result;
  }

  val initializer_generation :
    initializer_request -> Ir.Integer_interpreter.native_generation

  val command_generation :
    command_request -> Ir.Integer_interpreter.native_generation

  val initializer_source_callback :
    initializer_request -> native_source_callback option

  val command_source_callback : command_request -> native_source_callback option

  val initializer_program :
    initializer_request -> Ir.Initializer_fragment_program.t

  val command_program : command_request -> Integer_unit.compiled
  val check_initializer_request : initializer_request -> (unit, string) result
  val claim_initializer_request : initializer_request -> (unit, string) result

  val initializer_function_source :
    initializer_request ->
    Ir.Retained_function.t ->
    (Ir.Integer_interpreter.task_function_source, string) result

  val check_command_request : command_request -> (unit, string) result

  val initializer_slot_binding :
    initializer_request ->
    runtime_calls:Ir.Runtime_call_context.t ->
    owner:Ir.Runtime_call_context.owner ->
    Ir.Runtime_call_context.call ->
    (Ir.Integer_interpreter.native_slot_binding, string) result

  val initializer_slot_address_binding :
    initializer_request ->
    runtime_calls:Ir.Runtime_call_context.t ->
    owner:Ir.Runtime_call_context.owner ->
    Ir.Runtime_call_context.function_slot_address ->
    (Ir.Integer_interpreter.native_slot_address_binding, string) result

  val initializer_slot_address_refresh :
    initializer_request ->
    Ir.Integer_interpreter.native_slot_address_binding ->
    (Ir.Integer_interpreter.native_slot_address_binding, string) result

  val initializer_provider_available :
    initializer_request ->
    runtime_calls:Ir.Runtime_call_context.t ->
    owner:Ir.Runtime_call_context.owner ->
    Ir.Runtime_call_context.call ->
    (bool, string) result

  val command_slot_binding :
    command_request ->
    runtime_calls:Ir.Runtime_call_context.t ->
    owner:Ir.Runtime_call_context.owner ->
    Ir.Runtime_call_context.call ->
    (Ir.Integer_interpreter.native_slot_binding, string) result

  val command_slot_address_binding :
    command_request ->
    runtime_calls:Ir.Runtime_call_context.t ->
    owner:Ir.Runtime_call_context.owner ->
    Ir.Runtime_call_context.function_slot_address ->
    (Ir.Integer_interpreter.native_slot_address_binding, string) result

  val command_slot_address_refresh :
    command_request ->
    Ir.Integer_interpreter.native_slot_address_binding ->
    (Ir.Integer_interpreter.native_slot_address_binding, string) result

  val command_provider_available :
    command_request ->
    runtime_calls:Ir.Runtime_call_context.t ->
    owner:Ir.Runtime_call_context.owner ->
    Ir.Runtime_call_context.call ->
    (bool, string) result
  (** Inspect an original admitted Print/PutChars call while its source request
      is still offered. Foreign contexts, domains and entered or expired
      requests reject. A joined source body disables provider fallback. *)

  val initializer_parameter_default :
    initializer_request ->
    globals:Ir.Integer_globals.t ->
    header:Sema.Function_type_resolution.resolved_function ->
    parameter:Sema.Function_type_resolution.parameter ->
    Ir.Prepared_parameter_default.t ->
    (unit, string) result

  val command_parameter_default :
    command_request ->
    globals:Ir.Integer_globals.t ->
    header:Sema.Function_type_resolution.resolved_function ->
    parameter:Sema.Function_type_resolution.parameter ->
    Ir.Prepared_parameter_default.t ->
    (unit, string) result

  val initializer_callback_default :
    initializer_request ->
    globals:Ir.Integer_globals.t ->
    pointer:Sema.Function_type_resolution.function_pointer ->
    parameter:Sema.Function_type_resolution.parameter ->
    Ir.Prepared_callback_default.t ->
    (unit, string) result

  val command_callback_default :
    command_request ->
    globals:Ir.Integer_globals.t ->
    pointer:Sema.Function_type_resolution.function_pointer ->
    parameter:Sema.Function_type_resolution.parameter ->
    Ir.Prepared_callback_default.t ->
    (unit, string) result

  val claim_command_request : command_request -> (unit, string) result

  val command_function_source :
    command_request ->
    Ir.Retained_function.t ->
    (Ir.Integer_interpreter.task_function_source, string) result
  (** Requests exist only during their original parser callback. Checking is
      pure; claiming consumes the one native-entry capability. Saved, expired,
      foreign or already-entered requests fail. *)
end

val observe_source_offset :
  t ->
  Task_declarations.t ->
  Frontend.Parser.declaration_event ->
  (unit, Common.Diagnostic.t list) result

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

module Native_static_allocation : sig
  type request
  type t = request -> (unit, Common.Diagnostic.t list) result

  val allocation : request -> Ir.Integer_static_allocation.t
  val context : request -> Ir.Integer_globals.t
  val check : request -> (unit, string) result

  val claim : request -> (unit, string) result
  (** Check or claim the original live allocation in its originating domain.
      Claims are single-use. No initializer values or arena addresses are
      supplied by the caller. *)
end

module Native_static_initializer : sig
  type request
  type t = request -> (unit, Common.Diagnostic.t list) result

  val program : request -> Ir.Static_initializer_program.t
  val generation : request -> Ir.Integer_interpreter.native_generation
  val source_callback : request -> native_source_callback option
  val check : request -> (unit, string) result
  val claim : request -> (unit, string) result

  val function_source :
    request ->
    Ir.Retained_function.t ->
    (Ir.Integer_interpreter.task_function_source, string) result

  val slot_binding :
    request ->
    runtime_calls:Ir.Runtime_call_context.t ->
    owner:Ir.Runtime_call_context.owner ->
    Ir.Runtime_call_context.call ->
    (Ir.Integer_interpreter.native_slot_binding, string) result

  val slot_address_binding :
    request ->
    runtime_calls:Ir.Runtime_call_context.t ->
    owner:Ir.Runtime_call_context.owner ->
    Ir.Runtime_call_context.function_slot_address ->
    (Ir.Integer_interpreter.native_slot_address_binding, string) result

  val slot_address_refresh :
    request ->
    Ir.Integer_interpreter.native_slot_address_binding ->
    (Ir.Integer_interpreter.native_slot_address_binding, string) result

  val provider_available :
    request ->
    runtime_calls:Ir.Runtime_call_context.t ->
    owner:Ir.Runtime_call_context.owner ->
    Ir.Runtime_call_context.call ->
    (bool, string) result

  val parameter_default :
    request ->
    globals:Ir.Integer_globals.t ->
    header:Sema.Function_type_resolution.resolved_function ->
    parameter:Sema.Function_type_resolution.parameter ->
    Ir.Prepared_parameter_default.t ->
    (unit, string) result

  val callback_default :
    request ->
    globals:Ir.Integer_globals.t ->
    pointer:Sema.Function_type_resolution.function_pointer ->
    parameter:Sema.Function_type_resolution.parameter ->
    Ir.Prepared_callback_default.t ->
    (unit, string) result
end

module Native_default : sig
  type request

  type t =
    request -> (Ir.Saved_parameter_value.t, Common.Diagnostic.t list) result

  val program : request -> Ir.Default_fragment_program.t
  val generation : request -> Ir.Integer_interpreter.native_generation
  val source_callback : request -> native_source_callback option
  val check : request -> (unit, string) result
  val claim : request -> (unit, string) result

  val function_source :
    request ->
    Ir.Retained_function.t ->
    (Ir.Integer_interpreter.task_function_source, string) result

  val slot_binding :
    request ->
    runtime_calls:Ir.Runtime_call_context.t ->
    owner:Ir.Runtime_call_context.owner ->
    Ir.Runtime_call_context.call ->
    (Ir.Integer_interpreter.native_slot_binding, string) result

  val slot_address_binding :
    request ->
    runtime_calls:Ir.Runtime_call_context.t ->
    owner:Ir.Runtime_call_context.owner ->
    Ir.Runtime_call_context.function_slot_address ->
    (Ir.Integer_interpreter.native_slot_address_binding, string) result

  val slot_address_refresh :
    request ->
    Ir.Integer_interpreter.native_slot_address_binding ->
    (Ir.Integer_interpreter.native_slot_address_binding, string) result

  val provider_available :
    request ->
    runtime_calls:Ir.Runtime_call_context.t ->
    owner:Ir.Runtime_call_context.owner ->
    Ir.Runtime_call_context.call ->
    (bool, string) result

  val parameter_default :
    request ->
    globals:Ir.Integer_globals.t ->
    header:Sema.Function_type_resolution.resolved_function ->
    parameter:Sema.Function_type_resolution.parameter ->
    Ir.Prepared_parameter_default.t ->
    (unit, string) result

  val initializer_remaining : request -> int

  val callback_default :
    request ->
    globals:Ir.Integer_globals.t ->
    pointer:Sema.Function_type_resolution.function_pointer ->
    parameter:Sema.Function_type_resolution.parameter ->
    Ir.Prepared_callback_default.t ->
    (unit, string) result
  (** Inspect original saved objects only while offered. Entry claims once in
      the originating domain; native work can be recorded once after entry. The
      request expires when its synchronous source callback returns. *)

  val record_steps : request -> int -> (unit, string) result
end

module Native_internal_binding : sig
  type request

  type t =
    request ->
    (Ir.Native_internal_binding_capture.t, Common.Diagnostic.t list) result

  val program : request -> Ir.Internal_binding_fragment_program.t
  val generation : request -> Ir.Integer_interpreter.native_generation
  val source_callback : request -> native_source_callback option
  val check : request -> (unit, string) result
  val claim : request -> (unit, string) result

  val function_source :
    request ->
    Ir.Retained_function.t ->
    (Ir.Integer_interpreter.task_function_source, string) result

  val slot_binding :
    request ->
    runtime_calls:Ir.Runtime_call_context.t ->
    owner:Ir.Runtime_call_context.owner ->
    Ir.Runtime_call_context.call ->
    (Ir.Integer_interpreter.native_slot_binding, string) result

  val slot_address_binding :
    request ->
    runtime_calls:Ir.Runtime_call_context.t ->
    owner:Ir.Runtime_call_context.owner ->
    Ir.Runtime_call_context.function_slot_address ->
    (Ir.Integer_interpreter.native_slot_address_binding, string) result

  val slot_address_refresh :
    request ->
    Ir.Integer_interpreter.native_slot_address_binding ->
    (Ir.Integer_interpreter.native_slot_address_binding, string) result

  val provider_available :
    request ->
    runtime_calls:Ir.Runtime_call_context.t ->
    owner:Ir.Runtime_call_context.owner ->
    Ir.Runtime_call_context.call ->
    (bool, string) result

  val parameter_default :
    request ->
    globals:Ir.Integer_globals.t ->
    header:Sema.Function_type_resolution.resolved_function ->
    parameter:Sema.Function_type_resolution.parameter ->
    Ir.Prepared_parameter_default.t ->
    (unit, string) result

  val initializer_remaining : request -> int

  val callback_default :
    request ->
    globals:Ir.Integer_globals.t ->
    pointer:Sema.Function_type_resolution.function_pointer ->
    parameter:Sema.Function_type_resolution.parameter ->
    Ir.Prepared_callback_default.t ->
    (unit, string) result
  (** Inspect original saved objects only while offered. Entry claims once in
      the originating domain; native work can be recorded once after entry. The
      request expires when its synchronous source callback returns. *)

  val record_steps : request -> int -> (unit, string) result
end

module Native_dimension : sig
  type request

  type t =
    request ->
    ( Ir.Dimension_fragment_program.t Ir.Native_scalar_capture.t,
      Common.Diagnostic.t list )
    result

  val program : request -> Ir.Dimension_fragment_program.t
  val generation : request -> Ir.Integer_interpreter.native_generation
  val source_callback : request -> native_source_callback option
  val check : request -> (unit, string) result
  val claim : request -> (unit, string) result

  val function_source :
    request ->
    Ir.Retained_function.t ->
    (Ir.Integer_interpreter.task_function_source, string) result

  val slot_binding :
    request ->
    runtime_calls:Ir.Runtime_call_context.t ->
    owner:Ir.Runtime_call_context.owner ->
    Ir.Runtime_call_context.call ->
    (Ir.Integer_interpreter.native_slot_binding, string) result

  val slot_address_binding :
    request ->
    runtime_calls:Ir.Runtime_call_context.t ->
    owner:Ir.Runtime_call_context.owner ->
    Ir.Runtime_call_context.function_slot_address ->
    (Ir.Integer_interpreter.native_slot_address_binding, string) result

  val slot_address_refresh :
    request ->
    Ir.Integer_interpreter.native_slot_address_binding ->
    (Ir.Integer_interpreter.native_slot_address_binding, string) result

  val provider_available :
    request ->
    runtime_calls:Ir.Runtime_call_context.t ->
    owner:Ir.Runtime_call_context.owner ->
    Ir.Runtime_call_context.call ->
    (bool, string) result

  val parameter_default :
    request ->
    globals:Ir.Integer_globals.t ->
    header:Sema.Function_type_resolution.resolved_function ->
    parameter:Sema.Function_type_resolution.parameter ->
    Ir.Prepared_parameter_default.t ->
    (unit, string) result

  val initializer_remaining : request -> int

  val callback_default :
    request ->
    globals:Ir.Integer_globals.t ->
    pointer:Sema.Function_type_resolution.function_pointer ->
    parameter:Sema.Function_type_resolution.parameter ->
    Ir.Prepared_callback_default.t ->
    (unit, string) result
  (** Inspect original saved objects only while offered. Entry claims once in
      the originating domain; native work can be recorded once after entry. The
      request expires when its synchronous source callback returns. *)

  val record_steps : request -> int -> (unit, string) result
end

module Native_offset : sig
  type request

  type t =
    request ->
    ( Ir.Offset_fragment_program.t Ir.Native_scalar_capture.t,
      Common.Diagnostic.t list )
    result

  val program : request -> Ir.Offset_fragment_program.t
  val generation : request -> Ir.Integer_interpreter.native_generation
  val source_callback : request -> native_source_callback option
  val check : request -> (unit, string) result
  val claim : request -> (unit, string) result

  val function_source :
    request ->
    Ir.Retained_function.t ->
    (Ir.Integer_interpreter.task_function_source, string) result

  val slot_binding :
    request ->
    runtime_calls:Ir.Runtime_call_context.t ->
    owner:Ir.Runtime_call_context.owner ->
    Ir.Runtime_call_context.call ->
    (Ir.Integer_interpreter.native_slot_binding, string) result

  val slot_address_binding :
    request ->
    runtime_calls:Ir.Runtime_call_context.t ->
    owner:Ir.Runtime_call_context.owner ->
    Ir.Runtime_call_context.function_slot_address ->
    (Ir.Integer_interpreter.native_slot_address_binding, string) result

  val slot_address_refresh :
    request ->
    Ir.Integer_interpreter.native_slot_address_binding ->
    (Ir.Integer_interpreter.native_slot_address_binding, string) result

  val provider_available :
    request ->
    runtime_calls:Ir.Runtime_call_context.t ->
    owner:Ir.Runtime_call_context.owner ->
    Ir.Runtime_call_context.call ->
    (bool, string) result

  val parameter_default :
    request ->
    globals:Ir.Integer_globals.t ->
    header:Sema.Function_type_resolution.resolved_function ->
    parameter:Sema.Function_type_resolution.parameter ->
    Ir.Prepared_parameter_default.t ->
    (unit, string) result

  val initializer_remaining : request -> int

  val callback_default :
    request ->
    globals:Ir.Integer_globals.t ->
    pointer:Sema.Function_type_resolution.function_pointer ->
    parameter:Sema.Function_type_resolution.parameter ->
    Ir.Prepared_callback_default.t ->
    (unit, string) result
  (** Inspect original saved objects only while offered. Entry claims once in
      the originating domain; native work can be recorded once after entry. The
      request expires when its synchronous source callback returns. *)

  val record_steps : request -> int -> (unit, string) result
end

module Native_static_copy : sig
  type request
  type t = request -> (unit, Common.Diagnostic.t list) result

  val destination : request -> Ir.Static_initializer_destination.t
  val check : request -> (unit, string) result

  val claim : request -> (unit, string) result
  (** Single-use original live byte-copy receipt. Claim charges its exact byte
      count against the originating task's initializer allowance. Source bytes
      and destination ownership cannot be supplied by the consumer. *)
end

type progress = private {
  runtime : Ir.Integer_interpreter.task_progress;
  dimension_work : int;
  switch_work : int;
}

val progress : t -> progress
(** Immutable cumulative task observation, including dimension preparation and
    effects reached before a later parse, compile or execution failure. This is
    neither a successful program outcome nor an executable artifact. *)

val compiled_units : t -> Integer_unit.compiled list
(** Immutable collection in compilation order, including checked units whose
    later execution failed and separate saved-compiler child units. The
    collection is not one isolated program. *)

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
  ?compiler_positions:Sema.Compiler_record.compiler_positions ->
  ?max_switch_work:int ->
  ?switch_budget:Sema.Integer_switch_preparation.budget ->
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
  ?native_dispatch:Native_dispatch.t ->
  ?native_static_allocation:Native_static_allocation.t ->
  ?native_static_initializer:Native_static_initializer.t ->
  ?native_static_copy:Native_static_copy.t ->
  ?native_default:Native_default.t ->
  ?native_dimension:Native_dimension.t ->
  ?native_offset:Native_offset.t ->
  ?native_internal_binding:Native_internal_binding.t ->
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

val saved_compiler : Session.t -> ledger:Task_declarations.t -> saved_compiler
(** Retain the original enclosing namespace for a directive adapter. Each use
    still requires its exact live parser suspension and observed source ledger;
    constructing this handle grants no execution authority. *)

val provider_source : t -> Common.Source_file.t option
(** Register source headers for missing hosted providers in this exact frontend.
    The caller must parse and execute those headers at its original source
    boundary; this function does not publish runtime provider entries. *)

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
  ?native_dispatch:Native_dispatch.t ->
  ?native_static_allocation:Native_static_allocation.t ->
  ?native_static_initializer:Native_static_initializer.t ->
  ?native_static_copy:Native_static_copy.t ->
  ?native_default:Native_default.t ->
  ?native_dimension:Native_dimension.t ->
  ?native_offset:Native_offset.t ->
  ?native_internal_binding:Native_internal_binding.t ->
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
  ?native_dispatch:Native_dispatch.t ->
  ?native_static_allocation:Native_static_allocation.t ->
  ?native_static_initializer:Native_static_initializer.t ->
  ?native_static_copy:Native_static_copy.t ->
  ?native_default:Native_default.t ->
  ?native_dimension:Native_dimension.t ->
  ?native_offset:Native_offset.t ->
  ?native_internal_binding:Native_internal_binding.t ->
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
val switch_work : t -> int

val initializer_steps : t -> int
(** Cumulative task preparation, including numeric dimension visits. *)

val synchronize_preparation_work : t -> work:int -> (unit, string) result
(** Charge other source contexts without granting execution or preparation
    authority. The task's existing allowance cannot grow or reset. *)

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

val execute_source :
  ?use_active_stream:bool ->
  ?stream_exe_print:Ir.Integer_interpreter.stream_exe_print ->
  t ->
  command ->
  (Native_dispatch.word option, Common.Diagnostic.t list) result
(** Execute one exact parser-resume command through the task's configured source
    path. Native dispatch claims its opaque live request immediately before
    entry and settles only task metadata; ordinary tasks retain interpreter
    execution. *)

val native_final_value : t -> Native_dispatch.word option
(** Final word metadata from a successfully reached native source command. This
    is not an interpreter execution receipt or native runtime counter. *)

val run :
  t ->
  source:Common.Source_file.t ->
  (Ir.Integer_interpreter.t, Common.Diagnostic.t list) result
(** Parse the exact registered source using the original runtime callbacks for
    initializers, supported integer defaults and array bounds. Each command
    compiles and executes at its parser resume boundary in the shared task.
    Reached publications, storage, writes and resource charges survive later
    errors. A later successful input may continue the task.

    The result freezes this input's final root value and cumulative work totals
    at its accepted completion. Declaration-only inputs have no final value;
    existing generation buffers must remain the exact same stack. Stream
    services require previously installed checked provider declarations.
    [compile_ast] retains its separate callback-free collection path. *)

val stream_executor :
  ?saved_compiler:saved_compiler ->
  ?allow_stream_exe_print:bool ->
  t ->
  Common.Span.t ->
  (Frontend.Parser.stream_execution, Common.Diagnostic.t list) result
(** Retained task adapter for [Parser.parse ~execute_stream]. It executes each
    stream command at its original resume boundary and returns the accepted
    block's generated buffer. Earlier ordinary effects and resource charges
    survive faults; abort injects no partial buffer.

    The task must already own checked provider declarations. Its ledger observes
    only stream commands; an unobserved outer parser must use a distinct
    frontend environment. [saved_compiler] retains that outer parser's original
    ledger for synchronous child input; the source driver supplies it when the
    enclosing namespace differs from the directive task. A bare adapter cannot
    reconstruct a foreign enclosing ledger. Within the stream, original
    initializer leaves finish before later leaves and reuse their retained
    storage at command completion. [allow_stream_exe_print] defaults to [true]
    for this active [#exe] block in both outer compilation modes. Ordinary
    nested source does not inherit that permission; a nested [#exe] establishes
    its own active context. This does not execute the outer unit or provide a
    whole-invocation report. *)

val run_suspended :
  t -> source:Common.Source_file.t -> (unit, Common.Diagnostic.t list) result

val prepare_source_callback_default :
  t ->
  session:Session.t ->
  ledger:Task_declarations.t ->
  Frontend.Parser.completed_callback_default ->
  (unit, Common.Diagnostic.t list) result
