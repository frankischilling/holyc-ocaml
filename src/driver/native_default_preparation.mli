type t

type native_request
(** One original default expression while its parser preparation callback is
    active. The request expires when its executor returns and cannot move to
    another OCaml domain. *)

type native_executor = native_request -> (unit, Common.Diagnostic.t list) result

val native_program : native_request -> Ir.Default_fragment_program.t
val native_entered : native_request -> bool
val check_native : native_request -> (unit, string) result

val claim_native : native_request -> (unit, string) result
(** Check the original program and preparation attempt, then claim native entry
    once. Checking alone neither executes nor consumes the request. *)

val native_remaining : native_request -> int

val finish_native :
  native_request ->
  steps:int ->
  Ir.Default_fragment_program.t Ir.Native_scalar_capture.t option ->
  (unit, string) result
(** Charge the reached work after native entry. A successful value requires its
    actual one-use capture while the exact retained image is still live. A
    failed execution charges work without publishing a saved value. *)

val task_state : t -> Ir.Integer_interpreter.task_state

type completion
(** A receipt issued only after actual checked preparation and successful
    completion charged to the original native source invocation. *)

val create :
  ?max_steps:int ->
  ?max_global_bytes:int ->
  ?max_literal_bytes:int ->
  ?max_frame_bytes:int ->
  ?max_call_depth:int ->
  ?max_output_bytes:int ->
  ?max_output_work:int ->
  ?max_generated_bytes:int ->
  ?native_default:native_executor ->
  ?native_initializer:
    (Ir.Integer_interpreter.native_constant_request ->
    (unit, Common.Diagnostic.t list) result) ->
  compilation_mode:Frontend.Preprocessor.compilation_mode ->
  max_initializer_steps:int ->
  ?max_default_bytes:int ->
  Session.t ->
  (t, string) result
(** Create an isolated preparation budget. Closed integer expressions retain
    their existing VM preparation. [native_default] supplies execution for
    admitted original native fragments; this budget itself owns no executable
    memory or source-command executor. Saved payloads default to 65,536 bytes
    and charge eight bytes per successful scalar integer, F64 or original
    one-star callback-word default. Saved words keep all bits before the
    callee's declared-width storage conversion. Numeric callback defaults have
    no executable ownership, independently of their return class. *)

val create_aot_peer :
  t ->
  parent:Session.t ->
  child:Session.t ->
  invocation:Sema.Aot_invocation.t ->
  (Ir.Integer_interpreter.task_state, string) result

val finish_unpaired_aot_output :
  t ->
  sequence:Frontend.Parser.completed_sequence ->
  globals:Ir.Integer_globals.t ->
  initialization:Ir.Global_initialization.t ->
  entry:Ir.X87_stack.t ->
  (unit, string) result

val begin_aot_invocation :
  t ->
  parent:Session.t ->
  ledger:Task_declarations.t ->
  context:Frontend.Parser.command_context ->
  fork:Sema.Symbol_table.empty_fork ->
  (Sema.Aot_invocation.t, Common.Diagnostic.t list) result
(** Begin the original AOT parser invocation with its empty namespace fork and
    source ledger. This does not execute or load the output module. *)

val prepare :
  t ->
  session:Session.t ->
  ledger:Task_declarations.t ->
  Frontend.Parser.completed_parameter_default ->
  (unit, Common.Diagnostic.t list) result

val work : t -> int
val remaining_work : t -> int

val synchronize_work : t -> work:int -> bytes:int -> (unit, string) result
(** Charge other original source contexts to the same invocation allowance.
    Counters can only increase within their existing bounds. This creates no
    preparation, saved-value, storage or source-completion receipt. *)

val prepare_callback :
  t ->
  session:Session.t ->
  ledger:Task_declarations.t ->
  Frontend.Parser.completed_callback_default ->
  (unit, Common.Diagnostic.t list) result
(** Prepare the original anonymous parameter default in the same declaration
    work and saved-byte budgets as named defaults and initializers. *)

val prepare_initializer :
  ?require_load:bool ->
  t ->
  session:Session.t ->
  ledger:Task_declarations.t ->
  Frontend.Parser.completed_initializer_leaf ->
  (unit, Common.Diagnostic.t list) result
(** Prepare one original global initializer leaf under the same declaration-work
    budget as defaults. The exact live layout decides scalar stores versus
    source-owned byte copies. Copy work is charged to [max_initializer_steps].
    AOT scalar leaves with original references retain their source and checked
    destination for load-time lowering, without evaluating or charging a
    prepared value. Their eventual calls and effects use the runtime meter.
    [require_load] rejects leaves that need constant or copy preparation before
    that preparation starts, while retaining the original AOT load checks. *)

val initializers : t -> Integer_initializers.native_preparation list

type initializer_completion
(** Original closed preparation charged to the source invocation budget, or an
    original AOT load leaf whose execution remains scheduled in its bundle. *)

val initializer_completions : t -> initializer_completion list

val initializer_preparation :
  initializer_completion -> Integer_initializers.native_preparation

val bytes : t -> int

val completions : t -> completion list
(** Successfully completed original default fragments, in declaration order.
    Failed preparation retains reached work but publishes no execution or saved
    word. *)

val execution : completion -> Ir.Integer_interpreter.default_constant
(** Read-only fragment facts; a raw fragment execution cannot create a receipt.
*)

type static_completion

val prepare_static :
  ?require_load:bool ->
  t ->
  session:Session.t ->
  ledger:Task_declarations.t ->
  Frontend.Parser.static_initializer_preparation ->
  (unit, Common.Diagnostic.t list) result
(** [require_load] permits only an original AOT load leaf and rejects constant
    or copy preparation before it executes or charges initializer work. *)

val static_completions : t -> static_completion list

val static_preparation :
  static_completion -> Integer_initializers.native_static_preparation

val static_initializers :
  t -> Integer_initializers.native_static_preparation list
