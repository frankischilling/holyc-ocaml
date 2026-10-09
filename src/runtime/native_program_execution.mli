type platform = Native_execution.platform =
  | Windows_x86_64
  | Linux_x86_64
  | Unsupported

val platform : unit -> platform
val platform_name : platform -> string

val hard_max_active_stack_bytes : int
(** Hard per-execution bound for generated native activations: 65,536 bytes. *)

val hard_max_global_bytes : int
(** Hard logical global and static byte bound: 16 MiB. *)

val hard_max_literal_bytes : int
(** Hard logical literal byte bound, including terminators: 16 MiB. *)

val hard_max_arena_bytes : int
(** Hard host allocation bound for data, initialization and callback ownership:
    32 MiB. *)

val hard_max_output_bytes : int
(** Hard captured-output byte bound: 16 MiB. *)

type report
type retained
type task_arena

val retain :
  ?max_global_bytes:int ->
  ?max_literal_bytes:int ->
  ?max_active_stack_bytes:int ->
  Backend.X86_64_program.t ->
  (retained, string) result
(** Retain the original sealed host image and one private data arena. Code moves
    from writable staging to executable/read-only protection once; data stays
    read/write and non-executable. Bounds and the host ABI are checked before
    allocation. Closed entries retain their original RSP spill frame or
    frameless code; callable entries retain their saved-RBP frame. Each charges
    its own exact entry stack footprint. No native pointer or replacement image
    is accepted or exposed. Task-fragment images are rejected here and require
    [retain_task_fragment]. *)

val create_task_arena :
  ?max_arena_bytes:int ->
  Backend.X86_64_global_storage.task_layout ->
  (task_arena, string) result
(** Allocate one opaque native storage owner for an exact append-only task
    layout. [max_arena_bytes] defaults to 32 MiB and is bounded by
    [hard_max_arena_bytes]. The host reserves one stable address range and
    commits it read/write and non-executable as task snapshots grow. No native
    address is exposed. A task layout admits exactly one arena owner; creating a
    second owner fails, including after the original arena has been released. *)

val release_task_arena : task_arena -> (unit, string) result
(** Close the arena and all canonical/current code mappings retained by its
    original executable owners. Active code and arena leases reject release.
    Release the task storage mapping. Successful release is idempotent. Active
    native entry or admission rejects release, and a released arena cannot admit
    or execute another fragment. Unreachable handles have a native finalizer. *)

val allocate_task_static :
  ?scope:Ir.Native_source_suspension.t ->
  ?max_global_bytes:int ->
  task_arena ->
  Driver.Integer_task.Native_static_allocation.request ->
  (unit, string) result
(** Admit original private static storage under the arena lease during its live
    allocation callback. Claim the request once immediately before committing
    the new zeroed suffix. Existing data and initialization flags remain
    authoritative. This executes no initializer or function entry. *)

val copy_task_static :
  ?scope:Ir.Native_source_suspension.t ->
  task_arena ->
  Driver.Integer_task.Native_static_copy.request ->
  (unit, string) result
(** Execute the pinned compiler's direct literal MemCpy branch in the original
    admitted native byte-array allocation. Exact live source ownership, extent,
    flags, domain and task allowance are checked before writing. This neither
    interprets an expression nor seeds storage from a prepared image. *)

val retain_task_fragment :
  ?scope:Ir.Native_source_suspension.t ->
  ?max_global_bytes:int ->
  ?max_literal_bytes:int ->
  ?max_active_stack_bytes:int ->
  task_arena ->
  Backend.X86_64_program.t ->
  (retained, string) result
(** Retain code for a source-task image whose opaque storage snapshot belongs to
    this exact arena layout. A larger snapshot commits and zero-initializes only
    the appended suffix, so earlier native values and initialization flags stay
    authoritative. New original literal payloads are copied into that suffix
    once; existing mutable bytes and reference descriptors are preserved. The
    retained fragment owns its executable mapping and unwind registration but no
    private data arena. Task fragments execute only through
    [execute_retained_budget_report], which uses the same shared arena and the
    image's live one-shot source activation. Request identity, image bounds,
    host ABI and stack metadata are checked before arena admission. *)

val release : retained -> (unit, string) result
(** Release the original code mapping and Windows unwind registration, plus the
    private arena of an ordinary retained image. A task fragment leaves its
    separately owned [task_arena] intact.

    Successful release is idempotent. A released image cannot execute; an active
    image rejects release. Once cleanup starts, the owner cannot activate again.
    Failed OS release keeps remaining resources only for a later release retry.
    Unreachable handles have a native finalizer. *)

val execute_retained_report :
  ?max_frame_bytes:int ->
  ?max_call_depth:int ->
  ?max_active_stack_bytes:int ->
  ?max_global_bytes:int ->
  ?max_literal_bytes:int ->
  ?max_output_bytes:int ->
  ?max_output_work:int ->
  max_steps:int ->
  retained ->
  report
(** Execute the same original mapped entry with persistent global/static/literal
    data and executable owners. Each activation has fresh bounded status and
    output capture, using the ordinary sealed-image decoder. Reached writes
    persist on checked faults. Overlapping activations or release reject across
    OCaml domains. The complete original entry runs again, including scheduled
    AOT load regions; this API supplies no once-only declaration scheduling.
    This host lifetime primitive does not schedule or replay parser callbacks,
    link separate images, or establish native JIT declaration execution. Shared
    task fragments are rejected because their source entry requires the
    cumulative task budget and live activation path. *)

type budget

type budget_progress = private {
  executed_steps : int;
  output_byte_length : int;
  output_work : int;
  output_bytes : string;
  error : string option;
}

val create_budget :
  ?max_output_bytes:int ->
  ?max_output_work:int ->
  max_steps:int ->
  unit ->
  (budget, string) result
(** Create one cumulative instruction and output allowance. Limits must be
    positive; output bytes have the same hard bound as [execute_report]. The
    default byte and work limits are each 1,048,576. The allowance cannot reset
    or grow. This does not allocate or admit a native image. *)

val budget_progress : budget -> budget_progress
(** Freeze the last verified cumulative counters and ordered output, including
    checked faults. The byte string is a copy. A concurrent activation publishes
    its complete observation atomically. [error] revokes further use after a
    host or status failure whose native effects could not be verified; counters
    then describe only the last verified prefix. *)

val budget_output_bytes : budget -> string
(** Return a copied ordered output prefix from the last verified cumulative
    budget state. *)

val execute_retained_budget_report :
  ?scope:Ir.Native_source_suspension.t ->
  ?max_activation_steps:int ->
  ?max_frame_bytes:int ->
  ?max_call_depth:int ->
  ?max_active_stack_bytes:int ->
  ?max_global_bytes:int ->
  ?max_literal_bytes:int ->
  ?source_callback:
    (Ir.Native_source_suspension.t ->
    Ir.Native_source_suspension.request ->
    int64 option) ->
  budget ->
  retained ->
  report
(** An optional positive [max_activation_steps] further limits this activation
    without resetting or increasing the shared allowance. Execute an original
    retained entry using this allowance's remaining instruction, output-byte and
    output-work limits. Zero remaining allowances reach the generated guards:
    exhausted output does not prevent quiet code, while exhausted steps stop
    before the first source instruction. A checked completion or fault reports
    cumulative executed steps, with output and work for this activation. Both
    consume the original shared allowance and preserve reached native writes.
    Preflight and rejected native admission consume nothing. The host marks
    entry only after acquiring the original image's activation guard and
    checking its lifetime. Once native entry begins, an unverified host/status
    failure revokes the allowance.

    Concurrent use of one allowance rejects overlap. An explicit [scope] may
    borrow its original active budget while the physical caller is suspended. C
    checks the exact scope, budget and generated-byte owner, inherits the
    caller's remaining quotas and joins actual child usage before returning.
    Ordinary retained images may share an allowance while keeping independent
    private arenas. Task fragments retained against the same [task_arena]
    instead use that single authoritative mapping; code and arena exclusion are
    both acquired before entry, and each fragment must name a snapshot already
    admitted to the exact layout. The first task entry binds that arena to this
    exact cumulative [budget]; later fragments reject another budget before
    consuming their live source request. Its opaque live source activation is
    checked after OCaml preflight and immediately before native dispatch. A
    rejected host admission leaves the allowance verified; once native entry is
    marked, an unverified host/status failure revokes it. Frame, call-depth,
    active-stack and image-storage bounds keep their per-activation meanings.
    The existing [execute_retained_report] keeps fresh limits only for ordinary
    retained images.

    An authenticated active task source site may invoke [source_callback] after
    formatting. Its C-created scope expires before native execution resumes;
    ordinary image, arena and budget entries remain excluded while it runs.
    Scoped task preparation and execution may borrow an active arena only when
    this caller or a suspended ancestor owns that exact mapping and budget.
    Callback exceptions are returned through C and raised after native cleanup
    and checked budget accounting. This low-level callback alone provides no
    parser, namespace or child-machine admission authority. *)

val suspension_owns_budget :
  Ir.Native_source_suspension.t -> budget -> (bool, string) result
(** Compare the original cumulative owner while its physical callback is
    suspended. Equal numeric allowances do not identify that owner. *)

val execute_report :
  ?max_frame_bytes:int ->
  ?max_call_depth:int ->
  ?max_active_stack_bytes:int ->
  ?max_global_bytes:int ->
  ?max_literal_bytes:int ->
  ?max_output_bytes:int ->
  ?max_output_work:int ->
  max_steps:int ->
  Backend.X86_64_program.t ->
  report
(** Execute a sealed program image and retain its checked outcome plus captured
    native output. [max_output_bytes] defaults to 1,048,576 and must be between
    1 and [hard_max_output_bytes]. [max_output_work] also defaults to 1,048,576
    and must be positive. The output-work budget has no corresponding buffer.

    Images without authenticated output sites retain an empty output report and
    use the established program bridges. Images with output sites receive a
    rooted private capture buffer and output counters in the existing status
    context. A reached program fault retains bytes published before the fault
    and their exact work count. Host, ABI and status-integrity failures expose
    no captured bytes or work. Source-task images are rejected here and require
    [retain_task_fragment] plus [execute_retained_budget_report]. *)

val outcome : report -> (Backend.X86_64_program.outcome, string) result
val output_bytes : report -> string
val output_work : report -> int

val generation_capture :
  report ->
  Ir.Integer_interpreter.native_generation Ir.Native_generation_capture.t option
(** The reached capture has already been published to its original generation
    target. Observing it grants no replay or foreign-stream authority. *)

val value_captured : report -> bool
(** A checked native END_EXP site was reached during this activation. An empty
    command leaves an earlier task value alone; an explicit no-value capture can
    clear it. This observes the returned native site, not source syntax. *)

val finish_task_data_default :
  ?scope:Ir.Native_source_suspension.t ->
  ?max_copy_bytes:int ->
  task_arena ->
  Backend.X86_64_program.t ->
  Ir.Saved_parameter_value.t ->
  max_copy_steps:int ->
  (Ir.Saved_parameter_value.t, string) result * int
(** Retain the exact native data capture. Miscellaneous-data defaults copy the
    bounded resulting string into fresh task storage and return attempted copy
    work even when the scan fails. The caller settles the original request. *)

val execute :
  ?max_frame_bytes:int ->
  ?max_call_depth:int ->
  ?max_active_stack_bytes:int ->
  ?max_global_bytes:int ->
  ?max_literal_bytes:int ->
  ?max_output_bytes:int ->
  ?max_output_work:int ->
  max_steps:int ->
  Backend.X86_64_program.t ->
  (Backend.X86_64_program.outcome, string) result
(** Execute a sealed program image with positive instruction, semantic-frame,
    call-depth and output-work limits plus bounded stack, storage and output
    bytes. This is [execute_report] projected through [outcome]. Source-task
    images have the same shared-arena restriction as [execute_report].

    [max_frame_bytes] defaults to 1,048,576 and [max_call_depth] to 128,
    matching the checked interpreter. [max_active_stack_bytes] defaults to
    65,536 and may not exceed [hard_max_active_stack_bytes]. [max_global_bytes]
    defaults to 1,048,576 and may not exceed [hard_max_global_bytes]. The root's
    exact stack cost and the image's complete logical global storage must fit
    their bounds before any executable-memory allocation. Generated code charges
    every reached IR instruction before executing it, including branches and
    loop transfers, and restores semantic-frame, call-depth and physical-stack
    quotas across every normal return and checked fault unwind. The image's
    private status ABI must match the host before any executable-memory
    allocation.

    The bridge owns fresh per-call status and result storage. A data-bearing
    image additionally receives a fresh private read/write, non-executable arena
    copied from the sealed initial image; its address is carried only in the
    immutable private context and is shared by entry and generated callees. The
    arena is released before status is boxed, including checked faults. Code
    shares the checked expression executor's writable-to-executable mapping
    lifecycle, and releases mapping and unwind registration before returning any
    OCaml values. Windows registers callable images' checked table of generated
    function ranges and unwind records for the lifetime of that mapping. Closed
    singleton images keep the established one-record bridge only when they
    contain no globals; storage-bearing entries use the callable/unwind bridge
    even with zero named functions. The callable bridge verifies that the
    immutable instruction budget, arena and output pointers, and callable quotas
    retain their expected values before boxing status. Output byte and work
    counters may only decrease within their supplied bounds, and their consumed
    amounts must agree with the captured prefix. The image validates the
    returned site, step count and last-expression identity before exposing
    completion or a typed fault. Unsupported hosts, ABI mismatches, malformed
    status and OS failures return an error. Failed Windows unwind removal
    retains its mapping and registered table so the OS cannot retain a dangling
    reference.

    This executes in the current process while retaining the OCaml runtime lock.
    The IR budget bounds checked loops; it is not a CPU timeout or recovery from
    arbitrary machine-code faults. *)

val finish_task_internal_binding :
  ?scope:Ir.Native_source_suspension.t ->
  task_arena ->
  Backend.X86_64_program.t ->
  (Ir.Native_internal_binding_capture.t, string) result
(** Take the actual successful scalar capture from this original image and arena
    once. Metadata, a different image or arena, and replay are rejected. *)

val finish_task_dimension :
  ?scope:Ir.Native_source_suspension.t ->
  task_arena ->
  Backend.X86_64_program.t ->
  (Ir.Dimension_fragment_program.t Ir.Native_scalar_capture.t, string) result
(** Take this original image's actual scalar capture once while its arena lives.
*)

val finish_task_offset :
  ?scope:Ir.Native_source_suspension.t ->
  task_arena ->
  Backend.X86_64_program.t ->
  (Ir.Offset_fragment_program.t Ir.Native_scalar_capture.t, string) result
(** Take this original image's actual scalar capture once while its arena lives.
*)
