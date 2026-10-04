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
    is accepted or exposed. *)

val release : retained -> (unit, string) result
(** Release the original mapping, arena and Windows unwind registration.
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
    link separate images, or establish native JIT declaration execution. *)

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

val execute_retained_budget_report :
  ?max_frame_bytes:int ->
  ?max_call_depth:int ->
  ?max_active_stack_bytes:int ->
  ?max_global_bytes:int ->
  ?max_literal_bytes:int ->
  budget ->
  retained ->
  report
(** Execute an original retained entry using this allowance's remaining
    instruction, output-byte and output-work limits. Zero remaining allowances
    reach the generated guards: exhausted output does not prevent quiet code,
    while exhausted steps stop before the first source instruction. A checked
    completion or fault reports cumulative executed steps, with output and work
    for this activation. Both consume the original shared allowance and preserve
    reached native writes. Preflight and rejected native admission consume
    nothing. The host marks entry only after acquiring the original image's
    activation guard and checking its lifetime. Once native entry begins, an
    unverified host/status failure revokes the allowance.

    Concurrent use of one allowance rejects overlap. Multiple original images
    may share an allowance while retaining their separate data arenas. Frame,
    call-depth, active-stack and image-storage bounds keep their per-activation
    meanings. This host execution primitive supplies no source-command receipts,
    once-only declaration scheduling, cumulative allocation admission, or
    linkage between images. The existing [execute_retained_report] keeps fresh
    limits. *)

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
    no captured bytes or work. *)

val outcome : report -> (Backend.X86_64_program.outcome, string) result
val output_bytes : report -> string
val output_work : report -> int

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
    bytes. This is [execute_report] projected through [outcome].

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
