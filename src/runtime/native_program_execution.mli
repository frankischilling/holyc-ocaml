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
(** Hard host allocation bound for data plus per-slot initialization state: 32
    MiB. *)

val execute :
  ?max_frame_bytes:int ->
  ?max_call_depth:int ->
  ?max_active_stack_bytes:int ->
  ?max_global_bytes:int ->
  ?max_literal_bytes:int ->
  max_steps:int ->
  Backend.X86_64_program.t ->
  (Backend.X86_64_program.outcome, string) result
(** Execute a sealed program image with positive instruction, semantic-frame and
    call-depth limits plus a bounded physical native-stack budget.
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
    immutable instruction budget, arena pointer and all remaining quotas are
    back at their exact supplied values before boxing status. The image
    validates the returned site, step count and last-expression identity before
    exposing completion or a typed fault. Unsupported hosts, ABI mismatches,
    malformed status and OS failures return an error. Failed Windows unwind
    removal retains its mapping and registered table so the OS cannot retain a
    dangling reference.

    This executes in the current process while retaining the OCaml runtime lock.
    The IR budget bounds checked loops; it is not a CPU timeout or recovery from
    arbitrary machine-code faults. *)
