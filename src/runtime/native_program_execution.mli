type platform = Native_execution.platform =
  | Windows_x86_64
  | Linux_x86_64
  | Unsupported

val platform : unit -> platform
val platform_name : platform -> string

val hard_max_active_stack_bytes : int
(** Hard per-execution bound for generated native activations: 65,536 bytes. *)

val execute :
  ?max_frame_bytes:int ->
  ?max_call_depth:int ->
  ?max_active_stack_bytes:int ->
  max_steps:int ->
  Backend.X86_64_program.t ->
  (Backend.X86_64_program.outcome, string) result
(** Execute a sealed program image with positive instruction, semantic-frame and
    call-depth limits plus a bounded physical native-stack budget.
    [max_frame_bytes] defaults to 1,048,576 and [max_call_depth] to 128,
    matching the checked interpreter. [max_active_stack_bytes] defaults to
    65,536 and may not exceed [hard_max_active_stack_bytes]. The root's exact
    stack cost must fit that bound before any executable-memory allocation.
    Generated code charges every reached IR instruction before executing it,
    including branches and loop transfers, and restores semantic-frame,
    call-depth and physical-stack quotas across every normal return and checked
    fault unwind. The image's private status ABI must match the host before any
    executable-memory allocation.

    The bridge owns fresh per-call status and result storage, shares the checked
    expression executor's writable-to-executable mapping lifecycle, and releases
    mapping and unwind registration before returning any OCaml values. Windows
    registers callable images' checked table of generated function ranges and
    unwind records for the lifetime of that mapping. Closed singleton images
    keep the established one-record bridge. The callable bridge verifies that
    the immutable instruction budget and all remaining quotas are back at their
    exact supplied values before boxing status. The image validates the returned
    site, step count and last-expression identity before exposing completion or
    a typed fault. Unsupported hosts, ABI mismatches, malformed status and OS
    failures return an error. Failed Windows unwind removal retains its mapping
    and registered table so the OS cannot retain a dangling reference.

    This executes in the current process while retaining the OCaml runtime lock.
    The IR budget bounds checked loops; it is not a CPU timeout or recovery from
    arbitrary machine-code faults. *)
