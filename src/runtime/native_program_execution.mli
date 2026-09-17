type platform = Native_execution.platform =
  | Windows_x86_64
  | Linux_x86_64
  | Unsupported

val platform : unit -> platform
val platform_name : platform -> string

val execute :
  max_steps:int ->
  Backend.X86_64_program.t ->
  (Backend.X86_64_program.outcome, string) result
(** Execute a sealed program image with a positive instruction budget. Generated
    code charges every reached IR instruction before executing it, including
    branches and loop transfers. The image's private status ABI must match the
    host before any executable-memory allocation.

    The bridge owns fresh per-call status and result storage, shares the checked
    expression executor's writable-to-executable mapping lifecycle, and releases
    mapping and unwind registration before returning any OCaml values. The image
    validates the returned site, step count and last-expression identity before
    exposing completion or a typed fault. Unsupported hosts, ABI mismatches,
    malformed status and OS failures return an error. Failed Windows unwind
    removal retains its mapping so the OS cannot retain a dangling reference.

    This executes in the current process while retaining the OCaml runtime lock.
    The IR budget bounds checked loops; it is not a CPU timeout or recovery from
    arbitrary machine-code faults. *)
