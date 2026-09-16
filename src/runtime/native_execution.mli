type platform = Windows_x86_64 | Linux_x86_64 | Unsupported

val platform : unit -> platform
val platform_name : platform -> string

type execution_outcome =
  | Returned of int64
  | Fault of Backend.X86_64_expression.arithmetic_fault

val execute_detailed :
  Backend.X86_64_expression.t -> (execution_outcome, string) result
(** Execute an opaque checked image using its declared private-status ABI.
    Foreign ABIs reject before executable allocation. The host bridge releases
    the mapping and its unwind registration before returning bits or status;
    OCaml validates the status against the image's original arithmetic sites. A
    reached arithmetic fault is distinct from a host/integrity error. *)

val execute : Backend.X86_64_expression.t -> (int64, string) result
(** Explicitly execute an already checked native expression in this process. The
    host bridge allocates writable memory, copies the sealed image, changes it
    to executable/read-only, calls it, and releases it before returning the
    complete 64-bit result. Spill frames are bounded by the checked image;
    Windows registers its generated unwind information during execution.
    Unsupported hosts and OS failures return an error. If Windows cannot remove
    a registered function table, its mapping is retained instead of leaving a
    dangling OS reference. This compatibility wrapper renders a checked
    arithmetic fault as an error string; [execute_detailed] retains its type and
    original site. This is not a sandbox or a general HolyC runtime. *)
