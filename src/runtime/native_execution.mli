type platform = Windows_x86_64 | Linux_x86_64 | Unsupported

val platform : unit -> platform
val platform_name : platform -> string

val execute : Backend.X86_64_expression.t -> (int64, string) result
(** Explicitly execute an already checked native expression in this process. The
    host bridge allocates writable memory, copies the sealed image, changes it
    to executable/read-only, calls it, and releases it before returning the
    complete 64-bit result. Spill frames are bounded by the checked image;
    Windows registers its generated unwind information during execution.
    Unsupported hosts and OS failures return an error. If Windows cannot remove
    a registered function table, its mapping is retained instead of leaving a
    dangling OS reference. This is not a sandbox or a general HolyC runtime. *)
