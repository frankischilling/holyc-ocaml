# Retained native images

`Native_program_execution.retain` keeps one original sealed native image and a
private data arena alive across activations. `execute_retained_report` enters
that same mapped entry. Global/static words, initialization flags, mutable
literals, object references and callback executable owners retain their state.
The image is compiled once; activation does not reconstruct code or resolve a
stored callback through a later name.

Activation enters the complete original entry again, including any scheduled
AOT load regions. Those regions execute as ordinary generated code against the
retained arena. Scheduling each source declaration once belongs to the remaining
native task driver.

Each activation has fresh bounded status, frame/depth/stack allowances and
output capture. The ordinary sealed-image decoder checks its actual native
completion or reached fault. Writes before a checked fault persist, including
when the next activation recovers under a larger execution quota. The existing
single-program executor continues to create a fresh arena for each execution.

The owner is opaque. Callers supply a sealed backend image and cannot substitute
another image, expose a host pointer or import a numeric address. Retention
checks the host ABI, complete unwind metadata, entry stack and data bounds
before mapping. Activations also check their execution limits before entry.
Separate owners have separate arenas, even when created from the same image.

The host bridge stages code as writable memory, then seals it executable and
read-only. Its data arena remains read/write and non-executable. Windows keeps
the original checked unwind table registered for the mapping's lifetime. Linux
checks `READ_IMPLIES_EXEC` before reserving writable mappings. The original
OCaml image is rooted while the native owner lives; garbage collection and
compaction cannot replace its executable or storage identity.

`release` unregisters unwind metadata and releases the original arena and code.
Successful release is idempotent; later activation rejects before entry. An
atomic host guard rejects overlapping activation or release across OCaml
domains. Once cleanup starts, entry is revoked even if OS release fails partway.
Failed OS release preserves remaining resources only for a release retry. A native
finalizer attempts cleanup for an unreachable owner. Failed Windows unwind
removal retains its registered mapping so the OS keeps no dangling table.

The compiler, checked image, decoding and API ownership remain in OCaml. The C
boundary handles native mappings, cache synchronization, entry and host resource
lifetime. It adds no instruction selection, assembler or language evaluation.

The maintained nine-group suite covers both source modes, actual native
globals/statics/arrays/literals, callback owners across GC, reached arithmetic
and step faults, recursive frame/depth/stack failures, recovery, output capture,
separate arenas, release, collection, original bounds and foreign ABI rejection.
Concurrent domain activations verify distinct writes for accepted entries. AOT
load-region calls and reached load faults execute against retained state.

The pinned source context is `c26482bb6ad3f80106d28504ec5db3c6a360732c`:
`Compiler/PrsStmt.HC:150-194` emits source-owned function code, and
`Kernel/KTask.HC:251-264` initializes task-owned code/data heaps. Hosted retention
is a resource-lifetime foundation. Native execution during original JIT parser
callbacks, persistent source-task admission, linking separate images and live
native replacement/expiry remain unfinished. No new TempleOS capture, exported
HolyC ABI, loader, bootstrap or full-compiler proof is claimed.
