# Retained native images

`Native_program_execution.retain` keeps one original sealed native image and a
private data arena alive across activations. `execute_retained_report` enters
that same mapped entry. Global/static words, initialization flags, mutable
literals, object references and callback executable owners retain their state.
The image is compiled once; activation does not reconstruct code or resolve a
stored callback through a later name.

Activation enters the complete original entry again, including any scheduled
AOT load regions. Those regions execute as ordinary generated code against the
retained arena. The separate [native source-task driver](native-source-tasks.md)
schedules supported scalar and array JIT declarations once against a shared task
arena and compiles direct calls from their original retained function sources.

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

Closed entries also retain their original code. A frameless entry charges its
eight-byte CALL slot; a spilled entry adds its original RSP allocation. Callable
entries charge their separate saved-RBP frame. The host validates each original
metadata form before mapping or activation. Closed entries require a complete
single code range, the original spill allocation and host status prologue, and
no data arena. Windows registers a spilled entry's original unwind record; a
frameless leaf needs no unwind table. Retention does not recompile an entry into
another frame convention.

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

## Shared execution allowances

`create_budget` gives retained activations one cumulative instruction,
output-byte and output-work allowance. `execute_retained_budget_report` charges
that allowance when an activation completes or reaches a checked fault. Its
step counter is cumulative; the returned output and output work belong to that
activation. `budget_progress` freezes the verified totals and ordered output.

For a retained entry `I64 G=40;++G;`, a budget covering two activations permits
41 and 42. The next activation reaches the original native instruction guard
with zero remaining steps. It cannot increment `G` again. Reached writes during
a partial activation remain in the arena and its consumed work stays charged.

Exhausted output allowances still permit code that produces no output. A later
output operation reaches the native byte or work guard with zero remaining
capacity. PutChars retains its published byte prefix on a fault; Print keeps
its atomic draft behavior. No extra byte or work unit is granted to get past a
positive-limit API check. Configured total limits remain positive, and the
existing single-activation interfaces keep their original validation rules.

An atomic guard rejects concurrent use of the same budget. Verified counters
and output publish together. Saved output uses bounded, coalesced chunks;
activation reports and progress snapshots cannot modify that saved prefix.
Invalid preflight limits or an already released owner leave the allowance
unchanged. The host marks entry only after its activation and lifetime checks;
a rejected concurrent entry cannot revoke an untouched allowance. Once native
entry begins, an unverified host or status failure revokes the allowance
because its native effects cannot be accounted for.
Progress then retains the last verified prefix and records the failure state.

Different retained images may share an allowance while keeping separate arenas.
Frame, depth, active-stack and image-storage bounds remain per activation.
The source-task path connects original source receipts, cumulative data
allocation and declaration scheduling to one shared arena. It compiles retained
direct function bodies into each caller fragment. Persistent function addresses
and the wider source-session requirements remain open.

The maintained suite covers both source modes, actual native
globals/statics/arrays/literals, callback owners across GC, reached arithmetic
and step faults, recursive frame/depth/stack failures, recovery, output capture,
separate arenas, release, collection, original bounds and foreign ABI rejection.
Concurrent domain activations verify distinct writes for accepted entries. AOT
load-region calls and reached load faults execute against retained state.
Closed source entries cover frameless code and both small and large spill
allocations, repeated entry across GC, exact stack/work quotas, reached division
and remainder faults, recovery, and changed range/frame/prologue rejection.
The cumulative-budget suite compares public IR values and output with native
execution, then exercises repeated writes, exact and one-below steps, exhausted
byte/work limits, partial and atomic output faults, retryable preflight,
separate arenas, concurrent admission and malformed consumed-counter tuples.

The pinned source context is `c26482bb6ad3f80106d28504ec5db3c6a360732c`:
`Compiler/PrsStmt.HC:150-194` emits source-owned function code, and
`Kernel/KTask.HC:251-264` initializes task-owned code/data heaps. Hosted retention
is a resource-lifetime foundation. Original scalar and array JIT parser callbacks
and retained direct calls execute through the source-task path. Persistent
executable addresses and their replacement/expiry rules remain unfinished. No new
TempleOS capture, exported HolyC ABI, loader, bootstrap or full-compiler proof
is claimed.
