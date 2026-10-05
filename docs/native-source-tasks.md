# Native source tasks

`holyc run --target=host-jit-task --mode=jit` executes integer scalar and fixed
array initializer leaves during their original parser callbacks. Later source
commands use the same native storage. The scalar example is:

```c
I64 A=41;
I64 B=A+1;
B;
```

```text
opam exec -- dune exec --root . -- bin/holyc.exe run --target=host-jit-task --mode=jit --format=json examples/native-source-initializers.hc
```

The initializer for `A` writes its original native cell. At the original live
leaf for `B`, generated code reads that cell and stores 42 into `B`. The resumed
`B;` expression reads the same arena. Initializer side effects run once; later
declaration completion retains the original allocation and reference.

Fixed arrays retain their original checked dimensions and strides. Each leaf
writes its own element, so a later initializer can read an earlier element:

```c
I64 A[2]={41,1};
I64 B=A[0]+A[1];
B;
```

This is `examples/native-source-arrays.hc`. Its two array leaves, scalar leaf
and final command execute as four native fragments. Indexed reads, assignments
and numeric updates support signed and unsigned 8-, 16-, 32- and 64-bit storage,
including Bool's signed byte storage. Multidimensional indexing retains flat
offsets within the original object. An unwritten element remains uninitialized;
writing another element does not authorize its load.

Named direct calls use the exact function selected by the original source:

```c
I64 A=41;
I64 F(){return A+1;}
F();
```

This is `examples/native-source-functions.hc`. The declaration retains its
original checked body, frame and call context. A later call compiles that body
into the caller's native fragment and reads `A` from the same task arena.
Initializer calls such as `I64 B=F();` execute at that original live leaf.
The example returns 42 in three native fragments with nineteen runtime steps
and three separately counted preparation steps. Its eight-byte global and one
initialization flag occupy the same nine-byte arena throughout.
Supported integer and checked scalar-pointer arguments, integer word tails,
and integer or U0 results use the existing native calling path. Automatic local
storage and nested calls retain their original frame and argument owners.
Arguments retain their right-to-left evaluation order; modifying `argc` does
not change the original variadic extent.

Self-recursion uses the original current body and physical frame. Its JIT
unresolved call form may bind only to that same body's checked definition and
declaration ancestry. General unresolved extern calls and later slot replacement
remain outside this direct-call path.

The task publishes function source records when its original declaration
request claims admission. Rejection before that claim leaves the registry
unchanged. An admitted declaration keeps its source after a later reached fault. Each
retained function link identifies its original definition; a newer declaration
with the same name cannot retarget an earlier call. Historical function bodies
also retain their original global references after a global name is replaced.

Each compiled body uses its own original runtime-call context and source
storage. Retained callee lookup consumes the original emission link while
caller arguments retain their selected header. The backend validates those
records before collecting its direct callees. Every body included in a fragment
counts toward that fragment's IR, block and code limits. Recompiling a retained
body in another fragment charges
that new code again. The shared data arena preserves native writes across
those fragment lifetimes.

The collector checks IR and block limits before admitting each body to its work
queue. These source records contain no executable address. The host releases
each fragment's code after execution; stored function addresses and callbacks across
source events still require their own persistent native code owners.

`Native_source_execution.evaluate` accepts the original session, preprocessor
configuration and source. An internal synchronous dispatch connects source
admission to the hosted native executor. The public task API preserves its
existing interpreter interface; the native source API owns dispatch and entry.
The driver retains source and declaration metadata without allocating
interpreter cells.
Closed expression preparation has its own counter. JIT dimensions share that
cumulative initializer allowance; `dimension_work` also reports their reached
node visits. The separate dimension limit applies to ordinary source compilation.
Native steps come from actual native outcomes. Reports retain detached image
metadata and each checked
completion or fault, without keeping executable images or closed source tasks
alive. The JSON `native.fragments` array records their source order, cumulative
native work, storage sizes and compiled function counts; `native.image` is null because
the source task has separate fragments.

Each layout has one opaque native arena. Its data and initialization flags
occupy stable offsets as later globals append. Admission passes a checked
extent to the host, which zeros only the new suffix. Compilation, retention and
report metadata do not build a seed copy of the whole arena. Old native writes
remain in that arena without a second interpreter storage copy. Retention
checks the original live request, layout, ABI and limits before admission.
Execution claims that request once, and return or failure closes it. Source
ownership and the originating domain are checked before the arena binds to its
cumulative budget, so a rejected first call cannot claim the allowance.

Array elements use the existing eight-byte spacing between initialization flags.
The flag base points to the last flag, and indexed access subtracts the scaled
element offset. The array example occupies sixteen data bytes and sixteen flag
bytes before `B` appends eight data bytes and one scalar flag. Logical storage
is 24 bytes; the arena extent is 41 bytes. A two-element U8 array needs two
logical bytes and sixteen flag bytes. Arena reservation accounts for that ratio
and remains capped by the hard native arena limit. Logical byte limits and
private metadata limits are checked separately before admission.

Code and arena leases cover source admission, entry and release. A concurrent
release rejects before changing an active owner. Failed OS cleanup revokes
further activation while preserving the handle needed to retry cleanup.

Code bytes and IR instructions are bounded across all emitted fragments.
Runtime steps, output bytes and output work share the task allowance, including
zero remaining capacity. Reached native faults preserve prior writes and work;
later source does not execute. Uninitialized scalar and element reads retain
their native fault. Declared integer widths govern storage, while full I64/U64 words govern
expression results. The C bridge owns mappings, protection, entry and release;
source admission, lowering, instruction selection and report validation remain
in OCaml.

This source-task path supports integer globals, fixed integer arrays and
retained direct functions. Runtime-dependent dimensions, task callback and
literal storage, function statics, declaration defaults, native `#exe`,
persistent executable addresses and AOT source-task execution remain separate
work under #704.
The existing `host-jit` target keeps
its isolated compilation and AOT load-region contracts. This path adds no
exported HolyC ABI, object or BIN loader, bootstrap, whole-tree compilation or
full-compiler completion claim.

The pinned source is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/PrsVar.HC:53-107` compiles and calls an initializer expression before
writing its declared-width destination. `PrsVar.HC:123-212` traverses array
dimensions and initializes each fixed-count leaf in order.
`Compiler/PrsExp.HC:1068-1100` scales each subscript by its original remaining
dimension stride and element width. `Compiler/CMain.HC:1-32` compiles the
original statement and returns its final expression. `PrsStmt.HC:62-137`
distinguishes original function declarations and reused extern records;
`PrsStmt.HC:140-207` compiles each original body and installs its executable.
`PrsExp.HC:544-586` retains the argument order, selects the unresolved JIT
address-slot call while `Cf_EXTERN` is set, and applies the selected cleanup.
`PrsStmt.HC:181-191` installs the JIT body before clearing that flag.
The hosted direct-call path retains that source identity while compiling code
for each caller fragment. The arena bounds and
ownership checks are hosted policy. These source audits and host tests add no
new TempleOS oracle capture.
