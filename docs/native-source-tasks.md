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

`Native_source_execution.evaluate` accepts the original session, preprocessor
configuration and source. An internal synchronous dispatch connects source
admission to the hosted native executor. The public task API preserves its
existing interpreter interface; the native source API owns dispatch and entry.
The driver retains source and declaration metadata without allocating
interpreter cells.
Closed expression preparation has its own counter. JIT dimensions share that
cumulative initializer allowance; `dimension_work` also reports their reached
node visits. The separate dimension limit applies to ordinary source compilation.
Native steps come from actual native outcomes. Reports retain detached image metadata and each checked
completion or fault, without keeping executable images or closed source tasks
alive. The JSON `native.fragments` array records their source order, cumulative
native work and storage sizes; `native.image` is null because
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

This source-task path supports integer globals, fixed integer arrays and their
ordinary numeric commands. Runtime-dependent dimensions, callback storage,
retained functions, literals, effectful defaults, native `#exe`, live function
replacement and AOT source-task execution remain separate work under #704.
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
original statement and returns its final expression. The arena bounds and
ownership checks are hosted policy. These source audits and host tests add no
new TempleOS oracle capture.
