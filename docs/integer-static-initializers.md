# Scalar static declaration initializers

[Static array initializers](integer-persistent-arrays.md) extend the same phase
and owner checks to a complete batch of original expression leaves. Initialization
still occurs at the containing definition's module position, including functions
that are never invoked.

A declaration region can materialize [scalar references](integer-pointers.md)
for fixed pointer arguments. `IC_ADDR` checks authority at its own instruction;
canonical metadata cannot be borrowed after the region ends. Explicit references
retain their object when passed to a callee, without passing region authority.

`holyc run --target=ir examples/integer-static-initializers.hc` initializes
`Next`'s static word by calling the earlier `Seed` definition, then calls `Next`
twice. It returns 42 in 32 runtime instructions and zero constant-preparation
instructions in both JIT and AOT modes. Eight persistent bytes, one frame byte
and call depth one suffice. The separate positive initializer-step limit still
applies to constant preparation elsewhere in the program.

Nonconstant scalar I64/U64 static initializers execute at the containing function's
source position, in lexical declaration order, including unused functions,
false branches and declarations after returns. No initializer store runs on
each invocation. Persistent reads, assignments, scalar updates and supported
direct calls use the existing expression and call planners. Ordinary top-level
expressions retain their last reached value; initializer expression ends do not
replace it. Every execution starts with fresh words.

## Phase and invocation context

A JIT static region records `compile-initializer`; a normal AOT region records
`load-initializer`. Earlier module statements affect subsequent static values.
The VM checks JIT availability against the actual supplied callee graphs,
transitively: each called definition must precede the containing function.
Earlier recursive callees are valid. A self-call to the containing function
fails with HCIRVM0017 in JIT, while AOT load execution can call the completed
body. Transitive publication failures retain the calling function's identity,
operator span, initializer identity and phase.

A static initializer has no invocation of its containing function. Reads of
that function's parameters or automatic locals reject with HCRUN0006; pure
metadata queries remain available. Called functions receive their own frames
and consume normal active-frame and call-depth bounds. Initializer faults keep
the active callee identity and declaration phase through calls.

Static allocation remains on the code heap regardless of globals-on-data-heap.
That option changes AOT initializer deferral in the reference, however, so
nonconstant AOT statics with it enabled retain an explicit HCRUN0006 boundary.
Their separate compile-time execution phase remains required. JIT scheduled
values and constant preparation retain the option snapshot.

## Checked regions

The source driver retains each exact static root and published storage slot.
The common initialization context checks all pending globals and statics in
semantic order, exact root/slot ownership, canonical destination/store/end,
closed operands and call scopes, increasing instruction IDs and nonoverlapping
single-block regions. Contexts bind the exact entry graph and persistent image.
Static-only replay still requires complete matching evidence.

Both static address producers and storage consumers require the declaring
function or a checked region owned by that function. Region authority ends at
its boundary and is never passed to callee preflight, even when entry and callee
instruction IDs overlap. Global-only region and lowering APIs remain available;
the old lowering projection rejects static regions that it cannot return.
Preparation exposes separate read-only static classifications and keeps raw
initial-image updates private.

## Reference and remaining work

All source locations refer to `c26482bb6ad3f80106d28504ec5db3c6a360732c`:

- `Kernel/KTask.HC:338-351` compiles and executes JIT statements in order.
- `Compiler/PrsVar.HC:53-112,215-244` handles static initializers while parsing
  bodies; lines 57-59 select the deferral option and 82-85 queue AOT MAIN work.
- `Compiler/CMain.HC:82-90,513-531` queues ordinary MAIN work after parsing and
  serializes the remaining MAIN entries in order.
- `Kernel/KLoad.HC:158-165` calls those MAIN entries in loader order.
- `Compiler/PrsStmt.HC:160-184` publishes a function after its body is compiled.

These tests establish hosted source behavior and checked raw-IR boundaries;
they add no native TempleOS capture. Whole-program preflight and constant
preparation precede hosted mutable execution. Exact native JIT parsing,
effect and fault order needs stateful compilation. Existing initializer shift
and constant-divisor optimizer guards remain. General memory and output,
stateful `#exe`, full optimizer parity, native backends, BIN/loader acceptance
and bootstrap remain unfinished requirements of the full compiler.
