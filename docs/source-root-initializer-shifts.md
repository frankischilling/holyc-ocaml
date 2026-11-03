# Source initializer shifts

A source-owned global or static initializer can prepare a scalar integer
constant or variable-count left or right shift. This includes a shift inside a
call argument and an initializer that updates its source global. The runtime
executes the update once and keeps the initialized result when that source later changes.

## Original observations

At pinned reference `c26482bb6ad3f80106d28504ec5db3c6a360732c`, thirteen
source/capture pairs in `test/oracle/source-root-shifts` record JIT and compiled
BIN-load results twice in one boot. The original compiler reported no errors.
They cover signed and unsigned right shifts, negative and large constant counts,
and a global increment followed by a later mutation. The saved shifted result
remains unchanged after that later mutation. The original optimizer also rewrites
a nonconstant signed division by two to a right shift: -3 / 2 produces -2 in
this context, unlike a folded constant division. A static initializer capture
shows its update occurs before the first call and its value survives later calls.
Variable-count captures cover signed and unsigned results; negative, zero and
word-boundary counts; and updates to both operands in a static initializer.
Narrow signed and unsigned captures cover 8-, 16- and 32-bit operands,
including a static initializer with updates to both operands and left-shift
results larger than their source storage width.

`Compiler/PrsVar.HC` supplies initializer compilation and evaluation;
`OptPass012.HC` and `BackA.HC` select and emit the shift operations. The
existing constant-shift tests describe the underlying optimizer and class
semantics in more detail.

## Ownership and limits

Preparation uses a graph freshly lowered from the exact retained global root or
live initializer fragment. For static roots, the exact frame and static slot
must also match. The lowering must still own that exact typed expression and
global context. Only its original zero-flag `IC_SHL_CONST`, `IC_SHR_CONST`,
`IC_SHL` and `IC_SHR` instructions qualify. The result and each operand producer
must have supported non-pointer scalar integer types. The instruction identity
must be the one selected from that graph. Unsupported or unproved shifts remain
outside this path.

The IR and live native-task paths cover both source modes; native AOT source
also covers this case. Standalone native JIT initializers with source references
remain outside the existing native closed-initializer path. A global shift used
inside an AOT call can still encounter separate argument, relocation, frame, or
return-value boundaries.

The focused oracle tests compare original values and once-only effects, and
reject foreign frames, global contexts and typed roots. The native initializer
authority suite checks the original AOT load bundle, reordered and foreign
evidence, and both x86-64 status ABIs. CLI tests exercise
IR and native paths with exact and one-below execution, preparation, and global
storage limits.
