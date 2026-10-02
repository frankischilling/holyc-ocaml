# Owned pointer bit calls

Issue #771 connects the plain numeric Bt, Bts, Btr and Btc declarations to IR,
retained source tasks and hosted native execution. Each call returns the prior
bit as I64 zero or one. Bts sets it, Btr resets it, Btc complements it and Bt
leaves the original object unchanged.

```text
holyc run --mode=jit --format=json examples/pointer-bit-internals.hc
holyc run --target=host-jit --mode=aot --format=json examples/pointer-bit-internals.hc
holyc run --mode=aot --format=json examples/stream-pointer-bit-internals.hc
```

The ordinary example captures `0:1:1:0:-128;` and returns I64 42. It selects
bit 70 in a two-word array and bit 7 in a Bool object. Both modes and targets
use 125 runtime instructions, no preparation and 29 formatting-work units.
The retained example prepares a mutating default once, checks its original
effects and reuses the saved prior bit after another object write. Both IR
modes return I64 42 without captured output, using 143 runtime instructions,
18 preparation instructions and seven formatting-work units.

## Original calls and owned storage

Only the original numeric targets 0x77, 0x78, 0x79 and 0x7A with exact
`Bool(U8*,I64)` signatures enter this path. Both provided arguments retain
their original producers, types, source origins and parameter roles. The
index executes before the pointer expression. Original call phases and
immutable producer records authorize execution; copied records, foreign
contexts and changed instructions reject. Renamed numeric declarations work;
ordinary functions with the same spelling execute their own bodies.

The pointer may identify a live owned Bool or nonzero scalar integer object,
array or literal. This admission is specific to these calls. The formal U8
pointer does not replace the original pointee type, width, extent, offset,
initialization state or lifetime. Copied aliases and interior references
continue to identify the original object.

A nonnegative index selects its byte relative to the original pointer offset,
then its bit within the original storage cell. Bits follow the existing
little-endian byte layout. Bounds are checked before the selected cell's
initialization. Mutations preserve other bits and use the original declared
width; a later signed narrow load sign-extends. Bool bounds expose one byte
even when its parameter occupies an eight-byte ABI slot. Negative indexes,
one-past accesses and selected bytes beyond the original extent fault with
HCIRVM0019. Unknown selected cells fault with HCIRVM0012. Earlier argument
effects, reached output and attempted work remain visible.

Each bit operation consumes one IC tick. Native code checks the original
descriptor, selects the cell and initialization flag, loads its declared
width, executes the qword register bit form, captures carry with SETC and
stores mutable results at that same width. Private staging, code, frame,
storage, call, output and preparation limits retain their existing controls.
Retained defaults execute the actual original call once. Native closed
internal-call preparation reports HCRUN0006; native retained frontend
execution reports HCPP0008 under #704.

## Source and verification

The reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Kernel/KernelB.HH:13-20` supplies the four plain signatures and
`Compiler/CompilerA.HH:162-165` their numeric targets.
`Compiler/PrsExp.HC:440-586` supplies argument and internal call phases.
`Compiler/OptPass789A.HC:787-804` dispatches them.
`Compiler/BackB.HC:202-264` distinguishes pointed memory, BY_VAL and LOCK,
and returns the prior bit through carry and SETC. Qword register forms occur
in `Compiler/OpCodes.DD:738,748,758,768`.
`Compiler/Asm.HC:397-423` tests I64 argument masks;
`Compiler/PrsLib.HC:88,232-243,309` and `Compiler/OptLib.HC:539-553` supply
compiler flag callers. Aggregate flag owners remain under #700.

Tests cover every bit in a word, zero, mixed and all-ones words, all nine
integer/Bool storage types and array widths, aliases, literals, persistent cells,
narrow computed versus stored indexes, nested and recursive calls and
right-to-left effects. Both modes and targets, original/foreign/changed/copied
authority, independent encoder bytes, faults, both ABIs, fresh images and
exact/one-below runtime, preparation, code and frame limits are checked.

This bounded hosted path preserves bit results and original owned storage.
It does not reproduce flat-address memory access, source immediate/BY_VAL
or branch rewrites, or concurrent atomic behavior. Locked targets 0x7B-0x7D
remain under #693/#695, optimizer parity under #696/#697 and broader
declaration preparation under #685. General pointer
casts, escapes and deeper pointers remain under #687/#699; aggregate storage
under #700; native retained publication under #704; full ABI, artifact,
BIN/loader and bootstrap acceptance under #702/#682. Expectations come from
the pinned source and hosted execution; no new TempleOS oracle capture is
included.
