# Canonical Bool execution

Issue #769 admits the pinned `Bool ToBool(I64)` internal declaration and Bool
objects, parameters and returns through the existing scalar execution paths.
The interpreter, retained source tasks and hosted native target retain Bool's
distinct public identity. Its audited backing is signed one-byte `I8i` storage.

```text
holyc run --mode=jit --format=json examples/canonical-bool.hc
holyc run --target=host-jit --mode=aot --format=json examples/canonical-bool.hc
holyc run --mode=aot --format=json examples/stream-canonical-bool.hc
```

The ordinary example captures `384:-128:1;` and returns I64 42. Both modes and
targets use 68 runtime instructions, no preparation and 21 formatting-work
units. The retained example evaluates the original ToBool default once,
resets the argument's effect counter, then reuses the saved value to generate
`42;`. Both IR modes return I64 42 without captured output, using 77 runtime
instructions, six preparation instructions and seven formatting-work units.

## Storage, computation and calls

Bool remains separate from I8 and U8 in checked type equality and call metadata.
`Primitive_type.integer_storage_info` derives its backing from the audited
storage spelling. Scalar storage and computation consumers use that backing;
they do not change Bool's Boolean category or create an internal Bool spelling.

A Bool store writes its low byte. A later read sign-extends that byte to I64.
Each fixed Bool parameter occupies the existing eight-byte argument slot but
exposes a one-byte object. Arrays use one-byte strides and original extents;
static objects keep their whole-object padding and persistent lifetime.
An assignment expression retains its full computed result, and ordinary Bool
returns retain all 64 register bits until a store or parameter entry.

```c
Bool Computed(){return 0x180;}
Computed(); // I64 384
Bool stored=0x180;
stored; // I64 -128
Bool zero=0x100;
zero; // I64 0
```

Storage does not automatically normalize truth. Only the explicit original
`_intern 0x1d Bool ToBool(I64 i)` call tests the complete computed argument
word and produces zero or one as I64. The previously supported U8 return
signature still produces U64. I8/I64 return substitutions, pointer signatures,
defaults, variadic parameters and foreign or changed call/producer records
remain rejected. Renamed original numeric declarations work; ordinary functions
called ToBool execute their own bodies.

Constant Bool defaults retain the original full saved word and normalize at
parameter entry. Public JIT defaults can activate separate declaration units,
so their whole-source runtime meter differs from the native batch meter.
Native tests compare against execution of the isolated original checked batch,
keeping preparation work and saved bytes separate. Retained ToBool defaults
execute in their original activation and preserve argument effects exactly once.
Native closed preparation still rejects ToBool calls with HCRUN0006; native
retained frontend execution reports HCPP0008 under #704.

## Source and verification

The reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/CInit.HC:1-15` gives Bool and I8 the same signed raw class and byte
size. `Kernel/KernelB.HH:119` gives the canonical signature and
`Compiler/CompilerA.HH:51` fixes opcode 0x1D.
`Compiler/PrsExp.HC:440-586` supplies original argument/call phases.
`Compiler/BackB.HC:289-303` tests the complete argument and emits SETNZ/MOVZX;
`Compiler/OptPass789A.HC:835-837` dispatches it. Width-specific moves and full
register transport are audited at `Compiler/BackLib.HC:281-309,509-534,550-572`.
`Compiler/PrsVar.HC:619-657` supplies eight-byte parameter slots and evaluated
saved defaults; `Compiler/OptPass789A.HC:779-782` preserves register returns.
The separate ToBool immediate rewrite at `Compiler/OptPass012.HC:1069-1088`
remains outside this runtime increment.

Tests cover all 64 input bits, computed versus stored values, signed arithmetic,
returns, parameters, locals, globals, prepared arrays, statics, aliases, updates,
defaults, nesting, recursion and argument effects. They also check exact
signatures, public type identity, original/foreign/changed/copied authority,
uninitialized reads, true one-byte bounds, reached output, both native ABIs,
fresh images and exact/one-below runtime, preparation, code and frame limits.
These are pinned-source expectations and hosted execution without a new
TempleOS oracle capture. Floating/zero-sized storage, aggregates, general
pointer conversions, full runtime, optimizer parity, native retained execution,
artifact/loader acceptance and bootstrap remain open.
