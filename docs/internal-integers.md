# Unary integer internal calls

Issue #761 connects the pinned unary integer internal operations to public IR
and hosted native execution. Original numeric declarations select the operation;
a matching function name cannot authorize it.

| Operation | Fixed parameter | Result | Full-word behavior |
| --- | --- | --- | --- |
| `ToBool` | I64 | Bool/U8 | Zero becomes 0; every nonzero word becomes 1. |
| `AbsI64` | I64 | I64 | Negative words are negated; signed minimum retains its bits. |
| `SignI64` | I64 | I64 | Negative, zero and positive words return -1, 0 and 1. |
| `SqrI64` | I64 | I64 | Retains the low 64 bits of the square. |
| `SqrU64` | U64 | U64 | Retains the low 64 bits of the square. |

Squares wrap without an arithmetic fault. This increment does not expose the
high product word or processor-flag services. Boolean conversion tests the
complete supplied word: `ToBool(0x100)` returns 1. Computed narrow return values
keep their full register bits; declared-width storage still narrows its writes.
The public result uses the existing I64/U64 report classes, including unsigned
U64 for a Bool/U8 call result.

```text
holyc run --target=ir --mode=jit --format=json examples/internal-integers.hc
holyc run --target=host-jit --mode=aot --format=json examples/internal-integers.hc
holyc run --mode=aot --format=json examples/stream-internal-integers.hc
```

The ordinary example captures `42:1;` and returns I64 42 on both targets and in
both source modes. IR JIT execution now includes the original binding targets
and declaration work: 104 runtime steps and fifteen preparation steps. AOT and
closed native execution use 81 runtime steps and no preparation. Both use twelve
formatting-work units. The retained example evaluates an effectful
argument once in its original default callback, saves the absolute value and
reuses it after a later write. It generates the outer expression `42;`, returns
I64 42 and captures no output. Both outer modes use 113 runtime instructions,
eighteen preparation instructions and seven formatting-work units. Exact limits
pass; a runtime allowance one below the example's count fails.

## Original calls and preparation

Calls preserve their original numeric binding, publication, exact signature,
argument producer and source phase. The canonical sequence is `IC_CALL_START`,
the unpushed argument computation, the selected operand-bearing internal opcode,
and `IC_CALL_END` with the declared result. It needs no ordinary callee frame or
stack cleanup. Renamed numeric declarations work; ordinary same-name functions
execute their own bodies. Nested calls, computed arguments and retained generated
source reuse the existing checked call context. Foreign contexts or changed
opcodes, operands, flags, payloads and result classes reject before execution.

Each internal operation charges one ordinary instruction tick. Original argument
computation and call markers have their own ticks. Private native result staging,
code bytes, active frames and named-call depth keep their existing quotas. Reached
output and work survive a later argument or instruction-limit fault.

Runtime support does not implement the source optimizer's constant-folding
rewrite. Checked internal scopes remain scheduled in retained preparation,
including literal `ToBool` calls. Native defaults and initializers retain their
closed preparation gate and reject these calls with HCRUN0006. The separate
`OptPass012.HC:1069-1088` ToBool rewrite and broader preparation remain open.

## Source evidence

The reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Kernel/KernelB.HH:95-121` declares the signatures;
`Compiler/CompilerA.HH:51,221-229` fixes their numeric IC identities.
`Compiler/PrsExp.HC:440-586` supplies internal argument and call emission.
`Compiler/OptPass789A.HC:820-824,835-837,859-865,881-886` selects sign, Boolean,
absolute and square code. `Compiler/Templates.HC:101-108` branches on the signed
word for SignI64. `Compiler/BackB.HC:289-303` tests the full word for ToBool, and
`Compiler/BackC.HC:456-462` selects the square multiplication.

Tests use independent expected words, both source modes, both native ABI
compilations, repeated images, original retained defaults, public CLI programs,
exact/one-below budgets and foreign or changed call evidence. They add no
TempleOS oracle capture. Floating internals, other numeric and runtime services,
native retained frontend execution, artifact/loader support and compiler
completion remain open under #689, #695, #701, #704 and #682.
