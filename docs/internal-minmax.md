# Signed and unsigned Min/Max internal calls

Issue #763 connects the four pinned two-argument integer operations to public
IR and hosted native execution. `MinI64` and `MaxI64` compare signed full words;
`MinU64` and `MaxU64` compare unsigned full words. The selected result retains
its complete bits and the declaration's I64/U64 class. Computed narrow return
words stay distinct from declared-width storage.

```text
holyc run --target=ir --mode=jit --format=json examples/internal-minmax.hc
holyc run --target=host-jit --mode=aot --format=json examples/internal-minmax.hc
holyc run --mode=aot --format=json examples/stream-internal-minmax.hc
```

The ordinary example captures `42:8000000000000000;` and returns I64 42 in both
modes and targets. IR JIT execution includes the original binding targets and
declaration work: 95 runtime steps and twelve preparation steps. AOT and closed
native execution use 76 runtime steps and no preparation. Both use 27
formatting-work units. The retained example evaluates both effectful
arguments in its original default callback, saves the selected word and reuses
it after a later write. It generates `42;`, returns I64 42 and captures no output.
Both modes use 132 runtime instructions, fifteen preparation instructions and
seven formatting-work units.

## Original calls, argument order and limits

The original numeric binding, publication and exact two-parameter signature
select the operation. Renamed numeric declarations work; ordinary same-name
functions execute their own bodies. The canonical sequence is `IC_CALL_START`,
the two unpushed argument computations in right-to-left order, the selected
internal opcode with operands in parameter order, and `IC_CALL_END`. Each sealed
argument retains its original producer, source origin, type and parameter role.
Foreign contexts or changed operands, order, result, type, flags, payload or
opcode reject before execution.

Each internal operation charges one ordinary instruction tick. Argument
computations and call markers retain their own work, and the internal operation
needs no ordinary callee activation or stack cleanup. Native private result
staging, code bytes, active frames and named-call depth keep their existing
quotas. Reached output and work survive an argument or instruction-limit fault.
Exact limits pass; one-below runtime, preparation, native code and native frame
limits reject in the maintained controls.

Retained scalar defaults schedule these operations in their owning source
activation. Runtime support does not implement the source optimizer's folding
rewrite or widen ordinary closed preparation. Native closed defaults reject
these calls with HCRUN0006. Native retained frontend execution remains under
#704; the CLI reports HCPP0008 for a retained example on that target.

## Source evidence and verification

The reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Kernel/KernelB.HH:99-102` declares the signatures and
`Compiler/CompilerA.HH:223-226` fixes their numeric identities:
MinI64 0xAB, MinU64 0xAC, MaxI64 0xAD and MaxU64 0xAE.
`Compiler/PrsExp.HC:440-586` supplies argument ordering and internal call emission.
`Compiler/OptPass789A.HC:867-878` chooses signed G/L or unsigned A/B conditions;
`Compiler/BackC.HC:418-454` performs full-word comparison and result selection.

Tests use independent expected words for extrema, equality, reversed operands
and high bits, plus nested and recursive callers, effects, faults, stored and
computed narrowing, original retained defaults and public CLI examples. Native
tests compile both ABIs and repeat fresh host images. These are source-derived
and hosted execution results; no new TempleOS oracle capture is included. Other
internals, floating execution, broader preparation, native retained publication,
the full ABI, artifacts, actual loader and bootstrap remain open under #695,
#689, #685, #701, #704, #702 and #682.
