# Full-word Bsf and Bsr internal calls

Issue #765 connects the pinned bit scans to public IR and hosted native
execution. `Bsf` returns the lowest set-bit index; `Bsr` returns the highest.
Both examine all 64 bits and return I64 -1 for a zero word. Computed narrow
return words retain their full bits; declared-width storage still narrows writes.

```text
holyc run --target=ir --mode=jit --format=json examples/internal-bitscan.hc
holyc run --target=host-jit --mode=aot --format=json examples/internal-bitscan.hc
holyc run --mode=aot --format=json examples/stream-internal-bitscan.hc
```

The ordinary example captures `42:-1;` and returns I64 42 in both modes and
targets. IR JIT execution includes the original binding targets and declaration
work: 56 runtime steps and six preparation steps. AOT and closed native
execution use 45 runtime steps and no preparation. Both use thirteen
formatting-work units. The retained example evaluates an effectful
argument once in its original default callback and reuses the saved index after
a later write. It generates `42;`, returns I64 42 and captures no output. Both
modes use 86 runtime instructions, nine preparation instructions and seven
formatting-work units.

## Original calls, encoder forms and limits

Original numeric bindings 0x7E/0x7F, publication, exact I64 signatures and source
call phases select the operations. Each argument retains its original producer,
type and source origin. Renamed numeric declarations work; ordinary same-name
functions execute their bodies. The canonical sequence uses `IC_CALL_START`,
unpushed argument computation, operand-bearing `IC_BSF`/`IC_BSR`, and
`IC_CALL_END`. Foreign contexts and changed operand/result/type/flags/payload
or numeric operation reject before execution.

The interpreter scans at most 64 positions. Native code uses the project's
checked qword BSF/BSR encoder forms and handles a zero input explicitly before
the scan. Each operation charges one ordinary IC tick. Argument computations
and call markers retain their work; no ordinary callee activation or cleanup is
needed for the internal operation. Private native result staging, code bytes,
active frames and named-call depth keep their existing quotas. Reached output
and work survive a later argument or instruction-limit fault.

Retained scalar defaults schedule the original calls in their owning source
activation. Runtime admission does not implement the separate immediate-folding
rewrite or widen ordinary closed preparation. Native closed defaults report
HCRUN0006 for these calls. Existing declaration-time shift restrictions also
remain; retained setup masks in the example use literal words. Native retained
frontend execution remains under #704 and reports HCPP0008 for that example.

## Source evidence and verification

The reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Kernel/KernelB.HH:9-12` defines the signatures and zero sentinel;
`Compiler/CompilerA.HH:169-170` fixes the numeric identities.
`Compiler/PrsExp.HC:440-586` supplies internal call emission.
`Compiler/OptPass789A.HC:806-819` emits scans and zero-result handling.
`Compiler/OpCodes.DD:724-731` supplies the encoder forms, including the qword
R64,RM64 forms at 727/731. `Compiler/Asm.HC:959-966` consumes both scans for
alignment. `Compiler/OptPass012.HC:333-356,426-428,451,832-853` uses them in
power-of-two rules; its distinct immediate rewrite is at 912-933.

Tests cover independent zero/high/mixed/all-ones words, each input bit, stored
versus computed narrowing, nested Min/Max calls, recursive/effectful callers,
original retained defaults and public CLI examples. Independent encoder bytes
cover both ModRM directions and extended registers. Native tests compile both
ABIs and repeat fresh host images. Runtime, preparation, code and frame limits
have exact/one-below controls. These are source-derived and hosted results;
no new TempleOS oracle capture is included. Broader runtime, folding, F64,
native retained publication, full ABI/artifact/loader and bootstrap work remains
open under #695, #685, #689, #701, #704, #702 and #682.
