# Global callback initializers

The IR source runner initializes one-star global callback cells and arrays with
integer words, original checked function addresses, callback copies and supported
integer expressions. JIT and AOT retain the initializer's original declaration,
leaf order and eight-byte physical elements. Return metadata, including narrow
integers, F64, U0 and return pointers, does not change that storage width.

```sh
holyc run --target=ir --mode=jit examples/global-callback-initializers.hc
holyc run --target=ir --mode=aot examples/global-callback-initializers.hc
holyc run --target=host-jit --mode=jit examples/native-callback-initializers.hc
holyc run --target=host-jit --mode=aot examples/native-callback-initializers.hc
```

The IR example returns 42 and writes `A` once while initializing `Word`.
Its array copies the first element's original executable into the second.
Overwriting the first element does not revoke the second element's ownership.
JIT task inputs also retain original bodies across later same-name definitions.
Initializer calls use their selected callback header, capture the callee before
reverse arguments and preserve reached effects on null, numeric or signature
faults.

The streaming declaration path validates the original parser publication and
initializer fragment before the completed type record exists. Its destination
keeps the original declaration, retained allocation, checked array coordinates
and complete initializer region. A callback call must belong physically to that
region's original expression subtree. Nested direct-call arguments retain the
same source ownership. Copied instructions, foreign contexts or matching names
cannot replace those records.

The task view also retains the original completed anonymous header during the
open declaration. Earlier initialized elements can be copied or called, including
their saved defaults, while later leaves are still being parsed. Copies preserve
the selected executable after the original cell is overwritten. JIT reads of
unwritten elements retain `HCIRVM0012`; AOT uses its zeroed global storage.
Missing or foreign headers and substituted signature metadata fail before
storage admission.

JIT function-address producers stay scheduled until the original executable is
available. They cannot be folded into a numeric constant. Callback copies retain
opaque executable identity; integer initializers retain their complete numeric
bits and acquire no executable authority. Numeric invocation reports
`HCIRVM0024` after argument effects.

Native execution currently accepts closed numeric global callback initializers.
Each successful scalar leaf consumes its original parser preparation and charged
completion before the complete callable bundle is sealed. Full-word preparation
is independent of the callback's return type. The native example returns 42,
uses sixteen global bytes, and shares nine preparation steps with its saved
callback parameter default. That default occupies eight saved bytes. Its F64
callback metadata does not require a floating-point invocation.

Tests compare fresh public IR, separately executed checked batch IR and native
values and runtime work in both modes. They cover full bits, arrays, calling
flags, reached faults, exact and one-below limits, quota recovery, both image
ABIs, and missing, reordered, duplicate or foreign preparation evidence.
The maintained CLI also replays the initialized global value from the existing
TempleOS callback fixture; its two native repeats agree. That capture is JIT
evidence and does not establish TempleOS AOT execution.

The pinned audit follows `Compiler/PrsStmt.HC:209-458` for global publication and
physical allocation, `Compiler/PrsVar.HC:50-158,205-246` for initializer execution
and AOT scheduling, and `Compiler/PrsVar.HC:286-378` for separate callback storage
and return metadata, at commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Demo/Graphics/Grid.HC:13` preserves the direct automatic callback initializer
syntax boundary. No new TempleOS capture or exported ABI proof is claimed.

Native owned-code and effectful initializers, member storage, callback updates,
multistar consumers, live native replacement/linking, general F64/aggregate
execution and the full compiler remain unfinished. Direct automatic and static
callback initializers continue to report `HCPARSE0137`.
