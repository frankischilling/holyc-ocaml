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
holyc run --target=host-jit --mode=aot examples/native-aot-callback-initializers.hc
```

The IR example returns 42 and writes `A` once while initializing `Word`.
Its array copies the first element's original executable into the second.
Overwriting the first element does not revoke the second element's ownership.
JIT task inputs and AOT programs retain original bodies across later same-name
definitions. AOT may reuse one canonical callable record, while each checked
definition keeps its own body, frame and declaration. Earlier callback addresses
and direct calls execute their selected original definition. Repeated bodies,
foreign canonical records and substituted frames still fail preflight.
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

Native execution accepts closed numeric global callback initializers in both modes.
Each successful scalar leaf consumes its original parser preparation and charged
completion before the complete callable bundle is sealed. Full-word preparation
is independent of the callback's return type. The native example returns 42,
uses sixteen global bytes, and shares nine preparation steps with its saved
callback parameter default. That default occupies eight saved bytes. Its F64
callback metadata does not require a floating-point invocation.

Native AOT execution also consumes the original checked load-time regions for
function addresses, callback copies and supported direct or indirect calls.
Each source receipt retains its original declaration, leaf and checked storage
destination. It supplies no prepared value or executable address. Completion
joins those receipts, in order, to the exact load regions and callable bundle.
The image starts with AOT zero storage and any closed prepared leaves; generated
code then executes the scheduled stores before ordinary entry statements.
Private callback owners preserve the selected original executable across copies.
Calls, argument effects and reached faults consume runtime work.

The AOT example returns 42, prints `A`, uses 24 global bytes and executes 86
runtime steps. Only its anonymous saved default consumes preparation work:
three steps and eight saved bytes. The example covers an earlier-element copy,
an indirect call using that default, and a direct initializer call storing a
numeric callback word. A later same-name definition does not replace the body
selected by either copied callback. Every execution starts with fresh storage.

Global initializer start, leaf and delimiter receipts require their exact
private parser identity and the current context at the top of the source stack.
Clones, suspended parent callbacks and expired receipts cannot prepare a leaf.
Missing, repeated, reordered or foreign load receipts cannot authorize an image.

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

`Compiler/PrsStmt.HC:67-143` distinguishes AOT function-record reuse from ordinary
JIT replacement. A reused AOT record rebuilds its header but keeps the existing
stored cleanup flags: `fsp_flags` are applied only when creating a new record.
Calls use those checked flags, including when a later definition spells a
different modifier. A callback header that disagrees still faults after reached
argument effects. Public IR, isolated batch IR and native tests cover separate
old/new bodies, parameter counts, selected defaults, recursion and persistent
static storage. CLI tests check exact runtime limits and recovery for each case.

Native JIT initializers with references still require execution during their
original parser callback. They currently report `HCRUN0006`. Static initializers
and saved defaults retain their closed-expression boundary. The native AOT
consumer does not establish live native replacement or task linking.

Member storage, callback updates,
multistar consumers, live native replacement/linking, general F64/aggregate
execution and the full compiler remain unfinished. Direct automatic and static
callback initializers continue to report `HCPARSE0137`.
