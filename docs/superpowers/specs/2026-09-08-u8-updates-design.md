# U8 update execution design

Issue #629 / Section 61 extends the existing update pipeline after #627. The
reference stays `c26482bb6ad3f80106d28504ec5db3c6a360732c`; the baseline is merged
`f5b0e8f55e042fcb04f519b8c077fe36fa59e48a`. All eight maintained continuation
probes fail HCRUN0003 in both modes on that revision and its tested source.

## Execution contract

Keep the existing canonical reference update path: flags zero, one checked
address operand, an optional checked integer RHS, a materialized result, no
payload, and an exact destination type. These are raw `runtime-ir` instructions;
there is no BY_VAL, result-memory, discarded-result or register-allocation pass.
The existing address plans evaluate the destination before the RHS and retain
the original object across calls. Execution reads its current value after RHS
effects. All 14 existing update opcodes retain their identities and barriers.

Extend checked update types and VM validation from I64/U64 to U8. Preserve the
storage class separately from register result policy. For a byte, compound
operations compute with the existing unsigned 64-bit arithmetic, store the low
eight bits and return the full computed register. Prefix updates store the new
low byte and return its zero-extended value; postfix returns the old byte. Word
updates preserve their current behavior. Faults perform no update store and
retain earlier RHS effects. Do not add a fabricated RHS instruction for ++/--.

This raw path is supported by non-BY_VAL memory branches in BackA.HC:555-570,
325-352,393-421,637-657; memory prefix/postfix branches in BackB.HC:323-341,
364-380; and byte/move behavior in BackLib.HC:291-296,509-535. It is not proof of
which native optimized path an arbitrary source expression selects.

## Native distinctions and initializer proof

BY_VAL plus an immediate memory RHS can reread a narrowed compound result
(BackA.HC:448-454). Register destinations and general computed-register paths
differ. OptPass4.HC:389-397 can also redirect a result to destination memory;
constant-vs-runtime RHS syntax alone is not sufficient evidence. Native narrow
automatic scalars can become 64-bit register variables (OptPass3.HC:536-610),
including the pinned warning about raw-type changes. Static locals explicitly
use REG_NONE (PrsVar.HC:492-494). Discarded immediate bitwise updates can become
word-sized bit operations (BackC.HC:539-600), including access beyond one byte.

The existing initializer arithmetic guard must therefore gain byte-specific
proof, including its transitive callee traversal. Raw flags zero alone cannot
authorize native initializer values. Build a small internal analysis over the
original graph and checked storage/frame evidence. It must distinguish proven
memory destinations from direct automatic scalar locations that native register
allocation can change. References, persistent objects and arrays remain tied to
their original owners. Retain explicit noreg evidence where available; an
unknown destination or register-selection fact is not a proof.

For proven memory destinations, admit operations whose observable values are
path-independent: memory prefix/postfix; range-safe compounds; a compound that
directly supplies a terminal U8 declaration leaf; or an actually discarded
compound whose storage effect is invariant. Never infer the old value merely
from its declaration initializer, since aliases and RHS calls can change it.
Value/range facts must describe the update's read after RHS effects. Do not use
the public U8 result type alone to infer a byte-range register value: plain
assignment and raw compounds can retain full bits.

Conservative range-safe families for an unknown current memory byte include
unsigned dynamic /= and %= (preserving the existing constant-divisor guard),
&=, |=/^= with a proved byte-range RHS, *= by zero/one and +=/-= by zero.
Broader cases require a sound range proof. Terminal-byte erasure is valid only
when the original result directly supplies that leaf; it cannot pass through
arbitrary arithmetic or assignment register results. A discarded result does
not erase native bit-operation side effects: reject a provable or unresolved
outside-byte bit optimization when its source shape could select that path.
Preserve all existing initializer shift/constant-divisor guards and attach
HCRUN0006 with the original owner, source span and specific missing proof.

Review strengthened two requirements. Apparent result uses can disappear under
native identity folding, so hazardous bit updates need a positive observation
sink (declaration, return or pushed argument, optionally through an audited
bit-preserving cast). Also prove native agreement for every byte read across
transitive callees, including operands, helper results and control flow. Ordinary
register candidates may read a proved byte range only when every direct store
has a bounded RHS and no direct update targets the same location. A conservative
fixed point admits dependencies; unproved cycles remain rejected. Explicit
hardware registers cannot use this exception, and their byte addresses cannot
escape into an apparently proven pointer-memory update.

## Validation and coverage

Keep existing scalar/array/literal/pointer bounds, exact target types and
source-owned initializer regions. Byte references cannot reach neighbors,
static padding or expired frames. Canonical validation rejects BY_VAL,
RES_NOT_USED, locked flags, missing results/payloads and numeric pseudo-addresses.
No new runtime instruction, storage allocation or dependency is required.

Tests cover all eight gates, all compound opcodes, every storage owner,
immediate/runtime RHS, used/discarded results, all four wrap forms, aliases,
once-only destinations, right-to-left calls, recursion, fresh replay and fault
effects. Initializer tests pair admitted invariant cases with rejected exposed
word, register-local and neighboring-bit cases, including transitive helpers.
Add one CLI example, measure its exact counters and test one-below bounds,
deterministic dumps and v1/v2 reporting. Full packaging, checksum, provenance,
corpus and five-check CI gates remain required before normal protected merge.

## Architecture choices

Reuse the shared update instructions and checked references. A blanket byte
store adaptation would give prefix the wrong overflow result. Rejecting every
byte compound would discard a supported raw path and invariant initializer
cases. The selected design separates storage width, raw result policy and
native initializer proof without replacing the existing pipeline.

This is an architectural extension of the checked execution contract, authorized
by the continuing implementation mission. Full compiler work beyond #629 stays
active: broader storage/memory, formatting/runtime linking, stateful compilation,
optimizer parity, native backends, BIN/loader acceptance and bootstrap.
