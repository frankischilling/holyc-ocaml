# Narrow integer storage and signatures

Issue #633 completes numeric storage admission for I8, I16, U16, I32 and U32.
The reference remains `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
The baseline reproduced failures for all eight Section 63 programs in both
execution modes; the implementation plan records subsequent verification.

## Storage and transport

Reuse generated primitive metadata through an abstract integer scalar
descriptor. It provides width, signedness, normalization and value bounds.
Only nonzero integer primitives qualify; Bool, pointers and aggregates do not.
Public spelling remains required where persistent storage already requires it.

Every store, initial image, prepared array overlay and parameter entry stores
the low declared bits and sign- or zero-extends reads. Narrow signed values
use runtime I64; unsigned values use U64. Parameters retain eight-byte ABI
allocations and one declared-width object. Returns transport the full register
value, including values outside their declared narrow range. Assignment and
compound results also preserve the full computed value; prefix/postfix results
use normalized storage values. Pointer identity, ownership, lifetime, strides,
padding and exact source/IR joins remain enforced.

Persistent fixed arrays use the same descriptor, including I8 string copies.
Automatic brace/string initialization remains outside the pinned parser's
producer path; this change does not introduce C array initialization.

## Native initializer proof

Generalize the byte proof using constant and interval evidence, while keeping
value ranges separate from physical bit widths. Ordinary register locals need
whole-graph proof that every write fits the destination. Parameter entry starts
with declared bounds. Calls, explicit registers, unknown cycles and direct
updates do not manufacture bounded register-read evidence.

Memory compound updates require proof that native full results and stored
results agree, or an exact terminal sink no wider than the updated object.
Signed AND with a positive mask and signed minimum divided by minus one are
counterexamples to the old unsigned-byte rules. Discarded native bit-operation
hazards inspect bit positions above the physical object width. Sink exemptions
must never manufacture result bounds for a subsequent expression.

## Computation classes

Storage type identity and effective native computation class are distinct.
The pinned parser forwards public integer storage loads and ordinary arithmetic
through their internal classes. Direct public U16/U32/U64 call ends retain the
public return class until unary fixup. Unary minus inspects the original
operand class, so negating a public unsigned storage read and negating a direct
public unsigned call can have different signed result classes. U8 is already
an internal native class. NOT forwarding also matters to nested minus.

Complement retains the forwarded operand node class while exposing an I64
stack/result type. Parent unary, binary and comparison operations consume node
class evidence. Apply this to bare expression execution as well as programs;
do not fold unsigned COM into an immediate that discards the distinct class.

Preserve exact storage/call type joins while carrying or deriving the effective
producer class for unary and binary results. Tests must distinguish storage,
arithmetic, direct public/internal calls and nested NOT. Unsupported producer
combinations must fail before effects rather than inherit a convenient type.

## Validation

Maintained API and CLI tests cover the eight original gates, each width and
signedness at every entry path, update result semantics, aliases, call ABI,
prepared initialization, unsafe native initializer counterexamples and forged
IR. A measured fixture exercises all eight resource counters with exact and
one-below limits in both modes, fresh images, prior output and report v1/v2.

Run the full suite, packaging/generated checks, 82 pinned checksums, eleven
provenance scenarios and exact lexer/parser corpora. Obtain independent review
and all five final-source checks before normal protected merge; verify merged
execution, tree identity, post-merge CI and synchronized Git afterward.
