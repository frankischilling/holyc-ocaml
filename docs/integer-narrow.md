# Narrow integer storage and signatures

`holyc run --format=json examples/integer-narrow.hc` returns I64 42 and captures
`42` without a newline in JIT and AOT. I8, I16, U16, I32 and U32 now share the
checked integer storage and direct-call pipeline with I64, U64 and U8.

## Stored values and register values

| Type | Object bytes | Stored read | Runtime word |
| --- | ---: | --- | --- |
| I8 | 1 | Sign-extend low 8 bits | I64 |
| I16 | 2 | Sign-extend low 16 bits | I64 |
| U16 | 2 | Zero-extend low 16 bits | U64 |
| I32 | 4 | Sign-extend low 32 bits | I64 |
| U32 | 4 | Zero-extend low 32 bits | U64 |

Generated primitive metadata supplies a shared width and signedness descriptor.
Normalization applies to automatic, global and static scalars, fixed arrays,
plain stores, update writeback, prepared images and publications. I8 persistent
string copies sign-extend high bytes while retaining the original owned source
bytes and terminator rules. Array strides and object bounds use declared widths;
static allocation rounds once per whole object. Padding is inaccessible.

Each scalar parameter occupies an eight-byte ABI slot and exposes one object
of its declared width. Both standalone and nested calls normalize at entry.
Returns preserve full register bits until a later store or parameter entry:

```c
I64 Read(I8 n){return n;}
Read(255); // I64 -1

I8 Wide(){return 255;}
Wide(); // I64 255
```

Plain assignments and the ten canonical compound opcodes retain full computed
register results while storing only the declared width. Prefix/postfix updates
return the normalized new/old stored value. Signed division, remainder and
right shift follow the destination class for compounds. An address is evaluated
once; RHS effects precede the old-cell read. Aliases retain object, activation
and lifetime identity. Arithmetic faults do not publish a new stored value.

## Native computation classes

Exact declared type, runtime result type and native producer class have
different roles. Storage loads and arithmetic forward public integer classes
to internal classes. Direct public U16/U32/U64 calls retain their public class
until unary fixup; public-spelled U8 already denotes an internal native class.
Unary minus selects a signed partner only for an originally internal unsigned
operand. It preserves all 64 value bits, without a narrow store.

```c
I64 F(){U16 n=84;I16 d=2;return -n/d;}
F(); // -42

U16 V(){return 84;}
I16 D(){return 2;}
I64 G(){return -V()/D();}
G(); // 9223372036854775766
```

Complement has an I64 stack/result type but retains its operand's forwarded
native node class. Subsequent unary, binary and comparison operations use that
class. For example, `I64 F(){U16 n=84,d=2;return (~n)/d;}F();` returns
9223372036854775765. Ordinary binary classes use the greater raw ID; comparisons
use their separate unsigned-operand rule. Grouping and unary plus preserve
producer evidence; an admitted explicit word cast resets it.

The VM derives computation classes from the checked instruction producers while
retaining exact storage/call targets. This also applies to bare expression
execution. Unary folding keeps an unsigned complement intact when a plain
immediate cannot retain both its I64 result type and unsigned node class.
Malformed forms, widths, flags and payloads fail preflight before effects.

## Declaration initializer proof

Native register allocation can retain a wider value in an automatic narrow
local. Initializer compatibility therefore checks all narrow reads across the
initializer and its transitive callees. Ordinary parameter entry seeds declared
bounds; every subsequent direct assignment must fit the destination's signed or
unsigned range. Direct updates, unproved cycles and explicit registers do not
supply bounded register-read evidence. A narrow return declaration does not
prove a bounded call result. Explicit noreg and proven memory paths remain
available.

The proof keeps constants, intervals and physical widths separate. Signed AND
with a positive mask and signed minimum divided by minus one can expose full
results different from storage. A terminal destination no wider than the updated
object can erase that difference, but a wider sink cannot. Such an exemption
does not manufacture bounds for a later expression. Discarded bit operations
are checked against physical bit positions, including results hidden by identity
folding. Unproved native paths retain HCRUN0006 with the original initializer
owner and span.

## Maintained fixture and limits

The fixture uses signed copied bytes, I16 array preparation, I8 parameter entry,
a U16 global and pointer, an I32 static, a full U32 return, narrowing stores and
captured Print output.

| Resource | Exact limit |
| --- | ---: |
| Runtime instructions | 139 |
| Preparation units | 9 |
| Persistent bytes | 16 |
| Active frame bytes | 40 |
| Call depth | 2 |
| Literal bytes | 3 |
| Output bytes | 2 |
| Output-work units | 5 |

The 40 active frame bytes include Result's 16-byte allocation and Print's
24-byte argument allocation. Global objects use eight bytes; the I32 static
uses eight padded bytes. All exact limits pass together. Each one-below limit
has an API/CLI diagnostic and capture assertion. Runtime step exhaustion at 138
retains capture `3432`; the other fixture failures retain empty capture. Repeated
execution starts with fresh persistent, parameter and literal cells. All eight
original #633 gates return I64 42 without output. Report v1/v2 and dump schemas
remain unchanged.

## Source and remaining boundaries

Reference: `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`BackLib.HC:281-309,509-534,550-572` defines width-specific memory operations and
full register transport. `OptPass789A.HC:710-717,779-782,1026-1030` covers parameter
entry and returns. `PrsVar.HC:132-134` permits I8/U8 persistent string copies.
`PrsExp.HC:116,149-161,586`, `OptLib.HC:9-14,101-122,196-202` and
`OptPass012.HC:151-191` establish class forwarding, complement and negation.
`KernelA.HH:1566-1571` gives narrow raw ranks. The update and register-path
evidence remains documented in [byte updates](integer-byte-updates.md).
These are pinned-source audits and hosted tests, without a new native capture.

Bool, zero-sized and floating storage, aggregates, pointer returns, general
pointer conversions and narrow word-cast/index producers remain outside this
execution slice. Automatic brace/string array initialization does not follow
the pinned persistent initializer parser and remains explicitly unsupported.
Full formatting/runtime linking, broader memory, stateful compilation/#exe,
optimizer parity, native backends, TempleOS BIN/loader acceptance and bootstrap
remain required for the complete compiler.
