# U8 numeric parameters and returns

[Narrow integers](integer-narrow.md) in #633 extend these rules to
I8/I16/U16/I32/U32 and repair unary/complement producer-class handling.

`holyc run --format=json examples/integer-byte-signatures.hc` returns I64 42
and captures `42` without a newline in JIT and AOT. U8 parameters and return
values now use the existing checked function, frame and direct-call pipeline.
The eight original #631 gates also return 42 with their declared runtime
classes and empty capture.

## Entry and return behavior

Each U8 parameter receives the low byte of its supplied argument. The parameter
has one accessible byte, initialized even when the argument is zero, inside an
eight-byte ABI slot. Loads zero-extend the byte; padding and adjacent slots
remain inaccessible. All eight ABI bytes count toward active frame limits.
This applies to `execute_function` and every nested call, including mixed
word, byte and pointer signatures. Arguments still execute right to left and
bind to their original formal positions.

```c
I64 Id(U8 n){return n;}
Id(554); // I64 42
```

A U8 return declaration does not narrow the register value. Return execution
and call completion preserve every bit and report runtime U64. Original
checked U8 types remain in the IR and exact call/return validation. A later
byte store or byte parameter entry narrows independently.

```c
U8 Wide(){return 554;}
Wide(); // U64 554
```

Passing `Wide()` to the preceding `Id` returns I64 42. Assigning it to an I64
local preserves 554; assigning to a U8 local stores 42. A returned -1 retains
all 64 bits and reports unsigned 18446744073709551615. Missing required U8
returns retain HCIRVM0013, including after earlier captured output.

The function-return classifier is separate from the existing word-only
storage/expression classifier. U8 remains byte storage; this change does not
expand unary, index, cast or other unsupported expression matrices.

## Initializers and native evidence

Reference: `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

`OptPass789A.HC:710-717` initializes positive-offset register parameters using
their declared raw memory class. `BackLib.HC:528-530` uses MOVZX for U8 memory,
so ordinary register-allocated byte parameters also begin narrowed.
`PrsExp.HC:468-483` and `BackLib.HC:312-347` preserve supplied integer bits in
eight-byte argument pushes. Narrowing is justified by parameter entry loads.

`OptPass3.HC:249-258` adds integer/F64 return conversion where needed, without
a general byte truncation. `OptPass789A.HC:779-782,1026-1030` transports return
and call-result values through I64 RAX. This supports the full-bit result rule
for canonical materialized register execution. No new native capture or
complete register-allocation/optimizer parity is claimed.

The [byte-update initializer proof](integer-byte-updates.md) now seeds exact
ordinary U8 named parameters in each bounded-write scan. Every direct write
must remain byte-bounded and any direct update disqualifies the candidate.
Every byte read is then checked throughout the initializer and its transitive
callees. Unknown cycles and explicit hardware registers remain excluded.
Readonly `Id(554)` therefore qualifies, while an ordinary parameter later
assigned 554 or incremented through an unproved register path retains HCRUN0006.

Actual ordinary address escape can force native memory, and indirect byte
stores narrow there. Direct `*(&n)=554` retains exact candidate identity and
the conservative wide-write rejection. Explicit-register escape still fails.
The analysis does not infer a byte range from a U8 call-result declaration.
For example, storing `Wide()` into an automatic byte may still retain wider
bits in a native register, so using that local in a scheduled initializer
requires additional proof. noreg memory controls retain their existing support.

Preparation barriers, original declaration leaves, exact scheduled regions,
source ownership, transitive calls and JIT/AOT publication phases are unchanged.

## Maintained fixture and limits

The example combines a scheduled U8 initializer, byte and pointer arguments,
a full-width U8 return, persistent bytes, local stores and captured Print.

| Resource | Exact limit |
| --- | ---: |
| Runtime instructions | 110 |
| Preparation units | 7 |
| Persistent bytes | 12 |
| Active frame bytes | 32 |
| Call depth | 2 |
| Literal bytes | 3 |
| Output bytes | 2 |
| Output-work units | 8 |

The 32 active bytes include Result's eight-byte local allocation and Print's
24 ABI bytes, including its hidden argument count. The twelve persistent
bytes comprise Seed, Text and Calls' whole-static allocation. The scheduled
Seed call executes at runtime; preparation charges the copied Text bytes and
the constant static initializer.

All exact limits pass together. Each one-below limit has an API and CLI
diagnostic/capture assertion. Step exhaustion at 109 preserves `output_hex=3432`
(captured text `42`); the
other one-below fixture failures occur before output and retain empty capture.
Recursive U8 parameters separately verify eight-byte per-call charges and
depth exhaustion after prior successful output. Repeated execution starts from
fresh parameter, persistent and literal cells.

CLI v1/v2 reporting and dump schemas remain unchanged. Tests retain exact U8
call/return types, argument cleanup sizes, source ownership, malformed-IR
preflight, aliases, neighbors, full high-bit values and missing returns.

Other primitive/aggregate storage, pointer returns, general pointer operations,
full formatting/runtime linking, stateful compilation/#exe, optimizer parity,
native backends, TempleOS BIN/loader acceptance and bootstrap remain unfinished.
