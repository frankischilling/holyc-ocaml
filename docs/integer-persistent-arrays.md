# Persistent integer arrays

Issue [#627](https://github.com/frankischilling/holyc-ocaml/issues/627) connects
fixed I64/U64/U8 global and static-local arrays to hosted source execution.
The same objects support declaration initializers, indexed reads and stores,
and checked pointer aliases across calls and recursion.

```c
I64 A[2][2]={{10,10},{20,2}};
A[0][0]+A[0][1]+A[1][0]+A[1][1];
```

```c
extern U0 Print(U8 *fmt,...);
I64 F() {
    static U8 Msg[3]="42";
    Print("%s",Msg);
    return 42;
}
F();
```

The first example produces I64 42. The second copies an owned, terminated
three-byte array, prints `42` without a newline, and returns I64 42. Both use
the existing checked source, IR and execution interfaces in JIT and AOT modes.

## Checked shapes and source evidence

Each declaration owns one storage object with a checked element type, positive
dimensions, element count, byte size and suffix byte strides. Globals retain
their exact declaration record and dimension-binding environment before the
owner's publication. Static arrays retain the exact definition frame, location
and compiler options. Shapes are established before storage allocation; later
accesses do not infer or enlarge them.

Global and local type producers retain the original dimension AST expressions.
The driver checks those exact objects, and semantic layout checks the complete
derived expression, including literal payloads, operators and origins. Completed
local layouts retain whether every dimension had validated source evidence.
Floating payloads compare by bits, so changing positive zero to negative zero
cannot silently change a retained extent expression.
Executable static arrays require that evidence; the legacy semantic layout API
still accepts manually supplied dimensions without marking them source-checked.

Declaration initializers also retain their original AST, delimiters, ordered
leaves and expression subtrees. A leaf's owner or source span alone cannot
authorize a replacement expression. The joins reject missing, duplicate,
reordered and foreign leaves, including identifier-free expressions and calls
outside the retained subtree. The source driver consumes one complete batch
per declaration, including statics in uncalled functions. Destination layout
follows recursive token consumption rather than the number of brace-path
components.
Source-backed scalar declarations use the same leaf checks. Legacy scalar
constructors accept only manually supplied declarations without a source
manifest; dropping a leaf marker cannot discard retained source evidence.

Closed extent evaluation preserves native literal classes. An integer or
character literal whose 64-bit payload has its high bit set is internal U64.
Integer binary operations use unsigned comparison, division, remainder and
right shift when either operand has that class. Internal U64 unary minus and
integer complement produce internal I64; logical not retains its operand
class. Existing manually constructed signed layout expressions retain their
signed API meaning.

For example, `(0xffffffffffffffff>0)+1` and
`(0x8000000000000000>>63)+1` both give extent two. Final F64 extents truncate
toward zero, so `2.9` gives two in global and local layout. Mixed constant F64
folding follows the pinned signed intermediate-code payload conversion;
`0xffffffffffffffff+3.0` therefore also gives two. Non-finite results,
conversion overflow and nonpositive persistent extents are rejected.
Unparenthesized comparison chains are explicitly unsupported in this layout
path, including inside a skipped logical branch. Explicitly grouped comparison
results remain usable in arithmetic, as in `((0<1)<2)+1`. The remaining optimizer
comparison-chain behavior is unsupported. [Declaration-time runtime bounds](integer-runtime-dimensions.md)
add retained global reads, calls and updates in ordinary JIT and directive bodies.

## Delimiters, arity and direct strings

Fixed numeric initializers consume exactly the declared counts. Nested lists,
flat braced lists and the parser's supported unbraced forms share the same
checked traversal:

```c
I64 A[2][2]={{10,10},{20,2}};
I64 B[2][2]={10,10,20,2};
I64 C[2][2]=10,10,20,2;
```

Missing elements are not filled with C-style zeros. Extra expressions and
unmatched delimiters fail. Commas and closing braces follow the pinned token
rules: `I64 A[2]={40,2,};` and `I64 A[2]=40,2};` are supported, while
`I64 A[1]={42,};` is rejected. This does not establish a universal trailing-comma
or brace-spill rule. Braced scalar declarations such as `I64 n={42};` remain
unsupported for globals, statics and automatic locals. Automatic array
declaration initialization remains a separate boundary.

A direct string can initialize a final-rank U8 array. The copy uses that
dimension's declared count and must fit within the decoded source bytes plus
their one terminator. Adjacent string tokens form one source string.

| Declaration | Stored bytes or boundary |
| --- | --- |
| `U8 A[3]="42";` | `34 32 00` |
| `U8 A[2]="42";` | `34 32`, with no appended terminator |
| `U8 A[1]="";` | `00` |
| `U8 A[4]="42";` | Rejected: the requested copy exceeds the owned source bytes |
| `U8 A[2][3]={"42","ab"};` | Two separately located row copies |

Only the determined prefix is copied. An oversized destination does not gain
fabricated fill bytes. Grouped or scalar-position strings such as
`U8 A[3]=("42");` and `U8 A[3]={"42"};` do not become direct copies.
`U8 A[2][3]="42";` remains unsupported because the pinned non-final-rank
string branch does not establish a whole-object copy.

Each direct copy remains one checked source leaf and produces independent
mutable destination bytes. It does not retain a runtime string-literal site;
ordinary literals used in expressions or Print formats still have their own
literal storage and quota. Scanning an unterminated copied array stops at its
object bound with the existing diagnostic.

## Initialization phases and lifetime

Preparation classifies each numeric leaf independently and retains immutable
default states plus sparse prepared updates. In AOT mode every prepared
constant and direct copy is present in the initial image before scheduled load
initializers run. Other AOT cells start at zero. In JIT mode prepared array
leaves become visible in declaration and leaf order, interleaved with scheduled
initializer regions. A reached cell that has not been initialized reports the
hosted unknown-JIT diagnostic.

This distinction matters for `I64 A[2]={A[1],42};A[0];`: AOT's first initializer
can read the prepared second element, while JIT reaches that read before the
second leaf is published. Static arrays follow the same per-leaf ordering.
Their scheduled work and JIT publications belong to the function definition's
declaration position in module entry, including statics in unreachable
statements or functions that are never called. Prepared AOT cells are already
in the initial image. Initialization is not repeated on invocation or delayed
until first use.

Complete entry lowering produces an immutable publication receipt bound to the
exact entry graph, storage context, original roots and instruction markers.
Initialization-context validation checks that receipt and the complete ordering
of prepared publications and scheduled regions. A marker cannot be moved across
ordinary statements by supplying a new descriptor. Scheduled numeric stores
use canonical destinations derived from every checked array stride.

Each execution allocates fresh mutable cells and separate publication tracking.
A prepared JIT leaf publishes once, before its checked entry instruction,
without a runtime instruction charge or a change to the last ordinary
expression value. Callees and recursion do not replay entry publications.
Calls preserve object contents; another execution starts fresh. Completed
effects and captured output survive a later initializer fault under the
existing execution-report contract.

U8 stores retain the low eight bits and reads zero-extend them. An assignment's
register result keeps the full RHS bits, so using that assignment to initialize
an I64/U64 element does not substitute the narrowed byte. Existing scalar
initialization and result interfaces keep their contracts.

## Aliases and resource accounting

Persistent arrays reuse the checked [array reference rules](integer-arrays.md):
partial indexing, grouping and pointer materialization preserve the original
object and its byte offset. For I64 `A[2][3]`, declared brackets use strides 24
and 8; `A[0][3]` aliases `A[1][0]`. References passed to helpers retain the same
object across calls and recursion. One-past pointers may be formed, but a read
or store must remain within the declared object. Neighbors and static padding
are inaccessible. I64/U64 elements retain supported word updates; U8 supports
plain assignment and [compound/prefix/postfix updates](integer-byte-updates.md).

The persistent-byte quota charges global declared bytes and rounds each whole
static allocation to eight bytes. `U8 A[9]` therefore charges nine bytes as a
global and sixteen as a static, with nine accessible bytes in either case.
Each byte uses one typed execution cell but charges one logical byte. Unused
declarations count; static storage consumes no invocation-frame slots. AOT
inter-object alignment gaps and host bookkeeping remain outside this quota.

Checked dimensions, products, strides, padding and cumulative cell/byte counts
precede image construction. Host array capacity and the configured persistent
byte bound are checked before count-sized execution allocation. Metadata keeps
sparse updates rather than expanding the default image early. Numeric constant
preparation shares the existing initializer-work budget; direct string copies
charge one preparation work unit per copied byte. Runtime instructions,
ordinary literal bytes, output work, active frames and call depth retain their
separate limits. These bounds do not equate logical byte charges with host
allocator overhead.

## Pinned source and remaining work

Reference: `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

| Pinned compiler source | Relevant behavior |
| --- | --- |
| `PrsVar.HC:1-113,123-204` | Scalar evaluation, constant/scheduled work, recursive fixed counts and byte-string copies |
| `PrsVar.HC:247-281` | Dimension parsing, `LexExpressionI64`, first-empty metadata and products |
| `PrsVar.HC:530-588` | Whole-static padding, initializer passes and AOT byte publication |
| `PrsStmt.HC:285-296,334-435` | Global sizes, allocation, initial states, declaration publication and inferred-size passes |
| `LexLib.HC:248-275` | Joined decoded strings and terminator-inclusive length |
| `PrsExp.HC:679-685,1140-1151` | I64/U64 literal selection and final F64-to-I64 extent conversion |
| `OptLib.HC:96-179`, `OptPass012.HC:153-191,211-217,403-445,809-822` | Closed-expression classes, unsigned operations, unary results and comparison-chain boundaries |

Ordinary first `[]` produces zero dimension metadata in `PrsArrayDims`, while
the initializer's inferred-allocation branches test for negative counts. The
audited path does not connect those states. Empty, zero and negative persistent
extents remain rejected; inferred allocation and native pass-one/pass-two
effect parity are not claimed.

Pointer-valued globals/statics, other primitive widths, aggregate and callback
storage, pointer returns and general pointer arithmetic remain outside this
execution path. Nonconstant AOT static initialization with globals-on-data-heap
retains its separate-phase boundary. Full formatting/runtime linking, broader
memory, stateful compilation and `#exe`, optimizer parity, native backends,
TempleOS BIN/loader acceptance and bootstrap remain compiler work. This
connection uses pinned-source and hosted execution evidence, with no new native
TempleOS capture.

## Maintained execution fixture

`examples/integer-persistent-arrays.hc` combines a copied global message, a
numeric two-dimensional global, a numeric static byte array and a row passed to
a mutating helper. Both modes return I64 42 and capture exactly `3432` (`42`,
without a newline).

| Resource | Exact successful limit |
| --- | ---: |
| Runtime instructions | 101 |
| Initializer preparation work | 23 |
| Persistent bytes | 43 |
| Active frame bytes | 24 |
| Call depth | 2 |
| Ordinary literal bytes | 3 |
| Output bytes | 2 |
| Output work | 8 |

The persistent charge is 3 message bytes, 32 global word bytes and 8 padded
static bytes. The format literal alone consumes the separate three-byte literal
budget. Preparation charges 3 copy bytes, 12 global numeric instructions and 8
static numeric instructions. All exact limits pass together; one below each
limit has a maintained CLI diagnostic assertion. Runtime and call-depth faults
after Print retain its earlier capture. Preflight, preparation and failed Print
drafts produce no capture for this fixture.

CLI v1 and v2 reporting remain unchanged. Deterministic dumps add
`holyc-persistent-arrays-v1` for dimensions, strides, cell/byte destinations and
prepared payloads. `holyc-array-publications-v1` lists JIT publication markers;
AOT prepared images need no such entry events. Existing scalar dump components
retain their format.
