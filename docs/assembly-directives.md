# Assembler directives

The standalone `asm --emit` command processes directives in source order and
returns one raw byte stream. Instructions share the encoder described in
[instruction byte encoding](instruction-encoder.md). Directive behavior follows
the pinned TempleOS source at
`c26482bb6ad3f80106d28504ec5db3c6a360732c`.

`USE16`, `USE32` and `USE64` change the instruction mode. `DU8`, `DU16`,
`DU32` and `DU64` emit data. `ALIGN` emits fill bytes, while `ORG` records an
origin. The report also retains import declarations, labels, relocation
records, binary-file provenance and the active listing state.

## Commands and example

```sh
dune exec holyc -- asm --emit examples/assembly-directives.asm
dune exec holyc -- asm --emit --format=json --mode=aot examples/assembly-directives.asm
dune exec holyc -- asm --emit --code-byte-limit=37 --output=assembly-directives.bin examples/assembly-directives.asm
```

The [example](../examples/assembly-directives.asm) emits 37 bytes in both JIT
and AOT mode:

```text
23ff410042909090b83412b8785634123412fecafeca7856341288776655443322115a5ac3
```

Its data and instruction ranges are:

| Raw offset | Source operation | Bytes |
| --- | --- | --- |
| 0 | `DU8 0x123,-1,"A\0B"` | `23 ff 41 00 42` |
| 5 | `ALIGN 8,0x190` | `90 90 90` |
| 8 | `USE16` followed by `MOV AX,0x1234` | `b8 34 12` |
| 11 | `USE32` followed by `MOV EAX,0x12345678` | `b8 78 56 34 12` |
| 16 | `DU16 0x1234,2 DUP(0xCAFE),""` | `34 12 fe ca fe ca` |
| 22 | `DU32 0x12345678` | `78 56 34 12` |
| 26 | `DU64 0x1122334455667788` | `88 77 66 55 44 33 22 11` |
| 34 | `DU8 2 DUP(0x5A)` | `5a 5a` |
| 36 | `RET` in USE64 | `c3` |

The example's origin is `0x1003`. Its raw start remains zero and its raw end
is `0x25`. The first five data bytes therefore need three fill bytes to
reach the next eight-byte boundary.

The command without `--emit` returns the parsed `holyc-assembly-v1` report.
It checks syntax and retains expression and source metadata.

## Data values and repetition

`DU8`, `DU16`, `DU32` and `DU64` write the low 1, 2, 4 or 8 bytes of each
scalar, in little-endian order. This follows
[`AsmStoreNum`, `Compiler/Asm.HC:268-390`](../third_party/TempleOS/Compiler/Asm.HC#L268-L390).
For example, `DU8 0x123,-1;` writes `23 ff`. This truncation rule belongs
to data directives; instruction immediate fields retain their checked
encoding ranges.

Every data directive requires a semicolon. Its list may be empty. Commas
are optional between values, and a trailing comma is accepted. A quoted
string contributes its exact byte contents at every `DU` width. The implicit
string terminator is omitted, embedded NUL bytes remain, and no element-width
padding is added. `DU64 "A\0B";` therefore writes three bytes:
`41 00 42`. A character literal is a scalar value and uses the directive's
numeric width. These rules come from
[`PrsAsmDefine`, `Compiler/Asm.HC:815-885`](../third_party/TempleOS/Compiler/Asm.HC#L815-L885).

`count DUP(value)` evaluates one scalar value and repeats its stored word.
The value is evaluated once before the repetition. `DU8 2 DUP($$),$$;`
at raw position zero writes `00 00 02`. Counts at or below zero emit no
bytes, but their inner expression must still be valid. A nested repetition
or a comma-separated list inside `DUP(...)` is a syntax error in the
original grammar.

Closed numeric expressions retain the original operator precedence and
signedness. Shift operators bind more tightly than addition:
`DU64 1+2<<3;` writes the integer 17. A high-bit unsigned literal and a
signed negative value also differ under right shift:
`0xFFFFFFFFFFFFFFFF>>63` is 1, while `(-1)>>63` retains all one bits.

F64 expressions contribute their IEEE 754 payload. `DU64 1.0;` writes
`00 00 00 00 00 00 f0 3f`. HolyC postfix casts change the interpretation
of an immediate without converting or narrowing its stored bits:
`DU16 257(U8);` writes `01 01`, and `DU64 3(F64);` writes
`03 00 00 00 00 00 00 00`. The emitted data width still controls the final
number of stored bytes. Primitive evaluation requires the parser's original
type selection. A newer class or imported symbol with the same spelling
cannot authorize the primitive cast rules.

The expression behavior follows
[`Compiler/PrsExp.HC`](../third_party/TempleOS/Compiler/PrsExp.HC),
[`Compiler/OptPass012.HC:87-96`](../third_party/TempleOS/Compiler/OptPass012.HC#L87-L96)
and [`Compiler/BackLib.HC:454-505`](../third_party/TempleOS/Compiler/BackLib.HC#L454-L505).

The power operator follows the zero and sign branches in
[`SYS_POW`, `Kernel/KMathA.HC:461-512`](../third_party/TempleOS/Kernel/KMathA.HC#L461-L512).
A positive-zero exponent yields 1.0. A positive-zero base with a nonzero
exponent yields positive zero, including a negative exponent. Negative
bases use their magnitude and the parity of the exponent truncated to I64
to select the result's sign: ``(-4.0)`0.5`` yields 2.0, while
``(-4.0)`1.5`` yields -8.0.

General exponentiation uses hosted floating-point operations. TempleOS's
x87 `FYL2X`/`F2XM1` path sets truncation rounding, so exact last-bit parity
for arbitrary exponents remains unverified. Negative-zero bases, nonfinite
inputs and out-of-range negative-base sign conversion on that path produce
diagnostics. Native byte comparison is tracked in #375.

Calls, memory reads and expressions requiring compiler-owned data addresses
remain explicit diagnostics on this standalone path. Their source integration
is tracked in #715.

Instruction immediates and displacements use the same closed value evaluation
before their byte ranges are fixed. At raw position zero,
`MOV RAX,[RBX+$$];` emits `48 8b 03`, and `MOV RAX,1.0;` selects a
full eight-byte immediate containing the F64 payload. A forward label whose
resolved value would resize an instruction still requires the separate layout
work in #714 and produces a diagnostic.

## Origin, alignment and current position

`ORG value` sets metadata without creating a hole or changing stored bytes.
It requires raw position zero and permits one active origin. TempleOS uses
`I64_MAX` as its unset-origin sentinel, so `ORG 9223372036854775807`
leaves the origin unset and permits a subsequent `ORG`.
These checks follow
[`PrsAsmBlk`, `Compiler/Asm.HC:948-956`](../third_party/TempleOS/Compiler/Asm.HC#L948-L956).

`ALIGN boundary,fill` rounds the raw output position upward and writes the
low byte of `fill` until that position is reached. A boundary must have
exactly one set bit. Ordinary positive JIT alignment is limited to eight;
AOT mode permits larger powers of two within the output limit. The report
records the largest requested exponent as `max_align_bits`.

The high-bit boundary `0x8000000000000000` retains the original signed
behavior: it passes the one-bit test and JIT comparison, records exponent
63, and emits no fill when the rounded value is negative as an `I64`.
The implementation follows the signed loop in
[`Compiler/Asm.HC:957-974`](../third_party/TempleOS/Compiler/Asm.HC#L957-L974).

`$$` captures the current raw position when its expression is evaluated.
`ORG` does not contribute to that value. `--position` supplies the initial
raw coordinate, so subsequent values include that starting coordinate and
the bytes already emitted.

Absolute-address occurrence parity determines relocation metadata. A
`DU16 $$` field carries `AAT_ADD_U16`; `DU16 $$-$$` emits zero without
that relocation. The original counter records address occurrences even in
an expression such as `0*$$`. Each repeated absolute field receives its own
relocation record. This follows
[`Compiler/PrsExp.HC:707-721`](../third_party/TempleOS/Compiler/PrsExp.HC#L707-L721)
and `AsmStoreNum`.

The returned bytes contain the raw field values. Applying an origin or load
base belongs to the relocation and object-output stages. The report makes
those required patches visible; `--output` writes the raw stream.

## Labels, imports and listing

Data expressions can refer to a label defined later in the same assembly
unit. A repeated label at the same raw position is accepted; assigning it
another position fails. Local `@@` labels retain the scope introduced by
the preceding exported `name::` label. An ordinary `name:` label does not
reset that local scope. These rules follow the label branches in
[`Compiler/Asm.HC:1039-1098`](../third_party/TempleOS/Compiler/Asm.HC#L1039-L1098).

`IMPORT` records unresolved external names and emits no bytes. Empty lists,
duplicate names, optional commas and a trailing comma are accepted.
Declarations remain in source order in the report. An unused import creates
no relocation. An imported name such as `if`, `sizeof`, `offset` or `defined`
keeps its symbol meaning in later expressions. Reading an imported value or
using an imported call target
requires external linkage and produces an explicit diagnostic until that
stage is available under #714 and #717. The source behavior is in
[`Compiler/Asm.HC:928-947`](../third_party/TempleOS/Compiler/Asm.HC#L928-L947)
and [`Compiler/AsmResolve.HC`](../third_party/TempleOS/Compiler/AsmResolve.HC).

Function assembly and top-level JIT blocks use a temporary import scope.
`IMPORT` changes lookup within the current block or direct instruction run;
new imports disappear on exit, including parser recovery. Top-level AOT
blocks retain imports in the enclosing context. This follows the `CmpJoin` branches
in [`PrsStmt.HC`](../third_party/TempleOS/Compiler/PrsStmt.HC#L944-L958)
and the child hash-table creation and restoration in
[`CMain.HC`](../third_party/TempleOS/Compiler/CMain.HC#L60-L153).

`PrsAsmBlk` ends a direct run using the token already read at
[`Asm.HC:1106-1109`](../third_party/TempleOS/Compiler/Asm.HC#L1106-L1109).
That token keeps its original lookup identity when the import scope ends.
The hosted scope preserves this lifetime; full child compiler execution
remains under #715.

An existing local `@@` binding, including one introduced by a forward
reference, takes precedence over a later global import of the same spelling.
`DU64 @@x; IMPORT @@x; @@x: DU8 1;` therefore resolves the local address
and retains its absolute data relocation. With no earlier local binding,
`IMPORT @@x; @@x:` instead fails when the emitter resolves the definition.

Listing starts disabled. `LIST` enables it for subsequent emitted ranges,
and `NOLIST` disables it. Every report entry retains the state that was active
when its bytes were emitted. This is a structured listing contract. It does
not reproduce TempleOS's terminal columns, delayed source-line flushing or
word-group formatting from
[`Compiler/AsmLib.HC`](../third_party/TempleOS/Compiler/AsmLib.HC).

## Binary files and inserted binary tokens

`BINFILE "name";` inserts the resolved file at that source position.
A name without an extension defaults to `.BIN`. Resolution checks the
requested name first and tries the alternate `.Z` spelling only when that
file is absent. The command uses its working directory, configured
`--include` roots and `--templeos-root` mapping.

Raw files retain every byte, including embedded NULs. Recognized `.Z` files
use the original 17-byte header and `CT_NONE`, `CT_7_BIT` or `CT_8_BIT`
payload format. A malformed archive fails before output; it is never
treated as a raw binary fallback. The original path starts at
[`PrsBinFile`, `Compiler/Asm.HC:888-902`](../third_party/TempleOS/Compiler/Asm.HC#L888-L902)
and continues through the file-reading and compression routines in
[`Kernel/BlkDev/DskFile.HC`](../third_party/TempleOS/Kernel/BlkDev/DskFile.HC)
and [`Kernel/Compress.HC`](../third_party/TempleOS/Kernel/Compress.HC).

Each `binary_inputs` entry includes its source spelling, resolved path,
raw and decoded sizes, SHA-256 digests and compression kind. Compression
names are `raw`, `CT_NONE`, `CT_7_BIT` and `CT_8_BIT`. Repeating a `BINFILE`
directive counts its input again.

A saved DolDoc `$IS` token supplies an integer size. The directive report's
embedded source retains the binary record number, declared size and payload
completeness. `$IB` is an expression yielding a pointer to compiler-owned
data, so it requires the deferred storage path. It does not append its
payload as though it were a quoted string. This distinction follows
[`Compiler/PrsExp.HC:680-705`](../third_party/TempleOS/Compiler/PrsExp.HC#L680-L705)
and `PrsAsmDefine`.

## Limits and reports

`Assembly_encoding.encode` accepts the original parsed assembly and these
inputs:

| Input | Purpose |
| --- | --- |
| `position` | Optional initial nonnegative signed 64-bit raw coordinate |
| `config` | Optional original JIT/AOT mode and permitted binary-file resolution roots |
| `max_work` | Optional cumulative item, expression-token and emitted-byte work allowance |
| `max_binary_input_bytes` | Optional cumulative raw bytes read by `BINFILE` |
| `max_code_bytes` | Required bound on the complete emitted stream |

Without a configuration, the API uses JIT rules and provides no binary-file
resolution authority. The default work allowance is 1,000,000 and the default
binary-input allowance is 16 MiB. The corresponding command options are
`--directive-work-limit` and `--binary-input-byte-limit`; emission options
require `--emit`.

Output planning checks multiplication for `DUP`, alignment growth and each
signed output-position advance before allocating the final string. Binary
decoding checks both raw input and expanded output sizes. These limits are
hosted resource policy. An error returns no partial encoding, and the CLI
preserves an existing output file or leaves a missing output path absent.

The `holyc-assembly-encoding-v1` report retains `assembly`, `instructions`
and the complete hexadecimal `bytes`. Ordered `entries` add source locations,
item indices, raw positions, offsets, sizes and listing state for directives
and instructions. Instruction records retain the actual encoding selection;
the embedded assembly preserves the original parsed operations. JSON item
indices connect emitted ranges to that source representation.
`origin`, `max_align_bits`, `imports`, `symbols`,
`relocations`, `binary_inputs`, `work` and `input_bytes` expose the remaining
layout and accounting information.

The regression fixtures in
[`test_assembly_directives.ml`](../test/test_assembly_directives.ml) and
[`test_assembly_directives_cli.ml`](../test/test_assembly_directives_cli.ml)
use expected bytes derived from the pinned source. They cover mixed
instruction/data output, primitive bit casts, raw strings, repetition,
relative alignment, label scopes, imports, source provenance and exact
resource boundaries. CLI checks compare the actual file with the reported
bytes and check preservation after failures. These fixtures do not add a
TempleOS runtime capture or establish executable-loader acceptance.
