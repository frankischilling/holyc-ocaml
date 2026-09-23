# Integer and byte formatting

Checked Print calls support decimal, hexadecimal and binary words, field widths,
digit grouping, truncation, owned strings and packed characters. The interpreter,
task StreamPrint service and emitted native Print use the same supported grammar
and byte/work contract. Formats remain live byte objects, so argument effects can
change their contents before the call reads them.

```text
holyc run --target=ir --mode=jit --format=json examples/integer-formatting.hc
holyc run --target=host-jit --mode=aot --format=json examples/integer-formatting.hc
```

The maintained example returns I64 42 and captures these 42 bytes, including the
final newline:

```text
0000002A|OK   |-0042|18446744073709551615
```

Its first call uses a mutable format and a function-supplied width. The two calls
require 68 output-work units. Source modes keep their ordinary declaration and
initialization rules.

## Format grammar

After `%`, the parser accepts an optional `-`, then an optional `0`. Width is
zero or more decimal digits followed by an optional `*`. A star consumes the
next captured integer word and replaces any literal width. An optional `.`
introduces precision with the same digits-and-star grammar. Precision is parsed
and its argument is consumed, but it does not change these conversions.

The parser then accepts repeated `,`, `t`, `l`, `$` and `/` modifiers before the
conversion. The supported conversions are `%`, `d`, `u`, `x`, `X`, `b`, `B`, `s`,
`c`, `C`, `q` and `Q`. `l` is ignored; `$` and `/` affect only the quoted byte
conversions described in [quoted formatting](quoted-formatting.md). Modifier order follows
the pinned parser: `%0-5d` is invalid, while `%-05d` is accepted.

Literal width and precision must fit a nonnegative signed I64. An overflowing
literal reports `HCIRVM0024` before a later star could replace it. This checked
limit avoids wrapping field arithmetic. Dynamic fields keep the supplied I64
bits, including negative values. Missing or wrongly typed star arguments report
`HCIRVM0025` before the parser reads the following conversion.

Argument evaluation still runs right to left. Formatting consumes the resulting
captured tail in its original order. `%*.*d` therefore consumes width, precision,
then value; `%*%:%d` consumes a width even though percent ignores its field. Extra
arguments are evaluated and retained but need not be consumed by the format.

## Words and padding

`d` interprets the full word as signed I64. `u` prints unsigned decimal; `x` and
`X` print unsigned hexadecimal with their respective letter case; `b` and `B`
both print unsigned binary. Narrow storage loads retain their declared extension
rules. A full computed word or source-function result is preserved when staged
for the variadic call.

Numeric fields pad on the left. The `-` flag does not change numeric alignment
in the pinned formatter. Negative widths act as zero. Space padding precedes
the sign; zero padding follows it. With `t`, the formatter keeps the low digits
that fit, including any commas in that part of the converted payload. A negative
decimal sign is emitted even when no digits fit.

Comma grouping uses three digits for decimal and four for hexadecimal or binary.
Grouped zero padding follows the source's group counter, so a field can begin
with a comma. These are source-derived examples:

| Format and arguments | Captured bytes | Work |
| --- | --- | ---: |
| `%u`, -1 | `18446744073709551615` | 23 |
| `%05d`, -42 | `-0042` | 10 |
| `%-5d`, 42 | `   42` | 10 |
| `%08,d`, 123 | `,000,123` | 14 |
| `%4,td`, 1234567 | `,567` | 10 |
| `%0*tX`, 8, 0x10000002A | `0000002A` | 14 |
| `%1.2*d`, 1234, 42 | `42` | 9 |

The reverse conversion buffer has a fixed bound: the longest supported payload
is 64 binary digits and 15 commas. Neither literal nor dynamic width allocates a
buffer proportional to the field size. Padding stops at the existing byte or
work limit.

## Strings and packed characters

`s` scans a checked U8 object through its first NUL. `c` visits up to eight
low-to-high bytes in a word, stopping at the first zero. Bytes above 127 are
preserved. `C` follows the same path with ASCII-only uppercase conversion.
`q` and `Q` decode or escape owned byte strings before field layout; their
complete scan, lookahead and fixed-chunk rules are documented separately.
PutChars retains its separate rule of skipping interior zero bytes.

For strings and packed characters, width is a minimum, `-` moves padding to the
right, and `t` retains the left prefix that fits. Padding uses spaces even with
the `0` flag. Precision, comma grouping and the other admitted modifiers have no
effect. Negative width with `t` emits an empty field; without `t`, it emits the
whole content.

Positive-width or truncated strings are measured through their first NUL before
padding or copying. Truncation cannot make an unterminated or uninitialized
object readable: `%1ts` must still finish that scan. The copy then reads only the
selected prefix. `%5s` with `AB` emits `   AB` with 14 work units; `%1ts` emits
`A` with ten units; `%0ts` emits nothing but still uses eight units.

## Work, faults and generated source

Each format byte read costs one work unit, including its NUL and failed reads.
Each attempted output byte costs another before the capacity check. Numeric
conversion uses its fixed buffer without artificial scan charges.

A string without positive width or truncation retains its existing interleaved
read-and-append path. A measured string charges every first scan read and every
selected-prefix reread, with no second terminator read. Packed characters use
the existing interleaved visit-and-append path when width and truncation do not
require measurement. Otherwise they charge at most eight measurement visits,
including the first zero, and append from the saved word. These rules preserve
the established bare-format fault order and counts.

Print publishes a complete successful draft. Faults retain earlier output and
all charged work, while discarding the failed call's bytes. The native formatter
keeps its original checked call instruction, span, frame and depth accounting;
its internal formatting loops add no IR execution steps.

StreamPrint uses the shared formatter with separate generated-byte capacity and
the task's common output-work allowance. Its formatted text can reenter the
ordinary parser through `#exe`, including hexadecimal expressions, declarations
and truncated byte fragments. Native retained source execution and native
StreamPrint remain separate requirements.

## Source evidence and remaining conversions

The reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Kernel/StrPrint.HC:20-40` supplies string alignment and truncation;
lines 236-323 define fields and modifiers; lines 390-411 supply packed-character
behavior; lines 432-522 cover signed/unsigned decimal and grouped padding;
lines 787-867 cover hexadecimal, binary and percent. Print at 890-895 formats its
complete buffer before publication. `Compiler/CMisc.HC:68-80` connects the
formatter to generated source.

Independent expected bytes and work are shared as test data across source,
checked-batch, native, CLI and task tests. The implementations do not call a host
formatter. The checked object model, explicit unsupported-format errors and work
quotas are hosted policies; no new TempleOS execution capture is claimed.

Full formatting remains under #694. Auxiliary formats such as `h`,
floating-point, date, symbol, pointer and
other runtime-dependent conversions still require their own source consumers
and tests. The full compiler, ABI, artifact, loader and bootstrap gates remain
required.
