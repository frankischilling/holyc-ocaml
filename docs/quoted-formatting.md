# Quoted bytes and uppercase packed output

Checked Print and task StreamPrint support `Q`, `q` and `C` with the existing
field grammar. The native Print formatter emits the same byte conversion and
work checks. The conversions operate on bytes and use ASCII ranges; they do not
consult the host locale or treat the input as Unicode.

```text
holyc run --target=ir --mode=jit --format=json examples/quoted-formatting.hc
holyc run --target=host-jit --mode=aot --format=json examples/quoted-formatting.hc
```

The maintained example combines escaped text, decoded hexadecimal digits and
uppercase packed characters. It returns I64 42 and captures 14 bytes with 54
output-work units. The output contains literal backslashes in `a\"\d\n`, then
`|42|OK` and a newline. Its source shows the separate escaping required to place
a quote, backslash or dollar sign inside a HolyC string.

## Escaping with Q

`%Q` reads an owned U8 string through its first NUL and escapes its contents.
It does not add surrounding quotation marks. Dollar signs become `$$`; the `$`
modifier changes that result to `\d`. Percent signs remain unchanged unless the
`/` modifier is present, in which case they become `%%`.

Line feed, carriage return and tab become `\n`, `\r` and `\t`. A quote or
backslash receives a leading backslash. Other nonzero bytes below `0x1f`, and
byte `0x7f`, become `\xHH` with two uppercase hexadecimal digits. Byte `0x1f`
and bytes `0x80` through `0xff` pass through unchanged.

## Decoding with q

`%q` decodes the byte sequences `\0`, `\'`, backslash-backtick, `\"`, `\\`,
`\d`, `\n`, `\r` and `\t`. `\x` or `\X` consumes up to two ASCII hexadecimal
digits. One digit is allowed; zero digits produce a NUL. An invalid candidate
remains available for the next input iteration. An unknown escape, such as
`\z`, preserves both bytes, and a trailing backslash remains a backslash.

Dollar pairs always collapse to one dollar. Percent pairs collapse only with the
`/` modifier. The `$` modifier does not change decoding. A decoded NUL ends the
visible converted prefix, but the conversion still scans the rest of the encoded
input through its real NUL. A bounds or initialization failure after that decoded
NUL therefore still fails the call.

## Fields and packed characters

Width, `-` and `t` apply to the converted visible prefix. Width is a minimum;
`-` pads on the right; `t` truncates on the right. Padding uses spaces, including
with `0`. Precision is consumed and ignored. Negative width with `t` selects an
empty prefix; without `t`, it emits the full converted prefix.

Truncation can split an escape: `%3tQ` applied to byte `0x01` emits the three
bytes `\x0`, not a complete four-byte escape. This follows the source's sequence
of conversion followed by string layout.

`%C` visits the low-to-high bytes of one word through the first NUL or all eight
bytes. It converts `a` through `z` to `A` through `Z`; every other byte remains
unchanged. Its padding, truncation and work rules match `c`. A later `c` field
does not inherit the uppercase conversion.

## Work and atomic output

Quoted fields use two bounded passes. The first converts the complete input and
records the visible prefix length. The second reconstructs only the selected
prefix. Each conversion step retains at most four bytes, so input length,
expanded length and field width do not allocate a proportional temporary buffer.
The final captured draft retains its existing independent capacity.

Every source read costs one output-work unit before the read, including NULs and
failed reads. The decoder follows `MPrintq`'s current-byte and lookahead reads
separately. Hexadecimal candidates have their own reads; a rejected candidate
can be read again as the next current byte. The copy pass stops when it has
produced the selected prefix, without an extra terminator read. Conversion within
a four-byte chunk adds no work. Every attempted append costs another unit before
the output-capacity check.

These examples describe runtime input bytes; backslashes in the table are
literal input or output bytes.

| Format | Input | Output | Work |
| --- | --- | --- | ---: |
| `%Q` | `AB` | `AB` | 10 |
| `%Q` | byte `0x7f` | `\x7F` | 10 |
| `%3tQ` | byte `0x01` | `\x0` | 11 |
| `%q` | `\x41` | `A` | 13 |
| `%q` | `\x4G` | byte `0x04`, then `G` | 18 |
| `%q` | `\x` | empty | 7 |
| `%/q` | `%%` | `%` | 10 |
| `%C` | packed `abcdefgh` | `ABCDEFGH` | 19 |

Print and generated-source drafts publish only after a successful complete
format. Faults retain earlier output and reached work at the original call site.
Native formatting loops add no IR instruction steps and retain the original
provider frame/depth and physical code/frame limits. Work and output limits also
bound huge widths and zero-output scans.

## Source and remaining scope

The pinned revision is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Kernel/StrPrint.HC:56-110` defines `MPrintQ`; 113-197 defines `MPrintq`;
337-360 connects both to `OutStr`; 396-411 defines packed `C`.
`Kernel/KernelA.HH:3456` fixes shift-space at `0x1f`.
`Kernel/StrA.HC:353-354` supplies the hexadecimal character bitmap, and
`Compiler/BackB.HC:266-275` defines ASCII-only `ToUpper` emission.
`Compiler/Lex.HC:93-104` consumes escaped text in quoted lexer input.

The checked implementation replaces intermediate kernel allocation and recursive
formatting with fixed chunks and explicit work/object checks. It preserves the
claimed bytes within that hosted domain. Tests cover the public interpreter,
native API/CLI and StreamPrint source reentry; no new TempleOS execution capture
is claimed.

[Auxiliary `h` fields](auxiliary-formatting.md) consume their captured arguments
and repeat packed `C` output; they leave Q/q conversion unchanged. Floating-point,
engineering and other runtime-dependent conversions remain under #694.
MStrPrint allocation, null/raw pointers,
pointer-returning helpers, source-defined variadic forwarding and native retained
StreamPrint need their own runtime and ABI paths. The complete compiler, artifact,
actual-loader and bootstrap requirements remain under #682.
