# Auxiliary fields and repeated packed output

Print and task StreamPrint accept the `h` auxiliary modifier for the supported
nonfloating conversions. Native Print emits the same parser and repeated-byte
operations. This supports the `%h*c` indentation used by the pinned compiler's
`AsmLineLst` consumer.

```text
holyc run --target=ir --mode=jit --format=json examples/auxiliary-formatting.hc
holyc run --target=host-jit --mode=aot --format=json examples/auxiliary-formatting.hc
```

The example returns I64 42 and captures 16 bytes with 45 formatting-work units.
Its output is `  AB  AB|---|2A` followed by a newline. The first field applies
four-character width to each uppercase copy; the second takes its repeat count
from an argument. The hexadecimal field consumes an auxiliary argument without
changing the hexadecimal conversion.

## Parsing and captured arguments

The auxiliary value starts at zero for each format field. Encountering `h` marks
it present. With no `h`, a packed `c` or `C` field retains its single-copy behavior.

An immediately following `*` consumes an integer word and replaces the auxiliary
value. An immediately following `?` preserves that value. Otherwise the parser
accepts an optional minus and decimal digits. These digits append to the current
value, including a value set by an earlier `h` or star in the same field. Literal
auxiliary arithmetic wraps as I64, including multiplication, addition and negation.
Existing checked width and precision arithmetic retains its overflow diagnostics.

A literal auxiliary minus remains set for the rest of the field. Each subsequent
literal or bare `h` applies that sign after appending any digits. Star and question
forms preserve the sign without applying it. These details matter when `h` repeats:

| Format | Auxiliary value | Result with packed `a` |
| --- | ---: | --- |
| `%h3c` | 3 | `aaa` |
| `%h1h2c` | 12 | twelve copies of `a` |
| `%h-2h3c` | 17 | seventeen copies of `a` |
| `%h-2hc` | 2 | `aa` |
| `%h2h?c` | 2 | `aa` |
| `%hc` or `%h?c` | 0 | empty |

A literal followed by a star is not an overriding auxiliary field: `%h2*c`
reports `HCIRVM0024`. The star form must immediately follow `h`.

Width, precision and auxiliary stars consume the captured arguments in that
order. `%*.*h*C` takes width, precision, repeat count and packed word. Each star
checks its integer argument before reading the next format byte. A missing or
wrongly typed argument reports `HCIRVM0025`, even when that next read would also
fail. Ordinary argument evaluation and its right-to-left effects still finish
before formatting consumes the staged values.

## Packed copies and other conversions

`c` and `C` consume and validate their packed word once. A nonpositive auxiliary
count then produces no copies. Each positive copy starts from the same word and
applies its own width, alignment and prefix truncation. `C` changes only ASCII
lowercase letters. Both stop at the first packed NUL or after eight bytes.

For example, `%4h2C` with packed `ab` emits `  AB  AB`. `%1th2C` with packed `az`
emits `AA`; `%0th2C` emits nothing while still visiting each copy's packed bytes.
Zero copies still require the packed argument and its correct type. Auxiliary
state and flags reset before the next field.

The admitted `%`, `s`, `Q`, `q`, `x`, `X`, `b` and `B` conversions consume any
auxiliary stars but otherwise keep their existing behavior. In particular, a
zero auxiliary count cannot suppress a quoted input scan or hide a bounds fault.

In the reference, any `h` on `d` or `u` enters engineering-number formatting,
even `h0` or `h?`. That floating path remains under #694. The checked formatter
consumes and validates the numeric argument, then reports `HCIRVM0024` for the
unsupported engineering conversion. A missing or wrongly typed numeric argument
retains its earlier `HCIRVM0025` fault.

## Work and failure behavior

Each repeated packed field keeps the existing byte visits, measurement and
appends. There is no additional repeat charge. Even an empty packed word visits
its first NUL on every copy; truncation to zero still performs each measurement.
Huge counts therefore reach the output-work limit when they emit no bytes.
Counts and widths never allocate proportional temporary buffers.

An unmeasured short packed field visits its NUL after its last append. A capacity
failure can stop before that visit, so failure work need not be successful work
minus one. With `Print("|");Print("%h3c",'A');`, success captures `|AAA` with 17
work units. Capacity for only three total bytes fails with 15 reached units and
preserves only `|`. A work limit of 16 fails at the final format read and also
preserves only the earlier call.

Print and StreamPrint publish complete successful drafts. Native failures retain
the original Print instruction, block and source span. Formatting work remains
separate from IR instruction, provider frame/depth and native code/frame limits.
Native repeated fields add only fixed auxiliary and saved-word scratch storage.
The lowercase and uppercase paths share one emitted packed-field body, keeping
their repeated operations within the existing code-size checks.

## Source evidence and remaining work

The reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Kernel/StrPrint.HC:236-323` defines the parser state and auxiliary grammar;
lines 390-411 repeat packed fields through `OutStr` at lines 20-40. Lines 441-445
and 518-522 select engineering formatting for auxiliary decimal output.
`Compiler/AsmLib.HC:147-165` uses captured repeat counts for listing indentation.
`Compiler/CMisc.HC:68-80` supplies the StreamPrint source consumer.

Independent fixtures cover source execution, native API/CLI reports, generated
source reentry, argument/read order, wrapping counts and exact resource limits.
The tests add no TempleOS execution capture. Floating and runtime-dependent
formatting remain under #694; native StreamPrint and retained native execution
remain under #705 and #704. The full compiler, ABI, artifact, loader and bootstrap
requirements remain under #682.
