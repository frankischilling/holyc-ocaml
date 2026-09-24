# Indexed byte-list formatting

Print and task StreamPrint accept lowercase `%z` with an integer index and an
owned U8 list pointer. Native Print uses the same selection, layout and work
contract. The pinned disassembler uses this form to select names such as `RAX`
from an embedded-NUL list.

```text
holyc run --target=ir --mode=jit --format=json examples/list-formatting.hc
holyc run --target=host-jit --mode=aot --format=json examples/list-formatting.hc
```

The example returns I64 42, captures 11 bytes and uses 61 output-work units.
Its output is `RAX|    B|` followed by a newline. The second selection skips an
alias and pads the selected name; the negative final index produces an empty field.

## Entries, indices and aliases

A list contains NUL-terminated byte strings and ends at an empty entry. A source
literal such as `"A\0BB\0CCC\0"` supplies the final empty entry through its
explicit last NUL and the literal's implicit terminator. Entries have zero-based
indices. A space string is an ordinary entry; an empty string ends the list.

After skipping an entry, selection examines the following byte. An `@` marks an
alias: it skips that marker and scans the alias without consuming another index.
An initial `@` has no such effect because it was not reached after a skipped
entry. These are the pinned `LstSub` rules, including their unusual edge cases:

| List | Index | Selected bytes |
| --- | ---: | --- |
| `A\0BB\0CCC\0` | 1 | `BB` |
| `A\0@Alias\0B\0` | 1 | `B` |
| `@A\0B\0` | 0 | `@A` |
| `A\0@\0B\0` | 1 | empty |
| `A\0BB\0CCC\0` | 3 | empty |

Negative or exhausted indices produce an internal empty payload. They do not
grant raw-pointer access: the list argument still requires an owned pointer, and
a negative index still performs the first checked list-byte read. Unknown or
out-of-bounds storage can therefore fail even when no entry will be emitted.

Selecting an entry does not require scanning the list beyond that entry's NUL.
The final selected entry may end at the last owned byte. Skipping that entry,
however, requires reading the next byte for an alias or end marker. An absent
marker is a bounds fault rather than silent list exhaustion.

## Argument order and layout

Width, precision and auxiliary stars consume their captured words first. `%z`
then requires both its index and list slots before inspecting either argument's
kind. It validates the integer index before the pointer shape. The first checked
read retains the usual U8, bounds, lifetime and initialization rules.

A found entry is fully measured before any field padding or copy, including
bare `%z`. Width, left justification and `t` truncation apply to that selected
entry. For index 1 in the first list above, `%5z` emits `   BB`, `%-5z` emits
`BB   `, and `%1tz` emits `B`. Truncating to zero still checks the complete entry.
An internal empty payload receives ordinary space padding. The `0` flag does
not turn that padding into zeroes.

Precision, comma, `l`, `$`, `/` and auxiliary `h` state do not transform selected
bytes; their parsing and argument consumption remain intact. Each field keeps
its own selection and layout state. Uppercase `%Z` remains unsupported because
it uses the task's Define/hash state rather than a supplied byte list.

## Work, storage and faults

The selector charges each source probe independently. It preserves the outer
list read before the index comparison, the inner scan's repeated first-byte
read, the post-NUL alias probe and the final existence check when the remaining
index is zero. It does not reuse a cached byte to suppress a reached read.

After selection, a complete length scan precedes the selected prefix's reads
and appends. For bare `%z`, index 0 and the entry `A`, the nine work units are
three format reads, two selection probes, two length-scan reads and one copied
byte's read and append. With `AB`, the same operation uses twelve units. A
negative index into valid storage uses four units and emits nothing.

Large indices terminate at the actual list sentinel or a reached work/memory
fault. No loop counts down an index without examining list data, and no selected
string or repeated list buffer is allocated. Native selection uses one emitted
checked-read block with four probe phases, then shares the measured-string body
with formatted `%s`. Left and right alignment share one emitted copy body,
preserving the existing default code-size checks. The selector adds no scratch
slots. Existing `%s` streaming and all previous formatting counts remain covered
separately.

For calls with fewer than two captured variadics, native emission omits the
unreachable selector body. Reaching `%z` still reports the same missing-argument
fault after parsing the format. Calls with more arguments retain the runtime
pair check because earlier fields and stars can consume those arguments.

Calls publish complete drafts. With earlier output already committed, a selected
string's missing terminator faults before an output-capacity failure, and the
earlier output survives. Native failures retain the original Print instruction,
block and source span. Work, output bytes, IR steps and native code/frame limits
remain separate checks.

## Source evidence and remaining work

The reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Kernel/StrA.HC:397-414` defines `LstSub`; `Kernel/StrPrint.HC:373-386` checks the
two arguments and passes the result to `OutStr` at lines 20-40.
`Compiler/UAsm.HC:578-581,617-620` supplies the disassembler's register-list
consumers. These sources already have audited hashes in the reference manifest.

Public source, checked batches, native API/CLI and generated-source tests cover
selection, aliases, storage faults, field layout and exact limits. The tests add
no TempleOS execution capture. Full formatting remains under #694; native
StreamPrint and retained native source publication remain under #705 and #704.
The complete compiler, ABI, artifact, loader and bootstrap gates remain under #682.
