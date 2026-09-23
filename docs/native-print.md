# Native Print formatting

The native target executes the checked `extern U0 Print(U8 *fmt,...);`
provider and implicit string statements in both source modes. Its format string
and `%s` arguments are live owned byte objects. They can come from literals,
automatic arrays or persistent storage, including an interior pointer.

```text
holyc run --target=host-jit --mode=jit --format=json examples/integer-persistent-arrays.hc
holyc run --target=host-jit --mode=aot --format=json examples/integer-persistent-arrays.hc
```

The formatter supports ordinary bytes, `%%`, `%d`, `%u`, `%x`, `%X`, `%b`, `%B`,
`%s` and `%c`. Checked field widths, grouping, truncation and byte padding match
the shared interpreter. Precision arguments are consumed but do not affect
these conversions. [Integer and byte formatting](integer-formatting.md) gives
the grammar, source-specific examples and remaining conversions.

`%d` formats signed 64-bit word bits, including the minimum signed value and
high-bit U64 arguments. Loads extend narrow stored values; argument staging
preserves the full computed word, including function results. `%s` reads an
owned byte object through its first NUL. `%c`
visits at most eight packed bytes and stops at the first zero byte. PutChars
instead skips interior zero bytes; its separate behavior is documented in
[native character output](native-output.md).

## Calls and source values

Admission uses the original selected declaration and sealed call context.
The backend checks the format parameter, hidden argument count, each variadic
position, original producer and selected JIT/AOT call opcode. Argument fragments
execute right to left, with values staged in their original formal positions.
Later argument effects cannot replace an earlier captured pointer value.

Every supplied argument is evaluated and charged even when the format ignores
it. A consumed missing argument or wrong value kind reports `HCIRVM0025`.
A source-defined Print function executes its own body. The native target still
rejects a provider selection followed by later same-name body publication;
that program needs the retained execution phases under #704.

Explicit U0 calls clear the ordinary numeric result. Implicit string statements
preserve the preceding result through their original discard identity. Provider
output inside a function does not overwrite the outer result latch.

## Atomic capture and work

Each Print call writes an uncommitted draft into the unused tail of the bounded
capture buffer. The generated loop checks capacity before each write. Only a
completed format updates the committed byte count and remaining capacity.
The host returns exactly the committed prefix after native teardown.

A format, pointer, initialization, work or capacity fault discards all draft
bytes from that call. Output from earlier completed Print or PutChars calls
survives. Draft work remains charged even when none of its bytes are committed.
Repeated execution of an image starts with empty capture and fresh data arenas.

One output-work unit precedes every format or `%s` byte read, including its NUL
and a read that later faults. Every attempted append consumes another unit
before its capacity check. `%c` charges each visited packed byte and each
attempted append. Positive-width or truncated strings first scan through NUL,
then reread their selected output prefix. Measured packed fields charge their
visits once and append from the saved word. Plain fields retain their existing
interleaved read/visit and append order. Numeric conversion charges emitted characters without adding
artificial scan work. The formatter remains one reached IR call instruction;
its internal loops do not consume extra runtime steps.

Byte exhaustion reports `HCIRVM0022`; work exhaustion reports `HCIRVM0023`.
The image records atomic Print sites separately from PutChars sites, so a Print
capacity failure can retain less than the total byte limit without weakening
PutChars status validation. Unknown elements, object bounds and scan addition
overflow preserve `HCIRVM0012`, `HCIRVM0019` and `HCIRVM0020` respectively.
Each failure names the original call instruction, source span and reached work.

## Memory and resource limits

Format and string scans use the reference's original data address, logical
offset, full extent and per-element initialization state. They check bounds
before reading flags or bytes. A one-past pointer has no readable byte. Wider
pointees fail with `HCIRVM0018`. The shared interpreter treats an initialized
signed I8 cell as an invalid output byte (`HCIRVM0008`); native execution keeps
that existing distinction, including bounds and unknown-read precedence.

The provider checks one semantic call depth and eight frame bytes for each
format, hidden-count and supplied variadic slot before formatting. Its typed
argument table, counters and bounded numeric buffer occupy the ordinary private
native frame and must fit the physical-frame quota before emission. Indexed
argument lookup checks the count before forming a private stack address.

Output bytes and work retain the existing independent limits. The capture
pointer remains immutable in the native context, and Print needs no host
formatter callback or allocation while machine code runs. Runtime status
decoding accepts format faults only at an original Print site; malformed
status data exposes no successful capture.

## Evidence and remaining work

Pinned `c26482bb6ad3f80106d28504ec5db3c6a360732c` declares Print in
`Kernel/KExts.HC:83`. `StrPrintJoin` in `Kernel/StrPrint.HC:208-873` supplies the
format traversal, `%s`, packed `%c` and signed decimal behavior. Print at
lines 890-895 formats its buffer before publication. The host implementation
adds checked object ownership, bounded work and explicit errors; it does not
claim TempleOS invalid-pointer diagnostics.

The backend consumes existing source/call/storage evidence and emits the
formatter with the shared x86-64 encoder. Tests compare independent byte results
and the checked interpreter through public API and CLI execution. They cover
mutable formats, narrow values, argument effects, failed drafts, original fault
sites, repeated images and exact/one-below limits on both ABI images.

Full formatting remains under #694; StreamPrint and broader native runtime
services remain under #705. Retained source execution, the complete ABI,
artifact/BIN writers, actual loader acceptance and bootstrap retain their own
release gates. No new TempleOS execution capture is claimed.
