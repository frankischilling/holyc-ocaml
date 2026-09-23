# Native character output

`run --target=host-jit` captures bytes from the checked `PutChars` provider in
both source modes. The declared provider is `extern U0 PutChars(U64 ch);`.
Explicit calls and implicit character statements use the original selected
declaration, argument producer and call instructions.

```text
holyc run --target=host-jit --mode=jit --format=json examples/integer-putchars.hc
holyc run --target=host-jit --mode=aot --format=json examples/integer-putchars.hc
```

The fixture returns I64 42, captures `34320a` and consumes nine runtime
instructions and six output-work units. Output is reported as bytes separately
from the final numeric value. An explicit U0 call clears the ordinary expression
result; an implicit character statement preserves the preceding result.

## Packed bytes and faults

The emitter visits the low byte of the argument and shifts the remaining word
right by eight until it becomes zero. Nonzero bytes are appended, including
bytes above 127. Interior zero bytes are visited but produce no output.
`PutChars(0x00420041)` therefore captures `4142`. A zero word does no output work.

Each visited byte consumes one work unit. An attempted append consumes another
unit before checking remaining output capacity. A later work or byte-limit fault
retains the earlier prefix and its work, including bytes from earlier calls.
The checks have the same order as the interpreted provider.

`HCIRVM0022` reports output-byte exhaustion and `HCIRVM0023` reports output-work
exhaustion. Both identify the original call instruction and consume that one IR
instruction. A byte iteration adds no runtime instruction. Reached argument
faults occur before the provider, and provider frame/depth checks precede even
an empty output operation. Invalid limits fail before parsing or preparation.

## Admission and limits

`Runtime_call_context` must recognize the exact original extern signature,
flags, linkage, fixed argument and selected JIT/AOT call opcode. The native
backend validates that context against the entry, initialization and source
functions. It never selects a host service solely from an identifier's spelling.
A source-defined function named PutChars executes its own body.
A call made after the source definition also uses that body when an earlier
extern declaration exists.

The native source path rejects a provider call combined with a source definition
of that name. Such a program needs the retained extern/body publication phases
tracked by #704. Print, StreamPrint and other providers remain separate work
under #705. Keyboard/display hooks and TempleOS device behavior are outside
captured hosted output.

`--output-byte-limit` and `--output-work-limit` default to 1,048,576 and must be
positive. Output capacity is additionally capped at 16 MiB. Each provider needs
one available semantic call depth and eight available ABI frame bytes. The
inline loop does not allocate a native callee frame. Its argument staging and
code remain charged to the compiled image's ordinary physical-frame and code
quotas. Output storage has its own limit, independent of global, literal and
private data-arena limits.

The host bridge allocates a fresh bounded capture buffer before entry. Its
pointer is read-only in the generated context; only counters can change. After
native teardown, the bridge and report decoder check the counter ranges,
restored call quotas and permitted fault site before exposing the result.
Repeated image execution starts with empty capture. Image/status integrity
failures return an error without a successful capture projection.

`Native_program.output_bytes` and `output_work` expose the source invocation's
capture even on a reached execution fault. The runtime layer provides the same
information through `execute_report`; the existing `execute` result remains
available. Human and JSON reports never mix raw program bytes into metadata.

## Source and verification

Pinned `c26482bb6ad3f80106d28504ec5db3c6a360732c` supplies the provider declaration
in `Kernel/KExts.HC:84` and packed-byte iteration in `Kernel/KeyDev.HC:1-27`.
The reference manifest verifies the complete `KeyDev.HC` Git blob alongside the
existing audited sources.
`Compiler/PrsExp.HC:383-413` selects implicit Print/PutChars targets. The native
implementation reuses the selected call metadata and shared encoder; the C
boundary handles host memory, machine-code entry and result transport.

Compile-only tests retain exact provider ownership, malformed call rejection,
both ABI images, private pointer write protection and fault-site decoding.
Explicit native API/CLI tests cover packed bytes, source calls, recursive output,
implicit result retention, fresh images, argument faults and exact/one-below
budgets. Raw bridge tests reject malformed limits and storage before entry.
These are hosted execution tests and pinned-source comparisons; no new TempleOS
execution capture is claimed.
