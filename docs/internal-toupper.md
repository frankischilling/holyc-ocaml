# Internal ASCII conversion

The interpreter and native target execute the pinned `IC_TOUPPER` operation
through its original internal declaration:

```c
#define IC_TOUPPER 0x1e
public _intern IC_TOUPPER I64 ToUpper(U8 ch);
ToUpper('a');
```

The result is I64 65. Values from 97 through 122 become ASCII A through Z;
other words retain their complete bits. This operation does not decode Unicode
or use the host locale.

```text
holyc run --target=ir --mode=jit --format=json examples/internal-toupper.hc
holyc run --target=host-jit --mode=aot --format=json examples/internal-toupper.hc
```

The example converts owned byte storage, captures `AZ!:3` and returns I64 42.
Both execution targets use 194 runtime instructions and 15 formatting-work
units. The internal operation itself emits no bytes or formatting work.

## Argument and declaration ownership

The U8 formal does not truncate a supplied internal argument. The pinned
argument parser requests floating/integer conversion where needed, but does
not store integer arguments into declared-width parameter slots for internal
calls. `ToUpper(0x161)` therefore returns 353, and `ToUpper(-1)` returns -1.
An ordinary U8 storage load still reads and extends its stored byte: storing
353 into U8 storage produces 97, which the conversion changes to 65. A U8
source function can return full register bits that this internal call retains.

The numeric target, original declaration, checked I64 return and one U8 scalar
parameter authorize the operation together. A renamed declaration retains its
operation; a source-defined function called ToUpper runs its own body. Later
macro replacement cannot change an earlier retained binding. Default-bearing,
variadic, pointer and other signatures remain outside this gate.

Calls reuse the checked non-template internal path: `IC_CALL_START`, an
unpushed argument producer, operand-bearing `IC_TOUPPER`, then `IC_CALL_END`
with its result. There is no ordinary callee frame, stack cleanup or host
function address. Original call phases, selected source types and producer
origins remain sealed. Foreign contexts and altered operations, arguments,
flags, payloads or result markers reject before execution.

## Native execution and limits

The own encoder emits signed word comparisons and subtraction into a bounded
private result slot. Nested calls preserve pending arguments and results.
Both supported host ABIs compile the same checked operation; execution uses
the active host bridge. Repeated image execution restores fresh storage.

The operation consumes one ordinary IR instruction tick. Its source argument
and call markers retain their own instruction charges; there is no additional
byte scan, output work or semantic activation. Private staging and encoded
bytes remain subject to frame and code limits. A failed argument read or later
instruction-limit fault preserves previously reached output and work.

## Source evidence and remaining work

The reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Kernel/KernelB.HH:58` supplies the declaration. `Compiler/CompilerA.HH:52`
and `Compiler/CInit.HC:47` define its IC identity and metadata.
`Compiler/PrsExp.HC:440-586` supplies argument conversion and call emission.
`Compiler/OptPass789A.HC:826-827` selects the word comparisons and subtraction
in `Compiler/BackB.HC:266-275`. `Compiler/Lex.HC:409,519-522,656` and
`Kernel/StrPrint.HC:164,403` consume the result.

Tests use independent ASCII alphabets, full-word controls, public source and
CLI execution, both modes, native ABI compilation and authority mutations.
They add no TempleOS oracle capture. General internal binding-expression
preparation, floating arguments, other internal operations and broader runtime
services remain under #695 and #705. [Retained IR tasks](retained-internal.md)
now admit original numeric internal headers and preserve their installed target
through nested lookahead. Native retained source execution remains under #704.
The compiler, artifacts, actual loader
and bootstrap requirements remain under #682.
