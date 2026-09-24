# Internal byte-string length

The interpreter and native target execute the pinned `IC_STRLEN` internal
operation on an owned byte pointer. The result is an I64 byte count, beginning
at the pointer's captured offset and ending before the first zero byte.

```c
#define IC_STRLEN 0x84
public _intern IC_STRLEN I64 StrLen(U8 *st);

StrLen("abc");
```

This returns 3. An empty string returns 0, and `"A\0BC"` has length 1. Bytes
above 127 are ordinary nonzero bytes; the operation does not decode text.

```text
holyc run --target=ir --mode=jit --format=json examples/internal-strlen.hc
holyc run --target=host-jit --mode=aot --format=json examples/internal-strlen.hc
```

## Declaration and call ownership

The original `_intern` target selects the operation. This gate reads the parsed
integer literal retained after macro expansion. The target `0x84` maps to
`IC_STRLEN` in the generated reference table. Renaming this declaration to
`ByteCount` preserves its operation. An ordinary source function called `StrLen`
executes its own body.

This gate requires the checked I64 return type and one U8 pointer parameter.
The declaration retains its original binding alongside its checked signature.
The argument producer and returned value remain attached to their original
source and graph. A later macro redefinition cannot replace that binding.
Other internal targets and signatures remain unsupported. Parenthesized,
arithmetic and other binding expressions require their own original evaluation
and preparation evidence; this gate does not evaluate them again at call time.

For a source batch, the declaration collection retains the original prototype
when it allocates the semantic symbol. The resolver checks that association,
the owning table and scope, and the typed header's original parameter nodes.
A reconstructed module cannot substitute another numeric binding while sharing
the old signature children. Command-local views preserve the original parser
publication's binding. Hand-built declarations without retained source evidence
carry no executable internal target. Live parser calls also retain their
publication and phase checks, including the declaration selected for emission
after argument evaluation.

The completed internal header establishes the supported numeric operation and
its non-extern state. It does not install a host function pointer. Calls use the
pinned non-template form: `IC_CALL_START`, an unpushed argument, `IC_STRLEN`, then
`IC_CALL_END` with the scalar result. There is no ordinary call or stack cleanup.
The checked intrinsic records remain separate from ordinary runtime-call records.

## Reads and execution limits

Every reached byte probe checks the owned object's lifetime, extent and
initialization state. A pointer into an object begins at that logical offset.
The scan stops immediately at the first zero, so unknown bytes after it are not
read. An unknown byte before the terminator faults. A missing terminator reaches
a bounds fault, and an aligned one-past pointer cannot supply even the first byte.

Each attempted probe consumes one runtime step, including the terminating zero.
The ordinary `IC_STRLEN` instruction charge covers the first probe; later probes
consume additional steps before accessing storage. An exhausted step budget
therefore fails before the next read, even when that read would fail a storage
check. For otherwise identical source, an N-byte string uses N more runtime
steps than an empty string. These additional steps retain the `IC_STRLEN`
instruction, block and source span in fault reports.

The operation emits no output and consumes no Print output work. Earlier output
and source effects survive a later scan fault. Ordinary enclosing calls keep
their existing frame and depth checks; the internal scan adds no callee
activation. Native execution uses a bounded private result slot between its
start and end markers. That slot is covered by the native stack limit.

The native loop reads the current owned bytes through checked descriptors.
It does not precompute literal lengths, allocate a length-sized buffer or call
the host C library. Existing native pointer admission supplies descriptor
ownership and active frame lifetimes; pointer returns and persistent pointer
storage remain outside that admission.

## Source evidence

The reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

`Kernel/KernelB.HH:61` declares the operation. `Compiler/PrsStmt.HC:1055-1061`
evaluates the binding target, while lines 244-249 join the header, store the
numeric target, set the internal flag and clear the extern flag.
`Compiler/PrsExp.HC:544-586` supplies the unpushed internal call form.

`Compiler/CompilerA.HH:177` and `Compiler/CInit.HC:149` define the instruction
identity and operand/result counts. `Compiler/OptPass789A.HC:905-908` selects
the template at `Compiler/Templates.HC:87-94`, which scans bytes and excludes
the terminator from its count. `Compiler/UAsm.HC:293-303` consumes that scalar
result while checking segment-prefix text length.

Owned-memory checks and per-probe execution limits are hosted policies.
The tests use source-derived expectations and the host execution targets;
this feature adds no TempleOS execution capture. Other internal operations,
general pointers and runtime services remain under #695, #699 and #705.
The complete compiler and bootstrap requirements remain under #682.
