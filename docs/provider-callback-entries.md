# Hosted provider callback entries

JIT source tasks can capture the original checked Print or PutChars extern
slot and invoke it through a one-star callback. The IR runner and `host-jit-task`
execute the same formatting and packed-byte operations. The native task emits a
private machine entry with its own executable owner and stable entry cells.

```holy-c
extern U0 PutChars(U64 ch);
U0 (*p)(U64 ch);
p=&PutChars;
p('AB');
42;
```

The callback captures the current slot before its argument expressions execute.
Copies, indexed cells, automatic and static storage, fixed callback parameters
and saved JIT defaults preserve that captured entry. A later joined source body
can replace the extern slot. Earlier provider captures still print; later
captures call the new body. Repeated captures of the same original provider
compare equal. A provider address has no exported numeric word representation.
An address captured during the replacement header's own static initialization
retains UndefinedExtern; that phase does not regain the earlier provider.

Provider selection requires the original extern declaration, approved primitive
signature and flags, and complete sealed IMM-slot/DEREF pair. PutChars uses
U0/U64 with RET1 cleanup. Print uses U0/U8*, a hidden count and a variadic tail.
Native admission also requires the original task binding, publication and source
generation. A copied receipt or expired source request cannot supply that
binding. The slot receipt alone grants no body installation or native mapping.
The native provider has a separate private entry; no synthetic HolyC body is
registered for it.

Invocation checks the callback's original return class, fixed parameters,
variadic shape and cleanup policy. A reached mismatch reports `HCIRVM0014`
after argument effects. Installed providers use intrinsic storage spellings;
the same checked public primitive has the provider ABI. User aggregates with
those names retain their separate identities. Output byte and work limits
preserve already committed bytes. Call depth, active frame, native stack, machine-code and IR limits remain
bounded. The provider's native output faults identify the original callback
call site, and successful calls clear the temporary fault-site value.

Print callbacks use the shared [native formatter](native-print.md). Owned
primitive pointer tails retain their checked kinds, so `%s` can read a U8
object and unused pointer arguments still evaluate normally. Runtime count and
kind checks precede argument-table reads, including `%z`'s two arguments. A
failed format discards that call's draft while retaining prior output and
charging reached work. The native provider frame has fixed scratch storage;
it makes no host formatting call.

Retained functions can accept a Print callback and use pointer tails before
the provider is captured by a later source fragment. The original callback
shape reserves the required argument-kind staging. Reached source-defined
variadic callees still require the current word-tail ABI; forwarding pointer
tails into those bodies remains separate work.

Each native fragment currently emits its admitted private provider entry.
Larger histories and saved defaults can exceed the unchanged 64 KiB cumulative
code default. The maintained Print example uses an explicit allowance:

```text
holyc run --target=host-jit-task --code-byte-limit=262144 examples/print-callback-entries.hc
```

It prints `AB` through an earlier saved callback after the source Print body
replaces its slot, clears the original cell, and returns 42. Cross-image
provider-code reuse must still preserve original executable ownership and
resource accounting.

## Stream callbacks

IR and native JIT tasks capture StreamPrint and StreamExePrint through the same original
slot receipts. StreamPrint formats first, then writes to the active task-owned
generation buffer. Saved captures, arrays and defaults work across `#exe`
directives. Nested buffers retain their LIFO ownership and cumulative generated
byte bound.

Native StreamPrint formats in its emitted entry and commits an executed capture
to the original active generation buffer. The generated source resumes the
original parser. Ordinary output stays separate, while both destinations share
the native output-work budget. See [native stream generation](native-stream-generation.md)
for the maintained example, source and resource boundaries, and capture checks.

StreamExePrint formats first and requires an active `#exe` context in either
outer compilation mode. The IR task executes the formatted source, returns its
word, and shares runtime, output-work and nested execution limits. Inactive
calls report `HCIRVM0027` after formatting, including calls through a retained
body or saved callback. Ordinary child source has no inherited `#exe` permission;
a child directive establishes and closes its own context. Nested source selects
the saved enclosing compiler namespace in both modes. Its original completed
types, replacements and reached child publications remain visible there, while
directive-only names stay in their own task. Directive caller locals are hidden;
saved compiler-local shadows remain checked. Ordinary output remains separate
from generated text.

Native active calls resume the same original parser and execute live child
requests in machine code. C checks the exact scope and budget, permits arena
borrowing only from an actual suspended owner and joins measured child usage.
Class publication is visible before the outer parser resumes, and accepted
declaration completion returns zero. Child failure retains original diagnostics
and reached effects. See [native streams](native-stream-generation.md).

The provider writes packed nonzero bytes to the hosted output buffer. TempleOS
`PutKey` device hooks, scheduling and display behavior remain outside this hosted
contract. Native entries use the existing private status/arena convention;
the exported HolyC ABI is separate work.

The isolated `host-jit` path still rejects original extern slot addresses
without task storage. Original AOT
runtime data/function imports, compiler-local metadata, callback members, wider indirection and full compiler
acceptance remain open. Native AOT source sessions execute their separate
directive tasks and outer module. Unresolved ordinary AOT extern addresses still report
`HCSEMA0046` outside assembly.

## Source and checks

The pinned reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/PrsExp.HC:624-652` selects the original function entry and captures a
JIT extern through its current `exe_addr` slot. `Compiler/PrsStmt.HC:95-114,181-191`
installs the placeholder and later compiled body. `Kernel/KeyDev.HC:20-28`
defines the U0/U64 packed-byte loop; `Kernel/KExts.HC:84` declares that extern.
The existing function flag audit governs RET1 and callback cleanup.
`Kernel/StrPrint.HC:890-895` formats Print's draft before publication.
`Compiler/CMisc.HC:68-81` formats StreamPrint before checking its active stream
block; `Compiler/CMain.HC:673-690` requires the enclosing AOT context for
StreamExePrint. `Compiler/CompilerB.HH:21-22` declares the stream signatures.

`test/provider_callback_cases.ml` supplies original source fixtures for the IR
and native consumers. Tests cover capture order, copies, arrays, parameters,
recursion, statics, saved defaults, joined definitions, byte values, mismatches,
owned numeric guards and measured limits. Native source authority tests compile
the original entries for both private ABIs, execute the host ABI, and check
copied receipts, malformed mappings, released owners and expired requests.
These source audits and hosted executions add no TempleOS runtime capture.
Print callbacks also consume the shared numeric/string format fixtures with
exact source-derived work. Stream tests cover installed primitive signatures,
active/inactive contexts, history, defaults, nested buffers, failed drafts,
source faults and exact/one-below cumulative execution and work limits.

`examples/provider-callback-entries.hc` prints `AB` through a saved provider
after a source body replaces the extern slot, then returns 42.
