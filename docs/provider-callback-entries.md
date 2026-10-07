# Hosted provider callback entries

JIT source tasks can capture the original checked `PutChars(U64) -> U0` extern
slot and invoke it through a one-star callback. The IR runner and `host-jit-task`
execute the same packed-byte operation. The native task emits a private machine
entry with its own executable owner and stable entry cells.

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

Provider selection requires the original extern declaration, approved U0/U64
signature, derived RET1 cleanup flag and complete sealed IMM-slot/DEREF pair.
Native admission also requires the original task binding, publication and source
generation. A copied receipt or expired source request cannot supply that
binding. The slot receipt alone grants no body installation or native mapping.
The native provider has a separate private entry; no synthetic HolyC body is
registered for it.

Invocation checks the callback's original return class, fixed parameters,
variadic shape and cleanup policy. A reached mismatch reports `HCIRVM0014`
after argument effects. Output byte and work limits preserve already emitted
bytes. Call depth, active frame, native stack, machine-code and IR limits remain
bounded. The provider's native output faults identify the original callback
call site, and successful calls clear the temporary fault-site value.

The provider writes packed nonzero bytes to the hosted output buffer. TempleOS
`PutKey` device hooks, scheduling and display behavior remain outside this hosted
contract. Native entries use the existing private status/arena convention;
the exported HolyC ABI is separate work.

The isolated `host-jit` path still rejects original extern slot addresses
without task storage. Callback entries for Print, StreamPrint and StreamExePrint,
native AOT source tasks, callback members, wider indirection and full compiler
acceptance remain open. Unresolved ordinary AOT extern addresses still report
`HCSEMA0046` outside assembly.

## Source and checks

The pinned reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/PrsExp.HC:624-652` selects the original function entry and captures a
JIT extern through its current `exe_addr` slot. `Compiler/PrsStmt.HC:95-114,181-191`
installs the placeholder and later compiled body. `Kernel/KeyDev.HC:20-28`
defines the U0/U64 packed-byte loop; `Kernel/KExts.HC:84` declares that extern.
The existing function flag audit governs RET1 and callback cleanup.

`test/provider_callback_cases.ml` supplies original source fixtures for the IR
and native consumers. Tests cover capture order, copies, arrays, parameters,
recursion, statics, saved defaults, joined definitions, byte values, mismatches,
owned numeric guards and measured limits. Native source authority tests compile
the original entries for both private ABIs, execute the host ABI, and check
copied receipts, malformed mappings, released owners and expired requests.
These source audits and hosted executions add no TempleOS runtime capture.

`examples/provider-callback-entries.hc` prints `AB` through a saved provider
after a source body replaces the extern slot, then returns 42.
