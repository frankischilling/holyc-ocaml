# Automatic U0 declarations

An uninitialized scalar `U0` local retains its original zero-byte frame slot.
It does not need a runtime value cell or an initialization flag. The original
local symbol, retained type reference, automatic storage kind and zero-sized
slot must agree before the hosted executors omit storage for it. Parameters,
arrays, pointers and explicit register requests do not use this path.

`I64 F(){U0 x;return sizeof(x)+42;}F();` returns 42 through IR, native source tasks
and isolated native programs in both outer modes. The maintained
`examples/automatic-void-declarations.hc` adds a persistent integer and returns
the same result. Multiple declarations, nested
blocks, repeated calls, recursion and neighboring integer storage have command
checks. A function containing only these declarations requires no source frame
bytes. Public resource limits still require a positive bound.

Reads, addresses and assignments to the U0 local remain unsupported. In
particular, an automatic initializer is not the zero-byte copy used by a
persistent U0 initializer. `Compiler/PrsVar.HC` reparses an initialized automatic
declaration as an ordinary assignment. `OptPass012.HC` warns about U0 assignment;
`OptPass3.HC`, `OptPass4.HC`, `BackC.HC` and `BackLib.HC` then carry the original
register/address and move decisions.

Native probes show why this distinction matters. An initializer call runs once,
but an initialized U0 local can share an earlier local's displacement and change
that variable's observed value. The captured before-local example returns 42,
not the earlier value 7. Treating that assignment as a no-op would be incorrect.
The declaration-only path rejects initialized U0 locals instead. Their register
selection, address behavior and warnings remain unfinished under #690 and #686.

The pinned reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
Fixtures under `test/oracle/automatic-void` retain the native JIT observations,
source and capture hashes. They do not establish original AOT behavior or
complete automatic U0 assignment support.
