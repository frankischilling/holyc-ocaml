# Function joins across task commands

`holyc run --format=json examples/stateful-exe-function-joins.hc` returns 42
in JIT and AOT modes. Separate directive commands replace two extern headers
and then define their function. Each header evaluates its own default once;
calls use the definition's saved value. The counter finishes at 41.

JIT joins reuse the newest unresolved extern in the current task table.
The canonical callable symbol stays the same, while the new definition keeps
its own source symbol, parameters, locals and frame. A completed definition
causes a new identity to shadow the old one. Earlier compiled calls to that
definition retain its body and dependencies.

Each accepted replacement preserves the prior record's accumulated flags.
Public visibility is replaced on each header; variadic and Ret1 flags remain
sticky. A retained variadic flag prevents a later fixed header from newly
deriving Ret1. Header comparison can receive the prior header's exact saved
default inputs without evaluating its syntax again.

The semantic passes retain the exact predecessor declaration, mode and
classified flag record. Task admission requires that predecessor to remain
the newest publication. Old publication snapshots remain available to compiled
consumers. An earlier unresolved extern selection remains unresolved after a
later definition; it cannot silently acquire the new executable.

The behavior follows the pinned `Compiler/PrsStmt.HC:63-139` and
`Compiler/PrsVar.HC:378` at `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
JIT uses the current hash table for joins. AOT directive commands use their
separate JIT task; this does not join the isolated outer AOT image to that task.

Native extern address-slot updates, parent-table joins, general default values,
alternate bindings and native linkage remain unfinished. The callback-free
`Integer_task.run` entry point does not evaluate runtime defaults or dimensions;
the source executor provides those original parser callbacks.
