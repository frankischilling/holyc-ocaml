# Native AOT source sessions

`host-jit-task` now accepts AOT source. It executes each `#exe` block in its
original retained JIT task, resumes the outer parser with committed StreamPrint
text, then executes the checked AOT module in a separate native image:

```sh
holyc run --mode=aot --target=host-jit-task --code-byte-limit=1048576 examples/native-aot-streams.hc
```

The example prints `parse43;load1;` and returns 42. Its directive task keeps
`Seed=41` and the original `Read` function across blocks. Its module keeps its
own `Seed=1` and receives the generated `Answer=42` declaration. The task
frontend is frozen before the outer parser publishes any module declarations;
an outer variable or function cannot supply a task binding.

Directive commands, initializers, defaults, runtime layouts and callbacks use
their existing original parser receipts and native entries. The module compiler
consumes the original accepted outer AST and its checked preparation receipts.
It never reparses the source or combines unrelated task graphs. The module's
load-time initializers run after parsing, before its entry commands. Task and
module data occupy separate non-executable arenas.

Instruction and output allowances cover the entire invocation. Code and IR
allowances decrease before compiling the module. Declaration preparation and
saved-default bytes stay cumulative across both source contexts; closed module
expressions retain their existing compile-time preparation. Logical global and
literal allocations, including copied saved strings, must fit their combined
allowances before module entry. The report lists directive fragments followed
by one `aot-module` fragment. Native work comes from actual machine outcomes;
the source task records zero interpreted runtime instructions.

Earlier task output, native work and committed generation charges survive a
later syntax, resource or runtime failure. An unsuccessful stream injects no
partial source. A directive fault prevents module entry. A reached module fault
retains output from both stages. Formatting still precedes stream-context
validation.

The existing native module boundary applies to the outer source: checked
integer/void functions, supported integer storage, callbacks and load-time
initializers. General outer aggregate declarations, runtime AOT dimensions,
reference-default relocation, floating execution and wider signatures remain
open. AOT StreamExePrint currently reaches its native formatter and then reports
`HCIRVM0027`; its successful synchronous path still needs the original suspended
caller, parser/frame ownership and reentrant arena and budget leases. Running
generated source after the caller returns would change that behavior.
The reference also allows StreamExePrint in JIT `#exe`, since both modes set
`CCF_EXE_BLK`. Its AOT child uses the saved enclosing compiler table; the IR
child's current use of the detached task table still needs correction.

This path adds no TempleOS runtime capture, exported HolyC ABI, object/BIN
output, loader acceptance, whole-tree compilation or bootstrap evidence.

The pinned `Compiler/PrsStmt.HC:805-840` switches a stream block to its JIT task
tables, clears surrounding locals and AOT flags, then restores the enclosing
context before injecting its body. `Compiler/Lex.HC:1031-1035` dispatches `#exe`
through that path. `Compiler/CMisc.HC:68-81` supplies StreamPrint's formatting
and active-buffer rules. `Compiler/CMain.HC:673-690` retains the separate
synchronous StreamExePrint requirement.
