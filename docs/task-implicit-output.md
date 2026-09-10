# Retained implicit output

`Integer_task.run` and the shared JIT/#exe source executor support implicit
`Print` and `PutChars` calls through checked task declarations. After registering
`extern U0 Print(U8 *fmt,...);` and `extern U0 PutChars(U64 ch);`, a later input
`"%d",42;'!';` emits `42!`. Source-defined functions with these names execute
their retained bodies; provider behavior requires the checked provider header.

The pinned TempleOS parser selects the function at the first literal marker,
before parsing its arguments (`Compiler/PrsExp.HC:396-405`, dispatched from
`Compiler/PrsStmt.HC:1201`). The lookup filters for functions, so a global or
local value with the same name does not hide the output function. A directive
encountered during lookahead can publish a replacement, but the pending output
keeps its earlier target. The next output sees the replacement. An already
compiled function body keeps its own selected target.

The parser records the exact environment, command, selected function entry or
absence, and original marker. Only the live callback can enter this selection
in the task journal. Successful parsing attaches the exact completed implicit
statement. Source activation retains that journal entry and checks the exact
admitted header; it does not repeat name lookup in the later environment.
Missing or unfinished targets do not acquire a function from a later command.

Semantic resolution requires the original statement from the sealed command.
Its fixed expression, every trailing argument and nested call are checked
against that source. The original call objects must belong to the final
function or statement batch. Reconstructed calls, copied statements, omitted
arguments and duplicate uses of a source statement cannot certify output.
Runtime lowering checks the retained function metadata, exact declaration,
classified record and task function link before executing it.

[The executable example](../examples/stateful-exe-implicit-output.hc) emits
`AB` and returns 42 in both JIT and AOT modes. Its AOT directives execute in
the separate JIT directive task. It uses 101 runtime and 3 preparation
instructions in JIT, and 103 runtime and 3 preparation instructions in AOT.
Exact limits succeed; reducing either instruction allowance by one stops at
that limit. Tests cover selection before literal
concatenation, nested directives, generated source, retained function bodies,
absent targets before argument effects, and function-kind filtering.

Implicit calls also use saved integer defaults for trailing omitted parameters.
The remaining fixed parameters must all have defaults. Each value comes from
the original completed parameter in the selected header and task snapshot;
calling again does not repeat declaration-time evaluation. Function bodies and
pending calls keep their saved values after replacement. Ordinary AOT calls
can use closed integer defaults, while JIT task defaults can read and update
live storage. Narrow integer parameters still apply their normal entry store.

For example, `U0 Print(U8 *s,I64 n=++N){Out=n;}` saves the incremented value
when its header is parsed. A later `"saved";` assigns that value to `Out`
without changing `N`. The [defaults example](../examples/stateful-exe-implicit-defaults.hc)
also exercises replacement during lookahead and an earlier compiled body.
It returns 42 with empty output using 111 runtime / 9 preparation instructions
in JIT and 113 / 9 in AOT. Exact limits succeed; a limit one instruction lower
stops at that boundary.

Native `PrsFunCall` chooses available defaults without consuming expressions
in parenthesis-free calls (`Compiler/PrsExp.HC:455-470`). Source parsing uses the
original selected header shape to reject a supplied value at such a defaulted
slot before later lexer effects. This includes an empty marker followed by an
adjacent string. A required parameter after the omitted defaults also stops
before command lookahead. HCPARSE0164 identifies the unconsumed-argument
boundary; HCPARSE0165 identifies the missing required parameter.

The checked subset still excludes separator-only implicit omissions, general
parenthesized multi-argument implicit forms, general default values and argument
conversions. Some excluded omission forms are valid native syntax; these
diagnostics do not establish complete native parser parity. It does not
provide native extern linkage, general format/runtime parity, a
native backend, BIN loading or bootstrap. The pinned source comparison is
static evidence; no native execution capture is claimed.
