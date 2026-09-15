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
Missing selections cannot acquire a function from a later command. A selected
unresolved extern can reach its exact joined definition after publication,
while keeping its captured header and arguments; see
[extern call publication](integer-extern-calls.md).

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

Implicit calls also use saved integer defaults for omitted parameters after
the initial supplied output argument. Each value comes from
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
adjacent string. Each fixed Print parameter processes its own separator.
For `U0 Print(U8 *s,I64 saved=40,I64 required)`, the statement `"x",,2;`
uses the saved 40 and supplies 2 to `required`. A trailing separator can omit
a default too: `"x",;` for a header with one trailing default.

The AST retains each omitted formal position, consumed comma when present,
and the unconsumed lookahead location. Semantic binding reads these from the
same original statement used by target selection. Supplied expressions stay
in source order and are assigned around the omitted slots. Human and JSON
AST dumps expose the omission evidence. The
[omissions example](../examples/stateful-exe-implicit-omissions.hc) checks
both top-level and function-body calls and returns 42 with empty output.
It uses 112 runtime / 9 preparation instructions in JIT and 114 / 9 in AOT.
Exact limits succeed; each allowance one instruction lower stops with
HCIRVM0007 and empty capture.

A missing required parameter stops before command lookahead. HCPARSE0164
identifies the unconsumed-argument boundary; HCPARSE0165 identifies the missing
required parameter. Callback-free legacy AST inputs retain separate semantic
arity diagnostics.

An opening parenthesis after an empty marker starts an implicit argument list:
`""("format",40,2);` and `''(40,2);` retain the original call delimiters
separately from any expression groups inside them. Supplied fixed values can
override saved defaults, including the first parameter. Interior and trailing
omissions use their original formal positions and lookahead locations. Calls
retain their selected header across closing-parenthesis lookahead and across
separate task inputs.

The pinned separator rules differ by target. Print requires a comma before each
trailing default, so `""("format",,)` selects two defaults; `""("format")`
does not omit them. PutChars can select all trailing defaults at the closing
parenthesis, as in `''(40)`. A parenthesized Print call with fixed parameters
and a variadic tail requires a supplied variadic argument. PutChars permits an
empty variadic tail. These rules follow `Compiler/PrsExp.HC:438-535`.
HCPARSE0167 reports an invalid separator or closing delimiter before later
lexer effects. The [parentheses example](../examples/stateful-exe-implicit-parentheses.hc)
checks saved and supplied arguments through top-level and body calls and returns
42 with empty output in JIT and AOT.
It uses 145 runtime / 12 preparation instructions in JIT and 147 / 12 in AOT.
Both exact limits pass together; either allowance one instruction lower stops
with HCIRVM0007 and empty capture.

Implicit calls can also omit the initial fixed parameter, as in `""(,2)`
for `Print(I64 saved=40,I64 required)`. The omission has formal index zero,
no consumed leading comma, and the original unconsumed lookahead. `''()`
can select all PutChars defaults, while Print still needs a comma before each
subsequent default. Empty markers without parentheses select available initial
defaults without consuming a supplied expression. A zero-parameter header can
be called with `""()` or `''()` and with an empty marker followed by `;`.
A comma after a completed nonvariadic absent-value call belongs to the enclosing
statement sequence, as does a comma after a parenthesized call. It is not a new
argument. This follows `Compiler/PrsStmt.HC:920-922,1201-1214`; the next statement
and any directive it reaches retain their original sequence boundary.

The AST explicitly marks an absent initial value. No placeholder literal or
expression root is created. Both semantic binding paths retain optional supplied
values. Top-level outputs belong to the original containing statement even when
they have no expression roots. Missing, swapped, copied or duplicate source
groups cannot certify a call. A supplied initial expression cannot also carry
omission zero. Legacy supplied-value constructors and accessors remain available;
general consumers use the option accessors for absent values.

The [absent-values example](../examples/stateful-exe-implicit-absent.hc) checks
saved first defaults, retained function bodies and zero-value calls together.
Built and installed JIT/AOT runs return 42 with empty output. It uses 143 runtime
/ 9 preparation instructions in JIT and 145 / 9 in AOT. Exact limits pass
together; either allowance one instruction lower stops with HCIRVM0007.

Without parentheses, PutChars consumes required fixed arguments as adjacent
expressions: `''20 20 2;` supplies three values. Defaults consume neither an
expression nor a comma. For `PutChars(I64 a=40,I64 b)`, `''2;` and `'A'-63;`
both use the saved 40 and supply 2 to the second formal. Several initial defaults
can retain the same unconsumed marker as lookahead before a later required
parameter consumes its original expression. The marker is not reparsed.

Adjacent arguments have no comma location in the original AST or semantic
input. The JSON dump records `comma: null`; the human dump marks an adjacent
separator. Original comma-bearing constructors remain available, while general
source consumers use the optional separator accessors. Separator checks reject
adjacent arguments attached to parenthesized syntax, and legacy implicit-call
constructors cannot admit adjacent values without the original source.

Expression boundaries still apply: `''40-2;` is one subtraction expression,
whereas `''40 2;` supplies two adjacent expressions. A comma after all fixed
parameters belongs to statement sequencing. A comma before a remaining required
parameter fails. Without parentheses, PutChars does not consume variadic values.
These rules follow `Compiler/PrsExp.HC:438-531` and the enclosing statement parser.

The [adjacent-values example](../examples/stateful-exe-implicit-adjacent.hc)
checks initial/interior saved defaults, original marker expressions and retained
body calls together. Built and installed JIT/AOT runs return 42 with empty output:
139 runtime / 9 preparation instructions in JIT and 141 / 9 in AOT. Exact limits
pass together; either allowance one instruction lower stops with HCIRVM0007.

Source-defined variadic functions now execute integer tails through ordinary,
implicit and retained calls. Each invocation gets its own writable I64 `argc`
and `argv` array over the actual tail values. The declared 127-element extent is
the arbitrary placeholder in `Compiler/PrsVar.HC:373-409`; it does not allocate
127 cells or restrict longer tails. Zero tails allow an empty array address but
reject element access. Changing `argc` does not change the owned array extent.
Recursion and pointer forwarding preserve each invocation's storage and lifetime.
The I64i/I64 cell conversion follows the public union in `Kernel/KernelA.HH:105`;
it preserves exact source identities and object bounds at pointer conversions.

Frame quotas include fixed slots, the hidden count, actual tail words and local
storage. Limits are checked before allocating the tail. The
[variadic example](../examples/stateful-exe-variadic.hc) combines a saved default,
top-level implicit output and a retained body's variadic call. JIT and AOT return
42 with empty ordinary output, using 106 / 6 and 108 / 6 runtime / preparation
instructions respectively. Both exact limits pass together; either allowance
one instruction lower stops with HCIRVM0007.

General default values, argument conversions and remaining implicit argument
boundaries remain incomplete. Source variadic tails currently require integer
values; floating point and machine-address tail representations remain outside
the hosted subset. Native extern linkage, general format/runtime parity, a native
backend, BIN loading and bootstrap are unfinished. The pinned source comparison
is static evidence; no native execution capture is claimed.
