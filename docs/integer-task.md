# Incremental integer task execution

`Holyc_lib.Integer_task` compiles separate JIT commands against retained global
objects and functions. For example, running `I64 N=40;`, then `N+=2;`, then `N;` in one task
returns 42 from the last two commands. The initializer runs once. Fixed arrays,
narrow integer storage, updates, references passed to newly compiled functions,
and checked Print/PutChars arguments use the same storage rules as a complete
integer program. Defining `I64 Add(I64 a,I64 b){return a+b;}` makes
`Add(20,22);` available to later commands and newly compiled function bodies.

Create a task with `Integer_task.create session`. Add each source to that session
and call `Integer_task.run task ~source`, or pass a parsed module to `compile_ast`
and execute the resulting opaque command. `output_bytes`, `output_work`,
`executed_steps` and `initializer_steps` expose cumulative task results. The
existing complete-program APIs still allocate fresh execution images.

## Ownership and timing

Compilation captures an immutable view of the task's published declarations. Each global
entry retains the exact semantic symbol, declaration metadata and final storage
object. A checked outer identifier emits a `Retained_global` IR payload carrying
an opaque reference. A spelling, numeric symbol ID or ordinary Symbol payload
cannot select an earlier command's object.

The VM retains each admitted command's fixed-size allocation. Old arrays and
scalars keep their original storage identity and offsets after later allocations.
Earlier commands never contribute new initializer work to a later command.
Unknown JIT cells remain unknown until reached writes initialize them.

Retained function calls carry the selected declaration and classification
snapshot through ordinary argument binding, conversion and graph sealing. An
opaque task link selects the admitted executable. Each body keeps its original
callee table, global/static objects and mutable literal image across calls and
returns. New function names or reused command-local indexes do not replace those
owners. Joined extern/definition identities within one command preserve the
distinct canonical callable and definition header. An earlier unresolved extern
selection cannot acquire a later executable through a matching name.

Later global and static initializers can call retained definitions. The existing
arithmetic and update guards inspect those bodies and their transitive callees
with the original frame, compiler options and storage context. Guard inspection
does not execute a body or grant runtime call authority.

Every body and entry passes preflight before new storage becomes visible.
Preflight failure leaves runtime cells, output and execution receipts unchanged.
Compilation preparation is a separate phase: work already performed there stays
charged even if later compilation or preflight fails. Once execution starts,
success and faults both consume the command. Reached writes and output remain.
Recompiling the exact parsed command, including module/item/statement wrappers
around its retained contents, cannot execute it again. Separately parsed commands
may contain identical text.

A pending command keeps its selected object or function header even if another admitted command
shadows that name. Thus an earlier compiled `N+=2;` still updates the earlier N,
while a newly compiled `N;` selects the newer declaration. Parser-aware pending
and predecessor receipts are still needed before connecting this API to #exe.

## Parser-assigned declarations

`Integer_task.run` assigns global and function symbols at the parser's native
publication events. Completed commands reuse those exact symbols in one task
module scope. The ordinary type, frame, call and IR checks then process the new
command. Earlier admitted objects remain explicit retained outer bindings even
though their symbols share that scope.

`Task_declarations` records private parser events and exposes a checked
declaration collection for an exact AST and semantic table. It retains the
source manager, registered input object, provisional and completed parser
entries, original source children and publication order. Nested commands can
occupy gaps in that order; a later seal cannot reverse the original declarations.
All associations are checked before any publication is claimed. Replayed or
out-of-order events, substituted children and foreign source owners are rejected.

Semantic publication does not allocate runtime storage or install code. For
example, `I64 Broken=;` leaves a reached semantic symbol after its syntax error,
but a later command cannot read it as an admitted global. A completed `I64 N;`
receives storage through normal preflight and keeps its unknown-cell state until
a reached assignment. Parsing must use the exact registered Source_file object.
The existing `compile_ast` API keeps its callback-free collection path.

## Bounds

Positive creation limits cover cumulative runtime instructions, constant
preparation, global bytes, literal bytes, output bytes and formatting work.
Preparation includes pending commands and reached constant-evaluation faults;
commands requiring no preparation remain possible when that budget reaches zero.
Frame bytes and call depth bound active calls. Rejected runtime admission does
not charge new global/literal allocations. Literal limits include every site's
terminator, including unreachable sites. Function activations are invalidated
when they return or unwind; admitted task allocations survive command completion.

`test_integer_task.ml` covers separate scalar/array commands, narrow updates,
unknown cells, independent tasks, selected references after shadowing, calls and
formatting with retained arrays, preflight/fault behavior, replay and cumulative
limits. Function controls include recursion, nested owner restoration, literal
mutation, declaration snapshots, checked providers, rejected argument lists,
initializer guards and admission versus reached-fault publication. Its preparation
controls include a configured limit above 100,000.

## Remaining #635 work

StreamPrint is not yet connected to parser generation. Cross-command extern
joins, partial type/storage/header publication and initializer execution,
selected query receipts, parser pending-command authority and the fourteen
maintained #exe execution groups remain part of issue #635. The existing integer
function domain remains unchanged, including its pointer-return boundary.

The streaming parser now exposes private declaration events for provisional
globals and functions, completed global declarators, function headers and bodies.
A global is visible after its dimensions and before its initializer; its
completion precedes lookahead into the next declarator. A function is visible
before parameter defaults, but its header completes only after lookahead past
the closing parenthesis. Body completion follows the full statement sequence
and terminating lookahead. Assigned symbols now survive completion; consumers
still need partial type/storage/header publication, initializer execution and
executable installation at these points. Whole-command membership/grouping and
pending/predecessor receipts remain separate from declaration association.

Header completion preserves newer shadows and already consumed reference
snapshots. It updates only unconsumed tokens selecting the exact provisional
function. Global alias candidates retain the selection made at the name token,
before directives in dimensions. These frontend controls do not demonstrate
stateful #exe execution.

The reference remains `c26482bb6ad3f80106d28504ec5db3c6a360732c`. The hosted task
tests do not constitute a new native TempleOS capture.
