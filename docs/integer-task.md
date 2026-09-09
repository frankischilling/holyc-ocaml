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
`generated_bytes`, `executed_steps` and `initializer_steps` expose cumulative task results. The
existing complete-program APIs still allocate fresh execution images.

Each task retains a frontend view available through `Integer_task.frontend`.
Independent tasks sharing a session see baseline registrations and their own
declarations and definitions. Their local contexts are separate. The caller's
root session can inspect every publication once, in original order; it cannot
complete another owner's provisional header. Use the task frontend for
callback-free parsing that needs its exact function shapes and definitions.
`Session.fork_frontend` still creates a detached snapshot with fresh semantic
state; `Session.task_frontend` shares source files and semantic table identity.

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

Legacy AST commands publish new frontend entries when the VM admits them. This
lets syntax parsed in `Session.fork_frontend` feed `compile_ast`, `execute` and
later `run` calls: admitted globals become visible to conditionals, and functions
retain their bare-call, default-argument and variadic parser shapes. Runtime
omitted-default lowering remains outside the current integer execution domain.
Compilation alone and failed preflight publish no entries. A reached runtime
fault keeps admitted declarations and earlier effects; replay cannot publish
again or replace a newer source declaration.

An opaque VM admission receipt retains the exact task, compiled entry/storage
pair and source-ordered global/function publications. The ledger accepts only
its runtime's current receipt, once. It links fresh frontend entries to those
retained publications and their original function declarations, without matching
discarded parser entries by name or location. Parser-aware `run` keeps its early
declaration publication path. These receipts record actual legacy admission;
VM predecessor checks remain separate.

`run` also observes exact parser-selected identifiers. A sealed command retains
the original occurrence and command start, explicit local/absent selection,
source declaration stage and any already admitted runtime owner. Replayed,
missing, reconstructed and foreign reference evidence is rejected. A valid
selection without a runtime binding remains unresolved, even if a same-named
declaration is admitted later.

Selected bindings now pass through function bodies, top-level expressions,
global initializers and dimensions. Local references validate the existing
source-ordered local index. Current-unit references require the exact source
publication in the allowed prefix. Earlier commands use exact retained outer
bindings. Initializer and top-level walks reuse the same binding and environment;
neither repeats a name lookup after selection.

Native timing separates lexical identity from subsequent field reads.
`Lex.HC:494-509` saves the selected record pointer; global expression consumption
reads storage before advancing (`PrsExp.HC:870-898`). Terminating lookahead can
precede command admission, so an already buffered token may see that same record
admitted before consumption. After consumption, a new shadow does not replace
its selected storage. Function calls read parameter and target fields at later
phases (`PrsExp.HC:430-431,532-571`). The identifier-stage receipt does not freeze
all those fields: future extern/default integration needs separate receipts for
same-record joins at the later call phases.

The low-level `compile_integer_task_ast` API takes the owning VM task, validates
its semantic table and JIT mode before collection, and snapshots retained state
internally. Preparation uses that task's remaining cumulative budget, including
reached work on compilation failure. It returns an opaque checked program;
execution and frontend admission delivery remain explicit.
Parser-aware command seals retain their exact runtime owner. A sibling task
sharing the semantic table cannot compile the seal; the public reference
resolver also checks its snapshot's catalog owner. Different snapshots of the
same catalog remain valid. Semantic-only ledgers grant no runtime authority.
These owner checks are separate from pending predecessor admission checks.
This unit compiler does not provide `Integer_task`'s AST cache or overlap checks.
VM replay rejection belongs to each compiled entry; parser/source command
admission requires the higher-level orchestration.

## Query reads

Parser-aware commands retain separate receipts for `defined`, `sizeof` and
`offset`. A query owns its original command, environment and AST children.
`defined` preserves presence at operand consumption, including keyword entries
and non-name operands. Later declarations cannot fill a saved absence. Local
queries retain the original local binding and its warning-analysis use count.

Scalar `sizeof` reads checked metadata from the exact seeded primitive,
published source global or admitted retained record before following lookahead.
Metadata is separate from storage admission: `I64 N=sizeof N;` can read its own
published type. Pointer suffixes remain part of the original query. Arrays,
aggregate/member/local layouts and function debug extents still require their
own checked metadata. Unsupported reads report an explicit diagnostic.

Member queries have a distinct dot boundary before the member token. An
internal-type member rejects there; a scalar global can reach member-token
lookahead before rejecting unavailable member layout. Original dot and member
receipts prevent a later walk from moving either boundary.

Global dimensions and initializers retain ordered query manifests. Dimensions
bind before their owner is published; initializers bind through that publication.
Initializer descriptors retain exact source leaves, including non-name `defined`
operands. The repeated initializer walk consumes the same selected objects and
does not call its external query resolver again. An AST-only descriptor has no
selection; missing descriptors are errors. Reordered, omitted, repeated, foreign
or reconstructed source evidence cannot replace the original manifest. Constant
layout evaluation consumes checked results for actual query nodes without
creating replacement literal ASTs or rerunning name lookup.

These checks do not provide full source execution. Array and member preparation,
implicit providers, default arguments and later call-target reads remain separate
integration work.

## StreamPrint generation service

The task service exposes opaque `begin_stream`, `finish_stream` and `abort_stream`
tokens for the parser executor. Checked `extern U0 StreamPrint(U8 *fmt,...);`
calls append to the active buffer through the existing formatter and owned
pointer checks. Source-defined replacements execute their own bodies. Calls
without an active buffer perform bounded formatting first, then report HCIRVM0027.
Formatting faults and work charges precede that diagnostic. No inactive draft is
committed. This follows the ordering in `CMisc.HC:68-80`; native code logs the
missing-block error and returns, while this hosted service returns a diagnostic.
Argument effects that already ran remain reached.

Nested buffers are separate. Finishing returns only that buffer's text and
resumes its parent; aborting returns no text. Foreign, consumed and suspended
tokens cannot finish or abort another buffer. Retained functions use the current
active buffer while preserving their original provider selection, storage and
mutable literals. Ordinary Print and PutChars continue to use ordinary capture.

All output shares `max_output_work`. Generated bytes have their own cumulative
`max_generated_bytes` limit, defaulting to 16 MiB, and active stream depth has a
`max_stream_depth` limit, defaulting to 64. Successful StreamPrint fragments stay
charged after finish or abort; `generated_bytes` reports that count. A failed
formatting call commits no bytes and retains its reached work charge. The parser
also checks bytes at injection, a distinct boundary. Zero generated capacity
allows empty buffers. Exceeding generated capacity reports HCIRVM0028; exceeding
active depth reports HCIRVM0029. Ordinary byte and PutChars prefix rules remain
unchanged.

These APIs are the checked runtime service. Automatic service declarations,
parser callback orchestration and native partial declaration/initializer timing
remain part of the complete #635 connection below.

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
remaining query metadata, default/provider and later call-phase receipts,
VM pending-command authority and the fourteen
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
executable installation at these points.

Parser command receipts now prove whole-command membership and parser order.
Each source context retains its exact input/environment and suspended parent
phase; starts retain the exact completed predecessor. Declarations and expression
selections own their command start. The ledger accepts only original complete
command views or the parser's successful sequence AST, rejecting reconstructed
modules, subsets, reordered events and overlapping statement/declaration views.
This records parser order without granting VM admission or execution.

Sequence sealing waits until its completion callback returns successfully.
Rejected or exceptional completion cannot leave a sealable sequence, and abort
releases the context without losing its parent. Earlier complete command views
survive. An accepted child syntax sequence also survives later generation or
parent failure. Terminating statement lookahead can reach directives while the
command is still being read; global completion can precede the next lookahead.
Receipts retain those existing grammar boundaries.

Header completion preserves newer shadows and already consumed reference
snapshots. It updates only unconsumed tokens selecting the exact provisional
function. Global alias candidates retain the selection made at the name token,
before directives in dimensions. These frontend controls do not demonstrate
stateful #exe execution.

The reference remains `c26482bb6ad3f80106d28504ec5db3c6a360732c`. The hosted task
tests do not constitute a new native TempleOS capture.
