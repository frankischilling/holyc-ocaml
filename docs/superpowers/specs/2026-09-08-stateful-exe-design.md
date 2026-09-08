# Stateful #exe execution

Issue #635 connects the integrated parser to persistent task execution and
generated source. The reference is
`c26482bb6ad3f80106d28504ec5db3c6a360732c`. All eight issue gates currently
fail with HCPP0008 in both modes at merged `2c966212f7749185ead749b8dcf3cdb793c13325`.
The complete compiler mission remains active beyond this connection.

## Command lifetime and native timing

The parser must expose commands without collecting a replacement whole-file
preprocessor. A command has distinct parsing, preparation, publication and
execution boundaries. Lexer requests can enter a nested #exe while an outer
command is pending. A global declaration initializer can execute during parsing;
an ordinary statement executes after its terminating lookahead returns.

`ExeCmdLine` in `Kernel/KTask.HC:337-344` calls `LexStmt2Bin` before executing
its code. `PrsStmt.HC:1209-1211` lexes past an expression semicolon first.
Thus JIT `I64 N=0;N=1;#exe {StreamPrint("%d;",N);}` generates `0;`, then
executes the pending store, then evaluates zero. `PrsVar.HC:97` executes a
global initializer during parsing instead. This distinction needs maintained
source tests, not just callbacks counted at completed-AST boundaries.

Function headers become visible before executable bodies are installed.
`PrsStmt.HC:104-108,183-184,929` makes a directive immediately after a function's
closing brace observe an unfinished callable; an intervening semicolon lets
installation finish. Preserve that state and provide an explicit hosted
unfinished-call diagnostic. Partial surrounding globals and functions must
survive generated text; a block is not a synthetic ordinary function.

## Task and output namespaces

`PrsStreamBlk` (`PrsStmt.HC:805-841`) saves hash context, clears current
function/local bindings, selects task hash tables, clears AOT and assembly
expression flags and enables EXE mode. Blocks inside AOT therefore run as task
JIT commands. Outer JIT globals are task-visible; AOT output globals are separate.
Generated source is parsed in the restored outer context.

Retain task definitions, symbols, options, callable publication and memory
across commands and blocks. A fresh session owns fresh state. Restore the native
selected mode flags, not an invented snapshot of all compiler options. Hosted
fault cleanup restores the parser context deterministically; the native block
has no local exception-cleanup contract to copy.

`StreamExePrint` is a separate explicit bridge to saved lookup context. Its
source tests EXE mode, despite its AOT-worded diagnostic. AOT compiler staging
globals and emitted data are distinct, and AOT function offsets are not loaded
callable addresses. General bridge execution remains explicitly unsupported
until this distinction is implemented.

## Published commands and retained identities

Use an opaque task compiler and independently sealed command contexts. A task
catalog retains exact published symbols, declaration/header snapshots, compiled
bodies, frames, static locations and storage objects. New commands resolve
against this catalog and their current declarations. Existing bodies and
selected call sites remain unchanged when a later declaration is published.

Retain a task module scope and ordered publication epochs. Command-local
declaration views may use that scope; checked publication seeds must preserve
extern/definition joins. Ordinary module-publication constructors must not
accept a foreign symbol merely because its name matches. Add explicit outer
call and storage evidence where the current pipeline only retains semantic
outer lookup.

The existing `Outer_environment`, `Outer_expression_binding` and
`Top_level_outer_expression_binding` are the lookup seams. Extend function
entries with exact selected header/declaration metadata, reuse direct argument
binding and conversion rules, and retain that selection through IR call
classification and runtime sealing. Outer storage addresses require the same
entry-to-object proof. A service binding includes an explicit provider capability;
source-defined replacements retain their ordinary body.

A pending command retains its exact predecessor epoch and source AST. Publication
commits only the validated additions. Commands are consumed once; stale, duplicate
or foreign plans fail before effects. Configuration/preflight failures differ
from reached execution faults. Reached writes survive, but unfinished commands
are not silently retried. Retyping a prefix must never replace already published
identities or repeat initialization; the implementation uses command deltas
instead of a cache layered over complete-prefix recompilation.

## Persistent runtime objects

Global/static flattened indices and literal instruction IDs are local to a
checked command. They cannot identify persistent cells. Each task object owns
a stable region, exact declaration identity, type, extent and unknown-cell state.
Each command maps its verified local storage positions to those regions.
Appending a global must not move an earlier static or invalidate an address.

Retained literal-site tokens identify exact source occurrences and declaration
owners. A checked manifest binds each token to the current literal instruction
and immutable original bytes. Equal strings remain distinct sites; mutated bytes
survive later commands. Names, spans and graph-local IDs alone grant no authority.

Initialization history is separate from graph-bound initializer receipts.
Published declarations retain their preparation results and executed leaves;
only new command effects enter preparation/scheduling. Preserve the existing
ordering in which a reached prepared JIT array leaf may publish before the next
instruction-budget check. Quotas cover accumulated live global/literal storage
and cumulative runtime/preparation work. SSA values, call scopes and return
latches remain local to an execution. Every completed or faulted activation is
invalidated while task regions remain live.

Keep existing `execute_program` and report APIs fresh by using disposable task
state for ordinary isolated execution. The persistent interface remains opaque
and exposes neither writable cells nor caller-supplied completion flags.

## Stream generation

The parser owns grammar and input positions. A configured executor receives
parsed task commands and returns a completed generated byte buffer. It never
scans braces or reparses raw command text. The frontend supports an executor
callback without depending on Driver or IR modules.

Each active block owns a buffer. `CMisc.HC:68-80` appends formatted calls in
execution order and injects only after the block completes. Nested blocks return
to the immediately enclosing task/parser context. Reuse `Integer_output`'s
checked format domain with a separate generation sink and shared work accounting.
Ordinary Print capture remains separate. Failed blocks inject no partial body;
later errors after successful injection do not undo earlier generated commands.

Generated lexical frames retain the directive, nested include/definition origins
and exact generated ranges. They insert no separator: `#exe {StreamPrint("4");}2;`
must form the integer token 42. Generated-byte and nested-generation limits are
shared across the complete stream. Native initializer rewinds need separate
evidence; this design does not assert universal once-only lexical evaluation.

## Validation and integration

Execution-aware parsing freezes identifier lookup when each token is produced,
before later lookahead can execute a directive. An opaque receipt retains the
exact AST identifier occurrence, environment and selected entry. The task
compiler must resolve that receipt to its retained semantic publication rather
than look up the final spelling again. Selected absence and local shadowing are
also decisions. `PrsExp.HC:862-866` retains the callee record before lexing ahead.
Function header and completed-declarator callbacks must publish semantic state
at the native boundaries described above. A completed whole item alone cannot
publish the first declarator before a second declarator's #exe initializer.

Extend selection evidence to query operands before supporting directives inside
sizeof/offset/defined queries. `PrsExp.HC:317-329` computes sizeof from its
selected record before subsequent lexing; `942-947` similarly evaluates defined.
The current identifier-expression receipt is not authority for these other
AST forms. Keep this integration requirement explicit while the VM connection
is incomplete.

The opening brace is obtained in the outer environment before switching task
lookup (`Lex.HC:1031-1034`). This includes a brace produced by a definition.
Definition recursion checks across environments must use retained definition
objects because numeric IDs are environment-local. Execution-aware parsing
stops at the first error instead of pulling more directives during recovery;
ordinary callback-free syntax recovery keeps its existing behavior.

Maintain the eight API/CLI issue gates plus timing, namespace, partial
declaration, nested-generation, cross-frame token, old-function/static/literal,
fault and malformed-authority controls. Measure a combined fixture and every
resource bound with exact and one-below controls. Verify fresh sessions and
separate ordinary/generated output, versioned reports and deterministic dumps.

Run the full suite, packaging, 82 checksums, eleven provenance scenarios and
corpus comparisons. Any corpus change must be explained by real source behavior;
do not replace a baseline merely to obtain a passing check. Independent review
and all five final-source checks precede normal protected merge. Verify the
merged executable, post-merge CI and synchronized Git afterward.
