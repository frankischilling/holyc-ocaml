# Selected reference binding

Use the approved stateful #exe design and serial implementation workflow.
Independent review is read-only; root owns edits and executable use.

The parser fixes identifier lookup before later lookahead. Checked selections
must survive local/module/outer binding and the duplicated initializer walk.
Parser-owned command views are the source authority; reducing an occurrence to
name and origin loses the selected declaration.

## Required dependency: legacy AST admission

- [x] Reproduce missing frontend visibility after a genuine Parser AST from a
  forked frontend is compiled and executed by Integer_task. Preserve existing
  compile_ast -> execute -> run interoperability and the callback-free API.
- [x] Issue an opaque VM admission receipt only at actual command admission.
  Retain exact global/function publications in source order, exact task and
  compiled storage ownership, and original function metadata. Failed preflight
  produces no receipt; reached faults retain one; replay cannot issue another.
- [x] For the legacy AST path, publish new frontend entries linked to those
  admitted publications. Never recover a discarded parser receipt by matching
  names, origins, spans or IDs. Compilation alone must not publish task entries.
  Keep parser-aware source declarations on their existing publication path.
- [x] Verify forked frontend global conditionals, bare calls, default/variadic
  parser shapes, ordering/shadowing, failed/unexecuted commands, reached faults,
  foreign receipts and replay. Runtime omitted-default lowering stays pending.
- [x] Make checked task compilation validate its owning table and JIT mode before
  mutation and charge cumulative VM preparation for pending and failed units.
  Reuse this entry point from Integer_task to preserve existing resource counts.

The visibility controls reproduced missing globals and bare-call shapes
(H9TA6ECG). Read-only review found stale receipt delivery could reverse frontend
header order (3TVFRCH4); the ledger now requires the exact current VM admission.
The low-level compilation control caught uncharged preparation (7BS6YP6Y).
Owned task compilation now supplies the snapshot, remaining budget and progress
callback internally. The resulting 131 task/parser/ledger/service groups pass.
Final read-only review found no remaining bridge blocker. The full suite passes
2,185 of 2,199 tests in 70.102 seconds (SJ68K1TQ); only the fourteen original #exe
groups fail. The raw task-unit compiler does not supply Integer_task's source
AST cache/overlap protection; that public API boundary is documented.
CLI checks, formatting, generated files, build/install, 82 pinned checksums and
eleven incremental provenance scenarios pass. Complete lexer JSON and parser
JSON/normalized text match the existing baselines: 528/528 lexes, 25 standalone
parses and 126 with the prelude. Commit rebuilding and hosted results belong to
the external checkpoint; this dependency does not complete selected semantics.

## Preserve selected source occurrences

Removing name fallback reproduced frontend leakage between independent tasks
sharing one Session. Globals, function headers and definitions need a persistent
task publication owner. The root environment keeps aggregate inspection;
task views read unowned baseline entries and their own entries only. IDs and
publication order belong to one shared store, entries are allocated once, and
header completion requires exact entry membership and writer ownership. Local
contexts belong to each view. Detached copies preserve only the visible snapshot.
Session.task_frontend shares sources and semantic table identity, unlike the
fresh semantic state in fork_frontend. Integer_task retains and exposes this view.
The three failures reproduced in 4WJOK0KE pass in AGCI48KS (86 focused groups).

Native timing distinguishes lexical identity from later field reads. Lex.HC
494-509 saves the chosen record pointer. Global consumption reads its storage
before following Lex (PrsExp.HC:870-898). Terminating lookahead precedes command
execution (KTask.HC:339-343), and closing-brace lookahead precedes function code
installation (PrsStmt.HC:929,183-191). Therefore a buffered token may see its same
record admitted before consumption; do not freeze VM availability at token pull.
After consumption, later shadows cannot replace the chosen global storage.

Function calls have additional native boundaries: PrsExp.HC:865-866 advances
after the selected name, reads parameters at 430-431, and reads target
classification/code after argument lookahead at 532-571. A same-record extern
join during those reads may update fields without changing selected identity.
Keep separate call-phase receipts in the extern/default integration; do not turn
an identifier-stage snapshot into a claim that every function field is frozen.
The source control is `extern I64 F(); F #exe {I64 F(){return 42;}};`, distinct
from a nonextern same-spelling shadow. This is source analysis, not a new native
execution capture.

- [x] Add ledger reference observation and a sealed command/table/AST-owned
  reference view. Require the exact identifier and original command start.
  Reads are repeatable; observation rejects replay. Capture entry stage now,
  not through later mutable declaration completion.
- [x] Add checked Sema selection evidence for explicit absence, required local,
  exact current source declaration/stage, and exact retained outer binding.
  Keep callback-free APIs on their established path. Missing receipts on the
  parser-aware task path are errors, not ordinary lookup requests.
- [x] Preserve this evidence through function/top-level/global initializer/
  global dimension identifier events and occurrences. Local selections validate
  the existing source-ordered binding index. Module selections use exact source
  publications within the permitted prefix, not newest-name maps. Retained
  selections remain outer candidates until exact outer binding resolution.
- [x] Validate physical environment/table/entry ownership for retained bindings.
  Reuse the same binding across initializer and top-level walks, retaining the
  original before-owner dimension and through-owner initializer boundaries.
- [x] Verify real nested parser selections, selected absence, local suspension,
  old globals/functions after shadows, provisional non-promotion, initializer
  reuse and foreign or missing evidence before runtime effects.

The selected-global runtime control returned 102 instead of 42 before semantic
integration (5NPN1Q7F); the top-level path passed in TTATTOYJ. Nested function
shadows retain their original call shape and executable. Function bodies and
initializers preserve their selected globals/functions; locals remain local.
Selected absence cannot acquire a later nested publication, while that reached
publication remains available to subsequent commands. Dimension controls verify
exact older bindings, original before-owner prefixes and foreign entry/table
rejection. Missing reference receipts reject before runtime effects.

Read-only review found a second-pass initializer environment mismatch: distinct
environments sharing exact tables could reuse the same binding. The negative
control reproduced this in 1RHK2Q2W; validation now requires the original
initializer environment as well as the exact binding. Complete call-phase
receipts, extern history and source orchestration remain outside this checkpoint.

A second review confirmed that table identity alone allowed a parser command
to transfer between sibling runtimes. The identifier-free `40+2;` control
reproduced the gap (AOXJ5MO9). Command seals now retain the ledger's runtime;
checked compilation rejects foreign and semantic-only seals before preparation.
The public reference resolver separately checks snapshot catalog ownership,
while permitting different snapshots of that same owner. Runtime ownership is
distinct from the still-pending VM predecessor admission rule. The revised
171 focused groups pass (81BO2LA1).

Final read-only review found no remaining blocker in the initializer or runtime
owner fixes. The full Dune suite passes 2,200 of 2,214 groups, with only the
fourteen original #exe failures. An unbuffered run (J21AYOF2, 70.807 seconds)
verifies every passing/failing result; default Dune output truncates the list.
Use `dune runtest -j 1 --no-buffer` for that audit: a direct whole-suite launch
from the repository root cannot find the test-directory fixtures.
Formatting, generated files, build/install, 82 pinned checksums and eleven
provenance scenarios pass. Complete lexer JSON and parser JSON/normalized text
match their existing baselines: 528/528 lexes, 25 standalone parses and 126 with
the prelude. Committed-source rebuilding, CLI reports and hosted results are
recorded in the external prompt and #635/#636 checkpoint.

## Following full #635 integration

Parameter defaults have a separate binding pass not currently called from
Integer_source. Queries and implicit Print/PutChars need their own parser
receipts at native lookup points. A function selection must retain exact source
declaration/header/classification, not only canonical identity. Joined history
needs a separate retained path: task catalog deduplication and outer-table
validation currently collapse/reject repeated canonical symbols. Do not merely
disable deduplication or fabricate identifier, extern or partial-source ASTs.

Partial type/storage/header admission, native initializer execution, extern joins,
VM predecessor admission and the source-execution facade remain required. Keep
ordinary no-#exe resource counts, task/AOT namespaces, caller limits before parse,
reached output on compile errors and cumulative result/artifact ownership.

For each implementation checkpoint, run meaningful RED/GREEN controls, review,
full tests, build/format/generated checks, 82 reference checksums, eleven
provenance scenarios, exact corpora and committed-source CLI reports. Push and
inspect hosted results; update #635/#636 and the external prompt while preserving
all 102 examples and all unfinished full acceptance criteria.
