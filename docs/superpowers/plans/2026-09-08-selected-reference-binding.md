# Selected reference binding

Use the approved stateful #exe design and serial implementation workflow.
Independent review is read-only; root owns edits and executable use.

The parser freezes identifier lookup before later lookahead, but four Driver
walkers currently discard the exact AST occurrence into a name and origin.
Carry checked selections through local/module/outer binding and the duplicated
initializer walk. Parser-owned command views are the source authority.

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

- [ ] Add ledger reference observation and a sealed command/table/AST-owned
  reference view. Require the exact identifier and original command start.
  Reads are repeatable; observation rejects replay. Capture entry stage now,
  not through later mutable declaration completion.
- [ ] Add checked Sema selection evidence for explicit absence, required local,
  exact current source declaration/stage, and exact retained outer binding.
  Keep callback-free APIs on their established path. Missing receipts on the
  parser-aware task path are errors, not ordinary lookup requests.
- [ ] Preserve this evidence through function/top-level/global initializer/
  global dimension identifier events and occurrences. Local selections validate
  the existing source-ordered binding index. Module selections use exact source
  publications within the permitted prefix, not newest-name maps. Retained
  selections remain outer candidates until exact outer binding resolution.
- [ ] Validate physical environment/table/entry ownership for retained bindings.
  Reuse the same binding across initializer and top-level walks, retaining the
  original before-owner dimension and through-owner initializer boundaries.
- [ ] Verify real nested parser selections, selected absence, local suspension,
  old globals/functions after shadows, provisional non-promotion, initializer
  reuse and foreign or missing evidence before runtime effects.

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
