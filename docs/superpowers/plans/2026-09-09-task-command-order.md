# Task command admission order

> Use superpowers:executing-plans for serial implementation. Independent reviews
> are read-only; root owns edits, builds and executable use.

The existing stateful #exe design requires pending commands to retain original
parser order through VM admission. Continue on `feat/635-stateful-exe`; preserve
reference `c26482bb6ad3f80106d28504ec5db3c6a360732c`, package identities,
dependencies, legacy pending commands and all existing execution/resource gates.

Use the original parser resume event, after terminating lookahead, to make each
command ready. Commands in nested contexts share their parser-root order; separate
parse roots remain independent. A shared task-owned order ledger retains exact
contexts, command and accepted sequence receipts. Whole-sequence admission must
satisfy every intervening nested command. Empty sequences retain replay identity.

Carry the proof in the task storage view, then bind it to the final entry,
initialization, runtime calls and function bodies before exposing compiled output.
VM preflight checks order and the exact compilation bundle without consuming it.
The existing admission callback commits original source identities. Reached faults
consume commands; failed preflight leaves them pending. Actual parser JIT mode is
required. No caller-supplied completion flags or name-based association is used.

- [x] Reproduce reversed admission, early execution and recompilation replay with
  real parser receipts and the low-level compiler/VM API.
- [x] Implement the internal order ledger and checked driver/storage/VM joins.
- [x] Cover nested lookahead, independent roots, empty sequences, substituted
  bundles, foreign/mode errors, preflight rejection and reached faults.
- [x] Run focused/full/CLI, format/generated/build/install, pinned checksum,
  provenance and corpus verification; resolve independent review findings.
- [ ] Update docs and the external prompt, commit and push a useful checkpoint,
  then verify exact source identity and hosted checks. Keep #636 draft while
  the fourteen original #exe gates remain unfinished.

Partial declaration/initializer execution, effectful dimensions, remaining
metadata and source-read phases, extern joins and production source orchestration
remain required. The complete compiler, native/BIN/loader and bootstrap objective
also remains active.

Working-source verification passes 2,278 of 2,292 groups in 47.984 seconds;
only the fourteen existing stateful #exe groups fail. The complete CLI checks
pass. Ten new order groups and all 198 focused task/parser controls pass.
Formatting, generated source, all/install builds, 82 reference checksums and
eleven incremental provenance scenarios pass. Full lexer/parser JSON and
normalized serialized parser output match the existing baselines: 528/528
tokenize, 25 parse standalone and 126 with the prelude.

Independent read-only design, production and test review found no remaining
blocker. Initial controls reproduced reversed admission, early execution and
source replay before implementation. Existing nested-execution test plumbing
now executes at the original resume event. Final-source identities and hosted
checks are recorded in issue #635 and the external prompt after pushing.

A final API audit reproduced receipt omission through legacy compilation. The
compiler now rejects already observed source ASTs and wrappers borrowing their
original items before collection or preparation when the seal is missing. The
same registered input can still be parsed afresh through the callback-free API.
Independent re-review found no remaining blocker after this regression and fix.
