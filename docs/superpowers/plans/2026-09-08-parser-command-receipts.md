# Parser command receipts

Use the existing stateful #exe design and serial implementation workflow.
Independent review is read-only; root owns edits and executable use.

The declaration ledger currently accepts reconstructed statement-only modules.
Give it parser-owned whole-command and whole-sequence witnesses, preserving the
existing initial-lookahead/resume timing and nested task environments. This is
parser lifecycle evidence; VM admission and execution remain separate.

- [x] Reproduce statement subset, reordering and wrapper acceptance through the
  existing parser and ledger APIs.
- [x] Issue private context/start/completion/resume/sequence receipts. Record
  exact source/environment, predecessor and the parent's suspended phase.
  Attach declarations and expression selections to their owning command start.
  Return the parser's original sequence module and expose complete command views.
- [x] Consume receipts in Task_declarations before declarations, validate phase
  and replay, and seal only exact parser-owned views. Preserve atomic rejection
  and require every original command to be unclaimed, including statements.
- [x] Connect Integer_task.run and verify nested lookahead, incomplete/error
  paths, callback rejection, source/environment ownership and cross-view replay.
- [x] Run focused controls, independent review, full tests, build/format/generated
  checks, pinned checksums, provenance, exact corpora and existing CLI gates.
- [ ] Commit/push and inspect exact-source hosted checks. Update the external
  prompt and #635/#636 with evidence; keep unfinished acceptance criteria open.

Selected-reference semantic consumption, partial initializer/storage/function
admission, extern joins, VM predecessor admission and actual source execution
remain required. No native reference or callback-free AST schema changes.

The statement subset regression failed before implementation (EVGO70QN). Review
then reproduced a sealable sequence after late callback rejection (45IYUYJ0).
Parser-owned acceptance now becomes valid only after successful callback return;
the ledger checks it before its seal cache and handles tentative abort without
popping the parent. Re-review found no remaining blocker. Controls include root
rejection/exception and nested rejection with an active declaration parent.

The final full suite passes 2,176 of 2,190 tests in 66.915 seconds (I5PE385A);
only the fourteen original #exe execution groups remain RED. CLI checks pass.
Formatting, generated files, build/install, 82 pinned checksums and eleven
provenance scenarios pass. Exact lexer JSON and parser JSON/normalized text match
the existing 528/528, 25 standalone and 126 prelude baselines. Source rebuilding,
CLI identity reports and hosted checks are recorded at the external checkpoint.
