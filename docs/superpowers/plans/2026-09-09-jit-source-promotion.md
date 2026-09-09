# Live JIT source promotion

Continue #635's approved source-execution design in the existing feature
checkout. Root owns serial edits and executable checks; independent review is
read-only. Preserve the complete compiler/native/BIN/loader/bootstrap mission.

The first directive must retain the source parser's original namespace,
declarations, selections, checked dimensions and command lifecycle. A newly
collected task ledger would lose that authority. Promotion therefore attaches
the live source ledger to one fresh runtime and transfers its already validated
command events into the runtime's source-order projection. It performs no parser
callback, symbol lookup, source replay, declaration compilation or execution.

- [x] Reproduce the existing public JIT failures and add promotion regressions.
- [x] Add atomic source-order import into a pristine runtime, including one-time
  transfer of previously reached dimension work into its preparation allowance.
- [x] Promote only the exact live JIT source/session/environment. Reject sealed,
  foreign, repeated, analysis, AOT, closed and aborted ownership before mutation.
- [x] Construct the retained task around the existing frontend and ledger.
  Compile original parser-owned commands through the existing checked pipeline.
- [x] Test predecessor readiness, original symbols/extents/selections, budget
  boundaries, failure retry, and runtime states whose counters are still zero.
- [ ] Run focused/full/CLI and ordinary format/generated/build/install checks;
  resolve review, commit/push, inspect hosted checks and record the checkpoint.

Frozen references with no admission remain unadmitted. Deferred materialization
needs separate evidence of the original native publication boundary; promotion
cannot supply it. Shared outer execution still needs partial global/header and
initializer admission, checked provider installation during a suspended root,
whole-invocation results and explicit stateful artifacts. Do not manufacture
completed commands around partial syntax. The ordinary inactive-source path
must retain its measured single-unit counts.

The thirteen promotion groups include a review regression for delayed parser
events. Remembered ledger state alone cannot prove a live parser: the parser
owns context lifetime and checkpoint count, and promotion checks both. The stale
finished-root and skipped-live-checkpoint cases failed before those guards.

Local verification passed 2,351 of 2,365 groups in 57.072 seconds; exactly the
fourteen pending public JIT groups retain HCPP0008. Complete CLI checks pass.
The thirteen promotion groups include real nested StreamPrint execution through
the adopted root, generated function bodies and initializer operands. Formatting,
generated sources, build/install and all 82 pinned checksums pass. Complete
lexer/parser JSON and normalized parser text match the existing baselines.
Independent read-only review found no remaining blocker after the parser-lifetime
fix. Committed-source provenance and hosted results are recorded in #635/#636
and the external prompt after pushing.
