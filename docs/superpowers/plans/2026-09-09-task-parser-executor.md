# Retained task parser executor

Continue the approved #635 design on `feat/635-stateful-exe`. Use serial edits
and executable verification with independent read-only review. Preserve the
pinned reference, package identities, original source witnesses and ordinary
single-unit APIs/resource counts.

Connect the existing Parser.stream_execution interface to the retained task.
Each block owns an exact task buffer. Observe original command/declaration/query/
dimension/reference receipts; compile and execute each command on its original
resume event. Finish requires the exact accepted sequence and returns only its
buffer. Fault and exception cleanup aborts that buffer while preserving reached
ordinary effects and cumulative charges. This is a production task adapter used
by the future whole-source facade, not an alternate parser or synthetic function.

- [x] Add real parser/VM/generated-AST RED controls with explicit checked provider
  headers and a distinct outer environment. Cover original generation forms,
  retained state, nested buffers and joined lexer tokens in both outer modes.
- [x] Implement the task adapter, exact finish/abort lifetime and early execution
  reference checks. Missing, unbound and unadmitted foreign-command references
  must fail when consumed, before subsequent directive lookahead.
- [x] Cover source/environment ownership, early faults, reached ordinary effects,
  retained statics/literals, resource limits and exact accepted-sequence finish.
- [x] Run the full suite, complete CLI runner, formatting/generated/all/install,
  reference/provenance and full corpus checks; resolve independent review findings.

The final local suite passed 2,311 of 2,325 groups in 51.085 seconds; only the
fourteen maintained public #exe gates remain RED with HCPP0008. All 25 adapter
groups pass. Regression controls first reproduced missing generation, delayed
unavailable reads, consumed/context-substituted executors and parser-context
leakage after an early explicit buffer abort. The final cleanup controls cover
Error and exception paths during active commands and tentative completion, plus
abort replay. Depth-one limits detect leaked buffers beneath later streams.
Independent read-only review found no remaining blocker in this adapter.

Full lexer/parser JSON and normalized parser text match the existing baselines:
528/528 files tokenize, 25 parse standalone and 126 with the prelude. The 82
reference checksums and eleven provenance scenarios pass.

Before publishing the checkpoint, commit and push. Rebuild the committed source, verify
focused groups and exact-revision CLI reports, inspect hosted CI/corpus/dependency
results and record the evidence in #635/#636 and the external prompt. Keep the
pull request draft while the maintained public gates fail.

This adapter does not run the outer compilation unit. Its distinct outer
environment is not the eventual shared JIT outer-command facade. Runtime provider
installation, partial global/header/initializer admission, whole-invocation
outcomes and checked artifact collections remain required for the public run
API, alongside remaining metadata/source-read/extern work and all full compiler,
native/BIN/loader/bootstrap gates. Do not close any complete #635 criterion merely
because adapter-level generation works.
