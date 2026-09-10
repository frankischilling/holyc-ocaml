# Ordinary JIT declaration defaults

The compiler prompt requires default expressions to execute during declaration
parsing and later omitted arguments to reuse saved values. The retained task
already implements this for streams and activated JIT source. Ordinary JIT source
without a directive must use the same original event and ownership path when an
expression default is observed. Native boundaries: pinned PrsVar.HC:631-656 and
PrsExp.HC:455-468.

- [x] Reproduce failures through the public source API for constant, effectful,
      unused-function and prototype defaults without directives.
- [x] Reuse source activation at the first completed expression-default receipt.
      Do not observe or evaluate the triggering receipt twice. Keep provider
      installation tied to actual stream execution. Preserve ordinary inputs
      without defaults and the separate AOT namespace contract.
- [x] Verify retained defaults in later calls, original lookahead, reached faults,
      exact/one-below shared budgets, and compilation report ownership.
- [ ] Review, run complete checks, commit/push, and update the prompt and draft
      PR with exact validation and remaining full compiler requirements.

General pointer/string/floating/lastclass defaults, callback declarators and
ordinary AOT default preparation retain their separate unfinished requirements.

Pre-publication verification: all 2,477 compiler tests pass in 45.747 seconds,
plus three standalone authority tests and both CLI suites. The final focused
run passed 66 groups. Formatting, generated sources, build/install and 82 pinned
reference checksums pass. Review found no high/medium issues. Two older tests
now assert successful JIT defaults while retaining explicit AOT rejection checks.
