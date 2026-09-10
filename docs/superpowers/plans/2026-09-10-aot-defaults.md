# Output-owned AOT defaults

Native source review confirms that ordinary parameter defaults retain AOT flags
and the AOT lookup chain. They do not use the PrsStreamBlk task hash-table switch
or the global-initializer staging path. Closed integer defaults and checked
queries can use the existing bounded preparation engine. Global and call operands
require output relocations; meaningful native dynamic behavior remains unproved.

- [x] Reproduce ordinary AOT default failures through the public source API.
- [x] Give original AOT default fragments mode-correct typing and storage context.
      Evaluate through the existing initializer preparation engine, preserving
      original receipt, namespace, predecessor, and single-attempt authority.
- [x] Retain prepared values in the output source seal and materialize them from
      that exact compiled output's globals. Never publish output values as task
      defaults or create replacement argument ASTs.
- [x] Charge preparation to the invocation budget at each declaration event,
      including faults and interleaved #exe blocks; keep AOT execution isolated.
- [x] Verify constants, checked queries, unused/prototype defaults, nested calls,
      quotas, foreign/replayed/missing source evidence, and dynamic diagnostics.
- [ ] Review, run full checks, publish verified progress, and preserve the full
      compiler/native/BIN/loader/bootstrap goal and remaining general defaults.

Pre-publication verification: all 2,479 compiler tests pass in 45.949 seconds,
plus six standalone authority tests and both CLI suites. Formatting, generated
sources, build/install and all 82 pinned reference checksums pass. Complete
corpus reports match the pinned baselines. Review found one missing invocation
budget check; a real-fragment regression reproduced it before the fix and now
passes. Additional regressions reproduced and closed incomplete source seals.
The final review found no remaining high/medium ownership or timing findings.
