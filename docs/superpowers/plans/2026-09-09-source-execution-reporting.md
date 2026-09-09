# Source execution reporting foundation

Continue the approved stateful #exe design on `feat/635-stateful-exe`. The
source facade must sit above the checked compilation core and the retained task,
and must preserve reached effects when subsequent parsing or compilation fails.
Use serial implementation and executable verification; independent review is
read-only. Preserve the pinned reference, public isolated-unit APIs, package
identities and exact ordinary resource counts.

- [x] Extract AST compilation, task compilation and compiled-unit inspection
  into `Integer_unit`. Keep `Integer_program` source parsing and public entry
  points, with exact type aliases. Remove the task's dependency on that facade.
- [x] Add immutable VM-owned task progress snapshots. Capture cumulative runtime,
  preparation, storage, output and generation counts and the outer value latch.
  Update the latch at the original reached discard instructions, including
  reached faults. Declarations and implicit output preserve it; explicit U0
  expressions clear it. Active stream execution leaves the outer latch alone.
- [x] Expose snapshots through the retained task API. No snapshot grants runtime
  admission, execution, memory or compilation authority, and no last command is
  presented as a whole-invocation graph or result.
- [x] Reproduce snapshot and latch regressions before implementing them; test
  fault, preflight, preparation, nested stream and immutable snapshot behavior.
- [x] Run focused/full/CLI and format/generated/all/install checks, reference,
  provenance and exact corpus checks. Resolve independent review findings.
- [ ] Record verified source and hosted results, commit and push the checkpoint.

This prepares the real source facade; it does not replace its still-required
partial declaration/header/initializer admission, source read timing, extern
joins, provider installation, artifact collection and parser orchestration. All
fourteen original #exe run groups, the seven #635 criteria and the full compiler,
native/BIN/loader/bootstrap objective remain required.

Verification passes 2,286 of 2,300 groups in 46.919 seconds, with only the
fourteen existing stateful #exe groups failing. CLI checks pass. Eight progress
groups cover reached discard limits, explicit U0 and pointer clearing, unsigned
values, immutable output/literal counts, nested streams, preflight/replay and
preparation/parse failures. The six initial latch controls failed before wiring
the VM observation to reached discards.

Formatting, generated/all/install builds, 82 reference checksums and all eleven
provenance scenarios pass. Full lexer JSON and parser JSON/normalized serialized
output match the existing baselines: 528/528 tokenize, 25 parse standalone and
126 with the prelude. Independent read-only review found no code or API blocker;
its preparation-coverage refinement now separates initializer work from the
dimension-work component. Final pushed identity and hosted checks are recorded
in #635 and the external prompt after committing.
