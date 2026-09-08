# Narrow integer implementation plan

> Use superpowers:executing-plans for serial implementation and verification.

**Goal:** Execute the five remaining narrow integer storage/signature families.

**Spec:** `docs/superpowers/specs/2026-09-08-narrow-integers-design.md`.

**Architecture:** Generated primitive descriptors drive declared storage;
separate interval and producer-class evidence protects native parity.

**Constraints:** Work on `feat/633-narrow-integers` in the established checkout.
Keep the source pin, dependencies and supported OCaml versions unchanged.
Serialize builds and executable use; freeze compiled files during checks.

## 1. Maintained RED and shared storage

- [x] Add the eight Section 63 gates and per-width entry/return tests in
  `test/test_integer_narrow.ml`; register and run them before production edits.
- [x] Extend `integer_scalar_storage` from generated metadata; retain public
  admission separately. Share the descriptor with shapes/prepared arrays.
- [x] Generalize lowering and VM numeric storage at every frame, global,
  static, array, pointer, argument, return, assignment and update entry path.
- [x] Verify all families, signed/high-bit normalization, full return and
  compound values, eight-byte ABI allocation, aliases and neighboring bounds.

## 2. Native initializer proof and computation classes

- [x] Add signed AND/division, mixed-width sinks, register writes, transitive
  calls and physical-bit hazard RED controls to the maintained tests.
- [x] Generalize initializer constants/ranges and whole-graph location proof;
  preserve source ownership, observer, terminal sink and optimizer boundaries.
- [x] Implement exact effective computation-class handling for unsigned
  storage/arithmetic versus direct public calls, including nested unary.
- [x] Verify forged IR rejection before effects and retain prior byte/word
  suites and unrelated type/index/cast/automatic-array boundaries.
- [x] Refine external prompt and issue text with the source producer finding.

## 3. Maintained fixture and documentation

- [x] Add and measure a fixture using all five families, persistent data,
  calls, output and owned literals. Assert all eight exact/one-below resources.
- [x] Add original CLI gates, v1/v2 reports, deterministic dumps and replay.
- [x] Update public support docs, traceability, changelog and prompt evidence.
- [x] Request independent review and resolve reproduced findings.

## 4. Verification and integration

- [x] Run `opam exec -- dune runtest -j 1` and quoted packaging aliases.
- [x] Verify 82 checksums, eleven provenance scenarios and exact corpora.
- [ ] Review diff, commit/push with configured identity and create draft PR.
- [ ] Rebuild committed source and verify exact gates/quotas and provenance.
- [ ] Obtain five final-source checks and merge normally after review.
- [ ] Verify merged tree, execution, post-merge CI, synchronized Git and
  issue/project/parent/prompt state. Keep the overall compiler goal active.

## Local implementation evidence

The first maintained run reproduced 18 admission failures. Shared storage passed
those gates; five signed/native initializer counterexamples then failed before
the generalized range proof. Review exposed COM node/result class loss, bare
execution propagation and an unsafe unary fold. Each has a maintained RED/GREEN
control. The first full run found 13 failures: obsolete support expectations,
the corrected public unsigned negation expectation and a real top-level-call
provenance omission. Those are resolved.

The 53 narrow groups pass. Final full verification passed 2,049 tests in 40.856
seconds plus CLI checks. Both final read-only reviews found no production or
test blocker; a stale public convenience-API comment was corrected afterward.
The fixture returns I64 42/capture 3432 with 139 runtime instructions, nine
preparation units, sixteen persistent bytes, forty active frame bytes, depth
two, three literal bytes, two output bytes and five output-work units. Exact
and one-below bounds and fresh images pass in both modes. Formatting, generated
sources, build/install, 82 checksums and all eleven provenance scenarios pass.
Lexer JSON matches the preceding 528-file, zero-error result exactly. Complete
parser JSON and text after Windows newline normalization match the committed
AOT baseline: 25 standalone and 126 prelude successes. Committed-source checks
and protected integration remain pending at this checkpoint.
