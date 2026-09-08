# Owned string storage implementation plan

Goal: execute issue #617's string-pointer Read fixture with final I64 42 in
both modes, preserving the full compiler objective.
Spec: [owned strings](../specs/2026-09-07-owned-strings-design.md).
Stack: existing OCaml/Dune, Alcotest and CLI suites. Keep packaging unchanged.

- [x] Inspect current main, remote state, #617 and the pinned reference.
- [x] Capture maintained source tests failing at the existing connection.
- [x] Add the narrow checked U8 pointer conversion predicate and lowerer joins.
- [x] Add private per-owner literal plans, preflight, initialized image storage,
  resource bounds and reference coercion to the VM.
- [x] Thread optional literal limits through the public program API and CLI,
  including configuration errors and additive result reporting.
- [x] Expand source and malformed-IR tests for identity, lifetime, initializers,
  fresh images, exact bytes, bounds, flags and resource limits.
- [x] Maintain the source example and CLI regressions; update previously
  unsupported string-storage assertions while preserving unrelated boundaries.
- [ ] Update docs, traceability and the external compiler prompt with evidence.
- [x] Pass focused/full/CLI, formatting/generated/build/install, 82 reference
  checksums, provenance scenarios and exact corpus comparisons. One coordinator
  owns all builds/executions; compiled source/test files stay fixed during them.
- [ ] Obtain independent review, address findings, commit/rebuild/push and
  require all five checks on the final source before protected merge.
- [ ] Compare source/merge trees, synchronize main, verify rebuilt identity and
  post-merge workflows, then record the next missing execution connection.

Validation records will distinguish source audits, hosted execution, corpus
results and native captures. The reference stays
`c26482bb6ad3f80106d28504ec5db3c6a360732c`. The full compiler goal stays active.

Local source snapshot: all 1,825 tests pass in 117.064 seconds, including 14
string groups and CLI checks. The example is 42/45+0, with two literal bytes,
16 frame bytes and depth two. All quoted build gates, 82 reference checksums,
11 provenance scenarios and the 528-file corpus checks pass; the complete
parser JSON matches after Windows newline normalization. Independent review
approved the frozen code and initializer phase tests. Delivery boxes remain
pending in this source snapshot; #617/#618 record exact revision, final CI,
merge and external-prompt checkpoint evidence.
