# Live initializer leaves

Continue the approved #635 source-execution design in the existing feature
checkout. Root owns edits and serial builds; independent review is read-only.
The complete compiler/native/BIN/loader/bootstrap requirements remain active.

The pinned `PrsVar.HC` evaluates and stores a scalar after expression lookahead,
before advancing past its separator. `LexExtStr` reads adjacent strings and the
first following token before the copy. Record those boundaries in the original
parser, preserving the exact expression, initializer owner, source-tree path and
predecessor. Paths describe syntax; checked layout must still determine storage.

- [x] Reproduce the absent initializer callbacks with real streaming-parser
  controls before adding implementation.
- [x] Emit private initializer-start and leaf receipts. Rejection stops before
  later lexer effects; callback lifetime prevents delayed event observation.
- [x] Retain those leaves in the declaration ledger, validate the completed tree
  against every original receipt, and reuse the same semantic leaf identities in
  ordinary checked compilation. Preserve the legacy AST-only compiler entry.
- [x] Test nested braced/unbraced arrays, adjacent/copied strings, generated
  operands, malformed delimiters, missing/replayed/foreign events, promotion,
  callback failure and physical identity through the existing initializer IR.
- [ ] Resolve independent review; run focused, full, CLI, format, generated,
  build/install and provenance checks. Commit/push and inspect hosted results.

These receipts supply original partial syntax to the next storage/initializer
admission work. They do not admit storage, execute initializers, infer omitted
dimensions or enable public shared JIT execution by themselves. No synthetic
completed commands, source replay or changed selected-reference authority.

Local verification passed 2,363 of 2,377 groups in 50.539 seconds, with exactly
the fourteen pending public JIT HCPP0008 failures. All twelve new groups pass.
Complete CLI checks, quoted format/generated/build/install aliases, 104 provenance
cases and all 82 reference checksums pass. Complete lexer/parser reports match
the existing baselines. Independent read-only review found no remaining blocker.
The tests exposed a read-only lookup incorrectly rejecting sealed declarations;
that regression passed after the accessor stopped using the admission guard.

Committed-source identity and hosted results will be recorded in #635/#636 and
the external prompt after pushing. Shared public JIT and partial storage/leaf
execution remain the next integration work.
