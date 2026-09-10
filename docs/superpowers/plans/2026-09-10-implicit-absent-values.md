# Implicit calls without a supplied first value

Continue #635 from 219c3c83c9ac51ebe58633f8abd7b5559d45a1a7 against
TempleOS c26482bb6ad3f80106d28504ec5db3c6a360732c.

Represent an absent first value explicitly in the original AST. Record an
omitted first fixed parameter at position zero; a zero-parameter call has no
such omission. Preserve original delimiters and selected headers. Semantic
inputs and typed results carry optional supplied values. Top-level output
statements retain identity independently of expression roots, including when
there are no roots. Validate complete root/call ownership and duplicate use.
Legacy supplied-value constructors retain their existing contracts.

- [x] Reproduce omitted-first and zero-value calls through top-level and body
      execution in JIT/AOT, including declaration-time default side effects.
- [x] Implement absent AST values, native token consumption, optional semantic
      values and source-owned output groups without placeholder expressions.
- [x] Cover selection across lookahead, retained task inputs, malformed source
      ownership, native separator boundaries and resource accounting.
- [ ] Run focused/full/build/install/CLI/reference/corpus/provenance checks,
      request review, verify committed consumers and publish the checkpoint.

General defaults/conversions, native linkage and the complete compiler,
BIN/loader and bootstrap requirements remain active.

Local verification before publication: four initial execution regressions failed
at HCPARSE0166. Separate regressions reproduced foreign rootless statement
ownership, legacy absent/supplied inconsistency and omission-zero role mismatch.
The final parser preserves native comma-separated statements after completed
calls, following PrsStmt as well as PrsFunCall; a remaining comma is not by itself
an invalid argument. All 257 focused groups pass. The final full run passes
2,547 compiler tests in 55.993 seconds, fourteen standalone authority tests and
both CLI suites. Format, generated sources, build and install pass. All 104
provenance scenarios and 82 reference checksums pass. Complete corpus reports
match pinned baselines: 528 lexical files, 25 standalone parser acceptances and
126 with the prelude. Independent review found no remaining blocker, including
the final statement-comma correction. Exact-revision consumers and publication
follow the source commit.
