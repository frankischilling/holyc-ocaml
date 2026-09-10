# Function joins across task commands

The full compiler objective remains active. This step connects the existing
source-ordered function reconciliation to the persistent task namespace.

Pinned PrsStmt.HC:63-139 uses HashSingleTableFind for JIT function joins and
reuses only a still-extern record. A completed definition shadows with a new
record. Reused records retain their prior flags, receive the new header, and
replace the old member list. Earlier compiler selections remain frozen to the
header and execution state observed when they were made.

- [x] Add a failing source-task regression proving canonical identity is reused
      across separately parsed prototype/definition commands, while the new
      definition keeps its own source symbol and frame.
- [x] Carry exact prior declarations from the current task table into semantic
      reconciliation. Validate table, namespace, mode, newest-name selection
      and eligibility; do not search parent tables or reconstruct source headers.
- [x] Preserve prior record flags through classification, and retain the
      replaced header's exact identity for compatibility analysis.
- [x] Publish the new immutable declaration snapshot under its canonical
      callable identity. Keep old retained selections and compiled calls frozen.
- [x] Cover repeated prototypes, definitions, shadowing, defaults, runtime
      bounds, nested directives and source/namespace ownership.
- [ ] Review, verify complete checks and exact consumer revisions, then publish
      the new checkpoint without closing unfinished #635 or compiler scope.

Review added exact classified-predecessor admission, originating-mode checks,
and immutable source-history prefixes that reject ancestor header reuse.
Fixed replacement headers can execute with an inherited Variadic flag only
when their exact bound frame has no variadic signature or synthetic bindings.
Actual variadic definitions and unbound flagged bodies remain rejected.

Local validation: 2,492 compiler tests and 10 standalone authority tests pass,
along with both CLI suites and the format/generated/build/install checks.
Final committed-revision consumer checks and publication follow this commit.
