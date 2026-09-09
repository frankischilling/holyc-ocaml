# Executable provenance verification

Continue #637 independently from the unfinished #635 branch. The observed stale
revision came from PowerShell dropping unquoted simple Dune aliases: the argument
probe retained only `@generated-check`; quoted arguments retained all four
requested targets. Do not change the Dune rule without a failing consumer case.

- [x] Extend `tools/test-version-metadata.ps1` with a real executable consumer of
  the production version module. Build separate fixture families through the
  explicit metadata target, executable target, `@all` and `@install`, using
  disabled and enabled Dune cache modes. Supply aliases as string arguments.
- [x] Preserve all eleven ref/override scenarios, add branch switches and
  source-archive unknown fallback, and compare each already-built executable
  against external expected
  revisions. Prove that running it before rebuilding retains its old identity.
  Keep environment restoration and checked temporary cleanup paths.
- [x] Correct copyable README/testing/reference command examples to quote Dune
  aliases in both PowerShell and POSIX shells. Document the argument-loss cause
  and the expanded consumer contract; keep Bash CI commands unchanged.
- [x] Verify the original rule with the consumer matrix, quote every alias in
  actual local builds, run the full suite and required reference/corpus checks,
  and resolve independent read-only review. Root owns edits and serial builds.
- [ ] Commit/push, compare the rebuilt executable with external HEAD, inspect
  source and protected-merge CI, and update #637, #635 and the external prompt to
  correct the earlier defect classification. Preserve the full compiler goal.

No runtime Git query, reference-pin change, build-generator replacement or
language behavior change is planned. A failing consumer case must be diagnosed
before any production build-rule edit.

Local evidence: all 104 scenarios pass with the production rule unchanged.
Independent mutation controls reject a removed universe dependency through the
normal `@all` path and reject a stale executable even when generated metadata is
correct. Empty target/cache selections fail before fixture execution. The eight
Git diagnostics in the local matrix are expected source-archive unknown cases;
the child process exits zero. The clean main baseline passes 2,049 tests plus
CLI, and final quoted aliases, reference checks and exact corpus comparisons
pass. Independent read-only review found no substantive issue. Source/merged
revision and hosted evidence will be recorded in #637 and the external prompt.
