# Ordered deferred dimension charging

The approved compiler prompt requires outer JIT effects and preparation to retain
their source order. Promotion currently charges all earlier dimensions before
replaying an earlier Print. Preserve the existing inert promotion API and its
atomic budget rejection/retry contract.

- [x] Reproduce the two-step preparation limit with earlier output A.
- [x] Add an activation promotion path carrying original checked dimension
      preparations. Validate namespace, source journal, and current root before
      binding. Keep ordinary promotion unchanged.
- [x] Charge each deferred preparation at its original event, retain reached
      visits on failure, forbid skipped/repeated/foreign charges, and reuse the
      checked extent without reevaluating its expression.
- [x] Verify shared budgets across initializer/default and dimension events,
      exact/one-below limits, partial headers and post-directive dimensions.
- [ ] Run focused and full checks, review the authority boundaries, update the
      prompt and draft tracking metadata, commit and push verified work.

This work does not complete general effectful dimensions, defaults, #635, or the
full compiler/native/BIN/loader/bootstrap goal.

Validation before publication: 2,474 compiler tests pass in 42.233 seconds,
plus three standalone authority tests and both CLI suites. Format, generated
sources, build/install, and 82 pinned reference checksums pass. Review found
that consumed/expired journals could close fresh promotion; a failing regression
reproduced the issue, and current/unconsumed preflight now rejects before mutation.
