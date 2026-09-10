# Declared default execution

Use the executing-plans workflow for this continuation of
`docs/superpowers/specs/2026-09-08-stateful-exe-design.md` and the live initializer
execution plan. Continue the full #635/compiler/native/BIN/loader/bootstrap goal.

Default evaluation belongs to declaration parsing, before the next parameter.
`PrsVar.HC:631-656` compiles and executes the expression, converts its result to
the parameter's numeric class and retains its value. `PrsExp.HC:455-468` later
materializes that stored default. String defaults and lastclass have distinct
ownership/AOT behavior and must retain that distinction. The reference remains
`c26482bb6ad3f80106d28504ec5db3c6a360732c`.

- [x] Add original named-function parameter/default receipts at the existing
  parser boundary. Retain provisional function, parameter index/predecessor,
  original type/declarator and exact completed default AST. Observe synchronously
  after expression lookahead and before parameter delimiter handling. Completion
  must consume the same original parameter objects; stale, skipped and foreign
  receipts cannot authorize evaluation or completed-header reuse.
- [ ] Type and prepare the original default expression in the retained task
  environment. Reuse shared expression/call traversal, conversion, VM and
  resource accounting. A distinct default root owns its expression; do not wrap
  it in a synthetic function/global/initializer AST or synthesize provided
  argument results. Failed preparation retains reached effects and charges and
  cannot be retried through header completion. Preserve partial function timing,
  exact source reads, declaration-time order and outer value capture.
- [ ] Bind prepared values to the matching declaration and parameter. Reuse
  them in direct-call lowering and canonical runtime call-context verification,
  including nested and retained calls, explicit omissions and repeated calls.
  Keep provided-argument behavior, target snapshots, ABI conversions and guards.
  Test default effects before the following parameter/directive, stored values
  after later writes, exact and exhausted budgets, replay and foreign owners.
- [ ] Integrate ordinary source/JIT/AOT preparation without changing existing
  no-default resource/report contracts. Preserve owned strings, lastclass,
  prototypes/extern joins, partial function headers and aggregate/debug metadata
  as requirements, with explicit visible acceptance gaps until implemented.
- [ ] Obtain independent review, run meaningful focused/full/CLI/reference and
  corpus checks, then commit, push and update the prompt and draft PR truthfully.

The original nested-default acceptance and declaration-effect/repeated-call
cases now pass through the checked live integer path. Review found and fixed
foreign snapshot, skipped predecessor and whole-command admission gaps.
Shared typing/preparation and saved-value materialization are implemented for
integer expression defaults; the unchecked broader tasks above remain open.
This is not completion of the full default or compiler plan.
