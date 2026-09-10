# Runtime-evaluated array dimensions

The pinned PrsArrayDims calls LexExpressionI64 while parsing each dimension,
after expression lookahead and before validating the closing bracket. Local
array bounds are prepared during function compilation, not on each function
call. Temporary expression execution has no future function frame. AOT keeps
its output address requirements; #exe dimensions use the task's JIT context.

- [x] Cover global reads, calls and updates in ordinary JIT dimensions and
      JIT/AOT directive bodies, including local declaration-time bounds.
- [x] Scope each original parser preparation receipt to its exact callback.
- [x] Add an explicit dimension fragment through the existing checked binding,
      typing, expression lowering and VM pipeline; do not manufacture a default,
      initializer or command AST. Preserve selected references and queries.
- [x] Activate ordinary JIT source immediately before observing its first
      runtime-required dimension. Replay the completed prefix only, retaining
      the existing fully checked closed-dimension promotion manifest.
- [x] Require an original single-use runtime attempt, current callback, owning
      namespace and exact preparation charges. Seal the returned nonnegative
      extent with that execution evidence and reuse it for grammar/layout/sizeof.
- [x] Cover declaration-time side effects, lookahead, faults, budgets, negative
      extents, skipped/replayed/foreign authority and unchanged closed counts.
- [ ] Review, run complete checks, commit/push and publish exact progress.

Runtime classification is structural and side-effect-free. Closed numeric
dimensions retain their existing numeric-visit counter and evaluator. A closed
evaluation error must never cause a second evaluation through the VM. Runtime
fragments charge ordinary preparation and execution work to their task. The
dimension report records the preparation work reached by those fragments;
layout and query reads must not charge it again.

Early review confirmed that prefix activation before the preparing declaration
event is compatible with command-event lifetime checks and frozen reference
replay. A new parser activity guard is required because the existing command
Reading state alone would permit delayed execution within the same function.
