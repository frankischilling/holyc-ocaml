# AOT source execution through retained tasks

Continue the approved #635 plan serially in the established branch. The prior
parser adapter now executes real task commands. Connect the independent AOT
source path to it while preserving the complete shared-JIT and partial-publication
requirements. Independent review remains read-only; root owns edits and builds.

- Add public AOT run regressions that fail with HCPP0008 before wiring.
- Fork the task frontend before parsing outer AOT declarations. Activate checked
  runtime providers once at the first actual parser stream entry; charge their
  real setup work. Strings, comments and skipped directives do not activate it.
- Preserve ordinary source-ledger compilation and exact no-#exe unit counts.
  Keep the complete outer AOT artifact separate from compiled task units.
- Share VM instruction, storage, literal, output/work and initializer allowances
  across reached stream work and the isolated outer run. Source AOT dimensions
  retain their separate allowance; stream dimensions remain task preparation.
  Keep zero remaining capacity a resource boundary, not invalid configuration.
- Freeze compilation observations independently of success. Parse/lowering
  failures retain reached task output, work and checked task units. Both run APIs
  project one orchestration path; the VM supplies cumulative successful outcomes.
- Cover namespace exclusion, joined/nested/generated source, retained state,
  fault cleanup, fresh runs/artifacts, caller limits and exact ordinary counts.
- Verify focused/full/CLI, format/generated/all/install, reference/provenance and
  corpus results; resolve review, then commit/push and inspect final-source CI.

Do not wrap partial declarators in invented complete commands. Shared outer JIT
execution needs source-ledger promotion retaining its original namespace,
references, dimensions and lifecycle. Both modes still need partial JIT global
and array-leaf initializer admission inside directives, implicit-provider and
later-call/default reads, extern joins and broader metadata. Maintain these
requirements and the full compiler/native/BIN/loader/bootstrap objective.

## Local implementation checkpoint

The source orchestrator, VM-owned isolated preparation/admission, immutable
reports, separate artifact collection and public AOT example are implemented.
Twenty-seven AOT groups cover real source generation, namespace separation,
inactive text, reached failures, caller limits, exact preparation ownership,
preflight/replay and distinct source/stream dimension accounting. CLI controls
preserve v1 result/diagnostic contracts and verify v2 cumulative capture on
success and one-below failures.

Regression runs exposed and fixed later read/opener priority, lost generated
capacity, foreign uncharged preparation and task replay/resource diagnostic
ordering. Independent read-only review found no remaining production blocker.
The final local run DBEX6J4H passes 2,338 of 2,352 groups in 50.752 seconds;
exactly the fourteen existing shared-JIT stateful exe groups remain red. CLI,
format/generated/all/install, 82 reference checksums and eleven provenance
scenarios pass. Committed-source and hosted evidence is recorded in #635/#636
and the external prompt after each push. The draft stays open until every full
acceptance criterion passes.
