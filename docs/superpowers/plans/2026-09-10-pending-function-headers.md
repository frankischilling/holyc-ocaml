# Pending function header runtime admission

> Use superpowers:subagent-driven-development for the independently reviewable
> semantic, typing and runtime tasks below. This continues the full compiler
> mission; completing this plan does not complete #635.

**Goal:** Calls selected after completed-header publication use that original
header before executable publication, evaluate their arguments, and reach
UndefinedExtern when no executable or approved provider exists.

**Architecture:** Keep pending-header state distinct from source binding kind.
The declaration ledger retains one collected and typed header. Task publication
requires its exact live/replay authority, original defaults and command order.
Body completion extends the same parameter scope with locals, retains the typed
header and completes its exact pending declaration. Calls compiled earlier keep
the original selected header and acquire only its joined executable.

**Spec:** [Completed headers](../../completed-function-headers.md), the latest
checkpoint in the desktop prompt, and TempleOS revision
`c26482bb6ad3f80106d28504ec5db3c6a360732c` (PrsStmt, PrsVar and PrsExp).

**Constraints:** Preserve metadata-only replay, exact source identity, existing
JIT/AOT separation, prepared defaults, quotas, providers and declaration joins.
Do not manufacture ASTs or move body publication before closing-brace lookahead.
Provisional parameter lists remain a separate phase. Keep existing package/API
names and run one root Dune process at a time.

- [x] Write and reproduce end-to-end failures for a pending call after `}`,
  argument output before HCIRVM0030, a skipped pending call, and eventual body
  execution. Include preactivated JIT and the separate AOT directive task.
- [x] Extend semantic function resolution/classification with source-backed
  pending headers and exact completion. Tests reject copied/foreign headers,
  repeated completion and substituted predecessor chains. Pending definitions
  retain extern call access without changing their source declaration kind.
- [x] Extend function collection/type adapters to retain a completed header's
  parameter scope, symbols and typed signature while adding eventual locals.
  Tests prove exact parameter identity and reject incompatible reuse before
  allocation. Existing ordinary whole-module typing remains available.
- [x] Admit checked headers into the existing task catalog at their original
  header event, after successful default completion and predecessor admission.
  Source selection uses the admitted publication. Eventual command compilation
  consumes the exact pending header and publishes its executable normally.
- [x] Cover retained calls, same-name shadows, provider timing, saved defaults,
  variadics, argument effects, failure recovery, activation replay and quotas.
  Review native source ordering and ownership independently.
- [ ] Run focused and complete tests, format/generated/build/install checks,
  exact-revision consumers, reference/corpus/provenance checks; update docs and
  the desktop prompt, commit/push and inspect CI. Keep #635 open until every
  original acceptance criterion is implemented and verified.

Review found two missing retained-adapter checks: modified modifiers/bindings
and empty parameter entries. Both regressions failed before the identity fixes.
Nested same-name replacement while an outer body remains pending requires
separate current-header and executable versions and remains an explicit #635
gap. The source-derived acceptance matrix and present HCEVAL0003 boundary are
recorded in the completed-header document. This checkpoint does not complete
that requirement or the full compiler mission.
