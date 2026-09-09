# Selected query and implicit provider integration

Continue the approved stateful #exe design in
`docs/superpowers/specs/2026-09-08-stateful-exe-design.md`. Root owns edits and
executable use; independent review is read-only. Use the serial implementation
workflow. This extends the existing architecture under #635; the complete
compiler goal remains active.

## Goal and architecture

Preserve native query and implicit-call reads across streaming lookahead.
Dedicated parser receipts retain actual query children and output-start tokens,
their command/environment owners, and completion associations. Checked compiler
record metadata is independent of VM storage or executable admission. Presence,
type/size/member reads, parameter reads and later call-target classification are
different facts and must not be collapsed into one reference lookup.

The existing ordinary identifier checkpoint is 33f3b31. Its local unbuffered
suite verifies 2,200 passes and only fourteen pending #exe failures; all five
hosted jobs completed with only those known CI failures. The external prompt and
#635/#636 contain exact commit/tree, CLI and hosted verification records.

## Native requirements

- `defined` tests the actual post-preprocessing operand token and saved hash or
  local presence before following Lex (PrsExp.HC:942-947). Present declarations
  remain true without runtime admission. Non-name operands are false.
- `sizeof` validates the selected root and reads size/debug extent before the
  following Lex; each member uses the retained class at its own MemberFind point
  (PrsExp.HC:311-346). Unavailable metadata is distinct from absence.
- `offset` retains the selected root's class before following Lex, then reads
  each selected member's offset/class (PrsExp.HC:360-380).
- Implicit Print/PutChars use function-kind-filtered lookup before consuming
  the marker (PrsExp.HC:394-407). Test emptiness from the first literal token,
  before concatenation. Nonempty markers read the header at 430-431 before
  argument lookahead; empty markers advance once at 399/406 first. Later target
  classification/code reads remain distinct for same-record extern joins.
- Invalid query roots fail before following directives. Ordinary absent names
  likewise fail before following Lex once expression parsing has begun
  (PrsExp.HC:810-812). Bare statement-start names have a separate unresolved-label
  probe that performs lookahead first (PrsStmt.HC:1182-1196). Do not conflate them.

The existing selected_runtime_source test helper delays outer semantic checking
until parsing completes. It proves selection binding, not production error
timing. Its reached-child assertions do not authorize a production source
facade to execute directives past a native early error.

## Implementation sequence

- [x] Reproduce incorrect `defined` presence and sizeof selection with real
  nested Parser commands, including function bodies and initializers. Add a
  constant dimension query control, visible-but-unadmitted declarations and
  non-name operands. Keep native error-order controls distinct from the delayed
  outer-compilation helper.
- [x] Add dedicated query receipts in `src/frontend/parser.ml/.mli` for real
  Ast.defined_operand and sizeof/offset root/member children. Issue each at its
  native consumption point. Retain original command/environment and ordered
  root/member/completion associations. Extend `Task_declarations` with exact
  query ownership, replay rejection and sealed command views. Do not manufacture
  replacement identifier, expression, literal or extern ASTs.
- [x] Record exact frontend-entry/semantic-primitive associations during Session
  seeding. Share them with task_frontend; rebind the known entries to fresh
  semantic symbols for fork_frontend. Never recover primitive authority by
  matching a name or pinned origin after selection.
- [ ] Add checked compiler-record metadata capabilities for native type,
  dimensions, member layout and available debug extent. Factor partial
  global/local/type/header preparation from existing type/layout checking using
  original parsed children. A query may use published metadata before runtime
  storage/code admission. Missing metadata has an explicit diagnostic at the
  read point. Preparation must use the owning task's cumulative budget.
- [ ] Carry query evidence through function and top-level Name_query events,
  module/outer resolution, expression trees and constant results. `defined`
  consumes a checked presence predicate. Sizeof/offset consume the selected
  metadata and member chain; selected absence must never reach a spelling-based
  primitive fallback. Validate table/environment/source ownership throughout.
- [x] Extend global initializer and dimension manifests to retain query reads.
  Preserve the original before-owner dimension and through-owner initializer
  prefixes. Remove the unselected binding rerun in Driver.Global_array_layout;
  validate original ownership and consume checked query results for actual AST
  nodes during constant extent evaluation. Preserve exact initializer evidence
  through its repeated semantic walks.
- [ ] Add opaque implicit-output starts before take_string_literal_sequence.
  Capture the exact filtered function entry and first-marker emptiness, then
  associate the start with the completed real statement. Separate empty-marker
  advancement, header/argument reads and later target classification. Existing
  source-defined replacements retain their bodies; names grant no service
  capability. Defaults retain their separate source and binding ownership.
- [ ] Enforce native early read/validation failures before subsequent lookahead,
  including ordinary expression absence, query roots/members and missing
  providers. Preserve the separate statement-label probe boundary, parent
  restoration and already reached effects. Whole-command VM predecessor
  admission and native initializer execution remain distinct integration work.
- [ ] Verify real parser/runtime controls, original versus newer records,
  self/published-but-unadmitted metadata, member chains, first-empty markers,
  function defaults, local suspension, foreign/missing/replayed evidence and
  exact/one-below resources. Review fixes before the full unbuffered suite,
  build/format/generated checks, reference/provenance/corpus and committed-source
  CLI verification. Push and inspect hosted results; preserve all 102 prompt
  examples and all seven unfinished #635 acceptance criteria.

## In-progress evidence

The query work is uncommitted and extends 33f3b31. Initial runtime controls
WQ87H89R had four failures among 87 focused groups: published-but-unadmitted
`defined` returned zero, selected absence rebound after a nested publication,
retained-global sizeof was unsupported (HCRUN0003), and its dimension required
unresolved closed layout evidence (HCSEMA0027).

Parser receipts retain original roots, member children and completed expressions.
Task seals reject missing, replayed and foreign phases; repeated semantic walks
reuse one query-selection object. Rejection after a completed query cannot create
a containing command seal, and subsequent parsing can reuse the ledger.

Native Lex.HC:493-514 keeps keyword hash entries under TK_IDENT, while the hosted
lexer classifies them as Keyword tokens. The initial identifier-only predicate
was corrected after readonly review. Controls 8974SU7Q failed twice among 44
parser groups before the fix; R02JR8FI passed all 44. Keyword absence in a fresh
environment, literal macro expansion and local presence are covered.

Session seeding now records exact frontend-entry/semantic-symbol/primitive links.
Tasks share the links; frontend forks bind the known entries to fresh semantic
symbols. A matching name, kind, pinned origin or numeric ID cannot substitute an
entry. UQ9YVD98 reproduced the missing association; 94VD2UGS passed all 60 selected
session, visibility and parser groups after implementation.

Checked query evidence now crosses function and top-level event, module and outer
binding stages. `defined` uses its captured presence; selected queries do not
acquire later name-based bindings. Query tree matchers require the original AST
expression. Selected sizeof cannot reach the spelling-based primitive fallback;
its metadata remains pending. SOXAK05X passed 220 of 222 focused groups, with only
the original retained-sizeof and dimension controls failing. Global query manifests, checked
size/member metadata, implicit providers and full native error timing remain open.

Readonly review also identified lost local-query use counts. S8KXL3F9 reproduced
the new selected-local regression (one unrelated pinned-source control also
failed because that focused invocation used the repository root). THS4LFZL
passed the dedicated local-use control after preserving the checked local
binding through function, module and outer query stages. Saved absent/global
roots cannot count as uses of a later same-spelled local. The complete suite
must use dune runtest's fixture working directory.

After the local-use fix and negative missing-binding control, build/format/
generated/install checks passed. Full unbuffered suite PUCNU1KQ passed 2,210 of
2,226 groups in 84.310 seconds. All sixteen failures were exactly the fourteen
pending stateful #exe controls plus the retained-sizeof and query-dimension
controls; `_build/check-query-current.py` verifies every status. No hosted claim
is made for this uncommitted work. Fresh fetch confirmed feature HEAD and main
each remain 0/0 against origin; the pushed checkpoint is still 33f3b31.

Before implementing metadata, also cover `sizeof U8`, `sizeof I64i`, pointer
suffixes and `I64 N=sizeof N;N;`. The selected path currently rejects these while
metadata is pending; restore their support from exact seeded or published type
records before committing. A present query root must not depend on executable
or storage admission, and member/function/debug metadata must retain explicit
unsupported boundaries until implemented.

Those two additional runtime groups reproduce the pending metadata boundary:
72AE0CMU failed both the seeded-U8 sizeof and self-global sizeof controls with
HCRUN0003 (2 groups, 0.077 seconds). They were added after PUCNU1KQ; the current
test inventory is therefore 2,228 groups. No metadata implementation has been
added yet, and these controls must be green before this work is committed.

## Remaining full integration

### Scalar metadata and dimension integration in progress

Checked scalar compiler records now preserve seeded primitive associations,
original parser global publications and newly published retained scalar entries.
Source_type_reference factors existing primitive/internal type and pointer
checking. Read scalar size at the original query root, independently of storage
admission; complete only against the original query. Frontend forks rebind the
known seeded metadata without a second spelling lookup. Arrays, aggregate/local
metadata, function-pointer signatures and function debug extents still require
their separate checked preparation.

82MA7W28 passed 11/12 focused groups: builtin, self-global and retained-global
sizeof passed, with only the query dimension still failing. Dimension bindings
now retain exact ordered query manifests; Selected_query_expression preserves
the checked query through closed layout evaluation without replacement ASTs.
Driver.Global_array_layout consumes original bindings and source children without
rerunning name resolution. The dimension control subsequently passed.

Readonly review found the sizeof constructor could retain a pointer-size result
after removing its source pointer suffix. Y19MGU2U reproduced that gap among 14
focused groups. sizeof_source_matches now rejects changed source facts before
consuming the selected result, including added/removed pointers and added members.

Native PrsExp.HC:329-335 also requires a separate dot boundary before producing
the member token. XPKOYP9T reproduced a late failure: an internal-type member
executed a nested assignment (1 instead of retained 40). Query_member_started
now retains the actual dot location and original root/ordinal; completed members
retain that exact start. Task declarations validate pending-dot ownership and
reject an internal type at the dot. A scalar global reaches member-token effects
before reporting unavailable member layout, then stops subsequent directives.
Full member/class layout remains pending. HCGYFCDB passed all 139 focused groups.

Further review identified lost global name/position checks after removing the
binding rerun, and foreign query metadata accepted in aggregate offset/member
dimensions. HQ1WBQH3 reproduced both gaps (2/3 groups). Direct retained-record
comparisons restore name, name-origin and item/declarator position validation;
aggregate layout recursively checks query tables through offsets, unary/binary
expressions, field dimensions and anonymous unions. Q055S5JP passed all 148
focused groups in 0.545s. Readonly review found no remaining blocker in those
fixes. Build, format, generated-file and install checks then passed. Full
unbuffered suite BOEUK6HV passed 2,218 of 2,232 groups in 78.440s; all fourteen
failures are exactly the pending stateful #exe facade controls. The sizeof and
query-dimension regressions are restored. `_build/check-query-metadata.py`
verifies every status and all 102 preserved prompt examples. No hosted test
claim is made for this uncommitted work.

The external prompt now additionally requires native pre-initializer size
publication, pre-inference unsized-array snapshots, pre-publication dimension
preparation, distinct dot/member reads, declared rather than padded extents,
actual debug metadata and exact query/layout ownership. All 102 fenced examples
remain byte-identical. This work is uncommitted; the pushed checkpoint remains
33f3b31. Initializer query manifests and the remaining full query/provider plan
are not yet complete.

### Initializer manifests and original dimension ownership

Global_initializer_binding now retains the complete ordered query manifest for
each original initializer leaf, including non-name defined operands. Its selected
path validates exact leaf/expression/table associations. AST-only descriptors
remain explicit; missing evidence is an error. Inputs must use the original
global record from the supplied source batch. The second expression-binding
walk consumes the same selections through query_for and never calls its external
query resolver for initializer queries. Named events must match the retained
order, source leaf and selected object, without upgrading or dropping selection.

ALT3UKJC reproduced acceptance of reversed, missing and repeated initializer
queries; G7MBYX3U passed after manifest validation. The stricter validation exposed
the driver's fresh query path in 6WC10W9C. Capturing once and reusing the retained
descriptors restored all 45 focused groups in VSIXMUMC. Negative controls cover
non-name descriptors, distinct braced leaves, foreign tables, reconstructed
records, replacement selections and missing lookups. 89LZUJIQ passed all eight
initializer groups before the additional braced control.

RG62XO0G then reproduced dimension binding's acceptance of reconstructed global
records and dimension children, including a child with lost source. Exact record
and dimension membership checks close that gap while preserving the explicit
source batch authority. QHF6UCYG passed all 64 focused groups in 0.210s. Read-only
review found no remaining blocker in these changes. Build, format, generated-file
and install checks passed. Full unbuffered suite 6X2HKWZ3 passed 2,222 of 2,236
groups in 75.486s; all fourteen failures remain exactly the pending stateful #exe
controls. This records local work before its query checkpoint commit.

The pinned reference's 82 checksums and all eleven build-provenance scenarios
also pass. Full lexer JSON and parser JSON/normalized text match their existing
baselines: 528/528 files tokenize, 25 parse standalone and 126 with the prelude.
The prompt adds explicit initializer manifest and source-membership requirements
while preserving all 102 original fenced examples byte for byte.

Queries alone do not complete source execution. Partial storage/header admission,
native initializer effects, same-record extern joins and retained declaration
history, VM predecessor admission, source orchestration and result/artifact
projection remain required. Preserve ordinary no-#exe resource counts, the early
AOT task namespace split, limits before parsing, reached output on compile errors
and the full numeric/runtime/native/BIN/loader/bootstrap objective.
