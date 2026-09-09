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

### Array preparation receipts after 59dc72b

Continue the approved serial workflow. `Parser.declaration_event` will carry a
private prospective array owner (original name, command and environment), an
expression-preparation receipt, and an exact completed-dimension receipt.
Preparation occurs after expression lookahead but before validating `]`;
completion precedes Lex beyond `]`. Each receipt retains its original opening,
expression, index and predecessor. Empty first dimensions retain `None`, since
the pin assigns zero and does not establish negative-sentinel inference.

- [x] Add real parser RED controls in `test/test_stream_parser.ml`: two callbacks
  before the directive following each `]`, one preparation on a missing `]`, and
  callback rejection before following directives. Use the existing declaration
  sink so RED measures missing behavior rather than a missing API.
- [x] Extend `src/frontend/parser.ml/.mli` and all three dimension readers
  (global, local, aggregate member) with exact prospective owners. Publish the
  expression receipt before bracket validation and completion before subsequent
  lookahead. Argument arrays remain rejected at `[` without preparing a value.
- [x] Extend `src/driver/task_declarations.ml/.mli` to validate active command,
  environment, original owner, predecessor, once-only preparation/completion,
  and publication association. Sealed readers require the original table, AST
  and dimension. Add replay, omitted phase, cross-owner, aborted-command and
  nested-input controls in `test/test_task_declarations.ml`.
- [x] Check the focused parser/declaration suites and original corpus outputs;
  run the full suite and required build checks once the implementation settles.

These receipts do not themselves claim evaluated dimensions or runtime effects.
The following implementation must consume one checked preparation result for
layout, sizeof and unbraced initializer parsing, including task budget ownership.
The latter needs a receipt-bound result returned to the parser's extent-driven
element consumption; a unit observer alone does not supply that result.
It must not add a dependency
from Compiler_record to Aggregate_layout/Query_selection, re-evaluate at a query,
or treat VM-padded storage as declared extent. The full #635 gates remain open.

Receipt verification: the initial three behavioral controls failed in 2QZKPVDR
before implementation. The expanded parser/declaration suite ORBJOYDO passed all
97 groups in0.201s. Full unbuffered run3EYQZMJQ passed2248 of2262 in75.172s;
every failure is one of the same fourteen stateful #exe groups. Quoted Dune
format/generated/all/install targets, all82 checksums and eleven provenance
scenarios pass. Complete lexer JSON and parser JSON/normalized text match the
existing baselines. Independent read-only review found no receipt blocker.
Evaluated metadata, runtime preparation and all wider compiler work remain open.

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

### Ordinary source query integration

Continue the existing source integration design from pushed b080359. The ordinary
Integer_program.compile path invokes Parser.parse without query callbacks; its
task counterpart supplies the query ledger. Add original source evidence without
granting an ordinary compilation task runtime authority or weakening existing
runtime seal checks. Preserve limits-before-parse and current diagnostic origins.

Compatibility audit at b080359 confirms sizeof on scalar locals, parameters,
global scalars inside functions and I64 works in ordinary compilation. Local and
global array sizeof still fail HCRUN0003; defined return incorrectly yields zero.
The current runtime query ledger lacks local size metadata. Capture the actual
local/parameter publication and type children when its token is selected before
moving ordinary source compilation to that ledger. Do not regress supported
local sizeof or substitute a late name lookup. Investigate seeded I64 lookup
against native hash-mask rules before changing the ordinary primitive behavior.

- [x] Add maintained ordinary source controls for internal aliases, self-sizeof,
  sizeof dimensions, keyword presence and early error versus later definitions.
- [x] Retain original scalar local/parameter source publications and selected
  token associations in Parser, including self-initializer reads and restoration
  around nested parsing. Bind scalar metadata to those exact receipts and tables;
  preserve local warning uses. Arrays/member/signature preparation stay explicit.
- [x] Add an opaque ordinary source command seal distinct from task runtime
  authority. Reuse original declaration collection/query receipts in
  Integer_source/Integer_program without recapturing names after parsing.
- [x] Audit native seeded type lookup and protect currently supported source
  programs, exact resources, diagnostic order and source ownership.
- [x] Review source/foreign/replayed/late-effect controls, then verify the full
  suite, relevant CLI/corpus/reference checks and accurate remaining scope.

The parser retains private local/parameter publications at token production,
including the actual function owner and original type/declarator children. Local
metadata controls passed after correcting the native class distinction: public
unions and positive pointer companions reach member-token Lex; internal base
records reject at the dot. Native argv uses internal I64 with extent127 and
sizeof1016, while its frame slot remains8. O666G3RF reproduced both mistakes;
F6BMFC2A passed all three local groups after the fixes.

Session seeds the six generated public unions as exact Class/Aggregate_type
associations with KernelA.HH origins. Forks bind those known entries to fresh
semantic symbols. E7IS3IZ5 reproduced missing class metadata. The source ledger
now has a separate authority constructor and opaque seal, with exact registered
input validation before namespace allocation and the original display-path
scope name. Native PrsStmt45-51 also accepts those unions as class bases; the
parser control now tests I64 acceptance and U8 internal-base rejection. Five CLI
snapshots change only their IDs after the six added seeds.

Independent read-only review identified module-name loss and local function-
pointer compilation regression. N4OTA1A6 reproduced both. The saved b080359
binary confirms local and parameter function-pointer queries compile, while
function-pointer frame execution fails HCIRVM0011. Original local declarators
now provide native RT_PTR companion width/class metadata and preserve that VM
preflight boundary. Global function-pointer compilation was already outside the
integer domain and remains pending. 5CWUD73P passed all82 focused groups after
the compatibility fixes. Final read-only review found no remaining blocker in
the source-owner or local metadata changes; full verification follows.

Full unbuffered verification JR2O15B6 passed2238 of2252 groups in66.498s;
exactly the fourteen pending stateful #exe groups fail. The separate CLI tests
and updated deterministic snapshots pass. The full lexer JSON and parser
JSON/normalized text remain identical to their existing baselines:528/528 lex,
25 standalone parses and126 with the prelude. The external prompt now records
ordinary source authority, exact local source ownership, public-union class
seeding, pointer member timing, argv's declared extent and function-pointer
compile/preflight boundaries, preserving all102 original fenced examples.

All82 reference checksums and eleven build-provenance scenarios pass. Working-
tree CLI verification covers46 JSON reports and four function-pointer IR
artifacts across JIT/AOT:28 successful query results,16 still-pending #exe
diagnostics and two preserved function-pointer preflight failures. Twenty
successful-query reports exactly match the saved b080359 binary's resource and
result fields. These runs precede the new checkpoint commit; repeat their
identity checks on the rebuilt committed executable. Ordinary source query
integration is verified within this scope; all wider compiler and #635 work
listed above remains required.
