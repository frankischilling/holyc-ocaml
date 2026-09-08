# Persistent arrays and declaration initializers

Issue #627 connects persistent I64/U64/U8 arrays to the existing hosted
source-to-IR execution path. The user-authorized implementation mission and
Section 60 of the external prompt define the scope; this design does not
replace the complete compiler requirements.

Reference: `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
Baseline main: `1bef50509c728e6736ffb043db180e873b02919a`.

## Shape and storage

Retain one storage object per real declaration. A checked global layout joins
the exact global record, dimension bindings and source expressions before the
owner's publication; it uses the existing closed layout-expression evaluator.
Static arrays consume their exact Function_frame_layout locations. Both retain
positive dimensions, element width, checked product, suffix byte strides and
accessible extent. Unsupported dependencies, empty/zero dimensions, arithmetic
overflow and host capacity exhaustion remain explicit diagnostics.

Reuse typed VM cells and existing indexed references. Each persistent root gets
a base cell index and element count; stored references retain the same object
extent across calls and recursion. One byte element uses one typed cell but
charges one logical byte. Globals charge declared bytes, while each complete
static allocation rounds to eight bytes. Padding is inaccessible. AOT alignment
gaps and host bookkeeping remain outside the documented quota.

Immutable initial images retain a default state and sparse prepared element
updates. Do not allocate count-sized metadata arrays before the persistent-byte
limit is checked. Mutable execution cells are allocated only after checked
products, cumulative counts, host capacity and the execution limit. Each run
owns a fresh image. Scalar accessors and v1/v2 outcomes keep their contracts.

## Initializer source and destinations

Keep one statement/publication group per global declaration, containing an
ordered list of checked source leaves. Retain the complete initializer shape
and exact expression source at the initial global/local type producer. The
binding/tree stages join that source to the semantic expression and reject
missing, duplicate, reordered or substituted leaves, including leaves without
identifiers. Do not mint an owner/path marker that can bless arbitrary semantic
expressions. Static initializer inputs retain the same source-leaf evidence and
their exact local/type/frame identity.

The initializer layout joins the source tree to checked dimensions and produces
element destinations or direct byte-string copy ranges. Offsets follow native
recursive traversal, not the number of brace-path components. Retain each
destination's source leaf, owner, shape and semantic result. One direct string
copy remains one source leaf; do not fabricate scalar source expressions for
its bytes. Automatic braced initialization remains a separate existing boundary.

The source driver consumes one complete initializer batch per declaration,
including statics in uncalled functions. Numeric leaves use the existing typed
expression, constant-preparation and scheduled-store pipeline. Preparation
classifies each leaf separately. AOT embeds every constant before scheduled
load work; JIT executes leaves in declaration order. Destination narrowing never
changes an assignment's full RHS register result. Execution regions retain exact
leaf subtrees, declaration phase/order, transitive arithmetic guards and prior
completed effects on faults.

## Native details and explicit hosted boundaries

PrsVar.HC:123-204 recursively consumes fixed counts. Support exact nested lists,
flat braced lists and the parser's supported unbraced forms without C missing-
element fill. Validate arity against the retained source shape. Native commas
and closing braces are token-driven; do not assign universal C trailing-comma
or aggregate-spill behavior to irregular forms.

Direct U8 string initialization copies the current dimension's declared count.
LexExtStr includes one terminator. Equal-size and shorter copies are determined;
truncation does not append a terminator. Reject copies requesting bytes beyond
the owned literal. Row strings such as U8 A[2][3]={"42","ab"} have a checked
recursive interpretation. A multidimensional root string has unusual native
partial-copy behavior and must not silently become a whole-object copy.

PrsArrayDims sets ordinary first [] to zero, while inferred allocation branches
require negative counts. This audit did not establish an ordinary-source path
between them. Reject empty/zero persistent extents and preserve the unresolved
inference requirement. Do not claim native inference or pass-count parity.

## Acceptance and verification

All eight Section 60 fixtures must pass in JIT/AOT, returning numeric 42. The
two string-copy fixtures capture 3432; the others capture nothing. Preserve exact
result classes from checked typing. Cover U64, multiple ranks, aliases, recursion,
joined definitions, independent objects, fresh runs, phase-sensitive mixed
initializers, narrowing/RHS bits, exact source rejection and bounded scans.

Measure exact and one-below persistent/literal/preparation/runtime/frame/depth
limits. Include U8[9] globals at nine bytes and statics at sixteen, with nine
accessible bytes in each. Keep all earlier scalar/automatic-array/output gates.
Run focused and full tests plus CLI, formatting/generated/build/install,
82 checksums, 11 provenance scenarios and exact lexer/parser corpus comparisons.
Request independent review and require all five final-source CI checks before
normal protected merge, then verify merged identity, tree, fixtures and CI.

The storage extension does not complete formatting/runtime linking, broader
primitive/aggregate memory, stateful compilation/#exe, optimizer parity, native
backends, TempleOS BIN/loader acceptance or bootstrap. No new native capture is
claimed.
