# Register allocation beside local arrays

A local array can affect register selection without receiving a register.
TempleOS's `OptPass3.HC` first builds and sorts the complete candidate list.
It then trims unused candidates, rejects unbalanced address uses and assigns
registers. Dimensioned locals do not qualify for either automatic register bank,
but candidates that survive the earlier steps still count toward pressure.

For example, consider six integer parameters with unequal access scores. An
unused ordinary array has score zero and is trimmed. A `noreg` array is not a
candidate. An unused plain-`reg` array receives the forced score and survives.
That seventh survivor enables R12/R13 for the highest-scoring eligible scalars;
the array itself remains in its frame storage. Filtering the array out before
sorting would choose the wrong scalar homes.

## Complete members, separate homes

The source-owned member ledger retains each array's declaration position,
primitive class, dimensions, total extent, frame displacement and request.
Positive extents and the original layout establish nonoverlapping member ranges.
Dimensions must retain their original source and checked values. An array is
not replaced by its first element or by a synthetic scalar member.
The admitted declarations are automatic, nonpointer and noncallback arrays with
closed dimensions and no initializer. Multiple dimensions can participate in
membership without granting multidimensional element access.

Ordinary F64 members are excluded from candidate construction unless they use
plain `reg`. That distinction also applies to F64 arrays in the allocation
algorithm. It does not grant executable F64-array storage: the existing native
storage boundary remains separate.

The allocator keeps the pinned QSort tie order and the original sequence of
candidate construction, sorting, trimming, address filtering and bank scans.
The hosted physical-home report contains eligible members only; array storage
and its initialization metadata remain in the frame.

## Array address events

The admitted access recipes distinguish three rank-one index shapes:

- A constant zero index folds to the original frame displacement. A final read,
  assignment or supported update can cancel the exact-start address event,
  producing two score points and zero address balance.
- A constant in-bounds interior index folds to a different displacement inside
  the array. Address formation scores the first containing candidate range, but
  there is no exact-start cancellation.
- A direct scalar-variable index leaves the base-address event and the scaled
  index arithmetic. The final indexed consumer cannot cancel that base event.
  The scalar index keeps its own ordinary access score.

Taking `&array[0]` retains the address event through the original `IC_ADDR`
path. It is not treated as a read of the first element. These distinctions come
from `PrsExp.HC:772-803,1055-1099`, the admitted `OptPass012` folds and
`OptPass3.HC:129-162,193-197,239-245`.

Candidate matching uses source frame offsets and ranges, not just declaration
names. Constant negative and one-past indices therefore reject the complete
allocation proof even in dead arms or after a return. They must not be omitted
or attributed to the array when the source offset could reach another member.
Dynamic indexing retains the existing hosted bounds checks; it does not grant
raw access to neighboring storage.
Source scoring and executable lowering remain separate checks. Narrow integer
and Bool dynamic indices still hit the existing lowering restriction; a scoring
recipe alone cannot authorize execution.

## Large frames and safety

Scalar homes can coexist with the existing checked local-array storage and
large-frame page probes. Source semantic bytes, private native allocation,
simultaneous physical stack, instruction count and code bytes remain separate
limits. A selected home does not initialize an unwritten array element or
weaken pointer bounds, lifetime checks or checked-fault cleanup.

See [large native frames](native-stack-probing.md) for probe and unwind contracts.
Host ABI encoding checks are not a substitute for executing Windows CI, and
guest stack quotas do not measure available stack on an arbitrary host thread.

## Boundaries and evidence

The pinned parser rejects public array-formal syntax in
`PrsVar.HC:255-257`. Later dimension-aware branches for positive offsets do not
authorize that syntax; the existing parser rejection is source-correct.
Classes and unions are not automatically ineligible for register allocation,
so their treatment also cannot be inferred from local primitive arrays.

Explicit array register requests, zero-sized or overlapping ranges, broader
aggregate/callback members, unproved index expressions and unsupported numeric
or storage operations remain excluded. Membership for a multidimensional array
does not by itself prove multidimensional access normalization.
Static arrays, bare array decay and computed or grouped index expressions are
outside this allocation extension.

The [array-member example](../examples/array-member-registers.hc) compares the
six-parameter maps with noreg and plain-reg scratch arrays. Both functions
return 21; their independently checked physical homes differ.

The contract is derived from TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c` and checked with independent expected
maps and hosted tests. It is not a new TempleOS runtime capture, a byte-equality
claim or completion of issue #703.
