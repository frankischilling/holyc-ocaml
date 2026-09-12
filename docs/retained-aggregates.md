# Retained aggregate sizes

Classes and unions with primitive members publish size metadata as their
original members are parsed. `sizeof` uses the entry selected by the original
parser read, including in a later `#exe` block or a retained function body.
Replacing the type does not change a size already consumed by an expression.
This supports metadata queries, not aggregate object execution.

`examples/stateful-exe-aggregates.hc` defines a sixteen-byte class and uses its
size as a member array bound. A retained function returns that array's size.
A second stream replaces the first class with a one-byte definition; the saved
size and the new size combine to return 42 in both outer modes. The CLI tests
require exactly 31 runtime steps and three preparation units, and check each
one-below failure. Stream dimensions use the task initializer allowance.

`examples/stateful-exe-aggregate-phases.hc` reads a class at name lookahead,
after its first member and during closing-brace lookahead. Those reads see
0, 8 and 16 bytes and combine to return 42. Both modes pass at 35 runtime steps
and three preparation units; the CLI checks each one-below failure too.

## Source and ownership

At reference `c26482bb6ad3f80106d28504ec5db3c6a360732c`,
`Compiler/PrsStmt.HC:1-59` publishes a class before name lookahead and completes
its size after `PrsVarLst`. `Compiler/PrsVar.HC:660-682` adds class member sizes
or takes the union maximum. The hosted path uses the shared checked aggregate
layout engine for packed members, closed array extents and anonymous unions.
Forward declarations expose zero-sized metadata.

The body, member and anonymous-union phases follow
`Compiler/PrsVar.HC:408-494,660-721`. Member placement follows type/array
lookahead but precedes metadata parsing and delimiter advancement. A directive
before a member's semicolon can therefore see the previous size, while one
after it sees the placed member. Anonymous unions use the enclosing size as
their base and share the containing class's size. Placement uses the same
checked addition, multiplication and union-maximum rules as completed layout.
The driver advances each original phase once without rebuilding a prefix.

Publication and completion require live receipts from the original parser.
The completion retains the exact AST item and the namespace's original
publication. Equal names, origins, reconstructed publications, foreign
namespaces and expired receipts cannot create a compiler record. The task
ledger caches the resulting metadata and seals only the original command item.
It does not grant type entries object storage or callable authority.
Partial progress also requires the exact predecessor chain and final phase.
Missing, repeated, expired and foreign phases cannot advance it. Each phase
creates an immutable size snapshot. New reads require the current snapshot and
the original live query callback; previously consumed values stay frozen after
later member placement or completion. Query lookup hashes immutable locations,
not callback-lifetime fields.
`Compiler/PrsExp.HC:303-348` reads the selected class size before advancing
past its name; that consumed value survives later lookahead.
Malformed forward declarations retain their reached name publication but do
not receive a completion receipt or become sealable commands. This follows the
`KW_EXTERN` call to `PrsClass` before `sm_semicolon` in
`Compiler/PrsStmt.HC:1029-1040`.

Member dimensions reuse the original checked dimension receipts, including
their command, member name, AST identity and predecessor chain. The later
semantic layout validates and reuses the same counts. It does not reevaluate
the original expressions or resolve their queries against a newer type.

## Offset expressions

`$$=expression;` evaluates its original expression after expression lookahead
and before semicolon validation. Class offsets replace the current size. Union
offsets replace the current union base without changing the containing size.
`$$` reads that current position; saved `sizeof` queries keep their original
values. These phases follow `Compiler/PrsVar.HC:408-449`.

Closed numeric expressions consume one preparation unit per evaluated leaf or
operator. Short-circuited operands consume none. Offset preparation uses the
initializer allowance, not the dimension-work counter. Floating results retain
their raw `F64` bits, as in `LexExpression` at `Compiler/PrsExp.HC:1168-1178`.
Runtime variables and calls still
require a separate execution path.

Negative offsets retain the greatest negative magnitude. After closing-brace
lookahead, the body-completion phase adds that padding, before attached
declarators and the final declaration delimiter. A query during closing-brace
lookahead sees the unpadded size. The shared layout checker rejects unrepresentable
magnitudes and final-size overflow.

Preparation receipts belong to the original aggregate, namespace, expression
and predecessor phase. Failed attempts consume the boundary too. Completed
layout reuses the checked bits. JIT source activation charges saved work at the
original offset event without evaluating the expression again.
Ordinary AOT offsets share the detached directive task's preparation allowance
at their original source events, without importing outer source names into that
task. Completed-source compilation checks those same receipts and does not
charge them again.

`examples/stateful-exe-aggregate-offsets.hc` moves a class position from one byte
to eight, observes that partial size, then appends an eight-byte member. Its
saved and completed sizes combine to return 42.
Both outer modes use 27 runtime steps and six preparation units. The CLI checks
those combined limits and each one-below failure.

## Remaining work

Runtime-dependent offsets, inheritance, aggregate-valued members, callbacks, member
metadata, attached storage and runtime-dependent member bounds remain outside
retained layout execution. Native extern-record reuse still needs its own
phase-aware admission. The supported partial sizes do not establish those
dependent layouts, member lookup or aggregate object storage.

The source gates and receipt tests are hosted observations. No new native
TempleOS capture was made; native/BIN/loader execution and bootstrap remain open.
