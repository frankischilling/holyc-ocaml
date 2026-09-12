# Retained aggregate sizes

Completed classes and unions with primitive members can publish size metadata
to the current source task. `sizeof` uses the entry selected by the original
parser read, including in a later `#exe` block or a retained function body.
Replacing the type does not change a size already consumed by an expression.
This supports metadata queries, not aggregate object execution.

`examples/stateful-exe-aggregates.hc` defines a sixteen-byte class and uses its
size as a member array bound. A retained function returns that array's size.
A second stream replaces the first class with a one-byte definition; the saved
size and the new size combine to return 42 in both outer modes. The CLI tests
require exactly 31 runtime steps and three preparation units, and check each
one-below failure. Stream dimensions use the task initializer allowance.

## Source and ownership

At reference `c26482bb6ad3f80106d28504ec5db3c6a360732c`,
`Compiler/PrsStmt.HC:1-59` publishes a class before name lookahead and completes
its size after `PrsVarLst`. `Compiler/PrsVar.HC:660-682` adds class member sizes
or takes the union maximum. The hosted path uses the shared checked aggregate
layout engine for packed members, closed array extents and anonymous unions.
Forward declarations expose zero-sized metadata.

Publication and completion require live receipts from the original parser.
The completion retains the exact AST item and the namespace's original
publication. Equal names, origins, reconstructed publications, foreign
namespaces and expired receipts cannot create a compiler record. The task
ledger caches the resulting metadata and seals only the original command item.
It does not grant type entries object storage or callable authority.
Malformed forward declarations retain their reached name publication but do
not receive a completion receipt or become sealable commands. This follows the
`KW_EXTERN` call to `PrsClass` before `sm_semicolon` in
`Compiler/PrsStmt.HC:1029-1040`.

Member dimensions reuse the original checked dimension receipts, including
their command, member name, AST identity and predecessor chain. The later
semantic layout validates and reuses the same counts. It does not reevaluate
the original expressions or resolve their queries against a newer type.

## Remaining work

Queries during a partial definition, including closing-brace lookahead, fail
explicitly. Offset directives, inheritance, aggregate-valued members, callbacks,
member metadata, attached storage and runtime-dependent member bounds remain outside retained
layout execution. Native extern-record reuse and partial size mutation still
need their own phase-aware admission. Completed forward/definition queries do
not establish that wider behavior.

The source gates and receipt tests are hosted observations. No new native
TempleOS capture was made; native/BIN/loader execution and bootstrap remain open.
