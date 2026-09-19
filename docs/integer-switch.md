# Bounded integer switch execution

Issue #668 connects ordinary `switch (...)` statements to the shared integer
source pipeline, checked block graphs, interpreter and generated x86-64 code.
Both preprocessing modes use the original case-preparation callbacks. Native
execution still uses an isolated hosted image rather than a TempleOS BIN.

```c
I64 Pick(I64 value)
{
    switch (value) {
        case 1: return 42;
        default: return 0;
    }
}
Pick(1);
```

This source returns I64 42 through `run --target=ir` and
`run --target=host-jit`. The maintained `examples/integer-switch.hc` fixture also
exercises ranges, source fallthrough and nested control flow.

```text
opam exec -- dune exec --root . -- bin/holyc.exe run --target=ir --mode=aot --format=json examples/integer-switch.hc
opam exec -- dune exec --root . -- bin/holyc.exe run --target=host-jit --mode=jit --format=json examples/integer-switch.hc
opam exec -- dune exec --root . -- bin/holyc.exe dump-ir --program examples/integer-switch.hc
```

## Original preparation and dispatch

Case expressions prepare while the parser reads their original labels, including
labels in unused functions and unreachable bodies. This first executable gate
uses the shared closed numeric preparation engine. Unsupported references,
queries, calls and effects reject; lowering never tries again with a later
environment. Closed floating results use the finite signed-I64 conversion bound
already used by dimension preparation. This is a bounded hosted evaluator, not
general native execution of `LexExpressionI64` or complete optimizer parity.

Each explicit endpoint prepares after its expression's terminating lookahead
and before the following colon or range delimiter is validated. A case completes
after the colon's following lookahead. The switch completes after the closing
brace's following lookahead; final range and overlap checks belong to that later
phase. Callback failure stops further parser delivery. Callback-free syntax
parsing retains its existing contract and does not confer execution authority.

Parser-owned receipts retain the original switch, endpoint role, predecessor
and source node. The declaration ledger retains evaluated values on its sealed
command, bound to the exact semantic table and module AST. Equal source text,
spans or integer values cannot supply a missing preparation. The source adapter
consumes each original typed selector and case endpoint once, then maps the
descriptor's ordered cases and default to the structured block builder.

The first implicit `case:` is zero. Later implicit cases advance from the
previous normalized upper endpoint. Descending inclusive ranges swap their
endpoints, so `case 5...3:` covers 3, 4 and 5 and a following implicit case is 6.
The source's I64_MIN sentinel remains distinct from ordinary increment: an
implicit case following that sentinel starts at zero. A switch without a case
has no admissible range. Final preparation lowers a positive minimum from 1
through 16 to zero and checks the resulting inclusive range against 65,535.
Overlapping case values reject after final range validation.

The selector executes once per reached switch. An internal-I64 zero addition
promotes narrow scalar classes while retaining their full register bits. A U64
selector instead uses the existing internal-I64 word view before subtraction. Lowering materializes the saved lower bound and range as internal-I64 immediates, subtracts the lower bound, and
emits the existing `IC_SWITCH`. Its two operands are the adjusted selector and
range; its ordered payload is `[default; target0; ...; target(range-1)]`.
Holes retain the default target. The graph validator checks the producer forms,
range/cardinality agreement and every target; repeated destinations remain valid.

An unsigned comparison sends every adjusted value outside the table range to
default, including negative/high-bit words. The interpreter indexes its checked
table. Native code uses the project's existing encoder and bounded direct
branches, with no interpreter fallback. Each reached `IC_SWITCH` charges one IR
instruction and retains one source/fault site despite its expanded machine code.
Selector instructions, class normalization and the lower-bound/range instructions
charge separately.

Case and default labels begin structural blocks in source order. Ordinary
fallthrough, a default in the middle of the body, the nearest `break`, nested
switches/loops and function-local gotos retain their normal continuations.
Calls, saved defaults, initialization faults, U0 completion and native
word-return completeness retain the existing shared execution contracts.

## Work, storage and exclusions

`--switch-work-limit` and the public `max_switch_work` argument default to
100,000 and must be positive. They bound evaluated numeric nodes separately
from dimension work, initializer instructions and runtime instructions.
For example, `case 2+3:` uses three preparation nodes; repeated calls reuse 5.
The invocation shares this allowance with nested source execution rather than
granting every `#exe` buffer a new allowance. Reports preserve reached work after
preparation or later execution fails.

Version-2 reports add `switch_work_limit` and `switch_preparation_work`; human
reports use the corresponding hyphenated names. The source report and native
report expose the same preparation count separately from runtime progress.

Dispatch storage has an additional hard invocation cap of 65,536 target slots,
including one default slot for each switch. The cap is checked before expansion
and is cumulative across nested source tasks and unused definitions. A
per-switch 65,535 range limit alone would not bound total table allocation.
Native code, IR, block, frame, call-depth and active-stack limits remain in force.

No-bound `switch [...]`, sub-switch `start:`/`end:` regions, multiple default
labels and effectful case evaluation remain explicit execution gates. The
multiple-default gate is a hosted support limit, not a C-style HolyC syntax
rule. Floating selectors, general conversions, full ABI/optimizer support,
native declaration execution, object/BIN writing, actual loader acceptance,
whole-tree compilation and bootstrap remain separate requirements.

## Source evidence

Reference commit: `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/OpCodes.DD:164` spells the source keyword `default`; `KW_DFT` is its
internal enum, while `dft:` remains an ordinary function label.
`Compiler/PrsStmt.HC:578-789` supplies original endpoint evaluation, implicit
values, inclusive ranges, label/body order, final lookahead, lower-bound
adjustment, range validation and duplicate detection.
`Compiler/PrsExp.HC:1117-1151` compiles and calls the case expression, then converts
an F64 result to I64. `Compiler/BackC.HC:602-771` supplies bounded unsigned
dispatch. Original receipt ownership, finite numeric conversion guards,
cumulative resource limits and checked host faults are explicit hosted policies.
Source-derived tests and own-encoder execution are not new TempleOS oracle
captures or a whole-language compatibility claim.
