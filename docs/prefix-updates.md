# Pending prefix updates

TempleOS consumes a pending prefix update at the term's implicit dereference,
before reducing ordinary unary operators on its expression stack. Consequently,
`++*p` advances `p` and then reads its new pointee. `++(*p)` updates the pointee
without advancing `p`. The same distinction applies to decrement.

The parser now retains that binding for pending dereference, negation, logical
not, complement and unary plus. For example, `++-value` increments `value`
before negating the result. Parentheses start a separate expression and stop
this rearrangement. Original operator locations, complete expression spans and
macro provenance remain attached to their source tokens. The maintained
[example](../examples/prefix-updates.hc) checks both forms and returns 42.

Pointer field and indexed pointer field updates use their selected pointee
stride. The index executes once, and grouping keeps a pointee update separate
from a pointer update. Existing examples and tests that intended to update one
pointee now use explicit parentheses.

## Callback selection

A selected callback identifier or indexed callback slot consumes one pending
star through `PrsPopDeref`. Moving the update below that star must not turn the
callback word into a data-memory read. A source-checked alias therefore retains
the original numeric callback update and its eight-byte stride. It grants no
owned code address or data pointer. A group under the star or another remaining
star does not qualify for that alias.

## Evidence and remaining work

The reference remains `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
Complete `PrsExpression2` in `Compiler/PrsExp.HC:65-263` consumes update
flags at `PE_DEREFERENCE`. `PrsUnaryTerm` at lines 591-958 pushes ordinary prefix
operators but records increment/decrement as flags. `PrsExpression` at lines
264-301 saves those flags across nested expressions. Callback selection and
`PrsPopDeref` retain their separate pending-star behavior.

`test/oracle/prefix-updates` contains two repeated native JIT captures. The main
capture records seven observations; execution comparisons cover its four fields
listed in `implemented_fields`. A second capture verifies four controls for
indexed pointer fields, grouped pointees, decrement stride and complement.
IR and native source tasks compare these supported observations in both outer
modes. These checks establish executed behavior, not emitted-byte or original
AOT parity.

The original flag precedence of mixed or repeated updates, prefix-plus-postfix
combinations, and discarded updates on grouped non-lvalue results remains
unfinished. The main capture retains those observations without treating a
hosted rejection as a compatibility result. This change does not close the
broader expression or aggregate requirements.
