# Conditional integer comparison chains

Integer chains now run in `if`, `while`, `do/while` and `for` conditions through
the IR interpreter and native x86-64 backend. Each operand runs once. In
`a<b<c`, a false `a<b` skips `c`; a true first link compares the original `b`
with `c`. All six integer comparison operators use this rule.

`examples/conditional-comparison-chains.hc` returns 42. Its first comparison is
false, so `Touch` runs twice. The corresponding native TempleOS control returned
9 with an operand count of two. The true chain and explicitly grouped
`(Touch(3)<Touch(2))<Touch(1)` each ran all three operands.

Grouping retains its source meaning. `(a<b)<c` compares the first Boolean with
`c`. An entire chain used as a condition can short-circuit, while a chain inside
an arithmetic expression, assignment, return value or call argument remains
eager. For example, `if((3<2<1/z)+1)` still reaches division by zero when `z` is
zero. Conditional logical AND/OR and logical not compose with chain conditions.

The cumulative unsigned computation class survives each link, including COM's
forwarded U64 class. `(~a)>b>c` and `((~a)>b)>c` therefore keep their distinct
ungrouped and grouped behavior.

Graph validation requires a unique value definition that dominates every use
in another block. Local forward and self references remain invalid. The
interpreter retains temporary values within an invocation and starts a fresh
map for each called function. Native code gives shared values permanent private
frame slots; calls, recursion and loop re-entry preserve the original middle
word. These slots count toward the existing native frame limit. The source call
context retains the exact graph layout and instruction records, so changed or
copied comparisons and branches fail preflight.

The [oracle fixture](../test/oracle/conditional-comparison-chains.json) records
50 field/source pairs, each observed twice in one TempleOS native JIT boot:
36 context values, six cumulative-class values and eight conditional result or
effect fields. It includes exact commands, setup records, decoded output and
capture hashes from the verified final ISO. Hosted tests replay these fields
in JIT/AOT parsing modes and both execution targets. These are same-boot native
JIT observations; they do not establish native AOT parity.

Floating chains and multiple pending reductions such as `1==2<3==1` remain
unsupported. The latter have separate compile/listing evidence under
[#593](https://github.com/frankischilling/holyc-ocaml/issues/593), without a
claimed numerical execution result. This change closes
[#797](https://github.com/frankischilling/holyc-ocaml/issues/797); the full ABI,
optimizer, module loader and compiler bootstrap remain separate work.
