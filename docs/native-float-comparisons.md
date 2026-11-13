# Native F64 comparisons

This subset follows TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`. Test expectations come from that
compiler and the documented x87 and integer instructions. They are not new
TempleOS runtime captures.

Original checked callable source can use scalar F64 `==`, `!=`, `<`, `>=`, `>`
and `<=` as integer values or direct conditions. Supported arithmetic operands
retain the existing source-proved extended-precision links. Fixed runtime
integer operands use the proved numeric conversion events. Both outer JIT/AOT
modes work with the `host-jit` and `host-jit-task` executors. The integer IR
interpreter does not gain floating-point execution.

The example [native-float-comparisons.hc](../examples/native-float-comparisons.hc)
returns `1`:

```text
holyc run --target=host-jit --mode=jit examples/native-float-comparisons.hc
holyc run --target=host-jit-task --mode=aot examples/native-float-comparisons.hc
```

## Equality compares materialized words

`OptPass789A.HC:567-595` sends `==` and `!=` to integer `ICCmp`, even when the
operands are F64. `BackB.HC:164-198` compares their materialized 64-bit words.
This differs from IEEE numeric equality:

- Positive and negative zero are unequal.
- Two identical NaN words are equal.
- Distinct NaN payloads are unequal.
- Arithmetic operands and implicit integer conversions materialize binary64
  before equality.

For `a=9007199254740992.0` and `b=1.0`, `(a+b)==a` is `1`: storing the sum rounds
it back to `a`. This equality rule does not authorize suffix bitcasts or arbitrary
NaN payload construction in source.

## Ordered values follow the selected x87 template

`BackFA.HC:519-546` selects the floating comparison template. The table below
lists the final FCOMIP operand order and the flags tested to produce `1`.
`L:R` means FCOMIP compares the left source operand against the right.

| Operand event | `<` | `>=` | `>` | `<=` |
| --- | --- | --- | --- | --- |
| No linked producer | L:R, B | L:R, AE | R:L, B | R:L, AE |
| Left producer linked | R:L, A | R:L, BE | R:L, B | R:L, AE |
| Right producer linked | L:R, B | L:R, AE | L:R, A | L:R, BE |

`B` tests carry, `AE` tests no carry, `A` tests neither carry nor zero, and `BE`
tests carry or zero. FCOMIP sets carry, parity and zero on unordered operands.
The resulting NaN value rows, in column order, are therefore `(1,0,1,0)`,
`(0,1,1,0)` and `(1,0,0,1)`. These are the instructions in
`Templates.HC:39-85,280-322,456-498`; replacing them with a host-language
floating comparison would change behavior.

Precision also depends on events, not just types. With the `a` and `b` above:

- `(a+b)>a` and `a<(a+b)` are `1` because the sum remains extended.
- `(a+b)<(a+b)` is `1`: the left sum materializes, while the right sum remains
  extended.
- `F64 t=a+b; return t>a;` is `0` because the assignment materializes the sum.
- Runtime integer `n=9007199254740993` compares greater than F64
  `x=9007199254740992.0`. The reversed `x<n` is also `1`.
- The integer literal in `9007199254740993>x` rounds before comparison and gives
  `0`. Mixed runtime `n==x` is `1` after materialization.

A runtime integer conversion can select a linked template within the comparison
itself, even without an incoming extended arithmetic value. The last conversion
event determines its orientation (`BackFA.HC:31-37`).

## Direct conditions have their own predicates

The reference fuses comparison conditions before choosing their instructions.
`OptLib.HC:230-245,357-372` complements the comparison for `BR_ZERO` and preserves
it for `BR_NOT_ZERO`; logical NOT switches that phase. `PrsStmt.HC:472,500,544`
uses `BR_ZERO` for `if`, `while` and `for`, while line 522 uses `BR_NOT_ZERO` for
`do`.

For the effective branch opcode `<`, `>=`, `>`, `<=`,
`OptPass789A.HC:188-224` and `BackFB.HC:226-426` select:

- No linked producer or a left link: compare R:L, testing A, BE, B, AE.
- A right link: compare L:R, testing B, AE, A, BE.

An unlinked NaN comparison can consequently behave differently when saved as a
value and when used directly:

```c
I64 saved=a<b;
if(saved) return 1;  // Taken when a is NaN.
if(a<b) return 1;    // Not taken when a is NaN.
```

The hosted implementation keeps its canonical comparison and branch sites and
their existing step charges. Hosted lowering can remove NOT by exchanging the
successors. Private comparison evidence retains the source NOT parity and
selects the fused source predicate; it does not replace the graph with an
unmetered branch. Branch role, operand events and consumer identity are also
part of that evidence.

Value templates load exact binary64 words with FLD. Direct branches may select
`ICBuiltInFloatConst`, including positive zero and one. Negative zero remains an
exact binary64 word load in arithmetic, value comparisons and direct branches:
the builtin selector's raw-word `!d` test does not consider it zero.
Option-sensitive PI/log constant loads remain rejected unless their original
option and event selection is proved. An ordinary value comparison does not acquire that branch restriction.

## Authority, allocation and limits

Comparison receipts bind the original runtime context, checked source body,
frame, instruction and result identities, operand order, types, flags, branch
role and graph edges. Retained receipts revalidate their source and graph before
emission. Equal source text, copied graphs or source mutations cannot authorize
a new comparison. Tests mutate both fresh and retained evidence, including
producer flags, comparison operators, result types, literal words, branch
polarity, destinations, condition roles, compiler options and successor order.

Comparison reads contribute the pinned source access counts to register
selection. The operation itself adds no read or write score. Ordinary F64
members remain excluded from automatic register candidates;
`noreg`, forced `reg`, explicit register reservations and narrow integer spills
keep their existing semantics. Physical scratch registers do not authorize new
source storage behavior.

Native tests cover finite order and equality, signed zeros, infinity,
arithmetic-produced NaNs, all ordered value/link orientations, stored versus
direct conditions, NOT parity and each loop role. Retained executions exercise
all earlier step limits around linked producers, comparisons and branches,
then rerun the same image after every fault. Separate architectural tests load
raw qNaN/sNaN payloads into FCOMIP and check flags and an empty x87 stack; those
are lower-level instruction tests, not HolyC source execution. Arbitrary NaN
payload and signaling-NaN source execution remain unqualified because the
public source subset does not provide a raw-F64 injection path.

The original `<` and `>=` operators have one additional source rule.
`OptLib.HC:141-160` changes a direct PUBLIC F64 frame read compared with a direct
positive-zero immediate into a signed integer-word comparison. Both operand
orders, integer `0` converted to F64, value results and direct conditions follow
that rule. With negative-zero `x`, `x<0.0` is `1` and `x>=0.0` is `0`. A negative
NaN word also compares below positive zero. `>` and `<=`, a negative-zero
immediate, and the distinct `F64i` class do not meet that rule. The proof recognizes
unary negation of literal zero as its exact negative-zero word. It does not grant
general constant folding. Computed-zero expressions and alias-simplified reads
such as `(x+0.0)<0.0` remain rejected without their own normalization proof.
An original direct integer read compared with F64 zero using `<` or `>=` also
remains an unproved conversion-phase boundary; integer operands compared with
runtime F64 parameters retain their separately proved conversion events.

Arithmetic-wrapped comparison conditions such as `if((a<b)+0)` remain
rejected. Removing the integer identity before branch fusion can change the
NaN predicate; ordinary arithmetic consuming a comparison value is a different
source context.

Constant-only comparisons without a normalization proof, comparison results
converted to F64, floating comparison chains, short circuit, floating globals,
floating callback signatures, compound updates, suffix bitcasts and other unproved source shapes
remain outside this increment. This does not claim complete TempleOS optimizer
parity, full floating storage, exported ABI support or bootstrap closure.
