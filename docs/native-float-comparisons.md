# Native F64 comparisons

This subset follows TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`. Test expectations come from that
compiler and the documented x87 and integer instructions. They are not new
TempleOS runtime captures.

Original checked callable source can use scalar F64 `==`, `!=`, `<`, `>=`, `>`
and `<=` as integer values or direct conditions. An original F64 return or typed
store can convert the completed integer result to `0.0` or `1.0`.
Supported arithmetic operands retain the existing source-proved
extended-precision links. Fixed runtime
integer operands use the proved numeric conversion events. Both outer JIT/AOT
modes work with the `host-jit` and `host-jit-task` executors. The integer IR
interpreter does not gain floating-point execution.

Operands can come from proved automatic, global and static scalars, fixed
arrays, packed members and owned F64 pointers. Their original address receipts
remain part of the source replay. A memory read supplies a binary64 word, while
subsequent arithmetic keeps only the extended links selected by the source.
Repeated calls and retained functions continue to use their original storage
bindings. See [storage expressions](native-f64-storage.md#expressions-and-updates).

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

The source's store and link events determine precision. With the `a` and `b` above:

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
Pi and logarithmic constants use builtin x87 loads in supported direct
conditions unless the original function enables `No_builtin_const`. The plan
authenticates the original options and load events. Ordinary value comparisons
retain their binary64 loads, so they can differ from conditions at a builtin
constant's binary64 boundary.

## Immediate comparisons fold before branch selection

Two original immediate operands can produce a saved I64 comparison word.
`OptLib.HC:124-139` first converts an integer immediate to binary64 when the
other operand is F64. Lines 161-166 then remove the two immediate operands,
and `OptPass012.HC:725-822` replaces the comparison with `IC_IMM_I64`.

The folded `==` and `!=` operators compare the binary64 words. Thus
`0.0==(-0.0)` is `0`, while `0.0>=(-0.0)` is `1`. Integer immediates round
before either kind of comparison: `9007199254740993==9007199254740992.0`
is `1`. Direct conditions use this saved word because folding precedes branch
fusion and builtin x87 constant selection.

`OptPass012.HC:180-188` also folds unary minus of an immediate F64 before
comparison selection. The receipt recognizes the original non-NaN literal,
including negative zero, with no attached result conversion. A negative
immediate can therefore be compared with a stored value or consume an extended
left operand. It cannot authorize arbitrary arithmetic folding or reassociation.

The hosted backend records the folded word in its original-source receipt and
emits it at the canonical comparison site. Operand and comparison step charges
remain visible, including during a quota fault. The receipt also applies to
closed native declaration defaults, whose proof binds the exact lowered entry,
storage context and typed expression. Parentheses and integer arithmetic around
the comparison keep that ownership. Computed floating operands that require
additional folding or reassociation still need their own normalization proof.

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
then rerun the same image after every fault. Source storage fixtures write an
exact quiet-NaN word through an owned U64 view and check value, direct-condition
and left/right arithmetic predicates. A packed signaling-NaN fixture checks
bitwise equality and unchanged payload transport. Separate architectural tests
load qNaN/sNaN payloads into FCOMIP and check flags and an empty x87 stack; their
coverage remains distinct from those source programs.

The original `<` and `>=` operators have one additional source rule.
`OptLib.HC:141-160` changes a direct PUBLIC F64 storage read compared with a direct
positive-zero immediate into a signed integer-word comparison. Both operand
orders, integer `0` converted to F64, value results and direct conditions follow
that rule. With negative-zero `x`, `x<0.0` is `1` and `x>=0.0` is `0`. A negative
NaN word also compares below positive zero. `>` and `<=`, a negative-zero
immediate, and the distinct `F64i` class do not meet that rule. The proved
immediate-negation fold preserves negative zero, so it cannot acquire the
positive-zero shortcut. Computed-zero expressions and alias-simplified reads
such as `(x+0.0)<0.0` remain rejected without their own normalization proof.
An original direct integer read compared with F64 zero using `<` or `>=` also
remains an unproved conversion-phase boundary; integer operands compared with
runtime F64 parameters retain their separately proved conversion events.

Arithmetic-wrapped comparison conditions such as `if((a<b)+0)` remain
rejected. Removing the integer identity before branch fusion can change the
NaN predicate; ordinary arithmetic consuming a comparison value is a different
source context.

Fixed callback signatures can transport F64 arguments and results for these
comparisons. `PrsFunCall` saves the callee before argument effects and retains
the selected parameter and return classes; `OptPass789A.HC:779-785` transports
the result through RAX. Arguments and results cross the call as materialized
words, without extending an x87 link across the call. Tests cover signed-zero
equality, unordered branches and rounded callback words passed to a separate
comparison function at the boundary `2^53+1`. Direct comparisons of callback
results, including the tested local materialization in the same caller, still
lack a source normalization proof and remain rejected.

An ordinary call's original typed consumer can require a numeric conversion of
its returned word. The compiler authenticates that conversion against the exact
`IC_CALL_END`, checked call result and original consumer's operand path. Mixed
arithmetic reuses the expression lowerer's conversion decision; grouping and
unary plus preserve the original child. Each use rechecks the source batch,
consumer, scalar metadata and conversion. `OptPass789A.HC:1026-1056` captures the
returned integer word and applies its result conversion. `OptPass3.HC:173-175`
and `OptLib.HC:545-553` allow the subsequent FILD to donate its extended result
to adjacent arithmetic through `BackFA.HC:21-50,123-166`. A call returning
`9007199254740993` therefore yields `9007199254740994.0` when immediately used
in `Word()+1.0`. The complete region is replayed from its original call targets,
arguments and saved defaults before this link is admitted.

Callback leaves use the same replay through their original selected signature,
callee capture and saved defaults. An integer callback result can donate its
post-call conversion; an F64 callback result remains a materialized word. The
maintained [AOT defaults example](../examples/aot-native-defaults.hc) combines
direct calls with an anonymous callback whose own saved default is `2.75`,
preserving that header's value when it invokes `Target` and produces `5.25`.

Fixed arguments and F64 returns still materialize binary64 words. Returning
`Word()` from an F64 function, or passing it to an F64 parameter, rounds that
word to `9007199254740992.0` before later arithmetic. Existing F64 call results
also remain materialized. An F64 result converted to I64 uses FLD/FISTTP.

These call conversions work in ordinary entry expressions, fixed arguments and
function returns. Each call keeps its argument evaluation order and its existing
instruction charges. A conversion receipt alone cannot authorize an x87 link;
the exact source region must also match. Tests check both conversion directions,
the post-call link and materialized boundaries, reached output,
quota failures and rejection of copied producers, foreign contexts or changed
source conversion flags or consumer children.

Callback declaration initializers work in AOT and live JIT source tasks;
isolated JIT compilation retains its closed-expression initializer restriction.
Live JIT native source tasks admit saved F64 defaults on the selected named or
anonymous callback header; their result transport crosses calls as materialized
words. Nested JIT `#exe` uses that source-task behavior under either outer mode.
[Closed numeric defaults](native-f64-defaults.md) also prepare in isolated
programs and ordinary outer AOT source. Source-owned numeric intrinsic calls use
their [separate declaration proof](aot-intrinsic-defaults.md). Ordinary AOT
module-address dependencies and variadic F64 callback arguments remain open.

Closed [numeric declaration initializers](native-numeric-initializers.md) can
convert a completed integer comparison word to F64. For example,
`F64 value=1.5<2.0;` stores `1.0`. This conversion follows expression completion;
it does not grant a new x87 link through the comparison.

Native functions also admit a comparison returned directly as F64 or used by an
original F64 initializer or assignment. `BackFA.HC:519-546` completes an ordered
comparison at `CN_INST`, consumes its floating inputs and produces an integer
Boolean. `OptPass789A.HC:1045-1056` then applies `ICF_RES_TO_F64` at `CN_RES`;
`BackFA.HC:123-166` converts the word with FILD/FSTP at these materialized
boundaries. Canonical replay must reproduce the comparison's original conversion
flag. Removing the typed conversion or changing the instruction flag revokes
both cached and fresh receipts.

The conversion preserves the comparison's selected operand precision. With
`a=9007199254740992.0` and `b=1.0`, an F64 function returning `(a+b)>a` returns
`1.0`; storing `a+b` in an F64 local before comparing it with `a` produces `0.0`.
Returning `(a+b)<(a+b)` produces `1.0` because the left sum materializes while the
right sum remains extended. Equality still compares stored words, so a direct
F64 return of `(a+b)==a` is `1.0`.

A comparison consumed by F64 arithmetic or passed directly as a fixed argument
still lacks a proved terminal consumer. Those forms, floating comparison chains,
floating short circuit, suffix bitcasts and other unproved source shapes remain
outside this subset. Supported increment/decrement and compound stores
retain their own source receipts; observing an indirect arithmetic compound
result or a dynamic narrow integer compound result still requires its original
result supplier. Complete TempleOS optimizer parity, remaining floating storage
shapes, exported ABI support and bootstrap remain open.
