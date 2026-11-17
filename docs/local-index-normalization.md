# Source-normalized local array indices

Register scoring depends on the address expressions that reach TempleOS's
OptPass3, not just the value of an index. The local-index proof reconstructs the
original dimension-scaled address tree and applies the admitted OptPass012
rewrites before counting member reads and address events.

The bounded index grammar includes integer literals, source-owned scalar reads,
parenthesized indices, unary plus/minus, and binary addition, subtraction and
multiplication. Full multidimensional chains retain each original dimension and
stride. Parentheses around an array base follow different parser flag rules and
remain excluded.

## Preserve the parser's intermediate steps

Scalar addition and subtraction do not enter the optimizer as bare arithmetic.
`PrsExp.HC:173-180` inserts a SIZEOF/multiply wrapper. `OptFixSizeOf` removes it
after the enclosing operator's rewrite attempt during the first pass. Some
constant folding therefore happens a pass later than an ordinary AST evaluator
would predict.

In an ordinary nonargument context, `(i*2)*(1+1)` reaches the end of the two
whole-function passes as nested shifts, rather than a prematurely combined
shift. The receipt keeps that timing even when both trees would produce the
same value and access count. A rewritten parent is not repeatedly optimized to
a fixed point.

Provided fixed-call arguments have additional preparation. `PrsExp.HC:469-485`
runs pass zero and pass one on each supplied argument before the whole-function
passes. Nested supplied arguments retain each enclosing preparation step.
These steps apply to the original index subtree; the later argument-value
conversion is not an extra index conversion. Authenticated numeric intrinsics
that use the same source PrsFunCall path receive the same preparation. This is
not a generic AST-call rule: defaults, variadic and indirect paths retain their
existing exclusions.

Multiplication by a dynamic zero is also distinct from multiplication by one.
`i*0` retains its scalar read and nonconstant subtree. It cannot acquire the
constant-zero address recipe merely because its runtime value is zero.

The normalizer is a bounded model, not an independent authority to lower code.
Original AST and typed operands, classes, conversion intent, dimensions, frame
geometry, completed headers and the complete canonical graph must still match.
Copied or changed owners invalidate saved and freshly requested proofs.

## Multidimensional address order

For `I64 a[2][3]`, the first index scales by 24 bytes and the final index by eight.
`a[0][0]` and `a[1][2]` have constant offsets zero and 40 within the array. A
dynamic prefix followed by a constant suffix must retain the original nested
address structure; it is not replaced by an algebraically flattened index.

The receipt identifies the surviving RBP-relative additions. OptPass3 charges
the first candidate range containing each displacement, and cancels an address
event only when the final value consumer reaches the member's exact start.
Explicit element address-taking keeps its IC_ADDR event. Scalar index reads
are counted separately, including reads inside expressions whose value is zero.

Constant bounds are checked against each original dimension. In a `[2][3]`
array, `[0][5]` and `[1][-1]` remain excluded even if a flattened byte offset
would lie inside the overall allocation. Dead arms and source after a return
do not erase those proof obligations.

## Limits and evidence

The model admits only the constant arithmetic and shift combinations whose
pinned rewrite is represented. Unsupported overflowing folds and combined
shift counts remain explicit boundaries. Side-effecting indices, nested calls
or updates, unproved casts and conversions, partial-array escapes and grouped
array bases are not admitted by this extension.

Existing array storage, initialization, bounds, lifetime and resource checks
remain in force. In particular, F64-array storage, narrow/Bool dynamic indexing
and unproved mixed numeric conversions do not become executable just because
an index tree can be scored. Public array-formal syntax stays rejected by the
original parser rule in `PrsVar.HC:255-257`.

The [grid example](../examples/local-index-normalization.hc) combines grouped
and computed constant indices with a full dynamic chain and returns 42.

Source references are TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`: `PrsVar.HC:247-281`,
`PrsExp.HC:728-747,1069-1099`, `OptPass012.HC:308-375,485-557,1259-1265`,
`OptLib.HC:484-503` and `OptPass3.HC:119-163,179-199,223-245`.
Independent expected maps and hosted tests qualify this bounded contract; no
new TempleOS runtime capture or complete issue #703 is claimed.
