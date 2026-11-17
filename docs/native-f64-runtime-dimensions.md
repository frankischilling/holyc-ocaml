# Native F64 declaration-time array bounds

The JIT source-task target accepts an original runtime array bound whose
evaluated expression has the F64 result class. An identifier-bearing bound
runs at its parser callback using the retained typed expression, selected
functions and task storage.
After the actual native entry completes, the captured binary64 word is
converted to signed I64 by truncating toward zero. The completed count is
then checked and used as the original array dimension.

This follows `Compiler/PrsVar.HC:247-283`, which calls `LexExpressionI64`
as each dimension is parsed, and `Compiler/PrsExp.HC:1140-1151`, which
calls the compiled expression before applying `ToI64` to an F64 result.
The conversion uses the same binary64-to-signed-word helper as F64 default
preparation. FISTTP's masked invalid result is the signed-minimum word for
NaN, infinity and out-of-range inputs, so the subsequent signed-negative
extent check rejects those counts.

```text
holyc run --target=host-jit-task --mode=jit --code-byte-limit=1048576 --format=json examples/native-f64-runtime-dimensions.hc
```

The example declares a global array, an automatic array and a static array
inside an uncalled function, and a class member array. Their shared `Bound`
function returns F64 `2.875`. Each declaration executes `Bound` once,
truncates its result to extent two and retains that same count for storage
layout and `sizeof`. The final I64 expression evaluates to 42.
The uncalled function's dimensions are prepared during its definition.

Each bound consumes its actual native execution steps under the cumulative
initializer allowance, including reached work on failure. Its effects
precede later syntax checks such as the closing `]`. Earlier committed
output survives a rejected negative bound or a later syntax error.
An F64 bound of `-1.5` converts to `-1` and fails the signed extent check
after its call. An F64 bound of `3.75` becomes three.

The native scalar capture can be consumed once with its exact compiled
dimension program, actual work and live task arena. Equal bits, foreign
images and arenas, replayed captures and expired parser callbacks cannot
authorize an extent. The original dimension dependency remains attached
to automatic frames, static allocations, aggregate layouts and size queries.

The independent integer interpreter remains integer-only for F64
expressions.

## Closed AOT bounds

Closed ordinary AOT bounds use the original numeric-expression evaluator.
After it produces a binary64 result, the array-dimension context applies
the same F64-to-I64 bit conversion as native JIT bounds. In particular,
an F64 result of `3.75` becomes three, while NaN, infinity and a finite
value outside the signed I64 range become the signed-minimum word. The
existing negative-extent check then rejects these three invalid cases.
Both successful and failed evaluations retain their original charged
numeric-expression visits, with exact and one-below limits.

An oversized decimal literal has a separate lexer boundary. The pinned
`Kernel/StrScan.HC:55-63` limits decimal fractions to 18 digits and
recommends exponent notation for larger numbers. Its signed 64-bit digit
accumulator also wraps on the 19-digit form
`9223372036854775808.0`, whose trailing decimal zero leaves a
positive-zero F64 value in this lexer. The native source tests retain
both that zero and the negative form's signed-zero bits. The invalid
F64-to-I64 conversion tests instead compute the exact F64 value
`4294967296.0*2147483648.0` using representable smaller literals.
These are source-audit and hosted observations; a native TempleOS
runtime capture is pending.

```text
holyc run --target=ir --mode=aot --format=json examples/aot-f64-dimensions.hc
```

The AOT example evaluates the two source leaves and their addition,
produces an extent of three, and returns I64 42 with three
dimension-preparation visits. The independent closed evaluator uses OCaml
floating arithmetic for some operators; these tests verify the completed
binary64 conversion and count publication, without proving native
TempleOS x87 optimizer or intermediate precision parity. Switch-case F64
conversion retains its separately audited validation behavior.

Ordinary AOT bounds depending on current runtime values still require
output relocation and callable authority. This hosted native execution
path and its closed-expression counterpart do not establish a TempleOS
runtime capture, exported HolyC ABI, BIN output or actual TempleOS
loader compatibility.
