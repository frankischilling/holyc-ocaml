# Native F64 offsets and member metadata

Native JIT source tasks evaluate supported F64 class `$$` offsets and member
metadata expressions during declaration parsing. Each expression executes once
and saves its raw 64-bit result. Declarations inside an uncalled function still
perform this work. Original JIT `#exe` tasks use the same path in either outer
mode.

Run the maintained example with:

```text
holyc run --target=host-jit-task --mode=jit --code-byte-limit=1048576 --report-version=2 --format=json examples/native-f64-member-preparation.hc
```

It returns 42 without output. `Offset()` and `Metadata()` each increment the
preparation counter once. The offset call returns `3.75`, whose word is
`0x400e000000000000`. The next `$$` expression subtracts that word and sets the
offset to 16, so the final `I64` member makes the class 24 bytes long. The
metadata call retains the raw word for `2.75`.

## The original result word

At the pinned TempleOS revision
`c26482bb6ad3f80106d28504ec5db3c6a360732c`, class offsets at
`Compiler/PrsVar.HC:430-441` and numeric metadata at
`Compiler/PrsVar.HC:675-686` call `LexExpression`. Its complete implementation
at `Compiler/PrsExp.HC:1168-1178` returns the word produced by `Call` without
converting F64 to I64. By contrast, array bounds use `LexExpressionI64` at
`Compiler/PrsExp.HC:1140-1151`, which applies `ToI64` to F64 results.

The offset and metadata destinations therefore accept checked scalar F64
results in native tasks and retain their bits. A subnormal F64 whose raw word is
16 sets the offset to 16. Signed zero, infinity and NaN words can be retained
as metadata. Later layout and allocation checks still apply to offsets.
Closed offset and metadata preparation also retains non-finite words; array
dimension conversion keeps its separate signed-count checks.

The hosted layout checks remain a compatibility boundary. A raw `-0.0` offset
is the signed word `I64_MIN`; calculating its negative displacement reports
`HCSEMA0008`. A raw positive-infinity offset followed by a member that overflows
the signed class size also fails. Both cases preserve the completed native
capture, reached preparation work and earlier output.

## Execution and ownership

The native result comes from the original machine entry. Its typed root,
source receipt, task, namespace, lowered program and work count must match the
pending request. A live arena owns the scalar capture, and completion consumes
it once. Metadata alone, a reconstructed program, another image or arena,
guessed work, an expired owner and replay cannot complete preparation.

Preparation uses the shared initializer allowance. Exact limits succeed;
one-below limits preserve the reached work and report `HCIRVM0007`. Output
from a reached expression remains visible if a later member or delimiter fails.

The native tests inspect actual F64 captures, including finite and subnormal
offsets and metadata with signed zero, infinity and a NaN payload. They also
exercise original and substituted captures, nested source tasks, declaration
timing and preparation limits. Semantic and compiler-record tests check closed
raw words, duplicate metadata lookup and retained class replacements. Public
CLI tests cover both outer modes, and installed compiler checks run the example
through built, staged and installed binaries.

## Remaining work

The independent integer interpreter still rejects live F64 preparation. Closed
expressions work in ordinary AOT. Expressions that read or call output-module
declarations still need executable or storage ownership and relocation support.
Isolated native live preparation needs its own original source-task context.
Data and function addresses need owned transport; runtime metadata reflection
also remains open under #686. These changes do not complete #685.

The evidence is pinned source and hosted execution. It adds no TempleOS runtime
capture or emitted machine-code comparison. Closed arithmetic uses the hosted
numeric evaluator; this does not establish identical intermediate x87 precision
for every expression.
