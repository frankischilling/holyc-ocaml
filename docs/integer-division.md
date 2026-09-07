# Integer division and remainder

Reference commit: `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

`Ir.Integer_interpreter` executes raw `IC_DIV` and `IC_MOD` on internal scalar
`I64` and `U64` words. The existing source driver exposes both through
`holyc eval`. For example, `examples/integer-division.hc` contains
`(85/2)+(85%2);` and returns `43`.

## Source contract

`Compiler/CInit.HC:66-67,293-294` gives both operations two arguments and one
result and maps the source `/` and `%` operators. `OptLib.HC:96-179` selects
the common integer class. Two `I64` operands produce `I64`; each other word
pairing produces `U64`.

`OptPass789A.HC:524-534` selects `ICDiv` or `ICMod` for integer backend
operations. `BackA.HC:355-370,425-440` moves the divisor into RCX and the
dividend into RAX. For unsigned words it clears RDX and emits `48 F7 F1`
(`DIV RCX`). For signed words it emits `48 99` (`CQO`) followed by `48 F7 F9`
(`IDIV RCX`). Division takes RAX; modulo takes RDX. The matching opcode forms
are in `OpCodes.DD:704-713,780`.

Signed division truncates toward zero. A nonzero signed remainder has the
dividend's sign: `-7/3` is `-2`, and `-7%3` is `-1`. Unsigned arithmetic
uses all 64 payload bits. In particular, `0xFFFFFFFFFFFFFFFF/3` returns
`U64` decimal `6148914691236517205`, retained as a string in JSON output.

## Faults and validation

`HCIRVM0009` reports a zero divisor. `HCIRVM0010` reports signed minimum
divided or reduced modulo minus one. Both signed instructions execute IDIV,
whose quotient overflows at that boundary even when only the remainder is
needed. The interpreter checks these cases before calling host arithmetic.
The unsigned pairing with the same bits is valid.

These are execution errors. Whole-graph preflight first validates all opcodes,
types, flags and payloads, including unreachable blocks. A supported arithmetic
operation in a skipped block does not fault. A reached fault consumes one
instruction step and retains its block, instruction and source span. A spent
budget reports `HCIRVM0007` before the next instruction. Faults return no
partial result, including when a prior instruction filled the return latch.

The current source-expression path evaluates both operand trees of Boolean
value expressions. `0&&(1/0);` and `1||(1%0);` therefore reach their faults.
This does not implement conditional short-circuit lowering or general
exception handling.

## Evidence and remaining work

The division test group has five table/control tests and a 500-case property
that compares both results with independent bit-at-a-time long division.
Source tests exercise the public parser, semantic passes, lowerer, verifier,
and interpreter. Five CLI golden rules cover numeric output, unsigned JSON,
both fault codes, JSON diagnostics, exit status 1 and empty stdout on failure.

All of this evidence is hosted. No TempleOS execution or native exception
delivery is claimed. [Issue #584](https://github.com/frankischilling/holyc-ocaml/issues/584)
implements the raw operations; [issue #585](https://github.com/frankischilling/holyc-ocaml/issues/585)
tracks optimized behavior and fault phases.

`OptPass012.HC:403-455` folds constant operands, removes division by one,
rewrites a power-of-two divisor as a shift, and can rewrite unsigned modulo as
a mask. Arithmetic right shift and truncating division differ for negative
nonmultiples. The source reachability and observable result of that rewrite
need native fixtures before optimizer equivalence is claimed. Floating-point
division, compound assignment, native code generation and whole-program
execution remain outside this implementation.
