# Integer division and remainder

Reference commit: `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

`Ir.Integer_interpreter` executes raw `IC_DIV` and `IC_MOD` on internal scalar
`I64` and `U64` words. `holyc eval examples/integer-division.hc` evaluates
`(85/2)+(85%2);` and returns `43`. Public program lowering additionally applies
the verified integer source rewrites before allocating instruction identities
and sealing source ownership. `holyc run examples/division-strength-reductions.hc`
returns `42` in JIT and AOT modes through both interpreted and native execution.
Raw expression and fragment callers keep their existing default behavior.

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

These raw arithmetic faults are execution errors. Source constant signed
minimum divided or reduced modulo minus one instead reports `HCIRL0007`
during lowering, before a graph or native image is published. Discarding the
result or placing the expression in `if(0)` does not suppress that compilation
fault. A zero divisor is not folded and faults only when reached.

Whole-graph preflight first validates all opcodes,
types, flags and payloads, including unreachable blocks. A supported arithmetic
operation in a skipped block does not fault. A reached fault consumes one
instruction step and retains its block, instruction and source span. A spent
budget reports `HCIRVM0007` before the next instruction. Faults return no
partial result, including when a prior instruction filled the return latch.

The current source-expression path evaluates both operand trees of Boolean
value expressions. `0&&(1/0);` and `1||(1%0);` therefore reach their faults.
The [integer program driver](integer-programs.md) lowers conditional AND and OR
into short-circuit branches. General exception handling remains unfinished.

## Evidence and regression coverage

The division test group has six table/control tests and a 500-case property
that compares both results with independent bit-at-a-time long division.
Source tests exercise the public parser, semantic passes, lowerer, verifier,
and interpreter. Five CLI golden rules cover numeric output, unsigned JSON,
both fault codes, JSON diagnostics, exit status 1 and empty stdout on failure.

The native [integer-division fixture](../test/oracle/integer-division.json)
records 46 reproduction commands: seven definitions, 31 result checks and
eight disassemblies. It ran on 2026-09-07 in the verified final TempleOS ISO
under QEMU 9.2.0, with one CPU, 512 MiB, no networking and no persistent disk.
It records each command, output, first failing phase, and SHA-256 hashes of
the accepted-source and result captures. Selected results were repeated in
the same boot. The ISO and captures remain outside the repository.

One regression test projects 13 recorded raw arithmetic values and four
target faults into the verified integer IR interpreter. This covers all four
signed/unsigned operand pairings. It does not run the native functions in
the hosted compiler or emulate native exception delivery.

The [division-strength-reduction fixture](../test/oracle/division-strength-reductions.json)
adds 34 accepted definitions, 20 result commands and 15 disassemblies from
2026-10-02. Its 38 primary fields were each observed twice within one boot.
All 69 commands retain accepted-source and result image hashes. Machine
listings end at the first return. The source regressions compare those fields
through both compilation modes, both native status ABIs and fresh executable
images. CLI regressions compare the same fields through `ir` and `host-jit`.
The original fixture remains the arithmetic-fault phase evidence.

Additional controls check addressed and indexed updates, effects retained
after divisor elimination, following comparisons, folded global/static/default
preparation, retained and live leaves, and exact runtime and preparation
allowances. Original masks, reduced updates and canonical shifts retain their
transitive source records; copies, mutations, foreign contexts and new
post-publication reductions fail ownership checks.

## Source optimizer behavior

For signed `x = -7` and variable `y = 2`, the native fixture observed:

| Source shape | Result | Evidence |
| --- | --- | --- |
| Constant `-7/2` | `-3` | Constant-folding source path |
| Function returns `x/y` | `-3` | `IDIV` |
| Function returns `x/2` | `-4` | `SAR RAX, 1` |
| Function performs `x/=2` | `-4` | `SAR RSI, 1` |
| Function returns `x%2` | `-1` | `IDIV`, taking the remainder |
| Function performs `x%=2` | `1` | `AND RSI, 1` |

These source shapes reach the distinct rewrites at
`OptPass012.HC:403-455,838-854`. The plain modulo reduction checks unsignedness;
the compound modulo reduction does not have that guard. The fixture also
covers exact negative multiples, division by one and signed minimum.

Signedness can change across a literal-divisor rewrite. With signed `x = -7`,
`x/0x8000000000000000` emitted `SAR RAX, 63` and returned all-one bits. The
constant expression and the version taking a `U64` divisor parameter both
returned `1`. The source explanation is that the division rewrite removes
the unsigned divisor, while `OptFixupUnaryOp` at `OptLib.HC:196-225` forwards
the surviving operand's class when the constant shift is revisited.
`OptPass789A.HC:682-687` and `BackA.HC:573-600` then select the signed shift.
The source rewrite now follows that captured class forwarding and the complete
count-merging policy in [constant shifts](constant-shifts.md). An unsigned
`x/2` function emits `SHR` and agrees with raw and constant unsigned division.

Fully constant nonzero division and remainder fold with the common integer
class. A nonconstant dividend divided by a literal one-bit value becomes a
constant shift using its bit-scan position. Consecutive right shifts and
division reductions merge their complete 64-bit counts before the consumer
masks them. Unsigned plain remainder by a one-bit value becomes an AND mask;
signed plain remainder retains `IC_MOD`. Compound `/=` and `%=` instead reduce
to addressed SHR and AND updates for supported scalar destinations, including
the captured `I8` and pointer destinations. The address is evaluated once and
the update reads its destination after evaluating the RHS.

Division by one is removed before early comparison classes are selected:
`(x/1(U64))<0` returns `1` for signed `x=-7`. Other divisor reductions can
retain an earlier unsigned comparison decision even after the shift recovers
its signed dividend class: `(x/2(U64))<0` returns `0`. The optimizer keeps that
early decision separately from the surviving operation's computation class.
Public call and cast classes retain their operation-specific forwarding rules.

## Observed fault phases

Both raw operations raised TempleOS `DivZero` for a zero divisor and for
signed minimum with minus one. `Kernel/KInts.HC:150-164` routes the processor
division exception to that same HolyC exception name, so its name alone does
not distinguish zero division from quotient overflow.

Constant signed overflow raised `DivZero` while `ExePrint` compiled a function,
including a discarded expression and an expression in `if(0)`. Literal
division by zero compiled successfully and faulted when executed, including
when its value was discarded. A skipped `if(0)` division by zero returned the
following value `7` without faulting. Eager `&&` and `||` value expressions
faulted on their zero-divisor operands; the corresponding conditional forms
short-circuited and returned `7`.

The interactive checks terminate each top-level `try`/`catch` with an explicit
delimiter and completion marker. Preliminary commands without that delimiter
produced their output with later input and were repeated; those preliminary
captures are excluded from the fixture.

[Issue #584](https://github.com/frankischilling/holyc-ocaml/issues/584) implements
the raw operations. [Issue #585](https://github.com/frankischilling/holyc-ocaml/issues/585)
tracks the remaining contexts beyond this verified integer rewrite. Nonconstant
division reductions in retained initializer callees still report `HCRUN0006`;
ordinary nonconstant shifts keep that preparation boundary too. Fully folded
division and remainder prepare through the existing immediate-value path, as
shown by [the retained example](../examples/stream-division-strength-reductions.hc).
Native publication of retained `#exe` source remains `HCPP0008`. Shared
comparison-chain power-of-two division shifts and plain remainder masks retain
their raw operations; other numeric or flagged domains keep their existing
contract. This increment does not establish general optimizer
coverage, native exception delivery, loader acceptance or compiler bootstrap.
