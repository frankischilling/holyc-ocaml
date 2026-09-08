# U8 compound and prefix/postfix updates

[#629](https://github.com/frankischilling/holyc-ocaml/issues/629) connects all ten
compound assignments and four increment/decrement forms to checked byte storage.
Automatic locals, globals, statics, array elements, literal bytes and passed
references use the existing update instructions and object identities.

`holyc run --format=json examples/integer-byte-updates.hc` captures `42` and
returns I64 42 in JIT and AOT. The example updates global and static bytes,
wraps an automatic byte, retains a full compound result and mutates a copied
message through a helper's pointer. The eight original gates failed HCRUN0003
at source `7cc6762f285689053fc8ee05418a41207e5c52a8` and merge
`f5b0e8f55e042fcb04f519b8c077fe36fa59e48a` in both modes.

## Stored bytes and results

The destination address is captured once. A compound RHS finishes its effects
before the update reads the selected cell. Successful arithmetic stores its low
eight bits; a compound expression retains the full computed U64 register bits.
Prefix returns the new stored byte and postfix returns the old stored byte,
both zero-extended. An update consumes one runtime instruction after its
address and RHS instructions; increment/decrement needs no extra RHS literal.

| Update | Expression bits/value | Stored byte |
| --- | ---: | ---: |
| 255 += 1 | 256 | 0 |
| 0 -= 1 | 0xffffffffffffffff | 255 |
| 129 *= 2 | 258 | 2 |
| 42 \|= 256 | 298 | 42 |
| 42 ^= 256 | 298 | 42 |
| 1 <<= 63 | 0x8000000000000000 | 0 |
| ++255 | 0 | 0 |
| 255++ | 255 | 0 |
| --0 | 255 | 255 |
| 0-- | 0 | 255 |

The byte destination owns unsigned division, remainder and logical right shift,
even with a signed RHS. For example, byte 255 divided by the I64 bits -1 yields
zero; its remainder is 255. Shift counts use the raw word path's low six bits.
Existing I64/U64 update behavior is unchanged. Byte arithmetic result tags use
U64; conversion into an I64 function result preserves the computed bits.

Unknown reads, zero divisors, invalid pointers and object bounds retain their
existing diagnostics. An arithmetic fault performs no update store. Earlier
RHS effects and published output survive, and resource exhaustion stops before
the next instruction. References never widen into neighboring bytes or static
padding. Recursion retains each activation's cells; every execution restores
fresh global, static and literal images.

## Raw execution and native initializer proof

Ordinary `runtime-ir` execution selects the audited canonical reference path:
exact checked address and destination type, zero operation flags, no payload,
and a materialized result. Malformed flags, numeric pseudo-addresses and type
substitution fail validation before effects; missing results fail sequence
validation. This contract is distinct from native optimizer selection.

The native pipeline can choose a narrow reread for BY_VAL immediate updates,
redirect results to memory, allocate narrow automatic locals to 64-bit
registers, or widen discarded bit updates. Declaration initializers therefore
need additional proof. `integer_update_initializers.ml` checks the original
value graph and every transitive callee, using each definition's own frame and
compiler options. Failures report HCRUN0006 with the exact initializer owner
and offending source span.

Memory prefix/postfix is invariant. Memory compounds are admitted when their
full results are byte-bounded: unsigned dynamic division/remainder, AND,
OR/XOR with a bounded RHS, multiplication by zero/one and addition/subtraction
by zero. Existing shift and constant-divisor guards still apply. The proof
does not infer a mutable old value from its original initializer: RHS calls
and aliases can change it before the read.

An exact terminal U8 declaration leaf erases a compound's full-versus-narrow
result difference. Thus `U8 H=(G+=10)` can be admitted even when an I64 sink
would be rejected. The exemption does not pass through arbitrary arithmetic,
assignment results or callees. A source-discarded memory update can similarly
preserve storage effects without exposing its register result.

Bit updates need a separate check. Native discarded `|=256`, `^=256`, or
`&=~256` can address a bit outside the byte. Apparent source uses can disappear
under identity folding, as in `(A[0]&=~256)-0;`. Hazardous bit operations need
positive retained-result evidence: the exact declaration sink, return,
protected pushed argument, or a supported bit-preserving cast leading to one.
The initializer graph's final IC_END_EXP is a synthetic wrapper and never
proves source discard. PUSH_RES counts as an implicit argument use even when
IC_CALL has no operand list.

Operand agreement matters independently of result range. An automatic
`U8 d=554` may retain 554 in a native register while hosted memory reads 42;
the quotients in `G/=d` can differ even though both fit a byte. Every byte read
in the initializer and its callees must therefore come from proven memory or
an ordinary register candidate whose every direct store has a bounded RHS
and which has no direct updates. A conservative fixed point admits dependent
locals; unknown cycles remain rejected. It does not track mutable old values.

Explicit hardware byte registers cannot use that range exception, and their
escaping addresses cannot be laundered through pointer slots or arguments.
Original reg/noreg selection is retained in frame locations; static locals are
forced to disabled allocation. No_reg_var and ordinary arrays prove memory
only after explicit register requests are excluded. Address/dereference
cancellation alone proves no escape. Genuine stored/passed ordinary byte
addresses keep the native object in memory; actual pointer-slot loads retain
indirect access.

## Maintained limits and coverage

| Resource | Exact successful fixture limit |
| --- | ---: |
| Runtime instructions | 79 |
| Initializer preparation work | 10 |
| Persistent bytes | 12 |
| Active frame bytes | 40 |
| Call depth | 2 |
| Ordinary literal bytes | 3 |
| Output bytes | 2 |
| Output work | 8 |

The persistent charge is Total's one byte, Text's three bytes and Calls' padded
eight-byte static request. Result's frame needs 16 bytes; Print's three ABI
slots, including its hidden count, add 24. The format literal consumes three
bytes. Preparation costs three instructions for Total, three copied message
bytes and four instructions for Calls.

All exact limits pass together. Each one-below limit has a CLI regression;
only the runtime-step failure occurs after captured `3432` in this fixture.
CLI v1/v2 reporting and existing dump schemas are unchanged. Dumps retain
original update opcodes and exact public U8 destination types.

The 43 `source byte updates` groups cover the eight gates, all opcodes across
storage owners, full/stored results, neighbors, immediate/runtime RHS values,
alias effects, argument order, loops, recursion, fresh images, faults,
canonical preflight and native initializer proof. Review regressions include
identity-folded bit updates, implicit pushed arguments, explicit byte register
escapes, wide local divisors, helper returns and changed control flow. The local
suite passed 1,968 tests in 40.986 seconds plus CLI checks. Formatting,
generated/build/install checks, 82 checksums and 11 provenance scenarios passed.
Exact lexer JSON retains 528/528 with no errors; parser JSON and normalized
text retain 25 standalone and 126 prelude successes. Protected integration
evidence is recorded in issue #629.

## Pinned source evidence

Reference: `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

- `Compiler/PrsExp.HC:98-118,201-208,235-241` retains update ICs and removes
  the destination value load before parsing the compound RHS.
- `Compiler/BackA.HC:285-423,442-658` supplies register computation, width
  stores, BY_VAL immediate rereads, unsigned byte arithmetic and raw shifts.
- `Compiler/BackB.HC:304-342,345-380` distinguishes prefix/postfix result order;
  `BackLib.HC:285-296,509-535` encodes widths and zero-extends byte loads.
- `Compiler/BackC.HC:539-600` selects discarded immediate bit operations;
  `OptPass012.HC:630-637,828-853` folds identities and power-of-two compounds.
- `Compiler/OptPass3.HC:29-35,187-198,238-243,524-610` covers explicit
  registers, escape balance, scratch registers and narrow automatic allocation.
- `Compiler/OptPass4.HC:243-275,348-434,541-545` selects address/result modes,
  preserves returns and protects pushed results from discard propagation.
- `Compiler/PrsVar.HC:492-494` forces static locals to memory;
  `CInit.HC:54-59,88-97` retains update preparation barriers.

This is pinned-source and hosted execution evidence, without a new native
TempleOS capture. [U8 numeric signatures](integer-byte-signatures.md) now add
parameter entry and full-bit returns. Other narrow and aggregate
storage, general pointer operations, full formatting/runtime linking, stateful
compilation/#exe, optimizer parity, native backends, BIN/loader acceptance and
bootstrap remain part of the full compiler mission.
