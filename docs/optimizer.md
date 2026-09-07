# Optimizer

Every TempleOS source claim on this page refers to commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

## Current implementation boundary

The optimizer currently consists of one source-audited pass, `Holyc_lib.Ir_integer_unary_folding`. It folds a checked integer immediate through bitwise complement, logical not, or unary minus. The public entry point is `fold : Ir.X87_stack.t -> (t, error list) result`, so an unchecked instruction sequence or block graph cannot enter the pass.

Compatibility is exact for the accepted subset, not approximate. For each accepted type and opcode pair, the pass applies the pinned 64-bit operation and preserves the checked facts listed below. Inputs outside that subset are left unchanged or fail at an earlier verifier boundary. This classification does not apply to the TempleOS optimizer as a whole, which is not implemented.

## Pinned source contract

The pass combines these source facts:

- `Compiler/OptPass012.HC:1-29,153-192` supplies the early traversal and the immediate rewrites for `IC_COM`, integer `IC_NOT`, and integer `IC_UNARY_MINUS`.

- `Compiler/OptLib.HC:17-23,196-225` supplies unary constant eligibility, result-class forwarding, flag transfer in the source compiler, and removal of the consumed producer.

- `Compiler/PrsExp.HC:608-616` maps `~`, `!`, and unary `-` to those three opcodes.

- `Compiler/CInit.HC:27,50-52` defines `IC_IMM_I64` as a zero-input producer and the unary operations as one-input producers.

- `Compiler/OptPass789A.HC:449-459` selects 64-bit complement, logical-not, and negation behavior in the backend.

TempleOS transfers flags during these rewrites. This first checked pass deliberately accepts only zero-flag producers and consumers. Copying an arbitrary flag combination before all of its checked IR and x87 effects are modeled would be a different compatibility claim.

## Preconditions

The input wrapper proves that `Ir.Instruction_sequence`, `Ir.Block_graph`, and `Ir.X87_stack` checks have already succeeded. A rewrite has further local preconditions:

- The producer is `IC_IMM_I64`, has no operands, defines one value, has an exact integer payload, and carries zero flags.

- The producer target is a zero-depth internal scalar `I64` or `U64`. Public spellings and all other widths are different types for this pass.

- The producer and consumer are in the same block, the consumer appears later in source order, and the produced value occurs exactly once across the graph.

- The consumer has one operand, one result, no payload, zero flags, and the exact target type shown below.

`Ir.Block_graph.create` checks each block's value sequence independently. A cross-block value operand therefore reports `HCIR0009` before an `Ir.X87_stack.t` can be constructed and never reaches this pass.

## Eligibility and type matrix

All entries in this table use exact `int64` word operations. `Int64.neg` wraps modulo 2^64, including `Int64.min_int`.

| Consumer | Producer target | Consumer target | Folded payload |
| --- | --- | --- | --- |
| `IC_COM` | internal `I64` or `U64` | internal `I64` | `Int64.lognot bits` |
| `IC_NOT` | internal `I64` | internal `I64` | `1` when `bits = 0`, otherwise `0` |
| `IC_NOT` | internal `U64` | internal `U64` | `1` when `bits = 0`, otherwise `0` |
| `IC_UNARY_MINUS` | internal `I64` or `U64` | internal `I64` | `Int64.neg bits` |

The pass does not infer or repair a target type. A producer and consumer that do not match one of these rows remain unchanged.

Canonical checked expression lowering now emits the unsigned unary-minus row from source. A high-bit integer or character target word is an internal-`U64` `IC_IMM_I64`; unary minus over that exact zero-depth operand is internal `I64`. The zero-flag pair is eligible without repair, and the `Int64.min_int` integration case folds to the same bits with an internal-`I64` result. Public `U64`, narrower integers, pointers, and same-typed internal `U64` unary-minus consumers do not enter this row.

## Rewrite and fixed point

An accepted pair becomes one `IC_IMM_I64`. The consumed producer is removed. The former unary instruction remains in its source position and keeps its instruction ID, result ID, target type, and source span. Its operand list becomes empty, its payload becomes the folded bits, and its flags remain zero. Block order, control-flow successors, and terminators are not rescheduled.

The scan visits blocks and instructions in source order. A folded unary result is immediately indexed as the current constant for its value, so it can feed the next eligible unary operation in the same traversal. A chain therefore reaches its fixed point without a separate convergence loop. Running the pass on its own result produces the same graph and a trace with zero rewrites.

The opaque result includes a source-ordered `holyc-ir-integer-unary-folding-v1` trace. Each row records the block, removed instruction and value, retained instruction and value, original opcode, input bits, and output bits. For example, the focused complement case fixes this row byte for byte:

```text
^b4 remove=!i10:%v100 retain=!i11:%v101 opcode=IC_COM input=0x0000000000000001 output=0xfffffffffffffffe
```

## Postconditions and failure boundary

After planning all rewrites, the pass reconstructs every block through `Ir.Block_graph.create` with the original entry block. It then reruns `Ir.X87_stack.verify` over the complete rebuilt graph. Only a result that passes both checks is exposed through the opaque `t` and its `x87` accessor.

A reconstruction or x87 failure returns no partial result. Each error identifies `Graph_rebuild` or `X87_reverification` and retains the child diagnostic code, message, block, instruction, and span when present. Successful output has the same entry block, block order, control-flow edges, and terminators as its input. Its only structural changes are the recorded unary replacements and removal of their consumed immediate producers.

## Before, after, and equivalence evidence

The focused tests construct the checked graph before the pass and inspect the rebuilt graph after it. They check the retained instruction, result, type, and operator span; the removed producer; the exact folded payload; unchanged block order and successors; the terminator; and the versioned rewrite trace. Repeated runs compare both graph dumps and trace bytes. Negative cases compare the complete graph dump before and after to prove that excluded inputs are unchanged.

A 500-case QCheck property supplies arbitrary 64-bit words and selects each of the three unary operations. It executes the original and folded verified graphs through `Ir.Integer_interpreter`, compares their returned 64-bit words, and checks both results against the direct `Int64.lognot`, zero-truth, or `Int64.neg` formula. This is bounded interpreter-equivalence evidence for the accepted unary word operations only. It does not cover unsupported opcodes, TempleOS runtime state, or optimizer behavior outside this pass.

Run the focused group with:

```text
dune exec test/test_main.exe -- test "IR integer unary folding"
```

## Exclusions and unavailable controls

The pass leaves shared values, flag-bearing candidates, public integer forms, other integer widths, `F64`, malformed or missing payloads, and unary instructions with a payload unchanged. It does not fold binary operations, propagate values between blocks, perform general dead-code elimination, reschedule instructions, persist IR, or emit machine code.

The compiler driver does not schedule this pass. There is no `holyc` optimizer command, compiler option, or optimization-level control that invokes it. Callers must explicitly provide an `Ir.X87_stack.t` to the library function. The bounded integer interpreter is a separate library entry point and never schedules the pass; callers may execute either the original verified graph or the verified result returned by `fold`. Integration with a pass pipeline, compiler controls, general interpretation, and backend emission remains unavailable.
