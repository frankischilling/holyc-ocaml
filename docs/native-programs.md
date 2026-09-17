# Native integer programs

`holyc run --target=host-jit` compiles closed integer statements and structured
control flow with the project's OCaml x86-64 backend, then executes the checked
image on Windows or Linux x86-64. It uses the same parsed and typed source roots
and verified block graph as the integer program lowerer. It does not execute the
entry through the interpreter or replace its result with a constant.

```text
opam exec -- dune exec --root . -- bin/holyc.exe run --target=host-jit --format=json examples/native-integer-program.hc
opam exec -- dune exec --root . -- bin/holyc.exe run --target=host-jit --mode=aot --step-limit=26 --format=json examples/native-integer-program.hc
```

The fixture returns I64 42 in 26 reached IR instructions. It combines nested
`if`, conditional `&&` and `||`, `for`, `do`/`while`, `break`, comparison and
guarded division. Its skipped bodies, loop update and short-circuited operands
contain zero divisors. A 26-step budget succeeds; 25 stops before stream end.
Both preprocessing modes execute the resulting host image immediately. Selecting
AOT preprocessing does not produce an object, executable or TempleOS BIN file.

## Source and IR boundary

Accepted statements are closed integer expressions, empty statements, blocks,
comma statement sequences, `if`/`else`, `while`, `do`/`while`, `for` and `break`.
The word operations are the same checked internal I64/U64 operations as
[native expressions](native-expressions.md): wrapping arithmetic, bitwise and
eager logical values, comparisons, masked shifts, guarded division/remainder
and payload-zero internal word views.

The source driver parses without command or stream-execution callbacks. An
iterative source gate rejects declarations, functions, calls, identifier
storage, assignments, updates, indexing, pointer operations, implicit output
and unsupported statements before semantic preparation. It reports the first
closed-domain violation while retaining parser diagnostics. A directive needing
`#exe` execution receives the parser's explicit missing-capability diagnostic;
it cannot route the ordinary program into the interpreter. Compilation also
checks that the resulting unit has no functions, storage, calls, initializers
or preparation work before invoking the backend.

The backend preflights every block in source order, including unreachable
instructions. It admits the word producers plus canonical flagged `IC_END_EXP`,
`IC_JMP`, `IC_BR_ZERO`, `IC_BR_NOT_ZERO` and `IC_END`. Each word is defined and
used within one block. Control targets must match the checked graph, and an
untaken conditional branch follows the physically next source block. Sparse
block or instruction IDs never determine allocation sizes.

Condition lowering distinguishes a boolean used to choose a branch from an
ordinary eager logical value. `if(0 && (1/0)) ;` skips division, whereas
`0 && (1/0);` reaches a zero-divisor fault. Preflight still checks the syntax,
types, flags and limits of skipped code. It does not execute or constant-fold
an unreachable arithmetic fault.

The final value comes from the last reached top-level `IC_END_EXP`. Conditions
and control transfers do not replace it, but a `for` initializer or update is
an expression statement and can do so. Consequently the earlier
`examples/integer-control-flow.hc` fixture returns I64 **0**, in 23 instructions,
because its final `for(0;0;1/0)` initializer is reached. The dedicated native
fixture above ends with a reached 42-valued expression instead.

## Metering and frame lifetime

Generated code reserves R10 for the remaining instruction budget and R11 for a
private host context. Before every IR instruction it tests the remaining count,
branches to that instruction's budget-fault site if zero, and otherwise
decrements the count. This includes jumps, conditional branches, expression
disposal, stream end and arithmetic instructions that subsequently fault.
Compiler-generated moves, guards and housekeeping branches do not create extra
IR steps. Empty-block transfers cost no IR steps; every cyclic control-flow path
contains a charged instruction.

`6*7;` therefore succeeds with five steps and fails before `IC_END` with four.
An empty source stream charges its sole `IC_END`, succeeds with one step and
has no final value. `while(1);` stops at the supplied positive budget. Reaching
stream end on the last allowed instruction succeeds.

`X86_64_word_codegen` shares word validation, register/spill allocation, exact
branch resolution and frame/unwind generation between expressions and programs.
Programs have five value registers, RAX/RCX/RDX/R8/R9; the reported register peak
also counts R10, R11 and any fixed or temporary registers. Each block has fresh
value ownership and reusable spill slots. One frame, sized to the largest block
requirement, remains allocated for the complete invocation. Frame sizes include
alignment padding and are bounded by 4,088 bytes. Every exit joins the same
step-counting, stack-restoring epilogue.

The hosted entry is a checked private convention. It is not the complete HolyC
function ABI, and these spill slots do not authorize source-visible memory or
function calls. The expression API keeps its separate return convention and
previous byte sequences, including ABI-neutral images that need no status.

## API and limits

`Holyc_lib.Native_program.compile` accepts a session, preprocessing configuration
and source. It returns a checked opaque `X86_64_program.t` and retained
diagnostics without executable-memory allocation. `Native_program.evaluate`
adds a positive `max_steps` and returns an opaque report. `outcome` exposes a
successful image/execution/platform or diagnostics; `native_outcome` and
`executed_steps` retain the typed native fault and actual progress after checked
arithmetic or budget faults. Host bridge and cleanup failures expose no trusted
native status or progress. The API's `image` getter can inspect a compiled image
after failure.

Low-level callers compile an `Ir_x87_stack.t` with `X86_64_program.compile` and
explicitly call `Native_program_execution.execute ~max_steps`. Code and unwind
getters return fresh copies. Neither image interface exposes a constructor for
arbitrary bytes. An optional compile-time `status_abi` selects Windows x64 or
System V code generation for inspection; execution rejects a foreign ABI before
allocating executable memory.

| Bound | Default | Allowed configuration |
| --- | --- | --- |
| `--step-limit` / `max_steps` | 100,000 in the CLI | Positive host integer |
| `--ir-instruction-limit` / `max_ir_instructions` | 4,096 | 1 through 100,000 |
| `--code-byte-limit` / `max_code_bytes` | 65,536 | 1 through 16 MiB |
| `--stack-byte-limit` / `max_stack_bytes` | 4,088 | 0 through 4,088; zero disables spills |
| `--block-limit` / `max_blocks` | 4,096 | 1 through 100,000 |

The compiler counts all IR and blocks before allocating its maps, preflights the
whole graph, and checks the complete planned image before allocating encoded
bytes. Prologue, meter, guard, fault-block and epilogue bytes all count toward
the code quota. Existing `run` storage, call-depth, output and preparation
options retain their configuration validation; this closed gate consumes none
of those resources. The native frame limit controls private compiler spills.

## Faults and reporting

Programs use the existing integer-program v2 report with `target=host-jit` and
`arithmetic=runtime-native`. Success retains exact executed steps, stream-end
termination and the full-width typed value as decimal and hexadecimal strings.
The `native` object records the platform, requested compilation limits and
successful image metrics. Failed JSON reports retain diagnostics and, for checked
arithmetic or budget faults, actual native steps. They expose no final value or
successful image; host bridge and cleanup failures expose no trusted step count.
The private context is never serialized. The existing IR v2 renderer is
unchanged; report v1 remains an IR-only compatibility format and explicitly
rejects the host-JIT target before source entry.

Reached zero divisors use `HCIRVM0009`, signed `INT64_MIN/-1` division and
remainder overflow use `HCIRVM0010`, and exhausted budgets use `HCIRVM0007`.
Those codes describe the shared program semantics even though generated code
detected the fault. Diagnostics retain the original source span, block ID,
instruction ID and executed-step count. Unsupported hosts use `HCNATIVE0001`;
foreign ABIs, invalid status or OS mapping/cleanup failures use `HCNATIVE0002`.
The existing native-expression diagnostic codes and JSON v1 schema are unchanged.

The private C context contains six full-width words: kind, fault site, budget,
executed steps, last-expression site and value bits. Sites are dense identities
owned by the image. A last-expression site must identify a checked `IC_END_EXP`;
OCaml derives the result's I64/U64 type from that metadata rather than trusting
an arbitrary native type tag. Status decoding checks kind/site consistency,
step bounds, arithmetic operation and signedness, and final-value presence.

The bridge shares the expression executor's writable-to-executable mapping and
Windows unwind lifetime. A fresh context is used for every call. Generated
guards catch zero and signed-overflow cases before hardware division. After
return, the bridge removes unwind registration and releases the mapping before
boxing any context values. Failed Windows unwind removal retains its registered
mapping rather than leaving a dangling OS reference. The OCaml runtime lock
remains held while the image runs.

This is an in-process executor. The step budget bounds checked IR loops; it is
not a wall-clock deadline, isolation boundary or recovery from arbitrary machine
faults. Ordinary `run --target=ir`, `eval`, preprocessing and `dune runtest` do
not select native execution. The explicit `@native-tests` target exercises
generated code, CLI behavior and unwind integration on Windows and Linux.

## Source evidence and remaining work

Reference: `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/PrsStmt.HC:459-565` establishes structured conditions, loop order and
break targets. `Compiler/OptLib.HC:229-484` supplies conditional NOT/AND/OR
rewrites. `Compiler/OptPass789A.HC:158-163,267-284` supplies test-and-branch and
relative-jump consumers. The checked opcode database supplies the qword moves,
decrement, tests and branches; existing word-operation references are recorded
in [native expressions](native-expressions.md). The private context, per-IR
budget and host completion wrapper are hosted execution policy, not additional
TempleOS language rules.

This gate does not complete general native source execution. Native storage,
function frames and calls, the complete HolyC ABI, floating operations, optimizer
parity, the integrated assembler, object/BIN writing, loader acceptance and
bootstrap remain required compiler work. Issues #574, #585 and #593 continue
to track their distinct shift, division-optimization and comparison-reduction
requirements.
