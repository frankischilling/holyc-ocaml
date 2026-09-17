# Native integer programs

`holyc run --target=host-jit` compiles integer statements, structured control
flow and fixed direct scalar integer functions and U0 procedures with the project's OCaml x86-64
backend, then executes the checked image on Windows or Linux x86-64. It uses the
integer program lowerer's original source roots, function bodies, frame layouts
and checked call context. It does not execute the entry through the interpreter
or replace its result with a constant.

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

The function fixture uses a caller local across a nested generated call:

```text
opam exec -- dune exec --root . -- bin/holyc.exe run --target=host-jit --format=json examples/native-integer-functions.hc
opam exec -- dune exec --root . -- bin/holyc.exe run --target=host-jit --mode=aot --format=json examples/integer-function.hc
```

Both return I64 42. The original `integer-function.hc` source, `Add(20,22)`,
requires 29 reached IR instructions: 29 succeeds and 28 exhausts the budget.

## Source and IR boundary

Accepted statements are integer expressions, empty statements, blocks,
comma statement sequences, `if`/`else`, `while`, `do`/`while`, `for` and `break`.
Source-defined functions add named fixed I8/U8/I16/U16/I32/U32/I64/U64 parameters,
automatic scalar integer declarations, local assignment and updates, and
value-return statements. U0 procedures also admit bare returns and fallthrough.
Function-local language labels and direct gotos use the same checked block
lowering as the interpreter; [goto execution](integer-goto.md) records their
source identity, empty-block fallthrough and initialization behavior. Switch,
assembly-label and protected-region control flow remain outside this gate.
Direct calls bind to checked definitions in the same compilation unit, including
self-recursion. Function locals persist across the function's control transfers
and are separate in every recursive invocation.
Every reachable terminal path in a word-returning function must supply a word.
Bare returns and fallthrough without a value reject for those functions.
[Native scalar functions](native-scalars.md) details narrow storage, full-width
register results and the separate checked U0 completion path. Prototypes are
also excluded, so forward or mutual calls requiring a prototype are not admitted.
The full-width machine word operations use the retained scalar computation
classes described in [native scalar functions](native-scalars.md). They extend
the operation families from [native expressions](native-expressions.md): wrapping arithmetic, bitwise and
eager logical values, comparisons, masked shifts, guarded division/remainder
and payload-zero internal word views.

The source driver parses without a command or stream executor. Original
declaration callbacks prepare bounded scalar defaults in both modes; an
iterative source gate rejects globals, statics, prototypes/externs,
explicit register/declaration modifiers, non-integer parameters or locals,
arrays, source pointer operations, indirect calls, implicit output and unsupported
statements. Arrays, globals and aggregates reject before their preparation.
Entry statements cannot declare storage.
It reports the first source-domain violation while retaining parser diagnostics.
A directive needing
`#exe` execution receives the parser's explicit missing-capability diagnostic;
it cannot route the ordinary program into the interpreter. Compilation also
checks that the resulting unit has no global/static initialization or storage
preparation work before invoking the backend. Every admitted default prepares
once, even when all arguments are supplied or its function is unused. Calls reuse
the saved value. Default-bearing definitions must precede executable top-level
statements; effectful/interleaved defaults and broader default types remain
unsupported. [Native defaults](native-defaults.md) defines the exact expression,
source-order, ownership and resource boundary.

The backend preflights the complete bundle, including unreachable instructions
and unused function bodies. The exact `Runtime_call_context` must match the entry
and definition bodies, including their original empty initialization context,
and every body must retain its exact associated frame. A foreign context with
the same empty contents cannot replace that original authority.
Zero storage bytes and zero prepared storage steps do not prove that a bundle
has no saved parameter defaults. Callable preflight checks the original argument
producer's preparation metadata and the selected and definition-owned headers,
including default-bearing functions whose arguments are supplied explicitly or
whose bodies are unused.
The optional native parameter-default certificate additionally binds each saved
value to its original completed preparation and this exact compiled bundle.
Without that certificate, low-level callable compilation still rejects defaults.
Besides the word and control operations, callable images consume checked frame
addresses, scalar loads/stores/updates, return operations, and the original
`IC_CALL_START`/argument/`IC_CALL`/cleanup/`IC_CALL_END` sequence. Temporary values
are defined and used within one block. Control targets must match the checked graph, and an
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
fixture above ends with a reached 42-valued expression instead. Function-internal
expression disposal and return capture never overwrite the entry's final-value
latch. A reached top-level U0 call discard clears that latch and reports no final
numeric value; it does not substitute zero or retain the preceding word.

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
value ownership and reusable spill slots. Closed programs keep their original
single frame, sized to the largest block, with a maximum of 4,088 bytes.

Callable owners save RBP and keep RSP fixed throughout their bodies. Parameters
occupy the original positive slots starting at `RBP+16`; automatic locals use
their checked negative frame displacements and exact declared widths. Narrow
loads sign- or zero-extend and stores touch only the object's bytes. Private initialization flags, spills,
pending arguments and a shared outgoing argument area occupy disjoint storage.
The allocation is a multiple of 16 bytes and at most 4,080 bytes; the return
address and saved RBP are counted separately by the active-stack limit. An owner
that needs no allocation still saves and restores RBP.

Argument expressions execute in the lowerer's right-to-left order. Each open
call has separate staging, so an inner call cannot overwrite an already evaluated
outer argument. Immediately before a real rel32 `CALL`, values are copied to the
outgoing area in fixed parameter order and live caller registers are spilled.
Word-returning callees preserve the full register result through RAX; U0 calls
have a separate nonnumeric completion and no result staging. Cleanup IR remains checked and metered,
but the private fixed-RSP convention needs no machine argument-pop instruction.
Every function uses plain `RET`; this is not the full TempleOS call ABI.

Each automatic local has a fresh hidden initialization flag. Stores mark it;
loads and read-modify-write operations test it before reading storage. An
uninitialized read produces the existing hosted integer diagnostic rather than
reading an arbitrary host-stack word. This is the interpreter's checked execution
policy, not a claim about TempleOS behavior for undefined local values.

Before a generated call, separate checks reserve one call-depth unit, its checked
parameter/local bytes, and its physical stack footprint. Entry consumes no named
call or semantic frame units, but its physical footprint is checked before host
entry. A fault returns through each actual generated epilogue; callers restore
all three reservations and propagate it without charging cleanup/end instructions
that were not reached. The final native bridge checks the restored counters.
The physical allowance is independently capped at 65,536 bytes so recursion with
zero semantic frame bytes cannot evade stack bounds.

The expression API keeps its separate return convention and previous byte
sequences, including ABI-neutral images that need no status.

## API and limits

`Holyc_lib.Native_program.compile` accepts a session, preprocessing configuration
and source. It returns a checked opaque `X86_64_program.t` and retained
diagnostics without executable-memory allocation. `Native_program.evaluate`
adds a positive `max_steps` and returns an opaque report. `outcome` exposes a
successful image/execution/platform or diagnostics; `native_outcome` and
`executed_steps` retain the typed native fault and actual progress after checked
arithmetic, budget, storage or call-resource faults. Host bridge and cleanup failures expose no trusted
native status or progress. The API's `image` getter can inspect a compiled image
after failure. `preparation_steps` and `default_bytes` independently retain reached
declaration-preparation work and completed scalar payload bytes, including after
later source, compilation, host or native failures.

Low-level callers compile an `Ir_x87_stack.t` with `X86_64_program.compile`, or
the exact entry/function/call bundle with `X86_64_program.compile_callable`, and
explicitly call `Native_program_execution.execute ~max_steps`. Code and unwind
getters return fresh copies. `windows_unwind_functions` exposes checked code
ranges and per-owner unwind bytes; `function_count` counts named functions and
`entry_stack_bytes` includes the entry's return address and saved RBP when present.
Neither image interface exposes a constructor for
arbitrary bytes. An optional compile-time `status_abi` selects Windows x64 or
System V code generation for inspection; execution rejects a foreign ABI before
allocating executable memory.

| Bound | Default | Allowed configuration |
| --- | --- | --- |
| `--step-limit` / `max_steps` | 100,000 in the CLI | Positive host integer |
| `--initializer-step-limit` / `max_initializer_steps` | 100,000 | Positive declaration-preparation work quota |
| `--default-byte-limit` / `max_default_bytes` | 65,536 | Positive saved scalar payload quota; eight bytes per prepared default |
| `--ir-instruction-limit` / `max_ir_instructions` | 4,096 | 1 through 100,000 |
| `--code-byte-limit` / `max_code_bytes` | 65,536 | 1 through 16 MiB |
| `--stack-byte-limit` / `max_stack_bytes` | 4,088 | 0 through 4,088 per generated owner; callable allocations are rounded to 16 bytes and capped at 4,080 |
| `--block-limit` / `max_blocks` | 4,096 | 1 through 100,000 |
| `--frame-byte-limit` / `max_frame_bytes` | 1,048,576 | Positive simultaneous named-function parameter/local bytes |
| `--call-depth-limit` / `max_call_depth` | 128 | Positive simultaneously active named calls |
| `--active-stack-byte-limit` / `max_active_stack_bytes` | 65,536 | 1 through 65,536; physical bytes for entry and all active generated calls |

The compiler counts all IR and blocks before allocating its maps, preflights the
whole bundle, and checks the complete planned image before allocating encoded
bytes. Prologue, meter, guard, fault-block and epilogue bytes all count toward
the code quota. Existing `run` global, literal and output options retain their
configuration validation; this gate consumes none of those resources. Default
preparation has independent work and saved-payload bounds. The semantic
live-frame limit counts the checked local frame plus
eight bytes per fixed argument, excluding compiler-private storage. The physical
limit counts every allocated private byte, return address and saved frame pointer.
Setting it below `entry_stack_bytes` rejects before native entry.

## Faults and reporting

Programs use the existing integer-program v2 report with `target=host-jit` and
`arithmetic=runtime-native`. Success retains exact executed steps, stream-end
termination and the full-width typed value as decimal and hexadecimal strings.
The `native` object records the platform, requested compilation/live-stack limits
and successful image metrics, including named-function count and entry stack
bytes. Failed JSON reports retain diagnostics and, for checked arithmetic, budget,
storage or call-resource faults, actual native steps. They expose no final value or
successful image; host bridge and cleanup failures expose no trusted step count.
The private context is never serialized. The existing IR v2 renderer is
unchanged; report v1 remains an IR-only compatibility format and explicitly
rejects the host-JIT target before source entry.

`compiled_initializer_steps` reports actual reached default-preparation work;
`prepared_default_bytes` reports successfully prepared scalar payload bytes.
They remain available after later failures and do not contribute to native
`executed_steps`. A preparation failure has no native outcome or native step
count. The requested saved-payload bound is `native.limits.default_bytes`.

Reached zero divisors use `HCIRVM0009`, signed `INT64_MIN/-1` division and
remainder overflow use `HCIRVM0010`, and exhausted budgets use `HCIRVM0007`.
Uninitialized automatic reads use `HCIRVM0012`, exhausted simultaneous semantic
frames use `HCIRVM0011`, call depth uses `HCIRVM0015`, and physical native stack
exhaustion at a call uses `HCNATIVE0006`.
Those codes describe the shared program semantics even though generated code
detected the fault. Diagnostics retain the original source span, block ID,
instruction ID and executed-step count. Named-function sites additionally retain
the original function ID and name, so repeated local block/instruction IDs cannot
identify another owner. Unsupported hosts use `HCNATIVE0001`;
foreign ABIs, invalid status or OS mapping/cleanup failures use `HCNATIVE0002`.
The existing native-expression diagnostic codes and JSON v1 schema are unchanged.

Closed images retain the six-word private context: kind, fault site, budget,
executed steps, last-expression site and value bits. Callable images add remaining
semantic frame bytes, call depth and physical stack bytes at offsets 48, 56 and
64. The C bridge verifies that all three return to their supplied initial values,
with the root footprint deducted from the physical allowance. Sites are dense identities
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
remains held while the image runs. Callable Windows images register one sorted
function table covering entry and every generated function, with separate unwind
records for their actual fixed-RSP prologues. The table and records share the
mapping's lifetime. Each allocation stays below one page; larger/probed frames
remain outside this gate.

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
in [native expressions](native-expressions.md). `PrsStmt.HC:114-170` establishes
fixed parameter offsets and function boundaries, while `PrsExp.HC:438-586`
establishes argument append order, direct call selection and cleanup/end metadata.
`PrsVar.HC:629-657` prepares defaults while parsing each parameter;
`PrsExp.HC:455-468` consumes their saved values at omitted arguments.
The private context, per-IR
budget and host completion wrapper are hosted execution policy, not additional
TempleOS language rules.

This gate does not complete general native source execution. Effectful defaults,
interleaved source execution, owned string/`lastclass` defaults,
global/static storage, pointer memory, arrays, indirect calls, variadics,
explicit register and function flags, the complete HolyC ABI, floating operations,
runtime output and native `#exe` remain required. Optimizer parity, the integrated
assembler, object/BIN writing, loader acceptance and bootstrap retain their own
gates. Issues #574, #585 and #593 continue
to track their distinct shift, division-optimization and comparison-reduction
requirements.
