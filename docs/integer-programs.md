# Integer programs in the IR interpreter

[U0 function calls](integer-u0.md) execute ordinary procedures, early bare
returns and fallthrough through the same checked call protocol. A discarded U0
call reports no final word; it preserves preceding storage effects and resumes
the caller. Numeric returns remain I64/U64.

[Owned string literals](integer-strings.md) use one mutable byte object per
source site and execution image. They compose with U8 pointer initialization,
assignment, indexing and fixed calls; `--literal-byte-limit` bounds their
payloads plus terminators separately from frame/global storage.

[U8 byte storage](integer-bytes.md) extends the same checked program context
with automatic byte scalars and arrays, plain assignment and exact U8* aliases.
Stored bytes narrow independently from assignment results, and frame limits
use checked allocation sizes. The linked guide records the fixture and its
verification status; I64/U64 numeric parameters and returns remain unchanged.

[Automatic I64/U64 arrays](integer-arrays.md) execute indexed loads, assignments,
updates and element aliases through this pipeline. The caller-element fixture
returns 42 in 51 runtime steps, zero preparation, 24 active frame bytes and depth
two in both modes. Grouping and whole-object bounds follow the documented rules.

[Scalar pointer aliases](integer-pointers.md) connect one-level I64/U64 pointer
locals and fixed parameters to existing scalar local, global and static storage.
The caller-writeback fixture returns 42 in 43 steps under exact frame/depth limits.

Reference commit: `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

`holyc run --target=ir FILE` executes checked integer functions and a batch of top-level statements.
The file needs no `main` function. `holyc dump-ir --program FILE` exposes the
verified graph used by that command. Both commands accept the existing include,
definition, deterministic predefined-value and JIT/AOT preprocessing options.

```text
opam exec -- dune exec bin/holyc.exe -- run --target=ir examples/integer-control-flow.hc
opam exec -- dune exec bin/holyc.exe -- dump-ir --program examples/integer-control-flow.hc
```

The accepted statements are ordinary integer expressions, empty statements,
blocks, comma statement sequences, `if`/`else`, `while`, `do`/`while`, `for`,
and `break`, plus scalar function-local declarations and returns. Functions use
checked I64/U64 parameters, the automatic storage described above and direct
call expressions. See
[integer source functions](integer-functions.md) for the original Add fixture,
call shapes, argument order and storage limits. The report retains the last
reached top-level expression value; it has no implemented Print operation.
`holyc eval` continues to return the value of exactly one expression statement.

[Scalar static locals](integer-statics.md) retain persistent function-owned
words, supported constant initial images and definition-time faults. They share
the global byte budget and use no invocation-frame slots. [Nonconstant static
initializers](integer-static-initializers.md) execute at declaration positions
with checked JIT publication and AOT load phases.

Ordinary scalar I64/U64 code-heap globals also execute through
this path. [Declaration initializers](integer-global-initializers.md) retain
constant initial images and source-ordered compile/load regions with a separate
positive `--initializer-step-limit`. Top-level and function expressions
share their storage across calls and loops. The [global accumulator](integer-globals.md)
returns 42 in 50 instructions in both modes and introduces `--global-byte-limit`.

[Scalar update expressions](integer-updates.md) include all compound assignments
and prefix/postfix increment/decrement for I64/U64 globals and checked
I64/U64 frame slots. Updates retain the original IC and destination class, read storage
after RHS effects, and return the old postfix or new prefix/compound word.

## Source behavior and graph construction

`Compiler/PrsStmt.HC:459-565` establishes the condition tests, loop backedges,
and the order of `for` initialization, condition, body and update. A `break`
targets the nearest loop body. The initializer and update of a `for` statement
have no inherited break target, matching the separate `PrsStmt` calls there.
Every dynamic loop visit reevaluates its expressions.

`Compiler/OptLib.HC:229-484` rewrites conditional `!`, `&&` and `||` into
branches. The new lowerer recursively follows those forms. Parentheses and
unary plus remain transparent, as in `PrsExp.HC:728-759`. Conditional XOR and
logical operations inside an ordinary value expression evaluate both operands.
For example, `if(0 && (1/0)) ;` completes, while `0 && (1/0);` faults.

`Driver.Integer_source` supplies the semantic preparation shared by expression
evaluation and program execution. The program driver joins each AST expression
root to its typed root by source ID and byte span, rejecting missing, duplicate
or unconsumed roots. Generated expansion frames keep their distinct source IDs.
`Ir.Integer_program_lowering` constructs blocks in deterministic order, using
explicit branch destinations and physical fallthrough blocks. Expression values
never cross a block boundary. Graph construction and x87 verification precede
whole-graph integer VM preflight.

The lowerer retains unreachable supported instructions. An unsupported opcode,
type or flag in a skipped branch therefore fails preflight. A supported division
fault occurs only if execution reaches that instruction. These are raw runtime
IR semantics. Native constant folding can fault earlier, and literal-divisor
rewrites can produce different values; [issue #585](https://github.com/frankischilling/holyc-ocaml/issues/585)
tracks those optimizer differences. [Issue #574](https://github.com/frankischilling/holyc-ocaml/issues/574)
tracks constant-form shifts.

## Limits and diagnostics

`--step-limit` defaults to 100000 and must be positive. Every attempted
instruction consumes a step, including branches, jumps, discarded values and
the stream-end marker. `6*7;` completes in five steps and fails with a budget
of four. Infinite loops stop at the configured budget. Runtime diagnostics
retain the failure stage, executed steps, block, instruction and source span.
Failure returns status 1 with diagnostics on stderr and no successful stdout
report. The JSON success schema is `holyc-integer-program-v1`; it records the
implementation commit, reference commit, mode, target, arithmetic policy,
budget, frame/depth limits, executed steps, termination and final expression value.

The library entrypoints include `compile_integer_program` and `run_integer_program`;
`lower_integer_program` retains its graph-only top-level boundary.
Successful results contain `value` and nonfatal `diagnostics`; the value is a
compiled program, VM execution result or graph, according to the entrypoint.
A failure returns all
earlier warnings and the error in one diagnostic list. Both `run` and program
IR dumping print retained warnings. JSON failures emit one diagnostics array.
Lowering can produce a graph containing a VM-unsupported type or opcode;
execution performs the VM-domain preflight before running any instruction.

`HCRUN0001` rejects unsupported declarations, output, top-level returns, labels,
`goto`, switches, exceptions and locks, even inside unreachable source.
`HCRUN0002` reports a break without an enclosing loop target. `HCRUN0003`
rejects unsupported expressions, including chains inside conditions.
Ordinary integer value chains such as `1<2==2` share their middle operands
and eagerly combine adjacent comparisons. `PrsExp.HC:49-52,225-230` supplies
the source rule. Parenthesized comparisons and tighter right operands such as
`0==1<2` retain their grouping and precedence. Floating chains and conditional
chains remain unsupported. Integer chains carry the cumulative unsigned
comparison class. Multiple pending comparison reductions, such as
`1==2<3==1`, remain unsupported under
[issue #593](https://github.com/frankischilling/holyc-ocaml/issues/593).
`HCRUN0004` reports an inconsistent
source/IR join, and `HCRUN0005` rejects an unavailable execution target.
`HCRUN0006` retains the initializer optimizer boundary, including transitive
callees; `HCIRVM0017` rejects missing or inconsistent initialization contexts.

General memory, indirect/external execution, compiler-state changes, `#exe`, native emission and
general program execution remain unfinished under [M5 issue #396](https://github.com/frankischilling/holyc-ocaml/issues/396)
and the backend milestones. The complete stateful compiler must execute source
and compiler effects in stream order as those operations become available.

## Verification

`test/test_integer_program.ml` runs real source through the parser, semantic
passes, graph verifier and VM. It covers taken and skipped branches, nested
logical forms, reached faults, preflight failures, loop budgets, break targets,
for ordering, JIT/AOT inputs and deterministic replay. The CLI test runs the
compiled executable and checks reports, exact budgets, failure status, empty
stdout on errors and deterministic program dumps.

```text
opam exec -- dune exec test/test_main.exe -- test "source integer"
opam exec -- dune runtest
```

These are hosted integration results with pinned source evidence. The existing
[division oracle fixture](../test/oracle/integer-division.json) separately
records selected native conditional fault behavior. This change adds no new
TempleOS execution captures, loader results or bootstrap claims.
