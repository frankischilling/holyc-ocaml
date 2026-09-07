# Integer programs in the IR interpreter

Reference commit: `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

`holyc run --target=ir FILE` executes a batch of integer top-level statements.
The file needs no `main` function. `holyc dump-ir --program FILE` exposes the
verified graph used by that command. Both commands accept the existing include,
definition, deterministic predefined-value and JIT/AOT preprocessing options.

```text
opam exec -- dune exec bin/holyc.exe -- run --target=ir examples/integer-control-flow.hc
opam exec -- dune exec bin/holyc.exe -- dump-ir --program examples/integer-control-flow.hc
```

The accepted statements are ordinary integer expressions, empty statements,
blocks, comma statement sequences, `if`/`else`, `while`, `do`/`while`, `for`,
and `break`. Expressions use the existing internal `I64`/`U64` VM domain.
Expression statements discard their values after evaluating them. This command
prints an execution report; it has no implemented program-output operation.
`holyc eval` continues to return the value of exactly one expression statement.

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
budget, executed steps and termination.

The library entrypoints are `lower_integer_program` and `run_integer_program`.
Successful results contain `value` and nonfatal `diagnostics`; the value is the
verified graph or VM execution result respectively. A failure returns all
earlier warnings and the error in one diagnostic list. Both `run` and program
IR dumping print retained warnings. JSON failures emit one diagnostics array.
Lowering can produce a graph containing a VM-unsupported type or opcode;
execution performs the VM-domain preflight before running any instruction.

`HCRUN0001` rejects declarations, function definitions, output, returns, labels,
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

Memory, variables, calls, compiler-state changes, `#exe`, native emission and
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
