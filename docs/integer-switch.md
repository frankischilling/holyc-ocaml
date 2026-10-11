# Integer switch execution

Integer switches execute through the checked interpreter and generated x86-64
code. Both preprocessing modes support ordinary `switch (...)`, bracket
`switch [...]`, repeated defaults, and nested `start:` / `end:` regions.
Case preparation follows the original parser callbacks, including cases in
unused functions and unreachable bodies.

`examples/switch-regions.hc` returns 42 and exercises bracket dispatch, a setup
region, and an in-table hole. The existing `examples/integer-switch.hc` and
`examples/integer-switch-large-range.hc` cover ordinary switches and the largest
supported dispatch range.

```text
opam exec -- dune exec --root . -- bin/holyc.exe run --target=ir --mode=jit examples/switch-regions.hc
opam exec -- dune exec --root . -- bin/holyc.exe run --target=host-jit --mode=aot examples/switch-regions.hc
```

## Case preparation

An explicit endpoint is evaluated after expression lookahead and before its
following range delimiter or colon is validated. A ranged case evaluates its
start and end separately, in source order. The completed case is retained after
the colon's following lookahead. The switch's final range and overlap checks
happen after the closing brace's following lookahead.

Closed numeric expressions use the bounded numeric evaluator. Original
`sizeof`, `offset`, and `defined` query selections supply their saved values;
a later declaration cannot change an earlier selection. Closed floating values
retain the finite signed-I64 conversion boundary.

In a current JIT task, supported scalar integer case expressions can read or
update retained globals and call retained functions. The expression is typed,
lowered, and executed at its original callback, even when the containing function
is never called. Its captured result is reused whenever the switch executes.
A native source task runs a native fragment and retains its captured result.
It requires the native adapter throughout this path.

Use `--target=host-jit-task` for these retained native case effects. The ordinary
`host-jit` target compiles an isolated program and retains its explicit boundary
for endpoints requiring task execution. For the maintained effectful example:

```text
opam exec -- dune exec --root . -- bin/holyc.exe run --target=ir --mode=jit examples/switch-case-effects.hc
opam exec -- dune exec --root . -- bin/holyc.exe run --target=host-jit-task --mode=jit examples/switch-case-effects.hc
```

For example, if `Next()` increments a retained global and returns it, the call
in `case Next():` runs while the containing function is compiled. Calling that
function twice does not call `Next()` twice. If the following colon is malformed,
the already reached increment remains visible. If evaluation itself fails,
the parser does not prepare later endpoints.

Each endpoint retains an opaque original parser receipt, its switch owner,
endpoint role, preceding case, and preceding range endpoint. The declaration
ledger binds it to the original command, semantic table, selected references,
and query reads. A runtime attempt can be claimed and completed once. A native
capture additionally belongs to its exact program, image, and task storage.
Reconstructed ASTs, copied receipts, equal spans, and equal numeric results
cannot replace this evidence. Source activation replays the original journal
and executes only its pending endpoint.

Ordinary AOT source expressions that need executable task values remain an
explicit `HCRUN0006` boundary. AOT source declarations alone do not provide
retained runtime storage or callable bodies. Current-task execution inside
supported directives retains its own source and execution rules. Automatic
locals of the function being compiled are not current runtime values.

## Values and dispatch

The first implicit `case:` is zero. Later implicit cases advance from the
previous normalized upper endpoint. Descending inclusive ranges swap their
endpoints: `case 5...3:` covers 3 through 5, and a following implicit case is 6.
The source I64_MIN sentinel makes a following implicit case start at zero.

Final preparation lowers a positive minimum between 1 and 16 to zero, then
requires an inclusive range of 1 through 65,535 values. A switch without a case
has no valid range. Overlap checks follow range validation. An inclusive range
whose upper endpoint is I64_MAX rejects with `HCSW0001`: the source's signed
iteration counter would wrap. This is a hosted termination boundary.

The selector executes once. Lowering preserves its integer register bits in an
internal-I64 value, subtracts the saved lower bound, and emits a checked dispatch
with the adjusted selector, saved range, and ordered targets
`[default; target0; ...; target(range-1)]`. Holes point to the default target.
Without a default, holes point to the end of the switch.

| Form | In-table selector | Out-of-table selector |
| --- | --- | --- |
| `switch (value)` | Selected case or default hole | Default or switch end |
| `switch [value]` | Selected case or default hole | Hosted fault `HCSW0005` |

TempleOS bracket dispatch omits the bounds check. This implementation traps an
out-of-table access before using a host address. That fault is an explicit
hosted policy. It is not a new observation of TempleOS fault behavior.

Repeated `default:` labels preserve their textual fallthrough positions.
Dispatch uses the last default label. In the pinned source, these definitions
reuse one label object whose final emitted address wins. `default` is the source
keyword; `KW_DFT` is its internal enum. `dft:` remains an ordinary goto label.

## Setup regions and control flow

A `start:` region's leading statements form its setup prefix when a case,
default, or nested start follows before `end:`. Dispatch to a case calls each
active prefix from outermost to innermost, then enters that case's body.
Ordinary fallthrough through the region runs its prefix once and skips the
setup calls at later labels. The prefix body is emitted once.

A region ending before any case, default, or nested start keeps its leading
statements inline. An empty prefix needs no local continuation. Cases still
belong to the enclosing switch's one case counter, range, and dispatch table.

A `break` in a case body exits its nearest start region, allowing the suffix
after `end:` to execute. Outside a start region, it exits the switch. Inner
loops and ordinary switches retain their own break targets. A direct break in
the setup prefix rejects in its original parser phase with `HCPARSE0170`.

Function-local gotos between case bodies and within one setup prefix retain
their original label identities. A goto entering or leaving a setup prefix,
crossing sibling prefixes, or a return from a prefix rejects with `HCRUN0001`.
The TempleOS documentation warns against leaving a prefix this way because it
bypasses the local return address.

The shared graph represents a prefix invocation as `IC_SUB_CALL` with an exact
entry and continuation, and its return as `IC_RET` with checked continuation
targets. The interpreter keeps a private continuation stack for each function
activation. Generated code reserves fixed frame slots for continuation tokens
and a count, so local calls do not move RSP after the function prologue.
A reached missing, foreign, or over-capacity continuation faults with
`HCSW0006`. Function calls and recursion preserve each activation's own tokens.

## Work and storage limits

`--switch-work-limit` and the public `max_switch_work` argument default to
100,000 and must be positive. Closed endpoints charge evaluated numeric nodes.
Runtime endpoints charge their reached preparation work and executed fragment
instructions. Reports retain consumed work after evaluation, parsing, or later
execution fails. All switch owners in an invocation share this allowance.

Nested `StreamExePrint` inside a supported `#exe` context shares that allowance.
The parent charges its reached instructions before the child parser runs.
Child runtime instructions count once; closed child endpoints still charge
their numeric evaluation work. Each fragment checks the remaining allowance,
and the suspended parent receives the reduced limit before it resumes.
Native reports keep actual executed instructions separate from withheld
allowance, including when a tightened limit stops the parent.

The initializer-work limit remains independent. Its phase accounting includes
instructions executed by nested source calls: a nested scalar fragment contributes
to its own preparation phase and the active parent phase. Native execution
reserves this reached work before admitting more child preparation, then commits
it when each original fragment finishes. An exhausted initializer allowance
reports `HCIRVM0007`; an exhausted switch allowance reports `HCSW0003`.

Dispatch tables have a separate cumulative limit of 65,536 target slots,
including each default slot. The limit is checked before table expansion.
Original graph shapes, target cardinalities, producer types, and continuation
targets are checked before execution. Structured lowering also bounds local
call sites before emitting the cases-by-prefix expansion.

Native dispatch groups consecutive entries with the same destination. A single
large range therefore has constant dispatch code size; alternating destinations
need separate branches. Each reached switch or local continuation operation
charges one IR instruction, regardless of its machine-code expansion.
Preparation, runtime instruction, block, code-byte, frame, call-depth, and
active-stack allowances retain their separate checks.

## Source evidence and remaining scope

The reference is TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`.

- `Compiler/PrsStmt.HC:578-789` supplies endpoint evaluation, implicit values,
  ranges, start/end construction, shared label ownership, break targets,
  lookahead order, and final range and overlap checks.
- `Compiler/PrsExp.HC:1117-1151` compiles and calls a case expression and converts
  an F64 result to I64.
- `Compiler/BackC.HC:602-771` supplies bounded and no-bound dispatch.
- `Compiler/OptPass789A.HC:286-295` assigns the emitted label address.
- `Compiler/OpCodes.DD:164` identifies the default keyword.
- `Doc/HolyC.DD:128` warns about nonlocal exits from setup prefixes.

Tests use source-derived expected results, public execution APIs, original
receipt controls, and generated native code. They do not constitute a new
TempleOS oracle capture. General F64 selectors and effectful floating
expressions, full ABI and optimizer parity, object/BIN writing, loader
acceptance, whole-tree compilation, and bootstrap remain separate work.
