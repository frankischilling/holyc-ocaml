# Nested integer call expressions

Reference: `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

`run --target=ir` now uses returned I64/U64 words in supported arithmetic,
assignments, initializers, conditions, returns and other calls' arguments.
`examples/integer-nested-calls.hc` combines Add, a returned call, a mutable
summation loop and top-level arithmetic. It returns 42 in JIT and AOT modes.

```text
opam exec -- dune exec bin/holyc.exe -- run --target=ir examples/integer-nested-calls.hc
opam exec -- dune exec bin/holyc.exe -- run --target=ir --mode=aot --format=json examples/integer-nested-calls.hc
opam exec -- dune exec bin/holyc.exe -- dump-ir --program examples/integer-nested-calls.hc
```

The existing final-value report, stream termination and process status are
unchanged. See [integer source functions](integer-functions.md) for the original
Add fixture, resource defaults and checked storage domain.

## Expression behavior

The shared expression planner keeps the audited operand order and treats each
checked call as one producer. Values computed before a call survive its callee;
the caller resumes the expression with its own slots and temporary values.
Returned recursion includes `return n*Fact(n-1);` and
`return Fib(n-1)+Fib(n-2);` under the existing shared execution and frame bounds.

Nested arguments retain right-to-left evaluation and their formal positions.
`Sub(Id(n=1),Id(n=2))` leaves n equal to 1 and returns minus one. Each call's
arguments remain associated with it while inner calls execute. Source-defined
variadic calls execute integer tails in the same order, with separate `argc`,
`argv` storage and actual tail bounds for every recursive invocation.

Calls in conditions use the existing short-circuit branches. Ordinary logical
values execute eagerly. A call used as a shared comparison-chain operand runs
once. Conditional chains and multiple pending comparison reductions retain
their unsupported boundaries under #593.

Program execution accepts checked public I64/U64 unary and binary results,
including top-level expressions with no local frame. Exact type relationships
still matter: public U64 negation preserves U64; same-width argument,
assignment and return crossings preserve bits and adopt the destination class.

## Lowering and verification

`Ir.Expression_lowering.call_lowerer` is an optional compiler callback for an
exact typed call at caller-seeded identities. The program builder supplies the
existing classified direct-call composer, which validates source-target
ownership. The expression emitter checks contiguous identities, matching
call-start/end symbols, the final producer, checked result type and source span.
Only the final call-end producer receives a retained result conversion.
Unsupported callbacks expose no partial expression sequence.

The callback passes through arguments, initializer values and return lowering.
There is one expression planner and one call protocol. Callers that omit the
callback retain their previous domain. The raw graph-only executor keeps its
type restrictions. Floating conversion intent can compose in IR, but floating
execution remains outside this integer VM.

`Compiler/PrsExp.HC:438-586` supplies independent argument trees, conversion
intent, selected calls, cleanup and caller results; `PrsLib.HC:107-111` supplies
COC stack order. `PrsVar.HC:592-619` supplies initializer restoration and
`PrsStmt.HC:1114-1117` supplies declared return classes. The implementation
reuses checked semantic children without repeating source binding.

All definitions and entry blocks pass preflight before execution. Reached
calls share instruction, active frame-byte and call-depth bounds. Faults retain
the active function, source operator and execution context.
`test/test_integer_nested_calls.ml` covers these source contexts, side effects,
caller value lifetime, comparison sharing, signedness, recursion, exact bounds,
deterministic dumps, nested faults, malformed callbacks and conversion flags.
The CLI test runs the checked-in fixture in both modes.

Defaults, variadic/indirect/external execution, joined prototypes, non-scalar
storage, general runtime output, stateful `#exe`, native backends and bootstrap
remain unfinished. Runtime arithmetic retains the optimizer/oracle gaps in
#574/#585. These are hosted tests and pinned-source evidence, with no new
native TempleOS capture.
