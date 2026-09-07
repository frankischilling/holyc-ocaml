# Integer source functions

[Automatic integer arrays](integer-arrays.md) add element loads, stores, updates
and pointer arguments at any rank. References retain the caller's full object
extent through copies and recursion; the caller-element fixture returns 42.

[Scalar pointers](integer-pointers.md) extend fixed parameters and automatic
locals with one-level I64/U64 references. Explicit arguments retain the original
caller's object across calls and recursion; pointer reassignment changes only
the receiving pointer slot. Returns remain integer words.

Reference: `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

The original V1 fixture in `examples/integer-function.hc` executes through the
shared frontend, semantic passes, verified IR and bounded interpreter:

```hc
I64 Add(I64 a, I64 b) {
    I64 c = a + b;
    return c;
}
(Add(20, 22));
```

```text
opam exec -- dune exec bin/holyc.exe -- run --target=ir examples/integer-function.hc
opam exec -- dune exec bin/holyc.exe -- run --target=ir --mode=aot --format=json examples/integer-function.hc
opam exec -- dune exec bin/holyc.exe -- dump-ir --program examples/integer-function.hc
```

Both execution modes produce 42 in 29 instructions. The human report contains
`final-value=42 type=i64 bits=0x000000000000002a`. JSON keeps the
`holyc-integer-program-v1` schema and adds `final_value`, an object containing
`type`, decimal `value` and hexadecimal `bits`, or null when no top-level
expression ran. Termination remains `stream-end`; successful process status is
0. The value is the last reached top-level expression, including one inside a
top-level block or branch. Function-internal expression statements do not
replace it. Failures return status 1, diagnostics on stderr and no result report.

## Accepted source and limits

Functions accept named, fixed I64/U64 parameters and automatic scalar I64/U64
locals. Scalar initializers, simple assignments, returns and the existing
structured statements compose into checked bodies. Calls compose through the
supported expression lowerer, including arithmetic, arguments, initializers,
conditions and returns. Function statement calls, repeated calls and recursion
have independent frames and explicit caller continuations. See
[nested integer calls](integer-nested-calls.md) for source examples and tests.

Arguments execute from right to left, then bind to their original formal
positions. For example, `Take(n=1,n=2)` leaves n equal to 1, while
`Sub(20,22)` still binds a=20 and b=22. Parsing, argument typing and default
selection retain source order. The direct-call IR composer also emits variadic
trees in reverse order, followed by the hidden argc value and reversed fixed
arguments; variadic execution remains unsupported by this integer VM.

`--step-limit` defaults to 100000 and covers all caller and callee instructions.
The Add fixture succeeds at 29 and fails at 28. `--frame-byte-limit` defaults
to 1048576 and bounds simultaneously active parameter/local slots; Add requires
24 bytes. `--call-depth-limit` defaults to 128. All limits must be positive.
An instruction that faults consumes its step; exhaustion stops before the next
instruction. Frame/depth exhaustion on a nested call is a reached execution
failure. Invalid definitions or individual over-budget frames fail preflight.

Ordinary scalar [global storage](integer-globals.md) is now shared with callers.
Global declaration initializers and [static initialization](integer-static-initializers.md)
now use the shared persistent executor. Arrays, narrow/floating storage, callbacks, arbitrary
pointers, defaults, variadic execution, external/import execution and joined
prototype identities remain unsupported. Every definition and unreachable block
is checked before any instruction runs. A reached uninitialized local read is
an explicit hosted diagnostic. Fault notes retain stage, total steps, block,
instruction and the owning function when one is active.

## Shared implementation and source evidence

`PrsVar.HC:592-619` allocates automatic storage, restores the local's source
expression and parses its initializer assignment. The existing semantic
traversal now retains that scalar root and its exact local declaration.
Initializer lowering checks the same frame symbol, declaration positions and
retained type reference, then emits an address, the value and IC_ASSIGN. It
does not invent an identifier occurrence or read the previous local value.

`PrsStmt.HC:151-169,1114-1117` establishes the function body, shared leave path
and declared return class. The structured graph builder reuses expression and
return fragments. Invocation allocates logical slot storage; the hosted leave
block ends with IC_RET. This does not implement native prologues, IC_ENTER or
IC_LEAVE execution, or a host ABI.

`PrsLib.HC:107-111` and `PrsExp.HC:487,491-555` show the argument-fragment
stack and reverse append order. `PrsExp.HC:555-586` supplies call selection,
cleanup and call-end results. Program preflight matches their exact symbol,
argument count, type and cleanup flag/byte relationships. Argument pushes and
returned words cross calls; temporary graph values remain block-local.

The public library offers `compile_integer_program`, `integer_program_entry`,
`integer_program_functions`, `integer_program_human` and `run_integer_program`.
Successful phase results retain `value` and nonfatal `diagnostics`.
`Ir_integer_interpreter.final_value` reads the program result. The run API
accepts optional `max_frame_bytes`, `max_call_depth` and `max_global_bytes` and a required
`max_steps`. `lower_integer_program` remains the graph-only top-level API.
The bounded `eval` format and execution contract are preserved.

`test/test_integer_functions.ml` covers actual source execution, initializer
order, parameter positions, right-to-left side effects, return signedness,
mutable bodies, repeated/nested invocations, recursion, bounds and malformed
call/frame evidence. The CLI test runs the checked-in fixture in both modes.
These are hosted execution and pinned-source results under
[#597](https://github.com/frankischilling/holyc-ocaml/issues/597). Optimizer
differences, general memory/runtime services, stateful #exe, native backends,
loader compatibility and bootstrap remain open under the full compiler mission.
