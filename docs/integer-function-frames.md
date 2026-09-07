# Integer function-frame execution

`Ir_integer_interpreter.execute_function` executes one verified named body with
its exact checked frame layout, argument bits, `max_steps` and
`max_frame_bytes`. The supported storage consists of scalar `I64`/`U64` named
parameters and automatic locals. This is the executable storage connection in
[issue #595](https://github.com/frankischilling/holyc-ocaml/issues/595).

The function symbol, body scope, ordered parameter/local symbols and types must
match the frame. Arguments initialize the named parameters in source order.
Locals start uninitialized. The byte limit covers the checked local frame size
plus eight bytes per parameter. Each invocation creates independent storage.

`Ir_expression_lowering.lower_typed_result` and
`Ir_return_lowering.lower_function_return` accept an optional `frame`. A scalar
bound identifier uses its checked `IC_RBP`, displacement and `IC_ADD` address,
followed by `IC_DEREF`. A simple assignment builds the destination address,
evaluates the right operand and emits `IC_ASSIGN`; it does not read the old
destination value. Parentheses around a direct assignment target preserve that
address. Array identifiers never become scalar loads. The assignment yields the
stored bits with the destination type. Planning validates exact binding evidence
before allocating identities.

The function interpreter accepts only canonical addresses of known slots.
Saved-frame and return-address positions, unallocated displacements and
arbitrary pointer arithmetic are unavailable. Public `I64`/`U64` types retained
by checked storage and arithmetic are accepted in this entry point; immediate
constants and `IC_HOLYC_TYPECAST` views retain their internal-word restriction.
Public `U64` negation retains `U64`; internal `U64` negation produces internal
`I64`, following `OptPass012.HC:191-192`. A same-width integer return preserves
bits and adopts the declared return type (`PrsStmt.HC:1114-1117`).
Address instructions, loads and stores each consume one step. Block transfers
clear temporary values while preserving slot contents and the pending return.
The complete graph, including unreachable instructions, passes preflight before
any instruction executes.

`HCIRVM0011` reports invalid, unsupported or over-budget frame metadata before
execution. A reached uninitialized read reports `HCIRVM0012`; returning from an
integer function without a value reports `HCIRVM0013`. Reached failures retain
their block, instruction, source span and executed-step count. Uninitialized
reads are a hosted diagnostic, not a claim that TempleOS initializes its locals.

The source basis is TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`:

- `PrsStmt.HC:114-124,143-169` and the existing frame-layout audit establish
  parameter offsets and the completed local frame.
- `PrsExp.HC:762-805` establishes frame-relative identifier addresses.
- `OptPass012.HC:866-896` retains the assignment destination class and selects
  conversion intent. `BackC.HC:159-189` writes the destination and produces the
  assigned value.
- `PrsExp.HC:128-132,728-747,773-776` distinguishes array values from scalar
  loads and preserves parenthesized lvalues.

The focused tests join actual checked expression and return roots to a verified
named body, then execute parameter reads, local writes/reads and a returned 42.
They also cover unsigned bits, nested assignments, mutable parameters, loop
visits, invocation isolation, exact budgets and failure boundaries.

```text
opam exec -- dune exec test/test_main.exe -- test "integer frames"
```

This low-level entry point does not compose complete source function bodies or
execute calls. `IC_ENTER`, `IC_LEAVE`, native calling conventions, static/global
storage, arrays, callbacks, aggregates, floating values and numerical casts
remain unsupported by this entry point. The graph-only `execute` and `eval`
contracts are unchanged. [Integer source functions](integer-functions.md) adds
initializer retention, body composition, direct calls and caller continuation
through `run` for the original source:

```hc
I64 Add(I64 a, I64 b) {
    I64 c = a + b;
    return c;
}
(Add(20, 22));
```

The separate source integration observes 42 in both modes. These frame tests
add no native TempleOS capture and do not complete the general interpreter or M5.
