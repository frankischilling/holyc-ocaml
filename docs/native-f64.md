# Native scalar F64

The source-owned host-JIT path carries F64 values as tagged 64-bit words. Fixed
parameters, ordinary automatic locals, assignments, direct generated calls and
returns use those bits without converting them through an OCaml float. Literal
transport preserves signed zero and NaN payloads. JSON reports use a hexadecimal
bit string and a separate display string, so infinities and NaNs remain valid
JSON. This subset is available through `host-jit` and `host-jit-task` in both
outer preprocessing modes. The integer interpreter does not execute floating
arithmetic; the closed integer expression/image APIs remain integer-only.

The admitted arithmetic subset is unary minus and `+`, `-`, `*`, `/` operations
on F64 operands. Source-owned pure expression regions can retain intermediate
values in x87 extended precision. For example:

```c
F64 Add(F64 a,F64 b) { return a+b; }
Add(1.5,2.25);
```

The result is F64 bits `0x400e000000000000` (3.75). Floating division uses masked
x87 exceptions, including infinity for nonzero finite values divided by zero;
it does not raise the native integer division-by-zero fault.

## Precision and source ownership

`X86_64_float_plan` binds admission to the original graph and records instruction
and value identities, operand orientation, literal-load choices, transient
extended-value identities and peak hardware stack depth. Code generation consumes
that plan before emitting an operation. An admitted operation leaves the x87
stack empty at its materialization boundary. Calls, branches and exits therefore
cannot inherit an unproved extended value.

Linked arithmetic requires an authenticated original function, runtime context
and frame. The planner re-lowers disjoint pure source subtrees and compares their
ordered graph slices. Only proved frame reads acquire the source's MOV producer
classification. A raw graph cannot grant itself linked precision.

Linking follows the pinned compiler's asymmetric rule. With runtime operands
`a=9007199254740992.0` and `b=1.0`, `(a+b)-a` returns 1.0, while
`(a+b)-(a+b)` returns -1.0: the left subtree materializes to binary64 and the
right remains extended. Explicit storage, as in `F64 t=a+b;return t-a;`, returns
positive zero. Unary minus is also a materialization boundary in this subset.
The complete example is `examples/native-linked-f64.hc`; run it with
`holyc run --target=host-jit examples/native-linked-f64.hc` or `host-jit-task`.

Normalization happens before links are selected. Right-hand multiplication by
one and supported positive-zero identities can disappear; surviving copy
templates remain boundaries. Thus `((a+b)*1.0)-a` returns 1.0, but
`(1.0*(a+b))-a` returns positive zero. The implementation rejects linked regions
requiring unsupported constant folding or literal reassociation, including
`((a+0.5)+0.5)-a`.

Only one proved extended value lives between operations, with at most two
transient x87 operands. Deep expression trees pressure materialized word homes,
not an invented eight-value x87 evaluation stack. Calls, stores and control-flow
edges cannot carry an unproved extended value. Call-result and global-containing
regions remain outside the source-link proof.

Arithmetic immediate loading is distinct from bit transport. Zero, including
negative zero, uses FLDZ; one uses FLD1. Other ordinary literals load from a
binary64 word. PI and the logarithmic builtin constants currently reject in
arithmetic because their source load rule depends on compiler-option evidence
not yet retained by this plan. Their raw literal transport is unchanged.

The source's identity templates also matter: adding positive zero, subtracting
positive zero, multiplying by one or dividing by one can copy the other operand's
bits directly. These copies preserve signaling-NaN payloads without performing
an unnecessary x87 operation. Other arithmetic uses the source's memory-operand
orientation, including reversed subtraction and division when a right builtin
constant is loaded first.

## Storage and host state

Ordinary F64 locals stay in their checked stack slots. A separate authenticated
mixed-scalar proof admits supported explicit variable-register requests and
plain `reg` when its exact placement or stack selection is proved. GPR homes
carry binary64 words, never extended x87 values. Unproved requests reject rather
than silently spilling. See [mixed scalar register analysis](register-variable-analysis.md#mixed-scalar-register-analysis)
for candidate counts, calls, options and exact straight-line score pressure. The arithmetic scratch
word and private register saves count toward the generated frame's stack budget.

On supported compiler builds, entry and host-callback thunks save and restore the
full x87 state with FXSAVE64/FXRSTOR64. Guest entry uses FNINIT, whose architectural
control word is `0x037f`: masked exceptions, extended significand precision and
round-to-nearest. Host state is restored before returning to OCaml. Nested native
callbacks preserve the suspended guest state. This is an x87 contract, not a
claim to preserve every XSAVE component or implement SSE floating arithmetic.

Builds without the checked x87 thunk retain the integer execution path. Sealed
images containing x87 instructions reject before native mapping or execution on
those builds. Capability follows emitted instructions, not just the final return
type, so an integer-returning function cannot hide unsupported floating work.

## Current exclusions

Implicit and explicit numeric conversions, comparisons, remainder, power,
shifts, compound updates, increment/decrement, floating intrinsics, variadic F64
arguments, `%f`, F64 pointers, arrays, aggregate fields, globals, statics,
parameter defaults and callback invocation signatures remain unsupported. Mixed
functions containing F64 also reject narrow integer frame members unless their
register or stack selection has an authenticated allocation proof; `noreg` alone
does not supply that proof. I64/U64
control variables do not have this width-dependent ambiguity. An unused callback
header may still declare F64: its existing word-storage support does not admit
invocation of that signature. A nested source callback requiring an I64 result
rejects a final F64 value rather than reinterpreting its bits.

Unsupported definitions are preflighted even when unreachable. These boundaries
are native admission errors; there is no interpreter fallback.

## Pinned references

All references are to TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c` under `third_party/TempleOS`:

- `Compiler/OptLib.HC:535-554` and `Compiler/OptPass3.HC:165-228` select float
  links from expression producers.
- `Compiler/OptPass5.HC`, `Compiler/OptPass6.HC:119-165` and
  `Compiler/BackFA.HC:6-59` propagate and finalize those links.
- `Compiler/BackFA.HC:195-239,339-518` implements unary minus, binary arithmetic,
  operand orientation and materialization.
- `Compiler/BackLib.HC:423-442` selects builtin float loads.
- `Compiler/BackFB.HC:488-594` implements arithmetic identity copies.
- `Compiler/OptPass789A.HC:779-785,1026-1032` transports return words through RAX.
- `Compiler/OptPass3.HC:29-47` excludes ordinary F64 from automatic GPR selection;
  `Compiler/PrsVar.HC:449-516` retains explicit register requests.
- `Kernel/KStart64.HC:5-7`, `Kernel/KTask.HC:211-213` and
  `Kernel/Sched.HC:87,119` establish initialized task FPU state and its save/restore
  model.

These are source-derived contracts and hosted regressions, not a new TempleOS
runtime-oracle capture. This subset does not complete issue #703 or establish
general floating allocation, complete floating normalization or full HolyC ABI
compatibility.
