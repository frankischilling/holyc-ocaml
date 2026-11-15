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

Arithmetic immediate loading is distinct from bit transport. Positive zero uses
FLDZ; one uses FLD1. Negative zero and other ordinary literals load from a
binary64 word. `ICBuiltInFloatConst` tests `!d` using raw-word truth, so negative
zero's nonzero sign bit prevents the FLDZ shortcut (`BackLib.HC:423-428`,
`OptLib.HC:235-242`, `BackB.HC:640-660`). Pi and the four logarithmic constants
use their source-selected x87 builtin instructions unless the original function
enables `No_builtin_const`. Raw literal transport preserves binary64 bytes.

The source's identity templates also matter: adding positive zero, subtracting
positive zero, multiplying by one or dividing by one can copy the other operand's
bits directly. These copies preserve signaling-NaN payloads without performing
an unnecessary x87 operation. Other arithmetic uses the source's memory-operand
orientation, including reversed subtraction and division when a right builtin
constant is loaded first.

## Fixed callback calls

Both native targets support fixed F64 callback parameters and results in JIT
and AOT source modes. Global, static, automatic, array and member callback slots
retain the selected header and original executable owner. Mixed integer/F64
parameters, U0 returns, saved integer defaults and borrowed F64 pointer arguments
use the existing checked callback path. F64 defaults and variadic F64 arguments
remain unsupported.

The [callback example](../examples/native-f64-callbacks.hc) passes an owned
two-argument callback to `Apply` and returns F64 3.75. The callee is captured
before argument effects. Signature matching, frame and instruction limits,
retained mappings and overlapping-write checks still apply. Restoring address
bytes through an F64 view cannot restore executable ownership. Pure F64
transport preserves signaling-NaN words and requires no x87 operation; arithmetic
uses the original callee's floating plan. This does not authorize new precision
links across calls or change isolated declaration-initializer restrictions.

`PrsFunCall` at `Compiler/PrsExp.HC:383-591` saves the original indirect callee,
converts fixed arguments according to their selected classes, and emits
`IC_PUSH_REGS`, `IC_CALL_INDIRECT` and cleanup with the return class.
`Compiler/OptPass789A.HC:723-730,779-785` emits the indirect call and transports
the return word through RAX. These are source-derived expectations; the tests
add no independent TempleOS runtime capture.

## Storage and host state

[Automatic floating storage](native-f64-storage.md) supports F64 arrays, packed
members and one-level owned pointers, including fixed pointer parameters and
borrowed recursive calls. Binary64 stores materialize their values; these memory
operations do not grant an extended-precision link.
Globals and static locals also support F64 storage. Declaration initializers
execute in native source-task JIT. Original nonconstant same-class initializers
also run as AOT load assignments on both native targets, including fixed arrays
and packed aggregate leaves. Closed AOT and isolated floating preparation remain
unsupported.

Ordinary F64 locals stay in their checked stack slots. A separate authenticated
mixed-scalar proof admits supported explicit variable-register requests and
plain `reg` when its exact placement or stack selection is proved. GPR homes
carry binary64 words, never extended x87 values. Unproved requests reject rather
than silently spilling. See [mixed scalar register analysis](register-variable-analysis.md#mixed-scalar-register-analysis)
for candidate counts, calls, options and exact score pressure across supported
whole functions. The source ledger includes loop components, dead arms and
post-return statements once, before dead-code removal. Arithmetic scratch words
and private register saves count toward the generated frame's stack budget.

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

## Stored constants under `No_builtin_const`

Native source tasks honor `OPTf_NO_BUILTIN_CONST` (bit 36) for the five
option-dependent constants: pi, log2(10), log2(e), log10(2) and ln(2).
With the option enabled at original function compilation, arithmetic and direct
ordered conditions load the literal's stored binary64 word. With the option
disabled, including the default, they use `FLDPI`, `FLDL2T`, `FLDL2E`, `FLDLG2`
or `FLDLN2`. These instructions retain extended precision until the original
materialization boundary. For example, `3.141592653589793-x` with that binary64
value supplied as `x` returns bits `0x3ca1a80000000000` by default and positive
zero with `No_builtin_const` enabled. Zero and one keep their existing special
load and identity rules under either option value.

Ordinary value comparisons and stored variables use materialized binary64
words, so `return x<3.141592653589793;` and a direct `if` condition can differ
at the constant's binary64 boundary. This follows the separate original
comparison templates; constant selection does not grant extended precision to
stored values.

Isolated constant-only arithmetic loads the literal binary64 words, reproducing
the value of the earlier `OptPass012` fold. Constant `3.141592653589793-3.0` yields
bits `0x3fc21fb54442d180`, while subtracting a runtime parameter set to 3.0 yields
`0x3fc21fb54442d184`. This does not authorize links or reassociation through an
unproved constant fold; those regions retain their existing normalization gate.
The fold rules are `Compiler/OptPass012.HC:180-188,308-318,403-418,485-495,619-627`.

```c
#exe { Option(36,1); }
F64 AddPi(F64 x) { return x+3.141592653589793; }
AddPi(0.0);
```

Run this source with `holyc run --target=host-jit-task`. Both outer source modes
are supported. The isolated `host-jit` command retains its existing `#exe`
boundary. A retained function uses its original compilation options. Directives
reached while reading beyond its closing brace can still affect compilation;
place a subsequent declaration before a directive intended for later functions.

The load plan requires the original function, frame, runtime graph and option
snapshot. A raw graph or numeric option bits alone cannot authorize this path.
Retained plans recheck that source evidence before emission. The source rule is
`Compiler/BackLib.HC:423-442`, with memory loads selected by
`Compiler/BackFA.HC:339-518` and direct condition loads selected by
`Compiler/BackFB.HC:226-425`, at the pinned reference revision. Default builtin
loads also work through isolated `host-jit` without a compile-time directive.

## Numeric conversions

[Scalar numeric conversions](native-f64-conversions.md) cover checked fixed
arguments, returns, local initialization, assignment and mixed arithmetic.
Explicit `ToI64`/`ToF64` operations require the original intrinsic declaration
and a proved materialization boundary. Arithmetic arguments and isolated
consumers preserve that boundary rather than linking through CALL_END.
Conversion events have separate source receipts and x87 ownership; they do not
authorize arbitrary conversion flags.
Images using FISTTP additionally require runtime SSE3 support.

[Source-normalized identity events](native-numeric-normalization.md) cover a
bounded direct-return group for I64-to-I64, I64-to-F64 and F64-to-I64 values.
Deleted identities and surviving copy barriers
have different source phase histories and results; generic cast rules cannot
replace those histories.

## Comparisons

[Native F64 comparisons](native-float-comparisons.md) cover source-owned scalar
`==`, `!=`, `<`, `>=`, `>` and `<=` with I64 results and direct conditions.
Equality compares materialized words, ordered comparisons use source-selected
x87 predicates, and direct positive-zero comparisons have a separate signed-word
normalization rule. Linked arithmetic and implicit integer operands retain their
proved event order. Value results and direct conditions can differ on NaN.

## Current exclusions

Unproved conversion normalization, comparison chains, floating short circuit,
constant-only comparisons without a normalization proof, comparison results
converted to F64, arithmetic-wrapped comparison conditions, remainder, power,
shifts, compound updates, increment/decrement, other floating intrinsics,
variadic F64 arguments, `%f`, deeper F64 pointers, pointer arrays, closed
AOT/isolated F64 initializer preparation and F64 parameter defaults remain
unsupported. Mixed functions containing F64 also reject narrow integer frame
members unless their register or stack selection has an authenticated allocation
proof; `noreg` alone does not supply that proof. I64/U64 control variables do not
have this width-dependent ambiguity. A nested source
callback requiring an I64 result
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
