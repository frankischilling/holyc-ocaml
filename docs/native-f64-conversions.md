# Native scalar numeric conversions

This implementation is audited against TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`. Numerical expectations in its tests
are deductions from that source and instruction semantics. These tests add no
TempleOS runtime captures.

The runnable example is [native-numeric-conversions.hc](../examples/native-numeric-conversions.hc).

## Supported boundaries

Original checked callable source can convert scalar integer/F64 values at fixed
provided arguments, returns, local initialization and ordinary assignment.
Assignments include proved global/static scalars, array elements, packed
members and owned indirect destinations. Mixed `+`, `-`, `*`, `/` and `%`, and
their supported compound updates, admit their original conversion events.
Integer results follow the existing destination storage and register-selection
rules; a register variable must not be assumed to behave like a narrow stack
store.

Explicit `noreg` scalar locals and closed primitive arrays can establish stack
storage even when the function uses globals or packed objects outside the general
allocation proof. The compiler checks the original qualifier, member order,
dimensions and frame geometry with one cumulative proof allowance. Unproved
automatic narrow-register choices still reject. This follows the `REG_NONE`
selection in `Compiler/PrsVar.HC:462-465,513-518` and its exclusion from scoring in
`Compiler/OptPass3.HC:29-42`.

Conversions are numeric operations, not HolyC suffix bit reinterpretation.
F64 values still travel through general-purpose registers and eight-byte call
slots. F64-valued call arguments, returns and stores materialize binary64.

The separately recognized `_intern` opcodes `IC_TO_F64` and `IC_TO_I64` retain
exact selected declaration, parameter, result, argument and scope identity.
Renaming the function does not change its opcode; matching a familiar function
name does not make an ordinary function intrinsic. The explicit builtin subset
requires the actual argument's numeric family to match the declared parameter
and a materialized result boundary. ToI64 accepts source-proved floating
arithmetic arguments, including supported nested links within the argument.
The terminal arithmetic result must materialize binary64 before conversion.
ToF64 results also feed isolated F64 arithmetic whose other operand is an
unconverted F64 literal or frame load. ToI64 results similarly feed isolated
integer arithmetic with an unconverted integer literal or frame load and no
result conversion. The original CALL_END result is a materialized scalar leaf,
never an extended-value producer. Canonical mixed
source validation includes only exactly sealed numeric intrinsic declarations;
the ordinary call-target query and exact register-score proof remain unchanged.
Cached consumer plans revalidate the canonical source and original frame header
even when the body has no implicit conversion flags.
Opposite-conversion cancellation, nested numeric intrinsics and arithmetic
siblings around explicit builtin results remain rejected.

## Event ordering and precision

Pinned `OptPass789A.HC:66-94,1045-1063` orders floating events as operand 2
conversion, operand 1 conversion, instruction, then result conversion.
`OptPass5.HC:30-76` migrates eligible MOV/REG conversions into the consuming
operand. Conversion of a computed integer remains its own result event.
`OptLib.HC:535-554` and `BackFA.HC:6-59` determine which adjacent events may keep
an extended x87 result. A conversion's presence is not permission to retain every
arithmetic child.

For example:

```c
I64 F(I64 n,F64 y) { return n+y; }
F(9007199254740993,-9007199254740992.0);
```

The runtime integer is loaded exactly by FILD, added in extended precision, and
converted directly with FISTTP. The result is integer `1`. A computed runtime
integer such as `(n+1)` has a distinct conversion-producer event and can retain
the same precision when the original source permits a link.

By contrast, `return 9007199254740993+y;` rounds the integer literal to binary64
before arithmetic, as `OptLib.HC:124-139` and `OptPass012.HC:70-84` require. Its
result for the same `y` is `0`. Integer constant expressions and negative
literal expressions also materialize before arithmetic rather than acquiring a
runtime extended conversion link.

Explicit conversion differs from implicit result conversion. With runtime
`a=9007199254740992.0` and `b=1.0`, `ToI64(a+b)` produces integer
`9007199254740992`, whereas an I64 function returning `a+b` directly produces
`9007199254740993`. `PrsExp.HC:543-586` surrounds the explicit intrinsic with
CALL_START/CALL_END. `OptPass3.HC:444-445` does not call CmpF1PushPop for TO_I64,
and `CInit.HC:44-45` does not classify it as an fpop consumer. Consequently the
argument lacks the cross-instruction hint required by `BackFA.HC:21-32`:
its ADD stores binary64, then `BackB.HC:277-281` invokes FLD/FISTTP at CN_INST.
Implicit return conversion instead receives RES_TO_INT at
`OptPass3.HC:249-256` and the same-instruction link in `BackFA.HC:40-42`.
Nested argument arithmetic retains its independently proved internal links:
`ToI64((a+b)-a)` therefore produces `1` before the final argument store.

For the isolated consumer `ToF64(n)+y`, runtime `n=9007199254740993` and
`y=-9007199254740992.0` produce `0`, unlike implicit `n+y` which produces `1`.
`CInit.HC:23` marks CALL_END as non-fpop; `OptLib.HC:542-553` therefore does not
request an extended link from it. The intrinsic FILD result is stored before
CALL_END supplies binary64 bits to the arithmetic operation. No normalized
alias maps CALL_END back to its argument or conversion event.

Runtime conversion uses **signed FILD64**, including U64 source words with the
high bit set (`BackFA.HC:123-166`). Unsigned immediate normalization is a
separate source optimizer rule, not a reason to add an unsigned correction to
runtime FILD. Source forms requiring unsupported immediate migration or
normalization continue to reject.

FISTTP64 truncates toward zero. Under the established masked x87 environment,
NaN, infinity and out-of-range inputs produce integer-indefinite
`0x8000000000000000`, not saturation or zero. The encoder uses the pinned
`OpCodes.DD:1161,1165` FILD/FISTTP forms. FISTTP requires SSE3: images record that
requirement, and runtime CPU checks reject them before execution or retention
when SSE3 is unavailable. Pure floating images retain their existing host
capability contract.

## Storage and compound conversion events

The source assignment flags in `Compiler/OptPass012.HC:866-873` select conversion
before a typed store. F64-to-integer assignment truncates before the destination
width is written; a later signed or unsigned load uses that stored width.
This applies to packed members and array elements as well as scalar frame,
global and static locations. Declaration preparation retains its separate
[completed-word copy rule](native-numeric-initializers.md).

For an F64 arithmetic compound destination, an eligible integer RHS read can
remain exact through FILD until the update stores binary64
(`Compiler/OptLib.HC:535-539`, `Compiler/OptPass789A.HC:65-71`). With stored
`value=-9007199254740992.0` and integer `delta=9007199254740993`, `value+=delta`
stores `1.0`. A later read of the destination begins at its binary64 boundary.

An integer destination follows `ICFCvt2` in `Compiler/BackFB.HC:1-224` or
`Compiler/BackFA.HC:549-605`: materialize the old integer as binary64, compute
and store the floating result, then truncate it for the integer destination.
Remainder also materializes an integer RHS before FPREM. Consequently
`I64 value=9007199254740993; value%=3.0;` stores `2`, and
`I16 value=-7; value%=2.75;` stores `-1` after truncating the remainder `-1.5`.

Bitwise and shift compounds use a different RHS rule. `&=`, `|=`, `^=`, `<<=`
and `>>=` truncate an F64 RHS to an integer before editing the destination's raw
word (`Compiler/OptPass012.HC:888-894`). Ordinary mixed floating operators first
promote their integer operand to F64. In particular, `x<<1` and `x<<=1` can use
different shift counts; see [raw-word operators](native-f64.md#remainder-updates-and-raw-word-operators).

## Proof and current exclusions

Conversion receipts are private, tied to original instruction/result identities,
ordered operands, flags, source body, frame, runtime context and complete graph.
The mixed scalar source proof re-lowers the checked body and compares canonical
records. It does not grant exact optimizer scores: the separate precise-score
proof still excludes conversion-bearing source.

Integer conversion producers needing identity elimination or literal
reassociation also reject, rather than acquiring the wrong conversion-event
position. Scalar suffix bitcasts are not authorized by conversion receipts.

A bounded direct-return identity group for I64→I64, I64→F64 and F64→I64 has a separate
[source-normalized event proof](native-numeric-normalization.md). Deleted
identities such as `n*1.0` return raw binary64 bits in the integer carrier;
surviving copies such as `n/1.0` materialize before numeric truncation. Other
converted identity contexts still reject. Treating these cases uniformly as
floating arithmetic would silently change precision.

[Native F64 comparisons](native-float-comparisons.md) also admit proved implicit
integer operand conversions. Ordered predicates can retain signed FILD precision;
bitwise equality materializes the converted word. The comparison's original
source event determines its link orientation, including conversions within the
comparison itself. General comparison-result conversion to F64, comparison
chains, floating short circuit, arithmetic-wrapped comparison conditions,
variadic conversion and other unproved normalization forms remain outside this
subset. Observed indirect arithmetic compound results and dynamic narrow
integer compound results with floating operands require additional proof of
the original result supplier. The [storage contract](native-f64-storage.md)
distinguishes these results from supported discarded updates and typed stores.

Boundary fixtures use short-mantissa scientific notation or exact bit patterns
where applicable. TempleOS's lexer accumulates decimal mantissa digits in a
signed machine word before scaling (`Lex.HC:550-579`), so a long spelling such
as `9223372036854775808.0` does not denote the intuitive mathematical value.
That lexical behavior is independent of FISTTP conversion behavior.
