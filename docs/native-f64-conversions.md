# Native scalar numeric conversions

This implementation is audited against TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`. Numerical expectations in its tests
are deductions from that source and instruction semantics, **not fresh TempleOS
runtime captures**.

The runnable example is [native-numeric-conversions.hc](../examples/native-numeric-conversions.hc).

## Supported boundaries

Original checked callable source can convert scalar integer/F64 values at fixed
provided arguments, returns, local initialization and ordinary assignment.
Mixed `+`, `-`, `*` and `/` admit proved conversion events. Integer results still
follow the existing destination storage and register-selection rules; a register
variable must not be assumed to behave like a narrow stack store.

Conversions are numeric operations, not HolyC suffix bit reinterpretation.
F64 values still travel through general-purpose registers and eight-byte call
slots. F64-valued call arguments, returns and stores materialize binary64.

The separately recognized `_intern` opcodes `IC_TO_F64` and `IC_TO_I64` retain
exact selected declaration, parameter, result, argument and scope identity.
Renaming the function does not change its opcode; matching a familiar function
name does not make an ordinary function intrinsic. The initial explicit builtin
subset requires the actual argument's numeric family to match the declared
parameter and a materialized result boundary. Opposite-conversion cancellation
and explicit builtin calls with arithmetic arguments or arithmetic consumers
are rejected.

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

## Proof and current exclusions

Conversion receipts are private, tied to original instruction/result identities,
ordered operands, flags, source body, frame, runtime context and complete graph.
The mixed scalar source proof re-lowers the checked body and compares canonical
records. It does not grant exact optimizer scores: the separate precise-score
proof still excludes conversion-bearing source.

Integer conversion producers needing identity elimination or literal
reassociation also reject, rather than acquiring the wrong conversion-event
position. Scalar suffix bitcasts are not authorized by conversion receipts.

Converted identity expressions such as `n+0.0`, `n*1.0` and `n/1.0` currently
reject. Some identities disappear in the optimizer and merge conversion flags;
others introduce a bit-copy materialization barrier. Treating them uniformly as
floating arithmetic would silently change precision. Float comparisons,
conversion-bearing aggregate/indirect storage, variadic conversion and other
unproved normalization forms are outside this subset.

Boundary fixtures use short-mantissa scientific notation or exact bit patterns
where applicable. TempleOS's lexer accumulates decimal mantissa digits in a
signed machine word before scaling (`Lex.HC:550-579`), so a long spelling such
as `9223372036854775808.0` does not denote the intuitive mathematical value.
That lexical behavior is independent of FISTTP conversion behavior.
