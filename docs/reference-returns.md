# Owned reference returns

Functions can return one-level pointers to supported integer, Bool and class
storage through IR, native programs and native source tasks in JIT and AOT
modes. Named calls, callback cells and callback fields retain the declaration's
original return type. Direct caller-pop and callee-pop calls use the same
descriptor transport.

The [example](../examples/reference-returns.hc) returns a pointer to the second
packed class element in the caller's array. It prints `AB` and returns 42:

```sh
holyc run --target=ir examples/reference-returns.hc
holyc run --target=host-jit examples/reference-returns.hc
holyc run --target=host-jit-task examples/reference-returns.hc
```

A return preserves the original allocation, byte extent, logical offset,
initialization state and activation identity. Its declared type selects the
subsequent view without enlarging the root. Caller-owned objects, global and
static storage and string literals remain usable after the callee returns.
Interior and one-past addresses retain their original coordinates. A one-past
value can move back within the root; reading bytes there reports `HCIRVM0019`.
Returning an address does not read its pointee bytes. A later read still checks
every byte and reports `HCIRVM0012` for unknown storage.

Returning a callee-local address does not extend that activation. Capturing,
copying, passing or accessing the expired descriptor reports `HCIRVM0018`.
The interpreter and native executor preserve the same reached effects before
that check. In particular, an invalid assignment faults before later output,
and argument effects follow the original call evaluation order.

Native returns use a private five-word context buffer. `IC_RETURN_VAL` copies
the checked descriptor there before retiring the callee's frame. The caller
copies it into its own checked snapshot at `IC_CALL_END`, before another call
can replace the buffer. Nested calls and recursion therefore keep independent
returned values. This extends the private hosted ABI; it is not an exported
TempleOS or host C ABI. Native descriptor snapshots count toward frame and
physical stack limits; the context buffer belongs to the host wrapper.

The source audit uses TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`. Complete `PrsStmt` at
`Compiler/PrsStmt.HC:904-1223` evaluates the return expression, retains the
function's return class in `IC_RETURN_VAL` and transfers to the leave label.
Complete `PrsFunCall` at `Compiler/PrsExp.HC:383-591` carries that class through
calls and cleanup. `OptPass789A.HC:779-783` moves the return value to RAX through
`ICMov`; the complete move consumer is `BackLib.HC:445-659`.

Shared fixtures cover public and intrinsic integer pointees, packed and
inherited classes, root/member arrays, globals, statics, literals, conversions,
callbacks, nested calls, recursion, initialization and expiration. Exact and
one-below semantic frame, work, physical frame and image limits accompany the
values. Graph seals and foreign frame controls retain source ownership.

Numeric/null pointer construction, extra indirection, floating and U0 pointee
execution, code-valued returns and wider ABI families keep their existing
boundaries. Member metadata consumers, other aggregate backings and general
copies remain under #686. These checks add hosted execution evidence, without
an original TempleOS runtime capture or emitted machine-byte comparison.
Lifetime, initialization and invalid-memory faults remain hosted policy.
