# Native scalar pointer aliases

`run --target=host-jit` accepts one-level pointers to I8, U8, I16, U16, I32,
U32, I64 and U64 in automatic locals and named fixed parameters. Both JIT and
AOT source modes use this path. `examples/native-scalar-pointers.hc` returns 42
and combines a local alias with a static initializer and a scalar default.

```text
holyc run --target=host-jit --mode=jit examples/native-scalar-pointers.hc
holyc run --target=host-jit --mode=aot examples/native-scalar-pointers.hc
```

Address-of selects an existing checked scalar parameter, local, global or
static. Pointer copies, assignments, argument passing and `&*p` retain its exact
pointee type. Dereferences use the object's declared width and signedness.
Assignment and compound assignment return the full computed word; prefix updates
return the normalized new object value, and postfix updates return the old one.
The destination is evaluated before the RHS, but compound updates read the
object after RHS effects, matching the existing checked interpreter.

Taking an address does not read the object. A pointer local has its own
initialization flag; dereferencing it first requires an initialized reference.
Reading or updating the pointee separately requires an initialized object.
Writing through a reference marks that object initialized, so later direct and
indirect reads agree. Named scalar parameters start initialized. Pointer
parameter rebinding changes the callee's slot; writing its pointee changes the
caller's object. JIT/AOT global and static initial states remain as described in
[native globals](native-globals.md).

## Ownership and lifetime

Native preflight keeps frame/global address metadata separate from runtime
reference values. Only an exact checked scalar slot can materialize a reference;
an integer, guessed displacement, reconstructed symbol or foreign function frame
cannot supply an address. Each materialization reserves a private 16-byte
frame descriptor containing the object's address and its initialization flag
address. A zero flag address denotes an already initialized scalar parameter.
The descriptor never becomes an integer result. Discarding a top-level reference
clears the numeric result, as a U0 expression does.

The accepted language prevents references from escaping their owners. Pointer
returns, persistent pointer storage, pointer-to-pointer objects, integer/null
casts, pointer arithmetic and indirect calls all reject before native entry.
A reference can therefore travel only through the current activation's local
slots or down its synchronous call chain. Every recursive activation has a
separate frame and descriptor region. A callee's local reference cannot survive
its return; no generation counter or host-address lookup is needed for this
restricted lifetime proof. Broadening those escape paths will require a new
lifetime mechanism before admission.

Bounds are checked before emission: an address names one complete scalar range,
and every access uses that range's exact width with zero displacement. This
subset has no source operation that changes the reference's offset. Passing a
static reference authorizes access to that object through the reference; it does
not authorize the callee to materialize a different function's static symbol.
Original body, frame, symbol, type, call and preparation ownership checks still
apply to the entire bundle, including unused functions.

## Resource and API boundaries

Descriptors occupy compiler-private frame bytes, separately from semantic
object storage. They count toward per-function and simultaneous physical-stack
limits. Their instructions belong to the original `IC_ADDR` step; descriptor
setup adds no semantic-frame, global-byte or interpreter-step charge. Existing
code, instruction, call-depth, preparation and arena limits remain in force.
Repeated address evaluation reuses that instruction's descriptor for the same
owned object; `&*p` forwards its reference without allocating another descriptor.

The public execution bridge accepts a sealed program and limits. It has no
pointer-argument channel, and its result remains an optional I64/U64 word.
Every invocation gets fresh stack state and a restored arena image. Checked
faults unwind through the existing generated frames before those resources are
released. This is an in-process native executor, with the limitations documented
in [SECURITY.md](../SECURITY.md).

Persistent pointers, pointer returns and comparisons, null/integer conversions,
arithmetic, deeper indirection, arrays, aggregates, foreign calls and the full
HolyC ABI remain unfinished native work. This feature does not produce objects
or BIN files, establish loader acceptance, or complete the compiler.

## Evidence and verification

The pinned reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/PrsExp.HC:151-161` supplies address/dereference type changes and
address cancellation; `Compiler/BackLib.HC:693-707` selects the pointed-to load
width; `Compiler/BackC.HC:159-204` supplies indirect assignment and its result.
LEA uses `Compiler/OpCodes.DD:830-833`; indirect scalar loads and stores use the
already audited MOV, MOVSX, MOVZX and MOVSXD forms. Descriptors, initialization
faults and resource limits are hosted execution policy. No new TempleOS oracle
capture is claimed.

Compile-only tests check literal encoding bytes, both ABI images, exact frame
limits, altered producers, foreign/reconstructed ownership and rejected escapes.
The explicit native API and CLI suites cover both source modes, all scalar
widths, alias effects, recursion, pointer/pointee initialization faults, resource
boundaries and fresh repeated images. Expected results are checked independently
and against fresh interpreter executions of the source and checked batch IR.
