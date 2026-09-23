# Native scalar pointer aliases

`run --target=host-jit` accepts one-level pointers to I8, U8, I16, U16, I32,
U32, I64 and U64 in automatic locals and named fixed parameters. Both JIT and
AOT source modes use this path. `examples/native-scalar-pointers.hc` returns 42
and combines a local alias with a static initializer and a scalar default.

```text
holyc run --target=host-jit --mode=jit examples/native-scalar-pointers.hc
holyc run --target=host-jit --mode=aot examples/native-scalar-pointers.hc
```

Address-of selects an existing checked scalar parameter, local, global, static
or automatic array element. Pointer copies, assignments, argument passing and
`&*p` retain its exact pointee type and original object extent. Dereferences use
the object's declared width and signedness.
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
reference values. Only an exact checked object can materialize a reference;
an integer, guessed displacement, reconstructed symbol or foreign function frame
cannot supply an address. A private canonical table reserves one 32-byte record
for each element and one for the object's one-past address. Each record contains
the root data address, root initialization-flag address, signed logical byte
offset and original object extent. A zero flag address denotes an already
initialized scalar parameter. The record never becomes an integer result.
Discarding a top-level reference clears the numeric result, as a U0 expression
does.

Each record always identifies the same object and offset. Repeating an address
instruction can select another record without modifying earlier aliases.
Pointer loads, assignments and call staging copy the selected record address.
Arguments evaluate right to left, so `Use(p=&b,p)` preserves the right
argument's original value even though the left argument rebinds `p`.
Assignment-expression results and indexed bases
captured before later effects have the same protection.

The accepted language prevents references from escaping their owners. Pointer
returns, persistent pointer storage, pointer-to-pointer objects, integer/null
casts, general pointer arithmetic and indirect calls all reject before native entry.
A reference can therefore travel only through the current activation's local
slots or down its synchronous call chain. Every recursive activation has a
separate frame and descriptor region. A callee's local reference cannot survive
its return; no generation counter or host-address lookup is needed for this
restricted lifetime proof. Broadening those escape paths will require a new
lifetime mechanism before admission.

Checked indexing retains the original object range and validates signed scaling
and addition before forming a host address. Intermediate multidimensional
offsets remain internal until final materialization or access. Materialization
accepts aligned one-past addresses; reads and writes require an actual element.
Negative indexing from an interior pointer can reach earlier elements, but a
scalar reference cannot reach an adjacent local. Passing a static reference
authorizes access to that object through the reference; it does not authorize
the callee to materialize a different function's static symbol. Original body,
frame, symbol, type, call and preparation ownership checks still apply to the
entire bundle, including unused functions.

## Resource and API boundaries

Canonical records occupy compiler-private frame bytes, separately from semantic
object storage. The complete table is charged before allocation and counts
toward per-function and simultaneous physical-stack limits. Its instructions
belong to the original IR step; record setup adds no semantic-frame, global-byte
or interpreter-step charge. Existing code, instruction, call-depth, preparation
and arena limits remain in force. Repeated evaluation reuses canonical records
without allocating per loop iteration. `&*p` forwards its checked reference.

The public execution bridge accepts a sealed program and limits. It has no
pointer-argument channel, and its result remains an optional I64/U64 word.
Every invocation gets fresh stack state and a restored arena image. Checked
faults unwind through the existing generated frames before those resources are
released. This is an in-process native executor, with the limitations documented
in [SECURITY.md](../SECURITY.md).

Automatic scalar arrays use the same references; see
[native arrays](native-arrays.md). Global/static arrays and mutable byte literals
use the [persistent storage](native-persistent-storage.md) path. Persistent pointer
variables, pointer returns and
comparisons, null/integer conversions, general pointer arithmetic, deeper
indirection, pointer-valued arrays, aggregates, foreign calls and
the full HolyC ABI remain unfinished native work. This feature does not produce
objects or BIN files, establish loader acceptance, or complete the compiler.

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
