# Scalar pointer aliases

[Array indexing](integer-arrays.md) extends references with full declared-object
extents and byte offsets. Any-rank ordinary arrays can supply an element
pointer; one-past pointers can be copied but cannot be dereferenced. Explicit
address-of a remaining-rank array adds a deeper pointer layer and is excluded.

`holyc run --target=ir examples/integer-pointers.hc` passes the address of a
caller's local to `Set`. The callee adds two through its pointer parameter;
the caller reads its original local and returns 42. Both JIT and AOT modes use
43 runtime instructions, zero constant-preparation instructions, 16 active
frame bytes and call depth two. Reducing any of those runtime limits by one
fails with no result report.

```c
I64 Set(I64 *p) { *p+=2; return *p; }
I64 F() { I64 n=40; Set(&n); return n; }
F();
```

One-level `I64*` and `U64*` automatic locals and named fixed parameters hold
references to existing scalar parameters, automatic locals, module globals
and static locals. Address-of does not read the object: `I64 n; I64 *p=&n;
*p=42;` initializes n successfully. Pointer copies, exact-type assignments,
`&*p`, dereferences and all scalar compound/prefix/postfix updates compose with
branches, loops, nested calls and recursion. Reassigning a pointer parameter
changes that parameter's slot; writes through it change its current pointee.
Numeric arguments retain their existing conversions, and all arguments still
evaluate right to left and bind to their original formal positions.

## Storage identity and checks

The expression planner validates the retained prefix operand and exact type.
An indirect assignment computes its destination reference before the RHS;
the update reads that object's word after RHS effects. Thus with n=20, m=22
and p=&n, `*p+=*(p=&m)` stores 42 in n. Unsigned pointees retain all 64 bits and
their update signedness. Pointer arithmetic has no implementation in this gate.

Canonical `IC_RBP`/offset/`IC_ADD` and symbol address instructions keep their
metadata role. `IC_ADDR` materializes an internal reference to the actual storage
instance and slot, or forwards the same reference for `&*p`. A callee cannot
reinterpret a caller's frame index as its own. Recursive activations with equal
offsets own different cells. Pointer parameter slots have a canonical address
with two pointer layers; source pointer-to-pointer values remain unsupported.

Preflight checks complete graphs, exact slot/owner/type evidence and pushed
parameter positions before any effects. `IC_ADDR` itself checks canonical
static metadata against the declaring frame or exact initializer region, even
when the metadata was produced earlier inside that region. A materialized
reference may be explicitly passed to a callee, which gains access to that
object without gaining authority to construct other static addresses.
Initializer calls retain declaration phase, JIT publication checks and faults.
They have no containing invocation frame.

Returned frames are invalidated. Each hosted execution allocates fresh frame
and persistent cells, and references never become public integer results.
Uninitialized pointer slots and reached uninitialized pointees use HCIRVM0012;
invalid internal references use HCIRVM0018 with instruction provenance. These
are hosted diagnostics, not claims about native TempleOS invalid-pointer behavior.
`execute_function` accepts integer argument bits only and rejects pointer
parameters; checked calls through `execute_program` supply actual references.

## Pinned source and remaining work

All source locations refer to `c26482bb6ad3f80106d28504ec5db3c6a360732c`:

- `Compiler/PrsExp.HC:151-161` and `Compiler/OptPass789A.HC:461-464` retain
  address-of/dereference cancellation and forward the address.
- `Compiler/PrsExp.HC:776-784,800-802,877-896` supplies static, frame and
  JIT/AOT global address forms.
- `Compiler/BackLib.HC:693-707` loads through an address;
  `Compiler/BackC.HC:159-204` stores through it.
- `Compiler/OptPass3.HC:519-543` prevents ordinary register promotion when
  the object's address escapes.
- `Compiler/PrsExp.HC:201-208` and `Compiler/BackA.HC:555-566` retain the
  destination until the RHS has executed, then read and update it.
- `Compiler/PrsExp.HC:40-45` requires pointer arithmetic scaling;
  `Compiler/CInit.HC:49` marks `IC_ADDR` as a constant barrier.

Pointer returns and escapes, pointer-valued global/static initial images,
integer/null address conversions, casts, arithmetic, deeper pointers and other
pointee shapes remain explicit boundaries. General memory/runtime output,
stateful compilation and `#exe`, optimizer parity, native backends, BIN/loader
acceptance and bootstrap remain requirements of the full compiler. The tests
provide hosted source and malformed-IR evidence; no new native capture is claimed.
