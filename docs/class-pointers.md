# Owned class pointer operations

[Class value returns](class-value-returns.md) carry qualified integer
register words while preserving nominal declaration types. Object reads
use the selected scalar prefix; register results retain the full word.

[Default class and union values](default-aggregate-values.md) use the signed
eight-byte `RT_PTR` prefix while retaining nominal layouts and pointer strides.

[Integer-backed aggregate values](backed-aggregate-values.md) use the original
forwarded scalar width for whole reads, assignments and updates. Class
extents, member identities and pointer strides remain separate. General casts,
class defaults, callback class parameters and persistent objects still need
their own implementation.

One-level pointers to completed earlier, nonempty class and union layouts
support indexing, addition, subtraction, difference, comparisons, `++`, `--`,
`+=` and `-=` in isolated IR and native programs. Locals and fixed parameters
retain the original automatic object's bytes, extent, initialization state and
invocation lifetime. Both JIT and AOT modes use the same selected layout.

```c
class Box { U8 tag; U16 value; };
I64 Sum(Box *p) {
  I64 n=(p++)->value;
  return n+p->value;
}
I64 F() {
  Box a[2];
  a[0].value=20;
  a[1].value=22;
  return Sum(a);
}
F();
```

The result is 42 on both executors. `examples/class-pointers.hc` also indexes
a text member through a class pointer and prints `AB` before returning 42.

## Selected pointee size

`Box` above has a packed size of three bytes. `p[1]`, `p+1` and `p++` advance
three bytes, while `q-p` divides the byte difference by three. The stride comes
from the exact nominal class layout selected before the function declaration.
It stays separate from the eight-byte pointer slot, the one-byte physical
storage cells and the containing object's full extent. Unions, explicit
padding and backing classes retain their selected sizes. A later same-name
class or forward completion cannot supply a replacement size.

Indexing an array expression consumes its remaining dimensions. Arithmetic
after array decay uses the pointee class size. Thus, for `Box a[2][3]`, `a[1]`
selects the second row, while `Box *p=a+1` points to `a[0][1]`. This follows
the original HolyC producers described below. Primitive multidimensional
arrays use the same distinction.

Semantic results retain an opaque selected-layout proof. Lowering emits the
stride immediate and multiplication, then attaches that proof to the address
addition or subtraction. IR preparation checks its nominal pointer type,
stride and original function item against the actual frame. Missing, foreign,
altered or other-function proofs fail before execution. Native compilation
requires the sealed source graph and validates the selected stride before
image creation. Native mutation controls fail graph sealing; they do not
independently demonstrate the later stride check.

## Updates and reached faults

Prefix updates return the new pointer. Postfix updates return a snapshot of
the old descriptor, so `(p++)->value` reads the original element even after
the pointer binding changes. Callee parameter updates change the callee's
binding while preserving access to the caller's storage.

Compound updates capture the destination before evaluating the right-hand
side, then read its current pointer after that evaluation. For example,
`p+=((p=&a[1])==&a[1])` advances from `a[1]` to `a[2]`, including when the
right-hand side initializes an unknown pointer. Lowering expands updates into
checked load, scale, address and store instructions. It does not reproduce
TempleOS's machine instruction stream. Primitive integer pointer locals and
parameters now use that same update path.

Direct primitive postfix stores follow the original `IC_ASSIGN_PP/MM` rewrite:
`*p++ = value` reads the current pointer after the RHS, stores the value, then
advances the binding. For `a[0]=20`, `a[1]=22` and `p=a`, `*p++=*p` leaves
`a[0]` at 20 and advances `p` to `a[1]`. A RHS that rebinds or initializes `p`
supplies the current pointer for that store. Ordinary member destinations such
as `(p++)->value` retain their earlier captured address.

Every derived reference keeps the containing allocation's bounds. One-past
addresses can be compared; a reached member read or write must fit its full
width. Unknown bytes and pointer bindings fault with `HCIRVM0012`, out-of-object
addresses with `HCIRVM0019`, and address scaling overflow with `HCIRVM0020`.
Difference and ordering require a shared owned object and otherwise fault
with `HCIRVM0018`. Faults retain reached output. These ownership, initialization
and extent guards are hosted policy, not TempleOS invalid-memory claims.

## Verification and remaining work

The pointer fixtures contribute forty value groups to the shared member suite,
which now has 128 IR and 128 native groups. Independent expected words and
output cover parameter and local
copies, negative in-range indices, pointer loops, postfix snapshots, RHS
rebinding, delayed primitive postfix stores, nested arrays, union overlap,
padding, class shadowing and all nine
integer widths. Raw IR controls reject missing, foreign, altered and
other-function pointee proofs before storage. Exact and one-below controls
cover frame bytes, executed instructions and both native ABIs' stack and
encoded image bytes. Fresh native image executions retain the same results.
The maintained CLI checks run 834 IR reports and 1669 including native execution.

Completed earlier [inherited layouts](inherited-aggregates.md) also use these
selected strides. Partial or out-of-compilation bases, function-local layouts,
retained JIT aggregate imports, zero-sized objects, persistent aggregate
storage, pointer arrays, aggregate initializers, whole-value postfix casts,
class defaults and callback parameters and general aggregate copies, pointer
and callback fields, general class pointer casts, pointer returns and deeper
indirection remain separate work under
[#686](https://github.com/frankischilling/holyc-ocaml/issues/686),
[#687](https://github.com/frankischilling/holyc-ocaml/issues/687),
[#699](https://github.com/frankischilling/holyc-ocaml/issues/699) and
[#700](https://github.com/frankischilling/holyc-ocaml/issues/700). Functions
here return supported integers or U0. These source-derived hosted checks
include no TempleOS runtime capture. Full compiler parity remains open under
[#682](https://github.com/frankischilling/holyc-ocaml/issues/682).

## TempleOS source evidence

All references use commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
The complete producers and selected-size consumers below supply the evidence
for this implementation:

- `Compiler/PrsExp.HC:15-63`, `PrsAddOp`, emits pointee scaling for addition,
  subtraction and compound updates, and divides pointer differences by the
  selected class size.
- `Compiler/PrsExp.HC:960-1115`, `PrsUnaryModifier`, handles postfix modifiers;
  its indexing branch at `1069-1100` distinguishes a remaining array dimension
  cursor from a generic pointer's selected class size.
- `Compiler/OptLib.HC:484-507`, `OptFixSizeOf`, replaces selected-size producers
  with immediates and handles multiplication by one. `509-525` selects raw
  pointer and pointed-to classes.
- `Compiler/BackB.HC:304-383`, `ICPreIncDec` and `ICPostIncDec`, consume the
  selected pointee size and preserve their different result/update order.
- `Compiler/BackA.HC:56-166`, `ICAddSubEctImm`, consumes the signed immediate
  through register, memory and stack forms, including unit increments.
- `Compiler/BackA.HC:442-571`, `ICAddSubEctEqu`, consumes compound operations
  after their RHS and retains the stored result.
- `Compiler/OptPass012.HC:865-889` rewrites direct postfix assignment
  destinations. `Compiler/BackB.HC:384-467`, `ICDerefPostIncDec` and
  `ICAssignPostIncDec`, consume fused postfix reads and stores with their
  selected sizes and result/update order.
- `Compiler/PrsExp.HC:97-125,151-163,200-210` supplies prefix/postfix update,
  dereference, address-taking and assignment destination checks. Pointed member
  loads and stores retain the [aggregate member evidence](aggregate-members.md#templeos-source-evidence).
