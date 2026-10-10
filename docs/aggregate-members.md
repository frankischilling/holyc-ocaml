# Owned aggregate members

The interpreter and native executor read, assign and update integer members of
nonempty automatic class and union objects. The exact layout must complete
before the function declaration. Direct access, one-level owned class pointers,
nested aggregate members and multidimensional member arrays share the original
object's bytes. Automatic arrays of these classes and unions also retain their
selected element layout and dimensions:

```c
class Packed { U8 text[3]; U16 value; };
I64 Bump(Packed *p) { return ++p->value; }
I64 F() {
  Packed object;
  Packed *p=&object;
  object.value=40;
  Bump(p);
  return Bump(&object);
}
F();
```

Both `holyc run --target=ir` and `holyc run --target=host-jit` return 42 in JIT
and AOT mode. The maintained `examples/aggregate-members.hc` also fills the
packed text member and prints `AB` through array decay. Integer members support
Bool, I8, U8, I16, U16, I32, U32, I64 and U64. Loads and stores use the selected
field's width and signedness. Named and anonymous unions share overlapping
windows; explicit padding and backing classes retain their closed layouts.

`object.items[i][j].value` uses the selected member dimensions and aggregate
element size. An aggregate element remains an address for further member
access. Taking its address and dereferencing that address also preserves the
same field identity. Primitive member arrays and rows can decay to primitive
pointers. Owned class pointer locals and fixed parameters copy the existing
reference descriptor, so a callee updates the caller's object. A destination
is captured before its assignment RHS can rebind the pointer.

## Automatic aggregate arrays

`Item items[2][3]` allocates one byte object with six elements of the exact
earlier `Item` layout. `items[i][j].value` consumes the original dimensions;
each stride uses the selected class size, including packed fields and explicit
padding. That element size stays separate from the full allocation extent,
the one-byte storage cells and the eight-byte pointer slots.

An indexed element can supply a member address or an owned class pointer to a
callee. A root array or partially indexed row can decay to its first element.
`examples/aggregate-arrays.hc` prints `AB` and returns 42 after a callee updates
the selected element twice. Dynamic indices, nested member arrays and captured
assignment destinations use the same checked address path in both executors.
Explicit primitive views share the entire root array's extent and byte flags.
Bounds guard the containing allocation; they do not create separate row or
element allocations.

Only positive source extents with a representable product and a nonempty
earlier element layout receive storage. IR preparation checks every emitted
stride against the original frame dimensions. Native compilation preserves
dimension ownership and dependency checks before allocating storage and one
initialization flag per byte. The selected element size never comes from a
pointer slot or from dividing a containing object's extent.

## Selected field and storage ownership

Semantic typing retains the checked member base and the immutable member
lookup. Lowering authenticates the base's exact source expression, nominal
class identity, pointer depth, value category and consumed array rank. The
selected field supplies its type, offset and representable array strides.
Lowering emits the original `IC_IMM_I64` offset followed by `IC_ADD`, with an
opaque field proof attached to the latter instruction. A later same-name class
definition cannot replace an earlier function's field width or offset.

IR preparation validates that proof against the actual base pointer, target
pointer and constant offset. A missing proof, a proof from an independent
same-name class, or a changed offset fails before execution. Native compilation
requires the original sealed source graph and also checks each field projection.
Tests distinguish these guards: raw IR controls reach field-proof validation;
native mutation controls fail the whole graph seal before image allocation.

Projection changes the current view and byte offset while retaining the
original storage, initialization flags, extent and invocation lifetime. Native
projection copies the private reference descriptor. Each invocation starts
with unknown object bytes; a store initializes only its reached window. Reads
of untouched or partly initialized fields fail and retain earlier output.

Bounds use the containing object's original extent rather than a separate
allocation for each field. Intermediate addresses may be one past that object;
a reached read or write must fit its full access width. These initialization,
ownership and extent rules are hosted execution policy, not claims about
TempleOS behavior for invalid or uninitialized memory.

## Verification and remaining work

The shared fixtures have 58 IR and 58 native test groups. Independent expected
words and output cover nested fields, two-dimensional primitive and aggregate
member arrays, one- through three-dimensional root arrays, dynamic loop indices,
root and row decay, class pointer locals and parameters, captured destinations,
union overlap, signed views, padding, shadowing and all nine integer widths.
Faults cover unknown fields and pointers, partial union writes, fresh
activations and out-of-object windows. Exact and one-below controls cover
runtime frame bytes and instructions, plus both x86-64 ABIs' stack and encoded
image bytes. Root-array controls also reject borrowed frames, altered field
proofs and a forged element stride before IR execution. Native mutation checks
still exercise graph sealing. Native host checks execute fresh images. The
actual CLI runs 226 reports for IR and 453 when native execution is included,
including the original unused-local warning for an unused aggregate array.

Inheritance, retained JIT aggregate imports, function-local layouts,
zero-sized automatic objects, pointer arrays, aggregate array initializers, persistent
aggregate objects, whole-object values and copies, pointer and callback fields,
general casts to class pointers, generic class pointer indexing and arithmetic, pointer returns and
original named-local size/position consumers remain unfinished under
[issue #686](https://github.com/frankischilling/holyc-ocaml/issues/686).
Functions in this slice return supported integers or U0. The checks use the
pinned source and hosted execution without a TempleOS runtime capture; full
compiler parity remains under
[issue #682](https://github.com/frankischilling/holyc-ocaml/issues/682).

## TempleOS source evidence

All references use commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

- `Compiler/PrsExp.HC:967-1015`, `PrsUnaryModifier`, selects direct or pointer
  member access through `MemberFind`, emits the member offset and address
  addition, and retains the member's array dimension cursor.
- `Compiler/PrsExp.HC:1057-1100` emits array stride multiplication and address
  addition using the remaining dimensions and selected element class size.
- `Compiler/PrsExp.HC:97-125,151-163,200-210` selects dereferences and updates,
  removes a dereference for address-taking, and checks assignment destinations.
- `Compiler/PrsLib.HC:40-62` creates the adjacent class pointer records with
  eight-byte storage; `Compiler/PrsVar.HC:620-632` places fixed parameters in
  eight-byte slots. Hosted descriptors supply ownership independently of those
  original pointer words.
- `Compiler/PrsVar.HC:530-531,590-618,660-671` supplies automatic storage sizes,
  frame placement and packed or overlapping member offsets.
- `Compiler/OptLib.HC:9-15,509-525` follows class forwarding and selects raw
  pointed types; general class forwarding execution remains outside this slice.
- `Compiler/BackLib.HC:693-707`, `ICDeref`, and
  `Compiler/BackC.HC:159-204`, `ICAssign`, consume pointed load and store widths.
