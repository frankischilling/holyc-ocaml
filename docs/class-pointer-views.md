# Class pointer views

Postfix casts between owned integer and class pointers retain the same object
bytes. The target class must have a complete, nonempty layout at the original
cast. IR, isolated native programs and retained native JIT tasks support these
views in both source modes where those storage paths are available.

```c
class Item { U8 text[3]; U16 value; };
I64 F() {
  U8 bytes[10];
  Item *p=bytes(Item*);
  p[1].value=42;
  return p[1].value;
}
F();
```

This returns 42. `Item` is five bytes, so the second value occupies bytes eight
and nine of the original ten-byte array. The cast does not read or initialize
any bytes. A member read checks its selected width; a store initializes only
that window. The [maintained example](../examples/class-pointer-views.hc) also
prints `AB` and checks a round trip through `U8 *`.

The cast selects the target's nominal identity at its original source position.
Function-local definitions update cast visibility when their declaration is
reached. Later same-name definitions cannot change earlier casts, field offsets
or pointer strides. Retained functions preserve those original selections.
The source position comes from the owning function's immutable expression tree;
an independently reconstructed occurrence cannot supply that position.

A class view retains the original allocation, initialization region, logical
offset and full extent. Indexing and arithmetic use the target class size,
including packed members, inheritance and padding. Primitive pointer views can
recover the same bytes, and another class view can select a different layout
over them. Reference copies and call arguments retain separate snapshots before
later operands rebind a pointer.

Owned primitive globals, statics and literal bytes can also supply class views.
Their existing storage and lifetime rules still apply; the view does not create
a persistent class declaration or a new allocation.

An interior cast can index backward within its original allocation. One-past
references can be cast or moved back; they cannot access a field outside that
allocation. A larger target class grants no extra storage. Every reached field
window must fit the original extent. Comparisons and differences retain the
existing object-identity checks.

Shared source fixtures check all nine supported integer and Bool field types,
packed and inherited fields, nested arrays, scalar-cell crossings, backing
prefixes, primitive round trips, argument snapshots, captured destinations and
local and retained redefinitions. Faults cover unknown references and bytes,
partial initialization, narrow roots, one-past stores and unrelated allocations.
Exact and one-below instruction, frame, native stack and image checks exercise
class casts. Raw IR controls reject missing, foreign and other-function class
proofs; native controls require the original sealed graph before allocation.
Class-cast IR retains both the selected layout proof and the original
parenthesis flag. Stride proofs and cast proofs have separate payload forms.

Numeric and null casts cannot manufacture owned references. Incomplete and
empty target layouts, deeper pointers, reference returns and persistent pointer
cells retain their execution boundaries. Whole class-value casts and general
aggregate copies remain open under
[#686](https://github.com/frankischilling/holyc-ocaml/issues/686). Full compiler
parity remains open under
[#682](https://github.com/frankischilling/holyc-ocaml/issues/682).
These tests use independent expected words and hosted execution, without a
TempleOS runtime capture or machine-code byte comparison. Initialization,
ownership and bounds faults describe hosted policy for invalid memory.

The source audit uses commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
Complete `Compiler/PrsExp.HC:960-1115`, `PrsUnaryModifier`, selects cast classes
through `PrsType`, changes the current class and emits `IC_HOLYC_TYPECAST`.
It also selects member offsets and array strides. Complete
`Compiler/PrsVar.HC:285-371` retains the selected class and pointer depth.
`Compiler/OptPass012.HC:87-110` transfers cast classes while retaining the
parenthesized case; complete `Compiler/OptLib.HC:9-15,508-525` follows backing
classes and selects raw access widths. Complete `Compiler/BackLib.HC:693-708`,
`ICDeref`, consumes the pointed width. The
[class pointer notes](class-pointers.md#templeos-source-evidence) describe the
complete arithmetic and update producers and consumers.
