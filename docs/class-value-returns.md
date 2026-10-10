# Class value returns

Source-defined functions return integer words with the selected class or union
view in isolated IR and native JIT/AOT execution. The return register carries a
single word. A class return does not allocate or copy an aggregate object.

```c
U16 class Packet { U16 low; U8 guard; };
Packet Make() { return 0x07002a; }
Packet Read(Packet o) { return o; }
I64 Take(Packet o) {
  if (o.guard!=7 || Read(o)!=42) return -1;
  return o;
}
Take(Make());
```

This returns 42. `Make()` returns the full register word, including the guard
byte above its `U16` backing. The provided word initializes all eight bytes of
`Take`'s class parameter. `Read(o)` reads that parameter's two-byte scalar
prefix and returns 42. The same example is available in
`examples/class-value-returns.hc`.

All nine integer backings and ordinary signed `RT_PTR` class views are
supported. Integer register results retain their bits above a narrow backing;
only object reads select and extend the memory prefix. Assignment expressions
also retain the produced word while storing only the destination prefix.
Receiving a returned word in another class object writes only that object's
scalar prefix and preserves its other bytes.

The selected backing determines the result's signedness for later computation.
An ordinary derived class retains its default raw type independently of its
base's backing. Explicit backing chains retain their original selections after
same-name declarations. The layout adapter computes nominal class size without
resolving a named backing as a primitive; that backing contributes no member
bytes. Empty class returns still carry a word and preserve the original
`Function should NOT return val` warning. Their nominal size remains zero.

Returned words work in scalar and class arguments, variadic tails, implicit
output, grouping, unary plus, arithmetic, branches, nested calls and recursion.
The original evaluation order, cleanup policy and execution limits apply.
Unknown object bytes and reads beyond the owned root fail before a call can
complete its return. These memory checks describe the hosted execution model.

Nominal function and call return types retain their original selected class.
A bound definition separately acquires its integer return view from the exact
source namespace, header and frame. Direct call metadata keeps the nominal
declaration and the qualified scalar result view. Lowering uses that view for
the return and call-result producers; signature matching uses the nominal type.
Raw bodies cannot acquire the class return convention. A returned register word
grants no addressable class extent, callback ownership or executable target.

The shared cases check register and object returns for every integer backing,
cross-class argument transport, narrow stores, selected backing chains,
empty-class warnings, output, recursion, both cleanup policies and exact quotas.
Ownership checks reject raw bodies before execution; native checks cover both
host ABIs and fresh images. Expected words follow the pinned source audit.
These tests contain no TempleOS runtime capture or machine-byte comparison.

Prepared class defaults, callback class signatures, F64/pointer/callback
backings, zero-size class pointer operations, persistent class objects, native
task classes, retained aggregate imports and general aggregate copies remain
open. Complete class and ABI acceptance remain under
[#686](https://github.com/frankischilling/holyc-ocaml/issues/686) and
[#702](https://github.com/frankischilling/holyc-ocaml/issues/702); full compiler
and release acceptance remain under
[#682](https://github.com/frankischilling/holyc-ocaml/issues/682).

The reference is TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`. The return branch in
`Compiler/PrsStmt.HC:1089-1122` retains the declared class, checks nominal size
for warnings and emits `IC_RETURN_VAL` followed by the leave jump. The return
cases in `Compiler/OptPass3.HC:249-260` convert only between F64 and integer
classes. `Compiler/OptPass4.HC:415-435` keeps the selected raw result class.
`Compiler/OptPass789A.HC:779-786` moves the returned operand into signed-word
RAX. Complete `ICMov` at `Compiler/BackLib.HC:445-659` preserves register words
and selects width and signedness for memory reads. Complete `PrsFunCall` at
`Compiler/PrsExp.HC:383-591` retains the original return class and word call
convention. The class views follow the audits in
[integer-backed values](backed-aggregate-values.md),
[default class values](default-aggregate-values.md) and
[class parameters](class-value-parameters.md).
