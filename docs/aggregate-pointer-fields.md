# Owned data pointer fields

[Persistent class globals and statics](persistent-aggregates.md) also supply
owned byte roots for IR and native pointer fields. Their shadows retain the target's
lifetime; a persistent field cannot keep a retired activation alive.
[Aggregate initializer leaves](aggregate-initializers.md#owned-data-pointer-leaves)
can now initialize these fields from checked data references and strings.

The IR, `host-jit` and `host-jit-task` targets read, assign and update integer,
Bool and class pointers stored in class and union fields. Direct access,
inherited fields, nested objects and multidimensional pointer member arrays
retain their selected types and layouts in JIT and AOT modes.

```c
class Item { U16 value; };
class Box { Item *item; };
I64 Bump(Item *p) { return ++p->value; }
I64 F() {
  Box object;
  Item item;
  item.value=40;
  object.item=&item;
  Bump(object.item);
  return Bump(object.item);
}
F();
```

This returns 42 in JIT and AOT mode. The maintained
[`aggregate-pointer-fields.hc` example](../examples/aggregate-pointer-fields.hc)
returns the address of a class pointer array element from `ItemSlot`. It stores
through that alias, prints `AB` and returns 42. Retained JIT calls
preserve the member and pointee identities selected at their original source
occurrences, including after a same-name class replacement. Layouts of
[function-local types](function-local-aggregate-objects.md) require their
original allocation and expression receipts.

## Storage and lifetime

A typed pointer store copies the existing owned reference descriptor and writes
its eight address bytes into the containing object's storage. The descriptor
retains its original allocation, extent, offset, element width and invocation
lifetime. Loading a field does not extend the target's lifetime. A callee can
save a caller's address in the caller's object, but dereferencing a saved callee
local after return fails with `HCIRVM0018`. Retired frames release their byte
arrays and pointer shadows; an expired descriptor retains its dead storage
identity for the lifetime check.

Native descriptors carry a process-unique activation identity. Original object
bytes contain the address; a separate native shadow retains the descriptor and
selected pointee identity. Returning from a function removes its own field
shadows and retires its activation. Caller and persistent fields can retain a
descriptor for that expired activation, but later accesses fail before reading
its old stack storage. Reusing the same stack address cannot revive it.

Source-task fragments share persistent shadows through their original arena.
Scalar stores, atomic updates, bit operations, swaps and compiler byte copies
invalidate overlapping ownership. Native shadow entries and their bucket index
have a 64 MiB payload limit and fail with `HCIRVM0016` before writing the field.
These descriptors and shadows are private execution metadata; source queries
still report eight-byte pointer storage.

Taking `&object.p` needs no initialized bytes in `p`. Dereferencing the
result selects the original pointer cell, so a typed store through the alias
initializes the field. A field can hold an alias to another pointer field.
Fields declared with up to three stars can participate in expressions with
four pointer layers. Those aliases can flow through automatic locals and
fixed named function parameters or returns when their bindings fit the checked
address representation.

Arithmetic over a pointer to a pointer cell uses an eight-byte stride. The
hosted difference and ordering checks require a shared live allocation.
Lowering captures an assignment destination before evaluating the RHS, so a
call that rebinds the alias cannot move an already selected write.

Object bytes and pointer ownership are recorded separately. The interpreter
serializes a disjoint virtual address token; it does not expose a host machine
address. Indexing within an allocation advances those bytes by the selected
pointee width. Integer and byte views can observe initialized pointer bytes,
including through a union or the default class prefix. Reading or restoring
those numeric bytes does not grant pointer ownership.
Native execution stores its actual data address in the object bytes while
retaining ownership separately. Neither executor can rebuild an owned pointer
by copying numeric bytes.

Any scalar store overlapping a pointer field removes that field's ownership,
even when the stored bits are unchanged. A later typed pointer store restores
ownership from its owned RHS. Disjoint stores preserve other fields. Same-type
union pointer fields can share one value. A different pointer depth or an
incompatible nominal type fails the ownership check. Word transport through
a class parameter copies the scalar prefix.

Reads first require every address byte to be initialized. Unknown or partly
initialized fields fail with `HCIRVM0012`. Fully initialized bytes without a
matching owned pointer fail with `HCIRVM0018`. Bounds checks cover the complete
eight-byte field window and the target's original extent. These checks describe
the hosted execution policy for invalid memory, not TempleOS runtime behavior.

## Evidence and limits

The IR fixtures check independent words and output for the supported integer
and Bool pointee types, class pointers, arrays, inheritance, self pointers,
field copies, caller storage, captured destinations and pointer updates. Faults
cover callee-local escape, full and partial overlapping writes, restored numeric
bytes, nominal type mismatches, unknown fields and one-past accesses. Rebuilt
owned IR executes; foreign field proofs, missing proofs, altered offsets and
altered array strides fail before execution. Exact and one-below frame and
instruction budgets cover the pointer field path. Native checks also cover
both host ABIs, original graph seals, exact stack and encoded-image limits,
retained source tasks and reached output before lifetime and ownership faults.

For the maintained example in a JIT source task, allow the code emitted across
its declaration and command fragments:

```sh
holyc run --target=host-jit-task --code-byte-limit=524288 examples/aggregate-pointer-fields.hc
```

The native targets also support F64 field aliases and indirect F64 updates.
The integer interpreter still excludes F64 pointee execution. U0 pointers,
numeric pointer construction, general class copies and automatic root arrays
of data pointers remain open. Ordinary pointer locals and parameters use
private reference slots, so taking the address of those slots remains gated.
Track wider pointer support under [#687](https://github.com/frankischilling/holyc-ocaml/issues/687)
and [#699](https://github.com/frankischilling/holyc-ocaml/issues/699), and full
compiler parity under [#682](https://github.com/frankischilling/holyc-ocaml/issues/682).

Callback members use their own [executable ownership path](callback-fields.md).
[Owned reference returns](reference-returns.md) can supply or consume pointer
fields without changing the target's original extent or lifetime.

The source audit uses TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`:

- `Compiler/PrsLib.HC:40-60`, `PrsClassNew`, creates adjacent pointer records
  with `RT_PTR` and eight-byte storage.
- `Compiler/PrsExp.HC:15-63`, `PrsAddOp`, scales pointer arithmetic and compound
  updates by the pointed class size and divides pointer differences by that
  size. `Compiler/OptLib.HC:484-505`, `OptFixSizeOf`, resolves scaling from the
  adjacent pointee record. `PrsExpression2` at lines 65-262 consumes
  dereferences, address-taking, updates and assignment destinations.
- `Compiler/PrsExp.HC:593-958`, `PrsUnaryTerm`, retains local addresses and
  prefix dereference/update operations. `PrsUnaryModifier` at lines 960-1115
  selects the exact member class, offset and remaining array dimensions.
- `Compiler/BackLib.HC:445-659`, `ICMov`, consumes raw scalar load/store widths;
  `ICDeref` at lines 693-708 selects the pointed width.
- `Compiler/BackC.HC:159-206`, `ICAssign`, writes through the selected pointed
  type and preserves an assignment's expression result.

The expected values come from this source audit and hosted execution. No
TempleOS runtime capture or machine-code byte parity is claimed.
