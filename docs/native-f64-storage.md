# Native F64 storage

Both native source targets can read and write automatic F64 arrays, packed F64
members and one-level owned F64 pointers in JIT and AOT source modes. Fixed
parameters can borrow these pointers across generated calls and recursion.
Storage transports eight binary64 bytes; floating arithmetic continues to use
the separately authenticated x87 plan. The integer interpreter remains outside
this execution subset.

The [storage example](../examples/native-f64-storage.hc) returns F64 3.75. It
borrows an unaligned packed array through a pointer parameter, reads the next
element with pointer addition, and adds two materialized values.

Taking an address does not read the pointee. Array subscripts retain their
selected eight-byte stride, including multidimensional arrays. Packed fields
can start at unaligned byte offsets. Saved references retain the original
allocation, offset and activation; changing the index that produced a pointer
does not retarget it. Floating pointer fields use a distinct pointee identity
in the existing ownership metadata.

Pointer addition/subtraction scale integer offsets by eight bytes. Difference
divides the byte distance by eight and requires one live original object and
extent. One-past formation does not permit dereference. Earlier successful
writes and reached instruction charges survive a later fault.

The hosted guards still reject unread bytes, out-of-window accesses and retired
stack owners. These are hosted safety rules, not claims about TempleOS fault
behavior. Pointer-array storage, deeper pointers, general floating compound
updates, standalone F64 globals/statics and arbitrary linked floating regions
remain outside this increment. Existing JIT aggregate initializer support is
described in [aggregate initializers](aggregate-initializers.md).

## Source basis

The reference is TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`.

- `Compiler/PrsExp.HC:960-1115` selects member classes and emits their offsets
  and array strides before the corresponding index expression.
- `Compiler/PrsExp.HC:15-63`, `Compiler/OptLib.HC:484-506` and
  `Compiler/OptPass012.HC:1259-1265` produce and resolve pointer offset scaling
  and the division used for pointer difference.
- `Compiler/OptPass789A.HC:461-468,606-608` routes address, dereference and
  assignment operations to the original backend helpers.
- `Compiler/BackLib.HC:445-691,693-710` implements `ICMov` and `ICDeref`;
  `Compiler/BackC.HC:159-211` implements `ICAssign`. Same-class RT_F64 memory
  transport uses the eight-byte move path without an arithmetic conversion.

`test/test_native_f64_cli.ml` exercises exact values, packed fields, negative
zero, recursive borrowing, pointer fields and hosted storage faults through the
public CLI in both modes and both native targets. Expectations come from the
pinned source and exact binary64 words; no new TempleOS runtime capture is
claimed.
