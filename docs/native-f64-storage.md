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

## Persistent storage

Native globals and static locals also retain F64 scalars and fixed arrays.
Same-class F64 assignments, owned byte views and later reads work on both native
targets in both source modes. Storage geometry counts bytes without classifying
the value as an integer. The original F64 initializer type and byte extent remain
separate from the U8 units used by allocation and initialization cursors.

| Declaration behavior | Native source-task JIT | Native source-task AOT | Isolated native source |
| --- | --- | --- | --- |
| Uninitialized declaration followed by assignment | Supported | Supported | Supported in both modes |
| Same-class F64 initializer with an original reference | Supported | Supported at load time | Supported in AOT mode |
| Closed F64 declaration initializer | Supported | Unsupported preparation | Unsupported preparation |

JIT leaves execute once during declaration preparation, including statics in
unused functions. Scalar copies preserve all eight bytes, then later calls reuse
the stored value. The [persistent example](../examples/native-persistent-f64.hc)
prints `A` once and returns F64 3.75 through `host-jit-task --mode=jit`.
Earlier output and preparation work survive a later initializer fault, including
in an unused function's static array. F64/integer initializer conversions remain
unsupported.

Unread JIT bytes still fault. Original AOT zero images remain initialized;
partial byte writes do not erase that distinction. Native source-task replacement
keeps a saved function's original global. Explicitly retained images preserve
their writes across activations and GC compaction; ordinary image execution uses
a fresh arena. Instruction, logical global-byte and initializer-work limits
remain separate. One scalar F64 initializer needs three native expression steps
and eight copy bytes: 11 succeeds and 10 fails before the copy.

## AOT load initializers

In AOT mode, an original nonconstant F64 initializer can retain a load-time
assignment to its global or static destination. Both `host-jit` and
`host-jit-task` support same-class scalar, fixed-array, multidimensional and
packed aggregate leaves. Each assignment uses the original sequential byte
cursor and stores eight F64 bytes without an integer conversion. Negative zero
and signaling-NaN payloads survive unchanged.
Static leaf expressions retain their own result class while the enclosing
declaration keeps its storage identity. A packed class containing an F64 member
therefore does not impose an integer conversion on that member's initializer.

These assignments run in source order with the top-level statements in the AOT
entry. Statics in unused functions take part in the same load sequence; later
calls reuse their stored values. A static initializer cannot borrow an enclosing
call's parameter or automatic local. Reached calls, output and faults use the
runtime instruction budget, and a later fault retains earlier output and work.
Scheduling a leaf does not charge compile-time initializer work.

Only original AOT load leaves can follow an executable top-level statement in
isolated native compilation. A later constant or byte-copy initializer rejects
before its preparation begins. JIT declaration preparation, parameter defaults
and new class declarations retain their existing ordering boundaries.

The [AOT example](../examples/native-aot-f64-initializers.hc) prints `A` once and
returns F64 3.75. Run it with either native target:

```sh
holyc run --target=host-jit --mode=aot examples/native-aot-f64-initializers.hc
```

The original source expression must select the nonconstant path. A closed F64
literal still needs compile-time preparation, which remains unsupported in AOT
and isolated native execution. F64/integer initializer conversions also remain
unsupported. `Globals_on_data_heap` retains its original global rejection and
static compile-time preparation boundary.

## Addresses and ownership

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
updates, closed AOT/isolated floating initializer preparation and arbitrary
linked floating regions remain outside this increment. The JIT aggregate
initializer path is described in [aggregate initializers](aggregate-initializers.md).

## Source basis

The reference is TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`.

- `Compiler/PrsExp.HC:960-1115` selects member classes and emits their offsets
  and array strides before the corresponding index expression.
- `Compiler/PrsStmt.HC:350-463` allocates and publishes original global storage,
  zeroes AOT image bytes and reaches `PrsGlblInit`. `Compiler/PrsVar.HC:1-246`
  supplies sequential initialization, same-class F64 copying and static
  preparation; its declaration branch at lines 536-587 runs before publication.
- `Compiler/PrsVar.HC:53-112` separates original nonconstant AOT leaves from
  compile-time copies. It emits an assignment routine registered as `IET_MAIN`;
  `PrsStaticInit` at lines 215-246 shares that traversal for static declarations.
- `Compiler/PrsExp.HC:15-63`, `Compiler/OptLib.HC:484-506` and
  `Compiler/OptPass012.HC:1259-1265` produce and resolve pointer offset scaling
  and the division used for pointer difference.
- `Compiler/OptPass789A.HC:461-468,606-608` routes address, dereference and
  assignment operations to the original backend helpers.
- `Compiler/BackLib.HC:445-691,693-708` implements `ICMov` and `ICDeref`;
  `Compiler/BackC.HC:159-211` implements `ICAssign`. Same-class RT_F64 memory
  transport uses the eight-byte move path without an arithmetic conversion.

`test/test_native_f64_cli.ml` exercises exact values, packed fields, negative
zero, recursive borrowing, pointer fields and hosted storage faults through the
public CLI in both modes and both native targets. Expectations come from the
pinned source and exact binary64 words; no new TempleOS runtime capture is
claimed.
The AOT cases also check source order, unused statics, later faults, exact runtime
limits and separate conversion/preparation rejections.
`test/native_initializer_authority/test_native_initializer_authority.ml` checks
original declaration receipts and typed stores before native image emission.
