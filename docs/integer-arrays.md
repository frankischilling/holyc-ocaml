# Automatic integer arrays

`holyc run --target=ir examples/integer-arrays.hc` passes an automatic array
element's address to a helper. The helper changes the caller's element from 40
to 42. JIT and AOT modes use 51 runtime instructions, zero preparation
instructions, 24 active frame bytes and call depth two. One-below limits fail
without a result report.

```c
I64 Set(I64 *p) { *p+=2; return 0; }
I64 F() { I64 a[2]; a[0]=40; Set(&a[0]); return a[0]; }
F();
```

Positive-sized automatic I64/U64 arrays support indexed loads, assignments,
compound and prefix/postfix updates, and element address-taking. Each element
has its own initialization state. Taking its address does not read it. The
same checked address path evaluates the base and each index once, before the
RHS; compound updates read the destination word after RHS effects.

## Strides, grouping and pointer values

The exact checked frame supplies dimensions, element size and full object
extent. For `I64 a[2][3]`, successive brackets use byte strides 24 and 8.
Intermediate brackets produce addresses without loading a row. Native indexing
uses flat offsets: `a[0][3]` aliases `a[1][0]`, and `a[3][-4]` reaches `a[1][2]`.

Bare ordinary arrays at any rank, including partially indexed rows, can supply
an exact element-pointer local or fixed argument: `I64 *p=a`, `p=a[1]`,
`Set(a)` and `Set((a))` retain the original object. Grouping discards dimensions:
`(a)[1]` uses byte stride 8; `(a[1])[2]` retains the first row offset before
using pointer stride 8. `(a)[1][2]` is invalid for I64 elements. Ordinary `*a`
loads the first element at any rank. Callback arrays retain a separate domain;
their return type cannot establish ordinary array storage.

Explicit `&a` adds another layer to the array's implicit element pointer.
For I64 elements, `&a` and `&a[1]` while dimensions remain have type I64** and
stay outside this executor's one-level pointer domain. A fully indexed
`&a[i]` has type I64* and remains supported.

The IR emits base, pointer-typed stride constant, integer index, `IC_MUL`, then
`IC_ADD`. Preflight admits these operations only with the next checked array
stride or an element-pointer value and stride 8. Offset metadata cannot be a
stored, pushed or returned value. `IC_ADDR` explicitly materializes an array
address for a pointer context.

## Hosted bounds and ownership

References retain the actual live storage instance, declared object's base and
element count, and an int64 byte offset. Copies, fixed calls and recursion keep
those coordinates. An interior pointer can index backward within its original
array. A scalar object's extent remains one word; indexed access cannot reach
an adjacent local even when both occupy the same invocation's storage.

Intermediate bracket offsets may be outside the object. Final pointer
materialization permits aligned offsets from zero through one-past inclusive;
loads, stores and updates require an offset inside the object. A store's bounds
check occurs after RHS evaluation. Unwritten elements report `HCIRVM0012`,
invalid/lifetime references `HCIRVM0018`, object bounds `HCIRVM0019`, and checked
scale/add overflow `HCIRVM0020`. Unsigned indices above signed int64 range are
outside the hosted address domain. These diagnostics retain normal owner,
instruction and initializer-phase provenance.

Every element counts against active frame bytes. Total cell counts are checked
against both the byte budget and `Sys.max_array_length` before cell allocation
or expansion. Returned frames are invalidated and separate executions create
fresh storage images.

These are hosted bounds, not native invalid-pointer behavior. Pointer-valued,
zero-sized, global/static and aggregate arrays, array initialization and whole
array assignment, floating index execution, general pointer arithmetic and
pointer returns remain outside this gate. General memory/runtime, stateful
compilation and #exe, optimizer parity, native backends, BIN/loader acceptance
and bootstrap remain full-compiler requirements.

## Evidence

Pinned commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`:

- `Compiler/PrsVar.HC:247-281,532,590-606` supplies dimension products and full
  automatic allocation.
- `Compiler/PrsExp.HC:97-118,766-774,1055-1098` supplies ordinary array addresses
  and ordered index/stride/add operations.
- `Compiler/PrsExp.HC:72,151-161,739-746` establishes grouping's local dimension
  cursor and ordinary element dereference.
- `Compiler/PrsExp.HC:470-484` and `Compiler/PrsVar.HC:607-615` consume array
  pointers in fixed arguments and automatic pointer initializers.
- `Compiler/PrsExp.HC:201-208` and `Compiler/BackA.HC:555-566` retain destination
  and RHS/update ordering.

The eight original #613 source gates failed with HCRUN0003 at merged
`ffe1931fffd9cd6127c456b0c89d8f2c95423f95`; they now return 42 in both modes.
Source, malformed-IR, semantic-provenance and CLI tests exercise these rules.
No new native TempleOS capture is claimed.
