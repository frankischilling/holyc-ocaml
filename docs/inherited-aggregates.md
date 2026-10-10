# Owned inherited aggregates

[Integer-backed aggregate values](backed-aggregate-values.md) use the original
forwarded scalar width for whole reads, assignments and updates. Class extents,
member identities and pointer strides remain separate. General casts, class
value ABI paths and persistent objects still need their own implementation.

Completed earlier class and union definitions can supply inherited automatic
storage in isolated IR and native programs. Direct and pointer member access,
nested objects, member arrays, root arrays and class pointer operations retain
the base selected by the original declaration.

```c
class Base { U8 tag; };
class Item:Base { U16 value; };
I64 F() {
  Item a[2];
  Item *p=a;
  p[1].tag=20;
  p[1].value=22;
  return p[1].tag+p[1].value;
}
F();
```

The result is 42. `Item` has three bytes: the one-byte base prefix and a packed
two-byte child field. `p+1`, `p[1]` and pointer updates use that three-byte
stride. `examples/inherited-aggregates.hc` also reads an inherited text array,
prints `AB`, passes a derived pointer to a callee and returns 42. Both examples
run with `--target=ir` and `--target=host-jit` in JIT and AOT mode.

Each inherited definition retains its original completed base snapshot. The
driver requires that exact base definition and canonical identity to appear
earlier in the current object compilation. Every ancestor must have object
layout admission. Before publishing the member index, it compares the computed
child size, base size, base identity and zero base offset with that snapshot.
A later same-name declaration cannot replace the selected ancestor, even when
its byte size is equal.

A replacement inside base-name lookahead in IR uses the directive's parser
environment. In JIT mode that can leave the captured base outside the current
object compilation, so member execution fails explicitly. In AOT mode the
directive uses a separate environment and the completed outer base remains
available. Tests check both outcomes instead of borrowing the replacement's
same-size layout.
The isolated native executor rejects `#exe` in either mode; native source tasks
keep their separate metadata path and still lack aggregate object execution.

Member lookup searches the child first, then its single base chain. Inherited
member offsets remain absolute. Ordinary duplicate names fail; the original
`pad`, `reserved` and `_anon_` exceptions keep child-first lookup. Named and
anonymous unions overlap. A union child starts its own fields at zero and
retains a larger base extent when needed. A class child starts after the copied
base size. Explicit `$$` directives can move that position, and each definition
adds its own negative-offset adjustment once.

All nine integer field spellings use their checked widths and signedness.
Descriptors preserve root ownership, initialized bytes and invocation lifetime
through member addresses and pointer copies. A destination is captured before
its assignment RHS; compound pointer updates read the current pointer after
the RHS. Prefix and postfix updates keep their existing value ordering.

Bounds apply to the resulting object's actual byte extent. A child can shrink
its layout with `$$`, so an inherited field may no longer fit. That access
fails before touching bytes. Negative member offsets and one-past accesses
also fail. Unknown bytes, cross-object differences and scale overflow retain
the existing diagnostics and reached output. These checks are hosted policy;
they do not describe TempleOS behavior for invalid memory.

Thirty value groups are part of the shared 155 IR and 155 native groups.
Independent expected words cover chained bases, packed and signed fields,
empty children, union bases and children, nested inherited arrays, root/row
decay, pointer comparisons and updates, aliasing RHS effects, backing classes,
completed forwards, padding and same-size redefinitions. Fault fixtures cover
unknown bytes, fresh activations, shrinking layouts, negative offsets, bounds,
cross-object differences and scale overflow. IR controls reject borrowed,
missing, altered and other-function address proofs before execution. Native
controls distinguish graph sealing from those IR checks. Exact and one-below
frame, instruction, stack and image allowances cover both modes and native
ABIs, including fresh image executions. The CLI also checks the installed
compiler and maintained example. These checks have no TempleOS runtime capture.

Partial, self-referential and forward-only bases retain their separate
[captured size metadata](source-inherited-layouts.md); they do not supply owned
members. A declaration-time call can move a base outside the current object
compilation, so its captured size still does not authorize storage. Retained
JIT imports, native task object execution, function-local layouts, persistent
objects, aggregate initializers, general whole-object values and copies, pointer or
callback fields, general derived/base pointer conversions and pointer returns
remain open under [#686](https://github.com/frankischilling/holyc-ocaml/issues/686)
and related pointer/ABI issues. Full compiler acceptance remains under
[#682](https://github.com/frankischilling/holyc-ocaml/issues/682).

The source audit uses TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`: complete `PrsClass` at
`Compiler/PrsStmt.HC:1-60`, complete `PrsVarLst` at
`Compiler/PrsVar.HC:407-721`, `MemberFind` and `MemberAdd` at
`Compiler/LexLib.HC:67-86,103-148`, selected member and index producers at
`Compiler/PrsExp.HC:960-1115`, and the complete dereference and assignment
consumers at `Compiler/BackLib.HC:693-707` and `Compiler/BackC.HC:159-204`.
Pointer scaling and updates use the sources recorded in
[class pointer operations](class-pointers.md).
