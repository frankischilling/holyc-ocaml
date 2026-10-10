# Automatic objects of function-local types

A function can declare a class or union and then allocate automatic objects of
that completed type. The existing integer member, nested object, array and class
pointer operations use the layout selected at each original source occurrence.

```c
I64 Answer()
{
  class Item { U16 value; };
  Item items[2];
  items[0].value=20;
  items[1].value=22;
  return items[0].value+items[1].value;
}
Answer();
```

The maintained [example](../examples/function-local-aggregate-objects.hc) also
uses a class pointer and a member array. It prints `42` and returns 42.

## Declaration and allocation order

Class publication occurs during parsing, including inside an uncalled function
or a branch that never executes. The type enters the global compiler namespace.
Each automatic object still belongs to its function invocation, so recursive
calls have separate storage and initialization state.

An object keeps the exact class selected by its declaration. A later class with
the same spelling does not change its members, element size or pointer stride.
Retained JIT calls keep those original selections after subsequent declarations
replace the visible name.

The original parser consumes declaration lookahead and dimensions before it
allocates a local. Layout admission therefore distinguishes type selection from
the allocation event. Completing the selected forward during that lookahead can
supply its layout. A completion reached after allocation cannot enlarge the
earlier object. Member access and pointer arithmetic use their own original
source positions; they do not borrow the final state of the function body.

In outer AOT input, a `#exe` declaration belongs to the separate JIT task
namespace. It cannot complete or replace the selected outer class. Tests retain
that distinction while checking lookahead completion in a shared JIT namespace.

Supported runtime bounds and `$$` offsets execute once during their original JIT
parser callbacks. Calls reuse the captured dimensions even if the globals used
by a bound later change. Native source execution retains the corresponding
completed native work and layout dependencies.

## Execution and validation

IR and native execution share the existing automatic aggregate storage and
member paths. Default class values use their selected signed word prefix.
Objects of an earlier integer-backed type keep that backing when a local
declaration replaces its name. A value conversion does not copy the whole object.

The declaration-order evidence keeps the original function identity separate
from the point where a layout became available. Member, pointer-stride and
scalar-prefix proofs remain tied to their original types and functions. Tests
substitute foreign proofs, remove proofs and alter field offsets or strides to
check rejection before execution or native image creation.

Source tests cover local classes and unions, default scalar values, nested and
inherited members, multidimensional arrays, pointer updates, recursion,
same-name replacements, generated input and retained calls. They also cover
forward completion, unknown bytes, one-past accesses and exact instruction and
preparation limits. Public CLI tests run the maintained example through IR and
native execution.

Inline backed local definitions such as `I64 class C{I64 value;} object;` remain
gated. The pinned `PrsType` resets declaration mode to zero for that form, so
`PrsVarLst` does not reach its automatic allocation branch. Parsing the form
alone does not establish executable local storage.

Completing a forward in a directive between a base operand and its member or
index suffix also remains gated. The current proof records the original base
reference; admitting a later completion at the suffix needs a separate parser
receipt. This boundary is narrower than the pinned member lookup path.

Persistent aggregate objects, general object copies, pointer fields and class
pointer returns retain their separate implementation boundaries. Runtime AOT
layout expressions still require their relocation support. This feature is
tracked by [issue #818](https://github.com/frankischilling/holyc-ocaml/issues/818)
within the broader aggregate and compiler work.

## TempleOS source

The reference is commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

- `Compiler/PrsStmt.HC:1-60` publishes classes and completes their layouts.
  Lines 1143-1164 distinguish class statements from local variable declarations.
  Lines 805-840 switch and restore the namespace for `#exe` input.
- `Compiler/PrsVar.HC:286-370` retains selected classes and consumes declarator
  lookahead and array dimensions. Lines 310-318 and 367 carry the inline backed
  declaration's mode reset. Lines 521-532
  determine the selected object's size; lines 590-617 place it in the frame and
  parse its initializer.
- `Compiler/PrsExp.HC:960-1115` selects members and scales array indexing.
  `PrsAddOp` at lines 15-63 supplies class pointer arithmetic scaling.

Expected values come from these source paths and hosted tests. This change adds
no TempleOS runtime capture or full compiler acceptance claim.
