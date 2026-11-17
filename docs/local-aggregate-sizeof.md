# Local class and union sizes

`sizeof(object)` reads the size of the class selected by the original local
declaration. For an array, it multiplies that size by the declaration's checked
dimension count. The query uses the layout available when the parser reaches
`sizeof`.

```c
class Packet { U8 tag; U16 value; };
I64 Measure(Packet parameter)
{
  Packet items[2][3];
  return sizeof(parameter)+sizeof(items);
}
Measure(0); // 3 + 18 = 21
```

The packed class occupies three bytes. Its parameter still uses the existing
eight-byte argument slot, while `sizeof(parameter)` returns three. An
integer-backed class likewise uses its complete layout size for this query.
Scalar class pointers and callback locals use eight bytes; arrays include their
dimension count. A trailing `*` suffix returns eight.

The [maintained example](../examples/local-aggregate-sizeof.hc) also queries a
union and a callback. It prints `3 18 8 8 8` followed by a newline and returns 45.

## Original type and query timing

A local keeps the exact class selected by its declaration. A later class with
the same spelling cannot replace that identity. Completing the original forward
can change the size observed by a later query, while an earlier result keeps
its original value. The query-time size is separate from the size captured when
automatic storage was allocated.

The query requires the original local and function publication, the selected
type occurrence, its semantic table and namespace, and the current record for
that canonical class. A proof from another declaration cannot authorize the
query, even if it names the same class. An expired parser callback cannot mint
another aggregate size read.

Runtime dimensions and `$$` offsets retain their original preparation
dependencies. Supported JIT layout expressions execute once at their original
callbacks; a size query does not repeat their calls or output.

## Coverage and boundaries

The shared fixtures cover packed classes, unions, inheritance, integer backing,
multidimensional and comma declarations, class parameters, pointers, callbacks,
function-local types, retained functions and same-name replacements. JIT cases
also cover forward completion and replacement during declaration lookahead.

The runtime fixture prints `offdim` and returns 42. IR tests check exact and
one-below instruction and preparation allowances. Native tests require completed
machine-code fragments with zero interpreted runtime instructions and also check
IR, code-byte and output allowances.

Two parser authority groups run in JIT and AOT modes. Each records seven original
queries and rejects foreign class records, stale snapshots, borrowed type
occurrences, tables, namespaces and functions. They query one forward local
before and after completion, observe zero then two, and verify that both saved
results remain unchanged.

Member suffixes follow the separate [member query path](aggregate-member-sizeof.md).
These metadata checks do not authorize zero-sized automatic storage. Remaining
`offset` queries, general object copies and runtime AOT
layout relocation retain their existing boundaries.
[Issue #820](https://github.com/frankischilling/holyc-ocaml/issues/820)
tracks this query feature within [aggregate work](https://github.com/frankischilling/holyc-ocaml/issues/686).

## TempleOS source

The reference is commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

- `Compiler/PrsExp.HC:303-350` implements `PrsSizeOf`. Lines 311-315 read the
  local's selected class size and dimension count before consuming lookahead;
  lines 346-350 apply pointer suffixes and emit an immediate integer.
- `Compiler/PrsVar.HC:285-371` retains selected classes, pointer layers, callback
  storage types and dimensions. Lines 521-532, 590-628 distinguish local
  allocation size from fixed parameter slot width.
- `Compiler/PrsStmt.HC:1-60` completes an extern class or publishes a new class
  identity when an already completed name is declared again.

Expected results derive from these source paths and hosted execution tests.
No TempleOS runtime capture or full compiler acceptance result is supplied.
