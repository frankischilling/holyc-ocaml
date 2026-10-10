# Integer-backed aggregate values

[Class value returns](class-value-returns.md) carry qualified integer
register words while preserving nominal declaration types. Object reads
use the selected scalar prefix; register results retain the full word.

[Default class and union values](default-aggregate-values.md) use the signed
eight-byte prefix when the backing chain ends at an ordinary class.

Completed earlier classes and unions with an integer backing can read, assign,
and update a whole value in automatic storage. The value is the scalar prefix
selected by the backing chain. The class still owns its full byte extent, and
class pointers still advance by the class size.

```c
U16 class Word { U16 low; U8 guard; };
I64 F() {
  Word o;
  o.guard=9;
  o=42;
  if (o.low!=42 || o.guard!=9 || sizeof(Word)!=3) return -1;
  return o;
}
F();
```

This returns 42. Assignment writes two bytes at the beginning of the
three-byte object. `b=a` between these objects also writes that scalar prefix;
the destination's remaining bytes retain their previous state. The maintained
`examples/backed-aggregate-values.hc` uses two objects, checks their guard bytes
and three-byte pointer stride, and returns 42 in JIT and AOT with either
`--target=ir` or `--target=host-jit`.

The selection retains the original completed nominal class, its forwarded
integer type, the compiler table and namespace, and the owning function, scope and source
position. A backing chain uses the definitions selected when it was declared;
a later same-name class cannot change its width or signedness. Inheritance and
backing are separate relations. Completed inherited storage can have an
integer backing without changing its selected base or member lookup.

All nine integer spellings have their existing scalar widths and signedness.
`Bool` uses the reference's signed `RT_I8` storage. Whole values work through
direct locals, nested members, multidimensional array elements and owned class
pointers. Arithmetic, comparisons, prefix/postfix scalar updates and compound
assignments use the forwarded integer class. Byte and field views alias the
same root bytes. Scalar results can be returned from supported integer
functions and supplied to supported integer fixed or variadic parameters.

An assignment captures its destination before the RHS. The reference's fused
`*p++=rhs` and `*p--=rhs` path has a separate order: capture the pointer binding,
evaluate the RHS, read the binding's current pointer, store the scalar, then
advance by the original class size. Grouped destinations retain that order.
The backing projection changes the scalar access width while preserving the
private reference descriptor and its original root.

Hosted access checks use the root's actual byte extent. A backing wider than a
standalone object fails before touching bytes. The same window can fit inside
a larger root containing that object; the fixture checks writes across the
nested class's extent into adjacent root bytes. Reads require every reached
scalar byte to be initialized. Unknown pointers, one-past accesses, fresh
activation state and resource limits retain the existing diagnostics. These
are hosted ownership and bounds policies, not TempleOS behavior for invalid
memory.

Twenty-seven value groups cover every width, scalar prefix assignment,
backing chains and redefinitions, inherited and union storage, nested arrays,
signed and unsigned arithmetic, aliasing, captured destinations and fused
postfix stores. Shared fault and quota fixtures check initialization, wide
windows, unknown pointers, one-past accesses, exact and one-below limits, both
native ABIs and fresh images. IR controls reject missing, foreign,
other-function and changed-offset backing proofs before execution. Native
controls require the original sealed source graph before image creation.
The tests use independent expected words and hosted executions; they contain
no TempleOS runtime capture or machine-byte comparison.

[Function-local object execution](function-local-aggregate-objects.md) preserves
an earlier selected backing when a local type replaces its name. Inline backed
definitions with attached locals retain their separate declaration-mode gate.

Postfix casts involving whole backed values, aggregate initializers, prepared
AOT IR and ordinary native defaults containing references, callback parameter
ABI paths, floating execution and parameter storage, persistent objects,
zero-sized automatic objects, F64/pointer/callback
backings and general aggregate copies remain open under
[#686](https://github.com/frankischilling/holyc-ocaml/issues/686) and the
related value and ABI issues. Full compiler and release acceptance remain
under [#682](https://github.com/frankischilling/holyc-ocaml/issues/682).

The source audit uses TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`. Complete `PrsType` at
`Compiler/PrsVar.HC:285-371` attaches the selected `fwd_class`; complete
`OptClassFwd` at `Compiler/OptLib.HC:9-15` follows that relation. Local and
global address producers at `Compiler/PrsExp.HC:761-807,830-904`, dereference
and assignment construction at `81-154,183-201`, and complete
`PrsUnaryModifier:960-1115` retain nominal addresses before the scalar read.
`CmpRawType` and `CmpRawTypePointed` at `Compiler/OptLib.HC:509-525` forward
the value class. Complete `ICDeref` at `Compiler/BackLib.HC:693-707` and
`ICAssign` at `Compiler/BackC.HC:159-204` emit scalar loads and stores.
The fused assignment rewrite is at `Compiler/OptPass012.HC:865-889`; complete
`ICAssignPostIncDec` at `Compiler/BackB.HC:429-466` stores the forwarded
width and advances by the original pointed-to class size. Ordinary update
consumers are recorded in [class pointer operations](class-pointers.md).
