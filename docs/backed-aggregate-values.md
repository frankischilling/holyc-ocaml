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

Automatic declarations can initialize the same scalar prefix directly:
`Word o=42;`. The original local allocation, selected nominal class and owning
function authenticate the destination separately from its right operand.
Literal values, calls and reads of earlier comma declarators use the same
projection. The initializer runs in each activation and does not initialize
the object's remaining bytes. Named backing chains and inherited roots retain
their original class selection and extent. Empty integer-backed roots still
evaluate their right operand, then fail the hosted extent check when the store
needs bytes outside that root.
For declaration initialization, the backing chain must reach an internal integer
type. An ordinary class's default raw type does not satisfy the source assignment
lvalue check.

The selection retains the original completed nominal class, its forwarded
integer type, the compiler table and namespace, and the owning function or source
command, scope and source position. A backing chain uses the definitions selected when it was declared;
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

Value groups cover every width, scalar prefix assignment,
backing chains and redefinitions, inherited and union storage, nested arrays,
signed and unsigned arithmetic, aliasing, captured destinations and fused
postfix stores. Shared fault and quota fixtures check initialization, wide
windows, unknown pointers, one-past accesses, exact and one-below limits, both
native ABIs and fresh images. IR controls reject missing, foreign,
other-function and changed-offset backing proofs before execution. Native
controls require the original sealed source graph before image creation.
The tests use independent expected words and hosted executions; they contain
no TempleOS runtime capture or machine-byte comparison.

Function-body assignments retain three original observations. Before consuming the operator,
the lvalue selects its forwarded endpoint. After operator lookahead, the parser
checks that endpoint for internal storage. Function completion retains the
metadata used by optimization. Completion of an ordinary forward endpoint during
lookahead can make it assignable. An endpoint already forwarded to `U16` stays
`U16` when the original extern later completes with `U8` backing. Completing a
different same-name class cannot retarget either observation.

These observations retain the original root extent, field offsets and array
dimensions. Each member index records the class size reached after '[' lookahead,
before parsing its index value. An earlier index can retain a zero stride while
a later one uses the completed class size. Multidimensional indices keep their
separately reached sizes and original dimension multipliers. Member pointer
arithmetic is bounded by the containing byte root; a zero stride can alias the
same address even beyond a declared member dimension. The assignment
result retains its full register word independently of the stored prefix,
including through parentheses and primitive local initializers. Copied source
nodes, another function, another table, another namespace and replayed phases
cannot consume these observations.

The [assignment observation example](../examples/assignment-backing-observations.hc)
keeps its declarations inside `#exe`, prints `A` and returns 42 through IR and
native source tasks in both outer modes. Ordinary JIT declarations also use the
observations. An isolated AOT declaration keeps its own class identity: `#exe`
uses the task's JIT table, so task completion cannot supply backing to that outer
class. The classic native API retains its separate `HCPP0008` directive boundary.

Top-level source tasks retain the same lvalue and index observations. Their
optimization metadata belongs to the original completed command. An initializer,
static leaf, saved named or callback default, dimension, member offset, metadata
value, switch endpoint or internal binding instead retains the completed
expression at its own parser callback. These fragments do not wait for a later
command or function body to complete. Scalar consumers use the checked value
type while the destination retains its original nominal class and byte root.

Each observation keeps the latest reached record for its canonical class.
Completion replaces an earlier forward record for that identity; a same-name
definition with another identity cannot hide or retarget it. Fragment completion
requires the exact original expression within its current callback. Another
table, namespace or expression, and repeated or expired callbacks cannot supply
backing or stride metadata.

The [top-level assignment example](../examples/top-level-aggregate-assignments.hc)
checks zero and completed array strides, compound stores, an earlier `U16`
endpoint and a global initializer's full register word. It prints `A` and returns
42 through IR and native source tasks in both outer modes. Hosted bounds and
ownership checks still apply; completion cannot enlarge the original root.

[Function-local object execution](function-local-aggregate-objects.md) preserves
an earlier selected backing when a local type replaces its name. Inline backed
definitions with attached locals retain their separate declaration-mode gate.

[Aggregate value casts](aggregate-value-casts.md) distinguish selected storage
widths from computed register bits. Remaining aggregate initializer leaves, prepared
AOT IR and ordinary native defaults containing references, callback parameter
ABI paths, floating execution and parameter storage,
U0 and floating automatic initialization, F64/pointer/callback
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
Complete `PrsExpression2` at `Compiler/PrsExp.HC:65-263` produces the dereference
class before consuming the assignment operator and performs the lvalue gate
after lookahead. `ICAdd` at `Compiler/PrsLib.HC:79-97` retains that class.
Complete `OptPass3` at `Compiler/OptPass3.HC:1-621` sets assignment storage from
the result's raw class. `CmpMinTypePointed` at `Compiler/OptLib.HC:528-533`
assigns its existing field back to itself in this pinned source. The host follows
those bytes rather than inferring a narrowing operation from the helper's name.
Complete `PrsStreamBlk` at `Compiler/PrsStmt.HC:805-840` switches to the task
table and clears AOT mode before restoring the enclosing compiler context.
The fused assignment rewrite is at `Compiler/OptPass012.HC:865-889`; complete
`ICAssignPostIncDec` at `Compiler/BackB.HC:429-466` stores the forwarded
width and advances by the original pointed-to class size. Ordinary update
consumers are recorded in [class pointer operations](class-pointers.md).
