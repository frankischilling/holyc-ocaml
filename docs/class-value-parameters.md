# Class value parameters

[Class value returns](class-value-returns.md) carry qualified integer
register words while preserving nominal declaration types. Object reads
use the selected scalar prefix; register results retain the full word.

Source-defined functions accept provided integer words in fixed parameters of
completed earlier classes and unions in JIT and AOT, through isolated IR and
native execution. Each parameter owns
an eight-byte argument slot. Its original class still determines member offsets,
`sizeof`, pointer stride and the whole-value memory view.

```c
U16 class Packet { U16 low; U8 guard; };
I64 Take(Packet o, I64 n) {
  if (sizeof(Packet)!=3 || o.low!=33 || o.guard!=7 || n!=9) return -1;
  o.guard=11;
  o+=0x10000;
  if (o.guard!=11) return -2;
  return o+n;
}
Take(0x070021,9);
```

This returns 42. The incoming word initializes all eight bytes, including the
guard byte. Reading `o` uses its `U16` prefix. Updating that prefix preserves the
guard. `examples/class-value-parameters.hc` checks the same behavior with either
`--target=ir` or `--target=host-jit` in both modes.

Integer argument transport preserves the full word without narrowing it to the
formal class's backing width. All nine integer backings are supported; `Bool`
uses signed `I8` storage. Ordinary classes and unions use signed `RT_PTR` words.
Explicit backing chains follow their original selected headers. Inheritance
contributes layout without changing the derived class's default raw type.
Later same-name declarations cannot replace a parameter's original class view.

Passing a narrow class object first reads that object's scalar prefix. For
example, `Take(o)` does not copy the caller's guard byte. An assignment expression
such as `Take(o=0x07002a)` retains the full produced word at the call, while the
assignment itself stores only the caller object's prefix. Parameter writes
operate on the callee's own slot and leave caller storage intact.

A one-byte or empty class can therefore have a valid eight-byte parameter root.
A nine-byte class still receives only eight bytes. Its ninth member cannot
borrow storage from the next argument. Supported pointer steps use the nominal nonzero class size; member and
whole-value windows must fit the owned argument root. These bounds
and initialization checks belong to the hosted execution model and do not
describe TempleOS behavior for invalid memory. Empty automatic local objects
remain unsupported.

Fixed scalar and class parameters retain their source positions when their
internal storage uses different cell counts. Nested calls and recursion create
fresh slots. Class parameters also precede the original variadic count and tail.
The existing cleanup policy, call depth, frame, instruction and native image
limits apply. No new return convention or exported HolyC ABI is established.

Class header metadata retains the exact selected type receipt. A bound function
body separately checks its original definition, source namespace and frame
before acquiring the integer class view. Raw bodies, foreign members and
callback return metadata have no such authority. Call metadata retains the nominal target and a separate word
carrier. IR seeds individual byte cells from that carrier; native execution uses
the same eight-byte slot and owned reference extent. Numeric words do not grant
callback or executable ownership.

The shared fixtures add 36 value groups and four fault sources. The full
aggregate suites have 296 IR and 292 native groups, including ABI ownership,
foreign frames, both native host ABIs, fresh images and exact quota controls.
The actual CLI runs 872 IR reports and 1785 including native execution. Expected
words come from the pinned source audit; these checks contain no TempleOS
runtime capture or machine-byte comparison.

[Closed class default words](class-default-words.md) now prepare in AOT IR and
native JIT/AOT source compilation. Reference-bearing and retained-task class
defaults, callback class parameter signatures,
F64/pointer/callback backings, zero-size class pointer operations, persistent
class objects, native task classes, retained aggregate imports and general
aggregate copies remain open. Complete class and ABI acceptance remain under
[#686](https://github.com/frankischilling/holyc-ocaml/issues/686) and
[#702](https://github.com/frankischilling/holyc-ocaml/issues/702); full compiler
and release acceptance remain under
[#682](https://github.com/frankischilling/holyc-ocaml/issues/682).

The audit uses TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`. Complete `PrsVarLst` at
`Compiler/PrsVar.HC:408-721`, especially `620-659`, allocates fixed parameters in
eight-byte slots independently of their nominal class size. Complete
`PrsFunCall` at `Compiler/PrsExp.HC:383-591` converts fixed arguments only between
F64 and integer classes, retains the reversed push order and records cleanup.
Complete `ICPush` at `Compiler/BackLib.HC:312-348` pushes register words intact
and extends narrow memory operands. Complete `ICMov` at
`Compiler/BackLib.HC:445-659` likewise preserves register words while selecting
width and signedness for memory operands. The selected integer and default
whole-value views use the complete header and scalar consumers audited in
[integer-backed values](backed-aggregate-values.md) and
[default class values](default-aggregate-values.md).
