# Aggregate value casts

Integer postfix casts preserve TempleOS's distinction between a storage
address and a computed register value. Default class and union values use the
signed `RT_PTR` word; explicitly backed classes use their original integer raw
class. Both paths execute in IR, native programs and native source tasks, in
JIT and AOT modes.

```c
class Box { U64 word; };
I64 F() {
  Box o;
  o = 0x1002a;
  if (o(U8) != 42) return -1;
  if ((o)(U8) != 0x1002a) return -2;
  o(U8) = 40;
  o(U8) += 2;
  return o(U8);
}
F();
```

`o(U8)` retains the object's address and reads one unsigned byte. `(o)(U8)`
first reads the original class value, then changes the register's computation
class while preserving its bits. A literal such as `0x1002a(Box)` also keeps
its register bits. Assignment to an object still stores only that object's
original scalar prefix. A class register value supplies no byte root for
member access or address construction.

Storage casts can be assignment destinations, enter integer updates, or
expose fields of a selected class: `o(View).value` uses `View`'s original
member layout. Consecutive storage casts select the final read width.
Captured destinations survive RHS rebinding. Nested members, inherited
storage, automatic arrays, globals, statics, function-local classes and
retained functions use the same original storage and canonical class
identities. A later same-name class cannot replace an earlier selection.

The hosted root's extent, byte offset, initialized bytes and lifetime remain
unchanged. A cast can read a wider window within its containing root, but
cannot enlarge a short root, use frame padding or read uninitialized bytes.
For example, one initialized byte permits `o(U8)` while `(o)(U8)` still
requires the original default eight-byte read. Invalid-memory diagnostics are
hosted policy, not claimed TempleOS behavior.

The source audit uses pinned TempleOS commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`. Complete `PrsUnaryModifier` at
`Compiler/PrsExp.HC:960-1115` changes the preceding instruction's selected
class and records the parenthesis flag. `PrsExpression2` at `65-262` emits
the later dereference or update and validates assignment destinations.
`Compiler/OptPass012.HC:87-110` distinguishes immediate, grouped and ungrouped
casts. Complete `CmpRawType` and `CmpRawTypePointed` at
`Compiler/OptLib.HC:508-525` forward the selected raw class. Complete
`ICPush`, `ICPop` and `ICMov` at `Compiler/BackLib.HC:311-659` preserve full
register words while memory loads select their signedness and byte width.
`Compiler/OptPass789A.HC:445-448` emits a remaining cast through `ICMov`.

The maintained example runs through the public CLI in both outer modes for IR,
native programs and native source tasks. Source tasks also check cast faults,
reached output and exact execution limits in both modes.

Shared tests cover all nine integer raw classes, every backing/read-width
pair, grouped and immediate class values, cast destinations, class members,
original replacements, parameter and return words, and overlapping storage.
Faults check short roots, initialized bytes, unknown references and one-past
accesses. Foreign and other-function class proofs reject before execution;
native graph mutations reject before image creation. Exact and one-below
frame, instruction, stack and image limits accompany the value cases. These
checks use source-derived expected words and hosted executions; they add no
TempleOS runtime capture or machine-byte comparison.

F64 and pointer backings, callable callback fields, numeric reference
construction, pointer returns and general aggregate copies remain under
[#686](https://github.com/frankischilling/holyc-ocaml/issues/686). Owned
callback code retains its existing full-word view boundary. An ungrouped
postfix update cannot consume another cast; a grouped completed value can.
Inline backed local declarations retain their separate declaration-mode gate.
Full compiler and release acceptance remain under
[#682](https://github.com/frankischilling/holyc-ocaml/issues/682).
