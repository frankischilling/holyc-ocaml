# Class and union initializers

[The example](../examples/aggregate-initializers.hc) prints `AB` and returns 42
through IR, isolated native execution and native source tasks in JIT and AOT
modes. It initializes a packed class array and a function-static class object.

Global and static initializers retain the selected class record, direct member
order, member types and checked fixed dimensions. Integer and Bool leaves write
their own widths. Nested classes, member arrays, root arrays and fixed byte
strings share that traversal. Automatic local initializers use a different source
expression path; this change does not apply global member-brace grammar to them.

Numeric callback leaves copy eight bytes regardless of the callback's return
type. Their original anonymous header selects `RT_PTR` pointer storage, so a
callback returning `U16`, `F64`, U0 or a class keeps the same byte cursor. Byte
views can read those initialized words. Calls through stored callback fields and
initialization from owned function addresses still need their execution support.

## Source cursor and grammar

`PrsVarInit2` forwards a backed class before traversing its dimensions. An
unbacked class with direct members requires braces and recursively consumes each
direct member. The cursor advances by bytes written. Field offsets, padding and
inherited members do not reposition it.

For example, `class Box{U8 tag;$$=6;U16 value;};Box object={7,42};` writes
one byte at offset zero and two bytes at offset one. Later `object.value` still
accesses offset six. In JIT mode those unwritten field bytes remain unknown;
AOT allocation supplies zero bytes. The hosted initializer preserves this source
behavior even when the field-access layout would suggest another destination.

A derived class initializes only its own direct members, starting at byte zero.
A class with no direct members copies its nominal width from the computed word;
a three-byte inherited class copies three bytes. A zero-byte member evaluates its
expression and consumes no storage. Raw class copies wider than eight bytes stay
gated because the source would read beyond the scalar result word.

Backing forwarding also controls the initializer array cursor. Two three-byte
objects backed by `U16` receive two-byte writes at offsets zero and two. Later
class indexing still uses a three-byte stride. Anonymous union members remain in
the direct source chain. The hosted path rejects a sequential write that leaves
the original allocation, rather than granting extra union bytes.

Fixed arrays accept the source's optional braces and delimiters. Every required
leaf must be present; missing members are not filled in. A string leaf copies
that array rank's fixed count. This path requires a forwarded byte type; a
one-byte class with raw `I64` storage still consumes scalar expressions. The count
may include the source terminator but cannot request bytes beyond the retained
literal. Inferred dimensions and copies
that would read beyond the literal remain unfinished.

## Execution and checks

Closed leaf preparation, AOT images and runtime initializer stores retain the
same original destination. Wider leaves become individual byte writes with the
original root bounds and initialization flags. Runtime regions must contain the
canonical stores regenerated from their original leaf metadata. Replaced roots,
changed byte coordinates and substituted source leaves fail ownership checks.

Successful IR and native source task initializers preserve ordered calls.
Native source tasks keep earlier output when a later leaf fails. Isolated IR
preflight checks the complete layout before executing its initializers. Static
initialization runs once during source preparation, not on every function call.
Isolated native preparation retains its existing closed-expression restrictions
for static and JIT initializers.

The shared fixtures cover integer widths, numeric callback words and arrays,
nesting, strings, forwarding, inheritance, padding, union traversal and zero-byte
leaves. The quota fixture uses
three global bytes and eight charged static bytes: 11 succeeds and 10 rejects.
Tests also check exact preparation and execution budgets, one below each budget,
foreign equal-looking class headers and output retained before a later failure.

Initializer traversal reads the reached record for each selected class identity.
A forward-selected member can complete after its containing class declaration
and before its initializer is consumed. Its direct members and fixed counts then
supply the initializer leaves. Later same-name classes cannot replace that
selection, and completion cannot enlarge the object's original allocation.
JIT lookahead can complete the selected member through a `#exe` directive before
its nested braces are consumed. AOT directives use the task's separate JIT table;
an unrelated task class cannot complete an assembler-selected forward class.
Attached globals retain the original body-completion receipt before the enclosing
statement finishes.

Each global scalar expression now has an original preparation receipt before
expression parsing. It retains the reached first token, syntax path and both
predecessor chains. The completed leaf keeps that same receipt. Callback
authority expires before expression completion, including after failed parsing.

Scalar metadata can also retain the class selected before evaluation and settle
its reached nominal width afterward. Backing is followed at the initial
selection. Completion of a cached forward class can change its copy width;
later same-name classes cannot replace it. The cached class also keeps its
original raw representation when new backing is declared. Integer results remain
complete register words, including late integer and floating backing.
Settlement rejects foreign records, another domain and repeated consumption.
These metadata reads grant no execution or storage authority. Runtime stores
still need to consume the phases before current-leaf completion can execute.

Incomplete selections and changes during an executing leaf remain unfinished.
Floating,
data pointer and owned callback leaves, empty root allocations, inferred
dimensions, general aggregate copies and the remaining aggregate execution
work stay open under
[#686](https://github.com/frankischilling/holyc-ocaml/issues/686). Native data
pointer fields retain their separate owned storage and activation checks.

These checks use pinned source and hosted execution. They include no TempleOS
runtime capture or comparison of original emitted machine code. Root bounds,
unknown-byte faults and resource quotas are hosted policy.

## TempleOS source

The reference is commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
The audit covers complete `PrsVarInit`, `PrsVarInit2`, `PrsGlblInit` and
`PrsStaticInit` at `Compiler/PrsVar.HC:1-246`; `PrsType` at
`Compiler/PrsVar.HC:285-371`; `PrsVarLst` at
`Compiler/PrsVar.HC:405-742`; and `PrsGlblVarLst` at
`Compiler/PrsStmt.HC:209-463`. `OptClassFwd` at `Compiler/OptLib.HC:9-15`
selects backing before traversal. Complete `MemberAdd` at
`Compiler/LexLib.HC:103-148` explains the direct source member chain.
`PrsClass` at `Compiler/PrsStmt.HC:1-60` reuses the selected extern class record
on completion. `PrsStreamBlk` at `Compiler/PrsStmt.HC:805-840` switches `#exe`
input to the task table and JIT flags before restoring the enclosing context.

Allocation, byte roots and persistent lifetimes are described in
[persistent aggregate storage](persistent-aggregates.md).
