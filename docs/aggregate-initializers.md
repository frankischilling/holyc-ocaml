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

Each array rank retains the class reached by forwarding before advancing past
its optional opening brace. Its children recurse from that cached class. If
`Item` already forwards to `U16`, completion that changes `Item` to `U8` after
the brace cannot shorten the current rank's writes. Later recursion can still
follow new backing on the cached terminal class itself.

`extern U16 class Item;` retains a backed forward declaration. The parser keeps
the backing type, original selection and pointer layers. Source-task metadata
installs that backing after class-name lookahead, matching `PrsClass` followed
by the forwarding assignment in `PrsType`. An initializer reached during that
lookahead still sees the preceding zero-byte class. Its reached destination
remains attached when the forward declaration completes.

Ordinary definitions retain a separate backing-installation phase after their
body and following-token lookahead. A fresh class has no backing during its body;
completion of an extern class retains the prior backing until this phase.
An unbacked completion preserves that prior forwarding. The new phase keeps the
original backing selection, so body effects and same-name replacement cannot
retarget it. Each reached direct member can supply initializer grammar while
the ordinary body is still being parsed.

The [class-body example](../examples/class-body-initializers.hc) prints `A` and
returns 42 in both outer modes through IR and native source tasks. Its first
initializer runs during class-name lookahead: the `Item` leaf writes zero bytes,
then the tail writes 42. After the body finishes and forwarding is installed,
the second initializer copies `0x12a` as two `U16` bytes and writes 7 afterward.
Both objects retain their original eight-byte allocations and actual reached
destinations.

The [backed forward example](../examples/backed-forward-initializers.hc) prints
`A` and returns 42 through IR and native source tasks in both outer modes. Its
static array copies `0x12a` and 7 as two `U16` words, even though lookahead changes
`Item` to `U8`. Byte reads verify `2a 01 07 00` in the original eight-byte root.
Nested byte strings and zero-width forwarding use the same array selection.
Backing changes cannot enlarge the root or turn an originally wide copy into
a shorter write that bypasses its bounds.

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

Global and static scalar leaves in source tasks retain the class selected before expression
parsing and read its reached nominal width after evaluation. Backing is followed
at the initial selection. Completion inside a callee or during expression
lookahead can change the copy width. Newly added direct members do not restart
that scalar branch. The next leaf traverses its own reached selection, including
later elements of the same fixed array.

The copy uses the returned register word and advances the byte cursor by the
reached width. A still-incomplete zero-byte class evaluates its expression and
leaves the cursor in place. Same-name replacement cannot retarget the cached
class. New integer or floating backing also leaves its cached raw representation
intact: an eight-byte class still copies all eight bytes of an integer result.
The original allocation remains the bound for every write.

The [live scalar example](../examples/live-scalar-initializers.hc) prints `A`
and returns 42 in outer JIT mode, or 7 in outer AOT mode, through IR and native
source tasks. In JIT, `StreamExePrint` completes the cached task class, so the
first leaf copies two bytes and the second leaf writes byte two. The `tail`
member still accesses its declared offset zero and reads 42. In AOT,
`StreamExePrint` uses the saved compiler's separate table. The cached task class
remains zero bytes, so the second leaf writes 7 at byte zero.

IR writes invalidate overlapping pointer and callback ownership. Native leaves
use an actual scalar capture, an original checked destination plan and a private
receipt from the native byte copy. A copy enters once; completion consumes its
receipt once. Foreign programs, work counts, arenas, domains, expired requests
and replay reject. The retained completed layout preserves these source choices
when the global declaration or completed function frame is published. Expression work and actual copy bytes
share the initializer allowance. Earlier output survives a reached width, root
bound or quota failure.

Static expressions retain a separate receipt before parsing, including their
first token, syntax path, predecessor and already reached delimiters. The
completed static leaf keeps that receipt. Copies use the same cached-class timing
as global leaves and run once during declaration parsing. Later function calls
reuse their original static bytes and initialization flags.

Static delimiters also publish an original receipt before lexer advancement.
The layout consumes each opening brace, comma and closing brace during that
callback. Both predecessor chains and the original allocation remain attached;
expression preparation does not replay those delimiters. Completion checks the
retained delimiter sequence against the completed initializer.

This timing matters for a forward member. If its nested opening brace is already
reached while the selected class has no direct members, a directive after that
brace cannot change the selected branch. The hosted path rejects before running
the directive. Completion reached before the nested brace can supply its member
grammar. Earlier successful leaves and their output remain observable when a
later branch fails. The same checks apply during source-task adoption and native
execution.

The [static live scalar example](../examples/live-static-scalar-initializers.hc)
calls its function twice after preparation. It prints `A` once and returns 42 in
JIT or 7 in AOT. The second call sees the first call's mutation; it does not
repeat the initializer or its class completion. IR and native source tasks retain
the actual reached leaf destinations when joining the completed frame.

The [class-body member example](../examples/class-body-member-reads.hc) compiles
its function while `Item` is still being parsed. Member-name observations retain
the reached direct fields and the original attached base snapshot. Reads and
writes use those fields' selected types, offsets, array dimensions and placement
sizes. Anonymous union fields retain their shared offset. The later `Item.later`
declaration does not enlarge the earlier `Box` allocation or enter the function's
member snapshot. The example prints `A` and returns 42 through IR and native
source execution in both outer modes.

The [top-level member example](../examples/class-body-top-level-members.hc)
performs those reads and writes during the nested `#exe` block itself. It saves
the result before the class body resumes, then emits the final statement after
the class finishes. Top-level expression nodes retain their original AST
occurrences, including call results before their call records are created.
Offset, metadata and default fragments retain the original context needed to
validate special position nodes.

Member-name observations also retain an older class hidden by a new ordinary
definition with the same spelling. `MemberFind` uses the operand's raw class;
the operand's original canonical type selects that reached record. Old direct
and inherited members remain readable. Fields found only in the replacement
class cannot retarget the earlier object.

The observation follows dot/arrow lookahead and precedes skipping the member
name, matching `PrsUnaryModifier`'s call to `MemberFind`. A class completion
reached before the name can supply its members; completion reached after that
selection cannot authorize the earlier read. Missing reached fields still report
`HCSEMA0046`. Existing root bounds and initialized-byte checks apply to these
projections.

Isolated preparation of incomplete selections, partial callback calls and other
runtime class changes remain unfinished. Floating,
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
Complete `MemberFind` at `Compiler/LexLib.HC:67-85` searches reached direct
members before the attached base. `PrsUnaryModifier` at
`Compiler/PrsExp.HC:960-1115` selects the raw member after dot/arrow lookahead,
then uses its offset, class and array dimensions.
`PrsClass` at `Compiler/PrsStmt.HC:1-60` reuses the selected extern class record
on completion. `PrsStreamBlk` at `Compiler/PrsStmt.HC:805-840` switches `#exe`
input to the task table and JIT flags before restoring the enclosing context.

Allocation, byte roots and persistent lifetimes are described in
[persistent aggregate storage](persistent-aggregates.md).
