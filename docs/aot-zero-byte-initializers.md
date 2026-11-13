# Copy widths and AOT assignment stores

A JIT initializer copy is not equivalent to an AOT load-time assignment. Native
TempleOS captures show the difference for globals and function-static objects:

```c
I64 Get() { return 7; }
class Box { U0 empty; I64 value; };
Box object={Get(),42};
```

JIT leaves `object.value` equal to 42. After AOT compilation and loading the BIN,
it is 7. With `0x112233445566772a` as the following value, the AOT result is
`0x1122334400000007`: the U0-target assignment writes four bytes. A constant
initializer `{7,42}` still leaves 42 in both modes.

Empty classes and inherited classes without direct members have a different raw
type. Their default I64 assignment writes eight bytes even when their nominal
copy size is zero or three. Native captures also cover a static initializer whose
expression lookahead completes a class to two bytes. JIT copies those two bytes;
AOT writes the raw I64 width and overwrites the following byte.

## Original phases

`Compiler/PrsVar.HC:52-111` first compiles the expression with the AOT flag
cleared. A nonconstant expression outside the globals-on-data-heap path is
reparsed into an AOT assignment on the second pass. That assignment does not use
the scalar `MemCpy` byte count from the compile-time branch. The next initializer
leaf still follows the original nominal byte cursor.

`Compiler/BackC.HC:159-204` lowers the assignment through `ICMov`.
`Compiler/BackLib.HC:136-155,282-312,445-578` selects the machine store. The default
memory opcode and REX selection produce a 32-bit U0 store and a 64-bit I64 store.

## Supported execution

Bounded integer-result AOT leaves now use that raw store width. The lowering
keeps copy and load-assignment operations separate and emits one checked scalar
memory write for each assignment. It does not split that write into partial byte
stores. Canonical region validation reconstructs the operation from the original
source phase, scalar type, byte offset and allocation.

The full store window must fit the original object. Static alignment padding and
neighboring globals do not grant extra bytes. A write invalidates overlapping
data-pointer and callback ownership, including when it changes only part of an
eight-byte pointer field. Numeric bytes cannot restore that ownership.

A source opcode marked nonconstant in the pinned table must justify a changed
store width. Conservatively scheduled intrinsic expressions do not acquire a
load assignment merely because their original folding phase is unfinished.
Closed constant copies keep their nominal width, and live JIT keeps its copy
semantics.

U0 calls without an actual scalar result, reference values without an integer
word, out-of-bounds store windows, and unresolved constant/preparation phases
still reject explicitly. No machine-register value is invented. Supported closed
integer initializers of scalar U0 statics run at their source declaration; see
[persistent U0 declarations](persistent-void-declarations.md). Scalar U0 global
root initializers, nonconstant U0 static roots and the separate
globals-on-data-heap path remain unsupported. General AOT assignment warnings
and emitted-byte equality are not claimed.

The warning probe observes the compiler counter in successive `#exe` blocks.
A constant U0 copy adds no warning, an integer load assignment adds one, a U0
call followed by its load assignment adds two, and a static load assignment
adds one. These counts occur while parsing, before each following block. The
compiler now counts the corresponding warnings for a direct call to a completed
source-defined primitive function in a global or static scalar leaf, when the
call has up to 64 integer literals, selected closed integer expressions, or
completed nonzero numeric scalar globals supplied to nonzero numeric parameters.
Nonzero numeric parameters can also use closed integer defaults, including
omitted arguments between supplied values. The reference probes cover up to
three such parameters.
Expressions include bounded nesting of unary minus, addition, subtraction,
multiplication, bitwise operations, and shifts of integer literals or completed
integer scalar globals. Constant shift counts are limited to 0 through 63;
completed integer scalar globals can also supply a shift count. The 64-argument bound is
this path's verified coverage, not a claimed TempleOS limit.
The warning is counted at the original leaf callback, with single-use source
authority. The compiler checks the current data-heap option before counting the
load-assignment warning. `Compiler/OptPass012.HC` marks opcodes with the
nonconstant flag and warns on a zero-sized assignment operand; the pinned intermediate-code table
marks `IC_CALL` nonconstant. Other call forms and general expressions remain
unimplemented. Counting the warning does not make an unsupported F64 call frame,
dynamic global shift, or missing U0 return word executable. The native path
currently rejects an F64 global before it reaches the later warning callback.

## Evidence and checks

`test/oracle/aot-zero-initializers` retains five load source/capture pairs and
twenty-six warning-phase pairs, each run twice. Every compiled probe reports zero
errors. The load controls
cover ordinary and U0 callees, globals, statics, constants, patterned storage,
empty and three-byte classes, and expression-lookahead completion. The AOT
sources include `KernelA.HH`, where the public `I64` type is defined.

Tests compare supported stores with those original observations. CLI coverage
includes integer result widths, signed values, unaligned destinations, exact
windows, one-byte-short roots, pointer invalidation and exact/one-below work
limits. Native authority checks compile both ABIs and reject copied, reordered,
foreign or expired load evidence.

The earlier empty-copy implementation was incorrect for these AOT leaves. Its
first correction rejected them; bounded integer stores are now implemented.
Missing value families and the remaining AOT phases stay open under #686/#690.
