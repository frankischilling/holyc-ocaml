# Owned scalar pointer addition

[Owned scalar pointer ordering](pointer-ordering.md) compares offsets within
one original live object/extent; unrelated objects retain a checked fault.

Issue #775 executes `pointer + integer` through IR, retained source tasks and
hosted native programs in both source modes. The result keeps the original
one-level integer/Bool pointer type. A one-dimensional array address can supply
the left operand directly. Existing flattened and row aliases also work.

```text
holyc run --mode=jit --format=json examples/pointer-addition.hc
holyc run --target=host-jit --mode=aot --format=json examples/pointer-addition.hc
holyc run --mode=aot --format=json examples/stream-pointer-addition.hc
```

The ordinary example captures `42:42:42:B;` and returns I64 42. All four
mode/target combinations use 158 runtime instructions, no preparation and 26
formatting-work units. The retained example prepares a default by reading an
offset cell once, then reuses 42 after the cell changes. Both IR modes use 134
runtime instructions, four preparation instructions and seven formatting-work
units, with no captured output. Native retained publication remains HCPP0008.

## Original objects and effects

The left expression runs first and captures its reference before the right
expression runs. Rebinding the left pointer during a right-side call or
assignment cannot replace that snapshot. The offset is scaled by the original
pointee's byte size, then added to the captured byte offset. Stored narrow
integers use their normalized word; a computed narrow return keeps its full
word. For example, a U8 function returning 257 supplies offset 257, while a
stored U8 initialized to 257 supplies offset 1. Bool retains signed one-byte
backing as an object type and an offset type.

The result retains the original object, extent, offset, type, initialization
state and live owner. It can refer to automatic or caller cells, global/static
storage or literal bytes. Taking the offset address does not read the pointee.
Writes through that reference initialize the original cell. One-past addresses
may be formed and moved back with a negative addition; dereferencing them faults.
A scalar reference cannot reach a neighboring local object.

Unknown pointers, offsets or reached cell reads fault with HCIRVM0012.
Negative final offsets and references beyond the original extent fault with
HCIRVM0019, as do one-past reads or writes. Scale and byte-offset addition
overflow fault with HCIRVM0020. An unknown left pointer prevents right-side
effects. Other faults retain reached effects, output and attempted instruction
work. General raw word arithmetic never grants pointer authority.

Lowering reuses the checked stride, multiplication, addition and reference
materialization path. Native execution selects existing canonical 32-byte
records, so repeated additions, assignments, loops and recursive borrowing
preserve earlier aliases. It adds no allocation per evaluation or ordinary
callee activation. Original body, type, producer and context checks remain
required. The original pointer scale/add records and their transitive
producers are sealed with the completed source bundle. Foreign contexts, copied
records and type-compatible operand replacement reject.

Existing runtime, preparation, storage, descriptor, code, frame, call and output
limits still apply. The complete native descriptor table counts toward the
4,088-byte per-function native frame limit, including records for persistent objects. Large
arrays can therefore be rejected before native entry. The native code limit is
65,536 bytes; repeated output expansion can reach it. This feature does not
widen either limit. Closed native defaults that require runtime pointer loads
remain HCRUN0006; broader preparation stays under #685.

## Source and verification

The reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/PrsExp.HC:14-62,165-185` supplies pointer/integer scaling and
distinguishes subtraction and pointer difference. `OptLib.HC:9-14,96-179`
forwards classes and fixes binary raw types; `OptFixSizeOf` at 483-509 consumes
the original pointee size. The bounded lowering records that known stride and
its checked work. Full optimizer rewrites remain under #696/#697.

`Compiler/LexLib.HC:249-271` uses `st+len1-1` in string concatenation;
`Compiler/CMain.HC:128-141` uses `res->buf+j` in AOT buffer assembly. Their
complete allocation, memory-copy and aggregate requirements remain open.
This implementation covers the left-pointer addition within those expressions.

Tests check 116 independent value cases, all nine pointee and offset types,
positive/negative/zero offsets, aliases, loops, recursion, persistent storage,
literal bytes and effectful operands. Seven source groups and six native groups
cover exact faults and work, initialization, one-past references, scale/add
overflow, full computed words, both ABIs, fresh images and exact/one-below
runtime, preparation, code and frame limits. Maintained CLI fixtures check
values, captures, original fault kinds and retained defaults in both modes.

[Pointer subtraction](pointer-subtraction.md) uses the same owned model with
direct subtraction checks. [Equality](pointer-equality.md) compares owned objects
and offsets. Difference, integer-left addition, raw address ordering, compound
pointer updates, pointer returns, persistent pointer variables, casts, deeper
indirection, aggregate pointees and direct multi-rank array addition remain
outside this path under #687/#699/#700. Native retained tasks remain under #704,
larger native images under #703, and full ABI, artifacts, BIN/loader and
bootstrap acceptance under #702/#682. Expectations come from the pinned source
and hosted execution. No new TempleOS oracle capture is included.
