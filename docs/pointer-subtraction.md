# Owned scalar pointer subtraction

[Owned scalar pointer difference](pointer-difference.md) is implemented
by #783 with signed numeric results and sealed original size/division records.
Its preparation and raw-address boundaries remain explicit.

[Owned scalar pointer ordering](pointer-ordering.md) compares offsets within
one original live object/extent; unrelated objects retain a checked fault.

Issue #777 executes `pointer - integer` through IR, retained source tasks and
hosted native programs in both source modes. The result keeps its original
one-level integer/Bool pointer type and live owned object. The original pointee
width scales the full computed integer offset before subtraction.

```text
holyc run --mode=jit --format=json examples/pointer-subtraction.hc
holyc run --target=host-jit --mode=aot --format=json examples/pointer-subtraction.hc
holyc run --mode=aot --format=json examples/stream-pointer-subtraction.hc
```

The ordinary example returns I64 42 and captures `42:42:42:AB;`. Both modes
and targets use 157 runtime instructions, no preparation and 28 formatting-work
units. The retained example reads its original offset cell once while preparing
a default, then preserves 42 after later writes. Both IR modes use 138 runtime
instructions, four preparation instructions and seven formatting-work units,
with no captured output. Native retained publication remains HCPP0008.

## Captured objects and checked arithmetic

The left reference is captured before right-side effects. Rebinding its pointer
variable while evaluating the integer cannot replace that snapshot. Results
retain the original type, object, extent, offset, initialization and live owner.
Caller objects, globals/statics, scalar cells, arrays and literal bytes use the
same model. Stable canonical native records preserve aliases across repeated
evaluation, assignments, loops, nested calls and recursive borrowing.

Positive offsets move backward and negative offsets move forward. Taking an
address does not read the pointee. Valid one-past addresses can be formed, and
subtraction can move them back into the object. Reads and writes require an
actual original element; a scalar reference cannot reach adjacent locals.

Stored narrow integers use their normalized word; computed narrow returns keep
their full word. A computed U8 return of 257 subtracts 257, while a stored U8
initialized to 257 subtracts 1. Bool retains signed one-byte storage. Source
bracket-index operand restrictions remain separate.

Scaling checks signed range and rejects unsigned words beyond the hosted signed
address domain. Subtraction then checks its own signed overflow before reference
formation. In particular, subtracting the minimum signed word from a
nonnegative offset overflows. The VM uses direct subtraction bounds; native
code uses qword SUB and its overflow flag. Wrapping negation cannot supply a
different valid reference. Addition and existing indexing keep their behavior.

Unknown pointers, offset words or reached pointees fault with HCIRVM0012.
Negative or beyond-extent references and one-past accesses fault with
HCIRVM0019. Scale and offset-subtraction overflow fault with HCIRVM0020 at
their own original instructions. Native offset arithmetic retains the existing
`Index_addition_overflow` status kind, with the original IC_SUB site authorized
for subtraction. Reached effects, output and attempted work survive faults;
an unknown left pointer prevents right-side effects.

The completed source bundle seals original scale/add/subtract records and their
transitive producers. Foreign contexts, copies, type-compatible replacements
and changing an original subtraction to addition reject before execution.
Numeric words cannot manufacture pointer authority. No new callee activation
or descriptor allocation per evaluation is added. Runtime, preparation, storage,
descriptor, code, frame, arena, call and output limits remain checked. Native
frames still have the 4,088-byte limit and code the 65,536-byte limit. Closed
native defaults requiring runtime pointer loads remain HCRUN0006 under #685.

## Source and verification

The reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/PrsExp.HC:14-62,165-185` supplies scaling and separates subtraction
from pointer difference. `OptLib.HC:9-14,96-179,483-509` forwards classes,
fixes raw types and consumes the original pointee size. `OptPass789A.HC:559-566`
dispatches to `BackA.HC:166-222`'s ICSub, which preserves operand direction
through its SUB and NEG/ADD choices. The hosted bounded path emits the original
IC_SUB after checked scaling and uses direct checked subtraction.

`Compiler/LexLib.HC:249-271` uses `st+len1-1` in string concatenation. Its
complete allocation and memory-copy requirements remain open. Full optimizer
rewrites remain under #696/#697.

Tests check 116 independent values across all nine pointee and offset classes,
positive/negative/zero operands, caller/persistent/literal aliases, loops,
recursion and effectful operands. Seven source groups and six native groups
cover original/foreign/copied/changed authority, minimum-signed overflow,
initialization/bounds/scale faults, exact work, both ABI images, fresh execution
and exact/one-below runtime, preparation, code and frame limits. Maintained CLI
fixtures check values, captures, full computed words, original overflow/bounds
faults and once-only retained defaults.

[Equality](pointer-equality.md) compares original object identity and offset.
[Owned class pointers](class-pointers.md) add selected aggregate pointee
strides, pointer updates and multidimensional array decay arithmetic. Primitive
integer pointer updates use that same checked path. Raw difference, raw
ordering, integer-left operations, casts, escaping/returned/persistent pointer
variables and deeper pointers remain under
#687/#699/#700. Native retained tasks remain under #704, larger native images
under #703, and full ABI, artifacts, BIN/loader and bootstrap acceptance under
#702/#682. Expectations come from the pinned source and hosted execution.
No new TempleOS oracle capture is included.
