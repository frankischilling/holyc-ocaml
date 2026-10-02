# Owned scalar pointer difference

Issue #783 subtracts two matching live owned integer/Bool scalar references
within one original object and extent. Compatible U8 literal spellings keep
their existing contract. Both source modes support IR and hosted native
execution. The result is a signed I64 element count; subtraction reads no
pointee and gives the numeric result no reference authority.

```text
holyc run --mode=jit --format=json examples/pointer-difference.hc
holyc run --target=host-jit --mode=aot --format=json examples/pointer-difference.hc
holyc run --mode=aot --format=json examples/stream-pointer-difference.hc
```

The ordinary example returns 42 and captures `1:-1:0:-1:1;` with 149 runtime
instructions, no preparation and 28 formatting-work units in both modes and
targets. Print arguments run right to left: the fourth argument snapshots the
old `p`, then rebinds it before the first two arguments run. The byte-sized
retained example prepares its difference once, changes the offset and still
returns 42. Both IR modes use 122 runtime instructions, seven preparation
instructions and seven formatting-work units, with no captured output.

## Original operations and live objects

The left reference is captured before right-side effects. Each operand keeps
its original storage, base, extent, width, aligned offset and live owner.
Matching references subtract their byte offsets through original IC_SUB.
Pointee sizes above one then emit original IMM_I64/IC_DIV; byte-sized pointees
omit division. Equal, forward, backward and one-past differences work without
reading unknown cells. Valid nonnegative offsets fit the signed hosted range,
so their subtraction fits I64. The original size division returns an element
count even for unsigned pointee declarations.

Different objects or extents fault with HCIRVM0018 at original IC_SUB after
reached operand effects. Neighboring scalars, adjacent arrays, separate literal
producers and recursive activations remain distinct. Rows and flattened
addresses retain the full original object extent. Repeated evaluation of one
literal producer keeps its original region. This hosted rule does not subtract
unrelated raw TempleOS addresses or claim their placement.

The VM validates the operands left to right, then checks physical storage,
base, count and width. Native code compares canonical data-base,
initialization-region and extent fields before subtracting logical offsets.
Separate descriptor records for one object retain the same identity. Existing
synchronous call lifetimes remain required.

Original subtraction, size/division and transitive producer records are sealed
with the completed source bundle. Copies, foreign contexts, operator changes
and compatible operand replacements reject before entry. Current graph
matching also rejects numeric instructions rewritten into pointer difference.
The I64 result can participate in ordinary numeric operations and stored
narrowing, but cannot initialize an owned pointer.

Native `Pointer_difference_object_mismatch` status 18 requires an original
difference site in the sealed image and attempted instruction work. Other
sites and zero-step mismatch statuses reject. Its instruction, block, function,
span, work and HCIRVM0018 message match the original operation. The existing
generic host bridge carries the status; its implementation remains unchanged.

Unknown pointer/offset reads retain HCIRVM0012. Earlier formation/access bounds
and scaling/offset overflow retain HCIRVM0019/HCIRVM0020. A failed left operand
stops right-side effects. Reached output, formatting work and attempted
instruction work survive faults. Ordinary numeric subtraction and owned
pointer-minus-integer keep their existing behavior.

## Preparation, source and verification

Byte-sized retained differences execute operands once and preserve the saved
word. Wider retained differences still encounter HCRUN0006's constant-divisor
initializer optimizer gate. Closed native owned-reference preparation also
remains HCRUN0006; native retained publication remains HCPP0008. These boundaries
are tested separately under #685/#696/#697/#704. No general initializer
division or optimizer admission is added.

Difference adds no callee activation or descriptor allocation per evaluation.
Runtime, preparation, storage, arena, descriptors, calls, output, frames and
code remain bounded. Native frames retain 4,088 bytes and code 65,536 bytes.

The reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/PrsExp.HC:14-62` supplies subtraction and optional pointee-size
division. Its expression stack retains the final producer class.
`OptLib.HC:96-179` forwards raw classes; `OptPass012.HC:403-440,619-695`
handles division/subtraction. `OptPass789A.HC:524-530,559-566` dispatches
`BackA.HC:166-222,355-370`. `Kernel/KernelA.HH:1573` declares signed RT_PTR.
For byte-sized pointees the source keeps a raw pointer node class. Hosted I64
transport gives that difference no pointer authority and does not establish
complete source class-query or raw-address parity.

Eight source groups and seven native groups check 97 independent values and
16 faults across all nine types, both modes and targets. They cover signed
offsets, one-past references, aliases, rows, snapshots, loops, recursion,
operand/call effects, byte/wide IR shape, original results and fault work,
source and native status authority, preparation boundaries, both ABI images,
fresh execution and exact/one-below runtime, preparation, code and frame limits.
Maintained CLI examples check output, result replacement and reached faults.

Raw address difference/class-query parity, mismatched pointees,
integer-minus-pointer, casts, null/integer conversions, compound updates,
returned/escaping/persistent pointer variables and deeper/aggregate pointers
remain under #687/#699/#700/#702. Full optimizer, larger native images,
retained tasks, artifacts, BIN/loader and bootstrap remain under
#696/#697/#703/#704/#682. No new TempleOS oracle capture is included.
