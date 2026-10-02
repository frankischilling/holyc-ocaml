# Owned scalar pointer equality

[Owned scalar pointer difference](pointer-difference.md) is implemented
by #783 with signed numeric results and sealed original size/division records.
Its preparation and raw-address boundaries remain explicit.

[Owned scalar pointer ordering](pointer-ordering.md) compares offsets within
one original live object/extent; unrelated objects retain a checked fault.

Issue #779 executes `==` and `!=` for live owned scalar references through IR,
retained tasks and hosted native programs in both source modes. Matching
integer/Bool pointees and the existing compatible U8 literal spellings are
supported. The result is the original I64 word 0 or 1.

```text
holyc run --mode=jit --format=json examples/pointer-equality.hc
holyc run --target=host-jit --mode=aot --format=json examples/pointer-equality.hc
holyc run --mode=aot --format=json examples/stream-pointer-equality.hc
```

The ordinary example returns I64 42 and captures `1:0:1:1:1;`. Both modes and
targets use 185 runtime instructions, no preparation and 26 formatting-work
units. The retained example prepares a comparison once, then changes its offset
operand. Both IR modes still return 42 with 122 runtime instructions, seven
preparation instructions and seven formatting-work units, with no captured
output. Native retained publication remains HCPP0008.

## Object identity and offset

Comparison captures its left reference before right-side effects. Rebinding a
pointer while evaluating the right operand cannot change that snapshot.
Equality requires the same original live object and logical offset; inequality
returns its complement. Different owned objects compare unequal, including an
object's one-past reference and another object's base. This checked hosted
identity rule remains separate from raw TempleOS address placement.

Comparing a reference does not read its pointee or require that cell to be
initialized. Valid interior and one-past references can be compared. A later
read or write still requires an actual original element. Globals/statics,
caller objects, scalar cells, arrays, row/flattened/multi-rank addresses and
literal regions retain their original extent and live owner. Separate original
literal producers retain separate regions; reevaluating one producer keeps its
region. Pointee mutation leaves reference identity unchanged.

The VM compares the original storage object, base, element count/width and
logical offset after validating each reference. Native code compares original
data-base, initialization-region, offset and extent fields. Different source
address sites can own different descriptor tables for the same object, so those
fields preserve aliases across sites, loops and recursive activations. The
existing stable records and synchronous owner lifetimes remain required.

The completed source bundle seals each original pointer comparison and its
transitive producers. Foreign contexts, copied records, type-compatible operand
substitutions and changed operators reject before either execution target
enters the program. Matching also checks that a currently supplied pointer
comparison came from an original comparison receipt: rewriting an ordinary
numeric operation cannot acquire reference authority.

Unknown pointer/offset values or reached pointee reads retain HCIRVM0012.
Original out-of-bounds formation/access and scaling/offset overflow retain
HCIRVM0019 and HCIRVM0020 at their own instructions. An unknown or invalid left
operand prevents right-side effects. Reached output, formatting work and
attempted instruction work survive faults. Ordinary numeric comparisons keep
their existing behavior.

Comparison adds no callee activation or descriptor allocation per evaluation.
Runtime, preparation, code, frames, storage, descriptors, arena, calls and output
limits remain checked. Native frames retain the 4,088-byte limit and code the
65,536-byte limit. Closed native defaults requiring owned pointer comparison
remain HCRUN0006 under #685; retained native tasks remain under #704.

## Source and verification

The reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/PrsExp.HC:14-62,218-229` keeps ordinary comparison operands and the
separate comparison-stack path. `OptLib.HC:96-179` forwards raw operand classes.
`OptPass012.HC:724-740,808-827` handles both equality operators and their I64
result transport. `OptPass789A.HC:567-572` dispatches to
`BackB.HC:102-200`'s ICCmp. The hosted path retains IC_EQU_EQU/IC_NOT_EQU and
compares owned identity through the existing encoder forms. Full optimizer and
raw-address/ABI behavior remain separate.

Tests check 146 independent values across all nine pointee classes, both
operators, original objects/offsets, unknown pointees, one-past addresses,
aliases, recursion and effectful operands in both modes and targets. Seven
source groups and six native groups cover original I64 results, original
faults/work, foreign/copied/changed/transitive producers, numeric-to-pointer
forgeries, retained defaults, both ABI images, fresh host execution and
exact/one-below runtime, preparation, code and frame limits. Maintained CLI
fixtures check values, captures, operand order, overflow/bounds faults and
once-only defaults.

Raw ordering, raw pointer difference, chained pointer comparisons, mismatched pointees,
null/integer conversions, casts, compound updates, returned/escaping/persistent
pointer variables, deeper pointers and aggregate pointees remain under
#687/#699/#700. Native retained tasks, larger native images and full ABI,
artifacts, BIN/loader and bootstrap acceptance remain under #704/#703/#702/#682.
Expectations come from the pinned source and the checked hosted object model.
No new TempleOS oracle capture is included.
