# Retained scalar pointer-difference preparation

Issue #785 prepares wider scalar pointer differences reached through retained
calls. The original completed callee context authorizes its own size division.
All nine integer/Bool scalar types preserve signed element counts, operand
effects and once-only default evaluation in both IR source modes.

```text
holyc run --mode=jit --format=json examples/stream-pointer-difference-wide.hc
holyc run --mode=aot --format=json examples/stream-pointer-difference-wide.hc
```

The example saves `42+(p-Q)` at declaration time, then changes the offset and
resets the call counter. Both later omitted arguments retain 42 without
reevaluating the default. It returns I64 42 with 124 runtime instructions,
seven preparation instructions, seven formatting-work units and no captured
output. The byte-sized example keeps its 122 runtime instructions and the same
preparation/output work. The extra size constant and division belong to the
callee's runtime instruction accounting.

## Original context and width proof

At reference `c26482bb6ad3f80106d28504ec5db3c6a360732c`,
`Compiler/PrsExp.HC:14-62` emits original byte IC_SUB followed by size
IMM_I64/IC_DIV above byte width. `OptPass012.HC:403-440` rewrites power-of-two
division to IC_SHR_CONST. `OptPass789A.HC:682-688` and `BackA.HC:573-601`
select arithmetic shifting for the signed I64 result.

Valid reached operands have matching scalar types, aligned offsets and one
live object and extent. Their byte-offset difference is an exact multiple of
width 2, 4 or 8. Signed division and arithmetic shift therefore produce the same
element count for positive, negative and zero differences. This bounded value
proof does not implement the general optimizer rewrite or establish source
class-query and raw-address parity.

`Runtime_call_context.original_pointer_difference_divisions` validates the
current original/transitive graph records and collects original division
descriptions once per callee. The division and subtraction must have numeric
internal I64 targets, zero flags and no payload. The original size producer
must be a zero-flag, operand-free I64 constant equal to both original one-level
scalar widths, and its value must be 2/4/8.

The guard requires exact membership of the original division description.
Copies, foreign contexts or owners, altered sizes/flags/types and compatible
operand replacements cannot supply authority. Collection does not construct
or reseal a context. A previously collected record is a snapshot; actual call
execution rechecks the original source bundle and rejects later graph changes.

## Effects, limits and remaining gates

The driver uses the retained callee's already-completed context. It preserves
left-before-right reference capture, aliases, nested/recursive call behavior
and saved numeric results. No pointee read, reference authority, descriptor
allocation or larger resource limit is introduced.

Unknown pointers retain HCIRVM0012; formation bounds and scale/offset overflow
retain HCIRVM0019/HCIRVM0020. Different objects retain HCIRVM0018 at original
IC_SUB. A failed left operand prevents right-side effects. Earlier output,
formatting work, preparation and attempted instruction work survive failure.
Exact limits pass, and one-below runtime, preparation, output-byte and
output-work limits reject through their existing diagnostics.

Arbitrary constant division/modulo, shifts, outer user division, byte-sized
difference divided by a user size and direct expressions without a completed
callee context retain HCRUN0006. Native closed owned-reference defaults retain
HCRUN0006; native retained frontend publication retains HCPP0008. General
optimizer and declaration preparation remain under #696/#697/#685, and native
retained publication remains under #704.

Seven source groups and public CLI checks cover 53 independent saved values,
all nine types, forward/backward/equal/interior/one-past offsets, snapshots,
rows, loops, aliases, nested and recursive calls. Authority controls include
original/copied/foreign/current/transitive records and changed size, flags,
targets, operand order and compatible replacements. Four fault phases, reached
output and exact/one-below limits are checked in both modes. The ordinary IR
and native pointer-difference controls remain maintained.

Broader pointer and ABI domains, source raw class/address behavior, larger
images, artifacts, actual BIN/loader and bootstrap remain under
#687/#699/#700/#702/#703/#682. These source-derived and hosted results include
no new TempleOS oracle capture.
