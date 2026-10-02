# Owned scalar pointer ordering

Issue #781 executes `<`, `<=`, `>` and `>=` for matching live owned
integer/Bool scalar references within the same original object and extent.
Compatible U8 literal spellings retain their existing contract. Both source
modes support IR, retained tasks and hosted native programs. Results are the
original I64 words 0 or 1; comparisons do not read pointees.

```text
holyc run --mode=jit --format=json examples/pointer-ordering.hc
holyc run --target=host-jit --mode=aot --format=json examples/pointer-ordering.hc
holyc run --mode=aot --format=json examples/stream-pointer-ordering.hc
```

The ordinary example returns I64 42 and captures `0:1:1:1:1;` with 152 runtime
instructions, no preparation and 26 formatting-work units in both modes and
targets. Print arguments evaluate right to left: the last comparison captures
the old `p`, then rebinds it before `Before(p,alias)` executes. The retained
example prepares its ordering once, changes the offset, and still returns 42.
Both IR modes use 122 runtime instructions, seven preparation instructions and
seven formatting-work units, with no captured output. Native retained
publication remains HCPP0008.

## One original object and extent

Each ordering captures its left reference before right-side effects. Rebinding
a pointer on the right cannot change that snapshot. Matching original live
objects, extents and scalar widths authorize comparison of their aligned byte
offsets. Equal, earlier, later and valid one-past references produce ordinary
I64 predicates. Unknown pointees are allowed because no cell is read. Access
through a one-past reference still faults at the later read or write.

Different objects or extents fault with HCIRVM0018 at the original ordering
instruction after reached operand effects. Scalar neighbors, adjacent arrays,
distinct recursive owners and separate original literal regions remain
distinct. This checked hosted rule does not order unrelated host addresses or
claim raw TempleOS placement. Rows and flattened addresses retain their full
original object extent. Repeated evaluation of one literal producer retains
its region; different producers retain different regions.

The VM validates both references and requires the same original storage,
base, count and scalar width before comparing offsets. Native code compares
original data-base, initialization-region and extent fields before reading
the offset fields. Separate source address sites can allocate distinct
canonical tables for the same object; record addresses do not determine order.
Existing synchronous call lifetimes and original owner checks remain required.

The completed source context seals original ordering instructions and their
transitive producers. Foreign contexts, copied records, operator changes and
type-compatible operand substitutions reject before entry. Current graph
matching also rejects numeric instructions rewritten into pointer comparisons.

Native `Pointer_object_mismatch` status 17 requires an original ordering site
in the sealed image and at least one attempted instruction. Numeric/equality
sites and zero-step mismatch statuses reject. The fault retains its original
instruction, block, function, source span and work. Its diagnostic is HCIRVM0018.
The OCaml emitter and status decoder implement this contract through the
existing generic host bridge.

Unknown pointer/offset values retain HCIRVM0012 at their original reads. Prior
formation/access bounds and scaling/offset overflow retain HCIRVM0019 and
HCIRVM0020. A failed left operand prevents right-side effects. Reached output,
formatting work and attempted instruction work survive each fault. Ordinary
numeric comparisons retain their existing signed/unsigned rules.

Ordering adds no callee activation or descriptor allocation per evaluation.
Runtime, preparation, storage, arena, descriptors, calls, frames, code and
output limits remain checked. Native frames retain the 4,088-byte limit and
code the 65,536-byte limit. Closed native defaults requiring runtime owned
ordering remain HCRUN0006 under #685; retained native tasks remain under #704.

## Source and verification

The reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Compiler/PrsExp.HC:14-62,218-229` retains ordinary comparison operands and the
separate chain stack. `OptLib.HC:96-179` forwards raw operand classes.
`OptPass012.HC:741-827` handles all four operators and I64 result transport.
`OptPass789A.HC:573-596` selects `BackB.HC:102-200`'s complete ICCmp path.
`Kernel/KernelA.HH:1573` declares signed RT_PTR. The hosted path preserves
IC_LESS/IC_LESS_EQU/IC_GREATER/IC_GREATER_EQU and compares valid offsets within
one original object; raw addresses and the full optimizer remain separate.

Seven source groups and seven native groups check 246 independent values and
19 fault cases in both modes and targets. They cover all nine pointee widths,
every operator, unknown pointees, scalar/interior/backward/one-past references,
aliases across sites, rows, recursion, operand/call effects, original I64
results, fault origin/work, original/foreign/copied/transitive/numeric-forgery
authority, retained defaults, both ABI images, fresh execution and
exact/one-below runtime, preparation, code and frame limits. Native status
tests reject mismatches at unauthorized sites. Maintained CLI fixtures check
captures, operand order, mismatches, bounds/overflow and once-only defaults.

Pointer difference, raw address ordering, chained pointer comparisons,
null/integer conversions, casts, mismatched pointees, compound updates,
returned/escaping/persistent pointer variables, deeper pointers and aggregate
pointees remain under #687/#699/#700. Full native ABI, larger images, retained
tasks, artifacts, BIN/loader and bootstrap acceptance remain under
#702/#703/#704/#682. No new TempleOS oracle capture is included.
