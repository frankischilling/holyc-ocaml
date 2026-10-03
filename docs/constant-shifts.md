# Constant integer shifts

The pinned compiler combines nested constant counts before x86 masks the final
count. It also forwards the remaining word's computation class after removing
a constant count. The native fixture in
[constant-shifts.json](../test/oracle/constant-shifts.json) confirms both rules.

`Ir.Integer_interpreter` and the hosted x86-64 selector execute the canonical
`IC_SHL_CONST` and `IC_SHR_CONST` forms. Each has one full-width integer operand,
one internal `I64` or `U64` result, an integer count payload and zero flags. The
result class must equal the forwarded computation class of the operand. A
complement can retain an unsigned computation class while declaring `I64`;
the constant shift follows that computation class. Narrow values, pointers,
floating values, public result types and flagged forms remain rejected.

The payload retains all 64 bits. Execution masks it with 63. Left shift wraps
at 64 bits. Right shift is arithmetic for an `I64` result and logical for `U64`.
The native selector emits `D1 /4`, `/5` or `/7` only when the complete stored
count equals one. Every other count uses `C1` and its low byte. Count 65
therefore emits immediate `41`, and `0x8000000000000001` emits immediate `01`
through `C1`, even though both execute as count one. Constant shifts need no
count value or temporary reservation of RCX. Existing register, frame, code
and instruction limits still apply.

## Native observations

The October 2, 2026 capture uses the canonical final ISO. Its SHA-1 is
`1a1ec79990e21fa3d66ac680009da63d3ac512b0`; its SHA-256 is
`3c68ca395d0b64f779f1ffa330e2685798b17862e3c36ae4e349537fd676b40b`.
QEMU 9.2.0 runs one TCG CPU with 512 MiB, no network or persistent disk and
read-only boot media. The fixture records commands, original screen hashes,
49 result fields repeated twice within one boot and 15 disassembly prefixes.
Cross-boot stability is not claimed. Values were read from the original pixels
with the pinned font table and key results were checked visually. `%X` exposes
bits; the recorded computation classes come from the compiler source audit.

For a variable `I64 x=-7`, `(x<<63)<<1` returns -7 and emits `48 C1 E0 40`.
`(x>>63)>>1` also returns -7 and emits `48 C1 F8 40`. Changing the outer count
to 65 produces immediate `80`, and both results remain -7. The equivalent
variable-count left shifts return zero; variable-count arithmetic right shifts
return -1. Fully constant `(-7<<63)<<1` returns zero, and
`(-7>>63)>>1` returns -1. Those children fold before a nested unary count can
be merged. A source optimizer must distinguish the constant and variable-word
contexts instead of applying one expression identity to both.

For a variable `I64 x=-7`, `x>>0x8000000000000001` emits SAR and returns -4.
Using a variable `U64` count with the same bits instead emits SHR and returns
`0x7FFFFFFFFFFFFFFC`. The fully constant `-7>>0x8000000000000001` also returns
that positive value through common-class constant folding.

The enclosing `(x>>0x8000000000000001)<0` returns zero despite the SAR result.
Its capture contains `49 C1 F8 01` followed by an unsigned `JAE` decision.
The comparison retains `ICF_USE_UNSIGNED` from its earlier operand classes;
the later unary class fix does not clear it. Recomputing every parent's
behavior solely from the surviving word class would change this result.
The hosted canonical projection represents this decision with an existing
`U64` word view before the comparison. It does not admit nonzero comparison
flags. Public program lowering now uses the same representation.

One original unsigned-left definition command lost a Shift modifier during
input delivery. Its source capture contains `O574LU639U64 x)` rather than
`O574LU63(U64 x)`. The definition and its two failed result captures are
explicitly excluded. Seven slower, individually delivered definitions with
fresh names and two successful result commands replace them. Failed hypervisor
and relay startup attempts are recorded separately and supply no native data.

## Producers and consumers

All source locations refer to `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

| Stage | Source | Contract |
| --- | --- | --- |
| Opcode and arity | `CompilerA.HH:68-71`, `CInit.HC:60-63` | Raw shifts have two operands; constant shifts have one. |
| Class selection | `OptLib.HC:96-179,196-225` | Raw binary shifts use the common class; unary forms forward the operand class. |
| Immediate shifts | `OptPass012.HC:193-266` | Fold constant children; replace an immediate count; combine same-direction unary counts by wrapping addition before masking. |
| Strength reductions | `OptPass012.HC:337-356,403-455` | Power-of-two multiplication and division can also produce constant shifts; their complete policy remains #585. |
| Cleanup and addressing | `OptPass3.HC:405-406`, `OptPass4.HC:27-34,302-344,599-601` | Retain forms through cleanup and consume left shifts during addressing selection. Full LEA/SIB optimization is separate. |
| Immediate emission | `OptPass789A.HC:676-687`, `BackA.HC:573-601`, `OpCodes.DD:1103-1156` | Pass the full count to SHL, SHR or SAR; choose the one-count encoding before byte truncation. |
| Comparison decision | `OptPass012.HC:741-827`, `BackB.HC:158,193`, `KernelA.HH:1609` | An unsigned comparison flag can survive later operand class changes. |

## Source integration

Public `holyc run` programs rewrite checked full-width integer shift plans before
allocating IR identities or sealing source authority. The rewrite reaches
ordinary expressions, conditions, returns, provided fixed/variadic and internal
arguments, output arguments, and automatic/global/static initializer providers.
Raw fragment APIs keep their default behavior; callers can explicitly request
`optimize_shifts` where they own the checked publication path.

A literal or negated literal count becomes a complete 64-bit payload. Fully
constant children fold first with their common computation class and their own
masked counts. A remaining variable-word shift forwards its operand class.
Same-direction nested counts add with 64-bit wrapping before masking; opposite
directions and variable counts stay separate. Parentheses and unary plus retain
aliases. Calls and reads remain in source order and execute once, even when an
intermediate pure shift or count disappears. Following arithmetic, unary and
comparison consumers use the surviving computation class. An established
unsigned comparison keeps its unsigned word view.

Public call results and explicit casts keep their checked computation class
until the consuming operation forwards it. For example, `(-Word())>>1` with
`U64 Word(){return 7;}` and `(-7(U64))>>1` both return
`7FFFFFFFFFFFFFFC`. An ordinary `U64` parameter in `(-x)>>1` returns
`FFFFFFFFFFFFFFFC`: its variable producer has already forwarded to the internal
unsigned class, which negation changes to signed. The shift then forwards that
surviving class. Normalizing a public call or cast before negation instead
changes this decision and produces an invalid typed sequence.

`test/oracle/public-shift-classes.json` records these three values and the
following unsigned comparison with zero, each captured twice in one native
boot. Its function listings confirm arithmetic shift for the variable, logical
shift for the call and a folded immediate for the cast. Listings stop at the
first return. The additional division field in those result commands stays
outside this fixture's projections under #585. The source, CLI and native tests
check both source modes; native tests compile both status ABIs, replay fresh
images and enforce exact and one-below instruction allowances.

`examples/public-shift-classes.hc` returns I64 42 through `run --target=ir` and
`run --target=host-jit` in both source modes.

Checked calls still require the original argument source class and span. When a
rewrite changes that class, the composer appends a full-width word view before
pushing the argument. The view preserves bits and costs one IR instruction.
Sealed runtime contexts retain each canonical shift and its transitive input
records. Changed payloads, flags, types, spans, operands, copied producers,
foreign contexts and shifts introduced after sealing cannot acquire authority.

Fully constant shifts can now prepare global/static leaves and defaults as
immediate words. Narrow destinations apply their existing storage conversion;
this does not authorize narrowing the shift itself. Live retained calls may
contain those folded words and still execute their other effects once. The
original declaration/leaf/default completion proofs, source ownership and
cumulative allowances remain required. Original full-word constant shifts and
supported scalar right-shift updates also prepare through published retained
callees. See [retained shift preparation](prepared-integer-shifts.md).
Unsealed/raw preparation shifts retain `HCRUN0006`; native retained frontend
publication retains `HCPP0008`. Closed native initializers still reject reads
and effectful calls.

The fixture keeps the historical raw baseline at
`3ba28b0da0e9f93dae3978f51cef5f4897f3e897`: six fields (RH1, LN64, RN64, UN64,
LN128 and RN128) differed in 24 of 196 mode/target controls. All 49 captured
fields now match through JIT/AOT and interpreted/native program execution. The
new source and native suites also check following consumers, original ordering,
source mutations, fresh replay, reached output/faults and exact/one-below
runtime, preparation and image allowances. Each target retains its own
preparation accounting. The interpreted static preparation harness charges four instructions here;
native closed leaves charge three. JIT default source composition can
retain an additional class view that native prepared arguments do not need.

The pass skips plans with shared comparison links, indexed/pointer address
plans, pointer differences, array materialization, function addresses or current
position. Narrow, pointer and floating operands, casts that would supply a
constant count, arithmetic count folding, compound reductions and LEA/SIB
selection need separate evidence and implementation. The broader optimizer,
division/remainder policy, TempleOS loading and bootstrap remain under
#585/#696/#697 and their dependent gates. Issues #574 and #787 cover the
canonical consumers and this source integration.

[constant-shifts.hc](../examples/constant-shifts.hc) returns 42 and prints
`-7:-4:0:0;`. [stream-constant-shifts.hc](../examples/stream-constant-shifts.hc)
checks a folded retained default's once-only effects and submits `42;` through
`StreamPrint`.

Run the focused checks with:

```powershell
opam exec -- dune exec --root . test/test_main.exe -- test 'constant shift policy'
opam exec -- dune exec --root . test/native/test_constant_shift_execution.exe
opam exec -- dune exec --root . test/test_main.exe -- test 'source constant shifts'
opam exec -- dune exec --root . test/native/test_source_constant_shift_execution.exe
```
