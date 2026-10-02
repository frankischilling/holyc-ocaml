# Owned ModU64 calls

Issue #767 connects the original `_intern IC_MOD_U64` declaration to IR,
retained source tasks and hosted native execution. `ModU64(q,d)` reads the
complete word at `q`, divides it as unsigned, writes the quotient through the
same pointer and returns the remainder as U64. An I64 object supplies its raw
64 bits and keeps its original declared type and pointer identity.

```text
holyc run --target=ir --mode=jit --format=json examples/internal-mod-u64.hc
holyc run --target=host-jit --mode=aot --format=json examples/internal-mod-u64.hc
holyc run --mode=aot --format=json examples/stream-internal-mod-u64.hc
```

The ordinary example divides an I64 object containing 442 by 10, stores 44,
captures `44:2;` and returns I64 42. Both modes and targets use 52 runtime
instructions, no preparation and twelve formatting-work units. The retained
example prepares a default once, writes its original quotient, then reuses
the saved remainder after another object write. It generates `42;`, returns
I64 42 and captures no output. Both IR modes use 102 runtime instructions,
nine preparation instructions and seven formatting-work units.

## Calls, storage and faults

The original published numeric binding 0xAF, exact `U64(U64*,U64)` signature,
two provided arguments and canonical call markers authorize the operation.
Parameter indices distinguish the pointer from the divisor. Both producers
retain their original type, source origin and right-to-left evaluation order.
Renamed numeric declarations work; ordinary same-name functions execute their
bodies. Foreign contexts and changed operands, flags, payload, result type or
opcode reject before execution.
Preflight also requires the original immutable argument producer records,
including their source spans. Replacing a producer cannot reuse its sealed ID.

Only live owned I64/U64 objects enter the pointed-word path. A copied pointer
still identifies its original object, extent, offset and activation. The
operation checks a complete eight-byte cell and its initialization before
division. A zero divisor faults before storing a quotient or completing the
call. Unsigned high-bit inputs do not acquire signed division overflow.
Reached output and attempted instruction work survive faults. Each operation
consumes one IC tick; arguments and call markers retain their ordinary work.
Native code uses the existing checked load/store and unsigned DIV encoder,
private result staging, bounds/initialization checks and image/frame quotas.

The signed-object admission is specific to this operation. General pointer
casts, byte reinterpretation, deeper pointers and escaped references remain
under #687/#699. Ordinary native closed default preparation rejects these
calls with HCRUN0006. Native retained frontend execution remains under #704
and reports HCPP0008 for the retained example.

## Source and verification

The reference is `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
`Kernel/KernelB.HH:103` supplies the signature;
`Compiler/CompilerA.HH:227` fixes 0xAF;
`Compiler/PrsExp.HC:440-586` supplies argument and internal call phases.
`Compiler/BackC.HC:463-488` reads the pointer, performs unsigned division,
stores the quotient and returns RDX. `OptPass789A.HC:878-879` dispatches it.
`Kernel/StrPrint.HC:217-219,449,489,657,764` uses both U64 and I64 objects for
digit extraction. `Kernel/KDate.HC:40-68` chains four divisions of an I64
object for date conversion.

Tests check independent quotient/remainder pairs, all 64 divisor bits,
high-bit words and divisors, signed object storage, narrow computed versus
stored divisors, interior aliases, recursion, nested calls, pointer rebinding,
argument effects, actual caller patterns and retained mutation. Both modes,
public CLI targets, both native ABIs, repeated fresh images and exact/one-below
runtime/preparation/code/frame controls are covered. Negative controls check
source signatures, pointer domains, original/foreign/changed call authority,
uninitialized storage, one-past/beyond objects and zero-divisor fault ordering.
These are source-derived expectations and hosted execution; no new TempleOS
oracle capture is included. Full formatting, runtime, native retained, ABI,
artifact, loader and bootstrap requirements remain open under #694, #695,
#704, #702 and #682.
