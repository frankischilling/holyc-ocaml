# Retained integer shift preparation

Retained initializer calls prepare original full-word canonical constant shifts
and supported scalar right-shift updates in both source modes. This includes
the literal power-of-two division reductions described in
[integer division](integer-division.md). For signed `x=-7`, a retained `x/2`
default saves `-4`, while the constant expression `-7/2` saves `-3`.

Run `holyc run examples/stream-prepared-integer-shifts.hc`. It returns I64 `42`
from a default, global and static initializer, each saved before the source
global changes. Its source assertions produce no output. Both modes use 199
runtime instructions, seven preparation units and seven formatting-work units.
The example rejects a runtime limit of 198 or preparation limit of six.

## Native observations

At pinned reference `c26482bb6ad3f80106d28504ec5db3c6a360732c`,
`Compiler/PrsVar.HC:1-108` compiles and calls declaration expressions.
`OptPass012.HC:403-455,838-854` supplies division reductions, and
`OptLib.HC:96-225` supplies class forwarding.
`OptPass789A.HC:615-630` dispatches compound shifts.
`BackA.HC:573-661` computes shifts at full word width and stores addressed
results at the declared width.

[The fixture](../test/oracle/prepared-integer-shifts.json) preserves 32 accepted
commands and 64 source/result image hashes from an isolated read-only ISO boot
on 2026-10-03. Nineteen primary fields each repeat within that boot. Default,
global and static division results stay `-4` after changing the source global;
their three original effects do not replay. Signed and unsigned division,
merged counts, left/right shifts and compound shifts retain their captured
values. Both register-candidate and addressed I8/I32 `/= 2^32` return `-1`.
Machine listings end at the first return.

Two repeated default-compilation probes raise native `DivZero` after one
counter increment. The corresponding hosted retained source reports
`HCIRVM0009` from the original callee during preparation. Earlier output
survives reached faults. These observations establish values and phases within
one boot; they do not establish cross-boot or general compiler compatibility.

## Original authority and narrow storage

`Runtime_call_context.original_preparation_shifts` first checks the current
original source bundle and transitive producers. It collects only original
zero-flag full-word `IC_SHL_CONST`/`IC_SHR_CONST` descriptions, or original
scalar `IC_SHR_EQ` with an operand-free internal I64 constant count from 0 to
63. Canonical shift payloads retain their complete 64-bit counts; the existing
consumer masks them at execution.

The driver uses the retained callee's published context and exact description
identity. Collection never constructs or reseals a context. Copied records,
foreign owners or contexts, changed payloads/classes/flags/spans/operands,
changed transitive inputs and new instructions after sealing cannot supply
authority. Actual execution checks the original bundle again.

Addressed narrow updates keep declared storage conversion. An ordinary narrow
register candidate qualifies only when every write preserves its declared
range. A bounded right shift preserves that range; an out-of-range assignment,
another unproved update or an explicit hardware register remains outside this
proof. The whole-graph check includes writes after the shift and dependent
locals, rather than assuming the first initializer determines later values.

## Verification and remaining work

Seven maintained source groups compare all nineteen native fields in JIT/AOT,
check once-only defaults/global/static/nested/live consumers, reached output
and faults, original/transitive authority, full count payloads, narrow range
invariants and exact/one-below resource limits. Public CLI regressions compare
all 38 mode/field projections and the measured example. Existing native
operation and source-authority tests remain required.

Raw variable shifts, unsupported compound counts, direct or unsealed
preparation expressions and unproved narrow/numeric/flagged domains retain
`HCRUN0006`. General literal division and modulo admission is still guarded.
Shared nonconstant power-of-two division and plain modulo-mask plans remain
raw under #585/#696/#697. Native retained frontend publication retains
`HCPP0008` under #704. Broader declaration preparation remains under #685;
native exception delivery, artifacts, actual BIN loading and compiler bootstrap
retain their existing acceptance requirements.
