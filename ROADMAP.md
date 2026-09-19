# holyc-ocaml roadmap

GitHub milestones M0 through M10 hold measurable exit criteria. Current work spans M5 (IR and interpreter) and the bounded M6 hosted x86-64 backend. Earlier milestone gaps, including compile-time execution, remain tracked in their original issues.

M0 covers the build, source model, diagnostics, pinned reference, first lexer slice, tests, and CI. Later milestones cover the source audit, integrated preprocessor, parser, semantic model, verified IR, interpreter, hosted x86-64 backend, TempleOS assembler, `.BIN` and JIT support, whole-tree compatibility, bootstrap work, and the 1.0 release.

M6 now has a bounded source-to-native expression gate in issue #642. The own
encoder and explicit Windows/Linux x86-64 execution path cover bounded
I64/U64 expressions, with later increments below connecting control flow and
direct scalar functions. General memory, floating point, relocations and
object/BIN output remain open backend work.
Issue #644 extends that gate with six integer comparisons and logical NOT.
Issue #646 adds eager binary logical values and ordinary comparison chains,
including corrected forwarded computation classes in the shared lowerer.
Conditional and multiple-pending-reduction chains remain separate work.
Issue #648 adds reusable private spill slots, a separate frame quota and
Windows unwind registration for high-pressure expressions. Larger/probed
frames and the complete HolyC register-allocation and call ABI remain required.
Issue #652 adds left and arithmetic/logical right shifts, including masked
counts and fixed-RCX transport with preserved live values. Issue #654 adds
guarded signed and unsigned division/remainder plus a private checked arithmetic
fault channel across the Windows/System V host boundary.
Issue #657 extends the shared backend to closed source programs with structured
branches and loops, a generated per-IR step budget, exact fault sites and one
bounded spill frame. The public `run --target=host-jit` path compiles the entry
and retains native progress without interpreting ordinary source commands.
Issue #659 connects source-defined fixed I64/U64 functions, automatic scalar
storage and direct generated calls. It preserves checked body/frame/call identity,
caller live values, nested argument staging and per-activation initialization.
Generated checks bound simultaneous semantic frames, named-call depth and
physical native stack bytes; checked faults unwind the complete call chain.
Issue #660 connects bounded, source-owned scalar I64/U64 defaults. Every admitted
default prepares at its original header callback, including unused functions
and functions called with explicit arguments. Calls reuse saved values; exact
header/preparation/call evidence controls native admission. Preparation work and
saved payload bytes have explicit limits and survive later failures in reports.
Default-bearing definitions must precede executable top-level statements.
Effectful defaults, interleaved declaration execution, owned strings, `lastclass`
and broader default types remain separate work, alongside storage and full-ABI
requirements. See [native defaults](docs/native-defaults.md).
Issue #663 extends the same pipeline with I8/U8/I16/U16/I32/U32 storage and
signatures, narrow constant defaults, and U0 procedure completion. Declared-width
loads/stores remain distinct from full register results; U0 completion cannot
be consumed as a word or leave a stale top-level result. Original frame ranges,
call/default authority and all resource limits remain checked. See
[native scalar functions](docs/native-scalars.md).
Issue #664 connects existing function-local label resolution and goto fragments
to shared executable blocks. Both interpreter and native source execution use
the same owned occurrences and reserved label targets; labels are structural
boundaries and gotos use the existing checked jump. Source order stays separate
from `for` update execution order, and skipped initialization, return completeness
and resource limits remain checked. See [goto execution](docs/integer-goto.md).
Issue #668 connects original closed case preparation and bounded integer switch
dispatch to both execution targets. The shared graph preserves range holes,
fallthrough, nested breaks and source ownership. Preparation nodes and cumulative
table slots have independent bounds; native dispatch uses the existing encoder
and retains one metered IC site. No-bound/sub-switch regions and effectful case
evaluation remain separate gates. See [switch execution](docs/integer-switch.md).
The complete HolyC ABI, persistent and pointer memory operations, F64/x87 and
conversions, runtime output, and general declaration/`#exe` native integration
remain open. Optimizer parity, assembler and object/BIN output, actual TempleOS
loader acceptance, whole-tree compilation and bootstrap retain their own gates.

This file does not mark planned work as implemented. Current support is listed in the README and generated compatibility reports.
