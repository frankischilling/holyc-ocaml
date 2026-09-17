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
bounded spill frame. The public `run --target=host-jit` path uses a compile-only
source pipeline and retains native progress without an interpreter fallback.
Issue #659 connects source-defined fixed I64/U64 functions, automatic scalar
storage and direct generated calls. It preserves checked body/frame/call identity,
caller live values, nested argument staging and per-activation initialization.
Generated checks bound simultaneous semantic frames, named-call depth and
physical native stack bytes; checked faults unwind the complete call chain.
Issue #660 tracks preparing original declaration-time defaults before native
compilation and reusing their saved values at calls. The current native gate
rejects all defaults, including unused headers, rather than
silently skipping their effects. Broader scalar storage and full-ABI work must
continue through the same checked source/function/frame path.
The complete HolyC ABI, narrow and pointer memory operations, F64/x87 and
conversions, runtime output, and general declaration/`#exe` native integration
remain open. Optimizer parity, assembler and object/BIN output, actual TempleOS
loader acceptance, whole-tree compilation and bootstrap retain their own gates.

This file does not mark planned work as implemented. Current support is listed in the README and generated compatibility reports.
