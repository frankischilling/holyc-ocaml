# holyc-ocaml roadmap

GitHub milestones M0 through M10 hold measurable exit criteria. Current implementation work is in M5: IR and interpreter. Earlier milestone gaps, including compile-time execution, remain tracked in their original issues.

M0 covers the build, source model, diagnostics, pinned reference, first lexer slice, tests, and CI. Later milestones cover the source audit, integrated preprocessor, parser, semantic model, verified IR, interpreter, hosted x86-64 backend, TempleOS assembler, `.BIN` and JIT support, whole-tree compatibility, bootstrap work, and the 1.0 release.

M6 now has a bounded source-to-native expression gate in issue #642. The own
encoder and explicit Windows/Linux x86-64 execution path cover bounded
I64/U64 expressions; general call frames, calls, memory, branches, floating point,
relocations and object/BIN output remain open backend work.
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
fault channel across the Windows/System V host boundary. The full source branch,
call, memory and floating-point backends remain open.

This file does not mark planned work as implemented. Current support is listed in the README and generated compatibility reports.
