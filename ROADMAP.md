# holyc-ocaml roadmap

GitHub milestones M0 through M10 hold measurable exit criteria. Current implementation work is in M5: IR and interpreter. Earlier milestone gaps, including compile-time execution, remain tracked in their original issues.

M0 covers the build, source model, diagnostics, pinned reference, first lexer slice, tests, and CI. Later milestones cover the source audit, integrated preprocessor, parser, semantic model, verified IR, interpreter, hosted x86-64 backend, TempleOS assembler, `.BIN` and JIT support, whole-tree compatibility, bootstrap work, and the 1.0 release.

M6 now has a bounded source-to-native expression gate in issue #642. The own
encoder and explicit Windows/Linux x86-64 execution path cover register-only
I64/U64 expressions; frames, calls, spills, memory, branches, floating point,
relocations and object/BIN output remain open backend work.
Issue #644 extends that gate with six integer comparisons and logical NOT.
Binary logical operations and comparison-chain class propagation remain
separate native work.

This file does not mark planned work as implemented. Current support is listed in the README and generated compatibility reports.
