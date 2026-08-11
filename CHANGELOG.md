# Changelog

## Unreleased

- Established the `holyc-ocaml` Dune workspace and public `holyc_lib` library.
- Pinned TempleOS reference commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
- Added byte-oriented source tracking, structured diagnostics, and the first streaming lexer slice.
- Added human and JSON token dumps with focused unit, golden, negative, and property tests.
- Added a bounded `#include` token stream with canonical hosted path checks, nested source provenance, include backtraces, a public library entry point, and the `holyc preprocess` command.
- Added TempleOS `#define` text capture and expansion with session-scoped lookup, redefinition history, replacement source maps, invocation and declaration provenance, deterministic dumps, and explicit recursion, nesting, and generated-byte limits.
- Added nested `#ifjit` and `#ifaot` branch selection with explicit compilation modes, raw inactive-branch scanning, cross-frame conditional state, and stable mismatch diagnostics.
