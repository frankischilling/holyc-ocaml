# Changelog

## Unreleased

- Established the `holyc-ocaml` Dune workspace and public `holyc_lib` library.
- Pinned TempleOS reference commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
- Added byte-oriented source tracking, structured diagnostics, and the first streaming lexer slice.
- Added human and JSON token dumps with focused unit, golden, negative, and property tests.
- Added a bounded `#include` token stream with canonical hosted path checks, nested source provenance, include backtraces, a public library entry point, and the `holyc preprocess` command.
- Added TempleOS `#define` text capture and expansion with session-scoped lookup, redefinition history, replacement source maps, invocation and declaration provenance, deterministic dumps, and explicit recursion, nesting, and generated-byte limits.
- Added nested `#ifjit` and `#ifaot` branch selection with explicit compilation modes, raw inactive-branch scanning, cross-frame conditional state, and stable mismatch diagnostics.
- Added `#ifdef` and `#ifndef` with a session-owned model of TempleOS hash kinds, checked keyword and internal-type seeds, import filtering, local-variable shadowing, deterministic visibility dumps, and streaming symbol updates.
- Added deterministic `#if` evaluation for integer, character, and floating literals, definition-expanded constants, `defined`, HolyC precedence and comparison chains, 64-bit target arithmetic, retained branch lookahead, provenance-aware diagnostics, and a configurable expression-size limit.
- Added constant `#assert` evaluation with nonfatal `HCPP0024` warnings, retained token output, a diagnostic-preserving library result, and human and JSON CLI golden tests.
- Added source-ordered `#help_index` and `#help_file` metadata with explicit continuation handling, hosted path confinement without target reads, include and definition provenance, versioned dumps, and complete coverage of the pinned `KernelC.HH` directives.
