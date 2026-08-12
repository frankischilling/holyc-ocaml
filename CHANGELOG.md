# Changelog

## Unreleased

- Established the `holyc-ocaml` Dune workspace and public `holyc_lib` library.
- Pinned TempleOS reference commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.
- Added byte-oriented source tracking, structured diagnostics, and the first streaming lexer slice.
- Added human and JSON token dumps with focused unit, golden, negative, and property tests.
- Added a bounded `#include` token stream with canonical hosted path checks, nested source provenance, include backtraces, a public library entry point, and the `holyc preprocess` command.
- Added TempleOS `#define` text capture and expansion with session-scoped lookup, redefinition history, replacement source maps, invocation and declaration provenance, deterministic dumps, and explicit recursion, nesting, and generated-byte limits.
- Added lexical scans that continue through exhausted include and definition frames, with ordered token and trivia source segments, multi-source diagnostics, exhaustive compound-operator split tests, and a matching native TempleOS oracle fixture.
- Added nested `#ifjit` and `#ifaot` branch selection with explicit compilation modes, raw inactive-branch scanning, cross-frame conditional state, and stable mismatch diagnostics.
- Added `#ifdef` and `#ifndef` with a session-owned model of TempleOS hash kinds, checked keyword and internal-type seeds, import filtering, local-variable shadowing, deterministic visibility dumps, and streaming symbol updates.
- Added deterministic `#if` evaluation for integer, character, and floating literals, definition-expanded constants, `defined`, HolyC precedence and comparison chains, 64-bit target arithmetic, retained branch lookahead, provenance-aware diagnostics, and a configurable expression-size limit.
- Added constant `#assert` evaluation with nonfatal `HCPP0024` warnings, retained token output, a diagnostic-preserving library result, and human and JSON CLI golden tests.
- Added source-ordered `#help_index` and `#help_file` metadata with explicit continuation handling, hosted path confinement without target reads, include and definition provenance, versioned dumps, and complete coverage of the pinned `KernelC.HH` directives.
- Added a checked parser and deterministic generated table for all 106 registers, 325 canonical opcodes, 49 aliases, and 924 instruction forms in the pinned `OpCodes.DD`, then exposed every register and opcode spelling to symbol conditionals.
- Added deterministic, provenance-carrying expansions for `__DATE__`, `__TIME__`, `__LINE__`, `__CMD_LINE__`, `__FILE__`, and `__DIR__`, with explicit CLI overrides and shared generated-text limits.
- Added deterministic full-tree lexer corpus reports over the exact pinned Git blobs. All 528 `.HC`, `.HH`, and `.PRJ` files tokenize without a diagnostic or crash, with NUL terminators and trailing payload bytes reported explicitly.
- Added the first spanned AST and parser for empty input and ordered direct primitive global variables, with integrated preprocessing, provenance-aware `HCPARSE` diagnostics, deterministic human and JSON dumps, library entry points, and `parse` and `dump-ast` commands.
- Added explicit pointer layers for primitive global declarations, including the pinned four-star limit, definition-generated source provenance, stable depth and function-pointer diagnostics, and human and JSON CLI goldens.
- Added comma-separated primitive globals with a shared base type, independent pointer depths, explicit delimiter provenance, streaming symbol publication, and human and JSON CLI goldens.
- Added source-linked `public` and `static` prefixes on primitive globals, preserving their order and definition provenance without claiming linkage or storage behavior.
- Added source-linked `extern` and AOT-only `import` bindings on primitive globals, with a stable JIT rejection that does not publish the rejected name.
- Added source-linked target identifiers for `_extern` and AOT-only `_import` primitive globals, with a stable missing-target diagnostic and no symbol publication after a JIT rejection.
- Added bound primitive function prototypes for ordinary and alternate-name externs and AOT imports, including return pointers, empty or variadic parameter lists, optional parameter names, punctuation provenance, function symbol publication, deterministic dumps, and explicit diagnostics for deferred parameter forms.
- Added ordered `interrupt`, `haserrcode`, `argpop`, and `noargpop` syntax on bound prototypes, with source provenance and tests against the checked TempleOS staging transitions.
- Added `reg` and `noreg` qualifiers on fixed and variadic function parameters, including explicit U64 register names, source-positioned AST nodes, generated-source provenance, and stable diagnostics for misplaced qualifiers.
- Added recursive function-pointer parameters with distinct return and declarator depths, empty, fixed, variadic, and nested signatures, exact punctuation provenance, deterministic dumps, and a checked hosted nesting limit.
- Added source-positioned default expressions to ordinary and nested function parameters, including non-trailing defaults, every audited binary operator, HolyC's unary-minus power rule, deterministic AST dumps, and bounded malformed-input recovery.
- Added primitive global array declarators with ordered dimensions, an explicit unsized first dimension, shared core expression syntax, bracket provenance, deterministic dumps, and source-specific diagnostics for malformed suffixes.
- Added `_intern` bindings with source-positioned target expressions, JIT and AOT syntax coverage, pinned `KernelB.HH` prototypes, deterministic dumps, and bounded malformed-target diagnostics.
- Added parenthesized call expressions with explicit omitted argument slots, nested-call precedence, punctuation provenance, deterministic dumps, and distinct diagnostics for missing separators and closing parentheses.
- Added bracket index expressions with repeated and mixed postfix chaining, source-positioned brackets, deterministic dumps, and distinct diagnostics for empty or unclosed indexes.
- Added direct and pointer member expressions with explicit access kinds, source-positioned member names, mixed postfix chaining, deterministic dumps, and a dedicated missing-member diagnostic.
- Added source-shaped `sizeof` terms with matched wrapper parentheses, unresolved named and member targets, uncapped target stars, deterministic dumps, and diagnostics for malformed targets and forbidden direct suffixes.
- Added source-shaped `offset` terms with required member paths, repeated nested members, matched wrapper parentheses, deterministic dumps, and diagnostics for malformed targets and forbidden direct suffixes.
- Added source-shaped `defined` terms that retain one operand token, distinguish identifier-shaped names from other tokens, preserve repeated wrapper parentheses, and enforce the pinned restricted postfix state.
- Added a distinct `lastclass` parameter-default node for ordinary and nested function prototypes, with exact keyword provenance, deterministic dumps, and diagnostics that reject expression continuations after the keyword.
