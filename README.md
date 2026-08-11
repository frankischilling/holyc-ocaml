# holyc-ocaml

`holyc-ocaml` is the project for an OCaml implementation of the HolyC compiler. Its compiler command is `holyc`, and its OCaml library module is `holyc_lib`. Compatibility work is based on the TempleOS source tree at commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

The current build contains the source manager, byte spans, structured diagnostics, a handwritten streaming lexer, bounded `#include` and `#define` source frames, deterministic expansions for `__DATE__`, `__TIME__`, `__LINE__`, `__CMD_LINE__`, `__FILE__`, and `__DIR__`, constant `#if` and `#assert` evaluation, mode and symbol conditional selection, help-directive metadata, a spanned AST, the first declaration parser, and source-checked models of HolyC primitive types, compiler options, function flags, intermediate-code operations, the complete TempleOS opcode database, and TempleOS BIN records. The lexer handles identifiers, the complete language keyword list, integer and floating literals, strings, character constants up to eight bytes, nested comments, source line continuations, and the operators defined by the pinned compiler tables. During preprocessing, one lexical item can consume bytes from nested include, definition, and predefined-value frames. Tokens and trivia retain each contributing source segment, and cross-frame diagnostics point to every segment. The preprocessed token stream resolves quoted includes from a deterministic root list, expands TempleOS definition strings without adding C macro parameters, evaluates literal and definition-expanded expressions with the pinned precedence table, reports failed assertions without stopping preprocessing, selects nested `#ifjit` or `#ifaot` branches from an explicit mode, evaluates `#ifdef` or `#ifndef` against checked keywords, internal types, registers, canonical opcodes, aliases, and session-registered symbols, and records `#help_index` and `#help_file` in source order. It retains file, definition, invocation, and generated-value provenance and rejects path escapes, cycles, malformed conditional boundaries, excessive nesting, oversized inputs, excessive generated text, and oversized preprocessor expressions. The parser accepts empty input, direct primitive globals such as `I64 count;`, pointer globals such as `U0 **slot;`, and comma-separated groups such as `I64 first,*second;`. A group shares one primitive type while each declarator keeps its own pointer layers and comma or semicolon location. Human and JSON token, help-metadata, AST, and lexer-corpus reports are available.

`holyc corpus lex` verifies a clean reference checkout at the exact pinned commit and reads the committed Git blobs, so host line-ending conversion does not change the report. The current result is 528 of 528 `.HC`, `.HH`, and `.PRJ` files tokenized with no lexer diagnostic or crash. Fifty-four files contain a NUL after their textual prefix; the report records the terminator and all 1,266,852 trailing payload bytes. This is evidence for raw lexing only. It does not imply that the same files preprocess, parse, type-check, compile, or run.

The compiler-option and function-flag APIs are immutable specifications; no call checker, optimizer, or backend behavior is wired to them yet. The function-flag API does preserve the pinned modifier transitions, `RET1` boundary, and caller-cleanup predicate as pure operations for those later stages. The intermediate-code API defines opcode identity, argument shape, result count, structural type, and the two table flags, but it does not provide instructions, lowering, verification, optimization, or execution. The opcode API retains all source forms and aliases, but it does not parse operands, select a form, encode bytes, or disassemble. The BIN API records the exact header layout, record codes and shapes, relocation formulas, two-pass loader actions, and source-marked limitations. It does not serialize, parse, relocate, or load modules. Pointer stars are syntax only at this stage: the semantic layer does not yet resolve pointer types or perform pointer conversions. Array and function declarators, member layout, and expression checking are also unavailable. Native TempleOS can use calls, globals, memory, casts, and compiler state in `#if` and `#assert` because it compiles and executes an ordinary expression. This build rejects those nonconstant terms; [issue #33](https://github.com/frankischilling/holyc-ocaml/issues/33) tracks execution through the compile-time VM. General `#exe` execution and arbitrary generated source are not implemented. The six standard predefined values use a narrow source-checked expander instead of executing their `#exe` bodies. Hosted diagnostics for unmatched conditionals are deliberately stricter than the pinned lexer and tracked in [issue #27](https://github.com/frankischilling/holyc-ocaml/issues/27). Arrays, initializers, functions, aggregates, expressions, statements, the full semantic checker, executable IR, interpreter, assembler, native backends, JIT, and the TempleOS `.BIN` writer are unavailable. Unsupported nonempty parser input fails with an `HCPARSE` diagnostic; there is no raw-token AST fallback. There is no command that pretends to emit a module while later stages are absent. Progress and source evidence live in [the compatibility notes](docs/compatibility.md), [the language notes](docs/holyc-language.md), [the preprocessor notes](docs/preprocessor.md), [the oracle notes](docs/oracle-fixtures.md), [the assembler notes](docs/assembler.md), [the ABI notes](docs/holy-abi.md), [the IR notes](docs/intermediate-representation.md), [the BIN format notes](docs/templeos-bin-format.md), and [the traceability registry](reference/traceability.toml).

## Build

OCaml 5.1 or newer, opam, and Dune 3.12 or newer are required.

```text
git submodule update --init --depth 1
opam install . --deps-only --with-test
dune build
dune runtest
dune install
```

Run the compiler from the build tree:

```text
dune exec holyc -- version
dune exec holyc -- lex examples/lexer-tour.hc
dune exec holyc -- lex --format=json examples/lexer-tour.hc
dune exec holyc -- preprocess examples/include-tour.hc
dune exec holyc -- preprocess --mode=aot examples/mode-branches.hc
dune exec holyc -- preprocess examples/symbol-conditions.hc
dune exec holyc -- preprocess examples/constant-if.hc
dune exec holyc -- preprocess examples/assertions.hc
dune exec holyc -- preprocess --predefined-date=08/11/26 --predefined-time=05:42:17 --command-line-source test/cli/predefined-values.hc
dune exec holyc -- preprocess --dump-help-metadata --templeos-root=third_party/TempleOS third_party/TempleOS/Kernel/KernelC.HH
dune exec holyc -- parse test/cli/parse-globals.hc
dune exec holyc -- parse test/cli/parse-pointers.hc
dune exec holyc -- parse test/cli/parse-comma-globals.hc
dune exec holyc -- dump-ast --format=json test/cli/parse-globals.hc
dune exec holyc -- corpus lex --reference-root=third_party/TempleOS
dune exec holyc -- corpus lex --format=json --reference-root=third_party/TempleOS
```

The `lex`, `preprocess`, `parse`, `dump-ast`, and `corpus lex` commands exit with status 1 when they report an error. A failed constant `#assert` is a warning, so preprocessing and parsing retain the following input and exit successfully when no error follows. The ordinary source paths reject an embedded NUL. The corpus command uses the pinned `LexGetChar` terminator rule for committed hybrid source and payload files, and it reports the bytes before and after that terminator separately. All commands treat offsets and columns as byte positions, matching the byte-oriented pinned source. `preprocess` currently accepts `#include`, `#define`, constant `#if` and `#assert` with `defined`, `#ifdef`, `#ifndef`, `#ifjit`, `#ifaot`, `#else`, `#endif`, `#help_index`, and `#help_file`. It also expands the six standard predefined values. The default date and time are fixed at `01/01/70` and `00:00:00`; use `--predefined-date`, `--predefined-time`, and `--command-line-source` when a build needs other deterministic values. JIT is the default; `--mode=aot` selects AOT branches. `--dump-help-metadata` replaces token output with a versioned human or JSON report. `parse` and `dump-ast` use the same preprocessing controls and emit `holyc-ast-v1`. Definition rules, expression coverage, symbol visibility, conditional behavior, path handling, provenance, and hosted limits are documented in [docs/preprocessor.md](docs/preprocessor.md).

Compilation and execution commands are intentionally absent until the parser, semantic checker, and IR interpreter exist. The current executable can inspect the small program in `examples/lexer-tour.hc`, but it cannot compile or run that program yet.

## TempleOS modules

TempleOS `.BIN` output is part of milestone M8 and is currently unavailable. The pinned header and patch-record specification is documented in [docs/templeos-bin-format.md](docs/templeos-bin-format.md). The eventual command will be documented only after generated modules pass both the independent loader model and the actual TempleOS loader procedure.

## Reference source

The TempleOS checkout is a read-only submodule at `third_party/TempleOS`. Run `tools/verify-reference.ps1` before using it as compatibility evidence. [REFERENCE_SOURCES.md](REFERENCE_SOURCES.md) lists the retrieval procedure, and [reference/manifest.json](reference/manifest.json) records checksums for audited files.

## Compile-time execution

HolyC `#exe` runs code during compilation. This build does not execute arbitrary `#exe` input. Its six predefined values are handled directly because their exact pinned definitions and formatting consumers are audited. Future general support will use a deterministic VM with bounded steps and memory by default. Native compile-time execution will require an explicit unsafe option. See [SECURITY.md](SECURITY.md).

## License and attribution

New project code is licensed under the MIT License. TempleOS is retained as a pinned reference and is not covered by this project's license. The upstream credits call TempleOS public domain while also listing material with separate provenance or uncertain permission. [THIRD_PARTY_NOTICES.md](THIRD_PARTY_NOTICES.md) records the narrow reference use and those caveats.
