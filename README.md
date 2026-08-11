# holyc-ocaml

`holyc-ocaml` is the project for an OCaml implementation of the HolyC compiler. Its compiler command is `holyc`, and its OCaml library module is `holyc_lib`. Compatibility work is based on the TempleOS source tree at commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

The current build contains the source manager, byte spans, structured diagnostics, a handwritten streaming lexer, bounded `#include` and `#define` source frames, constant `#if` evaluation, mode and symbol conditional selection, and source-checked models of HolyC primitive types, compiler options, function flags, intermediate-code operations, and TempleOS BIN records. The lexer handles identifiers, the complete language keyword list, integer and floating literals, strings, character constants up to eight bytes, nested comments, and the operators defined by the pinned compiler tables. The preprocessed token stream resolves quoted includes from a deterministic root list, expands TempleOS definition strings without adding C macro parameters, evaluates literal and definition-expanded `#if` expressions with the pinned precedence table, selects nested `#ifjit` or `#ifaot` branches from an explicit mode, and evaluates `#ifdef` or `#ifndef` against the session's visible compiler symbols. It retains file, definition, and invocation provenance and rejects path escapes, cycles, malformed conditional boundaries, excessive nesting, oversized inputs, excessive generated text, and oversized conditional expressions. Human and JSON token dumps are available.

The compiler-option and function-flag APIs are immutable specifications; no declaration parser, call checker, optimizer, or backend behavior is wired to them yet. The function-flag API does preserve the pinned modifier transitions, `RET1` boundary, and caller-cleanup predicate as pure operations for those later stages. The intermediate-code API defines opcode identity, argument shape, result count, structural type, and the two table flags, but it does not provide instructions, lowering, verification, optimization, or execution. The BIN API records the exact header layout, record codes and shapes, relocation formulas, two-pass loader actions, and source-marked limitations. It does not serialize, parse, relocate, or load modules. The primitive type API does not yet provide declaration parsing, conversions, member layout, or expression checking. Native TempleOS can use calls, globals, memory, casts, and compiler state in `#if` because it compiles and executes an ordinary expression. This build rejects those nonconstant terms; [issue #33](https://github.com/frankischilling/holyc-ocaml/issues/33) tracks execution through the compile-time VM. `#assert`, `#exe`, predefined values, and general generated source are also not implemented. The standalone preprocessor sees checked compiler keywords and internal types plus symbols registered through the session API; it cannot discover declarations before a parser exists. Opcode and register names are tracked in [issue #30](https://github.com/frankischilling/holyc-ocaml/issues/30). Token formation currently resumes between lexical frames; the native edge where a token consumes bytes from both an exhausted frame and its caller is tracked in [issue #24](https://github.com/frankischilling/holyc-ocaml/issues/24). Hosted diagnostics for unmatched conditionals are deliberately stricter than the pinned lexer and tracked in [issue #27](https://github.com/frankischilling/holyc-ocaml/issues/27). The parser, full semantic checker, executable IR, interpreter, assembler, native backends, JIT, and TempleOS `.BIN` writer are also unavailable. There is no command that pretends to emit a module while those stages are absent. Progress and source evidence live in [the compatibility notes](docs/compatibility.md), [the preprocessor notes](docs/preprocessor.md), [the ABI notes](docs/holy-abi.md), [the IR notes](docs/intermediate-representation.md), [the BIN format notes](docs/templeos-bin-format.md), and [the traceability registry](reference/traceability.toml).

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
```

The `lex` and `preprocess` commands exit with status 1 when they report a diagnostic. Both treat offsets and columns as byte positions, matching the byte-oriented pinned source. `preprocess` currently accepts `#include`, `#define`, constant `#if` with `defined`, `#ifdef`, `#ifndef`, `#ifjit`, `#ifaot`, `#else`, and `#endif`. JIT is the default; `--mode=aot` selects AOT branches. Definition rules, expression coverage, symbol visibility, conditional behavior, path handling, provenance, and hosted limits are documented in [docs/preprocessor.md](docs/preprocessor.md).

Compilation and execution commands are intentionally absent until the parser, semantic checker, and IR interpreter exist. The current executable can inspect the small program in `examples/lexer-tour.hc`, but it cannot compile or run that program yet.

## TempleOS modules

TempleOS `.BIN` output is part of milestone M8 and is currently unavailable. The pinned header and patch-record specification is documented in [docs/templeos-bin-format.md](docs/templeos-bin-format.md). The eventual command will be documented only after generated modules pass both the independent loader model and the actual TempleOS loader procedure.

## Reference source

The TempleOS checkout is a read-only submodule at `third_party/TempleOS`. Run `tools/verify-reference.ps1` before using it as compatibility evidence. [REFERENCE_SOURCES.md](REFERENCE_SOURCES.md) lists the retrieval procedure, and [reference/manifest.json](reference/manifest.json) records checksums for audited files.

## Compile-time execution

HolyC `#exe` runs code during compilation. This build does not execute `#exe`. Future hosted support will use a deterministic VM with bounded steps and memory by default. Native compile-time execution will require an explicit unsafe option. See [SECURITY.md](SECURITY.md).

## License and attribution

New project code is licensed under the MIT License. TempleOS is retained as a pinned reference and is not covered by this project's license. The upstream credits call TempleOS public domain while also listing material with separate provenance or uncertain permission. [THIRD_PARTY_NOTICES.md](THIRD_PARTY_NOTICES.md) records the narrow reference use and those caveats.
