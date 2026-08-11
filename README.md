# holyc-ocaml

`holyc-ocaml` is the project for an OCaml implementation of the HolyC compiler. Its compiler command is `holyc`, and its OCaml library module is `holyc_lib`. Compatibility work is based on the TempleOS source tree at commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

The current build contains the source manager, byte spans, structured diagnostics, a handwritten streaming lexer, and a source-checked model of HolyC primitive types. The lexer handles identifiers, the complete language keyword list, integer and floating literals, strings, character constants up to eight bytes, nested comments, and the operators defined by the pinned compiler tables. Checked generators now supply keyword IDs, compound operators, precedence constants, binary IC mappings, primitive raw types, internal storage names, and public integer union headers. Human and JSON token dumps are available.

The primitive type API does not yet provide declaration parsing, conversions, member layout, or expression checking. The integrated preprocessor, parser, full semantic checker, IR, interpreter, assembler, native backends, JIT, and TempleOS `.BIN` writer are not implemented yet. There is no command that pretends to emit a module while those stages are unavailable. Progress and source evidence live in [the compatibility notes](docs/compatibility.md), [the language notes](docs/holyc-language.md), and [the traceability registry](reference/traceability.toml).

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
```

The `lex` command exits with status 1 when it reports a lexical diagnostic. It treats offsets and columns as byte positions, matching the byte-oriented pinned source.

Compilation and execution commands are intentionally absent until the parser, semantic checker, and IR interpreter exist. The current executable can inspect the small program in `examples/lexer-tour.hc`, but it cannot compile or run that program yet.

## TempleOS modules

TempleOS `.BIN` output is part of milestone M8 and is currently unavailable. The eventual command will be documented only after generated modules pass both the independent loader model and the actual TempleOS loader procedure.

## Reference source

The TempleOS checkout is a read-only submodule at `third_party/TempleOS`. Run `tools/verify-reference.ps1` before using it as compatibility evidence. [REFERENCE_SOURCES.md](REFERENCE_SOURCES.md) lists the retrieval procedure, and [reference/manifest.json](reference/manifest.json) records checksums for audited files.

## Compile-time execution

HolyC `#exe` runs code during compilation. This build does not execute `#exe`. Future hosted support will use a deterministic VM with bounded steps and memory by default. Native compile-time execution will require an explicit unsafe option. See [SECURITY.md](SECURITY.md).

## License and attribution

New project code is licensed under the MIT License. TempleOS is retained as a pinned reference and is not covered by this project's license. The upstream credits call TempleOS public domain while also listing material with separate provenance or uncertain permission. [THIRD_PARTY_NOTICES.md](THIRD_PARTY_NOTICES.md) records the narrow reference use and those caveats.
