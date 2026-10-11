# holyc-ocaml

`holyc-ocaml` is an OCaml implementation of the HolyC compiler. The command-line
tool is `holyc`, and the public OCaml library is `holyc_lib`. Compatibility work
follows the pinned [TempleOS reference](docs/reference-sources.md) at commit
`c26482bb6ad3f80106d28504ec5db3c6a360732c`.

The project can lex and preprocess HolyC, parse and inspect supported syntax,
and run documented subsets of integer programs through a checked interpreter.
Selected programs also compile and execute as native x86-64 code on Windows and
Linux. Support varies by command, source construct, and execution target.
General HolyC compilation, the full runtime, and TempleOS `.BIN` emission are
still unfinished. The [implementation notes](docs/compiler-status.md) and
[compatibility report](docs/compatibility.md) describe the current boundaries.

## Build and test

Install OCaml 5.1 or newer, opam, and Dune 3.12 or newer. From the repository
root, initialize the reference submodule and build:

```sh
git submodule update --init --depth 1
opam install . --deps-only --with-test
dune build
dune runtest
```

To install the command and library into the active opam switch, run `dune install`.
The [testing guide](docs/testing.md) lists the additional format, generated-file,
reference, and provenance checks used for development.

## Try the compiler

Run these commands from the repository root:

```sh
dune exec holyc -- version
dune exec holyc -- lex examples/lexer-tour.hc
dune exec holyc -- preprocess examples/include-tour.hc
dune exec holyc -- parse test/cli/parse-globals.hc
dune exec holyc -- run --target=ir examples/integer-arrays.hc
dune exec holyc -- run --target=host-jit examples/native-integer-program.hc
```

The `run` command executes only the supported source domain for its target.
Unsupported syntax or operations produce diagnostics. For native source-task
execution, task lifetime and generated-code limits, see
[native source tasks](docs/native-source-tasks.md).

## Documentation

- [Compiler status, examples, commands, and known gaps](docs/compiler-status.md)
- [Compatibility](docs/compatibility.md), [release requirements](docs/release-contract.md), and [roadmap](docs/roadmap.md)
- [Architecture](docs/architecture.md), [HolyC language notes](docs/holyc-language.md), and [test guide](docs/testing.md)
- [Reference sources](docs/reference-sources.md), [source map](docs/reference-source-map.md), and [traceability registry](reference/traceability.toml)
- [Changelog](docs/changelog.md), [contributing](docs/CONTRIBUTING.md), [security](docs/SECURITY.md), and [support](docs/SUPPORT.md)

New project code is licensed under the [MIT License](LICENSE). The pinned
TempleOS reference has separate provenance, described in the
[third-party notices](docs/third-party-notices.md).
