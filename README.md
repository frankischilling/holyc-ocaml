# holyc-ocaml

`holyc-ocaml` is an OCaml implementation of the HolyC compiler. The command is `holyc`, and the public OCaml library is `holyc_lib`. Compatibility work follows the TempleOS source tree at commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

## Current status

The repository has a byte-oriented source manager, structured diagnostics, a handwritten streaming lexer, an integrated preprocessing stream, a source-positioned AST, and the first parser slices. Human and JSON reports are deterministic.

Raw lexing handles the complete checked keyword and operator tables, numeric literals, strings, character constants up to eight bytes, nested comments, line continuations, and TempleOS files with a NUL-terminated text prefix. The pinned corpus result is 528 of 528 `.HC`, `.HH`, and `.PRJ` Git blobs tokenized without a lexer diagnostic or crash. That result applies only to raw lexing.

Preprocessing currently supports bounded quoted includes, TempleOS text definitions, constant `#if` and `#assert`, `#ifdef`, `#ifndef`, `#ifjit`, `#ifaot`, `#else`, `#endif`, `#help_index`, and `#help_file`. It also expands `__DATE__`, `__TIME__`, `__LINE__`, `__CMD_LINE__`, `__FILE__`, and `__DIR__` from explicit deterministic inputs. Include, definition, and generated-value frames retain their source origins. Hosted path, depth, input-size, generated-byte, and expression-size limits reject unsafe input with diagnostics.

The parser accepts primitive globals, pointer and array declarators, comma-separated declaration groups, declaration and calling modifiers, ordinary and alternate-name bindings, `_intern` expression targets, and bound primitive function prototypes. Prototype syntax includes named and unnamed parameters, recursive function-pointer parameters, register qualifiers, terminal varargs, ordinary defaults in non-trailing positions, and a distinct `lastclass` default.

The shared expression parser covers literals, identifiers, `$$`, grouping, the audited prefix and binary operators, parenthesized calls with omitted slots, indexes, direct and pointer members, postfix increment and decrement, primitive postfix casts, `sizeof`, `offset`, and `defined`. It follows the precedence and control flow in the pinned compiler rather than substituting C rules.

Expression, empty, `goto`, label, `lock`, paired `try`/`catch`, `break`, and `return` statements now have explicit AST nodes. The parser retains HolyC's comma-linked statement groups, including leading, repeated, and trailing separators. It also parses empty, nested, and top-level compound blocks, parenthesized `if` conditions with optional `else` branches, pre-test `while` loops, post-test `do ... while` loops, and source-shaped `for` headers. A `for` node distinguishes an empty initializer from its required condition and an absent update from a parsed update statement. `goto` retains its target and optional semicolon, while an unresolved name followed by `:` becomes a label without reinterpreting a known global or function. `lock` wraps one ordinary statement, including the unbraced form used by the pinned multicore demo. A `try` node keeps both its ordinary nested body and the required ordinary statement after `catch`; either side may be braced or unbraced. `break` retains its semicolon when present but keeps it absent at a statement comma or inside a semicolon-free `for` update. `return` separately records an absent or parsed value and follows the same comma and update boundaries for value-bearing forms. The nearest unmatched `if` owns an `else`, including when a loop lies between them, and every branch or loop body can use any currently supported statement form. String statements select `Print`; character statements select `PutChars`. Empty markers such as `"" fmt,args;` and `'' value;` retain the following fixed expression, while a nonempty marker remains the start of its complete expression. A comma after `PutChars` starts another statement, but commas before a `Print` semicolon remain call arguments. Punctuation, source order, and generated-source provenance stay visible in AST dumps.

The project also exposes checked, immutable specifications for all 12 HolyC primitive spellings, compiler options, function flags, all 185 intermediate-code identities, all 106 registers, 325 canonical opcodes, 49 aliases, 924 instruction forms, and the TempleOS BIN header and patch-record families.

## What is not implemented

The current parser work is syntax only. It does not create lexical scopes for blocks, resolve types or bindings, calculate storage or class layouts, validate calls, substitute defaults, resolve `Print` or `PutChars`, bind `goto` targets, reject duplicate or unresolved labels, propagate `lock` regions to `ICF_LOCK`, validate a `break` target or return context, check a return value against a function type, unwind active `try` regions, call `SysTry` or `SysUntry`, convert a condition to truth, build control flow, lower expressions, or execute top-level statements. Function bodies, local declarations, classes, unions, switch forms, exception semantics, lock-aware IR and code emission, assembly syntax, and whole-corpus parsing remain unavailable.

Executable IR, optimization, the interpreter, x86-64 emission, the hosted runtime, JIT execution, the assembler encoder, and the TempleOS `.BIN` writer are not present yet. The opcode and BIN APIs are audited specifications, not encoders or loaders. Unsupported parser input reports an `HCPARSE` diagnostic and prevents a successful public AST; there is no raw-token fallback.

Native TempleOS can execute nonconstant directive expressions and `#exe` code. This build accepts only its bounded constant preprocessor subset and directly implements the six audited predefined values. [Issue #33](https://github.com/frankischilling/holyc-ocaml/issues/33) tracks execution through the compile-time VM. Hosted unmatched-conditional diagnostics are intentionally stricter than the pinned lexer under [issue #27](https://github.com/frankischilling/holyc-ocaml/issues/27).

See [the compatibility report](docs/compatibility.md) and [the traceability registry](reference/traceability.toml) for evidence and exact boundaries.

## Build and test

OCaml 5.1 or newer, opam, and Dune 3.12 or newer are required.

```text
git submodule update --init --depth 1
opam install . --deps-only --with-test
dune build
dune runtest
dune install
```

The main local checks are:

```text
dune build @fmt
dune build @generated-check
dune build @all
dune runtest
powershell -File tools/verify-reference.ps1
```

## Use the current commands

Run from the build tree:

```text
dune exec holyc -- version
dune exec holyc -- lex examples/lexer-tour.hc
dune exec holyc -- lex --format=json examples/lexer-tour.hc
dune exec holyc -- preprocess examples/include-tour.hc
dune exec holyc -- preprocess --mode=aot examples/mode-branches.hc
dune exec holyc -- preprocess examples/constant-if.hc
dune exec holyc -- parse test/cli/parse-globals.hc
dune exec holyc -- parse --mode=aot test/cli/parse-bindings.hc
dune exec holyc -- parse test/cli/parse-default-parameters.hc
dune exec holyc -- parse test/cli/parse-implicit-output.hc
dune exec holyc -- parse test/cli/parse-compound-blocks.hc
dune exec holyc -- parse test/cli/parse-if-statements.hc
dune exec holyc -- parse test/cli/parse-while-statements.hc
dune exec holyc -- parse test/cli/parse-do-while-statements.hc
dune exec holyc -- parse test/cli/parse-for-statements.hc
dune exec holyc -- parse test/cli/parse-break-statements.hc
dune exec holyc -- parse test/cli/parse-return-statements.hc
dune exec holyc -- parse test/cli/parse-lock-statements.hc
dune exec holyc -- parse test/cli/parse-try-catch-statements.hc
dune exec holyc -- dump-ast --format=json test/cli/parse-implicit-output.hc
dune exec holyc -- corpus lex --reference-root=third_party/TempleOS
```

`lex`, `preprocess`, `parse`, `dump-ast`, and `corpus lex` exit with status 1 when they report an error. A failed constant `#assert` is a warning, so later input remains available and the command succeeds when no error follows. JIT preprocessing is the default; `--mode=aot` selects AOT branches. All columns and offsets are byte positions.

There is not yet a command that compiles or runs a HolyC program. The parser can inspect the current fixtures, but it cannot execute them. Compilation examples will be added only after semantic checking, verified IR, and an execution backend pass their own tests.

## TempleOS modules

TempleOS `.BIN` output belongs to milestone M8 and is currently unavailable. [The format notes](docs/templeos-bin-format.md) record the pinned header, records, and loader formulas. An emission command will be documented after generated modules pass both the independent loader model and the actual TempleOS loader procedure.

## Reference source

The TempleOS checkout is a read-only submodule at `third_party/TempleOS`. Run `tools/verify-reference.ps1` before using it as compatibility evidence. [REFERENCE_SOURCES.md](REFERENCE_SOURCES.md) explains retrieval, and [reference/manifest.json](reference/manifest.json) records checksums for audited files.

## Compile-time execution

HolyC `#exe` runs code during compilation. This build does not execute arbitrary `#exe` input. Future general support will use a deterministic VM with bounded steps and memory by default. Native compile-time execution will require an explicit unsafe option. See [SECURITY.md](SECURITY.md).

## License and attribution

New project code is licensed under the MIT License. TempleOS is retained as a pinned reference and is not covered by this project's license. The upstream credits call TempleOS public domain while also listing material with separate provenance or uncertain permission. [THIRD_PARTY_NOTICES.md](THIRD_PARTY_NOTICES.md) records the narrow reference use and those caveats.
