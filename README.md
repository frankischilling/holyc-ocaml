# holyc-ocaml

`holyc-ocaml` is an OCaml implementation of the HolyC compiler. The command is `holyc`, and the public OCaml library is `holyc_lib`. Compatibility work follows the TempleOS source tree at commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

## Current status

The repository has a byte-oriented source manager, structured diagnostics, a handwritten streaming lexer, an integrated preprocessing stream, a source-positioned AST, and the first parser slices. Human and JSON reports are deterministic. `holyc dump-symbols` exposes the parser's source-order visibility state; it is inspection data, not a completed semantic symbol table.

Raw lexing handles the complete checked keyword and operator tables, numeric literals, strings, character constants up to eight bytes, nested comments, line continuations, and TempleOS files with a NUL-terminated text prefix. The pinned corpus result is 528 of 528 `.HC`, `.HH`, and `.PRJ` Git blobs tokenized without a lexer diagnostic or crash. That result applies only to raw lexing.

The AOT parser corpus baseline measures the same 528 committed objects in fresh compiler sessions. Twenty-one currently parse without an error; 16 have a lexer or preprocessor first error and 491 have a parser first error. There are no read errors, internal errors, or omitted files. This is a progress measurement for syntax coverage, not a semantic compatibility score. The full per-file result is checked in at [`reference/parser-corpus-aot.json`](reference/parser-corpus-aot.json).

Preprocessing currently supports bounded quoted includes, TempleOS text definitions, constant `#if` and `#assert`, `#ifdef`, `#ifndef`, `#ifjit`, `#ifaot`, `#else`, `#endif`, `#help_index`, and `#help_file`. It also expands `__DATE__`, `__TIME__`, `__LINE__`, `__CMD_LINE__`, `__FILE__`, and `__DIR__` from explicit deterministic inputs. Include, definition, and generated-value frames retain their source origins. Hosted path, depth, input-size, generated-byte, and expression-size limits reject unsafe input with diagnostics.

The parser accepts primitive and intrinsic storage types, pointer, array, and function-pointer global declarators, comma-separated declaration groups, `extern class` and `extern union` forward declarations, ordinary and type-backed class and union definitions, optional single-base clauses, declaration and calling modifiers, ordinary and alternate-name bindings, `_intern` expression targets, bound function prototypes, and unbound function definitions. Aggregate definitions retain an optional backing type, its pointer layers, a separate source-spanned base name, ordered member groups, independent member pointers, array dimensions, recursive function-pointer members, recursive anonymous unions, empty member semicolons, and explicit `$$ = expression;` offset directives. Each offset node keeps the marker, equals sign, ordinary expression, semicolon, and complete provenance; the four direct pinned forms parse in JIT and AOT modes. Aggregate member names may use any checked language-keyword spelling, matching TempleOS's contextual `TK_IDENT`/`PrsKeyWord` split without changing lexer token dumps or keyword dispatch elsewhere. A member may also carry repeated metadata name/value pairs. String metadata keeps every adjacent literal and its combined decoded bytes; other values keep the ordinary expression tree. The six metadata-bearing members in pinned `Demo/ClassMeta.HC` parse in both modes. Both forwards and definitions preserve class versus union spelling. A definition publishes its name before reading an optional base or the body, so source-shaped self references and later preprocessing see it. A published class or union name can then appear in global and local declarations, function return and parameter types, nested function-pointer signatures, arrays, and postfix casts. Recognition follows the current symbol environment, including local shadowing; capitalization alone never makes a name a type. Function signatures include named and unnamed parameters, recursive function-pointer parameters, register qualifiers, terminal varargs, ordinary defaults in non-trailing positions, and a distinct `lastclass` default. Function-pointer globals keep their return stars, callback indirection, recursive signature, arrays, initializer, and source locations as separate AST data. Aggregate callback members use the same recursive signature node while remaining members rather than global symbols. Function bodies accept primitive, intrinsic, or published aggregate names in automatic and static locals. Local groups retain pointer layers and array dimensions. Both storage classes accept scalar initializers; static locals also retain recursive braced initializer trees, while automatic locals stay on the pinned expression-only path. Automatic declarations retain `reg` and `noreg` requests plus an optional canonical U64 register after `reg`. Callback locals use the shared recursive signature node and enter the parser environment as local variables. Direct callback initialization is rejected with `HCPARSE0137`, matching the boundary documented beside the pinned `Grid.HC` declaration. An explicit `;` remains an empty body statement, while a signature that reaches EOF has an absent body.

The shared expression parser covers literals, identifiers, `$$`, grouping, the audited prefix and binary operators, parenthesized calls with omitted slots, parenthesis-free direct calls, indexes, direct and pointer members, postfix increment and decrement, postfix casts to primitive or visible aggregate names, `sizeof`, `offset`, and `defined`. A function entry retains the order of its fixed parameters and whether each has a default. Without parentheses, defaulted positions become omitted slots, required positions consume adjacent expressions, and variadic extras remain absent. A local or newer nonfunction name suppresses this call form, and `&Function` remains an address expression. It follows the precedence and control flow in the pinned compiler rather than substituting C rules.

Expression, empty, local declaration, `goto`, label, `lock`, `switch`, paired `try`/`catch`, `break`, `return`, and brace-delimited `asm` statements have explicit AST nodes. The assembly node preserves its braces, physical source-line groups, global, exported, and `@@` local labels, and every body token. Those tokens are classified against the checked tables for 325 canonical opcodes, 49 aliases, 25 directives, and 106 registers. This is a structural syntax boundary: brace-block operations and directives are not checked yet, and the parser does not emit instruction bytes. The parser retains HolyC's comma-linked statement groups, including leading, repeated, and trailing separators. It also parses empty, nested, and top-level compound blocks, parenthesized `if` conditions with optional `else` branches, pre-test `while` loops, post-test `do ... while` loops, and source-shaped `for` headers. A `for` node distinguishes an empty initializer from its required condition and an absent update from a parsed update statement. Supported local declarations work in function statement positions and follow the pinned parser's continuation rule: after a local declaration, the same statement parse continues until a nonlocal statement finishes or `}` closes the block. This means an unbraced branch can absorb the statement after its local declaration, and a local declaration is rejected in a `for` initializer. `goto` retains its target and optional semicolon, while an unresolved name followed by `:` becomes a label without reinterpreting a known global, parameter, or local. `lock` wraps one ordinary statement, including the unbraced form used by the pinned multicore demo. Switch nodes distinguish parenthesized bounded mode from bracketed no-bound mode. Their ordered bodies retain implicit, single, ranged, and default cases, ordinary statements, and nested `start:`/`end:` sub-switch regions. A `try` node keeps both its ordinary nested body and the required ordinary statement after `catch`; either side may be braced or unbraced. `break` retains its semicolon when present but keeps it absent at a statement comma or inside a semicolon-free `for` update. `return` separately records an absent or parsed value and follows the same comma and update boundaries for value-bearing forms. The nearest unmatched `if` owns an `else`, including when a loop lies between them, and every branch, loop body, or parsed function body can use any currently supported statement form. A function becomes visible to recursive calls and following preprocessor conditionals before its body is read. Named parameters, prior local names, and the variadic `argc` and `argv` names participate in the parser's function-wide lookup context. String statements select `Print`; character statements select `PutChars`. Empty markers such as `"" fmt,args;` and `'' value;` retain the following fixed expression, while a nonempty marker remains the start of its complete expression. A comma after `PutChars` starts another statement, but commas before a `Print` semicolon remain call arguments. Punctuation, source order, and generated-source provenance stay visible in AST dumps.

Function-local direct assembly has a source-ordered AST item stream for adjacent instructions and assembler directives. An instruction keeps its zero, one, or two register, immediate, or memory operands, size and segment prefixes, comma, optional semicolon, source spelling, canonical opcode identity, and generated-source provenance. Directive items retain `IMPORT`, `DU8`, `DU16`, `DU32`, `DU64`, `DUP`, `BINFILE`, `LIST`, `NOLIST`, `USE16`, `USE32`, and `USE64` syntax, including optional commas where the pinned parser permits them. This follows the `CMPF_ONE_ASM_INS`, `PrsAsmArg`, and `PrsAsmBlk` paths without treating physical newlines as instruction boundaries. `HCPARSE0147` rejects direct items at top level; `HCPARSE0149` through `HCPARSE0156` report malformed operands or directives. Parsing `BINFILE` does not read the named file. Direct labels, directive evaluation and state changes, operand validation, form selection, lowering, encoding, and execution remain unavailable.

The project also exposes checked, immutable specifications for all 12 HolyC primitive spellings, compiler options, function flags, all 185 intermediate-code identities, all 106 registers, 325 canonical opcodes, 49 aliases, 924 instruction forms, and the TempleOS BIN header and patch-record families.

## What is not implemented

The current parser work is syntax only. Its function-wide lookup context routes identifiers and preprocessor conditionals, and its function entries retain enough signature shape to parse direct calls. It does not create semantic scopes, resolve type identities or bindings, allocate local storage, calculate class or union layouts, validate calls, evaluate or substitute defaults, resolve `Print` or `PutChars`, bind `goto` targets, reject duplicate or unresolved labels, propagate `lock` regions to `ICF_LOCK`, assign implicit case values, validate switch labels, enforce sub-switch front-porch rules, build switch tables, validate a `break` target or return context, check a return value against a function type, unwind active `try` regions, call `SysTry` or `SysUntry`, convert a condition to truth, build control flow, lower expressions, or execute top-level statements. Backing types are retained as syntax, but whole-value conversion and subinteger access are not implemented. Base clauses likewise do not yet affect size, alignment, or inherited member lookup. Member metadata expressions are not evaluated, indexed, or exposed through `MemberMetaData` and `MemberMetaFind`. A top-level aggregate definition may declare pointer, array, function-pointer, or comma-separated globals after its closing brace, and globals retain scalar or recursive braced initializer syntax. Callback signatures on globals, aggregate members, and locals are not yet type-checked, assigned storage or offsets, lowered, or executed. Initializer trees and aggregate-offset directives remain unevaluated and do not affect layout. Nested named definitions, direct local callback initializers, automatic local braced initializers, and many other corpus declaration forms remain unavailable and fail with explicit diagnostics. Exception semantics, lock-aware IR and code emission, direct assembly labels, directive evaluation and state changes, operand validation, and complete corpus parsing also remain unavailable.

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
dune exec holyc -- parse test/cli/parse-aggregate-forward.hc
dune exec holyc -- parse test/cli/parse-aggregate-definition.hc
dune exec holyc -- parse test/cli/parse-aggregate-offset.hc
dune exec holyc -- parse test/cli/parse-contextual-member-names.hc
dune exec holyc -- parse test/cli/parse-aggregate-member-metadata.hc
dune exec holyc -- parse test/cli/parse-aggregate-backing.hc
dune exec holyc -- parse test/cli/parse-named-types.hc
dune exec holyc -- parse test/cli/parse-default-parameters.hc
dune exec holyc -- parse test/cli/parse-parenthesis-free-call.hc
dune exec holyc -- parse test/cli/parse-implicit-output.hc
dune exec holyc -- parse test/cli/parse-compound-blocks.hc
dune exec holyc -- parse test/cli/parse-if-statements.hc
dune exec holyc -- parse test/cli/parse-while-statements.hc
dune exec holyc -- parse test/cli/parse-do-while-statements.hc
dune exec holyc -- parse test/cli/parse-for-statements.hc
dune exec holyc -- parse test/cli/parse-break-statements.hc
dune exec holyc -- parse test/cli/parse-return-statements.hc
dune exec holyc -- parse test/cli/parse-function-definitions.hc
dune exec holyc -- parse test/cli/parse-local-declarations.hc
dune exec holyc -- parse test/cli/parse-static-local-initializers.hc
dune exec holyc -- parse test/cli/parse-function-pointer-local.hc
dune exec holyc -- parse test/cli/parse-lock-statements.hc
dune exec holyc -- parse test/cli/parse-switch-statements.hc
dune exec holyc -- parse test/cli/parse-try-catch-statements.hc
dune exec holyc -- parse test/cli/parse-assembly-block.hc
dune exec holyc -- parse test/cli/parse-inline-assembly.hc
dune exec holyc -- dump-ast --format=json test/cli/parse-implicit-output.hc
dune exec holyc -- dump-symbols --source-only test/cli/symbol-dump.hc
dune exec holyc -- dump-symbols --source-only --format=json test/cli/symbol-dump.hc
dune exec holyc -- corpus lex --reference-root=third_party/TempleOS
dune exec holyc -- corpus parse --mode=aot --reference-root=third_party/TempleOS
dune exec holyc -- corpus parse --mode=aot --require-all --reference-root=third_party/TempleOS
```

`lex`, `preprocess`, `parse`, `dump-ast`, `dump-symbols`, and `corpus lex` exit with status 1 when they report an error. `dump-symbols` still writes the state accumulated before a parser failure, which makes partial corpus failures inspectable without turning them into successful parses. Its default output includes the 570 pinned compiler entries; `--source-only` keeps declarations published from the input stream. `corpus parse` normally succeeds after a complete scan because known incompatibilities are its output; `--require-all` returns status 1 unless every file parses. A failed constant `#assert` is a warning, so later input remains available and the command succeeds when no error follows. JIT preprocessing is the default for single files. The parser corpus defaults to AOT, and `--mode=jit` selects its other branch. All columns and offsets are byte positions.

There is not yet a command that compiles or runs a HolyC program. The parser can inspect the current fixtures, but it cannot execute them. Compilation examples will be added only after semantic checking, verified IR, and an execution backend pass their own tests.

## TempleOS modules

TempleOS `.BIN` output belongs to milestone M8 and is currently unavailable. [The format notes](docs/templeos-bin-format.md) record the pinned header, records, and loader formulas. An emission command will be documented after generated modules pass both the independent loader model and the actual TempleOS loader procedure.

## Reference source

The TempleOS checkout is a read-only submodule at `third_party/TempleOS`. Run `tools/verify-reference.ps1` before using it as compatibility evidence. [REFERENCE_SOURCES.md](REFERENCE_SOURCES.md) explains retrieval, and [reference/manifest.json](reference/manifest.json) records checksums for audited files.

## Compile-time execution

HolyC `#exe` runs code during compilation. This build does not execute arbitrary `#exe` input. Future general support will use a deterministic VM with bounded steps and memory by default. Native compile-time execution will require an explicit unsafe option. See [SECURITY.md](SECURITY.md).

## License and attribution

New project code is licensed under the MIT License. TempleOS is retained as a pinned reference and is not covered by this project's license. The upstream credits call TempleOS public domain while also listing material with separate provenance or uncertain permission. [THIRD_PARTY_NOTICES.md](THIRD_PARTY_NOTICES.md) records the narrow reference use and those caveats.
