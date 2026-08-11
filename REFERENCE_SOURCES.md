# Reference sources

The compatibility reference is:

```text
repository: https://github.com/cia-foundation/TempleOS
commit: c26482bb6ad3f80106d28504ec5db3c6a360732c
path: third_party/TempleOS
```

The submodule is detached at the exact commit. A moving branch is never used in a compatibility result.

## Retrieval

```text
git submodule update --init --depth 1 third_party/TempleOS
git -C third_party/TempleOS fetch --depth 1 origin c26482bb6ad3f80106d28504ec5db3c6a360732c
git -C third_party/TempleOS checkout --detach c26482bb6ad3f80106d28504ec5db3c6a360732c
powershell -File tools/verify-reference.ps1
```

The verifier rejects a missing, dirty, or mismatched checkout and checks every file in `reference/manifest.json`. Checksums cover the pinned Git blob bytes, so line-ending conversion in a host checkout cannot change the result.

## Update policy

A reference update requires a dedicated issue, an impact report, corpus and compatibility comparisons, regenerated fixtures, a dedicated branch, and a pull request. The pull request must describe any changed language or binary behavior.

## Current audit

The audit currently covers `Compiler/Compiler.PRJ`, lexer definitions and implementation, diagnostics, character bitmaps, keyword and assembler directive records, primitive raw type constants, public integer union headers, the internal type table, compiler-option state, and the complete intermediate-code definition and metadata tables. Optimizer and backend reads establish how the original compiler consumes the option and IC fields. [docs/reference-source-map.md](docs/reference-source-map.md) records the findings and implementation links.

## Generated keyword data

Regenerate the audited language and assembler directive records with:

```text
dune exec tools/opcode_table_gen.exe -- --source third_party/TempleOS/Compiler/OpCodes.DD --manifest reference/manifest.json --output src/generated/opcode_keywords.ml
```

`dune build @generated-check` compares the checked-in file with a fresh deterministic rendering. The generator reverses Git's CRLF checkout conversion before checking the pinned blob checksum, so Windows and Unix checkouts validate the same source object. It uses `digestif` for SHA-256 because OCaml's standard `Digest` module only supplies MD5.

## Generated primitive type data

Regenerate the audited raw type and internal type records with:

```text
dune exec tools/primitive_type_gen.exe -- --kernel third_party/TempleOS/Kernel/KernelA.HH --cinit third_party/TempleOS/Compiler/CInit.HC --manifest reference/manifest.json --output src/generated/primitive_raw_types.ml
```

The generator checks both source files against `reference/manifest.json`. It rejects changes to raw IDs, the `RT_PTR` alias, unavailable floating slots, public integer union headers, `INTERNAL_TYPES_NUM`, or any of the 17 internal type records. The same `@generated-check` target verifies this output in CI.

## Generated operator data

Regenerate token recognition, precedence, and binary operator records with:

```text
dune exec tools/operator_table_gen.exe -- --kernel third_party/TempleOS/Kernel/KernelA.HH --compiler third_party/TempleOS/Compiler/CompilerA.HH --cinit third_party/TempleOS/Compiler/CInit.HC --lex third_party/TempleOS/Compiler/Lex.HC --manifest reference/manifest.json --output src/generated/operator_tables.ml
```

This generator checks all four source files before parsing them. It preserves the three `dual_U16_tokens` arrays, comment openers, lexer-only shift and dot forms, precedence and association constants, and every `cmp.binary_ops` record. `dune build @generated-check` rejects stale operator output alongside the other generated tables.

## Generated compiler option data

Regenerate the compiler-option registry with:

```text
dune exec tools/compiler_option_gen.exe -- --reference-root third_party/TempleOS --manifest reference/manifest.json --output src/generated/compiler_options.ml
```

The generator checks 16 pinned sources before it writes anything. It extracts the 12 `OPTf_*` bit indices from `Kernel/KernelA.HH`, the initial mask from `Compiler/Lex.HC:CmpCtrlNew`, and the `Option`/`GetOption` contract from `Compiler/CMisc.HC` and `Kernel/KUtils.HC:_BEQU`. It also records code references in the lexer, parser, symbol table, optimizer, and backend. Comments and string literals do not count as consumers.

The gaps at bits 2 through 15 and 20 through 31 are retained. `OPTf_USE_IMM64` carries the pinned source's "Not completely implemented" note instead of being presented as finished behavior. The generated registry describes source facts only; later compiler stages must still implement each option's effect.

## Generated intermediate-code data

Regenerate the intermediate-code implementation and interface with:

```text
dune exec tools/intermediate_code_gen.exe -- --compiler third_party/TempleOS/Compiler/CompilerA.HH --cinit third_party/TempleOS/Compiler/CInit.HC --manifest reference/manifest.json --output-ml src/generated/intermediate_codes.ml --output-mli src/generated/intermediate_codes.mli
```

The generator verifies both source tables before parsing them. It requires 185 contiguous `IC_*` definitions, the `IC_ICS_NUM` value `0xB9`, the audited `CIntermediateStruct` layout, and one metadata record for each numeric code. Unknown argument shapes, structural types, Boolean values, padding, or extra fields stop generation.

Constant names and display names remain separate. The source contains 13 real differences, including `IC_SWAP_I64` versus `SWAP_U64`; the generator does not rewrite either spelling. `dune build @generated-check` compares both generated files with a fresh rendering.
