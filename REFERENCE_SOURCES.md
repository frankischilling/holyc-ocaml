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

The audit currently covers `Compiler/Compiler.PRJ`, lexer definitions and implementation, diagnostics, character bitmaps, keyword and assembler directive records, primitive raw type constants, public integer union headers, the internal type table, and the relevant language documentation. `Compiler/AsmInit.HC:AsmHashLoad` establishes how the original compiler consumes both generated-table sources. [docs/reference-source-map.md](docs/reference-source-map.md) records the findings and implementation links.

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
