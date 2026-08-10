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

The verifier rejects a missing, dirty, or mismatched checkout and checks every file in `reference/manifest.json`.

## Update policy

A reference update requires a dedicated issue, an impact report, corpus and compatibility comparisons, regenerated fixtures, a dedicated branch, and a pull request. The pull request must describe any changed language or binary behavior.

## Current audit

The foundation audit covers `Compiler/Compiler.PRJ`, lexer definitions and implementation, diagnostics, character bitmaps, keyword records, and the relevant language documentation. [docs/reference-source-map.md](docs/reference-source-map.md) records the findings and implementation links.
