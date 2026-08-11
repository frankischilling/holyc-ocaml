# Testing holyc-ocaml

Run the local checks with:

```text
dune build @fmt
dune build @generated-check
dune build
dune runtest
powershell -File tools/verify-reference.ps1
```

Unit tests cover source positions, spans, token construction, literal decoding, comments, diagnostic rendering, strict extraction of keyword and assembler directive records, and primitive type metadata. The generated-table tests reject malformed statements, duplicate records, missing or reordered entries, changed aliases, unavailable-type drift, and source checksum mismatches. Semantic type tests cover every supported spelling, raw ID, size, signedness, declaration form, `Bool`, zero-sized types, and the pointer raw alias. Golden tests compare deterministic token dumps. Negative fixtures check malformed comments, strings, characters, and bytes. QCheck generates source buffers to verify offset and line-column invariants.

Golden changes require a reviewed fixture update. Tests must not rewrite expected files automatically.

Corpus, differential, fuzz, loader, and bootstrap suites will be added with the stages they exercise. A suite is not marked passing until its command runs in CI and publishes its exact reference commit.
