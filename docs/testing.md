# Testing holyc-ocaml

Run the local checks with:

```text
dune build @fmt
dune build @generated-check
dune build
dune runtest
powershell -File tools/verify-reference.ps1
```

Unit tests cover source positions, spans, token construction, literal decoding, comments, diagnostic rendering, strict extraction of keyword and assembler directive records, operator tables, primitive type metadata, compiler options, function flags, the complete intermediate-code table, and the TempleOS BIN specification. The generated-table tests reject malformed statements, duplicate records, missing or reordered entries, changed aliases, unknown operator tokens or ICs, precedence drift, unavailable-type drift, option default drift, function-flag expression or transition drift, BIN record or loader-formula drift, API contract drift, and source checksum mismatches. The option, function-flag, and BIN scanners also prove that comments and literals do not create false consumers; the option tests separately confirm that `_BEQU` retains its previous-state result.

Operator API tests cover prefix boundaries, source provenance, all precedence bands, raw association flags, and binary mappings. Semantic type tests cover every supported spelling, raw ID, size, signedness, declaration form, `Bool`, zero-sized types, and the pointer raw alias. Compiler-option API tests cover all names and bit indices, intentional gaps, the initial mask, immutable updates, consumer lines, phase classifications, and the source-marked incomplete state of `OPTf_USE_IMM64`.

Intermediate-code tests cover all 185 constructors and numeric slots. They compare every generated metadata field with a fresh parse of the pinned source, check all lookup directions, retain the 13 constant/display-name differences, and reject malformed shapes, counts, Booleans, padding, and records. Golden tests compare deterministic token dumps. Negative fixtures check malformed comments, strings, characters, and bytes. QCheck generates source buffers to verify offset and line-column invariants.

Function-flag tests cover the two shared flag positions, all eight stored function bits, all eight parser-staging masks, both source groups, and every modifier transition. The transition test checks all 4,096 possible staging masks. Boundary and truth-table tests cover automatic `RET1`, varargs, caller cleanup, interrupt error-code handling, internal functions, and the distinction between public hash state and stored function flags.

BIN source tests cover the exact header layout and signature, entry order and numeric gaps, source status comments, import displacement biases, record payloads, loader-pass actions, AOT adjustments, checksum normalization, and source provenance. Mutation tests reject changed, duplicate, missing, or reordered entries, changed reserved gaps, altered import formulas, altered adjustment operations, unknown consumers, and incomplete source sets. Public API tests distinguish known, reserved, and unknown codes and check every name and numeric round trip. These are specification tests; loader and actual-TempleOS oracle tests remain pending with the serializer.

Golden changes require a reviewed fixture update. Tests must not rewrite expected files automatically.

Corpus, differential, fuzz, loader, and bootstrap suites will be added with the stages they exercise. A suite is not marked passing until its command runs in CI and publishes its exact reference commit.
