# Testing

Run the local checks with:

```text
dune build @fmt
dune build
dune runtest
powershell -File tools/verify-reference.ps1
```

Unit tests cover source positions, spans, token construction, literal decoding, comments, and diagnostic rendering. Golden tests compare deterministic token dumps. Negative fixtures check malformed comments, strings, characters, and bytes. QCheck generates source buffers to verify offset and line-column invariants.

Golden changes require a reviewed fixture update. Tests must not rewrite expected files automatically.

Corpus, differential, fuzz, loader, and bootstrap suites will be added with the stages they exercise. A suite is not marked passing until its command runs in CI and publishes its exact reference commit.
