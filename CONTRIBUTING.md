# Contributing

Start with an issue that names the TempleOS source evidence and a testable result. Branch from an updated `main` branch and keep each pull request focused on one issue.

Before requesting review, run:

```text
dune fmt
dune build
dune runtest
powershell -File tools/verify-reference.ps1
```

Compatibility changes must update `reference/traceability.toml`, relevant tests, and the human-readable source map. A reference commit update needs its own issue, impact report, branch, and pull request.

Commit messages should describe one coherent change. Do not hide unsupported syntax behind skips or broad fallback behavior. If a corpus file remains excluded, link the exclusion to an open issue and record the first failing phase.
