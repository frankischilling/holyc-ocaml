# Third-party notices

## TempleOS

This repository includes a pinned, read-only TempleOS submodule for compiler research, test fixture derivation, and compatibility comparison:

```text
https://github.com/cia-foundation/TempleOS
c26482bb6ad3f80106d28504ec5db3c6a360732c
```

`Doc/Credits.DD` says Terry A. Davis placed TempleOS in the public domain. The same file lists material from outside sources, including a Cyrillic font used without permission and algorithms or data adapted from other works. This project therefore makes no broader claim about every file in the upstream tree.

The compiler audit uses a narrow set of source files listed in `reference/manifest.json`. New OCaml code is independently implemented and licensed under the repository's MIT License. Any copied or mechanically derived table must retain provenance in `reference/traceability.toml` and its generated output.
