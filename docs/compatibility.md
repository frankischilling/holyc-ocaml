# holyc-ocaml compatibility status

Reference commit: `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

| Phase | Current result | Evidence |
| --- | --- | --- |
| Source loading | Implemented for bounded local files and in-memory buffers | Source and span unit tests |
| Raw lexing | Implemented for identifiers, keywords, literals, comments, and operators | Unit, golden, negative, and property tests |
| Operator specification | Implemented for compound recognition, precedence constants, association flags, and binary IC mappings | Generated-table and public API tests |
| Primitive type facts | Implemented for supported names, raw IDs, sizes, signedness, `Bool`, public integer union backing, and `RT_PTR` | Generated-table and semantic API tests |
| Integrated preprocessing | Not implemented | [Issue #3](https://github.com/frankischilling/holyc-ocaml/issues/3) |
| Parsing | Not implemented | M3 |
| Full semantic analysis | Not implemented | M4 |
| IR and interpretation | Not implemented | M5 |
| Hosted x86-64 | Not implemented | M6 |
| TempleOS assembler | Not implemented | M7 |
| TempleOS `.BIN` | Not implemented | M8 |
| Bootstrap | Not attempted | M9 |

No TempleOS corpus percentage is reported yet. A raw lexer fixture passing does not establish preprocessing, parsing, execution, or loader compatibility.
