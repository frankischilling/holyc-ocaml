# holyc-ocaml compatibility status

Reference commit: `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

| Phase | Current result | Evidence |
| --- | --- | --- |
| Source loading | Implemented for bounded local files and in-memory buffers | Source and span unit tests |
| Raw lexing | Implemented for identifiers, keywords, literals, comments, and operators | Unit, golden, negative, and property tests |
| Operator specification | Implemented for compound recognition, precedence constants, association flags, and binary IC mappings | Generated-table and public API tests |
| Primitive type facts | Implemented for supported names, raw IDs, sizes, signedness, `Bool`, public integer union backing, and `RT_PTR` | Generated-table and semantic API tests |
| Compiler option specification | Implemented for all 12 bit indices, initial state, gaps, core consumers, and the `Option`/`GetOption` contract; stage behavior is not wired yet | Generated-table and semantic API tests |
| Function-flag specification | Implemented for inherited and function-only bits, parser staging masks, modifier transitions, `RET1`, cleanup selection, and pinned consumers; function parsing and ABI lowering are not implemented | Generated-table and semantic API tests |
| Intermediate-code specification | Implemented for all 185 numeric identities, argument and result metadata, structural types, table flags, and source/display names; executable IR is not implemented | Generated-table and public API tests |
| TempleOS BIN specification | Implemented for the 32-byte header, all 21 defined entries, five reserved codes, relocation formulas, record payloads, two loader passes, eight AOT adjustments, and source-marked limitations; parsing and emission are not implemented | Generated-table and public API tests; [format notes](templeos-bin-format.md) |
| Integrated preprocessing | Not implemented | [Issue #3](https://github.com/frankischilling/holyc-ocaml/issues/3) |
| Parsing | Not implemented | M3 |
| Full semantic analysis | Not implemented | M4 |
| Executable IR and interpretation | Not implemented | M5 |
| Hosted x86-64 | Not implemented | M6 |
| TempleOS assembler | Not implemented | M7 |
| TempleOS `.BIN` serialization and loading | Not implemented | M8 |
| Bootstrap | Not attempted | M9 |

No TempleOS corpus percentage is reported yet. A raw lexer fixture passing does not establish preprocessing, parsing, execution, or loader compatibility.
