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
| Integrated preprocessing | Partially implemented: quoted `#include`; TempleOS text `#define`; deterministic `#if` over literals, definition-expanded constants, `defined`, and the audited HolyC operator table; nested `#ifjit` and `#ifaot` selection in explicit JIT/AOT modes; `#ifdef` and `#ifndef` over definitions and a typed session symbol environment; default import exclusion; local-variable shadowing; deterministic path search; file, definition, invocation, and conditional-error provenance; source-order sharing across includes and session streams; retained expression lookahead; caller resumption; deterministic definition and visibility dumps; inactive-branch side-effect suppression; and explicit hosted path, cycle, mismatch, depth, size, generated-byte, and expression-node limits. Native `#if` can call functions and inspect runtime or compiler state; those terms are rejected until #33 routes them through verified IR and the compile-time VM. The standalone command cannot discover declarations before the parser exists, and opcode or register seeds remain in #30. Cross-frame token formation remains a source difference in #24. Hosted unmatched-boundary diagnostics differ from the pinned permissive behavior and are tracked in #27. `#assert`, `#exe`, predefined values, and general generated source remain unavailable. | [Preprocessor notes](preprocessor.md), [constant expression issue #32](https://github.com/frankischilling/holyc-ocaml/issues/32), [VM expression issue #33](https://github.com/frankischilling/holyc-ocaml/issues/33), [include issue #21](https://github.com/frankischilling/holyc-ocaml/issues/21), [definition issue #23](https://github.com/frankischilling/holyc-ocaml/issues/23), [mode issue #26](https://github.com/frankischilling/holyc-ocaml/issues/26), [symbol issue #29](https://github.com/frankischilling/holyc-ocaml/issues/29), [opcode seed issue #30](https://github.com/frankischilling/holyc-ocaml/issues/30), [boundary issue #24](https://github.com/frankischilling/holyc-ocaml/issues/24), [diagnostic issue #27](https://github.com/frankischilling/holyc-ocaml/issues/27), and [epic #3](https://github.com/frankischilling/holyc-ocaml/issues/3) |
| Parsing | Not implemented | M3 |
| Full semantic analysis | Not implemented | M4 |
| Executable IR and interpretation | Not implemented | M5 |
| Hosted x86-64 | Not implemented | M6 |
| TempleOS assembler | Not implemented | M7 |
| TempleOS `.BIN` serialization and loading | Not implemented | M8 |
| Bootstrap | Not attempted | M9 |

No TempleOS corpus percentage is reported yet. A raw lexer fixture passing does not establish preprocessing, parsing, execution, or loader compatibility.
