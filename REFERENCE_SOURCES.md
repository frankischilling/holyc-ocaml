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

The audit currently covers `Compiler/Compiler.PRJ`, lexer definitions and implementation, include and definition frames, JIT/AOT and symbol conditional selection, path resolution, preprocessor documentation, diagnostics, character bitmaps, keyword and assembler directive records, primitive raw type constants, public integer union headers, the internal type table, compiler-option state, stored and parser-staging function flags, the complete intermediate-code definition and metadata tables, and the TempleOS BIN header and patch records. Parser, optimizer, kernel, loader, assembler, and backend reads establish how the original compiler consumes those fields. [docs/reference-source-map.md](docs/reference-source-map.md) records the findings and implementation links.

## Include-frame audit

The current include implementation uses complete reads of `Compiler/Lex.HC`, `Compiler/LexLib.HC`, `Doc/PreProcessor.DD`, `Doc/Lex.DD`, and `Doc/Directives.DD`, plus the `CLexFile` and `CCmpCtrl` definitions in `Kernel/KernelA.HH` and `DirNameAbs`, `FileNameAbs`, and `ExtDft` in `Kernel/BlkDev/DskStrA.HC`. Their Git blob checksums are in the manifest.

No source table is copied or generated for this behavior. `reference/traceability.toml` links the original functions to `include_resolver.ml`, `lexer_frame.ml`, `preprocessor.ml`, and the focused tests. The hosted filesystem restrictions are documented as project security policy rather than TempleOS behavior.

## Definition-string audit

The definition implementation uses complete reads of `Compiler/Lex.HC`, `Compiler/LexLib.HC`, `Compiler/CHash.HC`, `Kernel/KDefine.HC`, `Doc/PreProcessor.DD`, `Doc/Lex.DD`, and `Doc/Directives.DD`. The audit also follows `CHashDefineStr`, `LFSF_DEFINE`, `CCF_NO_DEFINES`, and task hash ownership in `Kernel/KernelA.HH`; insertion and lookup in `Kernel/KHashA.HC`; source metadata in `Kernel/KHashB.HC`; and character bitmaps in `Kernel/StrA.HC`. The manifest records each of those source blobs.

No replacement table is copied into the project. `Frontend.Definition` and `Frontend.Preprocessor` implement the observed raw-text capture and frame injection. Definition recursion, nesting, and generated-byte diagnostics are hosted safety additions and are identified as such in the traceability entry and preprocessor notes.

## Mode-conditional audit

The JIT/AOT selection uses the complete conditional dispatch, `LexGetChar`, and `LexFilePop` in `Compiler/Lex.HC`; the related lexical helpers in `Compiler/LexLib.HC`; `CmpBuf` in `Compiler/CMain.HC`; the keyword definitions in `Compiler/CompilerA.HH`; the `CCmpCtrl` flags in `Kernel/KernelA.HH`; the preprocessor documentation; and the working `#ifjit` region in `Demo/GlblVars.HC`. Every file already has a pinned checksum in the manifest.

No conditional table is copied. `Frontend.Lexer` provides the raw inactive scan used by `Frontend.Preprocessor`, and the configuration carries the AOT bit's meaning as a typed mode. Stray and unterminated-boundary diagnostics are hosted additions; [issue #27](https://github.com/frankischilling/holyc-ocaml/issues/27) records the difference from the pinned permissive paths.

## Symbol-conditional audit

The `#ifdef` and `#ifndef` implementation follows identifier lookup and both directive cases in `Compiler/Lex.HC`; controller hash-chain setup in `Compiler/CMain.HC`; compiler-hash initialization in `Compiler/AsmInit.HC`; the 17 internal type records in `Compiler/CInit.HC`; the `HTT_*` values and masks in `Kernel/KernelA.HH`; hash insertion and lookup in `Kernel/KHashA.HC`; and the wording in `Doc/PreProcessor.DD`. These files already have pinned checksums in the manifest.

`Frontend.Symbol_visibility` retains the 17 source hash kinds, stable entry identities, the default import exclusion, and the separate local-variable shadow result. `Driver.Session` seeds the generated language keywords, assembly keywords, and internal type spellings. Full opcode and register seeding depends on the complete `OpCodes.DD` model and remains tracked by [issue #30](https://github.com/frankischilling/holyc-ocaml/issues/30). The live `.HC`, `.HH`, and `.PRJ` corpus has no active use of either directive, so compatibility evidence comes from the pinned implementation and focused fixtures rather than a corpus percentage.

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

## Generated operator data

Regenerate token recognition, precedence, and binary operator records with:

```text
dune exec tools/operator_table_gen.exe -- --kernel third_party/TempleOS/Kernel/KernelA.HH --compiler third_party/TempleOS/Compiler/CompilerA.HH --cinit third_party/TempleOS/Compiler/CInit.HC --lex third_party/TempleOS/Compiler/Lex.HC --manifest reference/manifest.json --output src/generated/operator_tables.ml
```

This generator checks all four source files before parsing them. It preserves the three `dual_U16_tokens` arrays, comment openers, lexer-only shift and dot forms, precedence and association constants, and every `cmp.binary_ops` record. `dune build @generated-check` rejects stale operator output alongside the other generated tables.

## Generated compiler option data

Regenerate the compiler-option registry with:

```text
dune exec tools/compiler_option_gen.exe -- --reference-root third_party/TempleOS --manifest reference/manifest.json --output src/generated/compiler_options.ml
```

The generator checks 16 pinned sources before it writes anything. It extracts the 12 `OPTf_*` bit indices from `Kernel/KernelA.HH`, the initial mask from `Compiler/Lex.HC:CmpCtrlNew`, and the `Option`/`GetOption` contract from `Compiler/CMisc.HC` and `Kernel/KUtils.HC:_BEQU`. It also records code references in the lexer, parser, symbol table, optimizer, and backend. Comments and string literals do not count as consumers.

The gaps at bits 2 through 15 and 20 through 31 are retained. `OPTf_USE_IMM64` carries the pinned source's "Not completely implemented" note instead of being presented as finished behavior. The generated registry describes source facts only; later compiler stages must still implement each option's effect.

## Generated intermediate-code data

Regenerate the intermediate-code implementation and interface with:

```text
dune exec tools/intermediate_code_gen.exe -- --compiler third_party/TempleOS/Compiler/CompilerA.HH --cinit third_party/TempleOS/Compiler/CInit.HC --manifest reference/manifest.json --output-ml src/generated/intermediate_codes.ml --output-mli src/generated/intermediate_codes.mli
```

The generator verifies both source tables before parsing them. It requires 185 contiguous `IC_*` definitions, the `IC_ICS_NUM` value `0xB9`, the audited `CIntermediateStruct` layout, and one metadata record for each numeric code. Unknown argument shapes, structural types, Boolean values, padding, or extra fields stop generation.

Constant names and display names remain separate. The source contains 13 real differences, including `IC_SWAP_I64` versus `SWAP_U64`; the generator does not rewrite either spelling. `dune build @generated-check` compares both generated files with a fresh rendering.

## Generated function-flag data

Regenerate the function and parser flag specification with:

```text
dune exec tools/function_flag_gen.exe -- --reference-root third_party/TempleOS --manifest reference/manifest.json --output-ml src/generated/function_flags.ml --output-mli src/generated/function_flags.mli
```

The generator checks nine pinned files before parsing them. It keeps the inherited `CHashClass.flags` bits, function-only `Ff_*` bits, temporary `FSF_*` parser masks, and `FSG_FUN_FLAGS*` groups separate. It also verifies the declaration-modifier assignments and the source conditions governing `RET1`, varargs, caller cleanup, interrupt returns, and internal functions.

The generated helpers describe those source rules without implementing function parsing or machine-code emission. `dune build @generated-check` rejects stale implementation or interface output.

## Generated BIN record data

Regenerate the TempleOS module header and patch-record specification with:

```text
dune exec tools/bin_record_gen.exe -- --reference-root third_party/TempleOS --manifest reference/manifest.json --output-ml src/generated/bin_records.ml --output-mli src/generated/bin_records.mli
```

The generator checks 13 pinned sources before parsing them. It validates the `CBinFile` layout, `'TOSB'` signature, all `IET_*` and `AAT_*` values, reserved numeric gaps, source status comments, import displacement formulas, writer layout, two loader passes, boot absolute patches, and core consumers. Unknown symbols, missing or duplicate records, changed formulas, and stale output fail the generated-file check.

The output supplies typed metadata through `Holyc_lib.Templeos_bin_spec`; it is not a module reader or writer. [docs/templeos-bin-format.md](docs/templeos-bin-format.md) records the audited binary grammar and the work still required before loader compatibility can be claimed.
