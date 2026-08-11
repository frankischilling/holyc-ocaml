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

The verifier rejects a missing, dirty, or mismatched checkout and checks every individually audited file in `reference/manifest.json`. Checksums cover the pinned Git blob bytes, so line-ending conversion in a host checkout cannot change the result. The same manifest records the pinned root tree used by the lexer corpus.

## Update policy

A reference update requires a dedicated issue, an impact report, corpus and compatibility comparisons, regenerated fixtures, a dedicated branch, and a pull request. The pull request must describe any changed language or binary behavior.

## Current audit

The audit currently covers `Compiler/Compiler.PRJ`, lexer definitions and implementation, include and definition frames, the six standard predefined values and their date and time formats, constant `#if` and `#assert` evaluation, help directives and source-linked help symbols, JIT/AOT and symbol conditional selection, path resolution, preprocessor documentation, diagnostics, character bitmaps, global array dimensions, the complete register and opcode database, primitive raw type constants, public integer union headers, the internal type table, compiler-option state, stored and parser-staging function flags, the complete intermediate-code definition and metadata tables, and the TempleOS BIN header and patch records. Parser, optimizer, kernel, loader, assembler, and backend reads establish how the original compiler consumes those fields. [docs/reference-source-map.md](docs/reference-source-map.md) records the findings and implementation links. The manifest verifies 51 individually audited Git blobs and identifies the full pinned tree used by the 528-file lexer corpus.

## Lexer corpus audit

Run the pinned corpus check with:

```text
dune exec holyc -- corpus lex --reference-root=third_party/TempleOS
dune exec holyc -- corpus lex --format=json --reference-root=third_party/TempleOS
```

The command verifies the checkout before and after scanning, enumerates only committed `.HC`, `.HH`, and `.PRJ` paths, and reads each object directly from Git. It does not run reference code. Reading committed objects avoids host checkout line-ending conversion. At the pinned root tree `02b508a8ff9739e628f7eca19b0521f76632d325`, all 528 files tokenize without a lexer diagnostic or internal error. The report records 719,304 tokens, 54 NUL terminators, and 1,266,852 trailing payload bytes. These figures establish raw lexing only.

## Primitive-global, modifier, binding, pointer, and declaration-list parser audit

The declaration parser uses complete reads of `Compiler/Compiler.PRJ`, `Compiler/CompilerA.HH`, `Compiler/CompilerB.HH`, `Compiler/CExts.HC`, `Compiler/CExcept.HC`, `Compiler/PrsLib.HC`, `Compiler/PrsVar.HC`, `Compiler/PrsStmt.HC`, `Compiler/CMain.HC`, `Compiler/CInit.HC`, `Kernel/KernelB.HH`, `Kernel/KernelC.HH`, `Doc/HolyC.DD`, `Doc/Lex.DD`, `Doc/Options.DD`, and `Doc/ScopingLinkage.DD`. The array audit also checks expression and initialized declarations in `Compiler/Lex.HC` and `Compiler/Asm.HC`, plus pointer and multidimensional forms in `Adam/Gr/GrGlbls.HC`. The manifest records every checksum.

`PrsStmt` recognizes `KW_PUBLIC` and `KW_STATIC` before it dispatches a top-level class or internal type to `PrsGlblVarLst`. `public` keeps calling, public, and assembly state while setting `FSF_PUBLIC`; `static` sets `FSF_STATIC`, keeps assembly state, and clears the other staged bits. The later `KW_EXTERN` and `KW_IMPORT` cases select `PRS0_EXTERN` or `PRS0_IMPORT`. The `_extern` and `_import` paths consume a target identifier before the type and select `PRS0__EXTERN` or `PRS0__IMPORT`. `_intern` instead reads a compile-time expression through `LexExpressionI64`; its function path assigns the evaluated value as `exe_addr`, sets `Ff_INTERNAL`, and clears `Cf_EXTERN`. `PrsGlblVarLst` rejects both import modes outside AOT, uses the public bit when it registers a global name, and calls `PrsType` for every list element using the saved base class. It accepts another declarator after a comma and otherwise requires a semicolon. Pointer depth and array dimensions therefore start again for each name. `PrsType` consumes ordinary pointer stars before the identifier and rejects a fifth star with the shared `PTR_STARS_NUM` limit. `PrsClassNew` allocates class records for depths zero through four, and `ICClassPut` enforces the same bound when a class pointer is used. The OCaml parser accepts ordered modifiers, ordinary and alternate-name bindings, `_intern` expression bindings, and comma-separated primitive globals with zero through four stars and ordered array dimensions on each declarator. Each modifier, binding, target, star, bracket, expression, and delimiter retains its source and definition provenance. These syntax nodes do not yet evaluate `_intern` targets, resolve symbols, set internal-function state, create import records, export names, assign storage, or calculate array layout. Parenthesized function-pointer globals and initializers remain outside this slice.

No parser table or TempleOS code is copied. `Frontend.Ast`, `Frontend.Parser`, and `Frontend.Ast_dump` implement the bounded grammar and versioned output. [Issue #51](https://github.com/frankischilling/holyc-ocaml/issues/51) records pointer syntax, [issue #53](https://github.com/frankischilling/holyc-ocaml/issues/53) records comma-separated globals, [issue #55](https://github.com/frankischilling/holyc-ocaml/issues/55) records declaration modifiers, [issue #57](https://github.com/frankischilling/holyc-ocaml/issues/57) records ordinary declaration bindings, [issue #59](https://github.com/frankischilling/holyc-ocaml/issues/59) records alternate-name declaration bindings, and [issue #73](https://github.com/frankischilling/holyc-ocaml/issues/73) records `_intern` expression bindings. Issues #46, #47, and #48 track the remaining declaration, expression, and statement grammar.

## Include-frame audit

The current include implementation uses complete reads of `Compiler/Lex.HC`, `Compiler/LexLib.HC`, `Doc/PreProcessor.DD`, `Doc/Lex.DD`, and `Doc/Directives.DD`, plus the `CLexFile` and `CCmpCtrl` definitions in `Kernel/KernelA.HH` and `DirNameAbs`, `FileNameAbs`, and `ExtDft` in `Kernel/BlkDev/DskStrA.HC`. Their Git blob checksums are in the manifest. The `LexBackupLastChar`, `LexIncludeStr`, `LexGetChar`, and `LexFilePop` control flow establishes that an active scanner may exhaust a frame and continue with the caller's saved lookahead before returning a token.

No source table is copied or generated for this behavior. `reference/traceability.toml` links the original functions to `lexer.ml`, `include_resolver.ml`, `lexer_frame.ml`, `preprocessor.ml`, the focused tests, and the native fixture. The hosted filesystem restrictions are documented as project security policy rather than TempleOS behavior.

## Definition-string audit

The definition implementation uses complete reads of `Compiler/Lex.HC`, `Compiler/LexLib.HC`, `Compiler/CHash.HC`, `Kernel/KDefine.HC`, `Doc/PreProcessor.DD`, `Doc/Lex.DD`, and `Doc/Directives.DD`. The audit also follows `CHashDefineStr`, `LFSF_DEFINE`, `CCF_NO_DEFINES`, and task hash ownership in `Kernel/KernelA.HH`; insertion and lookup in `Kernel/KHashA.HC`; source metadata in `Kernel/KHashB.HC`; and character bitmaps in `Kernel/StrA.HC`. The manifest records each of those source blobs.

No replacement table is copied into the project. `Frontend.Definition` and `Frontend.Preprocessor` implement the observed raw-text capture and frame injection. Definition recursion, nesting, and generated-byte diagnostics are hosted safety additions and are identified as such in the traceability entry and preprocessor notes.

## Predefined-value audit

The six standard definitions come from `Kernel/KernelA.HH` and are repeated in `Doc/Directives.DD`. Their bodies use `StreamPrint` or `StreamDir`; `Compiler/CMisc.HC:StreamDir` derives the directory from the active source name. `Kernel/StrPrint.HC:MPrintDate` and `MPrintTime` establish the exact `MM/DD/YY` and `HH:MM:SS` output. The root and include depth rule for `__CMD_LINE__` comes from `Compiler/Lex.HC:LexFilePush` and the standard definition itself. The manifest includes the Git blobs used by these claims.

`Frontend.Predefined` records the six spellings, recognizes the pinned standard bodies, validates explicit deterministic date and time settings, and renders replacement text. `Frontend.Preprocessor` pushes that text through an ordinary bounded lexical frame. This is not a general `#exe` implementation. File and directory values use canonical hosted paths and are documented as a hosted representation of the TempleOS `full_name` and `DirFile` behavior.

## Mode-conditional audit

The JIT/AOT selection uses the complete conditional dispatch, `LexGetChar`, and `LexFilePop` in `Compiler/Lex.HC`; the related lexical helpers in `Compiler/LexLib.HC`; `CmpBuf` in `Compiler/CMain.HC`; the keyword definitions in `Compiler/CompilerA.HH`; the `CCmpCtrl` flags in `Kernel/KernelA.HH`; the preprocessor documentation; and the working `#ifjit` region in `Demo/GlblVars.HC`. Every file already has a pinned checksum in the manifest.

No conditional table is copied. `Frontend.Lexer` provides the raw inactive scan used by `Frontend.Preprocessor`, and the configuration carries the AOT bit's meaning as a typed mode. Stray and unterminated-boundary diagnostics are hosted additions; [issue #27](https://github.com/frankischilling/holyc-ocaml/issues/27) records the difference from the pinned permissive paths.

## Symbol-conditional audit

The `#ifdef` and `#ifndef` implementation follows identifier lookup and both directive cases in `Compiler/Lex.HC`; controller hash-chain setup in `Compiler/CMain.HC`; compiler-hash initialization in `Compiler/AsmInit.HC`; the 17 internal type records in `Compiler/CInit.HC`; the `HTT_*` values and masks in `Kernel/KernelA.HH`; hash insertion and lookup in `Kernel/KHashA.HC`; and the wording in `Doc/PreProcessor.DD`. These files already have pinned checksums in the manifest.

`Frontend.Symbol_visibility` retains the 17 source hash kinds, stable entry identities, the default import exclusion, and the separate local-variable shadow result. `Driver.Session` seeds the generated language keywords, assembly keywords, internal type spellings, registers, canonical opcodes, and aliases. The live `.HC`, `.HH`, and `.PRJ` corpus has no active use of either directive, so compatibility evidence comes from the pinned implementation and focused fixtures rather than a corpus percentage.

## Preprocessor-expression audit

The constant `#if` and `#assert` implementation follows both directive cases in `Compiler/Lex.HC`; `PrsExpression2`, `PrsUnaryTerm`, `LexExpression2Bin`, and `LexExpression` in `Compiler/PrsExp.HC`; `LexWarn` in `Compiler/CExcept.HC`; the precedence constants in `Compiler/CompilerA.HH`; the binary operator and internal type tables in `Compiler/CInit.HC`; the constant-folding cases in `Compiler/OptPass012.HC`; and the wording in `Doc/PreProcessor.DD`. Every source file has a pinned checksum in the manifest.

No expression table is copied for this feature. `Frontend.Conditional_expression` consumes the already checked operator records generated for issue #11. It implements deterministic literal and definition-expanded terms while `Frontend.Preprocessor` retains the expression lookahead and diagnostic provenance. A failed assertion is an ordered warning rather than an error, matching `LexWarn` and its separate warning count. TempleOS can compile and execute nonconstant terms in the same position. [Issue #33](https://github.com/frankischilling/holyc-ocaml/issues/33) tracks that difference and prevents this slice from being described as full `LexExpression` compatibility.

## Help-directive audit

The help metadata implementation follows `KW_HELP_INDEX` and `KW_HELP_FILE` in `Compiler/Lex.HC`, the explicit continuation behavior in `Compiler/LexLib.HC:LexExtStr`, `CHashSrcSym` and the help hash flags in `Kernel/KernelA.HH`, `HashSrcFileSet` in `Kernel/KHashB.HC`, and `FileExtDot`, `ExtDft`, `FileNameAbs`, and `DirNameAbs` in `Kernel/BlkDev/DskStrA.HC`. `Kernel/StrA.HC:StrUtil` establishes the file-name sanitation used by `FileNameAbs`. `Kernel/KernelC.HH` supplies the pinned corpus check. Every file has a checksum in the manifest.

No help document is copied or opened. `Frontend.Help_metadata` records source-ordered index changes and help files, while `Frontend.Preprocessor` attaches include and definition provenance. The pinned `KernelC.HH` test records all 104 `#help_index` directives and all 20 `#help_file` directives without a diagnostic. A later semantic pass must publish equivalent help-file hash entries; this slice records the information but does not claim that hash-table consumer yet.

## Generated opcode data

Regenerate the audited language and assembler directive records with:

```text
dune exec tools/opcode_table_gen.exe -- --source third_party/TempleOS/Compiler/OpCodes.DD --manifest reference/manifest.json --output src/generated/opcode_keywords.ml
```

`dune build @generated-check` compares the checked-in file with a fresh deterministic rendering. The generator parses all four ordered statement groups used by `AsmHashLoad` and retains 106 registers, 73 keyword records, 325 canonical opcodes, 49 aliases, and 924 instruction forms. It rejects unfamiliar syntax rather than producing a partial table. The generator reverses Git's CRLF checkout conversion before checking the pinned blob checksum, so Windows and Unix checkouts validate the same source object. It uses `digestif` for SHA-256 because OCaml's standard `Digest` module only supplies MD5.

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
