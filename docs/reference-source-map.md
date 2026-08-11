# Reference source map

All findings in this document refer to TempleOS commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`.

## Compiler assembly order

`Compiler/Compiler.PRJ` includes kernel headers first, enables conversion of extern declarations to imports while loading later kernel headers, and then loads compiler units in this order: templates, extensions, initialization, exceptions, lexer, hashes, assembler, parser, optimizer, backend, and final optimization passes. The OCaml architecture keeps the same conceptual dependencies without copying the source's global-state layout.

## Lexer state

`Kernel/KernelA.HH` defines `CLexFile` and `CCmpCtrl`. `Compiler/Lex.HC:LexFilePush` maintains the lexical frame stack, while `LexGetChar` owns one-byte pushback and line tracking. Root depth is -1 and the first pushed frame has depth 0. `LexBackupLastChar` saves the caller cursor and one-character lookahead before a push; `LexGetChar` restores that state and continues without an intermediate EOF after a buffer ends. The OCaml source model stores the full byte buffer and precomputed line starts. Each `Lexer_frame` owns one lexer cursor, its source, caller, kind, and relevant include or definition provenance. Hosted include and definition depth are tracked separately.

`Kernel/StrA.HC` shows that the lexer is byte oriented. Identifier starts include ASCII letters, underscore, at sign, and bytes 128 through 255. Digits are accepted after the first byte. The hosted lexer preserves non-ASCII bytes rather than requiring UTF-8.

## Include paths and source frames

The `KW_INCLUDE` branch in `Compiler/Lex.HC:Lex` requires a string token, calls `ExtDft` with `HC.Z`, resolves the result through `FileNameAbs`, and pushes the file with `LexIncludeStr`. `Doc/PreProcessor.DD` rules out a C-style angle-bracket form.

`Kernel/BlkDev/DskStrA.HC:FileNameAbs` delegates to `DirNameAbs`, which starts relative paths from `Fs->cur_dir`; it does not consult the including file's directory. `DirNameAbs` handles root, `::`, home, drive, dot, and parent components. `ExtDft` adds its default only when no extension is present.

`Frontend.Include_resolver` preserves that working-directory rule and adds ordered hosted include roots. `Frontend.Lexer_frame` retains the active canonical path, display spelling, source ID, cursor, include origin, caller, and source depth. `Frontend.Preprocessor` pushes the resolved source and resumes the caller at included EOF. It labels canonical root confinement, cycle checks, and depth and byte caps as hosted security behavior because the pinned lexer does not provide those rejections. [The preprocessor notes](preprocessor.md) record the command, diagnostics, and current directive boundary.

## Definition capture and expansion

The `KW_DEFINE` branch in `Compiler/Lex.HC:Lex` sets `CCF_NO_DEFINES` before reading the name. It then skips only space, tab, and byte `0x1f`, as established by `char_bmp_non_eol_white_space` in `Kernel/StrA.HC`. Replacement capture retains raw bytes, removes LF and CRLF continuations, recognizes `//` only outside double quotes, and leaves block comments for the replacement's later lexical pass. Empty and EOF-terminated definitions are valid.

`CHashDefineStr` in `Kernel/KernelA.HH` stores the name, text, source location, and `cnt = -1`. `Compiler/CHash.HC` and `Kernel/KDefine.HC` provide the related definition APIs. `Kernel/KHashA.HC:HashAdd` inserts at the front of a bucket and `HashFind` returns the first match, which makes the newest same-name definition visible while older entries remain allocated.

Identifier recognition in `Compiler/Lex.HC:Lex` checks `HTT_DEFINE_STR` before it returns a token. A match calls `LexIncludeStr` with a copy of the stored text and marks the pushed `CLexFile` with `LFSF_DEFINE`. `Compiler/CExcept.HC:ParenWarning` and `Compiler/PrsExp.HC` inspect that flag, so the OCaml frame model keeps definitions distinct from physical includes for later parser work.

`Frontend.Definition` assigns source-ordered identities, retains replacement spans and byte-segment maps, and keeps both current lookup state and redefinition history. `Driver.Session` owns that environment because the native `CmpCtrlNew` points definition lookup at the task hash table. `Frontend.Preprocessor` creates a logical source for each expansion, adds invocation and declaration provenance to tokens and diagnostics, and resumes the caller at replacement EOF. Active-definition cycle checks and definition depth and byte budgets are hosted security rules; the pinned lexer has no equivalent guard.

## Help directives and source links

The `KW_HELP_INDEX` branch in `Compiler/Lex.HC:Lex` replaces `CCmpCtrl.cur_help_idx` with one string. `Compiler/LexLib.HC:LexExtStr` receives `lex_next=FALSE`, so it joins more text only through an immediate backslash followed by another string token. `Kernel/KernelC.HH:195-196` contains the pinned continuation example. An empty string remains stored on the controller, but `HashSrcFileSet` treats it as no active index.

The `KW_HELP_FILE` branch applies `ExtDft(...,"DD.Z")`, passes the result to `FileNameAbs`, creates a public `HTT_HELP_FILE` source symbol, and calls `HashSrcFileSet`. `Kernel/BlkDev/DskStrA.HC:FileExtDot` establishes the extension rule. `FileNameAbs` and `DirNameAbs` establish default-directory, root, drive, dot, and parent handling. Their call to `Kernel/StrA.HC:StrUtil` removes leading and trailing whitespace and other control bytes as documented in the preprocessor notes. `Kernel/KHashB.HC:HashSrcFileSet` writes `FL:<full_name>,<line>` and copies a nonempty current index.

`Frontend.Help_metadata` keeps source-ordered index events and help-file records. `Frontend.Preprocessor` attaches source spans, include stacks, and definition traces, while `Frontend.Include_resolver.resolve_metadata` produces a confined hosted path without reading the named file. `holyc preprocess --dump-help-metadata` emits the versioned report. The pinned `Kernel/KernelC.HH` test observes all 104 index directives and 20 help-file directives with no diagnostic. Semantic hash insertion and DolDoc loading remain outside this slice.

## JIT and AOT conditional selection

`Kernel/KernelA.HH` defines `CCF_AOT_COMPILE` at bit 35. `Compiler/CMain.HC:CmpBuf` sets the flag before it joins an AOT compilation, while a normal controller starts without it. The OCaml preprocessor mirrors that distinction with an explicit `Jit` or `Aot` configuration and defaults to JIT. Host operating-system details do not affect the selection.

The `KW_IFAOT` and `KW_IFJIT` cases in `Compiler/Lex.HC:Lex` continue into the matching branch. A false condition scans with `LexGetChar` until the first depth-one `#else` or its `#endif`. Nested depth increases for all five opener spellings: `#if`, `#ifdef`, `#ifndef`, `#ifaot`, and `#ifjit`. The scan calls `Lex` only after it sees `#`, so ordinary discarded identifiers do not expand and inactive includes or definitions do not run. `PrsKeyWord` reads the resolved hash entry, which also permits a definition to supply the directive name.

`LexGetChar` removes exhausted include and definition frames during the search. `Frontend.Preprocessor` therefore owns conditional state above individual `Lexer_frame` values, and `Frontend.Lexer.scan_to_directive_marker` advances through inactive bytes without ordinary tokenization. A conditional can open in an included or definition-backed frame and close after that frame returns to its caller.

Outside `CCF_IN_IF`, the pinned lexer discards an unmatched `#endif`, treats an unmatched `#else` as a request to scan forward, and returns EOF without a mismatch diagnostic when a false branch is unterminated. The hosted stream reports `HCPP0015` through `HCPP0018` instead. [Issue #27](https://github.com/frankischilling/holyc-ocaml/issues/27) records that diagnostic difference and the missing oracle fixtures. The `KW_IFJIT` path also returns `TK_IFAOT` while `CCF_IN_IF` is set; that source quirk concerns later `#if` expression parsing and is not part of the mode-only state.

## Symbol-presence conditionals

The `KW_IFDEF` and `KW_IFNDEF` branches in `Compiler/Lex.HC:Lex` set `CCF_NO_DEFINES` while they read one identifier. Identifier lookup still finds the matching hash entry, but a definition's replacement text does not enter the stream. `#ifdef` continues when `cc->hash_entry` is non-null; `#ifndef` continues when it is null.

`CmpCtrlNew` initializes `hash_mask` to `HTG_TYPE_MASK-HTT_IMPORT_SYS_SYM`. The 17 `HTT_*` bits in `Kernel/KernelA.HH` show that this is a compiler-symbol predicate rather than a definition-only predicate. Imports are excluded. `Compiler/CMain.HC:LexStmt2Bin` builds the local and global table chain for the current compilation mode. Before that hash lookup, `Lex` calls `MemberFind` against `local_var_lst`; a local match suppresses `HashFind` and leaves `hash_entry` null.

`Compiler/AsmInit.HC:AsmHashLoad` inserts language keywords, assembly keywords, opcodes, registers, and all 17 records from `Compiler/CInit.HC:internal_types_table` into `cmp.asm_hash`. `Driver.Session` seeds every one of those static compiler names from checked generated data. `Frontend.Symbol_visibility` models every hash kind, masked lookup, and local shadows with deterministic identities and source provenance. The parser can publish later declarations to the same environment between calls to `Frontend.Preprocessor.next`.

The generated seed contains 48 language keywords, 25 assembly keywords, 17 internal types, 106 registers, 325 canonical opcodes, and 49 opcode aliases. The standalone preprocessor still cannot discover unparsed HolyC declarations. The source corpus has no active `#ifdef` or `#ifndef` occurrence, so focused fixtures establish this slice's behavior.

## Conditional and assertion expressions

The `KW_IF` branch in `Compiler/Lex.HC:Lex` sets `CCF_IN_IF`, reads one token, and calls `LexExpression`. It treats the returned 64 bits as the branch predicate and leaves the token following the expression in the ordinary lexer lookahead. `KW_ASSERT` uses the same evaluator but reports a warning when the result is false.

The assertion branch calls `LexWarn(cc,"Assert Failed ")` and then returns through `lex_end`. `Compiler/CExcept.HC:LexWarn` increments `warning_cnt` without incrementing `error_cnt` or throwing. `Frontend.Preprocessor` therefore queues `HCPP0024` as a warning, while `preprocess_detailed` returns that warning beside the retained tokens and the CLI exits successfully. TempleOS prints the warning at the current lookahead token. The hosted renderer instead uses the directive span and records the lookahead as related context; the compatibility report identifies that diagnostic-only difference.

`Compiler/PrsExp.HC:LexExpression2Bin` calls the normal expression parser, appends return operations, compiles the resulting intermediate code, and returns executable memory. `LexExpression` calls that code without forcing the result to I64 or F64. Native conditions can therefore use any expression term available to the current compiler controller, including calls, globals, memory, and compiler state. The hosted constant evaluator implements only the terms that do not need semantic state or execution. [Issue #33](https://github.com/frankischilling/holyc-ocaml/issues/33) records the remaining VM path.

`PrsExpression2` obtains binary identity, precedence, and association from `cmp.binary_ops`. `Compiler/CInit.HC:CmpFillTables` puts power and shifts in `PREC_EXP`, marks power right associated, and leaves shifts left associated. The parser moves unary minus below power unless parentheses force the negative value to become the base. Consecutive comparison operations use `IC_PUSH_CMP` and combine adjacent comparisons. `Compiler/OptPass012.HC` establishes the constant integer, floating, comparison, and logical behavior used by the hosted evaluator. Its result-class assignments make complement and integer shifts I64. Its immediate folding converts U64-class bits to F64 through the signed I64 field, compares F64 equality by raw bits, and retains an F64 result for unary `!`. Logical operations consume both operands rather than introducing a C-preprocessor short-circuit rule.

`PrsUnaryTerm` handles `defined` without setting `CCF_NO_DEFINES`, so ordinary definition expansion may replace its operand first. This is intentionally different from the `KW_IFDEF` and `KW_IFNDEF` paths. A local variable makes `defined` true, while it suppresses hash lookup and makes `#ifdef` false. The default hash mask still excludes imports.

`Frontend.Conditional_expression` parses with the checked operator records, stores explicit I64, U64, and F64 values, and returns its first unconsumed token. `Frontend.Preprocessor` owns the one-token lookahead so selected body text and adjacent conditional boundaries survive expression parsing. The hosted node limit, stable arithmetic diagnostics, and inert recovery after an invalid condition are project safety rules rather than native TempleOS behavior.

## Tokens and operators

`Kernel/KernelA.HH` assigns numeric values to compound tokens. `Compiler/CInit.HC:CmpFillTables` defines two-character tokens, three-character shifts, and expression precedence. `Compiler/OpCodes.DD` supplies the keyword spellings consumed by `Compiler/AsmInit.HC`.

Generated tables retain the complete language keyword list and operator spellings. Numeric TempleOS token IDs remain available for audit and deterministic dumps, while OCaml variants provide compiler identity after lexing.

## Operator recognition and precedence

`CmpFillTables` builds three `dual_U16_tokens` arrays. `Lex` checks them in order. The first array recognizes spellings such as `!=`, `&&`, `*=`, `++`, and `->`; the second handles alternatives with the same first byte, including `&=`, `--`, `<<`, and `^^`; the third supplies `-=` and `/=`. The `/` entries for `/*` and `//` have no token result because they enter nested block-comment or line-comment handling.

Shift assignments are not ordinary entries in those arrays. After recognizing `<<` or `>>`, `Lex` reads one more byte and changes the token when it sees `=`. Dot handling similarly distinguishes a floating literal, `.`, `..`, and `...`. The `$$` form comes from the lexer's dollar handling and has no `TK_*` compound-token ID.

`cmp.binary_ops` contains 31 records. Each record combines a precedence constant and any explicit association flag in the high half with an IC number in the low half. Some entries, including multiplication and addition, have no association flag in the source. `Frontend.Operator` exposes that state as `Unspecified` instead of assigning a conventional default.

`Doc/HolyC.DD` lists the same precedence bands but omits `%=` from its assignment line. `Compiler/CInit.HC` includes `TK_MOD_EQU` mapped to `IC_MOD_EQU`, so the generated source follows the compiler table and records the prose omission as a documentation difference.

`tools/operator_table_source.ml` validates token IDs, association flags, precedence constants, every dual sequence, required IC numbers, all binary mappings, and the lexer-only forms. `tools/operator_table_gen.ml` writes their source lines and the four pinned checksums to `src/generated/operator_tables.ml`. Expression parsing remains outside this slice.

## Opcode database records

`Compiler/AsmInit.HC:AsmHashLoad` parses `Compiler/OpCodes.DD` with the compiler lexer. Register statements select one of eight register kinds and pair a spelling with a numeric register value. `KEYWORD` and `ASM_KEYWORD` statements each contain a spelling and integer ID. An `OPCODE` statement contains one canonical spelling, up to 32 implicitly delimited instruction forms, an optional colon-led alias list, and a final semicolon.

`AsmPrsInsFlags` accepts the eight opcode modifiers, operand-size markers 16 and 32, `+` and `/` selectors, eight punctuation flags, and `$$`. Each form has no more than four opcode bytes and two `ST_ARG_TYPES` operands. Argument sizes come from the five masks initialized by `AsmHashLoad`. The pinned file contains 106 registers, 48 language keywords with IDs 0 through 47, 25 assembler directives with IDs 64 through 88, 325 canonical opcodes, 49 aliases, and 924 forms. The leading comma on `REP_OUTSB` causes the source loader to create an empty first form; the checked data keeps it.

`tools/opcode_table_source.ml` tokenizes and validates that complete grammar. It rejects missing semicolons, duplicate names or IDs, unexpected and reordered records, incomplete keyword ranges, malformed aliases, unknown arguments, out-of-range numeric fields, excess bytes, and excess forms. `tools/opcode_table_gen.ml` writes the deterministic OCaml table with the reference commit, `Compiler/OpCodes.DD` checksum, original source lines, and every parsed field. The lexer and `Asm_directive` use the keyword subsets, while `Driver.Session` publishes registers, canonical opcodes, and aliases with their source hash kinds. [The assembler notes](assembler.md) give the field-level map. Operand matching and encoding remain future assembler work.

## Primitive raw types and internal names

`Kernel/KernelA.HH` assigns raw IDs 2 through 15. The low bit is `RTF_UNSIGNED`; `RT_PTR` deliberately aliases signed `RT_I64` at ID 10. The same block marks `RT_F32` as unimplemented, `RT_UF32` as unimplemented and fictitious, and `RT_UF64` as fictitious. These slots remain visible in generated audit data but are not semantic HolyC primitive constructors.

`Compiler/CInit.HC:internal_types_table` contains 17 records. Several raw IDs have more than one name. In particular, `Bool` shares `RT_I8`, while the names ending in `i` provide internal storage for public declarations. `Compiler/AsmInit.HC:AsmHashLoad` inserts every record into the compiler hash and leaves the final record for each raw ID in `cmp.internal_types`.

The public `I16`, `U16`, `I32`, `U32`, `I64`, and `U64` names are unions declared near the start of `Kernel/KernelA.HH`. Their leading internal type names select whole-value storage, while their members expose smaller signed and unsigned views. `Primitive_type` records this union-backed declaration form but does not calculate member layout yet.

`tools/primitive_type_source.ml` validates the constants, the six union headers, and every internal record. `tools/primitive_type_gen.ml` embeds the pinned commit, both source checksums, and original line numbers in `src/generated/primitive_raw_types.ml`. `Sema.Primitive_type` uses those records for the 12 supported semantic identities.

## Compiler option state

`Kernel/KernelA.HH` assigns 12 compiler-option bit indices. They are not pre-shifted masks. Bits 0 and 1 control echo and trace behavior, bits 16 through 19 control warnings, and bits 32 through 37 affect linkage, symbol retention, allocation, optimization, and code emission. The ranges 2 through 15 and 20 through 31 are unused in the pinned definitions.

`Compiler/Lex.HC:CmpCtrlNew` creates a controller with `OPTf_WARN_UNUSED_VAR` and `OPTf_WARN_HEADER_MISMATCH` enabled, giving an initial mask of `0x90000`. `Compiler/CMain.HC` copies a parent controller's option mask when compilation is nested. This makes the options part of compile-stream state rather than process-wide settings.

`Compiler/CMisc.HC:Option` and `GetOption` operate on `Fs->last_cc->opts`. `Option` calls `BEqu`, whose `_BEQU` implementation in `Kernel/KUtils.HC` uses `BTS` or `BTR` followed by `ADC`; its result is the bit's state before the change. The OCaml API exposes the same return contract as a pure update that yields the new mask and previous state.

The audited core consumers establish these phase assignments:

| Option | Bit | Initially enabled | Observed core use |
| --- | ---: | --- | --- |
| `OPTf_ECHO` | 0 | No | Lexer echo and compiler option display |
| `OPTf_TRACE` | 1 | No | Parser and compiler trace output |
| `OPTf_WARN_UNUSED_VAR` | 16 | Yes | Function-level unused-variable diagnostics |
| `OPTf_WARN_PAREN` | 17 | No | Unnecessary-parenthesis diagnostics |
| `OPTf_WARN_DUP_TYPES` | 18 | No | Duplicate local type diagnostics |
| `OPTf_WARN_HEADER_MISMATCH` | 19 | Yes | Function header comparison |
| `OPTf_EXTERNS_TO_IMPORTS` | 32 | No | Declaration parsing and AOT linkage |
| `OPTf_KEEP_PRIVATE` | 33 | No | Private source symbol retention |
| `OPTf_NO_REG_VAR` | 34 | No | Register-variable optimization passes |
| `OPTf_GLBLS_ON_DATA_HEAP` | 35 | No | Global allocation and AOT initialization rules |
| `OPTf_NO_BUILTIN_CONST` | 36 | No | Floating constant selection in the backend |
| `OPTf_USE_IMM64` | 37 | No | Import call lowering and emission; marked incomplete in the source |

`tools/compiler_option_source.ml` validates the definitions, source comments, initial mask, public API, `_BEQU` behavior, and core references while ignoring occurrences in comments and literals. `tools/compiler_option_gen.ml` writes the pinned checksums and line references to `src/generated/compiler_options.ml`. `Sema.Compiler_option` provides immutable lookup and mask operations. It does not yet connect these values to a compiler controller or implement their downstream effects.

## Function and calling flags

`CHashFun` inherits the `U16 flags` field declared by `CHashClass` in `Kernel/KernelA.HH`. `Cf_EXTERN` and `Cf_INTERNAL_TYPE` occupy bits 0 and 1 of that shared field. The eight `Ff_*` function positions are contiguous from bit 8 through bit 15; the lower-byte gap is part of the representation and is not compacted in the OCaml model.

`Compiler/CompilerA.HH` defines a second flag set used while `PrsStmt` reads a declaration. `FSF_PUBLIC`, `FSF_ASM`, `FSF_STATIC`, and `FSF__` occupy bits 0 through 3 of this temporary value. The four calling modifiers use the same masks as their stored `Ff_*` counterparts. `FSG_FUN_FLAGS1` selects only those four stored bits, while `FSG_FUN_FLAGS2` adds public parser state.

`PrsFunJoin` copies `FSG_FUN_FLAGS1` into `CHashFun.flags` and transfers `FSF_PUBLIC` to the hash type separately. It derives `Ff_RET1` only when the fixed argument block is nonempty, no varargs marker is present, and the eight-byte argument slots fit a signed 16-bit return immediate. This gives a fixed-argument range of 1 through 4,095.

The declaration modifier assignments are order-sensitive. `interrupt` sets both `FSF_INTERRUPT` and `FSF_NOARGPOP`. The calling modifiers and `public` retain existing calling, public, and assembly state. `static` keeps assembly state but clears the other staged bits. A leading underscore handled by `_extern` or `_import` adds `FSF__` without clearing state.

`PrsExp.HC` selects caller cleanup with `(Ff_RET1 || Ff_ARGPOP) && !Ff_NOARGPOP`. `OptPass789A.HC` applies the same rule in function epilogues. That backend also uses `Ff_INTERRUPT` for interrupt save/restore and `IRETQ`, and it removes an eight-byte error-code slot when `Ff_HASERRCODE` is present. `PrsVar.HC` marks varargs with `Ff_DOT_DOT_DOT`; `OptPass3.HC` accounts for the synthetic `argc` and `argv` members. Internal functions are dispatched as IC operations in `PrsExp.HC`, excluded from ordinary call clobbering in `OptPass6.HC`, and omitted from address-to-function lookup in `Kernel/FunSeg.HC`.

`tools/function_flag_source.ml` checks the definitions, source expressions, structure ownership, modifier assignments, downstream predicates, and every core consumer while ignoring comments and literals. `tools/function_flag_gen.ml` emits immutable nested modules and source references in `src/generated/function_flags.ml`. `Sema.Function_flag` provides typed lookup and pure transition helpers. Function parsing, validity diagnostics, call lowering, and native ABI emission remain later work. [The ABI notes](holy-abi.md) keep that boundary explicit.

## Intermediate-code operation table

`Compiler/CompilerA.HH` defines `IC_END` at `0x00` through `IC_ATAN` at `0xB8`, followed by `IC_ICS_NUM` at `0xB9`. The numeric range is contiguous. The same file defines four argument shapes and four structural categories in `CIntermediateStruct`.

`Compiler/CInit.HC:intermediate_code_table` supplies one record per code. Each record contains the argument shape, result count, structural category, `fpop`, `not_const`, three zero padding bytes, and a display name. `Compiler/OptPass012.HC` uses the argument count to rebuild expression trees, sets `CCF_NOT_CONST` from `not_const`, and pushes results according to `res_cnt`. `Compiler/OptLib.HC` uses `fpop` while arranging floating-stack behavior. Other parser, optimizer, diagnostic, and backend units also query these fields.

The `IC_*` constant name and the display name are not interchangeable. Thirteen records differ:

| Numeric code | Constant | Display name |
| ---: | --- | --- |
| `0x8F` | `IC_BR_EQU_EQU2` | `BR_2EQU_EQU` |
| `0x90` | `IC_BR_NOT_EQU2` | `BR_2NOT_EQU` |
| `0x91` | `IC_BR_LESS2` | `BR_2LESS` |
| `0x92` | `IC_BR_GREATER_EQU2` | `BR_2GREATER_EQU` |
| `0x93` | `IC_BR_GREATER2` | `BR_2GREATER` |
| `0x94` | `IC_BR_LESS_EQU2` | `BR_2LESS_EQU` |
| `0xA8` | `IC_SWAP_I64` | `SWAP_U64` |
| `0xAB` | `IC_MIN_I64` | `I64_MIN` |
| `0xAC` | `IC_MIN_U64` | `U64_MIN` |
| `0xAD` | `IC_MAX_I64` | `I64_MAX` |
| `0xAE` | `IC_MAX_U64` | `U64_MAX` |
| `0xB0` | `IC_SQR_I64` | `SQRI64` |
| `0xB1` | `IC_SQR_U64` | `SQRU64` |

`tools/intermediate_code_source.ml` parses both tables without deriving one name from the other. `tools/intermediate_code_gen.ml` emits an exhaustive `Ir.Opcode.t` variant, typed metadata, safe lookups, pinned checksums, and original line numbers. The checked table is an input to future IR work; it does not yet classify complete side effects, represent operands, or make any opcode executable.

## TempleOS BIN header and patch records

`Kernel/KernelA.HH` defines the `'TOSB'` signature, the seven-field `CBinFile` header, 21 named `IET_*` values, five reserved numeric slots, and eight `AAT_*` adjustment operations. The header occupies 32 bytes. `Compiler/CMain.HC:Cmp` writes the code and initialized-data body immediately after that header, places the patch table after the body, terminates it with `IET_END`, and rounds the complete file size to 16 bytes.

`Kernel/KLoad.HC:Load` treats the byte after the header as `module_base`. `LoadPass1` registers exports, resolves or defers imports, applies absolute-address patches, and creates heap allocations. `LoadPass2` walks the same variable-length records and calls `IET_MAIN` entries. Relative import fields receive `target - field_address - field_width`; immediate fields receive the target itself. The source retains fictitious zero-width import values, marks 64-bit exports as not implemented, and marks three heap forms as not really used. The OCaml model preserves those comments instead of treating every loader switch case as a compatibility claim.

`Kernel/KStart16.HC` locates the boot patch table, and `Kernel/KStart32.HC:CPatchTableAbsAddr` performs the boot absolute-address pass. Compiler producers and consumers in `AsmResolve.HC`, `Asm.HC`, `PrsVar.HC`, `BackFA.HC`, `BackLib.HC`, `BackC.HC`, `OptPass3.HC`, and `OptPass789A.HC` establish which record and adjustment kinds reach assembler, parser, optimizer, and backend paths.

`tools/bin_record_source.ml` requires that exact 13-file source set. It validates checksums, table order, numeric gaps, status comments, header fields, writer behavior, loader formulas, both passes, boot handling, and source consumers while ignoring comments and literals. `tools/bin_record_gen.ml` emits typed entries, adjustments, record shapes, relocation metadata, loader actions, checksums, and line references to `src/generated/bin_records.ml`. `Backend.Bin_spec` exposes that data through `Holyc_lib.Templeos_bin_spec`.

This slice is an executable format specification, not format compatibility. It has no serializer, parser, loader model, generated module, or actual TempleOS loader result. [The BIN format notes](templeos-bin-format.md) describe the byte grammar and the validation still required for M8.

## Literals

`Compiler/Lex.HC:Lex` accumulates integers in a target `I64`, so overflow wraps. Hex and binary prefixes are case insensitive. Character constants store up to eight decoded bytes in little-endian order. `LexInStr` defines the recognized escapes. The hosted lexer diagnoses unterminated literals instead of relying on an in-memory NUL terminator.

## Comments

The pinned lexer nests `/* ... */` comments. It consumes `//` through the end of the line. The OCaml lexer retains both as leading trivia so later diagnostics and formatting tools can recover their exact bytes.

## Diagnostics

`Compiler/CExcept.HC` prints the current token, file, line, and source line before raising the compiler exception. Hosted diagnostics retain those source facts in structured values with stable codes. Rendering is separate from construction so JSON and terminal output share the same diagnostic.

## Interpretation decisions

- Files are finite OCaml byte strings. An embedded NUL is diagnosed because TempleOS uses NUL as an internal buffer terminator.
- Lines increment on LF, matching `LexGetChar`. CR is whitespace but does not start a new line by itself.
- A raw lexer command returns `#` as punctuation. `#include` and `#define` execute only through the separate preprocessed stream, so tooling can still inspect directive and replacement tokens.
- Extensionless hosted includes try `.HC.Z` and then the decompressed `.HC` form used by the pinned Git checkout. Transparent TempleOS `.Z` decompression remains unimplemented.
- Allowed-root enforcement, active-path cycle diagnostics, and nesting and size limits are hosted security differences. They are not attributed to the native TempleOS lexer.
- Dollar-delimited text is retained as comment trivia in raw-source mode. Interpreting DolDoc markup is outside this slice, while `$$` is recognized as the HolyC current-position token.
