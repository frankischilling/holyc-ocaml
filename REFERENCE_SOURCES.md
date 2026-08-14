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

The audit currently covers `Compiler/Compiler.PRJ`, lexer definitions and implementation, include and definition frames, the six standard predefined values and their date and time formats, constant `#if` and `#assert` evaluation, help directives and source-linked help symbols, JIT/AOT and symbol conditional selection, path resolution, preprocessor documentation, diagnostics, character bitmaps, statement-position output literals and their runtime prototypes, structural switch and sub-switch statements, brace-delimited assembly blocks and labels, function-local operand-free direct assembly, function-wide language and assembly-label identity, function signature type resolution, closed aggregate byte layout, aggregate member metadata, global array dimensions, parenthesized and parenthesis-free calls, omitted argument slots, bracket index expressions, direct and pointer member expressions, the complete register and opcode database, primitive raw type constants, public integer union headers, the internal type table, compiler-option state, stored and parser-staging function flags, the complete intermediate-code definition and metadata tables, and the TempleOS BIN header and patch records. Parser, optimizer, kernel, loader, assembler, and backend reads establish how the original compiler consumes those fields. [docs/reference-source-map.md](docs/reference-source-map.md) records the findings and implementation links. The manifest verifies 73 individually audited Git blobs and identifies the full pinned tree used by the 528-file lexer and parser corpus reports.

## Lexer corpus audit

Run the pinned corpus check with:

```text
dune exec holyc -- corpus lex --reference-root=third_party/TempleOS
dune exec holyc -- corpus lex --format=json --reference-root=third_party/TempleOS
```

The command verifies the checkout before and after scanning, enumerates only committed `.HC`, `.HH`, and `.PRJ` paths, and reads each object directly from Git. It does not run reference code. Reading committed objects avoids host checkout line-ending conversion. At the pinned root tree `02b508a8ff9739e628f7eca19b0521f76632d325`, all 528 files tokenize without a lexer diagnostic or internal error. The report records 719,304 tokens, 54 NUL terminators, and 1,266,852 trailing payload bytes. These figures establish raw lexing only.

## Parser corpus audit

Run the AOT parser measurement with:

```text
dune exec holyc -- corpus parse --mode=aot --reference-commit=c26482bb6ad3f80106d28504ec5db3c6a360732c --reference-root=third_party/TempleOS
```

The parser scan uses the same verified Git inventory and gives each root object a fresh session. Root bytes come from Git objects. Includes are resolved inside the clean pinned checkout because the current include stream reads files; the checkout is verified again after the scan. The command never executes native `#exe` code. It records every file, diagnostic counts, and the first error location without host-specific absolute paths.

The reviewed AOT report at [`reference/parser-corpus-aot.json`](reference/parser-corpus-aot.json) contains all 528 objects: 21 parse without an error, 16 have a lexer or preprocessor first error, 491 have a parser first error, and none fail to read or raise an internal exception. There are no exclusions. Its 37,415 accumulated diagnostics include parser recovery after unsupported input and are not a missing-feature count. Physical NUL ends root and included source before any trailing payload, matching the pinned lexical buffer rule; standalone hosted parsing still diagnoses embedded NUL. These figures describe the current OCaml parser at TempleOS commit `c26482bb6ad3f80106d28504ec5db3c6a360732c`; they do not establish semantic or runtime compatibility. `--require-all` turns the remaining 507 failures into a command failure when a strict gate is needed.

## Primitive-global, modifier, binding, pointer, and declaration-list parser audit

The declaration and statement parser uses complete reads of `Compiler/Compiler.PRJ`, `Compiler/CompilerA.HH`, `Compiler/CompilerB.HH`, `Compiler/CExts.HC`, `Compiler/CExcept.HC`, `Compiler/PrsLib.HC`, `Compiler/PrsVar.HC`, `Compiler/PrsStmt.HC`, `Compiler/CMain.HC`, `Compiler/CInit.HC`, `Kernel/KernelB.HH`, `Kernel/KernelC.HH`, and `Doc/HolyC.DD`. The array audit also checks expression and initialized declarations in `Compiler/Lex.HC` and `Compiler/Asm.HC`, plus pointer and multidimensional forms in `Adam/Gr/GrGlbls.HC`. The `for` audit checks empty initializers in `Kernel/StrPrint.HC` and empty or comma-linked updates in `Adam/Opt/Utils/StrUtils.HC`. The `break` audit follows its dispatcher, target check, jump, and common comma-or-semicolon boundary in `PrsStmt`, with switch examples in `Doc/HolyC.DD`. The `lock` audit follows its nesting counter in `PrsStmt`, the `ICF_LOCK` handoff in `PrsLib`, and both braced and unbraced examples in `Demo/MultiCore/LoadTest.HC` and `Demo/MultiCore/Lock.HC`. The switch audit reads all of `PrsSwitch`, the five keyword definitions, `OpCodes.DD`, and representative forms in `Compiler/UAsm.HC`, `Kernel/Compress.HC`, `Kernel/StrPrint.HC`, `Demo/NullCase.HC`, and `Demo/SubSwitch.HC`. The try/catch audit follows `PrsTryBlk`, the generic invalid-keyword path, unbraced forms in `Compiler/PrsExp.HC` and `Kernel/Job.HC`, and braced and nested forms in `Demo/Exceptions.HC`. `Doc/Lex.DD`, `Doc/Options.DD`, and `Doc/ScopingLinkage.DD` provide supporting language context. The manifest records every checksum.

The contextual aggregate member-name audit follows identifier retention in `Compiler/Lex.HC`, optional keyword interpretation in `Compiler/PrsLib.HC`, and the declarator-name path in `Compiler/PrsVar.HC`. Its direct consumers are `Kernel/KernelA.HH`, `Adam/Gr/Gr.HH`, and `Adam/Gr/SpriteMesh.HC`; all three have individual manifest checksums.

The aggregate member-metadata audit follows the class-mode branch in `Compiler/PrsVar.HC`, lookup and extended-string handling in `Compiler/LexLib.HC`, the `CMemberLstMeta` definition in `Kernel/KernelA.HH`, and all six metadata-bearing members in `Demo/ClassMeta.HC`. Each of those source files has an individual manifest checksum. The parser retains syntax and provenance; evaluation and runtime lookup remain semantic work.

The `return` audit follows the complete `KW_RETURN` branch in `PrsStmt`: the function-context check, active-`try` unwind calls, immediate-semicolon test, optional expression, `IC_RETURN_VAL`, function flag update, jump to `lb_leave`, and shared terminator. `Compiler/BackC.HC` supplies valueless forms, `Kernel/FunSeg.HC` supplies early value returns, and `Doc/HolyC.DD` supplies a documented returned expression. The OCaml parser records this syntax without claiming the context, type, unwind, flag, control-flow, lowering, or execution behavior.

`PrsStmt` recognizes `KW_PUBLIC` and `KW_STATIC` before it dispatches a top-level class or internal type to `PrsGlblVarLst`. `public` keeps calling, public, and assembly state while setting `FSF_PUBLIC`; `static` sets `FSF_STATIC`, keeps assembly state, and clears the other staged bits. The later `KW_EXTERN` and `KW_IMPORT` cases select `PRS0_EXTERN` or `PRS0_IMPORT`. The `_extern` and `_import` paths consume a target identifier before the type and select `PRS0__EXTERN` or `PRS0__IMPORT`. `_intern` instead reads a compile-time expression through `LexExpressionI64`; its function path assigns the evaluated value as `exe_addr`, sets `Ff_INTERNAL`, and clears `Cf_EXTERN`. `PrsGlblVarLst` rejects both import modes outside AOT, uses the public bit when it registers a global name, and calls `PrsType` for every list element using the saved base class. It accepts another declarator after a comma and otherwise requires a semicolon. Pointer depth and array dimensions therefore start again for each name. `PrsType` consumes ordinary pointer stars before the identifier and rejects a fifth star with the shared `PTR_STARS_NUM` limit. `PrsClassNew` allocates class records for depths zero through four, and `ICClassPut` enforces the same bound when a class pointer is used. The OCaml parser accepts ordered modifiers, ordinary and alternate-name bindings, `_intern` expression bindings, and comma-separated primitive globals with zero through four stars and ordered array dimensions on each declarator. Each modifier, binding, target, star, bracket, expression, and delimiter retains its source and definition provenance. These syntax nodes do not yet evaluate `_intern` targets, resolve symbols, set internal-function state, create import records, export names, assign storage, or calculate array layout. Parenthesized function-pointer globals and initializers remain outside this slice.

The call-expression audit adds a complete read of `Compiler/PrsExp.HC:PrsFunCall`, the call dispatches in `PrsUnaryTerm`, the default-argument examples in `Doc/HolyC.DD`, the lowercase `throw` declaration in `Kernel/KExcept.HC`, and omitted-slot call sites in `Compiler/AsmInit.HC`, `Compiler/CMain.HC`, and `Compiler/CMisc.HC`. `PrsFunCall` consumes parentheses when present and walks fixed parameters in declaration order. A comma, closing parenthesis, or parenthesis-free call selects a parameter default only when `MLF_DFT_AVAILABLE` is set. A required parameter in the direct form instead consumes an adjacent expression, and a variadic signature receives no extra arguments without a closing-parenthesis mode. The OCaml syntax tree implements both forms and preserves each omitted slot separately from an explicit expression. Accepted function entries retain fixed-parameter order, names, default availability, and the variadic marker. Address-of and local or newer nonfunction shadowing suppress the direct-call route. A native fixture records seven matching observations from the pinned compiler. Default evaluation, `lastclass` substitution, conversions, semantic resolution, lowering, and execution remain later work.

The index-expression audit follows the bracket case in `Compiler/PrsExp.HC:PrsUnaryModifier` and the modifier cycle in `PrsExpression2`. The pinned parser accepts an ordinary expression between the brackets, requires the closing bracket, and returns to the modifier state so another index or call can follow. It uses resolved array dimensions and element classes to form address arithmetic. The OCaml parser retains the syntax and postfix order but does not claim those type, stride, scaling, dereference, or lvalue effects. `Doc/HolyC.DD` supplies the `argv[i]` example, while `Compiler/AsmLib.HC`, `Compiler/UAsm.HC`, `Kernel/Compress.HC`, and `Kernel/Display.HC` supply representative compiler and kernel uses.

The member-expression audit follows the shared dot and `TK_DEREFERENCE` cases in `Compiler/PrsExp.HC:PrsUnaryModifier`. Both forms require an identifier and return to the modifier cycle, which permits chains that mix calls, indexes, direct access, and pointer access. `Compiler/CInit.HC:CmpFillTables` constructs `->` as `TK_DEREFERENCE`, and `Kernel/KernelA.HH` fixes that token ID. `Doc/HolyC.DD` and the compiler sources provide direct, pointer, and mixed chains. The OCaml AST keeps the access kind, original operator, member spelling, and complete base expression. Member lookup, class and union layout, pointer validation, offset calculation, address formation, dereference behavior, and lvalue classification remain semantic and lowering work.

No parser table or TempleOS code is copied. `Frontend.Ast`, `Frontend.Parser`, and `Frontend.Ast_dump` implement the bounded grammar and versioned output. [Issue #51](https://github.com/frankischilling/holyc-ocaml/issues/51) records pointer syntax, [issue #53](https://github.com/frankischilling/holyc-ocaml/issues/53) records comma-separated globals, [issue #55](https://github.com/frankischilling/holyc-ocaml/issues/55) records declaration modifiers, [issue #57](https://github.com/frankischilling/holyc-ocaml/issues/57) records ordinary declaration bindings, [issue #59](https://github.com/frankischilling/holyc-ocaml/issues/59) records alternate-name bindings, [issue #73](https://github.com/frankischilling/holyc-ocaml/issues/73) records `_intern` expression bindings, [issue #75](https://github.com/frankischilling/holyc-ocaml/issues/75) records parenthesized calls, [issue #77](https://github.com/frankischilling/holyc-ocaml/issues/77) records bracket indexes, [issue #79](https://github.com/frankischilling/holyc-ocaml/issues/79) records member access, and [issue #123](https://github.com/frankischilling/holyc-ocaml/issues/123) records parenthesis-free direct calls. [Issue #117](https://github.com/frankischilling/holyc-ocaml/issues/117) records switch and sub-switch syntax, and [issue #151](https://github.com/frankischilling/holyc-ocaml/issues/151) records brace-delimited assembly blocks. Issues #46, #47, and #48 track the remaining declaration, expression, and statement grammar.

## Assembly-block parser audit

The assembly-block audit follows `KW_ASM` dispatch in `Compiler/PrsStmt.HC`, brace handling, hash-type dispatch, opcode parsing, and label recognition in `Compiler/Asm.HC`, assembly expressions and listing in `Compiler/AsmLib.HC`, hash initialization and alias identity in `Compiler/AsmInit.HC`, the complete `Compiler/OpCodes.DD` table, and label and data-directive descriptions in `Doc/Asm.DD`. It also checks the top-level and function-local blocks in `Demo/Asm/AsmAndC1.HC` and `Demo/Asm/AsmAndC2.HC`. Each of these source files has an individual manifest checksum.

`reference/assembly-blocks.json` is a checked inventory of every adjacent `asm` and `{` token pair in committed `.HC` and `.HH` files under `Compiler`, `Kernel`, `Adam`, and `Demo`. The test reads the pinned files through the streaming lexer, tracks brace depth, and compares all paths, source lines, and contexts. It finds 45 blocks in 40 files, including six nested blocks. The pinned root-tree hash in the manifest covers every scanned object even when that object is not one of the individually audited files.

`Frontend.Parser` creates structural assembly nodes in both compilation modes. `Frontend.Ast` and `Frontend.Ast_dump` retain braces, source-line display groups, label forms, classified tokens, and generated-source provenance. `Asm.Opcode`, `Asm.Directive`, and `Asm.Register` resolve checked table identities. Structural labels now receive function-local semantic identities when a complete function AST is collected. This evidence does not cover assembly operand references, directive semantics, output-address validation, the one-instruction assembly label path, encoding, fixups, or execution.

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

## Semantic symbol-scope audit

The semantic table foundation uses complete reads of `Kernel/KHashA.HC`, `Kernel/KHashB.HC`, `Compiler/CHash.HC`, and `Doc/ScopingLinkage.DD`. It also follows hash-chain construction in `Compiler/CMain.HC`, declaration insertion in `Compiler/PrsStmt.HC`, member insertion in `Compiler/LexLib.HC`, and the 17 internal type records in `Compiler/CInit.HC`. Each source blob was already present in the pinned manifest.

`Sema.Symbol_table` reproduces the audited prepend, masked lookup, instance-count, and explicit chain order as a safe OCaml data structure. The task, module, function, block, aggregate, and assembler-block scope names describe contexts required by the semantic pipeline. No TempleOS table or executable code is copied.

Outer expression binding follows `CHashTable.next` in `Kernel/KernelA.HH`, single-table and chained lookup in `Kernel/KHashA.HC`, task ownership and `Spawn` parent links in `Kernel/KTask.HC`, Adam's assembler-table link in `Compiler/AsmInit.HC`, JIT and AOT successor selection in `Compiler/CMain.HC`, local precedence in `Compiler/Lex.HC`, ordinary identifier record cases in `Compiler/PrsExp.HC`, and the scope description in `Doc/ScopingLinkage.DD`. `Kernel/KTask.HC` now has its own pinned blob checksum in the manifest; the other sources were already present. `Sema.Outer_environment` and `Sema.Outer_expression_binding` implement immutable table snapshots and checked lookup without copying TempleOS code or reading a live task. Address assignment, alternate targets, imports, fixups, loader records, typing, and execution remain outside this boundary.

Top-level collection additionally follows `Compiler/PrsStmt.HC:PrsClass`, `PrsGlblVarLst`, and `PrsFunJoin`. `Driver.Semantic_collection` translates accepted AST items into dependency-free semantic declaration facts, and `Sema.Declaration_collection` creates one module scope with source-ordered entries. The collection itself does not reconcile entries or apply flags, addresses, storage, or JIT/AOT publication effects.

Aggregate reconciliation follows the full `PrsClass` selection branch, `Compiler/PrsLib.HC:PrsClassNew`, class lookup from `Compiler/PrsVar.HC:PrsType`, newest-first insertion and lookup in `Kernel/KHashA.HC`, `HTT_CLASS`, `Cf_EXTERN`, and `CHashClass` in `Kernel/KernelA.HH`, and the shadowing notes in `Doc/ScopingLinkage.DD`. `Sema.Aggregate_resolution` and `Driver.Aggregate_resolution` retain every forward and definition site, complete only the newest unresolved same-name identity, and leave older or shadowed identities distinct. Class and union spellings are both retained because the source selects through their shared class category. The result is module-local; it does not copy TempleOS code, traverse parent tasks, resolve named-type uses, calculate layout, or apply linkage.

Aggregate header resolution follows the complete type-selection loop in `Compiler/PrsVar.HC:PrsType`, definition publication and base lookup in `Compiler/PrsStmt.HC:PrsClass`, the five-record allocation in `Compiler/PrsLib.HC:PrsClassNew`, backing traversal in `Compiler/OptLib.HC:OptClassFwd`, the intrinsic spellings in `Compiler/CInit.HC`, and `CHashClass`, `PTR_STARS_NUM`, `HTT_CLASS`, and `HTT_INTERNAL_TYPE` in `Kernel/KernelA.HH`. `Driver.Aggregate_header_resolution` resolves named backings against the state before publication and bases against the state afterward. `Sema.Type` keeps public, intrinsic, and aggregate bases distinct, while `Sema.Aggregate_header_resolution` retains all header token origins and validates ownership without mutation. The implementation does not copy a TempleOS table or follow backing chains. Cycle checks, layout, inherited lookup, ordinary type-use resolution, and parent-task lookup remain outside this boundary.

Direct-member collection follows `Compiler/PrsVar.HC:PrsVarLst` and `Compiler/LexLib.HC:MemberAdd` and `MemberFind`. Recursive anonymous unions pass the same `CHashClass` to `PrsVarLst`, so their named fields belong to the containing class member list rather than a nested lookup table. `Sema.Member_collection` preserves that namespace and source order while retaining a path through the anonymous-union syntax. It does not apply `MemberAdd`'s duplicate exceptions, walk `base_class`, calculate offsets, or evaluate metadata.

Aggregate member type-reference resolution uses the same `PrsVarLst` and `PrsType` paths, the callback `fun_ptr` and `MLF_FUN` branch, `PrsArrayDims`, the five pointer records from `PrsClassNew`, `OptClassFwd`, the intrinsic table in `Compiler/CInit.HC`, and `CMemberLst`, `CArrayDim`, `PTR_STARS_NUM`, `HTT_CLASS`, and `HTT_INTERNAL_TYPE` in `Kernel/KernelA.HH`. `Driver.Member_type_resolution` binds public, intrinsic, and named member heads at their source publication point. `Sema.Member_type_resolution` retains ordinary pointers, callback return pointers, callback indirection, array-dimension origins, and member provenance separately. No TempleOS table or executable code is copied. Callback parameter resolution, extent evaluation, layout, and inherited lookup remain outside this boundary.

Closed aggregate layout follows the member-size and cursor branches in `Compiler/PrsVar.HC:PrsVarLst`, dimension accumulation in `PrsArrayDims`, base and negative-offset handling in `Compiler/PrsStmt.HC:PrsClass`, pointer records in `Compiler/PrsLib.HC:PrsClassNew`, expression result boundaries in `Compiler/PrsExp.HC`, primitive sizes in `Compiler/CInit.HC`, the layout records in `Kernel/KernelA.HH`, explicit alignment directives there, and the negative-displacement example in `Demo/Lectures/NegDisp.HC`. `Sema.Aggregate_layout` implements the observed packed cursor calculation without copying a source table or executing reference code. It returns an explicit dependency for symbol-dependent expressions and later by-value types. Metadata, duplicate rules, inherited lookup, backing conversion, subinteger access, allocation, and generated-code validation remain outside this boundary.

Function-binding collection follows `Compiler/PrsStmt.HC:PrsFunJoin`, `PrsFun`, and the automatic and static local branches in `PrsStmt`; `Compiler/PrsVar.HC:PrsVarLst` and `PrsDotDotDot`; `Compiler/LexLib.HC:MemberAdd` and `MemberFind`; `Kernel/KernelA.HH:CMemberLst` and `CHashFun`; and the function-scope category in `Doc/ScopingLinkage.DD`. These files already have pinned checksums in the manifest. `Sema.Function_collection` preserves named fixed-parameter positions, synthetic `argc`/`argv` order, source-ordered local declarations, storage classification, repeated names, and generated-source provenance. It does not copy source code or claim type resolution, duplicate legality, storage layout, label binding, or linkage.

Function signature type resolution follows `PrsGlblVarLst`, `PrsFunJoin`, and `PrsFun` in `Compiler/PrsStmt.HC`; recursive `PrsType`, fixed-slot and default handling in `PrsVarLst`, and `PrsDotDotDot` in `Compiler/PrsVar.HC`; `MemberLstCmp` in `Compiler/LexLib.HC`; `PrsFunCall` and the miscellaneous-data paths in `Compiler/PrsExp.HC`; the intrinsic type table in `Compiler/CInit.HC`; `CMemberLst`, `CHashFun`, and the `MLF_*` fields in `Kernel/KernelA.HH`; and the default and varargs notes in `Doc/HolyC.DD`. All source blobs already have pinned manifest checksums. `Sema.Type_reference` supplies one checked representation shared with aggregate member types. `Driver.Function_type_resolution` replays aggregate visibility at each function declaration and resolves return and recursive fixed-parameter signatures. `Sema.Function_type_resolution` validates slots, defaults, named top-level parameter bindings, and synthetic varargs without mutating the symbol table. It derives the pinned warning, callback, default, `lastclass`, accepted string-storage, and synthetic-vararg masks at every represented callback level. The implementation copies no TempleOS code or data table. It does not claim the unavailable `TK_INS_BIN` default path, prototype reconciliation, default evaluation, call checking, storage, ABI behavior, linkage, or other type-use sites.

Function-label resolution follows the `goto` and identifier-label branches in `Compiler/PrsStmt.HC`, `COCGoToLabelFind` and `COCDel` in `Compiler/PrsLib.HC`, function-local label publication in `Compiler/Asm.HC`, deferred label references in `Compiler/AsmResolve.HC`, the `CCodeMisc` fields in `Kernel/KernelA.HH`, and the function-scope notes in `Doc/ScopingLinkage.DD`. `Sema.Label_resolution` and `Driver.Label_resolution` give language and structural assembly-block definitions one function-local identity, preserve first-occurrence order and definition provenance, bind forward and backward language gotos, and reject invalid language batches before mutation. Repeated assembly definitions remain recorded for the source-required same-address check in [issue #174](https://github.com/frankischilling/holyc-ocaml/issues/174). No TempleOS code is copied.

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
